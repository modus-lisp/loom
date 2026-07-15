;;;; inspect/serve.lisp — weft as a raster web service (the Opera Mini approach).
;;;;
;;;; The engine renders each page to a full-height PNG on the server; a thin HTML
;;;; client shows that raster in an <img> (the browser scrolls it natively) and
;;;; sends clicks back as (x,y).  The server hit-tests, dispatches the DOM click /
;;;; follows the link, re-renders, and serves the new raster.  All the browsing
;;;; logic is loom's SDL-free page model; this file is just an HTTP front-end.
;;;;   sbcl --script inspect/serve.lisp [port]
(require :asdf)
(require :sb-bsd-sockets)
(push (truename (merge-pathnames "../" (directory-namestring *load-truename*)))
      asdf:*central-registry*)
(handler-bind ((warning #'muffle-warning))
  (asdf:load-system "loom"))

(defpackage #:weft-serve
  (:use #:cl)
  (:local-nicknames (#:l #:loom) (#:r #:weft.render) (#:fetch #:weft.fetch) (#:sock #:sb-bsd-sockets)))
(in-package #:weft-serve)

;;; A browsing CONTEXT (BCTX) is an isolated network identity — its own cookie jar.
;;; A TAB is a client-visible session pointing at a context and its current render.
;;; A freshly typed URL opens a NEW context, so a site can't be re-identified across
;;; visits; following a link keeps the tab's context (and a popup would share its
;;; opener's — future).  Tabs are keyed by an id the client mints per browser tab
;;; (sessionStorage), defaulting to "0" for a client that doesn't send one.
(defstruct bctx (jar (fetch:make-cookie-jar)))
(defstruct tab
  (bctx (make-bctx))
  (page nil)
  (status "type a URL and press Go")
  (gen 0)                  ; bumped per render so the client's <img> cache-busts
  (width 1024)             ; layout width (the client's viewport `vw`)
  (png nil) (png-gen -1)   ; per-gen PNG encode cache
  (timeline nil))          ; last navigation's phase timeline (inspector)

(defvar *tabs* (make-hash-table :test 'equal) "client tab-id -> TAB")
(defvar *tabs-lock* (sb-thread:make-mutex :name "tabs"))
(defun tab-for (id)
  "The TAB for client ID (minted per browser tab), created on first use."
  (let ((id (if (and (stringp id) (plusp (length id))) id "0")))
    (sb-thread:with-mutex (*tabs-lock*)
      (or (gethash id *tabs*) (setf (gethash id *tabs*) (make-tab))))))
(defun drop-tab (id)
  "Forget a closed tab (its page, context and cookie jar are released)."
  (when (and (stringp id) (plusp (length id)))
    (sb-thread:with-mutex (*tabs-lock*) (remhash id *tabs*))))

;;; ---- engine status (the pinned status tab reads this) ---------------------
(defvar *start-time* (get-universal-time) "When the server booted (for uptime).")
(defvar *rendering* nil "URL the (single, lock-serialized) active render is on, or NIL.")
(defvar *recent-errors* '() "In-memory ring of recent (universal-time kind url detail) for the status view.")
(defvar *recent-lock* (sb-thread:make-mutex :name "recent"))
(defun note-error (kind url detail)
  (sb-thread:with-mutex (*recent-lock*)
    (push (list (get-universal-time) (string kind) (or url "") (princ-to-string detail)) *recent-errors*)
    (when (> (length *recent-errors*) 50) (setf *recent-errors* (subseq *recent-errors* 0 50)))))

;;; The server is threaded (one thread per connection) so a slow render doesn't
;;; freeze the whole service — read requests (/view.png, /, status) are answered
;;; while a navigation renders.  These locks serialize the mutating work:
;;;  *RENDER-LOCK*  one render (navigate/click/relayout) at a time; readers skip it,
;;;                 so they see the CURRENT page until the new one atomically swaps in.
;;;  *PNG-LOCK*     guards the per-*GEN* PNG encode cache.
;;;  *ERRLOG-LOCK*  the sqlite handle is not reentrant.
(defvar *render-lock* (sb-thread:make-mutex :name "render"))
(defvar *png-lock* (sb-thread:make-mutex :name "png-cache"))
(defvar *errlog-lock* (sb-thread:make-mutex :name "errlog"))

;;; ---- error log (system libsqlite3 over sb-alien; no schema of our own) -----
(ignore-errors (sb-alien:load-shared-object "libsqlite3.so.0"))
(sb-alien:define-alien-routine ("sqlite3_open" %sqlite-open) sb-alien:int
  (filename sb-alien:c-string) (ppdb (* sb-alien:system-area-pointer)))
(sb-alien:define-alien-routine ("sqlite3_exec" %sqlite-exec) sb-alien:int
  (db sb-alien:system-area-pointer) (sql sb-alien:c-string)
  (cb sb-alien:system-area-pointer) (arg sb-alien:system-area-pointer)
  (errmsg sb-alien:system-area-pointer))

(defvar *errdb* nil "libsqlite3 handle (a SAP), or NIL if logging is unavailable.")
(defun %nsap () (sb-sys:int-sap 0))
(defun errlog-exec (sql)
  (when *errdb*
    (sb-thread:with-mutex (*errlog-lock*)
      (ignore-errors (%sqlite-exec *errdb* sql (%nsap) (%nsap) (%nsap))))))

(defun errlog-open (path)
  "Open (creating) the sqlite error log at PATH and ensure the table exists."
  (handler-case
      (sb-alien:with-alien ((h sb-alien:system-area-pointer))
        (when (zerop (%sqlite-open (namestring path) (sb-alien:addr h)))
          (setf *errdb* h)
          (errlog-exec "CREATE TABLE IF NOT EXISTS errors (id INTEGER PRIMARY KEY,
                        ts TEXT DEFAULT CURRENT_TIMESTAMP, kind TEXT, url TEXT, detail TEXT)")
          (format t "~&error log: ~a~%" (namestring path))))
    (error (e) (format t "~&error log unavailable (~a)~%" e))))

(defun %sqlq (s)
  "Single-quote S as a SQL string literal (doubling embedded quotes)."
  (with-output-to-string (o)
    (write-char #\' o)
    (loop for c across (or s "") do (when (char= c #\') (write-char #\' o)) (write-char c o))
    (write-char #\' o)))

(defun log-error (kind url detail)
  "Record one error row.  Never signals — logging must not break a request."
  (ignore-errors (note-error kind url detail))
  (ignore-errors
    (format *error-output* "~&[errlog] ~a ~a: ~a~%" kind (or url "") detail)
    (errlog-exec (format nil "INSERT INTO errors (kind,url,detail) VALUES (~a,~a,~a)"
                         (%sqlq (string kind)) (%sqlq (or url ""))
                         (%sqlq (princ-to-string detail))))))

;;; ---- browsing -------------------------------------------------------------
(defun navigate-tab (tab url &key (fresh t))
  "Load URL into TAB.  FRESH (a typed URL / address-bar open) starts a NEW browsing
   context so the visit shares no cookies with a previous one; following a link
   (FRESH NIL, via the page's on-navigate) keeps the tab's context.  Every fetch
   uses that context's cookie jar."
  (when fresh (setf (tab-bctx tab) (make-bctx)))
  ;; Render at the client's own width; height comes from content flow (reader view).
  ;; The exception is a page that clips at the root (a viewport-model page like
  ;; Acid2): it gets a fixed viewport whose height is a 4:3 slice of that width.
  (let ((vph (max 480 (round (* (tab-width tab) 3/4))))
        (jar (bctx-jar (tab-bctx tab)))
        (u (if (or (search "://" url) (eql 0 (search "http" url))) url
               (concatenate 'string "https://" url))))
   (handler-case
      (let ((pg (l:load-url u :width (tab-width tab) :viewport-height vph :cookie-jar jar)))
        ;; a link followed inside this tab stays in the same context (fresh nil)
        (setf (l:page-on-navigate pg)
              (lambda (p tgt) (declare (ignore p)) (navigate-tab tab tgt :fresh nil)))
        (when (l:page-js-error pg) (log-error "script" url (l:page-js-error pg)))
        (setf (tab-page tab) pg
              (tab-status tab) (format nil "~a  —  ~a" (or (l:page-title pg) "") (or (l:page-url pg) url)))
        (incf (tab-gen tab)))
    (error (e)
      (log-error "navigate" url e)
      (setf (tab-status tab) (format nil "couldn't load ~a  (~a)" url e))))))

(defun tab-png-bytes (tab)
  "Encode TAB's current page canvas to a compressed PNG, cached per its GEN — the
   DEFLATE encode costs seconds on a tall page, so encode once per navigation, not
   per /view.png.  Thread-safe: a /view.png reader encodes the CURRENT page (fields
   snapshotted) while another thread renders the next; the update is on *PNG-LOCK*."
  (let ((pg (tab-page tab)) (gen (tab-gen tab)))
    (when pg
      (sb-thread:with-mutex (*png-lock*)
        (unless (and (tab-png tab) (= (tab-png-gen tab) gen))
          (setf (tab-png tab) (coerce (r:canvas->png (l:page-canvas pg)) '(simple-array (unsigned-byte 8) (*)))
                (tab-png-gen tab) gen))
        (tab-png tab)))))

(defun tab-state-text (tab)
  "The client-facing tab state: current URL on the first line, status on the second."
  (format nil "~a~%~a" (or (and (tab-page tab) (l:page-url (tab-page tab))) "") (or (tab-status tab) "")))

(defun click-tab (tab x y)
  "Route a viewport click at (X,Y) in TAB — the raster is the full page, scroll-y
   stays 0, so image coords are page coords.  mouse-release follows links (via the
   page's on-navigate, which re-navigates this tab) + fires the DOM click."
  (let ((pg (tab-page tab)))
    (when pg
      (l:mouse-press pg x y)
      (l:mouse-release pg x y)   ; may re-navigate the tab via on-navigate
      (incf (tab-gen tab)))))

;;; ---- the client -----------------------------------------------------------
(defun client-html ()
  ;; A tab bar at the top holds a pinned \"status\" tab (engine status) plus one
  ;; browsing tab per open page.  Each browsing tab is its own server session and
  ;; browsing context (own cookies); its id is sent on every request.  Tabs persist
  ;; in sessionStorage so a reload keeps them.
  (format nil "<!doctype html><html><head><meta charset=\"utf-8\"><title>weft</title>
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
<script>document.cookie='vw='+Math.round(window.innerWidth)+';path=/;max-age=31536000';</script>
<style>body{margin:0;font:14px sans-serif;background:#222;color:#eee}
#hdr{position:sticky;top:0;z-index:10;background:#2a2a2a}
#tabs{display:flex;gap:2px;padding:5px 5px 0;overflow-x:auto;white-space:nowrap;scrollbar-width:thin}
#tabs .tab{display:inline-flex;align-items:center;gap:6px;padding:5px 9px;background:#363636;color:#bbb;border-radius:7px 7px 0 0;cursor:pointer;font-size:12px;max-width:190px;flex:none}
#tabs .tab.on{background:#444;color:#fff}
#tabs .tab .lbl{overflow:hidden;text-overflow:ellipsis;white-space:nowrap}
#tabs .tab .x{color:#888;font-weight:bold;padding:0 1px;border-radius:3px}
#tabs .tab .x:hover{color:#fff;background:#a44}
#tabs .status{background:#293a29;color:#9c9}#tabs .status.on{background:#3a4a3a;color:#cfc}
#tabs .add{background:transparent;color:#9c9;font-size:16px;padding:2px 11px}#tabs .add:hover{color:#fff}
#bar{background:#333;padding:6px;display:flex;gap:6px}
#bar input{flex:1;padding:5px;font-size:16px;min-width:0}#bar button{padding:5px 12px}
#s{padding:4px 8px;color:#9c9;font-size:12px}#v{display:block;width:100%;height:auto;background:#fff}
#p{position:fixed;top:0;left:0;right:0;height:3px;overflow:hidden;z-index:20;display:none}
body.loading #p{display:block}
#p::before{content:'';position:absolute;height:100%;width:40%;background:#6cf;box-shadow:0 0 8px #6cf;animation:sl 1.1s ease-in-out infinite}
@keyframes sl{0%{left:-42%}100%{left:100%}}
body.loading #v{opacity:.5;transition:opacity .15s}
body.loading #s{color:#6cf}
#st{display:none;padding:12px 14px;font:12px/1.7 ui-monospace,monospace;color:#cdc}
#st h4{margin:14px 0 4px;color:#6cf;font-weight:normal;border-bottom:1px solid #333;padding-bottom:3px}
#st .srow{padding:2px 0;overflow-wrap:anywhere}#st .k{color:#e88}#st .dim{color:#888}#st .det{color:#aaa;padding-left:10px}
body.statusview #bar,body.statusview #v,body.statusview #s{display:none}
body.statusview #st{display:block}
body.sidebar{padding-right:340px}
body.sidebar #st{display:block;position:fixed;top:0;right:0;bottom:0;width:340px;box-sizing:border-box;overflow:auto;background:#242424;border-left:1px solid #444;z-index:15}
#st .dock{cursor:pointer;color:#6cf;display:inline-block;padding:3px 9px;background:#2a2a2a;border-radius:5px;margin-bottom:8px}#st .dock:hover{background:#333}
#tabs .status.docked{opacity:.6}
#ip{display:none;position:fixed;left:0;right:0;bottom:0;z-index:30;padding:8px 10px;background:rgba(20,20,20,.96);color:#ccc;font:11px/1.5 ui-monospace,monospace;max-height:62vh;overflow:auto;border-top:1px solid #444;box-shadow:0 -3px 14px rgba(0,0,0,.6)}
body.showins #ip{display:block}
#ip .close{position:sticky;top:0;float:right;color:#9c9;cursor:pointer;padding:0 4px}
#ip h4{margin:9px 0 3px;color:#6cf;font-weight:normal;border-bottom:1px solid #333;padding-bottom:2px}
#ip .sum{color:#9c9;margin-bottom:6px}
#ip .row{display:flex;align-items:center;gap:6px;height:16px}
#ip .lbl{width:160px;flex:none;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}
#ip .track{flex:1;position:relative;height:9px;background:#242424;border-radius:2px}
#ip .bar{position:absolute;height:100%;background:#6cf;border-radius:2px}
#ip .ms{width:96px;flex:none;text-align:right;color:#8a8}</style></head>
<body><div id=p></div>
<div id=hdr><div id=tabs></div>
<form id=bar novalidate><input id=u placeholder=\"https://…\" type=\"url\" inputmode=\"url\" autocapitalize=\"none\" autocorrect=\"off\" spellcheck=\"false\">
<button type=submit>Go</button><button type=button id=flag title=\"flag this page as broken\">&#9873;</button><button type=button id=insp title=\"inspector\">&#9201;</button></form></div>
<div id=s></div><img id=v><div id=st></div><div id=ip></div>
<script>
var v=document.getElementById('v'),u=document.getElementById('u'),s=document.getElementById('s'),
    tabsEl=document.getElementById('tabs'),stEl=document.getElementById('st');
function newId(){return Date.now().toString(36)+Math.random().toString(36).slice(2,8);}
function esc(x){return String(x).replace(/[&<>]/g,function(c){return c=='&'?'&amp;':c=='<'?'&lt;':'&gt;';});}
function host(x){var m=/^https?:\\/\\/([^\\/]*)/.exec(x||'');return m?m[1]:(x||'');}
// ---- tabs: [{id,url,title}] + a pinned 'status' pseudo-tab; persisted per browser tab
var tabs,active;
try{tabs=JSON.parse(sessionStorage.getItem('loomtabs'))||[];}catch(e){tabs=[];}
active=sessionStorage.getItem('loomactive')||null;
if(!tabs.length){tabs=[{id:newId(),url:'',title:''}];active=tabs[0].id;}
if(active!=='status'&&!tabs.some(function(t){return t.id===active;}))active=tabs[0].id;
function save(){try{sessionStorage.setItem('loomtabs',JSON.stringify(tabs));sessionStorage.setItem('loomactive',active);}catch(e){}}
function tabById(id){for(var i=0;i<tabs.length;i++)if(tabs[i].id===id)return tabs[i];return null;}
// On a desktop-width window the status tab can dock as a right sidebar so the page
// stays visible; on narrow screens it is always a full tab.  Preference persists.
var sidebar=localStorage.getItem('loomsidebar')==='1';
function isDesk(){return window.innerWidth>=900;}
function sideOn(){return sidebar&&isDesk();}
function firstBrowseId(){if(!tabs.length){tabs=[{id:newId(),url:'',title:''}];}return tabs[0].id;}
function applyLayout(){document.body.classList.toggle('sidebar',sideOn());}
function toggleSidebar(){
  sidebar=!sidebar;try{localStorage.setItem('loomsidebar',sidebar?'1':'0');}catch(e){}
  if(sideOn()){if(active==='status')active=firstBrowseId();}else{active='status';}
  applyLayout();save();renderTabs();showActive();refreshStatus();
}
window.addEventListener('resize',function(){applyLayout();showActive();});
function T(u){return T2(u,active);}
function T2(u,id){return u+(u.indexOf('?')<0?'?':'&')+'tab='+encodeURIComponent(id);}
function label(t){return t.title||host(t.url)||'new tab';}
function renderTabs(){
  var h='<span class=\"tab status'+(active==='status'?' on':'')+(sideOn()?' docked':'')+'\" data-id=status title=\"engine status\">\\u2699 status</span>';
  tabs.forEach(function(t){h+='<span class=\"tab'+(t.id===active?' on':'')+'\" data-id=\"'+t.id+'\"><span class=lbl>'+esc(label(t))+'</span><span class=x data-close=\"'+t.id+'\">\\u00d7</span></span>';});
  h+='<span class=\"tab add\" id=addtab title=\"new tab\">+</span>';
  tabsEl.innerHTML=h;
}
tabsEl.onclick=function(e){
  var c=e.target.getAttribute&&e.target.getAttribute('data-close');if(c){closeTab(c);return;}
  if(e.target.id==='addtab'){newTab();return;}
  var el=e.target;while(el&&el!==tabsEl&&!(el.getAttribute&&el.getAttribute('data-id')))el=el.parentNode;
  var id=el&&el.getAttribute&&el.getAttribute('data-id');if(!id)return;
  if(id==='status'&&sideOn()){toggleSidebar();return;}   // click ⚙ while docked -> undock to a full tab
  switchTo(id);
};
function newTab(){var t={id:newId(),url:'',title:''};tabs.push(t);switchTo(t.id);u.focus();}
function idx(id){for(var i=0;i<tabs.length;i++)if(tabs[i].id===id)return i;return -1;}
function closeTab(id){
  fetch('/close?tab='+encodeURIComponent(id)).catch(function(){});
  var i=idx(id);if(i<0)return;tabs.splice(i,1);
  if(!tabs.length)tabs=[{id:newId(),url:'',title:''}];
  if(active===id)active=(tabs[Math.max(0,i-1)]||tabs[0]).id;
  save();renderTabs();showActive();
}
function switchTo(id){active=id;save();renderTabs();showActive();}
function showActive(){
  if(active==='status'&&!sideOn()){document.body.classList.add('statusview');refreshStatus();return;}
  document.body.classList.remove('statusview');
  if(active==='status')active=firstBrowseId();   // docked sidebar: status is on the side, show a page
  var t=tabById(active);u.value=t?t.url:'';
  if(t&&t.url){s.textContent='';reimg();}else{v.removeAttribute('src');s.textContent='new tab — type a URL';}
  if(sideOn())refreshStatus();
}
// ---- navigation (streams progress, then re-images the active tab) ----
var ti=0,t0=0,pend=null,phase='loading';
function reimg(){v.src=T('/view.png?g='+Date.now());}
function tick(){s.textContent=phase+'\\u2026 '+((Date.now()-t0)/1000).toFixed(1)+'s';}
function startLoad(p){phase=p||'loading';document.body.classList.add('loading');t0=Date.now();tick();if(!ti)ti=setInterval(tick,200);}
function endLoad(){document.body.classList.remove('loading');if(ti){clearInterval(ti);ti=0;}}
function failLoad(m){endLoad();s.textContent=m;pend=null;}
v.onload=function(){endLoad();if(pend!==null){s.textContent=pend;pend=null;}refreshInspector();};
v.onerror=function(){if(v.getAttribute('src'))failLoad('could not load image');};
// A navigation belongs to the tab that STARTED it (tid), not whatever is active when
// its stream finishes — so switching tabs mid-load can't cross their state.  The
// result always updates the owning tab's data; the view only follows if it's showing.
function finalState(url,st,tid){var t=tabById(tid);if(t){if(url)t.url=url;t.title=(st||'').split('\\u2014')[0].trim();save();renderTabs();}
  if(tid===active){u.value=(t?t.url:'');pend=st;reimg();}}
function onLine(ln,tid){if(!ln)return;if(ln.charCodeAt(0)===1){var p=ln.slice(1).split('\\x1f');finalState(p[0]||'',p[1]||'',tid);}else if(tid===active){phase=ln;tick();}}
function stream(endpoint,p){
  var tid=active;                    // capture the owning tab id up front
  startLoad(p);
  fetch(T2(endpoint,tid)).then(function(r){
    if(!r.body||!r.body.getReader){return r.text().then(function(t){t.split('\\n').forEach(function(l){onLine(l,tid);});});}
    var rd=r.body.getReader(),dec=new TextDecoder(),buf='';
    return (function pump(){return rd.read().then(function(res){
      if(res.value){buf+=dec.decode(res.value,{stream:true});var a=buf.split('\\n');buf=a.pop();a.forEach(function(l){onLine(l,tid);});}
      if(res.done){if(buf)onLine(buf,tid);return;}
      return pump();});})();
  }).catch(function(){if(tid===active)failLoad('network error');});
}
document.getElementById('bar').onsubmit=function(e){e.preventDefault();var val=u.value.trim();if(!val||active==='status')return;stream('/go?url='+encodeURIComponent(val),'connecting');};
document.getElementById('flag').onclick=function(){fetch(T('/flag')).then(function(r){return r.text();}).then(function(t){s.textContent=t;}).catch(function(){s.textContent='network error';});};
v.onclick=function(e){
  if(document.body.classList.contains('loading'))return;
  var r=v.getBoundingClientRect();
  var x=Math.round((e.clientX-r.left)*(v.naturalWidth/r.width));
  var y=Math.round((e.clientY-r.top)*(v.naturalHeight/r.height));
  stream('/click?x='+x+'&y='+y,'opening');
};
// ---- engine status tab ----
function fmtUp(sec){var d=Math.floor(sec/86400),h=Math.floor(sec%86400/3600),m=Math.floor(sec%3600/60);return (d?d+'d ':'')+(d||h?h+'h ':'')+m+'m';}
function renderStatus(d){
  var h=isDesk()?('<span class=dock id=dock>'+(sideOn()?'\\u21e5 back to tab':'\\u21e4 dock to sidebar')+'</span>'):'';
  h+='<h4>engine</h4><div class=srow>uptime '+fmtUp(d.uptime)+' \\u00b7 heap '+d.heapMB+' MB \\u00b7 '+d.tabCount+' tab(s) \\u00b7 '+(d.rendering?('<span style=color:#6cf>rendering '+esc(host(d.rendering))+'</span>'):'idle')+'</div>';
  h+='<h4>open tabs ('+d.tabs.length+')</h4>';
  if(!d.tabs.length)h+='<div class=dim>none</div>';
  d.tabs.forEach(function(t){h+='<div class=srow>'+esc(t.title||host(t.url)||'(blank)')+(t.url?' <span class=dim>\\u2014 '+esc(t.url)+'</span>':'')+'</div>';});
  h+='<h4>recent errors ('+d.errors.length+')</h4>';
  if(!d.errors.length)h+='<div class=dim>none</div>';
  d.errors.forEach(function(e){h+='<div class=srow><span class=k>'+esc(e.kind)+'</span> '+esc(host(e.url))+' <span class=dim>'+e.ago+'s ago</span>'+(e.detail?'<div class=det>'+esc(e.detail)+'</div>':'')+'</div>';});
  stEl.innerHTML=h;
}
function refreshStatus(){fetch('/status.json').then(function(r){return r.json();}).then(renderStatus).catch(function(){stEl.textContent='status unavailable';});}
stEl.onclick=function(e){if(e.target.id==='dock')toggleSidebar();};
setInterval(function(){if(active==='status'||sideOn())refreshStatus();},2000);
// ---- inspector: performance timeline + network waterfall ----
function shorten(u){var m=/^https?:\\/\\/([^\\/]*)(.*)$/.exec(u);return m?(m[1]+(m[2]||'/')):u;}
var KC={document:'#6cf',css:'#9c6',js:'#fc6',image:'#c9f',text:'#8ad',other:'#999'};
function ibar(color,left,w){return '<span class=bar style=left:'+left.toFixed(2)+'%;width:'+Math.max(0.4,w).toFixed(2)+'%;background:'+color+'></span>';}
function sz(b){return b>=1024?Math.round(b/1024)+'k':b+'b';}
function renderInspector(d){
  var done=d.phases.length?d.phases[d.phases.length-1].at:0,ne=0;
  d.network.forEach(function(n){if(n.end>ne)ne=n.end;});
  var total=Math.max(done,ne,1),h='<span class=close>\\u2715</span>';
  h+='<div class=sum>'+(done/1000).toFixed(2)+'s \\u00b7 '+d.width+'\\u00d7'+d.contentHeight+' \\u00b7 '+d.elements+' els \\u00b7 '+d.links+' links \\u00b7 '+d.images+' imgs'+(d.jsError?' \\u00b7 <span style=color:#e66>JS: '+esc(d.jsError)+'</span>':'')+'</div>';
  h+='<h4>timeline</h4>';
  for(var i=0;i<d.phases.length-1;i++){var p=d.phases[i],dur=d.phases[i+1].at-p.at;
    h+='<div class=row><span class=lbl>'+esc(p.label)+'</span><span class=track>'+ibar('#6cf',100*p.at/total,100*dur/total)+'</span><span class=ms>'+dur+'ms</span></div>';}
  h+='<h4>network ('+d.network.length+')</h4>';
  if(d.byKind){var parts=[];for(var k in d.byKind){var b=d.byKind[k];parts.push('<span style=color:'+(KC[k]||'#999')+'>'+k+'</span> '+b.count+'\\u00d7 '+sz(b.bytes)+' '+b.ms+'ms');}h+='<div class=sum>'+parts.join('   ')+'</div>';}
  d.network.forEach(function(n){var dur=n.end-n.start,c=n.ok?(KC[n.kind]||'#6cf'):'#e66';
    h+='<div class=row><span class=lbl>'+esc(shorten(n.url))+'</span><span class=track>'+ibar(c,100*n.start/total,100*dur/total)+'</span><span class=ms>'+dur+'ms \\u00b7 '+sz(n.bytes)+'</span></div>';});
  document.getElementById('ip').innerHTML=h;
}
function refreshInspector(){if(document.body.classList.contains('showins')&&active!=='status')fetch(T('/inspect.json')).then(function(r){return r.json();}).then(renderInspector).catch(function(){});}
document.getElementById('insp').onclick=function(){if(document.body.classList.toggle('showins'))refreshInspector();};
document.getElementById('ip').onclick=function(e){if(e.target.className=='close')document.body.classList.remove('showins');};
// restore each browsing tab's current page from the server (a reload keeps sessions)
tabs.forEach(function(t){fetch('/go?tab='+encodeURIComponent(t.id)).then(function(r){return r.text();}).then(function(x){var p=x.split('\\n');if(p[0]){t.url=p[0];t.title=(p[1]||'').split('\\u2014')[0].trim();renderTabs();if(t.id===active)showActive();}}).catch(function(){});});
applyLayout();renderTabs();showActive();
</script></body></html>"))

(defun %jsesc (s)
  "Escape S for a single-quoted JavaScript string literal."
  (with-output-to-string (o)
    (loop for c across (or s "") do
      (case c (#\' (write-string "\\'" o)) (#\\ (write-string "\\\\" o))
              (#\Newline (write-string "\\n" o)) (#\Return)
              (t (write-char c o))))))

(defun %esc (s)
  (with-output-to-string (o)
    (loop for c across (or s "") do
      (case c (#\< (write-string "&lt;" o)) (#\> (write-string "&gt;" o))
              (#\& (write-string "&amp;" o)) (#\" (write-string "&quot;" o))
              (t (write-char c o))))))

;;; ---- minimal HTTP/1.1 -----------------------------------------------------
(defparameter +crlf+ (coerce '(#\Return #\Newline) 'string))

(defun read-head (stream)
  "Read the request head (to CRLFCRLF) as an ASCII string."
  (let ((buf (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (handler-case
        (loop for b = (read-byte stream nil nil) while b do
          (vector-push-extend b buf)
          (when (and (>= (fill-pointer buf) 4)
                     (= (aref buf (- (fill-pointer buf) 1)) 10)
                     (= (aref buf (- (fill-pointer buf) 3)) 10))
            (loop-finish)))
      (error () nil))
    (map 'string #'code-char buf)))

(defun url-decode (s)
  (with-output-to-string (o)
    (loop with i = 0 while (< i (length s)) do
      (let ((c (char s i)))
        (cond ((char= c #\+) (write-char #\Space o) (incf i))
              ((and (char= c #\%) (< (+ i 2) (length s)))
               (write-char (code-char (parse-integer s :start (1+ i) :end (+ i 3) :radix 16 :junk-allowed t)) o)
               (incf i 3))
              (t (write-char c o) (incf i)))))))

(defun query-param (query name)
  (loop for pair in (uiop:split-string (or query "") :separator "&")
        for eq = (position #\= pair)
        when (and eq (string= (subseq pair 0 eq) name))
          return (url-decode (subseq pair (1+ eq)))))

(defvar *accept-enc* "" "The current request's Accept-Encoding.")

(defun maybe-compress (bytes)
  "zstd-compress BYTES when it's worth it and the client accepts zstd — the raster PNGs
are uncompressed RGB, so this cuts a multi-MB page to a few hundred KB (~20x) in <1s.
Returns (values out encoding-or-nil)."
  (if (and (> (length bytes) 32768) (search "zstd" *accept-enc*))
      (handler-case (values (zstd-pure:compress bytes) "zstd")
        (error () (values bytes nil)))
      (values bytes nil)))

(defun send (stream status ctype body &key location)
  (let ((raw (if (stringp body) (sb-ext:string-to-octets body :external-format :utf-8) body)))
    ;; the PNG is already DEFLATE-compressed; a zstd pass over it barely shrinks it and
    ;; wastes a second — only compress text/other bodies.
    (multiple-value-bind (bytes enc) (if (search "image/" ctype) (values raw nil) (maybe-compress raw))
      (let ((h (with-output-to-string (o)
                 (format o "HTTP/1.1 ~a~a" status +crlf+)
                 (when location (format o "Location: ~a~a" location +crlf+))
                 (when enc (format o "Content-Encoding: ~a~a" enc +crlf+))
                 (format o "Content-Type: ~a~aContent-Length: ~a~aConnection: close~a~a"
                         ctype +crlf+ (length bytes) +crlf+ +crlf+ +crlf+))))
        (write-sequence (sb-ext:string-to-octets h :external-format :latin-1) stream)
        (write-sequence bytes stream)
        (finish-output stream)))))

;;; ---- streamed progress -----------------------------------------------------
;;; A render blocks the (single-threaded) server for seconds, so the client can't
;;; poll for status.  Instead /go and /click stream phase lines as they work — the
;;; response has no Content-Length, and the client reads it incrementally until the
;;; connection closes.  The final line is SOH url US status.
(defun send-stream-headers (stream ctype)
  (let ((h (format nil "HTTP/1.1 200 OK~aContent-Type: ~a~aCache-Control: no-store~aX-Accel-Buffering: no~aConnection: close~a~a"
                   +crlf+ ctype +crlf+ +crlf+ +crlf+ +crlf+ +crlf+ +crlf+)))
    (write-sequence (sb-ext:string-to-octets h :external-format :latin-1) stream)
    (finish-output stream)))

(defun stream-line (stream text)
  "Write one newline-terminated line and flush it to the socket immediately."
  (write-sequence (sb-ext:string-to-octets (concatenate 'string text (string #\Newline))
                                            :external-format :utf-8)
                  stream)
  (finish-output stream))

(defparameter +phase-labels+
  '((:resolving . "resolving") (:connecting . "connecting") (:securing . "securing connection")
    (:verifying . "verifying certificate") (:redirecting . "following redirect")
    (:downloading . "downloading") (:decoding . "decoding")
    (:fetching . "fetching") (:parsing . "parsing") (:loading . "loading resources")
    (:scripting . "running scripts") (:images . "loading images")
    (:cascade . "matching styles") (:layout . "laying out") (:painting . "painting")
    (:rendering . "rendering") (:encoding . "encoding image")))

(defun phase-label (phase detail)
  (let ((base (or (cdr (assoc phase +phase-labels+)) (string-downcase (symbol-name phase)))))
    (if (and detail (plusp (length detail))) (format nil "~a ~a" base detail) base)))

(defun state-final-line (tab)
  "The terminal streamed message: SOH, the page URL, US, single-line status."
  (format nil "~c~a~c~a" (code-char 1)
          (or (and (tab-page tab) (l:page-url (tab-page tab))) "")
          (code-char 31)
          (substitute #\Space #\Newline (or (tab-status tab) ""))))

(defun %json-str (s)
  "S as a JSON string literal."
  (with-output-to-string (o)
    (write-char #\" o)
    (loop for c across (or s "") do
      (case c
        (#\" (write-string "\\\"" o))
        (#\\ (write-string "\\\\" o))
        (#\Newline (write-string "\\n" o))
        (#\Return (write-string "\\r" o))
        (#\Tab (write-string "\\t" o))
        (t (if (< (char-code c) 32) (format o "\\u~4,'0x" (char-code c)) (write-char c o)))))
    (write-char #\" o)))

(defun inspect-json (tab)
  "The last navigation's timeline, network log and page metrics as JSON."
  (let* ((pg (tab-page tab)) (doc (and pg (l:page-doc pg))))
    (multiple-value-bind (els txt) (if doc (l:dom-node-counts doc) (values 0 0))
      (with-output-to-string (o)
        (flet ((kv (k v) (format o "~a:~a," (%json-str k) v)))
          (write-char #\{ o)
          (kv "url" (%json-str (or (and pg (l:page-url pg)) "")))
          (kv "title" (%json-str (or (and pg (l:page-title pg)) "")))
          (kv "jsError" (if (and pg (l:page-js-error pg)) (%json-str (l:page-js-error pg)) "null"))
          (kv "width" (if pg (l:page-width pg) 0))
          (kv "contentHeight" (if pg (l:page-content-height pg) 0))
          (kv "elements" els)
          (kv "textNodes" txt)
          (kv "images" (if doc (length (weft.css:query-select-all doc "img")) 0))
          (kv "links" (if doc (length (weft.css:query-select-all doc "a")) 0))
          (format o "~a:[" (%json-str "phases"))
          (loop for (phase . ms) in (tab-timeline tab) for i from 0
                do (when (plusp i) (write-char #\, o))
                   (format o "{~a:~a,~a:~d}"
                           (%json-str "label") (%json-str (phase-label phase nil))
                           (%json-str "at") ms))
          (let ((net (reverse l:*net-log*)))
            (format o "],~a:[" (%json-str "network"))
            (loop for (url start end bytes ok kind) in net for i from 0
                  do (when (plusp i) (write-char #\, o))
                     (format o "{~a:~a,~a:~d,~a:~d,~a:~d,~a:~a,~a:~a}"
                             (%json-str "url") (%json-str url)
                             (%json-str "start") start (%json-str "end") end
                             (%json-str "bytes") (or bytes 0)
                             (%json-str "ok") (if ok "true" "false")
                             (%json-str "kind") (%json-str (string-downcase (symbol-name kind)))))
            ;; per-kind rollup: count, total bytes, summed transfer ms
            (format o "],~a:{" (%json-str "byKind"))
            (let ((agg '()))
              (dolist (e net)
                (destructuring-bind (url start end bytes ok kind) e
                  (declare (ignore url ok))
                  (let ((cell (assoc kind agg)))
                    (unless cell (setf cell (list kind 0 0 0)) (push cell agg))
                    (incf (second cell))
                    (incf (third cell) (or bytes 0))
                    (incf (fourth cell) (- end start)))))
              (loop for (kind cnt kb ms) in (nreverse agg) for i from 0
                    do (when (plusp i) (write-char #\, o))
                       (format o "~a:{~a:~d,~a:~d,~a:~d}"
                               (%json-str (string-downcase (symbol-name kind)))
                               (%json-str "count") cnt (%json-str "bytes") kb (%json-str "ms") ms)))
            (format o "}}")))))))

(defun status-json ()
  "Engine status for the pinned status tab: uptime, the active render, open tabs
   (id/url/title) and recent errors."
  (let ((tabs (sb-thread:with-mutex (*tabs-lock*)
                (loop for id being the hash-keys of *tabs* using (hash-value tb)
                      collect (list id (or (and (tab-page tb) (l:page-url (tab-page tb))) "")
                                    (or (and (tab-page tb) (l:page-title (tab-page tb))) "")))))
        (errs (sb-thread:with-mutex (*recent-lock*)
                (subseq *recent-errors* 0 (min 20 (length *recent-errors*)))))
        (now (get-universal-time)))
    (with-output-to-string (o)
      (flet ((kv (k v) (format o "~a:~a," (%json-str k) v)))
        (write-char #\{ o)
        (kv "uptime" (- now *start-time*))
        (kv "rendering" (if *rendering* (%json-str *rendering*) "null"))
        (kv "heapMB" (or (ignore-errors (round (/ (sb-kernel:dynamic-usage) 1048576))) 0))
        (kv "tabCount" (length tabs))
        (format o "~a:[" (%json-str "tabs"))
        (loop for (id url title) in tabs for i from 0
              do (when (plusp i) (write-char #\, o))
                 (format o "{~a:~a,~a:~a,~a:~a}" (%json-str "id") (%json-str id)
                         (%json-str "url") (%json-str url) (%json-str "title") (%json-str title)))
        (format o "],~a:[" (%json-str "errors"))
        (loop for (ut kind url detail) in errs for i from 0
              do (when (plusp i) (write-char #\, o))
                 (format o "{~a:~d,~a:~a,~a:~a,~a:~a}"
                         (%json-str "ago") (- now ut)
                         (%json-str "kind") (%json-str kind)
                         (%json-str "url") (%json-str url)
                         (%json-str "detail") (%json-str detail)))
        (format o "]}")))))

(defun run-streamed (stream tab thunk)
  "Stream THUNK's progress: emit each phase, force the PNG encode (so the follow-up
   /view.png is instant), then TAB's final state line.  Also captures the phase
   timeline and per-fetch network log for the inspector."
  (send-stream-headers stream "text/plain; charset=utf-8")
  (l:net-log-reset)
  ;; The loom pipeline scopes the lower-layer hooks (fetch/TLS around the main
  ;; document fetch, render around the paint) to REPORT-PROGRESS, which forwards
  ;; here.  Binding the loom hook alone thus surfaces every phase without the many
  ;; subresource/image fetches streaming their own network detail.
  (let* ((marks '())
         (emit (lambda (phase detail)
                 ;; Timeline marks are keyed on the phase KEYWORD and collapse
                 ;; consecutive repeats, so the many :downloading byte-counter ticks
                 ;; become one bar; the live stream still shows every tick.
                 (unless (and marks (eq (caar marks) phase))
                   (push (cons phase (l:nav-elapsed-ms)) marks))
                 (ignore-errors (stream-line stream (phase-label phase detail)))))
         (l:*progress* emit))
    (ignore-errors (funcall thunk))
    (funcall emit :encoding nil)
    (ignore-errors (tab-png-bytes tab))
    (push (cons :done (l:nav-elapsed-ms)) marks)
    (setf (tab-timeline tab) (nreverse marks)))
  (ignore-errors (stream-line stream (state-final-line tab))))

(defun header-value (head name)
  "Value of the NAME header (case-insensitive) in the raw request HEAD, or \"\"."
  (let* ((lower (string-downcase head))
         (key (concatenate 'string (string-downcase name) ":"))
         (p (search key lower)))
    (if p
        (let* ((start (+ p (length key)))
               (end (or (search +crlf+ head :start2 start) (length head))))
          (string-trim " " (subseq head start end)))
        "")))

(defun apply-tab-width (tab head)
  "Render TAB at the requesting client's own viewport width (its `vw` cookie),
   defaulting to 1024 when absent.  Relays out this tab's page when the width
   changes (mutating it, hence under *RENDER-LOCK*)."
  (let* ((cookie (header-value head "cookie"))
         (p (search "vw=" cookie))
         (vw (and p (ignore-errors (parse-integer cookie :start (+ p 3) :junk-allowed t))))
         (desired (if (and vw (<= 280 vw 2400)) vw 1024)))
    (when (/= desired (tab-width tab))
      (sb-thread:with-mutex (*render-lock*)
        (when (/= desired (tab-width tab))
          (setf (tab-width tab) desired)
          (when (tab-page tab)
            (ignore-errors (l:relayout (tab-page tab) (tab-width tab)) (incf (tab-gen tab)))))))))

(defun handle (stream)
  (let* ((head (read-head stream))
         (*accept-enc* (header-value head "accept-encoding"))
         (line (subseq head 0 (or (search +crlf+ head) (length head))))
         (parts (uiop:split-string line :separator " "))
         (target (or (second parts) "/"))
         (qpos (position #\? target))
         (path (if qpos (subseq target 0 qpos) target))
         (query (and qpos (subseq target (1+ qpos))))
         ;; the client mints a tab id per browser tab (sessionStorage); each tab has
         ;; its own page, status and browsing context.  "0" = a client without one.
         (tab (tab-for (query-param query "tab"))))
    (apply-tab-width tab head)
    (cond
      ((string= path "/view.png")
       (let ((png (tab-png-bytes tab)))
         (if png (send stream "200 OK" "image/png" png)
             (send stream "404 Not Found" "text/plain" "no page"))))
      ((string= path "/go")
       ;; A typed/address-bar URL opens a FRESH browsing context (:fresh t) — distinct
       ;; cookies, so this visit isn't tied to an earlier one.  Stream progress, then
       ;; the final state; the client reflects url/status and history lives in the hash.
       (let ((url (query-param query "url")))
         (if (and url (plusp (length url)))
             (sb-thread:with-mutex (*render-lock*)   ; one render at a time (shared caches)
               (setf *rendering* url)
               (unwind-protect
                   (run-streamed stream tab (lambda () (navigate-tab tab url :fresh t)))
                 (setf *rendering* nil)))
             (send stream "200 OK" "text/plain; charset=utf-8" (tab-state-text tab)))))
      ((string= path "/inspect.json")
       (send stream "200 OK" "application/json; charset=utf-8" (inspect-json tab)))
      ((string= path "/status.json")
       (send stream "200 OK" "application/json; charset=utf-8" (status-json)))
      ((string= path "/close")
       (drop-tab (query-param query "tab"))
       (send stream "200 OK" "text/plain; charset=utf-8" "closed"))
      ((string= path "/click")
       ;; a click may follow a link — the page's on-navigate re-navigates THIS tab in
       ;; the SAME context (:fresh nil), so a login/session carries across the click.
       (let ((x (ignore-errors (parse-integer (or (query-param query "x") "0"))))
             (y (ignore-errors (parse-integer (or (query-param query "y") "0")))))
         (sb-thread:with-mutex (*render-lock*)
           (setf *rendering* (or (and (tab-page tab) (l:page-url (tab-page tab))) "click"))
           (unwind-protect
               (run-streamed stream tab (lambda () (when (and x y) (click-tab tab x y))))
             (setf *rendering* nil)))))
      ((string= path "/flag")
       (let ((u (or (and (tab-page tab) (l:page-url (tab-page tab))) (query-param query "url"))))
         (when (and u (plusp (length u)))
           (log-error "flag" u (format nil "flagged; ~a" (tab-status tab)))
           (setf (tab-status tab) (format nil "flagged for review: ~a" u))))
       (send stream "200 OK" "text/plain; charset=utf-8" (or (tab-status tab) "")))
      ((string= path "/")
       (send stream "200 OK" "text/html; charset=utf-8" (client-html)))
      (t (send stream "404 Not Found" "text/plain" "not found")))))

(defparameter *errlog-path*
  (merge-pathnames "loom-errors.db"
                   (uiop:pathname-parent-directory-pathname (directory-namestring *load-truename*))))

(defun serve (&optional (port 8080))
  (errlog-open *errlog-path*)
  (let ((s (make-instance 'sock:inet-socket :type :stream :protocol :tcp)))
    (setf (sock:sockopt-reuse-address s) t)
    (sock:socket-bind s #(0 0 0 0) port)
    (sock:socket-listen s 16)
    (format t "~&weft-serve listening on http://0.0.0.0:~a/~%" port)
    (finish-output)
    (unwind-protect
        (loop
          ;; Nothing a single connection does (reset, malformed request, a render
          ;; that throws) may take the server down — wrap the whole accept.
          (handler-case
              (let ((c (sock:socket-accept s)))
                ;; Serve each connection on its own thread: a multi-second render
                ;; holds only *RENDER-LOCK*, so concurrent /view.png, / and status
                ;; requests are answered against the current page meanwhile.
                (sb-thread:make-thread
                 (lambda ()
                   (unwind-protect
                       (let ((stream (sock:socket-make-stream c :element-type '(unsigned-byte 8)
                                                                :input t :output t)))
                         (handler-case (handle stream)
                           (serious-condition (e) (format t "~&[req] ~a~%" e) (log-error "request" nil e)))
                         (ignore-errors (finish-output stream))
                         (ignore-errors (close stream)))
                     (ignore-errors (sock:socket-close c))))
                 :name "req"))
            (serious-condition (e) (format t "~&[accept] ~a~%" e) (log-error "accept" nil e))))
      (ignore-errors (sock:socket-close s)))))

(let ((port (or (loop for a in (rest sb-ext:*posix-argv*)
                      for n = (ignore-errors (parse-integer a))
                      when n return n)
                8899)))
  (serve port))
