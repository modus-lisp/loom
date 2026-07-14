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
  (:local-nicknames (#:l #:loom) (#:r #:weft.render) (#:sock #:sb-bsd-sockets)))
(in-package #:weft-serve)

(defvar *page* nil "The single live browsing session (MVP: one shared page).")
(defvar *status* "type a URL and press Go")
(defvar *gen* 0 "Bumped every render so the client's <img> cache-busts.")
(defvar *render-width* 1024
  "Layout width pages are rendered at — driven by the client's viewport width (a `vw`
   cookie) so mobile gets a readable device-width render instead of a shrunk 1024px page.")

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
  (when *errdb* (ignore-errors (%sqlite-exec *errdb* sql (%nsap) (%nsap) (%nsap)))))

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
  (ignore-errors
    (format *error-output* "~&[errlog] ~a ~a: ~a~%" kind (or url "") detail)
    (errlog-exec (format nil "INSERT INTO errors (kind,url,detail) VALUES (~a,~a,~a)"
                         (%sqlq (string kind)) (%sqlq (or url ""))
                         (%sqlq (princ-to-string detail))))))

;;; ---- browsing -------------------------------------------------------------
(defun follow (target)
  "on-navigate callback: load TARGET as the new page (keeps the callback)."
  (navigate target))

(defun navigate (url)
  ;; Render at the client's own width; height comes from content flow (reader view).
  ;; The only exception is a page that clips at the root (a viewport-model page like
  ;; Acid2): it gets a fixed viewport whose height is a 4:3 slice of that width.
  (let ((vph (max 480 (round (* *render-width* 3/4)))))
   (handler-case
      (let ((pg (if (or (search "://" url) (eql 0 (search "http" url)))
                    (l:load-url url :width *render-width* :viewport-height vph)
                    (l:load-url (concatenate 'string "https://" url)
                                :width *render-width* :viewport-height vph))))
        (setf (l:page-on-navigate pg) (lambda (p tgt) (declare (ignore p)) (follow tgt)))
        ;; the page rendered, but record a non-fatal script error for observability
        (when (l:page-js-error pg) (log-error "script" url (l:page-js-error pg)))
        (setf *page* pg
              *status* (format nil "~a  —  ~a" (or (l:page-title pg) "") (or (l:page-url pg) url)))
        (incf *gen*))
    (error (e)
      (log-error "navigate" url e)
      (setf *status* (format nil "couldn't load ~a  (~a)" url e))))))

(defvar *png-cache* nil)
(defvar *png-cache-gen* -1)
(defun page-png-bytes ()
  "Encode the current page canvas to a compressed PNG in memory, cached per *GEN* — the
   DEFLATE encode costs seconds on a tall page, so encode once per navigation, not per
   /view.png request.  No temp file, so a full disk can't break rendering."
  (when *page*
    (unless (and *png-cache* (= *png-cache-gen* *gen*))
      (setf *png-cache* (coerce (r:canvas->png (l:page-canvas *page*)) '(simple-array (unsigned-byte 8) (*)))
            *png-cache-gen* *gen*))
    *png-cache*))

(defun state-text ()
  "The client-facing page state: current URL on the first line, status on the second."
  (format nil "~a~%~a" (or (and *page* (l:page-url *page*)) "") (or *status* "")))

(defun handle-click (x y)
  "Route a viewport click at (X,Y) — the raster is the full page, scroll-y stays 0,
   so image coords are page coords.  mouse-release follows links + fires DOM click."
  (when *page*
    (l:mouse-press *page* x y)
    (l:mouse-release *page* x y)   ; may replace *page* via on-navigate
    (incf *gen*)))

;;; ---- the client -----------------------------------------------------------
(defun client-html ()
  ;; Navigation is driven by the URL hash so the browser Back/Forward buttons work:
  ;; the address bar and link clicks set location.hash, and a hashchange handler (which
  ;; also fires on Back/Forward) renders that URL.  The raster/status are fetched, not
  ;; embedded, so a hash change is one history entry without a full page reload.
  (format nil "<!doctype html><html><head><meta charset=\"utf-8\"><title>weft</title>
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
<script>document.cookie='vw='+Math.round(window.innerWidth)+';path=/;max-age=31536000';</script>
<style>body{margin:0;font:14px sans-serif;background:#222;color:#eee}
#bar{position:sticky;top:0;background:#333;padding:6px;display:flex;gap:6px;z-index:9}
#bar input{flex:1;padding:5px;font-size:16px;min-width:0}#bar button{padding:5px 12px}
#s{padding:4px 8px;color:#9c9;font-size:12px}#v{display:block;width:100%;height:auto;background:#fff}
#p{position:fixed;top:0;left:0;right:0;height:3px;overflow:hidden;z-index:20;display:none}
body.loading #p{display:block}
#p::before{content:'';position:absolute;height:100%;width:40%;background:#6cf;box-shadow:0 0 8px #6cf;animation:sl 1.1s ease-in-out infinite}
@keyframes sl{0%{left:-42%}100%{left:100%}}
body.loading #v{opacity:.5;transition:opacity .15s}
body.loading #s{color:#6cf}
#ip{display:none;position:fixed;left:0;right:0;bottom:0;z-index:30;padding:8px 10px;background:rgba(20,20,20,.96);color:#ccc;font:11px/1.5 ui-monospace,monospace;max-height:62vh;overflow:auto;border-top:1px solid #444;box-shadow:0 -3px 14px rgba(0,0,0,.6)}
body.showins #ip{display:block}
#ip .close{position:sticky;top:0;float:right;color:#9c9;cursor:pointer;padding:0 4px}
#ip h4{margin:9px 0 3px;color:#6cf;font-weight:normal;border-bottom:1px solid #333;padding-bottom:2px}
#ip .sum{color:#9c9;margin-bottom:6px}
#ip .row{display:flex;align-items:center;gap:6px;height:16px}
#ip .lbl{width:160px;flex:none;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}
#ip .track{flex:1;position:relative;height:9px;background:#242424;border-radius:2px}
#ip .bar{position:absolute;height:100%;background:#6cf;border-radius:2px}
#ip .barf{position:absolute;height:100%;background:#e66;border-radius:2px}
#ip .ms{width:96px;flex:none;text-align:right;color:#8a8}</style></head>
<body><div id=p></div><form id=bar novalidate><input id=u placeholder=\"https://…\" type=\"url\" inputmode=\"url\" autocapitalize=\"none\" autocorrect=\"off\" spellcheck=\"false\">
<button type=submit>Go</button><button type=button id=flag title=\"flag this page as broken\">&#9873;</button><button type=button id=insp title=\"inspector\">&#9201;</button></form><div id=s></div>
<img id=v>
<div id=ip></div>
<script>
var v=document.getElementById('v'),u=document.getElementById('u'),s=document.getElementById('s');
var serverUrl='~a';                 // the URL the server currently has rendered
u.value=serverUrl; s.textContent='~a';
var ti=0,t0=0,pend=null,phase='loading';   // timer handle, load start (ms), status to show once the image lands, current phase
function hurl(){return decodeURIComponent(location.hash.slice(1));}
function reimg(){v.src='/view.png?g='+Date.now();}
// A navigation streams phase lines (fetching, parsing, rendering, …) then a final
// line (SOH url US status).  Show each phase live with an elapsed counter, and hold
// the busy state from the tap until the freshly-encoded image actually loads.
function tick(){s.textContent=phase+'… '+((Date.now()-t0)/1000).toFixed(1)+'s';}
function startLoad(p){phase=p||'loading';document.body.classList.add('loading');t0=Date.now();tick();if(!ti)ti=setInterval(tick,200);}
function endLoad(){document.body.classList.remove('loading');if(ti){clearInterval(ti);ti=0;}}
function failLoad(m){endLoad();s.textContent=m;pend=null;}
v.onload=function(){endLoad();if(pend!==null){s.textContent=pend;pend=null;}refreshInspector();};
v.onerror=function(){failLoad('could not load image');};
function finalState(url,st){
  if(url){serverUrl=url;u.value=url;pend=st;var enc=encodeURIComponent(url);
    if(location.hash.slice(1)!==enc){location.hash=enc;return;}}   // hash change -> render() reimgs
  else pend=st;
  reimg();
}
function onLine(ln){
  if(!ln)return;
  if(ln.charCodeAt(0)===1){var p=ln.slice(1).split('\\x1f');finalState(p[0]||'',p[1]||'');}
  else{phase=ln;tick();}
}
function stream(endpoint,p){
  startLoad(p);
  fetch(endpoint).then(function(r){
    if(!r.body||!r.body.getReader){return r.text().then(function(t){t.split('\\n').forEach(onLine);});}
    var rd=r.body.getReader(),dec=new TextDecoder(),buf='';
    return (function pump(){return rd.read().then(function(res){
      if(res.value){buf+=dec.decode(res.value,{stream:true});var a=buf.split('\\n');buf=a.pop();a.forEach(onLine);}
      if(res.done){if(buf)onLine(buf);return;}
      return pump();
    });})();
  }).catch(function(){failLoad('network error');});
}
function render(){var h=hurl();if(!h)return;u.value=h;if(h===serverUrl){reimg();return;}stream('/go?url='+encodeURIComponent(h),'connecting');}
window.addEventListener('hashchange',render);
document.getElementById('bar').onsubmit=function(e){e.preventDefault();var t=u.value.trim();if(!t)return;var enc=encodeURIComponent(t);if(location.hash.slice(1)===enc)render();else location.hash=enc;};
document.getElementById('flag').onclick=function(){fetch('/flag').then(function(r){return r.text();}).then(function(t){s.textContent=t;}).catch(function(){s.textContent='network error';});};
v.onclick=function(e){
  if(document.body.classList.contains('loading'))return;   // busy — ignore taps rather than queue them
  var r=v.getBoundingClientRect();
  var x=Math.round((e.clientX-r.left)*(v.naturalWidth/r.width));
  var y=Math.round((e.clientY-r.top)*(v.naturalHeight/r.height));
  stream('/click?x='+x+'&y='+y,'opening');
};
// ---- inspector: performance timeline + network waterfall ----
function esc(s){return String(s).replace(/[&<>]/g,function(c){return c=='&'?'&amp;':c=='<'?'&lt;':'&gt;';});}
function shorten(u){var m=/^https?:\\/\\/([^\\/]*)(.*)$/.exec(u);return m?(m[1]+(m[2]||'/')):u;}
var KC={document:'#6cf',css:'#9c6',js:'#fc6',image:'#c9f',text:'#8ad',other:'#999'};
function ibar(color,left,w){return '<span class=bar style=left:'+left.toFixed(2)+'%;width:'+Math.max(0.4,w).toFixed(2)+'%;background:'+color+'></span>';}
function sz(b){return b>=1024?Math.round(b/1024)+'k':b+'b';}
function renderInspector(d){
  var done=d.phases.length?d.phases[d.phases.length-1].at:0,ne=0;
  d.network.forEach(function(n){if(n.end>ne)ne=n.end;});
  var total=Math.max(done,ne,1),h='<span class=close>✕</span>';
  h+='<div class=sum>'+(done/1000).toFixed(2)+'s · '+d.width+'×'+d.contentHeight+' · '+d.elements+' els · '+d.links+' links · '+d.images+' imgs'+(d.jsError?' · <span style=color:#e66>JS: '+esc(d.jsError)+'</span>':'')+'</div>';
  h+='<h4>timeline</h4>';
  for(var i=0;i<d.phases.length-1;i++){var p=d.phases[i],dur=d.phases[i+1].at-p.at;
    h+='<div class=row><span class=lbl>'+esc(p.label)+'</span><span class=track>'+ibar('#6cf',100*p.at/total,100*dur/total)+'</span><span class=ms>'+dur+'ms</span></div>';}
  h+='<h4>network ('+d.network.length+')</h4>';
  if(d.byKind){var parts=[];for(var k in d.byKind){var b=d.byKind[k];parts.push('<span style=color:'+(KC[k]||'#999')+'>'+k+'</span> '+b.count+'× '+sz(b.bytes)+' '+b.ms+'ms');}h+='<div class=sum>'+parts.join('   ')+'</div>';}
  d.network.forEach(function(n){var dur=n.end-n.start,c=n.ok?(KC[n.kind]||'#6cf'):'#e66';
    h+='<div class=row><span class=lbl>'+esc(shorten(n.url))+'</span><span class=track>'+ibar(c,100*n.start/total,100*dur/total)+'</span><span class=ms>'+dur+'ms · '+sz(n.bytes)+'</span></div>';});
  document.getElementById('ip').innerHTML=h;
}
function refreshInspector(){if(document.body.classList.contains('showins'))fetch('/inspect.json').then(function(r){return r.json();}).then(renderInspector).catch(function(){});}
document.getElementById('insp').onclick=function(){if(document.body.classList.toggle('showins'))refreshInspector();};
document.getElementById('ip').onclick=function(e){if(e.target.className=='close')document.body.classList.remove('showins');};
if(hurl())render(); else if(serverUrl){pend=s.textContent;startLoad('loading');reimg();location.hash=encodeURIComponent(serverUrl);}
</script></body></html>"
            (%jsesc (or (and *page* (l:page-url *page*)) ""))
            (%jsesc *status*)))

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

(defun state-final-line ()
  "The terminal streamed message: SOH, the page URL, US, single-line status."
  (format nil "~c~a~c~a" (code-char 1)
          (or (and *page* (l:page-url *page*)) "")
          (code-char 31)
          (substitute #\Space #\Newline (or *status* ""))))

(defvar *timeline* nil
  "The last navigation's phase marks: a list of (LABEL . ELAPSED-MS).  Paired with
   L:*NET-LOG* it drives the inspector's performance + network waterfalls.")

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

(defun inspect-json ()
  "The last navigation's timeline, network log and page metrics as JSON."
  (let* ((pg *page*) (doc (and pg (l:page-doc pg))))
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
          (loop for (phase . ms) in *timeline* for i from 0
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

(defun run-streamed (stream thunk)
  "Stream THUNK's progress: emit each phase, force the PNG encode (so the follow-up
   /view.png is instant), then the final state line.  Also captures the phase
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
    (ignore-errors (page-png-bytes))
    (push (cons :done (l:nav-elapsed-ms)) marks)
    (setf *timeline* (nreverse marks)))
  (ignore-errors (stream-line stream (state-final-line))))

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

(defun apply-viewport-width (head)
  "Render at the requesting client's own viewport width (its `vw` cookie), defaulting to
   1024 when the cookie is absent so a cookieless request never inherits a prior
   visitor's width.  Relays out the shared page when the width changes."
  (let* ((cookie (header-value head "cookie"))
         (p (search "vw=" cookie))
         (vw (and p (ignore-errors (parse-integer cookie :start (+ p 3) :junk-allowed t))))
         (desired (if (and vw (<= 280 vw 2400)) vw 1024)))
    (when (/= desired *render-width*)
      (setf *render-width* desired)
      (when *page* (ignore-errors (l:relayout *page* *render-width*) (incf *gen*))))))

(defun handle (stream)
  (let* ((head (read-head stream))
         (*accept-enc* (header-value head "accept-encoding"))
         (line (subseq head 0 (or (search +crlf+ head) (length head))))
         (parts (uiop:split-string line :separator " "))
         (target (or (second parts) "/"))
         (qpos (position #\? target))
         (path (if qpos (subseq target 0 qpos) target))
         (query (and qpos (subseq target (1+ qpos)))))
    (apply-viewport-width head)
    (cond
      ((string= path "/view.png")
       (let ((png (page-png-bytes)))
         (if png (send stream "200 OK" "image/png" png)
             (send stream "404 Not Found" "text/plain" "no page"))))
      ((string= path "/go")
       ;; stream navigation progress (fetching, parsing, rendering, …) then the final
       ;; page state; the client reflects url/status and history lives in the hash.
       (let ((url (query-param query "url")))
         (if (and url (plusp (length url)))
             (run-streamed stream (lambda () (navigate url)))
             (send stream "200 OK" "text/plain; charset=utf-8" (state-text)))))
      ((string= path "/inspect.json")
       ;; the last navigation's performance timeline, network log and page metrics
       (send stream "200 OK" "application/json; charset=utf-8" (inspect-json)))
      ((string= path "/click")
       ;; a click may follow a link (page-url changes) — stream its progress like /go
       (let ((x (ignore-errors (parse-integer (or (query-param query "x") "0"))))
             (y (ignore-errors (parse-integer (or (query-param query "y") "0")))))
         (run-streamed stream (lambda () (when (and x y) (handle-click x y))))))
      ((string= path "/flag")
       ;; record the currently-shown page for later diagnosis — catches renders that are
       ;; wrong but didn't error (unstyled, missing images), which nothing else logs.
       (let ((u (or (and *page* (l:page-url *page*)) (query-param query "url"))))
         (when (and u (plusp (length u)))
           (log-error "flag" u (format nil "flagged; ~a" *status*))
           (setf *status* (format nil "flagged for review: ~a" u))))
       (send stream "200 OK" "text/plain; charset=utf-8" (or *status* "")))
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
          ;; that throws) may take the server down — wrap the whole accept/serve/close.
          (handler-case
              (let ((c (sock:socket-accept s)))
                (unwind-protect
                    (let ((stream (sock:socket-make-stream c :element-type '(unsigned-byte 8)
                                                             :input t :output t)))
                      (handler-case (handle stream)
                        (serious-condition (e) (format t "~&[req] ~a~%" e) (log-error "request" nil e)))
                      (ignore-errors (finish-output stream))
                      (ignore-errors (close stream)))
                  (ignore-errors (sock:socket-close c))))
            (serious-condition (e) (format t "~&[accept] ~a~%" e) (log-error "accept" nil e))))
      (ignore-errors (sock:socket-close s)))))

(let ((port (or (loop for a in (rest sb-ext:*posix-argv*)
                      for n = (ignore-errors (parse-integer a))
                      when n return n)
                8899)))
  (serve port))
