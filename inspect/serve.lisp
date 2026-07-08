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
  (handler-case
      (let ((pg (if (or (search "://" url) (eql 0 (search "http" url)))
                    (l:load-url url :width *render-width* :viewport-height 100000)
                    (l:load-url (concatenate 'string "https://" url)
                                :width *render-width* :viewport-height 100000))))
        (setf (l:page-on-navigate pg) (lambda (p tgt) (declare (ignore p)) (follow tgt)))
        ;; the page rendered, but record a non-fatal script error for observability
        (when (l:page-js-error pg) (log-error "script" url (l:page-js-error pg)))
        (setf *page* pg
              *status* (format nil "~a  —  ~a" (or (l:page-title pg) "") (or (l:page-url pg) url)))
        (incf *gen*))
    (error (e)
      (log-error "navigate" url e)
      (setf *status* (format nil "couldn't load ~a  (~a)" url e)))))

(defun page-png-bytes ()
  "Encode the current page canvas to PNG bytes in memory — no temp file, so a full
   disk can't break rendering."
  (when *page*
    (coerce (r:canvas->png (l:page-canvas *page*))
            '(simple-array (unsigned-byte 8) (*)))))

(defun handle-click (x y)
  "Route a viewport click at (X,Y) — the raster is the full page, scroll-y stays 0,
   so image coords are page coords.  mouse-release follows links + fires DOM click."
  (when *page*
    (l:mouse-press *page* x y)
    (l:mouse-release *page* x y)   ; may replace *page* via on-navigate
    (incf *gen*)))

;;; ---- the client -----------------------------------------------------------
(defun client-html ()
  (let ((h (if *page* (r:canvas-height (l:page-canvas *page*)) 0)))
    (format nil "<!doctype html><html><head><meta charset=\"utf-8\"><title>weft</title>
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
<script>document.cookie='vw='+Math.round(window.innerWidth)+';path=/;max-age=31536000';</script>
<style>body{margin:0;font:14px sans-serif;background:#222;color:#eee}
#bar{position:sticky;top:0;background:#333;padding:6px;display:flex;gap:6px;z-index:9}
#bar input{flex:1;padding:5px;font-size:16px;min-width:0}#bar button{padding:5px 12px}
#s{padding:4px 8px;color:#9c9;font-size:12px}#v{display:block;width:100%;height:auto;background:#fff}</style></head>
<body><form id=bar action=\"/go\" novalidate><input name=url value=\"~a\" placeholder=\"https://…\" type=\"url\" inputmode=\"url\" autocapitalize=\"none\" autocorrect=\"off\" spellcheck=\"false\">
<button>Go</button><button formaction=\"/flag\" formnovalidate title=\"flag this page as broken\">&#9873;</button></form><div id=s>~a</div>
<img id=v src=\"/view.png?g=~a\">
<script>
var v=document.getElementById('v');
// Map a tap to raster (page) coordinates: getBoundingClientRect and clientX are both in
// layout-viewport CSS px, so naturalWidth/rect.width scales correctly even under pinch-zoom.
v.onclick=function(e){
  var r=v.getBoundingClientRect();
  var x=Math.round((e.clientX-r.left)*(v.naturalWidth/r.width));
  var y=Math.round((e.clientY-r.top)*(v.naturalHeight/r.height));
  location='/click?x='+x+'&y='+y;
};
</script></body></html>"
            (%esc (or (and *page* (l:page-url *page*)) "")) (%esc *status*) *gen* h)))

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
    (multiple-value-bind (bytes enc) (maybe-compress raw)
      (let ((h (with-output-to-string (o)
                 (format o "HTTP/1.1 ~a~a" status +crlf+)
                 (when location (format o "Location: ~a~a" location +crlf+))
                 (when enc (format o "Content-Encoding: ~a~a" enc +crlf+))
                 (format o "Content-Type: ~a~aContent-Length: ~a~aConnection: close~a~a"
                         ctype +crlf+ (length bytes) +crlf+ +crlf+ +crlf+))))
        (write-sequence (sb-ext:string-to-octets h :external-format :latin-1) stream)
        (write-sequence bytes stream)
        (finish-output stream)))))

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
       (let ((url (query-param query "url")))
         (when (and url (plusp (length url))) (navigate url)))
       (send stream "302 Found" "text/plain" "" :location "/"))
      ((string= path "/click")
       (let ((x (ignore-errors (parse-integer (or (query-param query "x") "0"))))
             (y (ignore-errors (parse-integer (or (query-param query "y") "0")))))
         (when (and x y) (handle-click x y)))
       (send stream "302 Found" "text/plain" "" :location "/"))
      ((string= path "/flag")
       ;; record the currently-shown page for later diagnosis — catches renders that are
       ;; wrong but didn't error (unstyled, missing images), which nothing else logs.
       (let ((u (or (and *page* (l:page-url *page*)) (query-param query "url"))))
         (when (and u (plusp (length u)))
           (log-error "flag" u (format nil "flagged; ~a" *status*))
           (setf *status* (format nil "flagged for review: ~a" u))))
       (send stream "302 Found" "text/plain" "" :location "/"))
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
