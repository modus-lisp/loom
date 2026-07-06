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

;;; ---- browsing -------------------------------------------------------------
(defun follow (target)
  "on-navigate callback: load TARGET as the new page (keeps the callback)."
  (navigate target))

(defun navigate (url)
  (handler-case
      (let ((pg (if (or (search "://" url) (eql 0 (search "http" url)))
                    (l:load-url url :width 1024 :viewport-height 100000)
                    (l:load-url (concatenate 'string "https://" url)
                                :width 1024 :viewport-height 100000))))
        (setf (l:page-on-navigate pg) (lambda (p tgt) (declare (ignore p)) (follow tgt)))
        (setf *page* pg
              *status* (format nil "~a  —  ~a" (or (l:page-title pg) "") (or (l:page-url pg) url)))
        (incf *gen*))
    (error (e) (setf *status* (format nil "couldn't load ~a  (~a)" url e)))))

(defun page-png-bytes ()
  "Encode the current page canvas to PNG bytes (via a temp file)."
  (when *page*
    (let ((tmp "/tmp/weft-serve-view.png"))
      (r:write-png (l:page-canvas *page*) tmp)
      (with-open-file (in tmp :element-type '(unsigned-byte 8))
        (let ((b (make-array (file-length in) :element-type '(unsigned-byte 8))))
          (read-sequence b in) b)))))

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
<style>body{margin:0;font:14px sans-serif;background:#222;color:#eee}
#bar{position:sticky;top:0;background:#333;padding:6px;display:flex;gap:6px;z-index:9}
#bar input{flex:1;padding:5px;font-size:14px}#bar button{padding:5px 12px}
#s{padding:4px 8px;color:#9c9;font-size:12px}#v{display:block;background:#fff}</style></head>
<body><form id=bar action=\"/go\"><input name=url value=\"~a\" placeholder=\"https://…\" autofocus>
<button>Go</button></form><div id=s>~a</div>
<img id=v src=\"/view.png?g=~a\" width=\"1024\">
<script>
var v=document.getElementById('v');
v.onclick=function(e){var r=v.getBoundingClientRect();
 var sx=v.naturalWidth/r.width, sy=v.naturalHeight/r.height;
 var x=Math.round((e.clientX-r.left)*sx), y=Math.round((e.clientY-r.top)*sy);
 location='/click?x='+x+'&y='+y;};
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

(defun send (stream status ctype body &key location)
  (let* ((bytes (if (stringp body) (sb-ext:string-to-octets body :external-format :utf-8) body))
         (h (with-output-to-string (o)
              (format o "HTTP/1.1 ~a~a" status +crlf+)
              (when location (format o "Location: ~a~a" location +crlf+))
              (format o "Content-Type: ~a~aContent-Length: ~a~aConnection: close~a~a"
                      ctype +crlf+ (length bytes) +crlf+ +crlf+ +crlf+))))
    (write-sequence (sb-ext:string-to-octets h :external-format :latin-1) stream)
    (write-sequence bytes stream)
    (finish-output stream)))

(defun handle (stream)
  (let* ((head (read-head stream))
         (line (subseq head 0 (or (search +crlf+ head) (length head))))
         (parts (uiop:split-string line :separator " "))
         (target (or (second parts) "/"))
         (qpos (position #\? target))
         (path (if qpos (subseq target 0 qpos) target))
         (query (and qpos (subseq target (1+ qpos)))))
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
      ((string= path "/")
       (send stream "200 OK" "text/html; charset=utf-8" (client-html)))
      (t (send stream "404 Not Found" "text/plain" "not found")))))

(defun serve (&optional (port 8080))
  (let ((s (make-instance 'sock:inet-socket :type :stream :protocol :tcp)))
    (setf (sock:sockopt-reuse-address s) t)
    (sock:socket-bind s #(0 0 0 0) port)
    (sock:socket-listen s 16)
    (format t "~&weft-serve listening on http://0.0.0.0:~a/~%" port)
    (finish-output)
    (unwind-protect
        (loop
          (let ((c (sock:socket-accept s)))
            (let ((stream (sock:socket-make-stream c :element-type '(unsigned-byte 8)
                                                     :input t :output t)))
              (handler-case (handle stream)
                (error (e) (format t "~&[req] ~a~%" e)))
              (ignore-errors (close stream)))
            (ignore-errors (sock:socket-close c))))
      (sock:socket-close s))))

(let ((port (or (loop for a in (rest sb-ext:*posix-argv*)
                      for n = (ignore-errors (parse-integer a))
                      when n return n)
                8899)))
  (serve port))
