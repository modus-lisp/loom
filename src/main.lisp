;;;; src/main.lisp — start-page and file-URL helpers.
;;;;
;;;; loom drives weft through a display backend (glass, over VNC — see
;;;; src/glass-shell.lisp).  These are the small backend-agnostic helpers a
;;;; driver needs to open its first page: the bundled home page and the
;;;; file:// -> path resolution a followed link uses.
(in-package #:loom)

(defun default-home ()
  "The bundled offline start page shipped in loom's assets/."
  (let ((p (merge-pathnames "assets/home.html"
                            (asdf:system-source-directory "loom"))))
    (and (probe-file p) p)))

(defun url->path (file-url)
  "The filesystem path of a file:// URL (or the string unchanged)."
  (if (url-prefix-p "file://" file-url) (subseq file-url 7) file-url))

;;; ---- a page on a thread of its own --------------------------------------------------------------
;;; On modus a thread may not store into shared memory, and a page stores into weft's caches,
;;; shuttle's realm state and loom's network log as it loads.  WITH-THREAD-STATE binds all of it
;;; for the thread, so a worker can own a page outright (load, script, lay out, paint).
(defmacro with-thread-state (() &body body)
  "BODY with weft's, shuttle's, scribe's and loom's globals bound for this thread alone."
  `(let ((*net-log* *net-log*) (*net-log-t0* *net-log-t0*))
     (weft.script:with-thread-state () ,@body)))
(export 'with-thread-state)
