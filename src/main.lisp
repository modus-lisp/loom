;;;; src/main.lisp — the run entrypoint and CLI.
;;;;
;;;; RUN is the one call a user makes.  It initializes SDL and opens the window
;;;; on the start page (a CLI URL/path, or the bundled home page).  macOS/Cocoa
;;;; requires the window + event loop to run on the process main thread; RUN must
;;;; therefore be called on the initial thread (the sbcl --eval / run.sh path,
;;;; not a spawned thread or a SLIME worker) — see the README.
(in-package #:loom)

(defun default-home ()
  "The bundled offline start page shipped in loom's assets/."
  (let ((p (merge-pathnames "assets/home.html"
                            (asdf:system-source-directory "loom"))))
    (and (probe-file p) p)))

(defun open-start-page (start &key (width 1024) (height 768))
  "Load START (an http(s) URL, a local file path, or NIL -> the bundled home
   page) into a fresh page."
  (cond
    ((null start)
     (let ((home (default-home)))
       (if home (load-file (namestring home) :width width :viewport-height height)
           (load-page "<title>loom</title><body><h1>loom</h1><p>No start page.</p></body>"
                      :width width :viewport-height height))))
    ((or (url-prefix-p "http:" start) (url-prefix-p "https:" start))
     (load-url start :width width :viewport-height height))
    (t (load-file start :width width :viewport-height height))))

(defun run (&key start (width 1024) (height 768))
  "Open loom's window on START and browse.  Call this on the process main thread
   (macOS/Cocoa requires it) — i.e. from the REPL's initial thread or an
   sbcl --eval / run.sh invocation, not a spawned thread.

     (loom:run)                         ; the bundled home page
     (loom:run :start \"https://example.com\")
     (loom:run :start \"/path/to/page.html\")"
  (sdl:init)
  (unwind-protect
       (run-shell (open-start-page start :width width :height height)
                  :width width :height height)
    (sdl:quit)))

(defun main ()
  "CLI entry: the first argv is an optional start URL/path."
  (let ((args (uiop:command-line-arguments)))
    (run :start (first args))))
