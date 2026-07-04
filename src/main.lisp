;;;; src/main.lisp — the run entrypoint and CLI.
;;;;
;;;; RUN is the one call a user makes.  It enters SDL through
;;;; sdl2:make-this-thread-main so the window + event loop run on the process
;;;; main thread, which macOS/Cocoa requires (see the README), then opens the
;;;; window on the start page (a CLI URL/path, or the bundled home page).
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
  "Open loom's window on START and browse.  This is the macOS-safe entry: it runs
   the SDL window and event loop on the process main thread via
   sdl2:make-this-thread-main.

     (loom:run)                         ; the bundled home page
     (loom:run :start \"https://example.com\")
     (loom:run :start \"/path/to/page.html\")"
  (sdl2:make-this-thread-main
   (lambda ()
     (sdl2:with-init (:video)
       (unwind-protect
            (run-shell (open-start-page start :width width :height height)
                       :width width :height height)
         (sdl2:push-quit-event))))))

(defun main ()
  "CLI entry: the first argv is an optional start URL/path."
  (let ((args (uiop:command-line-arguments)))
    (run :start (first args))))
