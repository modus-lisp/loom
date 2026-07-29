;;;; inspect/glass-forms.lisp — fill in a form over VNC, with no client but the
;;;; protocol.  The headless gate (forms-interact.lisp) drives the PAGE MODEL
;;;; directly; this proves the same thing survives the layer above it — RFB
;;;; PointerEvent/KeyEvent -> glass-shell -> page -> DOM -> repaint -> the pixels
;;;; that come back over the wire.  Everything between chrome offsets and keysym
;;;; translation lives only in that layer, so only this can catch it.
;;;;
;;;;   sbcl --control-stack-size 256 --dynamic-space-size 4096 --non-interactive \
;;;;        --load inspect/glass-forms.lisp
(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload '(:glass :zpng :chipz))
    (asdf:load-system :loom/glass)))

(defpackage #:lgforms (:use #:cl) (:local-nicknames (#:ws #:weft.script))) (in-package #:lgforms)

;;; ---- a minimal RFB client (same wire helpers as glass-demo) ----------------
(defun r8 (s) (read-byte s)) (defun r16 (s) (logior (ash (r8 s) 8) (r8 s)))
(defun r32 (s) (logior (ash (r16 s) 16) (r16 s)))
(defun rn (s n) (let ((b (make-array n :element-type '(unsigned-byte 8)))) (read-sequence b s) b))
(defun w8 (s v) (write-byte (logand v #xff) s))
(defun w16 (s v) (w8 s (ash v -8)) (w8 s v)) (defun w32 (s v) (w16 s (ash v -16)) (w16 s v))
(defun connect (port)
  (loop repeat 500 do
    (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
      (handler-case
          (progn (sb-bsd-sockets:socket-connect sock (sb-bsd-sockets:make-inet-address "127.0.0.1") port)
                 (return-from connect
                   (sb-bsd-sockets:socket-make-stream sock :input t :output t
                                                      :element-type '(unsigned-byte 8) :buffering :full)))
        (error () (ignore-errors (sb-bsd-sockets:socket-close sock)) (sleep 0.05))))))
(defun handshake (s)
  (rn s 12) (write-sequence (map 'vector #'char-code "RFB 003.008") s) (w8 s 10) (force-output s)
  (let ((n (r8 s))) (rn s n)) (w8 s 1) (force-output s) (r32 s) (w8 s 1) (force-output s)
  (let ((w (r16 s)) (h (r16 s))) (rn s 16) (let ((nl (r32 s))) (rn s nl))
    (w8 s 2) (w8 s 0) (w16 s 2) (w32 s 16) (w32 s 0) (force-output s) (values w h)))
(defun ptr (s mask x y) (w8 s 5) (w8 s mask) (w16 s x) (w16 s y) (force-output s) (sleep 0.08))
(defun key (s down keysym) (w8 s 4) (w8 s (if down 1 0)) (w16 s 0) (w32 s keysym)
  (force-output s) (sleep 0.05))
(defun typ (s text) (loop for c across text do (key s t (char-code c)) (key s nil (char-code c))))

;;; ---- the check -------------------------------------------------------------
(defvar *pass* 0) (defvar *fail* 0)
(defun check (name got want)
  (if (equal got want) (progn (incf *pass*) (format t "  ok   ~a = ~s~%" name got))
      (progn (incf *fail*) (format t "  FAIL ~a: got ~s, want ~s~%" name got want)))
  (finish-output))

(defun page-of (app) (loom.glass:glass-app-page app))
(defun js (app expr)
  ;; weft's JS context is single-threaded and the shell's RFB/pump threads are
  ;; already in it — take the same lock they do, or this observer corrupts the
  ;; very thing it is observing.
  (sb-thread:with-mutex ((loom.glass::glass-app-lock app))
    ;; String() on the way out: a JS boolean prints as #<js true> to Lisp, and a
    ;; harness that reports the engine's internal print name instead of the value
    ;; fails on itself.
    (let ((v (shuttle:eval-script (ws:context-realm (loom::page-ctx (page-of app)))
                                  (format nil "String(~a)" expr))))
      (if (stringp v) v (princ-to-string v)))))

(defun widget-point (app id)
  "A viewport point inside ID's widget — what a person would aim at.  Returned in
   RFB desktop coordinates, i.e. offset down past the browser chrome."
  (sb-thread:with-mutex ((loom.glass::glass-app-lock app))
   (let ((pg (page-of app)) (found nil))
    ;; A :line box's children are FRAGs (text runs), not boxes — walk only the
    ;; boxes; a form control always has one of its own.
    (labels ((walk (b)
               (let ((n (weft.render:lbox-node b)))
                 (when (and n (eq (weft.html:dnode-kind n) :element)
                            (equal (cdr (assoc "id" (weft.html:dnode-attrs n) :test #'string-equal)) id))
                   (setf found b)))
               (unless found
                 (dolist (c (weft.render:lbox-children b))
                   (when (weft.render::lbox-p c) (walk c))))))
      (walk (loom:page-root pg)))
    (unless found (error "no box for ~a" id))
    (values (+ (round (weft.render:lbox-x found)) 6)
            (+ (round (weft.render:lbox-y found)) 6
               (loom.glass::glass-app-chrome-h app)
               (- (loom:page-scroll-y pg)))))))

(defun widget-box (app id)
  (sb-thread:with-mutex ((loom.glass::glass-app-lock app))
    (let ((found nil))
      (labels ((walk (b)
                 (let ((n (weft.render:lbox-node b)))
                   (when (and n (eq (weft.html:dnode-kind n) :element)
                              (equal (cdr (assoc "id" (weft.html:dnode-attrs n) :test #'string-equal)) id))
                     (setf found b)))
                 (unless found
                   (dolist (c (weft.render:lbox-children b))
                     (when (weft.render::lbox-p c) (walk c))))))
        (walk (loom:page-root (page-of app))))
      (or found (error "no box for ~a" id)))))

(defun menu-point (app id row)
  "RFB desktop coordinates of ROW in ID's open dropdown — from the same geometry
   the painter used, so this aims at the pixels that are actually there."
  (let* ((app-lock (loom.glass::glass-app-lock app))
         (b (widget-box app id)))
    (sb-thread:with-mutex (app-lock)
      (let ((pg (page-of app)))
        (multiple-value-bind (x y w h row-h)
            (weft.render:select-menu-geometry
             b (ws:select-labels (weft.render:lbox-node b))
             (weft.render:canvas-height (loom:page-canvas pg)))
          (declare (ignore w h))
          (values (+ x 6)
                  (+ y 3 (* row row-h)
                     (loom.glass::glass-app-chrome-h app)
                     (- (loom:page-scroll-y pg)))))))))

(defun click-at (s x y) (ptr s 0 x y) (ptr s 1 x y) (ptr s 0 x y) (sleep 0.3))

(let* ((port 5972)
       (path "/tmp/loom-glass-form.html"))
  (with-open-file (o path :direction :output :if-exists :supersede)
    (write-string "<!doctype html><meta charset=utf-8><title>form over vnc</title>
<body style=\"font: 16px sans-serif; padding: 20px\">
<form id=f><p>Name: <input id=u type=text size=24 name=user>
<p><input id=k type=checkbox name=ok> I agree
<p>Plan: <select id=plan name=plan><option>free</option><option>paid</option></select>
<p><input id=s type=submit value=\"Sign in\"></form>
<p id=out>nothing submitted</p>
<script>document.getElementById('f').addEventListener('submit',function(e){
  e.preventDefault();
  document.getElementById('out').textContent =
    'submitted: ' + document.getElementById('u').value + ' / ' +
    document.getElementById('k').checked + ' / ' +
    document.getElementById('plan').value;});</script>" o))
  (let ((app (loom.glass:run-glass :start path :port port :width 900 :height 640 :background t)))
    (sleep 2)
    (format t "~&=== loom form interaction over real VNC ===~%") (finish-output)
    (let ((s (connect port)))
      (multiple-value-bind (w h) (handshake s)
        (format t "  vnc desktop ~dx~d, chrome ~d px~%" w h (loom.glass::glass-app-chrome-h app)) (finish-output)
        ;; click the text field, type into it
        (multiple-value-bind (x y) (widget-point app "u") (click-at s x y))
        (check "click focused the field" (js app "document.activeElement.id") "u")
        (typ s "ynniv")
        (check "typing over RFB reached .value" (js app "document.getElementById('u').value") "ynniv")
        ;; tick the checkbox
        (multiple-value-bind (x y) (widget-point app "k") (click-at s x y))
        (check "click ticked the checkbox" (js app "document.getElementById('k').checked") "true")
        ;; open the dropdown and pick the second row — the popup is painted over
        ;; the page, so this only works if those pixels really went out on the wire
        (multiple-value-bind (x y) (widget-point app "plan") (click-at s x y))
        (multiple-value-bind (x y) (menu-point app "plan" 1) (click-at s x y))
        (check "picking from the dropdown over RFB" (js app "document.getElementById('plan').value")
               "paid")
        ;; submit
        (multiple-value-bind (x y) (widget-point app "s") (click-at s x y))
        (sleep 0.5)
        (check "submit saw what was entered" (js app "document.getElementById('out').textContent")
               "submitted: ynniv / true / paid")
        (ignore-errors (close s)))))
  (format t "~&~%~d passed, ~d failed~%" *pass* *fail*))
(finish-output)
(sb-ext:exit :code (if (plusp *fail*) 1 0))
