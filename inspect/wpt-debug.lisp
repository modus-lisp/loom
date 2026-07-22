;;;; wpt-debug.lisp — print per-subtest failures for specific WPT test files.
;;;;   sbcl --dynamic-space-size 4096 --script inspect/wpt-debug.lisp <relpath> [relpath...]
(defvar *files* (cdr sb-ext:*posix-argv*))
(setf sb-ext:*posix-argv* (list (first sb-ext:*posix-argv*)))  ; hide args from the runner's defparameters
(load "/tmp/claude-1001/-home-claude/52922185-82fd-4e4b-8d4f-166ff2de6022/scratchpad/runner-nomain.lisp")
(in-package #:wpt-th)
(setf *wpt-root* (namestring (truename "/home/claude/wpt/")))
(dolist (rel cl-user::*files*)
  (let ((path (merge-pathnames rel *wpt-root*)))
    (format t "~&===== ~a =====~%" rel)
    (multiple-value-bind (bucket subs) (run-one path)
      (format t "bucket: ~a  (~d subtests)~%" bucket (length subs))
      (dolist (r subs)
        (unless (zerop (second r))
          (format t "  FAIL [~a] ~a~%    ~a~%" (second r) (first r) (third r)))))))
