;;;; nav.lisp — loom's navigation TREE (the model behind the browser chrome).
;;;;
;;;; The tired model is "one page + one linear history": every navigation
;;;; overwrites the page and going Back-then-elsewhere throws the forward branch
;;;; away.  Instead we keep a TREE.  Each visited page is a NAV-NODE; navigating
;;;; from a node adds a CHILD and moves the cursor there; nothing is ever
;;;; destroyed.  Back = go to the parent; a node's other children are forward
;;;; branches; a node's siblings are the alternatives you reached the same way.
;;;; So the browser's history is a living tree you can walk in every direction,
;;;; and every node is effectively a reopenable "tab".
;;; This is the first file in the LOOM.GLASS package, so it declares the package.
(defpackage #:loom.glass
  (:use #:cl)
  (:local-nicknames (#:r #:weft.render) (#:ui #:loom.ui))
  (:export #:serve #:run-glass #:attach #:attach-browser #:pump-loop #:on-key #:on-pointer #:stop
           #:glass-app #:glass-app-page #:glass-app-fb
           ;; the live selection, and whether finishing one also copies it
           #:selection-text #:*copy-on-select*
           ;; per-frame paint counters (see scroll-perf.lisp) — OFF by default
           #:*scroll-perf* #:scroll-perf-reset #:scroll-perf-report))
(in-package #:loom.glass)

(defstruct nav-node
  (id 0)
  (url "")                       ; the node's location (updated to the final URL once loaded)
  (title "")                     ; a short label for its crumb/chip (page title once loaded)
  page                           ; the loom page object, or NIL while still loading
  parent                         ; the nav-node we navigated FROM (NIL for a root)
  (children '())                 ; nodes navigated TO from here, in creation order
  (loading nil)                  ; T from creation until its render completes
  (error nil)                    ; error string if the load failed
  (created 0))                   ; monotonic tick, for ordering

(defun nav-root-p (node) (null (nav-node-parent node)))

(defun nav-add-child (parent node)
  "Append NODE to PARENT's children (PARENT may be NIL for a new root)."
  (when parent
    (setf (nav-node-children parent) (append (nav-node-children parent) (list node))))
  node)

(defun nav-path (node)
  "The nodes from the root down to NODE inclusive — the breadcrumb spine."
  (loop for n = node then (nav-node-parent n) while n collect n into acc
        finally (return (nreverse acc))))

(defun nav-siblings (node)
  "The nodes at NODE's level (its parent's children); (NODE) if NODE is a root."
  (let ((p (nav-node-parent node)))
    (if p (nav-node-children p) (list node))))

(defun nav-children (node) (nav-node-children node))

(defun nav-latest-child (node)
  "The most recently created child of NODE (the default Forward target), or NIL."
  (car (last (nav-node-children node))))

(defun nav-node-count (root)
  "Total nodes in ROOT's tree (for status / debugging)."
  (1+ (reduce #'+ (nav-node-children root) :key #'nav-node-count :initial-value 0)))

;;; ---- labels ---------------------------------------------------------------
(defun url-short-label (url)
  "A compact human label from a URL: 'about:blank' -> 'New'; else the host with a
   leading www. stripped (falling back to the whole string).  Used for a crumb's
   text before the page's real <title> is known."
  (cond
    ((or (null url) (zerop (length url))) "New")
    ((string-equal url "about:blank") "New")
    (t (let* ((s url)
              (p (search "://" s))
              (rest (if p (subseq s (+ p 3)) s))
              (slash (position #\/ rest))
              (host (if slash (subseq rest 0 slash) rest)))
         (when (and (>= (length host) 4) (string-equal (subseq host 0 4) "www."))
           (setf host (subseq host 4)))
         (if (plusp (length host)) host s)))))

(defun nav-label (node &key (max 30))
  "The crumb/chip label for NODE: its title if known, else a short URL label,
   truncated to MAX chars with an ellipsis."
  (let ((s (cond ((nav-node-loading node) (url-short-label (nav-node-url node)))
                 ((plusp (length (nav-node-title node))) (nav-node-title node))
                 (t (url-short-label (nav-node-url node))))))
    (if (> (length s) max) (concatenate 'string (subseq s 0 (1- max)) "…") s)))
