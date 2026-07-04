;;;; src/page.lisp — the persistent page model (no SDL).
;;;;
;;;; A PAGE holds one live weft document across frames: the parsed DOM, its
;;;; scripting context (so setTimeout state, event listeners and DOM mutations
;;;; persist), the last render (canvas + box tree + computed styles), and the
;;;; viewport scroll position.  The browsing loop is: load -> render -> translate
;;;; input into DOM events (weft dispatches them) -> pump timers -> re-render if
;;;; the DOM changed.  Every function here is pure Lisp and headlessly testable;
;;;; the SDL shell (shell.lisp) is a thin driver over this model.
(in-package #:loom)

(defstruct page
  html css (base "")
  doc ctx
  (width 1024) (viewport-height 768)
  canvas root styles
  (content-height 0)
  (scroll-y 0)
  (press-node nil)                      ; node that received the last mousedown
  (hover-node nil)                      ; node currently under the pointer
  (cursor "default")
  (title "loom")
  (url nil)                             ; this page's own URL (for link resolution)
  (loader nil)
  (image-loader nil)                    ; (url) -> (values bytes mime) for network <img>
  (on-navigate nil))                    ; (page absolute-url) -> t : the shell follows a link

;;; ---------------------------------------------------------------------------
;;; Loading
;;; ---------------------------------------------------------------------------
(defun document-title (doc)
  "The <title> text of DOC, or NIL."
  (let ((el (css:query-select doc "title")))
    (and el (let ((tx (dom:text-content el)))
              (and (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) tx))) tx)))))

(defun load-page (html &key (css "") (base "") (width 1024) (viewport-height 768) url loader image-loader)
  "Parse HTML, build a fresh scripting context, run inline <script> and drain the
   initial timer/microtask queue, then render.  Returns a live PAGE."
  (let* ((doc (h:parse-html html))
         (ctx (ws:make-context doc :css css :width width :base base :loader loader))
         (pg (make-page :html html :css (or css "") :base base :doc doc :ctx ctx
                        :width width :viewport-height viewport-height
                        :url url :loader loader
                        :image-loader (or image-loader (make-image-loader base)))))
    (ws:run-inline-scripts ctx)
    (ws:pump-timers ctx 0)          ; settle 0-delay tasks/microtasks; future timers wait
    (ws:fire-lifecycle-events ctx)  ; DOMContentLoaded + load, so on-ready code runs
    (render-page pg)
    (setf (page-title pg) (or (document-title doc) url "loom"))
    pg))

(defun url-suffix-p (suffix s)
  (and (stringp s) (>= (length s) (length suffix))
       (string-equal (subseq s (- (length s) (length suffix))) suffix)))

(defun make-http-loader (base)
  "A weft.script subresource loader: (ctx url) -> (values kind content).  Resolves
   URL against BASE and fetches http(s) resources, guessing kind by extension.
   Any failure yields (values nil nil) so a missing subresource never crashes the
   page.  data: URIs are handled by weft itself before the loader is consulted."
  (lambda (ctx u)
    (declare (ignore ctx))
    (handler-case
        (let ((abs (or (resolve-url u base) u)))
          (if (or (url-prefix-p "http:" abs) (url-prefix-p "https:" abs))
              (values (cond ((url-suffix-p ".css" abs) :css)
                            ((or (url-suffix-p ".js" abs)
                                 (search "only=scripts" abs))   ; MediaWiki load.php
                             :js)
                            (t :text))
                      (fetch:fetch-text abs))
              (values nil nil)))
      (error () (values nil nil)))))

(defun make-image-loader (base)
  "An (url) -> (values bytes mime) network <img> fetcher over seal, resolving
   relative and protocol-relative URLs against BASE."
  (lambda (url)
    (handler-case
        (let ((abs (cond ((and (>= (length url) 2) (string= (subseq url 0 2) "//"))
                          (concatenate 'string "https:" url))
                         (t (or (resolve-url url base) url)))))
          (let ((resp (fetch:fetch abs)))
            (when (and resp (<= 200 (fetch:response-status resp) 299))
              (values (fetch:response-body resp)
                      (fetch:get-header (fetch:response-headers resp) "content-type")))))
      (error () (values nil nil)))))

(defun load-url (url-string &key (width 1024) (viewport-height 768))
  "Fetch and load URL-STRING as a fresh page (the network browsing entry)."
  (multiple-value-bind (text charset resp) (fetch:fetch-text url-string)
    (declare (ignore charset))
    (let ((final (or (and resp (fetch:response-url resp)) url-string)))
      (load-page text :base final :url final
                 :width width :viewport-height viewport-height
                 :loader (make-http-loader final)))))

(defun load-file (path &key (width 1024) (viewport-height 768))
  "Load a local HTML file as a fresh page (the default/offline browsing entry)."
  (let* ((truename (uiop:truenamize path))
         (base (format nil "file://~a" (namestring truename)))
         (html (uiop:read-file-string truename)))
    (load-page html :base base :url base :width width :viewport-height viewport-height)))

;;; ---------------------------------------------------------------------------
;;; Rendering
;;; ---------------------------------------------------------------------------
(defun render-page (pg)
  "Re-cascade, lay out and paint PG's current document at its width (reader view:
   the canvas grows to full content height; the shell blits a viewport slice).
   Refreshes the canvas, box tree, styles and content height, and re-clamps the
   scroll position."
  (multiple-value-bind (cv root styles)
      (let ((r:*image-loader* (page-image-loader pg)))   ; network <img> over seal, cached
        (r:render-document (page-doc pg) :width (page-width pg) :css (page-css pg)))
    (setf (page-canvas pg) cv
          (page-root pg) root
          (page-styles pg) styles
          (page-content-height pg) (r:canvas-height cv)
          (page-scroll-y pg) (clamp-scroll (page-scroll-y pg)
                                            (r:canvas-height cv) (page-viewport-height pg))))
  pg)

(defun relayout (pg new-width &optional new-viewport-height)
  "Re-lay out PG at a new window width (and optionally viewport height) — the
   response to a window resize."
  (setf (page-width pg) new-width)
  (when new-viewport-height (setf (page-viewport-height pg) new-viewport-height))
  (ignore-errors (setf (ws::context-width (page-ctx pg)) new-width))
  (render-page pg))

(defparameter *frame-ms* 16
  "Virtual milliseconds advanced per pump — one animation frame (~60fps).")

(defun pump (pg)
  "Advance the timer/animation clock by one frame (so setTimeout/animations
   progress ~one step, not to the task cap), then re-render if a handler mutated
   the DOM.  Called after every dispatched event and once per idle frame.
   Returns T when it re-rendered (the frame needs a fresh blit)."
  (ws:pump-timers (page-ctx pg) *frame-ms*)
  (when (ws:context-dirty (page-ctx pg))
    (render-page pg)
    (setf (ws:context-dirty (page-ctx pg)) nil)
    t))

;;; ---------------------------------------------------------------------------
;;; Hit-testing  (viewport point -> DOM node)
;;; ---------------------------------------------------------------------------
(defun node-at-page (pg vx vy)
  "The DOM node under viewport point (VX,VY), accounting for the scroll offset."
  (and (page-root pg) (r:node-at (page-root pg) vx (+ vy (page-scroll-y pg)))))

(defun anchor-href (node)
  "The href of NODE or its nearest ancestor <a>, or NIL — so a click on inline
   text inside a link still follows the link."
  (loop for n = node then (h:dnode-parent n)
        while n
        when (and (eq (h:dnode-kind n) :element)
                  (string-equal (h:dnode-name n) "a"))
          do (let ((href (dom:get-attribute n "href")))
               (when (and href (plusp (length href))) (return href)))))

(defun link-at (pg vx vy)
  "The absolute URL of the link under viewport point (VX,VY), or NIL."
  (let* ((n (node-at-page pg vx vy)) (href (and n (anchor-href n))))
    (and href (resolve-url href (or (page-url pg) (page-base pg) "")))))

;;; ---------------------------------------------------------------------------
;;; Input -> DOM events
;;; ---------------------------------------------------------------------------
(defun mouse-press (pg vx vy &optional (button 0))
  "Route an SDL mouse-button-down at viewport (VX,VY) to a trusted mousedown on
   the hit node.  Returns the hit node."
  (let ((n (node-at-page pg vx vy)))
    (setf (page-press-node pg) n)
    (when n
      (ws:dispatch-mouse-event (page-ctx pg) n "mousedown"
                               :button button :client-x vx :client-y vy))
    (pump pg)
    n))

(defun mouse-release (pg vx vy &optional (button 0))
  "Route an SDL mouse-button-up at viewport (VX,VY): fire mouseup, and — if the
   pointer is still over the node that received the press — a click.  If the
   click's default action is not prevented and the node is (inside) a link, ask
   the shell to navigate.  Returns the hit node."
  (let ((n (node-at-page pg vx vy)) (ctx (page-ctx pg)))
    (when n
      (ws:dispatch-mouse-event ctx n "mouseup" :button button :client-x vx :client-y vy))
    (when (and n (eq n (page-press-node pg)))
      (let ((go (ws:dispatch-mouse-event ctx n "click"
                                         :button button :client-x vx :client-y vy :detail 1)))
        (when (and go (zerop button)) (maybe-follow-link pg n))))
    (setf (page-press-node pg) nil)
    (pump pg)
    n))

(defun maybe-follow-link (pg node)
  (let ((href (anchor-href node)))
    (when href
      (let ((target (resolve-url href (or (page-url pg) (page-base pg) ""))))
        (when (and target (page-on-navigate pg))
          (funcall (page-on-navigate pg) pg target))))))

(defun mouse-move (pg vx vy)
  "Route pointer motion: a mousemove on the hit node, and — when the hit node
   changes — mouseout on the old node and mouseover on the new one, plus a cursor
   update.  Returns the hit node."
  (let ((n (node-at-page pg vx vy)) (ctx (page-ctx pg)) (prev (page-hover-node pg)))
    (when n
      (ws:dispatch-mouse-event ctx n "mousemove" :client-x vx :client-y vy))
    (unless (eq n prev)
      (when prev (ws:dispatch-mouse-event ctx prev "mouseout" :client-x vx :client-y vy))
      (when n (ws:dispatch-mouse-event ctx n "mouseover" :client-x vx :client-y vy))
      (setf (page-hover-node pg) n
            (page-cursor pg) (cursor-for pg n)))
    (pump pg)
    n))

(defun cursor-for (pg node)
  "The cursor keyword for NODE: pointer over a link, else the computed CSS
   cursor, else default."
  (cond ((and node (anchor-href node)) "pointer")
        (t (let ((cs (and node (gethash node (page-styles pg)))))
             (or (and cs (css:cstyle-cursor cs)) "default")))))

(defun mouse-wheel (pg wheel-y)
  "Scroll the viewport by an SDL wheel notch (WHEEL-Y).  Returns the new
   scroll-y.  Pure viewport math — no relayout."
  (setf (page-scroll-y pg)
        (clamp-scroll (+ (page-scroll-y pg) (wheel->scroll-delta wheel-y))
                      (page-content-height pg) (page-viewport-height pg))))

(defun key-target (pg)
  "The node keyboard events target (no focus model yet — the body element)."
  (or (css:query-select (page-doc pg) "body")
      (css:query-select (page-doc pg) "html")
      (page-doc pg)))

(defun key-down (pg key &key key-code)
  "Dispatch a trusted keydown for DOM key string KEY (\"a\", \"Enter\", …)."
  (let ((n (key-target pg)))
    (when n (ws:dispatch-keyboard-event (page-ctx pg) n "keydown" :key key :key-code key-code))
    (pump pg)))

(defun key-text (pg text)
  "Dispatch keypress for typed TEXT (a thin start; editable fields/caret are a
   later round)."
  (let ((n (key-target pg)))
    (when (and n (plusp (length text)))
      (ws:dispatch-keyboard-event (page-ctx pg) n "keypress" :char text))
    (pump pg)))
