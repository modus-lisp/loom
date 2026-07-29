;;;; src/glass-shell.lisp — a glass (pure-CL VNC) driver over the page model.
;;;;
;;;; loom's other driver, shell.lisp, opens an SDL2 window and blits weft's canvas
;;;; into a GPU texture.  This one is the SAME driver over the SAME page model
;;;; (page.lisp) — but the "screen" is a glass framebuffer served over VNC, and
;;;; input arrives as RFB pointer/key events instead of SDL ones.  It needs NO
;;;; SDL and NO X: weft paints to a row-major RGB8 canvas, we pack that into a
;;;; glass 0x00RRGGBB framebuffer, and glass ships the dirty tiles to any VNC
;;;; client.  This is the pure-Lisp display path — the shell that finally makes
;;;; the whole weaving stack (scribe/shuttle/gesso/stencil/weft/loom) FFI-free.
;;;;
;;;; The RFB server runs its client loop in glass's own thread; its on-key/
;;;; on-pointer callbacks mutate the live page (dispatching DOM events through
;;;; weft), and a pump loop advances timers and repaints.  weft's JS context is
;;;; not reentrant, so a single mutex serialises input handling against the pump.

(in-package #:loom.glass)

(defstruct glass-app
  page fb
  (vw 0) (vh 0)
  ;; --- browser chrome (address bar + back/fwd/reload); CHROME-H 0 = bare page ---
  (chrome-h 0)                          ; toolbar height in px (0 = no chrome)
  (url "")                              ; current location shown in the address bar
  (editing nil)                         ; is the address bar being edited?
  (edit-buf "")                         ; the address-bar text while editing
  (edit-sel nil)                        ; text is "selected" (click-to-edit) — next key replaces it
  ;; --- navigation TREE (see nav.lisp): every navigation adds a child, nothing
  ;; is destroyed; the cursor is the node shown.  back/fwd are tree traversal now.
  (root nil)                            ; the tree's root node
  (cursor nil)                          ; the currently-shown node
  (next-id 0)                           ; monotonic node id / created tick
  (edit-parent nil)                     ; node a pending address-bar nav attaches under
  (uiframe nil)                         ; last painted ui frame (its hit-list drives clicks)
  (px -1) (py -1)                       ; last pointer position (chrome hover)
  (anim 0.0)                            ; spinner phase (advanced while any node loads)
  (lock (sb-thread:make-mutex :name "loom-glass-page"))
  (dirty t)
  (running t)                           ; pump-loop keeps going while true
  (buttons 0)                           ; last RFB button mask (low 3 bits)
  (shift nil)                           ; Shift held?  RFB has no modifier mask — it sends Shift as an ordinary key down/up, so we latch it
  ;; scroll-triggered lazy image loading (see MAYBE-WARM-LAZY):
  (warmed (make-hash-table :test 'equal)) ; lazy img URLs already warm-attempted (no re-warm)
  (warming nil)                         ; T while a background warm+re-render is in flight
  (last-scroll -1)                      ; page-scroll-y at the last lazy check (skip if unmoved)
  (last-lazy-check 0))                  ; internal-real-time of the last check (throttle to a few/sec)

(defun stop (app)
  "Stop APP's pump loop (e.g. when its host window is closed) so weft stops
   re-rendering into an orphaned framebuffer."
  (setf (glass-app-running app) nil))

;;; ---------------------------------------------------------------------------
;;; Paint — weft's RGB8 canvas slice -> the glass framebuffer
;;; ---------------------------------------------------------------------------
(defun paint (app)
  "Copy the visible slice of the page canvas (at the current scroll offset) into
   the glass framebuffer, packing RGB8 -> 0x00RRGGBB.  The page occupies the rows
   BELOW the chrome bar (CHROME-H, 0 for a bare page); rows past the content end
   (or the whole area, if the cursor node is still LOADING and has no page yet) are
   white.  The chrome is then drawn on top by the widget kit.  Caller holds the lock."
  (let* ((pg (glass-app-page app))
         (cv (and pg (loom:page-canvas pg)))
         (cw (if cv (r:canvas-width cv) 0))
         (ch (if cv (r:canvas-height cv) 0))
         (px (and cv (r:canvas-pixels cv)))
         (fb (glass-app-fb app))
         (fbpx (glass:fb-pixels fb))
         (fbw (glass:fb-width fb))
         (fbh (glass:fb-height fb))
         (ch-h (glass-app-chrome-h app))
         (page-h (- fbh ch-h))
         (sy (if pg (min (loom:page-scroll-y pg) (max 0 (- ch page-h))) 0))
         (cols (min cw fbw)))
    (glass:with-fb-locked (fb)
      (dotimes (y page-h)
        (let ((cy (+ sy y))
              (drow (* (+ y ch-h) fbw)))            ; page starts CH-H rows down
          (cond
            ((and px (< cy ch))
             (let ((srow (* cy cw 3)))
               (dotimes (x cols)
                 (let ((o (+ srow (* x 3))))
                   (setf (aref fbpx (+ drow x))
                         (logior (ash (aref px o) 16)
                                 (ash (aref px (+ o 1)) 8)
                                 (aref px (+ o 2))))))
               (loop for x from cols below fbw do (setf (aref fbpx (+ drow x)) #xffffff))))
            (t (loop for x from 0 below fbw do (setf (aref fbpx (+ drow x)) #xffffff))))))
      (when (plusp ch-h) (render-chrome app)))))

;;; ---------------------------------------------------------------------------
;;; Browser chrome — breadcrumb spine + branch rail, drawn with the widget kit.
;;; ---------------------------------------------------------------------------
;;; Two thin rows over the page:
;;;   row 1  [◄][►] news.yc › item3 › (b)                      ⟳ / spinner
;;;   row 2  siblings ( a )( b* )   children — …   + new
;;; render-chrome emits widgets via loom.ui; each clickable one pushes a hit rect
;;; tagged with an ACTION (a keyword or a (kind . node) cons).  The ui frame is
;;; stashed on the app so ON-POINTER can turn a chrome click into that action.
(defparameter +row1-y+ 4)   (defparameter +row1-h+ 30)
(defparameter +row2-y+ 37)  (defparameter +row2-h+ 22)
(defparameter +chrome-h+ 63)

(defun any-loading-p (app)
  "Is the cursor (the visible node) still loading?  Drives the spinner/progress."
  (let ((c (glass-app-cursor app))) (and c (nav-node-loading c))))

(defun render-chrome (app)
  "Draw the breadcrumb+rail chrome via the kit and stash the frame's hit-list on
   the app.  Caller holds the lock."
  (let* ((fb (glass-app-fb app)) (w (glass:fb-width fb))
         (u (ui:begin-frame fb :px (glass-app-px app) :py (glass-app-py app)))
         (cursor (glass-app-cursor app))
         (loading (any-loading-p app)))
    (ui:fill-bg u 0 +chrome-h+)
    ;; ---- row 1: back / forward + breadcrumb (or address field) + status ----
    (ui:row u 6 +row1-y+ +row1-h+)
    (ui:icon-button u :back :back :enabled (and cursor (nav-node-parent cursor)))
    (ui:icon-button u :forward :forward :enabled (and cursor (nav-node-children cursor)))
    (ui:gap u 6)
    (if (glass-app-editing app)
        ;; editing: the spine becomes an address field spanning to the status area
        (ui:text-field u :address (glass-app-edit-buf app)
                       :focus t :selected (glass-app-edit-sel app) :right-margin 40)
        ;; else: the breadcrumb spine, each crumb a jump target; current = accent chip
        (loop with path = (and cursor (nav-path cursor))
              for rest on path for node = (car rest) for last = (null (cdr rest))
              do (ui:breadcrumb-crumb u (cons :crumb node) (nav-label node :max (if last 40 22))
                                      :current last)
                 (unless last (ui:crumb-sep u))))
    ;; right-aligned status: spinner while loading, else reload
    (ui:row u (- w 34) +row1-y+ +row1-h+)
    (if loading (ui:spinner u (glass-app-anim app)) (ui:icon-button u :reload :reload :enabled cursor))
    (when loading (ui:row u 0 (+ +row1-y+ +row1-h+ 1) 2) (ui:progress-bar u 0.65))
    (ui:divider u (+ +row1-y+ +row1-h+ 2))
    ;; ---- row 2: branch rail — siblings (jump sideways) + children (jump in) ----
    (ui:row u 12 +row2-y+ +row2-h+)
    (when cursor
      (let ((sibs (nav-siblings cursor)) (kids (nav-children cursor)))
        (when (cdr sibs)                                 ; only show siblings if there's a choice
          (ui:label u "siblings" :size 11) (ui:gap u 8)
          (dolist (s sibs) (ui:chip u (cons :goto s) (nav-label s :max 16) :active (eq s cursor)) (ui:gap u 6))
          (ui:gap u 12))
        (ui:label u "children" :size 11) (ui:gap u 8)
        (if kids
            (dolist (k kids) (ui:chip u (cons :goto k) (nav-label k :max 16)) (ui:gap u 6))
            (progn (ui:label u "none yet" :size 12) (ui:gap u 8)))
        (ui:chip u :new "+ new")))
    (ui:divider u (1- +chrome-h+))
    (setf (glass-app-uiframe app) u)))

;;; ---------------------------------------------------------------------------
;;; Navigation — a followed link loads a fresh page at the same viewport
;;; ---------------------------------------------------------------------------
(defun load-target (target vw vh)
  "Load TARGET (an http(s) URL or a file path/URL) into a fresh, rendered page
   sized to the VW x VH viewport."
  (let ((pg (if (or (loom::url-prefix-p "http:" target) (loom::url-prefix-p "https:" target))
                (loom:load-url target :width vw :viewport-height vh)
                (loom:load-file (namestring (loom::url->path target)) :width vw :viewport-height vh))))
    (loom:render-page pg)
    pg))

(defun load-start (start vw vh)
  "Load START into a fresh rendered page sized VW x VH.  \"about:blank\" -> an empty
   document (instant, no network — the browser window appears immediately); an
   http(s) URL -> the network; anything else -> a local file."
  (let ((pg (cond
              ((string-equal start "about:blank")
               (loom:load-page "" :url "about:blank" :width vw :viewport-height vh))
              ((or (loom::url-prefix-p "http:" start) (loom::url-prefix-p "https:" start))
               (loom:load-url start :width vw :viewport-height vh))
              (t (loom:load-file (namestring (loom::url->path start)) :width vw :viewport-height vh)))))
    (loom:render-page pg)
    pg))

(defun nav-goto (app node)
  "Move the cursor to NODE and show its page (blank while it is still loading)."
  (setf (glass-app-cursor app) node
        (glass-app-page app) (nav-node-page node)
        (glass-app-url app) (nav-node-url node)
        (glass-app-editing app) nil
        (glass-app-dirty app) t)
  (when (nav-node-page node) (wire-navigation app))
  node)

(defun %spawn-render (app dest on-done)
  "Render DEST on a BACKGROUND thread (never the RFB/paint thread), then call
   (ON-DONE page-or-nil) under the lock.  ON-DONE decides where the page lands.
   This is what keeps a slow/huge/looping page from ever freezing VNC."
  (let ((vw (glass-app-vw app)) (vh (glass-app-vh app)))
    (sb-thread:make-thread
     (lambda ()
       (let ((pg (handler-case (load-start dest vw vh)
                   (error (e) (format *error-output* "~&loom.glass: load ~a failed: ~a~%" dest e) nil))))
         (sb-thread:with-mutex ((glass-app-lock app)) (funcall on-done pg))))
     :name "loom-glass-nav")))

(defun navigate (app dest &key (parent (glass-app-cursor app)))
  "Open DEST as a NEW child of PARENT (default: the current node) and move there.
   The node appears at once in a loading state (chrome shows the spinner); its page
   renders on a background thread, then fills the node.  Nothing is overwritten —
   every navigation BRANCHES the tree, so Back/Forward/sideways are all traversal.
   The synchronous part runs under the caller's lock (ON-POINTER/ON-KEY hold it)."
  (let ((node (make-nav-node :id (incf (glass-app-next-id app)) :created (glass-app-next-id app)
                             :url dest :parent parent :loading t)))
    (nav-add-child parent node)
    (unless (glass-app-root app) (setf (glass-app-root app) node))   ; very first nav = the root
    (nav-goto app node)                                              ; cursor -> the loading node
    (%spawn-render app dest
      (lambda (pg)
        (setf (nav-node-page node) pg (nav-node-loading node) nil)
        (if pg
            (setf (nav-node-url node)   (or (ignore-errors (loom:page-url pg)) dest)
                  (nav-node-title node) (or (ignore-errors (loom:page-title pg)) ""))
            (setf (nav-node-error node) t))
        (when (eq (glass-app-cursor app) node)                       ; still viewing it? sync + wire
          (setf (glass-app-page app) pg (glass-app-url app) (nav-node-url node))
          (when pg (wire-navigation app)))
        (setf (glass-app-dirty app) t)))
    node))

(defun go-back (app)
  "Up to the parent node (nothing lost — the branch we leave stays in the tree)."
  (let ((c (glass-app-cursor app)))
    (when (and c (nav-node-parent c)) (nav-goto app (nav-node-parent c)))))
(defun go-forward (app)
  "Down into the most-recently-opened child branch."
  (let ((c (glass-app-cursor app)))
    (when (and c (nav-node-children c)) (nav-goto app (nav-latest-child c)))))
(defun reload-page (app)
  "Re-render the current node's URL in place — replaces its page, keeps the node."
  (let ((node (glass-app-cursor app)))
    (when node
      (setf (nav-node-loading node) t (glass-app-dirty app) t)
      (%spawn-render app (nav-node-url node)
        (lambda (pg)
          (when pg (setf (nav-node-page node) pg))
          (setf (nav-node-loading node) nil)
          (when (eq (glass-app-cursor app) node)
            (setf (glass-app-page app) (nav-node-page node))
            (when (nav-node-page node) (wire-navigation app)))
          (setf (glass-app-dirty app) t))))))

(defun wire-navigation (app)
  "Install the current page's on-navigate callback so a clicked link opens a new
   CHILD of the current node (a branch), through NAVIGATE."
  (let ((pg (glass-app-page app)))
    (when pg (setf (loom:page-on-navigate pg)
                   (lambda (p target) (declare (ignore p)) (navigate app target))))))

;;; ---------------------------------------------------------------------------
;;; RFB input -> page-model calls (the SDL shell's handle-event, over RFB)
;;; ---------------------------------------------------------------------------
;;; RFB button mask: bit0 left, bit1 middle, bit2 right; bits 3/4 = wheel up/down
;;; (transient).  DOM button numbers: left 0, middle 1, right 2.
(defun start-edit (app buf parent &key (selected t))
  "Enter address-editing: the breadcrumb spine becomes a text field over BUF, and
   pressing Enter navigates a new child of PARENT."
  (setf (glass-app-editing app) t
        (glass-app-edit-buf app) buf
        (glass-app-edit-sel app) selected
        (glass-app-edit-parent app) parent
        (glass-app-dirty app) t))

(defun chrome-action (app action)
  "Dispatch a chrome hit-list ACTION (a keyword, or a (kind . node) cons)."
  (cond
    ((eq action :back)    (go-back app))
    ((eq action :forward) (go-forward app))
    ((eq action :reload)  (reload-page app))
    ((eq action :address))                                 ; click inside the field: keep editing
    ((eq action :new)                                      ; a new top-level branch off the root
     (start-edit app "" (or (glass-app-root app) (glass-app-cursor app)) :selected nil))
    ((and (consp action) (eq (car action) :goto)) (nav-goto app (cdr action)))
    ((and (consp action) (eq (car action) :crumb))
     (let ((node (cdr action)))
       (if (eq node (glass-app-cursor app))
           (start-edit app (nav-node-url node) node)       ; edit the current URL -> Enter opens a child
           (nav-goto app node))))))                        ; a past crumb -> jump straight there

(defun chrome-pointer (app mask x y)
  "A left-press in the chrome dispatches the hit-list action under (X,Y)."
  (let ((press-edge (and (logtest mask 1) (not (logtest (glass-app-buttons app) 1))))
        (frame (glass-app-uiframe app)))
    (setf (glass-app-buttons app) (logand mask 7))
    (when press-edge
      (let ((action (and frame (ui:hit-at frame x y))))
        (cond (action (chrome-action app action))
              ((glass-app-editing app)                     ; press on empty chrome cancels editing
               (setf (glass-app-editing app) nil (glass-app-dirty app) t)))))))

(defun on-pointer (app mask x y)
  (sb-thread:with-mutex ((glass-app-lock app))
    (let ((ch-h (glass-app-chrome-h app)))
      (cond
        ((and (plusp ch-h) (< y ch-h))                     ; in the chrome
         (setf (glass-app-px app) x (glass-app-py app) y   ; track pointer for hover
               (glass-app-dirty app) t)
         (chrome-pointer app mask x y))
        (t                                                 ; in the page (offset past the chrome)
         (setf (glass-app-px app) -1 (glass-app-py app) -1) ; no chrome widget is hot
         (when (glass-app-editing app)                     ; clicking the page ends address editing
           (setf (glass-app-editing app) nil (glass-app-dirty app) t))
         (let ((pg (glass-app-page app)))
           (when pg                                        ; a loading node has no page yet
             (let* ((py (- y ch-h)) (real (logand mask 7))
                    (changed (logxor real (glass-app-buttons app))))
               (when (logtest mask 8)  (loom:mouse-wheel pg 1))
               (when (logtest mask 16) (loom:mouse-wheel pg -1))
               (loom:mouse-move pg x py)
               (dotimes (b 3)
                 (when (logbitp b changed)
                   (if (logbitp b real)
                       (loom:mouse-press pg x py b)
                       (loom:mouse-release pg x py b))))
               (setf (glass-app-dirty app) t)))
           (setf (glass-app-buttons app) (logand mask 7))))))))

(defun keysym-name (keysym)
  "A DOM key string for a non-printable X keysym (thin — enough for keydown to
   fire; matches the SDL shell's key-name coverage)."
  (case keysym
    (#xff0d "Enter") (#xff08 "Backspace") (#xff09 "Tab") (#xff1b "Escape")
    (#xff51 "ArrowLeft") (#xff52 "ArrowUp") (#xff53 "ArrowRight") (#xff54 "ArrowDown")
    (#xffff "Delete") (#xff50 "Home") (#xff57 "End")
    (t "Unidentified")))

(defun normalize-input (s)
  "Turn address-bar text into a loadable location: keep a scheme/about: as-is,
   otherwise assume https://.  Empty -> about:blank."
  (let ((s (string-trim " " s)))
    (cond
      ((zerop (length s)) "about:blank")
      ((string-equal s "about:blank") s)
      ((or (loom::url-prefix-p "http:" s) (loom::url-prefix-p "https:" s)
           (loom::url-prefix-p "file:" s)) s)
      (t (concatenate 'string "https://" s)))))

(defun edit-key (app keysym)
  "Feed a keystroke to the address bar while it's being edited.  When the text is
   SELECTED (just clicked), the next edit replaces it whole."
  (cond
    ((= keysym #xff0d)                                     ; Enter -> open the URL as a new branch
     (setf (glass-app-editing app) nil (glass-app-edit-sel app) nil)
     (navigate app (normalize-input (glass-app-edit-buf app))
               :parent (or (glass-app-edit-parent app) (glass-app-cursor app))))
    ((= keysym #xff1b)                                     ; Escape -> cancel
     (setf (glass-app-editing app) nil (glass-app-edit-sel app) nil (glass-app-dirty app) t))
    ((= keysym #xff08)                                     ; Backspace (clears all if selected)
     (let ((b (glass-app-edit-buf app)))
       (setf (glass-app-edit-buf app)
             (if (glass-app-edit-sel app) "" (if (plusp (length b)) (subseq b 0 (1- (length b))) b))))
     (setf (glass-app-edit-sel app) nil (glass-app-dirty app) t))
    ((<= 32 keysym 126)                                    ; printable (replaces selection, else appends)
     (setf (glass-app-edit-buf app)
           (concatenate 'string (if (glass-app-edit-sel app) "" (glass-app-edit-buf app))
                        (string (code-char keysym)))
           (glass-app-edit-sel app) nil
           (glass-app-dirty app) t))))

(defparameter +shift-keysyms+ '(#xffe1 #xffe2)
  "Shift_L / Shift_R.  A modifier arrives as its own key event, never as a flag
   on the keystroke it modifies, so shift-selection depends on latching it.")

(defun on-key (app down keysym)
  (sb-thread:with-mutex ((glass-app-lock app))
    (cond
      ((member keysym +shift-keysyms+)
       (setf (glass-app-shift app) (and down t)))
      ((not down))                                     ; key-up: nothing else acts on it
      ((glass-app-editing app) (edit-key app keysym))
      (t
       (let ((pg (glass-app-page app))
             (shift (glass-app-shift app)))
         (cond
           ((<= 32 keysym 126)                         ; printable: keydown + textinput
            (let ((s (string (code-char keysym))))
              (loom:key-down pg s :key-code keysym :shift shift)
              (loom:key-text pg s)))
           (t (loom:key-down pg (keysym-name keysym) :key-code keysym :shift shift)))
         (setf (glass-app-dirty app) t))))))

;;; ---------------------------------------------------------------------------
;;; Scroll-triggered lazy image loading
;;; ---------------------------------------------------------------------------
;;; The glass driver scrolls by blitting a slice of the PRE-rendered full-page canvas
;;; (PAINT), so it never re-lays-out — a below-fold loading=lazy <img> would show its
;;; gray placeholder forever.  As the view scrolls, pull those images in like a real
;;; browser: notice when new deferred lazy images enter the scroll-relative in-view band,
;;; warm them (network) OFF the page lock so scrolling stays smooth, then re-render under
;;; the lock and mark the frame dirty so they "pop in" on the next paint.
(defparameter *lazy-check-interval* 1/5
  "Minimum seconds between scroll-driven lazy-image checks — throttles the box-tree walk
   to a few times a second so it never competes with the paint loop.")

(defun maybe-warm-lazy (app)
  "If scrolling has brought new deferred loading=lazy images into the in-view band, kick
   a background warm + re-render (non-blocking) so they pop in on a later frame.  Debounced
   three ways: skipped while a warm is already in flight, when the scroll hasn't moved since
   the last check, and throttled to *LAZY-CHECK-INTERVAL*; each URL is warmed at most once
   (a failed/offline image never re-triggers).  The slow network fetch runs off the page
   lock; only the final re-render (canvas swap) takes the lock, so the paint loop is smooth."
  (when (or (glass-app-warming app) (null (glass-app-page app)))  ; nothing to warm on a loading node
    (return-from maybe-warm-lazy nil))
  (let ((now (get-internal-real-time)))
    (when (< (/ (- now (glass-app-last-lazy-check app)) internal-time-units-per-second)
             *lazy-check-interval*)
      (return-from maybe-warm-lazy nil))
    (setf (glass-app-last-lazy-check app) now))
  (let ((pg (glass-app-page app)) (new '()))
    ;; read the pending in-view lazy set under the lock (it walks the live box tree)
    (sb-thread:with-mutex ((glass-app-lock app))
      (let ((sy (loom:page-scroll-y pg)))
        (when (/= sy (glass-app-last-scroll app))
          (setf (glass-app-last-scroll app) sy)
          (dolist (u (loom:inview-lazy-pending-urls pg))
            (unless (gethash u (glass-app-warmed app)) (push u new))))))
    (when new
      (dolist (u new) (setf (gethash u (glass-app-warmed app)) t))  ; don't re-warm these
      (setf (glass-app-warming app) t)
      (sb-thread:make-thread
       (lambda ()
         (unwind-protect
             (handler-case
                 (progn
                   (loom:warm-image-urls pg new)              ; network — OFF the page lock
                   (sb-thread:with-mutex ((glass-app-lock app))
                     (loom:render-page pg)                    ; cache-hit fill + repaint, under lock
                     (setf (glass-app-dirty app) t)))         ; next paint shows the pop-in
               (error (e) (format *error-output* "~&loom.glass: lazy warm failed: ~a~%" e)))
           (setf (glass-app-warming app) nil)))
       :name "loom-glass-lazy")
      t)))

;;; ---------------------------------------------------------------------------
;;; The loop
;;; ---------------------------------------------------------------------------
(defun pump-loop (app &key max-iterations)
  "Advance the page's timer loop and repaint when the DOM changed or input dirtied
   the view; glass's dirty-tile diff ships only what actually changed.  Runs until
   MAX-ITERATIONS frames (headless) or forever (a live session)."
  (loop with i = 0
        while (and (glass-app-running app) (or (null max-iterations) (< i max-iterations)))
        do (sb-thread:with-mutex ((glass-app-lock app))
             (let ((pg (glass-app-page app)))
               (when (and pg (loom::pump pg)) (setf (glass-app-dirty app) t)))
             (when (any-loading-p app)                     ; keep the spinner turning while loading
               (setf (glass-app-anim app) (mod (+ (glass-app-anim app) 0.04) 1.0)
                     (glass-app-dirty app) t))
             (when (glass-app-dirty app)
               (paint app)
               (setf (glass-app-dirty app) nil)))
           ;; off the lock: notice new lazy images the scroll brought into view and
           ;; warm+re-render them in the background (non-blocking — see MAYBE-WARM-LAZY).
           (maybe-warm-lazy app)
           (incf i)
           (unless max-iterations (sleep 1/60))))

(defun %init-root (app pg fallback-url)
  "Seed the navigation tree: wrap the already-rendered PG as the root node and
   point the cursor at it."
  (let ((node (make-nav-node :id (incf (glass-app-next-id app)) :created 1 :page pg
                             :url (or (and pg (ignore-errors (loom:page-url pg))) fallback-url)
                             :title (or (and pg (ignore-errors (loom:page-title pg))) ""))))
    (setf (glass-app-root app) node (glass-app-cursor app) node
          (glass-app-page app) pg (glass-app-url app) (nav-node-url node))
    (wire-navigation app)
    node))

(defun attach (page fb)
  "Build an app driving PAGE into the EXISTING framebuffer FB (viewport = FB
   size), seed the nav tree, paint once, and return the app — WITHOUT owning a
   server.  For embedding a live page as someone else's surface (e.g. a window in
   a compositor / window manager): the host forwards RFB input to ON-KEY /
   ON-POINTER and runs PUMP-LOOP to advance timers and repaint into FB."
  (let ((app (make-glass-app :fb fb :vw (glass:fb-width fb) :vh (glass:fb-height fb))))
    (%init-root app page (or (ignore-errors (loom:page-url page)) ""))
    (sb-thread:with-mutex ((glass-app-lock app)) (paint app))
    app))

(defun attach-browser (start fb)
  "Like ATTACH, but with browser CHROME (breadcrumb spine + branch rail) in the top
   strip; the page renders below it and its viewport is sized accordingly.  START
   is a URL, a file, or \"about:blank\" (instant).  The host forwards RFB input to
   ON-KEY / ON-POINTER and runs PUMP-LOOP as for ATTACH."
  (let* ((ch-h +chrome-h+)
         (vw (glass:fb-width fb)) (vh (max 1 (- (glass:fb-height fb) ch-h)))
         (pg (load-start start vw vh))
         (app (make-glass-app :fb fb :vw vw :vh vh :chrome-h ch-h)))
    (%init-root app pg start)
    (sb-thread:with-mutex ((glass-app-lock app)) (paint app))
    app))

(defun latin1 (string)
  "RFB desktop names are a byte string; fold any char > 255 (e.g. a title's
   em-dash) to ASCII so glass can write the ServerInit name cleanly."
  (map 'string (lambda (c) (if (< (char-code c) 256) c #\-)) (or string "loom")))

(defun serve (page &key (port 5900) max-iterations (background nil))
  "Serve PAGE over VNC on PORT: a glass framebuffer sized to the page viewport,
   RFB input wired into the page model.  Runs the pump loop on the calling thread
   (or, with BACKGROUND, in a new thread, returning the APP immediately)."
  (let* ((vw (loom:page-width page))
         (vh (loom:page-viewport-height page))
         (fb (glass:make-framebuffer vw vh (glass:rgb 255 255 255)))
         (app (make-glass-app :fb fb :vw vw :vh vh)))
    (%init-root app page (or (ignore-errors (loom:page-url page)) ""))
    (sb-thread:with-mutex ((glass-app-lock app)) (paint app))
    (sb-thread:make-thread
     (lambda () (glass:serve fb port
                             :on-key     (lambda (d k)   (on-key app d k))
                             :on-pointer (lambda (m x y) (on-pointer app m x y))
                             :name (latin1 (loom:page-title page))))
     :name "loom-glass-rfb")
    (if background
        (progn (sb-thread:make-thread (lambda () (pump-loop app)) :name "loom-glass-pump") app)
        (progn (pump-loop app :max-iterations max-iterations) app))))

(defun run-glass (&key start (port 5900) (width 1024) (height 768) max-iterations background)
  "Open START (a URL or file; default = the bundled home page) and serve it over
   VNC on PORT.  The glass counterpart of LOOM:RUN — no SDL, no X."
  (let ((page (load-target (or start (namestring (loom::default-home))) width height)))
    (serve page :port port :max-iterations max-iterations :background background)))
