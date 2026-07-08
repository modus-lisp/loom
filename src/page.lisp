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
  (js-error nil)                        ; message of a script error that didn't stop the render
  (on-navigate nil))                    ; (page absolute-url) -> t : the shell follows a link

;;; ---------------------------------------------------------------------------
;;; Loading
;;; ---------------------------------------------------------------------------
(defun document-title (doc)
  "The <title> text of DOC, or NIL."
  (let ((el (css:query-select doc "title")))
    (and el (let ((tx (dom:text-content el)))
              (and (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) tx))) tx)))))

(defparameter *js-budget* 2.0
  "Seconds of wall-clock a page's scripts may run during load before the raster is
   taken with the DOM as it stands.")

(defun dom-text-length (node)
  "Total length of NODE's visible text (excluding <script>/<style>) — a coarse signal
   of how much content the markup carries, used to detect when scripts blank the page."
  (case (h:dnode-kind node)
    (:text (length (or (h:dnode-data node) "")))
    (:element (if (member (string-downcase (h:dnode-name node))
                          '("script" "style" "template" "noscript") :test #'string=)
                  0
                  (loop for c across (h:dnode-children node) sum (dom-text-length c))))
    (t (loop for c across (h:dnode-children node) sum (dom-text-length c)))))

;;; ---- MathJax fallback ----------------------------------------------------
;;; MathJax renders `\(...\)` / `\[...\]` LaTeX via JavaScript we don't run, so the
;;; raw source (`\(u \leq_F v\)`) would otherwise show as text.  A JS-off browser
;;; shows the same; a reader can do better by transliterating the common LaTeX to
;;; Unicode (u ≤_F v) — not typeset math, but legible.  Non-standard, hence in loom.

(defparameter *tex-symbols*
  (let ((h (make-hash-table :test 'equal)))
    (dolist (pair '(("\\leq" "≤") ("\\le" "≤") ("\\geq" "≥") ("\\ge" "≥") ("\\neq" "≠") ("\\ne" "≠")
                    ("\\in" "∈") ("\\notin" "∉") ("\\ni" "∋") ("\\subset" "⊂") ("\\subseteq" "⊆")
                    ("\\supset" "⊃") ("\\supseteq" "⊇") ("\\cup" "∪") ("\\cap" "∩") ("\\setminus" "∖")
                    ("\\emptyset" "∅") ("\\varnothing" "∅") ("\\times" "×") ("\\cdot" "⋅") ("\\ast" "∗")
                    ("\\pm" "±") ("\\mp" "∓") ("\\div" "÷") ("\\star" "⋆") ("\\circ" "∘") ("\\bullet" "•")
                    ("\\to" "→") ("\\rightarrow" "→") ("\\leftarrow" "←") ("\\Rightarrow" "⇒")
                    ("\\Leftarrow" "⇐") ("\\Leftrightarrow" "⇔") ("\\leftrightarrow" "↔") ("\\mapsto" "↦")
                    ("\\uparrow" "↑") ("\\downarrow" "↓") ("\\implies" "⟹") ("\\iff" "⟺")
                    ("\\forall" "∀") ("\\exists" "∃") ("\\nexists" "∄") ("\\nabla" "∇") ("\\partial" "∂")
                    ("\\infty" "∞") ("\\sum" "∑") ("\\prod" "∏") ("\\int" "∫") ("\\oint" "∮")
                    ("\\sqrt" "√") ("\\approx" "≈") ("\\cong" "≅") ("\\equiv" "≡") ("\\sim" "∼")
                    ("\\simeq" "≃") ("\\propto" "∝") ("\\ll" "≪") ("\\gg" "≫") ("\\prec" "≺") ("\\succ" "≻")
                    ("\\ldots" "…") ("\\cdots" "⋯") ("\\dots" "…") ("\\vdots" "⋮") ("\\ddots" "⋱")
                    ("\\land" "∧") ("\\lor" "∨") ("\\lnot" "¬") ("\\neg" "¬") ("\\wedge" "∧") ("\\vee" "∨")
                    ("\\oplus" "⊕") ("\\otimes" "⊗") ("\\perp" "⊥") ("\\parallel" "∥") ("\\angle" "∠")
                    ("\\langle" "⟨") ("\\rangle" "⟩") ("\\lceil" "⌈") ("\\rceil" "⌉") ("\\lfloor" "⌊") ("\\rfloor" "⌋")
                    ("\\mid" "∣") ("\\backslash" "\\") ("\\%" "%") ("\\&" "&") ("\\#" "#") ("\\$" "$")
                    ("\\alpha" "α") ("\\beta" "β") ("\\gamma" "γ") ("\\delta" "δ") ("\\epsilon" "ε")
                    ("\\varepsilon" "ε") ("\\zeta" "ζ") ("\\eta" "η") ("\\theta" "θ") ("\\vartheta" "ϑ")
                    ("\\iota" "ι") ("\\kappa" "κ") ("\\lambda" "λ") ("\\mu" "μ") ("\\nu" "ν") ("\\xi" "ξ")
                    ("\\pi" "π") ("\\varpi" "ϖ") ("\\rho" "ρ") ("\\sigma" "σ") ("\\tau" "τ") ("\\upsilon" "υ")
                    ("\\phi" "φ") ("\\varphi" "φ") ("\\chi" "χ") ("\\psi" "ψ") ("\\omega" "ω")
                    ("\\Gamma" "Γ") ("\\Delta" "Δ") ("\\Theta" "Θ") ("\\Lambda" "Λ") ("\\Xi" "Ξ")
                    ("\\Pi" "Π") ("\\Sigma" "Σ") ("\\Phi" "Φ") ("\\Psi" "Ψ") ("\\Omega" "Ω")
                    ("\\mathbb{R}" "ℝ") ("\\mathbb{N}" "ℕ") ("\\mathbb{Z}" "ℤ") ("\\mathbb{Q}" "ℚ")
                    ("\\mathbb{C}" "ℂ")
                    ("\\quad" "  ") ("\\qquad" "    ") ("\\left" "") ("\\right" "") ("\\bigl" "") ("\\bigr" "")
                    ("\\Big" "") ("\\big" "") ("\\displaystyle" "") ("\\textstyle" "") ("\\limits" "")))
      (setf (gethash (first pair) h) (second pair)))
    h))

(defparameter *tex-subscripts*
  (let ((h (make-hash-table)))
    (loop for c across "0123456789+-=()aehijklmnoprstuvx"
          for u across "₀₁₂₃₄₅₆₇₈₉₊₋₌₍₎ₐₑₕᵢⱼₖₗₘₙₒₚᵣₛₜᵤᵥₓ"
          do (setf (gethash c h) u))
    h))

(defparameter *tex-superscripts*
  (let ((h (make-hash-table)))
    (loop for c across "0123456789+-=()abcdefghijklmnoprstuvwxyz"
          for u across "⁰¹²³⁴⁵⁶⁷⁸⁹⁺⁻⁼⁽⁾ᵃᵇᶜᵈᵉᶠᵍʰⁱʲᵏˡᵐⁿᵒᵖʳˢᵗᵘᵛʷˣʸᶻ"
          do (setf (gethash c h) u))
    h))

(defun %tex-script-arg (s i)
  "Read a sub/superscript argument at index I: a {group} or one char.  (values text new-i)."
  (let ((n (length s)))
    (cond ((>= i n) (values "" i))
          ((char= (char s i) #\{)
           (let ((j (1+ i)) (depth 1))
             (loop while (and (< j n) (> depth 0))
                   do (case (char s j) (#\{ (incf depth)) (#\} (decf depth))) (incf j))
             (values (subseq s (1+ i) (max (1+ i) (1- j))) j)))
          (t (values (string (char s i)) (1+ i))))))

(defun tex->unicode (s)
  "Transliterate a LaTeX math fragment S to a Unicode approximation."
  (with-output-to-string (out)
    (let ((i 0) (n (length s)))
      (loop while (< i n) do
        (let ((c (char s i)))
          (cond
            ((char= c #\\)
             (cond
               ;; \mathbb{R} etc. — a 10-char token including the braced letter
               ((and (<= (+ i 10) n) (gethash (subseq s i (+ i 10)) *tex-symbols*))
                (write-string (gethash (subseq s i (+ i 10)) *tex-symbols*) out) (setf i (+ i 10)))
               ((and (< (1+ i) n) (alpha-char-p (char s (1+ i))))
                (let ((j (1+ i)))
                  (loop while (and (< j n) (alpha-char-p (char s j))) do (incf j))
                  (let ((rep (gethash (subseq s i j) *tex-symbols*)))
                    (write-string (or rep (subseq s (1+ i) j)) out)   ; unknown: keep the name
                    (when (and (< j n) (char= (char s j) #\Space)) (incf j))
                    (setf i j))))
               (t (let ((d (if (< (1+ i) n) (char s (1+ i)) #\Space)))
                    (case d ((#\, #\; #\: #\Space) (write-char #\Space out))
                            ((#\{ #\} #\% #\& #\# #\$ #\_ #\^) (write-char d out))
                            (t nil))                                  ; \( \) \[ \] \! -> drop
                    (setf i (+ i 2))))))
            ((char= c #\_)
             (multiple-value-bind (txt j) (%tex-script-arg s (1+ i))
               (let ((u (tex->unicode txt)))
                 (write-string (if (and (= (length u) 1) (gethash (char u 0) *tex-subscripts*))
                                   (string (gethash (char u 0) *tex-subscripts*))
                                   (concatenate 'string "_" u))
                               out))
               (setf i j)))
            ((char= c #\^)
             (multiple-value-bind (txt j) (%tex-script-arg s (1+ i))
               (let ((u (tex->unicode txt)))
                 (write-string (if (and (= (length u) 1) (gethash (char u 0) *tex-superscripts*))
                                   (string (gethash (char u 0) *tex-superscripts*))
                                   (concatenate 'string "^" u))
                               out))
               (setf i j)))
            ((or (char= c #\{) (char= c #\}) (char= c #\&)) (incf i))   ; grouping/align -> drop
            (t (write-char c out) (incf i))))))))

(defun demath-text (s)
  "Replace \\(...\\) and \\[...\\] LaTeX runs in S with their Unicode transliteration."
  (if (or (search "\\(" s) (search "\\[" s))
      (let ((out (make-string-output-stream)) (i 0))
        (loop
          (let* ((p1 (search "\\(" s :start2 i)) (p2 (search "\\[" s :start2 i))
                 (p (cond ((and p1 p2) (min p1 p2)) (t (or p1 p2)))))
            (if (null p)
                (progn (write-string s out :start i) (return))
                (let* ((disp (eql p p2))
                       (close (search (if disp "\\]" "\\)") s :start2 (+ p 2))))
                  (if (null close)
                      (progn (write-string s out :start i) (return))
                      (progn (write-string s out :start i :end p)
                             (write-string (tex->unicode (subseq s (+ p 2) close)) out)
                             (setf i (+ close 2))))))))
        (get-output-stream-string out))
      s))

(defun demath-dom (node)
  "Transliterate MathJax LaTeX in every text node under NODE (skips code-ish elements)."
  (case (h:dnode-kind node)
    (:text (let ((d (h:dnode-data node)))
             (when (and d (or (search "\\(" d) (search "\\[" d)))
               (setf (h:dnode-data node) (demath-text d)))))
    (:element (unless (member (string-downcase (h:dnode-name node))
                              '("script" "style" "pre" "code" "textarea") :test #'string=)
                (loop for c across (h:dnode-children node) do (demath-dom c))))
    (t (loop for c across (h:dnode-children node) do (demath-dom c)))))

(defun load-page (html &key (css "") (base "") (width 1024) (viewport-height 768) url loader image-loader)
  "Parse HTML, build a fresh scripting context, run inline <script> and drain the
   initial timer/microtask queue, then render.  Returns a live PAGE.  If the scripts
   leave a degenerate render (SPA hydration cut mid-flight blanks or overlays the page),
   re-render the original markup without scripts."
  (let* ((doc (let ((d (h:parse-html html))) (demath-dom d) d))   ; MathJax LaTeX -> Unicode
         (ssr-text (dom-text-length doc))   ; content the server-rendered markup carries
         ;; prefetch external CSS/JS in parallel before the cascade needs them
         (loader (if (and loader (plusp (length base)))
                     (make-prefetching-loader doc base loader)
                     loader))
         (ctx (ws:make-context doc :css css :width width :base base :loader loader))
         (pg (make-page :html html :css (or css "") :base base :doc doc :ctx ctx
                        :width width :viewport-height viewport-height
                        :url url :loader loader
                        :image-loader (or image-loader (make-image-loader base)))))
    ;; Run the page's scripts under a wall-clock budget: a raster view doesn't need a
    ;; fully-settled JS app, and some pages spin for seconds.  On timeout — or an
    ;; uncaught script error — we render the DOM as it stands rather than blank the page
    ;; (a real browser reports the error and paints anyway); the error is kept on the
    ;; page for the caller to surface/log.
    (handler-case
        (sb-ext:with-timeout *js-budget*
          (ws:run-inline-scripts ctx)
          (ws:pump-timers ctx 0)          ; settle 0-delay tasks/microtasks; future timers wait
          (ws:fire-lifecycle-events ctx)) ; DOMContentLoaded + load, so on-ready code runs
      (sb-ext:timeout () (setf (page-js-error pg) "script budget exceeded"))
      (error (e) (setf (page-js-error pg) (princ-to-string e))))
    (render-page pg)
    ;; SPA hydration cut mid-flight by the budget can wreck the render — the content
    ;; collapses to near-nothing while the markup plainly carried a full article.  When
    ;; that happens, discard the mutated DOM and render the original markup without
    ;; scripts.  (Height, not ink: a dark-themed page legitimately inks most pixels.)
    (when (and (> ssr-text 500) (< (r:canvas-height (page-canvas pg)) 400))
      (let ((doc2 (let ((d (h:parse-html html))) (demath-dom d) d)))
        (setf (page-doc pg) doc2
              (page-ctx pg) (ws:make-context doc2 :css css :width width :base base :loader loader)
              (page-js-error pg) "scripts left a degenerate render; rendered the static markup")
        (render-page pg)))
    (setf (page-title pg) (or (document-title (page-doc pg)) url "loom"))
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
              (values (subresource-kind abs) (fetch:fetch-text abs))
              (values nil nil)))
      (error () (values nil nil)))))

(defun subresource-kind (abs)
  ;; test the extension on the path, not the whole URL — a query string (news.css?hash)
  ;; hides the suffix otherwise.
  (let ((path (subseq abs 0 (or (position #\? abs) (length abs)))))
    (cond ((url-suffix-p ".css" path) :css)
          ((or (url-suffix-p ".js" path) (search "only=scripts" abs)) :js)
          (t :text))))

(defun subresource-urls (doc base)
  "Absolute http(s) URLs of external stylesheets and scripts declared in DOC."
  (let ((urls '()))
    (labels ((add (attr node)
               (let ((v (dom:get-attribute node attr)))
                 (when (and v (plusp (length v)))
                   (let ((abs (or (resolve-url v base) v)))
                     (when (and abs (or (url-prefix-p "http:" abs) (url-prefix-p "https:" abs)))
                       (push abs urls)))))))
      (dolist (n (css:query-select-all doc "link[rel=stylesheet]")) (add "href" n))
      (dolist (n (css:query-select-all doc "script[src]")) (add "src" n)))
    (remove-duplicates (nreverse urls) :test #'string=)))

(defparameter *max-concurrent-fetches* 12
  "Cap on simultaneous subresource connections during a parallel prefetch.")

(defun parallel-fetch (urls)
  "Fetch URLS concurrently over seal (each its own connection), at most
   *MAX-CONCURRENT-FETCHES* at a time; return a hash-table abs-url -> text (NIL on
   failure)."
  (let ((cache (make-hash-table :test 'equal))
        (lock (sb-thread:make-mutex))
        (sem (sb-thread:make-semaphore :count *max-concurrent-fetches*)))
    (let ((threads (mapcar (lambda (u)
                             (sb-thread:make-thread
                              (lambda ()
                                (sb-thread:wait-on-semaphore sem)
                                (unwind-protect
                                    (let ((text (handler-case (fetch:fetch-text u) (error () nil))))
                                      (sb-thread:with-mutex (lock) (setf (gethash u cache) text)))
                                  (sb-thread:signal-semaphore sem)))
                              :name "prefetch"))
                           urls)))
      (dolist (th threads) (ignore-errors (sb-thread:join-thread th))))
    cache))

(defun make-prefetching-loader (doc base fallback)
  "Fetch every external stylesheet/script in DOC concurrently up front, then serve them
   from that cache — turning N serial TLS fetches into one parallel batch.  Anything not
   prefetched (dynamic, data:, script-added) falls through to FALLBACK."
  (let ((cache (parallel-fetch (subresource-urls doc base))))
    (lambda (ctx u)
      (let ((abs (or (resolve-url u base) u)))
        (multiple-value-bind (text present) (gethash abs cache)
          (if (and present text)
              (values (subresource-kind abs) text)
              (funcall fallback ctx u)))))))

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
