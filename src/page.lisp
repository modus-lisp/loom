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
  (fragment nil)                        ; the URL #fragment id, if any — scroll target for a viewport-model page
  (loader nil)
  (image-loader nil)                    ; (url) -> (values bytes mime) for network <img>
  (font-loader nil)                     ; (url) -> bytes for an @font-face src (web fonts)
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

(defparameter *js-budget* 6.0
  "Seconds of wall-clock a page's scripts may run during load before the raster is
   taken with the DOM as it stands.")

(defvar *progress* nil
  "When bound to a function of (PHASE &optional DETAIL), the load pipeline calls it
   at phase boundaries — :fetching :parsing :loading :scripting :rendering — so a
   host can surface live progress.  NIL (the default) disables reporting.")

(defun report-progress (phase &optional detail)
  "Call the progress hook if one is bound; never signals (progress is best-effort).
   The fetch (:resolving/:downloading), TLS (:securing) and render
   (:cascade/:layout/:painting) sub-phases are reported by weft and seal directly;
   this reports the loom-level phases in between (:parsing/:loading/:scripting)."
  (when *progress* (ignore-errors (funcall *progress* phase detail))))

;;; ---- navigation instrumentation (for the inspector) ----------------------
;;; Every document/subresource fetch is timed against a per-navigation clock so
;;; the inspector can draw a network waterfall.  NET-LOG-RESET starts the clock;
;;; the worker threads of a parallel prefetch append under a lock.
(defvar *net-log* nil
  "Reverse-chronological (URL START-MS END-MS BYTES OK) for the current
   navigation's fetches.  Reset per navigation; read by the inspector endpoint.")
(defvar *net-log-lock* (sb-thread:make-mutex))
(defvar *net-log-t0* 0 "internal-real-time the *NET-LOG* millisecond stamps are relative to.")

(defun rel-ms (abs) (round (* 1000 (/ (- abs *net-log-t0*) internal-time-units-per-second))))
(defun nav-elapsed-ms () (rel-ms (get-internal-real-time)))
(defun net-log-reset ()
  (sb-thread:with-mutex (*net-log-lock*) (setf *net-log* nil *net-log-t0* (get-internal-real-time))))
(defun net-log-add (url start end bytes ok &optional (kind :other))
  (sb-thread:with-mutex (*net-log-lock*) (push (list url start end bytes ok kind) *net-log*)))

(defun dom-node-counts (node)
  "Return (values element-count text-node-count) in NODE's subtree."
  (let ((els 0) (txt 0))
    (labels ((walk (n)
               (case (h:dnode-kind n) (:element (incf els)) (:text (incf txt)))
               (loop for c across (h:dnode-children n) do (walk c))))
      (walk node))
    (values els txt)))

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

(defun load-page (html &key (css "") (base "") (width 1024) (viewport-height 768) url fragment loader image-loader)
  "Parse HTML, build a fresh scripting context, run inline <script> and drain the
   initial timer/microtask queue, then render.  Returns a live PAGE.  If the scripts
   leave a degenerate render (SPA hydration cut mid-flight blanks or overlays the page),
   re-render the original markup without scripts."
  (let* ((doc (progn (report-progress :parsing)
                     (let ((d (h:parse-html html))) (demath-dom d) d)))   ; MathJax LaTeX -> Unicode
         (ssr-text (dom-text-length doc))   ; content the server-rendered markup carries
         ;; prefetch external CSS/JS in parallel before the cascade needs them
         (loader (if (and loader (plusp (length base)))
                     (progn (report-progress :loading) (make-prefetching-loader doc base loader))
                     loader))
         (ctx (ws:make-context doc :css css :width width :base base :loader loader))
         (pg (make-page :html html :css (or css "") :base base :doc doc :ctx ctx
                        :width width :viewport-height viewport-height
                        :url url :fragment fragment :loader loader
                        :image-loader (or image-loader (make-image-loader base))
                        :font-loader (make-font-loader base)))
         ;; kick off <img> fetches now so they run concurrently with the scripts
         ;; below and are warm before layout — off the critical path.  The whole
         ;; render (prefetch workers AND the main-thread layout) shares one image
         ;; deadline, so slow/hung image URLs can't stall the paint past the budget.
         (img-deadline (+ (get-internal-real-time)
                          (round (* *image-prefetch-budget* internal-time-units-per-second))))
         (r:*image-fetch-deadline* img-deadline)
         ;; @font-face fetches are serial; a script-built sheet can declare hundreds
         ;; of faces (nytimes.com), so bound the time spent on them (fallbacks cover
         ;; the rest) rather than stall the render fetching fonts the viewport skips.
         (r:*font-load-budget* *font-load-budget*)
         (img-threads (start-image-prefetch doc (page-image-loader pg) img-deadline)))
    ;; Run the page's scripts under a wall-clock budget: a raster view doesn't need a
    ;; fully-settled JS app, and some pages spin for seconds.  On timeout — or an
    ;; uncaught script error — we render the DOM as it stands rather than blank the page
    ;; (a real browser reports the error and paints anyway); the error is kept on the
    ;; page for the caller to surface/log.
    (report-progress :scripting)
    (handler-case
        ;; bind the image loader for the script phase too, so an <img> whose src a
        ;; script sets can fetch and fire its load event (onload-driven lazy images).
        (let ((r:*image-loader* (page-image-loader pg)))
         (sb-ext:with-timeout *js-budget*
          (ws:run-inline-scripts ctx)
          (ws:pump-timers ctx 0)          ; settle 0-delay tasks/microtasks; future timers wait
          (ws:fire-lifecycle-events ctx)  ; DOMContentLoaded + load, so on-ready code runs
          ;; then drain the macrotask queue (advancing the virtual clock) so a load
          ;; handler that chains work through setTimeout — a test runner like Acid3,
          ;; SPA bootstrapping — actually runs, not just its first tick.  The wall-clock
          ;; budget above still bounds a self-rescheduling animation/poll loop.
          (ws:run-event-loop ctx :max-tasks 2000000)))
      (sb-ext:timeout () (setf (page-js-error pg) "script budget exceeded"))
      (error (e) (setf (page-js-error pg) (princ-to-string e))))
    ;; wait for the <img> prefetch started before the scripts — usually already
    ;; done, so layout finds every bitmap in cache — but only up to the prefetch
    ;; budget, so a page whose slowest images hang to the read-timeout still paints.
    (report-progress :images)
    (join-threads-until img-threads img-deadline)
    ;; replay a JS Web Font Loader (WebFontConfig) the headless run can't finish,
    ;; so a page's own web fonts (and the .wf-active rules that gate them) apply
    (ignore-errors (apply-web-font-loader pg))
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
                                    (let* ((st (get-internal-real-time))
                                           (text (handler-case (fetch:fetch-text u) (error () nil))))
                                      (net-log-add u (rel-ms st) (nav-elapsed-ms)
                                                   (and text (length text)) (and text t)
                                                   (subresource-kind u))
                                      (sb-thread:with-mutex (lock) (setf (gethash u cache) text)))
                                  (sb-thread:signal-semaphore sem)))
                              :name "prefetch"))
                           urls)))
      (dolist (th threads) (ignore-errors (sb-thread:join-thread th))))
    cache))

(defparameter *image-prefetch-concurrency* 4
  "How many <img> bitmaps to fetch at once during prefetch.  Kept low: each fetch
   to a new host pays a CPU-bound pure-CL TLS handshake, and flooding both starves
   the cores and defeats keep-alive connection reuse, so a wide fan-out is slower
   than a handful of reusing workers.")

(defparameter *font-load-budget* 5.0
  "Seconds a render may spend fetching @font-face web fonts before falling back for
   the rest (see WEFT.RENDER:*FONT-LOAD-BUDGET*).  Bounds a page that injects a huge
   font sheet (nytimes.com) without hurting a page with a handful of real fonts.")

(defparameter *image-prefetch-budget* 12.0
  "Seconds (from prefetch start) the render will wait for <img> bitmaps before
   painting anyway.  A page with many images — some slow ad/CDN URLs that hang to
   the fetch read-timeout — must not block the whole render on the slowest ones
   (nytimes.com stalled ~85s this way).  Images that miss the budget keep fetching
   and simply aren't in this render.")

(defun join-threads-until (threads deadline)
  "Join THREADS, but stop blocking once the internal-real-time DEADLINE passes:
   already-finished workers are collected, still-running ones are left to finish
   on their own so one slow fetch can't hold the render hostage."
  (dolist (th threads)
    (let ((remaining (/ (- deadline (get-internal-real-time))
                        internal-time-units-per-second 1.0)))
      (if (> remaining 0.02)
          (ignore-errors (sb-thread:join-thread th :timeout remaining :default nil))
          (return)))))            ; budget spent: leave the rest running

(defun start-image-prefetch (doc image-loader deadline)
  "Spawn concurrent fetches for every network <img>, keyed exactly as layout looks
   them up (R:IMG-SOURCE-URL — the raw src/srcset value FETCH-IMAGE caches under),
   and return the threads.  Meant to run ALONGSIDE the script phase so the bitmaps
   are warm by the time layout needs their dimensions, off the critical path.
   Each worker binds *IMAGE-LOADER* and the fetch DEADLINE itself (dynamic bindings
   don't cross threads); FETCH-IMAGE does the caching and honours the deadline."
  (let* ((urls (remove-duplicates
                (loop for n in (css:query-select-all doc "img")
                      for u = (r:img-source-url n)
                      when (and u (not (and (>= (length u) 5) (string-equal (subseq u 0 5) "data:"))))
                        collect u)
                :test #'string=))
         (sem (sb-thread:make-semaphore :count *image-prefetch-concurrency*)))
    (mapcar (lambda (u)
              (sb-thread:make-thread
               (lambda ()
                 (sb-thread:wait-on-semaphore sem)
                 (unwind-protect
                     (let ((r:*image-loader* image-loader) (r:*image-fetch-deadline* deadline))
                       (ignore-errors (r:fetch-image u)))
                   (sb-thread:signal-semaphore sem)))
               :name "img-prefetch"))
            urls)))

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
                         (t (or (resolve-url url base) url))))
              (st (get-internal-real-time)))
          (let* ((resp (fetch:fetch abs))
                 (ok (and resp (<= 200 (fetch:response-status resp) 299)))
                 (body (and resp (fetch:response-body resp))))
            ;; log the image fetch so the inspector attributes "layout" time that is
            ;; really network — <img> dimensions are fetched during layout.
            (net-log-add abs (rel-ms st) (nav-elapsed-ms) (and body (length body)) ok :image)
            (when ok
              (values body (fetch:get-header (fetch:response-headers resp) "content-type")))))
      (error () (values nil nil)))))

(defun make-font-loader (base)
  "An (url) -> bytes fetcher for @font-face `src` web fonts over seal, resolving
   relative and protocol-relative URLs against BASE.  NIL on any failure (the
   render then keeps the bundled fallback face)."
  (lambda (url)
    (handler-case
        (let ((abs (cond ((and (>= (length url) 2) (string= (subseq url 0 2) "//"))
                          (concatenate 'string "https:" url))
                         (t (or (resolve-url url base) url))))
              (st (get-internal-real-time)))
          (let* ((resp (fetch:fetch abs))
                 (ok (and resp (<= 200 (fetch:response-status resp) 299)))
                 (raw (and resp (fetch:response-body resp)))
                 ;; font CDNs (gstatic) may gzip the response even for binary fonts;
                 ;; unlike HTML the font path gets the raw body, so decode the
                 ;; transport Content-Encoding here (gzip magic 1F 8B / deflate).
                 (enc (and resp (fetch:get-header (fetch:response-headers resp) "content-encoding")))
                 (body (font-decode-body raw enc)))
            (net-log-add abs (rel-ms st) (nav-elapsed-ms) (and body (length body)) ok :font)
            (and ok body)))
      (error () nil))))

(defun font-decode-body (bytes enc)
  "Decode a font response BYTES per its Content-Encoding ENC (or a sniffed gzip
magic), returning the raw font file.  Passes through when not compressed."
  (when bytes
    (let ((e (and enc (string-downcase (string-trim '(#\Space) enc)))))
      (cond
        ((or (equal e "gzip") (equal e "x-gzip")
             (and (>= (length bytes) 2) (= (aref bytes 0) #x1F) (= (aref bytes 1) #x8B)))
         (or (ignore-errors (deflate:gzip-decompress bytes)) bytes))
        ((equal e "deflate")
         (or (ignore-errors (deflate:zlib-decompress bytes))
             (ignore-errors (deflate:inflate bytes)) bytes))
        (t bytes)))))

(defun %wfl-slug (family)
  "Web Font Loader class slug for a Google-font spec: the family name up to the
first `:`, lowercased with spaces removed.  \"Fondamento:r:latin\" -> \"fondamento\"."
  (let* ((name (subseq family 0 (or (position #\: family) (length family)))))
    (remove #\Space (string-downcase (string-trim '(#\Space) name)))))

(defun %set-class (node value)
  "Set NODE's class attribute to VALUE (mutating its attr alist)."
  (let ((cell (assoc "class" (h:dnode-attrs node) :test #'string-equal)))
    (if cell (setf (cdr cell) value)
        (setf (h:dnode-attrs node) (cons (cons "class" value) (h:dnode-attrs node))))))

(defun apply-web-font-loader (pg)
  "Replay the Web Font Loader (WebFontConfig) a headless render can't finish: fetch
the declared font CSS, register the @font-face faces, and flip the <html> class from
`wf-loading` to `wf-active` (plus the per-family `wf-<slug>-n4-active`) so the theme's
`.wf-active …{font-family:…}` rules apply.  A no-op when the page declares no config.
Returns T when a config was found and applied."
  (let ((ctx (page-ctx pg)))
    (multiple-value-bind (api fams) (ws:web-font-config ctx)
      (when (and api fams)
        (report-progress :fonts)
        (let ((css (with-output-to-string (o)
                     (dolist (fam fams)
                       (let* ((u (format nil "~a?family=~a" api (substitute #\+ #\Space fam)))
                              (txt (ignore-errors (nth-value 0 (fetch:fetch-text u)))))
                         (when txt (write-string txt o) (terpri o)))))))
          ;; register the web fonts named by the fetched @font-face CSS
          (let ((r:*font-loader* (page-font-loader pg)))
            (ignore-errors (r:load-font-faces (css:parse-stylesheet css))))
          ;; activate the theme's web-font rules on <html>
          (let ((html (css:query-select (page-doc pg) "html")))
            (when html
              (let ((classes (remove-if (lambda (c) (member c '("wf-loading" "wf-inactive") :test #'string=))
                                        (loom-split-ws (or (cdr (assoc "class" (h:dnode-attrs html) :test #'string-equal)) "")))))
                (pushnew "wf-active" classes :test #'string=)
                (dolist (f fams) (pushnew (format nil "wf-~a-n4-active" (%wfl-slug f)) classes :test #'string=))
                (%set-class html (format nil "~{~a~^ ~}" (nreverse classes))))))
          t)))))

(defun loom-split-ws (s)
  "Split S on ASCII whitespace into non-empty tokens."
  (let ((out '()) (start nil) (n (length s)))
    (dotimes (i n)
      (if (member (char s i) '(#\Space #\Tab #\Newline #\Return))
          (when start (push (subseq s start i) out) (setf start nil))
          (unless start (setf start i))))
    (when start (push (subseq s start n) out))
    (nreverse out)))

(defun load-url (url-string &key (width 1024) (viewport-height 768))
  "Fetch and load URL-STRING as a fresh page (the network browsing entry)."
  ;; Scope the fine network hooks to the MAIN document fetch: the resolve/TLS/
  ;; download detail is for the page itself, not for the many subresources that
  ;; load-page fetches afterward (those are summarized by :loading / :scripting).
  (let ((st (get-internal-real-time)))
   (multiple-value-bind (text charset resp)
      (let ((fetch:*progress* #'report-progress) (seal:*progress* #'report-progress))
        (fetch:fetch-text url-string))
    (declare (ignore charset))
    (net-log-add url-string (rel-ms st) (nav-elapsed-ms) (and text (length text)) (and text t) :document)
    (let* ((final (or (and resp (fetch:response-url resp)) url-string))
           ;; the #fragment is client-side (never sent) — carry it as the scroll
           ;; target so a viewport-model page (e.g. Acid2's test.html#top) composes.
           (hash (position #\# url-string))
           (frag (and hash (< (1+ hash) (length url-string)) (subseq url-string (1+ hash)))))
      (load-page text :base final :url final :fragment frag
                 :width width :viewport-height viewport-height
                 :loader (make-http-loader final))))))

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
      (let ((r:*image-loader* (page-image-loader pg))   ; network <img> over seal, cached
            (r:*font-loader* (page-font-loader pg))     ; @font-face web fonts over seal
            (r:*progress* #'report-progress)            ; :cascade / :layout / :painting
            (fetch:*progress* nil) (seal:*progress* nil)) ; image/font fetches here aren't the document download
        ;; VIEWPORT-HEIGHT/SCROLL-TO only take effect when the page clips at the root
        ;; (a viewport-model page like Acid2); a normal page ignores them and its
        ;; canvas still grows to content height (reader view).
        (r:render-document (page-doc pg) :width (page-width pg) :css (page-css pg)
                           :viewport-height (page-viewport-height pg)
                           :scroll-to (page-fragment pg)))
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
