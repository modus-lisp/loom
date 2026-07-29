;;;; inspect/forms-interact.lisp — can a user actually FILL IN a form?
;;;;
;;;; The forms IDL is separately gated by weft's forms oracle (~2800 WPT
;;;; subtests), but that oracle only ever reads the DOM.  It cannot see whether
;;;; a control's live value reaches the SCREEN, whether a real click toggles a
;;;; checkbox, or whether a keystroke lands anywhere — the whole interactive
;;;; path is invisible to it.  This gate drives loom the way a person does
;;;; (click at pixel coordinates, press keys) and asserts on both the DOM and
;;;; the painted pixels.
;;;;
;;;; It runs as part of `asdf:test-system "loom"'.  Standalone:
;;;;   sbcl --dynamic-space-size 4096 --noinform --non-interactive \
;;;;     --eval '(asdf:load-system "loom")' --load inspect/forms-interact.lisp \
;;;;     --eval '(unless (loom.forms-interact:run) (sb-ext:exit :code 1))'
(defpackage #:loom.forms-interact
  (:use #:cl)
  (:local-nicknames (#:h #:weft.html) (#:r #:weft.render) (#:ws #:weft.script))
  (:export #:run))
(in-package #:loom.forms-interact)

(defvar *pass* 0)
(defvar *fail* 0)

(defun check (name got want &key (test #'equal))
  (if (funcall test got want)
      (progn (incf *pass*) (format t "  ok   ~a~@[ = ~a~]~%" name (and (stringp got) got)))
      (progn (incf *fail*) (format t "  FAIL ~a: got ~s, want ~s~%" name got want))))

;;; ---- helpers ---------------------------------------------------------------
(defun js (pg expr)
  (shuttle:eval-script (ws:context-realm (loom::page-ctx pg)) expr))
(defun jstr (pg expr) (let ((v (js pg expr))) (if (stringp v) v (princ-to-string v))))
(defun jbool (pg expr) (shuttle:js-truthy (js pg expr)))

(defun box-of (pg id)
  "The laid-out box for the element with ID (the widget's own box)."
  (labels ((walk (b)
             (let ((n (r:lbox-node b)))
               (when (and n (eq (h:dnode-kind n) :element)
                          (equal (cdr (assoc "id" (h:dnode-attrs n) :test #'string-equal)) id))
                 (return-from box-of b)))
             (dolist (c (r:lbox-children b)) (walk c))))
    (walk (loom:page-root pg))
    nil))

(defun ink-in (pg id)
  "Count of non-white pixels inside ID's box — how much is actually PAINTED
   there.  This is the assertion the DOM-only oracles cannot make."
  (let* ((b (or (box-of pg id) (error "no box for ~a" id)))
         (cv (loom:page-canvas pg))
         (px (r:canvas-pixels cv)) (cw (r:canvas-width cv)) (ch (r:canvas-height cv))
         (n 0))
    (loop for y from (max 0 (round (r:lbox-y b))) below (min ch (round (+ (r:lbox-y b) (r:lbox-h b))))
          do (loop for x from (max 0 (round (r:lbox-x b))) below (min cw (round (+ (r:lbox-x b) (r:lbox-w b))))
                   for i = (* 3 (+ x (* y cw)))
                   unless (and (> (aref px i) 250) (> (aref px (+ i 1)) 250) (> (aref px (+ i 2)) 250))
                     do (incf n)))
    n))

(defun click (pg id &key (dx 4) (dy 4))
  "Click the real pixels of ID's widget, as a pointer would."
  (let* ((b (or (box-of pg id) (error "no box for ~a" id)))
         (x (+ (round (r:lbox-x b)) dx)) (y (+ (round (r:lbox-y b)) dy)))
    (loom:mouse-press pg x y 0)
    (loom:mouse-release pg x y 0)))

(defun typing (pg text)
  "Type TEXT one printable character at a time, as the shell's on-key does."
  (loop for c across text
        do (loom:key-down pg (string c) :key-code (char-code c))
           (loom:key-text pg (string c))))

(defun press (pg key &optional code shift)
  (loom:key-down pg key :key-code code :shift shift))

(defun col-x (pg id col)
  "The x of text column COL inside ID's widget — the 4px inset the painters use."
  (+ (round (r:lbox-x (or (box-of pg id) (error "no box for ~a" id))))
     4 (* col r::*font-w*)))

(defun drag (pg id from-col to-col &key (dy 6))
  "Press at text column FROM-COL, move to TO-COL with the button held, release."
  (let ((y (+ (round (r:lbox-y (box-of pg id))) dy)))
    (loom:mouse-press pg (col-x pg id from-col) y 0)
    (loom:mouse-move pg (col-x pg id to-col) y)
    (loom:mouse-release pg (col-x pg id to-col) y 0)))

(defun sel (pg id)
  "ID's selection as \"start-end\", read through the IDL — the same numbers the
   caret is painted from, which is the point of asserting on them here."
  (jstr pg (format nil "(function(e){return e.selectionStart+'-'+e.selectionEnd})~
                        (document.getElementById('~a'))" id)))

(defun page (html &key (width 600))
  (let ((pg (loom:load-page html :url "about:forms-interact" :width width)))
    (loom:render-page pg)
    pg))

;;; ---- 1. the painter shows LIVE state, not the default ----------------------
(defun test-live-value-painted ()
  (format t "~&-- live value reaches the screen --~%")
  (let ((pg (page "<!doctype html><body>
<input id=t type=text size=20>
<input id=d type=text size=20 value=\"default\">
<textarea id=a rows=2 cols=20></textarea>")))
    (let ((empty (ink-in pg "t")))
      ;; a `value' ATTRIBUTE has always painted; the live value is the new part
      (check "default value paints" (> (ink-in pg "d") empty) t)
      (js pg "document.getElementById('t').value = 'script set me'")
      (js pg "document.getElementById('a').value = 'in the textarea'")
      (loom:render-page pg)
      (check "script-set input value paints" (> (ink-in pg "t") empty) t)
      (check "script-set textarea value paints" (> (ink-in pg "a") 20) t)
      ;; and it must not disturb the DOM-side truth
      (check "value IDL still right" (jstr pg "document.getElementById('t').value")
             "script set me"))))

(defun test-live-checked-painted ()
  (format t "~&-- live checkedness reaches the screen --~%")
  (let ((pg (page "<!doctype html><body><input id=c type=checkbox>")))
    (let ((unchecked (ink-in pg "c")))
      (js pg "document.getElementById('c').checked = true")
      (loom:render-page pg)
      (check "script-set checked paints a tick" (> (ink-in pg "c") unchecked) t))))

;;; ---- 2. a real click runs activation behaviour -----------------------------
(defun test-click-activation ()
  (format t "~&-- a real click activates the control --~%")
  (let ((pg (page "<!doctype html><body>
<input id=c type=checkbox>
<input id=r1 type=radio name=g><input id=r2 type=radio name=g>
<input id=p type=checkbox onclick=\"event.preventDefault()\">")))
    (let ((unchecked (ink-in pg "c")))
      (click pg "c")
      (check "click toggles checkbox (DOM)" (jbool pg "document.getElementById('c').checked") t)
      (loom:render-page pg)
      (check "click toggles checkbox (pixels)" (> (ink-in pg "c") unchecked) t)
      (click pg "c")
      (check "click again unchecks" (jbool pg "document.getElementById('c').checked") nil))
    (click pg "r1")
    (check "click selects radio" (jbool pg "document.getElementById('r1').checked") t)
    (click pg "r2")
    (check "radio group is exclusive" (jbool pg "document.getElementById('r1').checked") nil)
    (check "  ... the other is on" (jbool pg "document.getElementById('r2').checked") t)
    (click pg "p")
    (check "preventDefault blocks the toggle" (jbool pg "document.getElementById('p').checked") nil)))

(defun test-click-events ()
  (format t "~&-- activation fires input/change, in order, after the click --~%")
  (let ((pg (page "<!doctype html><body><input id=c type=checkbox>
<script>window.log=[];var c=document.getElementById('c');
for (var t of ['click','input','change'])
  c.addEventListener(t,function(e){window.log.push(e.type+':'+c.checked)});
</script>")))
    (click pg "c")
    (check "event order + checkedness visible to handlers"
           (jstr pg "window.log.join(',')") "click:true,input:true,change:true")))

;;; ---- 3. focus -------------------------------------------------------------
(defun test-focus ()
  (format t "~&-- clicking a control focuses it --~%")
  (let ((pg (page "<!doctype html><body>
<input id=t type=text size=20><input id=u type=text size=20>
<script>window.log=[];
for (var id of ['t','u']) { var e=document.getElementById(id);
  e.addEventListener('focus',function(ev){window.log.push('focus:'+ev.target.id)});
  e.addEventListener('blur', function(ev){window.log.push('blur:'+ev.target.id)}); }
</script>")))
    (check "activeElement starts at body" (jstr pg "document.activeElement.tagName") "BODY")
    (click pg "t")
    (check "click focuses the input" (jstr pg "document.activeElement.id") "t")
    (check "focus event fired" (jstr pg "window.log.join(',')") "focus:t")
    (click pg "u")
    (check "focus moves, blurring the old" (jstr pg "window.log.join(',')")
           "focus:t,blur:t,focus:u")
    (js pg "document.getElementById('u').blur()")
    (check "blur() returns focus to body" (jstr pg "document.activeElement.tagName") "BODY")
    (js pg "document.getElementById('t').focus()")
    (check "focus() works from script" (jstr pg "document.activeElement.id") "t")))

;;; ---- 4. typing ------------------------------------------------------------
(defun test-typing ()
  (format t "~&-- typing into a focused field --~%")
  (let ((pg (page "<!doctype html><body><input id=t type=text size=20>
<textarea id=a rows=2 cols=20></textarea>
<script>window.n=0;document.getElementById('t')
  .addEventListener('input',function(){window.n++});</script>")))
    (let ((empty (ink-in pg "t")))
      (click pg "t")
      (typing pg "hello")
      (check "typed text lands in .value" (jstr pg "document.getElementById('t').value") "hello")
      (check "one input event per keystroke" (jstr pg "String(window.n)") "5")
      (loom:render-page pg)
      (check "typed text is painted" (> (ink-in pg "t") empty) t)
      (press pg "Backspace" 8)
      (check "Backspace deletes" (jstr pg "document.getElementById('t').value") "hell")
      (press pg "Home" 36) (typing pg "S")
      (check "Home + type inserts at the start"
             (jstr pg "document.getElementById('t').value") "Shell")
      ;; caret is at 1 (after the S); ArrowRight puts it at 2, so Delete takes
      ;; the `e' — Delete removes the character AFTER the caret, not the one it
      ;; just passed over
      (press pg "ArrowRight" 39) (press pg "Delete" 46)
      (check "ArrowRight + Delete removes the next char"
             (jstr pg "document.getElementById('t').value") "Shll")
      (press pg "End" 35) (typing pg "!")
      (check "End + type appends" (jstr pg "document.getElementById('t').value") "Shll!"))
    ;; keys must not leak into the page when nothing is focused
    (js pg "document.getElementById('t').blur()")
    (typing pg "xyz")
    (check "typing with nothing focused changes nothing"
           (jstr pg "document.getElementById('t').value") "Shll!")
    ;; textarea takes text too
    (click pg "a") (typing pg "note")
    (check "textarea accepts typing" (jstr pg "document.getElementById('a').value") "note")))

(defun test-change-on-commit ()
  (format t "~&-- change fires on commit, not on every keystroke --~%")
  (let ((pg (page "<!doctype html><body><input id=t type=text size=20><input id=u type=text size=8>
<script>window.ch=0;document.getElementById('t')
  .addEventListener('change',function(){window.ch++});</script>")))
    (click pg "t")
    (typing pg "abc")
    (check "no change while typing" (jstr pg "String(window.ch)") "0")
    (click pg "u")                       ; blur commits
    (check "change fires on blur" (jstr pg "String(window.ch)") "1")
    (click pg "t") (typing pg "d") (press pg "Enter" 13)
    (check "change fires on Enter" (jstr pg "String(window.ch)") "2")
    (press pg "Enter" 13)
    (check "Enter with no edit does not re-fire" (jstr pg "String(window.ch)") "2")))

(defun test-readonly-disabled ()
  (format t "~&-- readonly / disabled reject input --~%")
  (let ((pg (page "<!doctype html><body>
<input id=r type=text size=20 value=\"keep\" readonly>
<input id=d type=text size=20 value=\"keep\" disabled>
<input id=x type=checkbox disabled>")))
    (click pg "r") (typing pg "no")
    (check "readonly field rejects typing" (jstr pg "document.getElementById('r').value") "keep")
    (click pg "d") (typing pg "no")
    (check "disabled field rejects typing" (jstr pg "document.getElementById('d').value") "keep")
    (click pg "x")
    (check "disabled checkbox rejects clicks" (jbool pg "document.getElementById('x').checked") nil)))

;;; ---- 5. selection ----------------------------------------------------------
;;; The caret and `input.selectionStart' are ONE selection.  Every assertion here
;;; reads the IDL after driving the keyboard/pointer, so a shell that kept its
;;; own private caret would fail on the first one.
(defun test-selection ()
  (format t "~&-- shift-arrows and drags select text --~%")
  (let ((pg (page "<!doctype html><body><input id=t type=text size=20 value=\"hello world\">")))
    (click pg "t") (press pg "Home" 36)
    (check "Home collapses at the start" (sel pg "t") "0-0")
    (loom:render-page pg)
    (let ((caret-only (ink-in pg "t")))       ; same text, same caret, no highlight
      (dotimes (i 5) (press pg "ArrowRight" 39 t))
      (check "shift-ArrowRight extends the selection" (sel pg "t") "0-5")
      (loom:render-page pg)
      (check "the highlight is painted" (> (ink-in pg "t") caret-only) t))
    ;; typing replaces what is selected — the whole reason a selection exists
    (typing pg "HELLO")
    (check "typing replaces the selection"
           (jstr pg "document.getElementById('t').value") "HELLO world")
    (check "  ... and leaves the caret after it" (sel pg "t") "5-5")
    ;; a plain arrow collapses rather than stepping from the moving edge
    (press pg "Home" 36)
    (dotimes (i 3) (press pg "ArrowRight" 39 t))
    (press pg "ArrowLeft" 37)
    (check "a plain arrow collapses the selection to its near edge" (sel pg "t") "0-0")
    ;; Backspace over a selection removes the selection, not one more character
    (press pg "End" 35)
    (dotimes (i 6) (press pg "ArrowLeft" 37 t))
    (check "shift-ArrowLeft selects backwards" (sel pg "t") "5-11")
    (press pg "Backspace" 8)
    (check "Backspace deletes the selection whole"
           (jstr pg "document.getElementById('t').value") "HELLO")
    ;; select() from script, then type over it
    (js pg "document.getElementById('t').select()")
    (check "select() selects the value" (sel pg "t") "0-5")
    (typing pg "x")
    (check "typing replaces a script-made selection"
           (jstr pg "document.getElementById('t').value") "x")))

(defun test-drag-selection ()
  (format t "~&-- dragging the pointer selects --~%")
  (let ((pg (page "<!doctype html><body><input id=t type=text size=20 value=\"drag over me\">")))
    (drag pg "t" 0 4)
    (check "drag selects the columns crossed" (sel pg "t") "0-4")
    (check "  ... and focus went to the field" (jstr pg "document.activeElement.id") "t")
    (drag pg "t" 9 5)
    (check "dragging leftwards selects backwards too" (sel pg "t") "5-9")
    (typing pg "X")
    (check "typing replaces the dragged selection"
           (jstr pg "document.getElementById('t').value") "drag X me")
    ;; a plain click collapses it again
    (click pg "t" :dx 4 :dy 6)
    (check "a click collapses the selection" (sel pg "t") "0-0")))

;;; ---- 6. tab order ----------------------------------------------------------
(defun test-tab-order ()
  (format t "~&-- Tab walks the focusable controls --~%")
  (let ((pg (page "<!doctype html><body>
<input id=a type=text size=8><input id=skip type=text size=8 disabled>
<input id=neg type=text size=8 tabindex=-1><input id=b type=text size=8>
<button id=c>go</button><a id=lnk href=\"#x\">link</a>
<input id=first type=text size=8 tabindex=1>")))
    ;; a positive tabindex comes FIRST, whatever the tree order
    (press pg "Tab" 9)
    (check "Tab from nowhere honours tabindex=1" (jstr pg "document.activeElement.id") "first")
    (press pg "Tab" 9)
    (check "then the first control in tree order" (jstr pg "document.activeElement.id") "a")
    (press pg "Tab" 9)
    (check "disabled and tabindex=-1 are skipped" (jstr pg "document.activeElement.id") "b")
    (press pg "Tab" 9)
    (check "a <button> is in the order" (jstr pg "document.activeElement.id") "c")
    (press pg "Tab" 9)
    (check "so is a link with an href" (jstr pg "document.activeElement.id") "lnk")
    (press pg "Tab" 9 t)
    (check "Shift-Tab goes back" (jstr pg "document.activeElement.id") "c")
    ;; Tabbing INTO a field selects its value, so the next keystroke replaces it
    (js pg "document.getElementById('b').value='replace me'")
    (press pg "Tab" 9 t)
    (check "Shift-Tab again reaches the field" (jstr pg "document.activeElement.id") "b")
    (check "tabbing in selects the value" (sel pg "b") "0-10")
    (typing pg "new")
    (check "so typing replaces it" (jstr pg "document.getElementById('b').value") "new")))

(defun test-tab-commits ()
  (format t "~&-- Tab away commits the edit --~%")
  (let ((pg (page "<!doctype html><body><input id=t type=text size=8><input id=u type=text size=8>
<script>window.ch=0;document.getElementById('t')
  .addEventListener('change',function(){window.ch++});</script>")))
    (click pg "t") (typing pg "abc")
    (check "no change while typing" (jstr pg "String(window.ch)") "0")
    (press pg "Tab" 9)
    (check "Tab moves focus" (jstr pg "document.activeElement.id") "u")
    (check "  ... and fires change on the way out" (jstr pg "String(window.ch)") "1")))

(defun test-textarea-lines ()
  (format t "~&-- a textarea is multi-line --~%")
  (let ((pg (page "<!doctype html><body><textarea id=a rows=4 cols=20></textarea>")))
    (click pg "a")
    (typing pg "one") (press pg "Enter" 13) (typing pg "two")
    (check "Enter inserts a newline" (jstr pg "document.getElementById('a').value")
           (format nil "one~atwo" #\Newline))
    (press pg "ArrowUp" 38)
    (check "ArrowUp keeps the column on the line above" (sel pg "a") "3-3")
    (press pg "Home" 36)
    (check "Home goes to the LINE start, not the value start" (sel pg "a") "0-0")
    (press pg "End" 35)
    (check "End goes to the line end" (sel pg "a") "3-3")
    (press pg "ArrowDown" 40)
    (check "ArrowDown comes back" (sel pg "a") "7-7")))

(defun test-form-round-trip ()
  (format t "~&-- a whole form, filled and submitted --~%")
  (let ((pg (page "<!doctype html><body>
<form id=f><input id=u type=text size=20 name=user>
<input id=k type=checkbox name=ok>
<input id=s type=submit value=\"Sign in\"></form>
<script>window.sub=null;document.getElementById('f')
  .addEventListener('submit',function(e){e.preventDefault();
     window.sub=document.getElementById('u').value+'/'+document.getElementById('k').checked});
</script>")))
    (click pg "u") (typing pg "ynniv")
    (click pg "k")
    (click pg "s")
    (check "submit sees everything the user entered" (jstr pg "window.sub") "ynniv/true")))

(defun run ()
  (let ((*pass* 0) (*fail* 0))
    (format t "~&=== loom form-interaction gate ===~%")
    (dolist (f (list #'test-live-value-painted #'test-live-checked-painted
                     #'test-click-activation #'test-click-events
                     #'test-focus #'test-typing #'test-change-on-commit
                     #'test-selection #'test-drag-selection
                     #'test-tab-order #'test-tab-commits #'test-textarea-lines
                     #'test-readonly-disabled #'test-form-round-trip))
      (handler-case (funcall f)
        (error (e) (incf *fail*) (format t "  FAIL (error) ~a~%" e))))
    (format t "~&~%~d passed, ~d failed~%" *pass* *fail*)
    (zerop *fail*)))
