;;;; inspect/tests.lisp — headless tests for loom.
;;;;
;;;; Everything here runs without SDL: the pure input/scroll/URL helpers and the
;;;; page model (load a page, hit-test, dispatch a synthetic click/hover, observe
;;;; the DOM react and the scroll math).  The SDL blit path is exercised
;;;; separately by inspect/smoke.lisp under SDL_VIDEODRIVER=dummy.
(defpackage #:loom.test
  (:use #:cl)
  (:local-nicknames (#:h #:weft.html) (#:dom #:weft.dom))
  (:export #:run))
(in-package #:loom.test)

(defvar *pass* 0)
(defvar *fail* 0)

(defun check (name got want &key (test #'equal))
  (if (funcall test got want)
      (progn (incf *pass*) (format t "  ok   ~a~%" name))
      (progn (incf *fail*) (format t "  FAIL ~a~%         got:  ~s~%         want: ~s~%" name got want))))

(defun truthy (name got)
  (if got (progn (incf *pass*) (format t "  ok   ~a~%" name))
      (progn (incf *fail*) (format t "  FAIL ~a (expected non-nil)~%" name))))

;;; ---- pure helpers ---------------------------------------------------------
(defun test-pure ()
  (format t "~&-- pure input / scroll / url --~%")
  (check "button left  -> 0" (loom:sdl-button->dom 1) 0)
  (check "button middle-> 1" (loom:sdl-button->dom 2) 1)
  (check "button right -> 2" (loom:sdl-button->dom 3) 2)
  ;; wheel: +Y (scroll up) => negative (upward) pixel delta
  (check "wheel up  -> -step" (loom:wheel->scroll-delta 1 40) -40)
  (check "wheel down-> +step" (loom:wheel->scroll-delta -2 40) 80)
  ;; clamp
  (check "clamp below 0"        (loom:clamp-scroll -50 2000 768) 0)
  (check "clamp within range"   (loom:clamp-scroll 300 2000 768) 300)
  (check "clamp past bottom"    (loom:clamp-scroll 5000 2000 768) (- 2000 768))
  (check "clamp short page"     (loom:clamp-scroll 100 400 768) 0)
  ;; url resolution
  (check "relative resolves against base"
         (loom:resolve-url "about.html" "http://example.com/dir/index.html")
         "http://example.com/dir/about.html")
  (check "absolute ignores base"
         (loom:resolve-url "https://other.org/x" "http://example.com/")
         "https://other.org/x")
  (check "parent path"
         (loom:resolve-url "../p.html" "http://example.com/a/b/c.html")
         "http://example.com/a/p.html")
  (truthy "bad url -> nil" (null (loom:resolve-url "" "http://example.com/"))))

;;; ---- page model: hit-testing + event dispatch -----------------------------
(defparameter +interactive+ "<!DOCTYPE html><html><head><title>T</title>
<style>#btn{display:inline-block;padding:10px} .box{height:60px}</style></head>
<body>
  <div class='box'><button id='btn'>Press</button>
    <a id='lnk' href='next.html'>go</a></div>
  <p id='p'>hello world</p>
  <script>
    var clicks = 0;
    document.getElementById('btn').addEventListener('click', function(){
      clicks++; document.getElementById('p').textContent = 'clicked ' + clicks;
    });
    var lnk = document.getElementById('lnk');
    lnk.addEventListener('mouseover', function(){ lnk.textContent = 'HOVER'; });
  </script>
</body></html>")

(defun box-center (pg id)
  "A viewport point that hit-tests to element ID (block OR inline), found by
   scanning a coarse grid through the exported NODE-AT — the same path the shell
   uses.  Returns (values x y) or NIL.  Assumes scroll-y 0 (viewport == document)."
  (let ((node (dom:get-element-by-id (loom::page-doc pg) id))
        (w (loom:page-width pg))
        (vh (loom:page-viewport-height pg)))
    (loop for y from 2 below vh by 3 do
      (loop for x from 2 below w by 3 do
        (when (eq (loom:node-at-page pg x y) node)
          (return-from box-center (values x y)))))
    nil))

(defun test-page ()
  (format t "~&-- page model: load / hit-test / dispatch --~%")
  (let ((pg (loom:load-page +interactive+ :width 800 :viewport-height 600
                            :base "http://test.local/dir/" :url "http://test.local/dir/")))
    (truthy "canvas present" (loom:page-canvas pg))
    (truthy "content height > 0" (plusp (loom:page-content-height pg)))
    (check "title read from <title>" (loom:page-title pg) "T")
    ;; hit-test the button by its center
    (multiple-value-bind (bx by) (box-center pg "btn")
      (truthy "button has a box" bx)
      (when bx
        (let ((n (loom:node-at-page pg bx by)))
          (check "node-at button -> <button>"
                 (and n (h:dnode-name n)) "button" :test #'string-equal)
          ;; dispatch a full click; the handler mutates #p
          (loom:mouse-press pg bx by 0)
          (loom:mouse-release pg bx by 0)
          (check "click ran handler (DOM mutated)"
                 (dom:text-content (dom:get-element-by-id (loom::page-doc pg) "p"))
                 "clicked 1")
          ;; second click increments
          (loom:mouse-press pg bx by 0)
          (loom:mouse-release pg bx by 0)
          (check "second click increments"
                 (dom:text-content (dom:get-element-by-id (loom::page-doc pg) "p"))
                 "clicked 2"))))
    ;; hover the link -> mouseover handler + pointer cursor + navigation target
    (multiple-value-bind (lx ly) (box-center pg "lnk")
      (truthy "link has a box" lx)
      (when lx
        (loom:mouse-move pg lx ly)
        (check "mouseover handler ran"
               (dom:text-content (dom:get-element-by-id (loom::page-doc pg) "lnk"))
               "HOVER")
        (check "cursor over link is pointer" (loom:page-cursor pg) "pointer")
        (check "link-at resolves href against base"
               (loom:link-at pg lx ly)
               "next.html" :test (lambda (got want)
                                   (and got (search want got))))))
    ;; navigation callback fires on a link click (default action not prevented)
    (let ((navigated nil))
      (setf (loom:page-on-navigate pg) (lambda (p target) (declare (ignore p)) (setf navigated target)))
      (multiple-value-bind (lx ly) (box-center pg "lnk")
        (when lx (loom:mouse-press pg lx ly 0) (loom:mouse-release pg lx ly 0)))
      (truthy "clicking a link fires on-navigate" navigated))
    ;; scroll math through the model
    (setf (loom::page-content-height pg) 2000)
    (loom:mouse-wheel pg -3)               ; wheel down
    (truthy "wheel scrolled down" (plusp (loom:page-scroll-y pg)))
    (loom:mouse-wheel pg 100)              ; wheel up hard
    (check "wheel clamps at top" (loom:page-scroll-y pg) 0)))

(defun test-timers ()
  (format t "~&-- page model: timers pumped across frames --~%")
  (let ((pg (loom:load-page
             "<title>t</title><body><span id=n>0</span>
              <script>var i=0;function s(){i++;document.getElementById('n').textContent=String(i);
              if(i<3)setTimeout(s,10);} setTimeout(s,10);</script></body>"
             :width 400 :viewport-height 300)))
    ;; at load only 0-delay tasks run; the setTimeout(_,10) chain advances one
    ;; step per frame pump (~16ms of virtual time each), not to the task cap.
    (check "no timer fired at load"
           (dom:text-content (dom:get-element-by-id (loom::page-doc pg) "n")) "0")
    (dotimes (i 5) (loom::pump pg))     ; five frames -> the 3-step chain completes
    (check "timer chain completed after frames"
           (dom:text-content (dom:get-element-by-id (loom::page-doc pg) "n")) "3")))

(defun run ()
  (setf *pass* 0 *fail* 0)
  (format t "~&=== loom headless tests ===~%")
  (test-pure)
  (test-page)
  (test-timers)
  (format t "~&=== ~d passed, ~d failed ===~%" *pass* *fail*)
  (zerop *fail*))
