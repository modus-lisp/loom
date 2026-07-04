;;;; src/sdl-ffi.lisp — minimal hand-written CFFI bindings to libSDL2.
;;;;
;;;; loom needs a native window, a streaming RGB24 texture and an event pump —
;;;; a dozen SDL2 entry points, not the whole API.  cl-sdl2 pulls those in
;;;; through cl-autowrap, which shells out to c2ffi (an LLVM tool) at build time
;;;; to regenerate bindings from the SDL headers; c2ffi is painful to install
;;;; (especially on macOS).  So instead we bind the handful of functions we use
;;;; directly with plain CFFI: no c2ffi, no autowrap, no :sdl2 — just cffi (pure
;;;; Quicklisp) and the SDL2 shared library (brew install sdl2 / libSDL2-2.0.so).
;;;;
;;;; This is the ONLY file in the whole stack that touches FFI.  Everything above
;;;; it (shell.lisp) speaks the small Lisp API exported from LOOM.SDL.
(in-package #:loom.sdl)

;;; ---------------------------------------------------------------------------
;;; The library
;;; ---------------------------------------------------------------------------
;; Homebrew installs SDL2 outside the default dlopen search path, so make those
;; directories visible before loading (Apple-Silicon /opt/homebrew, Intel
;; /usr/local).  Harmless on Linux, where the loader finds libSDL2-2.0.so.0.
(dolist (dir '(#p"/opt/homebrew/lib/" #p"/usr/local/lib/"))
  (pushnew dir cffi:*foreign-library-directories* :test #'equal))

(cffi:define-foreign-library libsdl2
  (:darwin (:or "libSDL2.dylib" "libSDL2-2.0.0.dylib" "libSDL2-2.0.dylib"))
  (:unix (:or "libSDL2-2.0.so.0" "libSDL2.so"))
  (t (:default "libSDL2")))

(cffi:use-foreign-library libsdl2)

;;; ---------------------------------------------------------------------------
;;; Constants (from the SDL 2.x ABI — SDL_video.h / SDL_render.h / SDL_pixels.h)
;;; ---------------------------------------------------------------------------
(defconstant +init-video+                #x00000020) ; SDL_INIT_VIDEO
(defconstant +windowpos-centered+        #x2FFF0000) ; SDL_WINDOWPOS_CENTERED
(defconstant +window-shown+              #x00000004) ; SDL_WINDOW_SHOWN
(defconstant +window-resizable+          #x00000020) ; SDL_WINDOW_RESIZABLE
(defconstant +renderer-software+         #x00000001) ; SDL_RENDERER_SOFTWARE
(defconstant +renderer-accelerated+      #x00000002) ; SDL_RENDERER_ACCELERATED
(defconstant +renderer-presentvsync+     #x00000004) ; SDL_RENDERER_PRESENTVSYNC
(defconstant +pixelformat-rgb24+         #x17101803) ; SDL_PIXELFORMAT_RGB24
(defconstant +textureaccess-streaming+   1)          ; SDL_TEXTUREACCESS_STREAMING

;; Event types (SDL_events.h)
(defconstant +quit+              #x100)
(defconstant +windowevent+       #x200)
(defconstant +keydown+           #x300)
(defconstant +keyup+             #x301)
(defconstant +textinput+         #x303)
(defconstant +mousemotion+       #x400)
(defconstant +mousebuttondown+   #x401)
(defconstant +mousebuttonup+     #x402)
(defconstant +mousewheel+        #x403)

;; An SDL_Event is a 56-byte union in SDL 2.x; SDL_PollEvent writes into it.
(defconstant +event-size+ 56)

;;; ---------------------------------------------------------------------------
;;; Foreign functions — pointers are opaque (SDL_Window*/Renderer*/Texture*).
;;; ---------------------------------------------------------------------------
(cffi:defcfun ("SDL_SetMainReady" %set-main-ready) :void)
(cffi:defcfun ("SDL_Init" %init) :int (flags :uint32))
(cffi:defcfun ("SDL_Quit" %quit) :void)
(cffi:defcfun ("SDL_GetError" %get-error) :string)

(cffi:defcfun ("SDL_CreateWindow" %create-window) :pointer
  (title :string) (x :int) (y :int) (w :int) (h :int) (flags :uint32))
(cffi:defcfun ("SDL_DestroyWindow" %destroy-window) :void (window :pointer))
(cffi:defcfun ("SDL_SetWindowTitle" %set-window-title) :void
  (window :pointer) (title :string))
(cffi:defcfun ("SDL_GetWindowSize" %get-window-size) :void
  (window :pointer) (w :pointer) (h :pointer))

(cffi:defcfun ("SDL_CreateRenderer" %create-renderer) :pointer
  (window :pointer) (index :int) (flags :uint32))
(cffi:defcfun ("SDL_DestroyRenderer" %destroy-renderer) :void (renderer :pointer))
(cffi:defcfun ("SDL_SetRenderDrawColor" %set-render-draw-color) :int
  (renderer :pointer) (r :uint8) (g :uint8) (b :uint8) (a :uint8))
(cffi:defcfun ("SDL_RenderClear" %render-clear) :int (renderer :pointer))
(cffi:defcfun ("SDL_RenderCopy" %render-copy) :int
  (renderer :pointer) (texture :pointer) (srcrect :pointer) (dstrect :pointer))
(cffi:defcfun ("SDL_RenderPresent" %render-present) :void (renderer :pointer))

(cffi:defcfun ("SDL_CreateTexture" %create-texture) :pointer
  (renderer :pointer) (format :uint32) (access :int) (w :int) (h :int))
(cffi:defcfun ("SDL_DestroyTexture" %destroy-texture) :void (texture :pointer))
(cffi:defcfun ("SDL_UpdateTexture" %update-texture) :int
  (texture :pointer) (rect :pointer) (pixels :pointer) (pitch :int))

(cffi:defcfun ("SDL_PollEvent" %poll-event) :int (event :pointer))
(cffi:defcfun ("SDL_Delay" %delay) :void (ms :uint32))

;;; ---------------------------------------------------------------------------
;;; Errors and pointer checks
;;; ---------------------------------------------------------------------------
(define-condition sdl-error (error)
  ((what :initarg :what :reader sdl-error-what)
   (detail :initarg :detail :reader sdl-error-detail))
  (:report (lambda (c s)
             (format s "SDL ~a failed: ~a" (sdl-error-what c) (sdl-error-detail c)))))

(defun %fail (what)
  (error 'sdl-error :what what :detail (%get-error)))

(defun %check-ptr (ptr what)
  "SDL_Create* returns NULL on failure; turn that into an SDL-ERROR."
  (if (cffi:null-pointer-p ptr) (%fail what) ptr))

;;; SDL_Rect is {int x,y,w,h}; UpdateTexture/RenderCopy take a pointer to one.
(cffi:defcstruct rect (x :int) (y :int) (w :int) (h :int))

(defmacro with-rect ((var x y w h) &body body)
  "Bind VAR to a freshly-populated SDL_Rect* for the duration of BODY."
  `(cffi:with-foreign-object (,var '(:struct rect))
     (setf (cffi:foreign-slot-value ,var '(:struct rect) 'x) ,x
           (cffi:foreign-slot-value ,var '(:struct rect) 'y) ,y
           (cffi:foreign-slot-value ,var '(:struct rect) 'w) ,w
           (cffi:foreign-slot-value ,var '(:struct rect) 'h) ,h)
     ,@body))

;;; ---------------------------------------------------------------------------
;;; Lifecycle
;;; ---------------------------------------------------------------------------
(defun init ()
  "Initialize SDL's video subsystem.  We dlopen SDL rather than link the
   SDL_main shim, so tell SDL main is ready before init (required on macOS)."
  (%set-main-ready)
  (unless (zerop (%init +init-video+))
    (%fail "SDL_Init"))
  t)

(defun quit ()
  (%quit))

;;; ---------------------------------------------------------------------------
;;; Window
;;; ---------------------------------------------------------------------------
(defun create-window (title width height &key (resizable t) (shown t))
  "Create a centered WIDTH x HEIGHT window titled TITLE.  Signals SDL-ERROR on
   failure."
  (let ((flags (logior (if shown +window-shown+ 0)
                       (if resizable +window-resizable+ 0))))
    (%check-ptr (%create-window title +windowpos-centered+ +windowpos-centered+
                                width height flags)
                "SDL_CreateWindow")))

(defun destroy-window (window) (%destroy-window window))

(defun set-window-title (window title) (%set-window-title window title))

(defun window-size (window)
  "The window's current (values width height)."
  (cffi:with-foreign-objects ((w :int) (h :int))
    (%get-window-size window w h)
    (values (cffi:mem-ref w :int) (cffi:mem-ref h :int))))

;;; ---------------------------------------------------------------------------
;;; Renderer
;;; ---------------------------------------------------------------------------
(defun %renderer-flags (flags)
  "Fold a list of renderer keywords into the SDL bitmask."
  (let ((bits 0))
    (dolist (f flags bits)
      (setf bits (logior bits (ecase f
                                (:software +renderer-software+)
                                (:accelerated +renderer-accelerated+)
                                (:presentvsync +renderer-presentvsync+)))))))

(defun create-renderer (window &optional flags)
  "Create a renderer for WINDOW.  FLAGS is a list like (:accelerated
   :presentvsync) or (:software).  Signals SDL-ERROR on failure."
  (%check-ptr (%create-renderer window -1 (%renderer-flags flags))
              "SDL_CreateRenderer"))

(defun destroy-renderer (renderer) (%destroy-renderer renderer))

(defun set-render-draw-color (renderer r g b a)
  (%set-render-draw-color renderer r g b a))

(defun render-clear (renderer) (%render-clear renderer))

(defun render-copy (renderer texture &key src dst)
  "Copy TEXTURE to RENDERER.  SRC and DST are (x y w h) lists, or NIL for the
   whole texture / whole target."
  (flet ((call (srcp dstp) (%render-copy renderer texture srcp dstp)))
    (if src
        (with-rect (s (first src) (second src) (third src) (fourth src))
          (if dst
              (with-rect (d (first dst) (second dst) (third dst) (fourth dst))
                (call s d))
              (call s (cffi:null-pointer))))
        (if dst
            (with-rect (d (first dst) (second dst) (third dst) (fourth dst))
              (call (cffi:null-pointer) d))
            (call (cffi:null-pointer) (cffi:null-pointer))))))

(defun render-present (renderer) (%render-present renderer))

;;; ---------------------------------------------------------------------------
;;; Texture (always a streaming RGB24 texture, matching weft's RGB8 canvas)
;;; ---------------------------------------------------------------------------
(defun create-texture (renderer width height)
  "A streaming RGB24 texture of WIDTH x HEIGHT.  Signals SDL-ERROR on failure."
  (%check-ptr (%create-texture renderer +pixelformat-rgb24+
                               +textureaccess-streaming+ width height)
              "SDL_CreateTexture"))

(defun destroy-texture (texture) (%destroy-texture texture))

(defun update-texture (texture x y w h pixels pitch)
  "Upload the W x H region at (X,Y) of TEXTURE from the foreign PIXELS pointer
   (row stride PITCH bytes).  This is the zero-copy blit: PIXELS points straight
   into weft's RGB8 canvas."
  (with-rect (r x y w h)
    (%update-texture texture r pixels pitch)))

;;; ---------------------------------------------------------------------------
;;; Events
;;; ---------------------------------------------------------------------------
;;; SDL_PollEvent fills a 56-byte SDL_Event union.  We read the type (Uint32 at
;;; offset 0) and, per type, the fields loom needs at their fixed SDL 2.x ABI
;;; offsets.  Wrapping this here keeps shell.lisp free of raw offsets: POLL-EVENT
;;; returns a plist (with :TYPE) describing one event, or NIL when the queue is
;;; empty.
(declaim (inline %u8 %u32 %s32))
(defun %u8  (ev off) (cffi:mem-ref ev :uint8  off))
(defun %u32 (ev off) (cffi:mem-ref ev :uint32 off))
(defun %s32 (ev off) (cffi:mem-ref ev :int32  off))

(defun %parse-event (ev)
  "Decode the filled SDL_Event at pointer EV into a plist."
  (let ((type (%u32 ev 0)))
    (cond
      ((= type +quit+) (list :type :quit))
      ;; SDL_MouseButtonEvent: button @16, x @20, y @24
      ((= type +mousebuttondown+)
       (list :type :mousebuttondown :button (%u8 ev 16) :x (%s32 ev 20) :y (%s32 ev 24)))
      ((= type +mousebuttonup+)
       (list :type :mousebuttonup :button (%u8 ev 16) :x (%s32 ev 20) :y (%s32 ev 24)))
      ;; SDL_MouseMotionEvent: x @20, y @24
      ((= type +mousemotion+)
       (list :type :mousemotion :x (%s32 ev 20) :y (%s32 ev 24)))
      ;; SDL_MouseWheelEvent: x @16, y @20
      ((= type +mousewheel+)
       (list :type :mousewheel :x (%s32 ev 16) :y (%s32 ev 20)))
      ;; SDL_KeyboardEvent: keysym @16 => scancode @16, sym @20
      ((= type +keydown+)
       (list :type :keydown :scancode (%s32 ev 16) :sym (%s32 ev 20)))
      ((= type +keyup+)
       (list :type :keyup :scancode (%s32 ev 16) :sym (%s32 ev 20)))
      ;; SDL_TextInputEvent: null-terminated UTF-8 text @12
      ((= type +textinput+)
       (list :type :textinput
             :text (cffi:foreign-string-to-lisp (cffi:inc-pointer ev 12) :max-chars 32)))
      ;; SDL_WindowEvent: event byte @12, data1 @16, data2 @20
      ((= type +windowevent+)
       (list :type :windowevent :event (%u8 ev 12) :data1 (%s32 ev 16) :data2 (%s32 ev 20)))
      (t (list :type :other :raw type)))))

(defun poll-event ()
  "Poll one pending SDL event.  Returns a plist (see %PARSE-EVENT) or NIL when
   the queue is empty."
  (cffi:with-foreign-object (ev :uint8 +event-size+)
    (when (= 1 (%poll-event ev))
      (%parse-event ev))))

(defun delay (ms) (%delay ms))
