#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.sdl2)

(defclass context (trial:context)
  ((title :initform "" :initarg :title :accessor title)
   (window :initform NIL :accessor window)
   (gl-ctx :initform NIL :accessor gl-ctx)
   (initargs :initform NIL :accessor initargs)
   (mouse-pos :initform (vec2 0 0) :accessor mouse-pos))
  (:default-initargs
   :resizable T
   :visible T
   :decorated T
   :hidpi NIL
   :robustness NIL
   :forward-compat T
   :debug-context NIL))

(defmethod initialize-instance ((context context) &key)
  (call-next-method)
  (create-context context))

(defmethod shared-initialize :after ((context context) slots
                                     &key (version NIL version-p)
                                          (profile NIL profile-p)
                                          (width NIL width-p)
                                          (height NIL height-p)
                                          (double-buffering NIL double-buffering-p)
                                          (stereo-buffer NIL stereo-buffer-p)
                                          (vsync NIL vsync-p)
                                     ;; Extra options
                                          (resizable NIL resizable-p)
                                          (visible NIL visible-p)
                                          (decorated NIL decorated-p)
                                          (hidpi NIL hidpi-p)
                                          (robustness NIL robustness-p)
                                          (forward-compat NIL forward-compat-p)
                                          (debug-context NIL debug-context-p))
  (flet (((setf g) (value name) (setf (getf (initargs context) name) value)))
    (macrolet ((maybe-set (var &optional (name (intern (string var) :keyword)))
                 `(when ,(let ((*print-case* (readtable-case *readtable*)))
                           (intern (format NIL "~a-~a" var 'p)))
                    (setf (g ,name) ,var))))
      (maybe-set width)
      (maybe-set height)
      (maybe-set double-buffering)
      (maybe-set stereo-buffer)
      (maybe-set resizable)
      (maybe-set visible)
      (maybe-set decorated)
      (maybe-set hidpi)
      (maybe-set robustness)
      (maybe-set forward-compat)
      (maybe-set debug-context)
      (when vsync-p
        (setf (g :vsync)
              (ecase vsync (:off 0) (:on 1) (:adaptive -1))))
      (when version-p
        (setf (g :context-version-major) (first version))
        (setf (g :context-version-minor) (second version)))
      (when profile-p
        (setf (g :opengl-profile) (ecase profile
                                    ((NIL) 0)
                                    (:core SDL2-FFI:+SDL-GL-CONTEXT-PROFILE-CORE+)
                                    (:compatibility SDL2-FFI:+SDL-GL-CONTEXT-PROFILE-COMPATIBILITY+)))))))

(defmethod create-context ((context context))
  (v:info :trial.barkend.sdl2 "Creating context ~a" context)
  (let ((initargs (initargs context)))
    (flet ((arg (name) (getf initargs name))
           (bitarg (name bit) (if (getf initargs name) bit 0)))
      (sdl2:gl-set-attr :context-flags
                        (logior (bitarg :robustness SDL2-FFI:+SDL-GL-CONTEXT-ROBUST-ACCESS-FLAG+)
                                (bitarg :forward-compat SDL2-FFI:+SDL-GL-CONTEXT-FORWARD-COMPATIBLE-FLAG+)
                                (bitarg :debug-context SDL2-FFI:+SDL-GL-CONTEXT-DEBUG-FLAG+)))
      (when (arg :context-version-major)
        (sdl2:gl-set-attr :context-major-version (arg :context-version-major))
        (sdl2:gl-set-attr :context-minor-version (arg :context-version-minor)))
      (when (arg :opengl-profile)
        (sdl2:gl-set-attr :context-profile-mask (bitarg :opengl-profile 1)))
      (sdl2:gl-set-attr :doublebuffer (bitarg :double-buffering 1))
      (sdl2:gl-set-attr :stereo (bitarg :stereo-buffer 1))
      (let* ((flags (append (list :opengl)
                            (when (arg :resizable) (list :resizable))
                            (if (arg :visible) (list :shown) (list :hidden))
                            (unless (arg :decorated) (list :borderless))
                            (when (arg :hidpi) (list :allow-highdpi))))
             (window (sdl2:create-window :title (title context)
                                         :w (arg :width)
                                         :h (arg :height)
                                         :flags flags))
             (gl-ctx (sdl2:gl-create-context window)))
        (sdl2:gl-set-swap-interval (arg :vsync))
        (setf (window context) window)
        (setf (gl-ctx context) gl-ctx)))))

(defmethod destroy-context ((context context))
  (unwind-protect
       (progn
         (sdl2:gl-delete-context (gl-ctx context))
         (sdl2:destroy-window (window context)))
    (setf (gl-ctx context) NIL)
    (setf (window context) NIL)))

(defmethod valid-p ((context context))
  (not (null (gl-ctx context))))

(defmethod make-current ((context context))
  (sdl2:gl-make-current (window context) (gl-ctx context)))

(defmethod done-current ((context context))
  (sdl2:gl-make-current (window context) (cffi:null-pointer)))

(defmethod hide ((context context))
  (sdl2:hide-window (window context)))

(defmethod show ((context context) &key (fullscreen NIL fullscreen-p))
  (sdl2:show-window (window context))
  (when fullscreen-p
    (sdl2:set-window-fullscreen (window context) fullscreen)))

(defmethod resize ((context context) width height)
  (sdl2:set-window-size (window context) width height))

(defmethod quit ((context context))
  (sdl2:quit))

(defmethod swap-buffers ((context context))
  (sdl2:gl-swap-window (window context)))

(defmethod show-cursor ((context context))
  (sdl2:show-cursor))

(defmethod hide-cursor ((context context))
  (sdl2:hide-cursor))

(defmethod lock-cursor ((context context))
  (sdl2-ffi.functions:sdl-set-window-grab (window context) T))

(defmethod unlock-cursor ((context context))
  (sdl2-ffi.functions:sdl-set-window-grab (window context) NIL))

(defmethod (setf title) :before (value (context context))
  (sdl2:set-window-title (window context) value))

(defmethod width ((context context))
  (nth-value 0 (sdl2:get-window-size (window context))))

(defmethod height ((context context))
  (nth-value 1 (sdl2:get-window-size (window context))))

(defmethod profile ((context context))
  (let ((attr (sdl2:gl-get-attr :context-profile-mask)))
    (ecase attr
      (0 NIL)
      (#.SDL2-FFI:+SDL-GL-CONTEXT-PROFILE-CORE+ :core)
      (#.SDL2-FFI:+SDL-GL-CONTEXT-PROFILE-COMPATIBILITY+ :compatibility))))

(defmethod version ((context context))
  (list (sdl2:gl-get-attr :context-major-version)
        (sdl2:gl-get-attr :context-minor-version)))

(defun make-context (&optional handler &rest initargs)
  (apply #'make-instance 'context :handler handler initargs))

(defun launch-with-context (&optional main &rest initargs)
  ;; SDL2's WITH-INIT and WITH-EVENT-LOOP do their own, rather annoying, main-thread
  ;; handling. Thus we replicate their functionality here, using TMT where required
  ;; instead. Note that we need to use C-REF, as SDL2's bindings do not seem to expose
  ;; event accessor functions at all other than by WITH-EVENT-LOOP. Cool.
  (flet ((thunk ()
           (unwind-protect
                (progn
                  (let ((init-flags (autowrap:mask-apply 'sdl2::sdl-init-flags '(:video))))
                    (sdl2::check-rc (sdl2-ffi.functions:sdl-init init-flags)))
                  (let* ((main (apply #'make-instance main initargs)))
                    (unwind-protect
                         (progn
                           (start main)
                           (sdl2-event-loop (trial:context main)))
                      (finalize main))))
             (sdl2-ffi.functions:sdl-quit))))
    #+darwin
    (tmt:with-body-in-main-thread ()
      (thunk))
    #-darwin
    (thunk)))

(defun sdl2-event-loop (context)
  (let ((quit NIL))
    (sdl2:with-sdl-event (ev)
      (loop until quit
            for event = (sdl2:next-event ev :wait)
            for type = (sdl2:get-event-type ev)
            do (case type
                 (:keydown
                  (let* ((keysym (plus-c:c-ref ev sdl2-ffi:sdl-event :key :keysym))
                         (sym (sdl2:sym-value keysym))
                         (mod-value (sdl2:mod-value keysym)))
                    (handle (make-instance 'key-press
                                           :key (sdl2-key->key sym)
                                           :modifiers (sdl2-mod->mod mod-value))
                            (handler context))))
                 (:keyup
                  (let* ((keysym (plus-c:c-ref ev sdl2-ffi:sdl-event :key :keysym))
                         (sym (sdl2:sym-value keysym))
                         (mod-value (sdl2:mod-value keysym)))
                    (handle (make-instance 'key-release
                                           :key (sdl2-key->key sym)
                                           :modifiers (sdl2-mod->mod mod-value))
                            (handler context))))
                 (:mousebuttondown
                  (let ((button (plus-c:c-ref ev sdl2-ffi:sdl-event :button :button))
                        (x (plus-c:c-ref ev sdl2-ffi:sdl-event :button :x))
                        (y (plus-c:c-ref ev sdl2-ffi:sdl-event :button :y)))
                    (vsetf (mouse-pos context) x y)
                    (handle (make-instance 'mouse-press
                                           :pos (mouse-pos context)
                                           :button (sdl2-button->button button))
                            (handler context))))
                 (:mousebuttonup
                  (let ((button (plus-c:c-ref ev sdl2-ffi:sdl-event :button :button))
                        (x (plus-c:c-ref ev sdl2-ffi:sdl-event :button :x))
                        (y (plus-c:c-ref ev sdl2-ffi:sdl-event :button :y)))
                    (vsetf (mouse-pos context) x y)
                    (handle (make-instance 'mouse-release
                                           :pos (mouse-pos context)
                                           :button (sdl2-button->button button))
                            (handler context))))
                 (:mousemotion
                  (let* ((new (vec2 (plus-c:c-ref ev sdl2-ffi:sdl-event :motion :x)
                                    (plus-c:c-ref ev sdl2-ffi:sdl-event :motion :y)))
                         (old (shiftf (mouse-pos context) new)))
                    (handle (make-instance 'mouse-move
                                           :pos new
                                           :old-pos old)
                            (handler context))))
                 (:mousewheel
                  (handle (make-instance 'mouse-scroll
                                         :pos (mouse-pos context)
                                         :delta (plus-c:c-ref ev sdl2-ffi:sdl-event :wheel :y))
                          (handler context)))
                 (:textediting
                  ;; FIXME: Don't know what to do with this, yet.
                  )
                 (:textinput
                  (handle (make-instance 'text-entered
                                         :text (plus-c:c-ref ev sdl2-ffi:sdl-event :text :text))
                          (handler context)))
                 (:quit
                  (setf quit T)))))))

(defun sdl2-button->button (button)
  ;; FIXME: I think this is already right?
  button)

(defun sdl2-key->key (key)
  key)

(defun sdl2-mod->mod (mod)
  ())
