(in-package #:org.shirakumo.fraf.trial.sdl2)

(cffi:defcstruct display-mode
  (format :uint32)
  (width :int)
  (height :int)
  (refresh-rate :int)
  (data :pointer))

(defclass monitor (trial:monitor)
  ((id :initarg :id :accessor monitor-id)))

(defmethod name ((monitor monitor))
  (sdl2-ffi.functions:sdl-get-display-name (monitor-id monitor)))

(defclass context (trial:context)
  ((title :initform "" :initarg :title :accessor title)
   (window :initform NIL :accessor window)
   (gl-ctx :initform NIL :accessor gl-ctx)
   (initargs :initform NIL :accessor initargs)
   (mouse-pos :initform (vec2 0 0) :accessor mouse-pos :reader cursor-position)
   (monitors :initform NIL :accessor monitors)
   (width :initform 1 :accessor width)
   (height :initform 1 :accessor height))
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
                                          (fullscreen NIL fullscreen-p)
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
      (maybe-set fullscreen)
      (maybe-set resizable)
      (maybe-set visible)
      (maybe-set decorated)
      (maybe-set hidpi)
      (maybe-set robustness)
      (maybe-set forward-compat)
      (maybe-set debug-context)
      (when vsync-p
        (setf (g :vsync)
              (ecase vsync (:off 0) ((:on T) 1) (:adaptive -1))))
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
                            (when (arg :fullscreen) (list :fullscreen))
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

(defmethod visible-p ((context context))
  (member :shown (sdl2:get-window-flags (window context))))

(defmethod hide ((context context))
  (sdl2:hide-window (window context)))

(defun find-matching-mode (monitor w h r mode)
  (loop for i from 0 below (sdl2-ffi.functions:sdl-get-num-display-modes monitor)
        do (sdl2::check-zero (sdl2-ffi.functions:sdl-get-display-mode monitor i mode))
        do (when (and (= w (cffi:foreign-slot-value mode '(:struct display-mode) 'width))
                      (= h (cffi:foreign-slot-value mode '(:struct display-mode) 'height))
                      (or (null r) (= r (cffi:foreign-slot-value mode '(:struct display-mode) 'refresh-rate))))
             (return))))

(defmethod show ((context context) &key (fullscreen NIL fullscreen-p) mode)
  (sdl2:show-window (window context))
  (cond (fullscreen-p
         (destructuring-bind (w h &optional r monitor) (or mode (current-video-mode (current-monitor context)))
           (let ((monitor (etypecase monitor
                            (null (monitor-id (current-monitor context)))
                            (integer monitor)
                            (monitor (monitor-id monitor))
                            (string (monitor-id (find-monitor monitor context))))))
             (sdl2:set-window-fullscreen (window context) fullscreen)
             (if fullscreen
                 (cffi:with-foreign-object (mode '(:struct display-mode))
                   (find-matching-mode monitor w h r mode)
                   (sdl2-ffi.functions:sdl-set-window-display-mode (window context) mode))
                 (resize context w h)))))
        (mode)))

(defmethod resize ((context context) width height)
  (sdl2:set-window-size (window context) width height))

(defmethod quit ((context context))
  (sdl2:push-event :quit))

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

(defmethod cursor ((context context))
  :default)

;; TODO: implement cursor setting
(defmethod (setf cursor) ((null null) (context context))
  )

(defmethod (setf cursor) ((name symbol) (context context))
  )

(defmethod (setf cursor) (icon (context context))
  )

(defmethod (setf title) :before (value (context context))
  (sdl2:set-window-title (window context) value))

(defmethod profile ((context context))
  (let ((attr (sdl2:gl-get-attr :context-profile-mask)))
    (ecase attr
      (0 NIL)
      (#.SDL2-FFI:+SDL-GL-CONTEXT-PROFILE-CORE+ :core)
      (#.SDL2-FFI:+SDL-GL-CONTEXT-PROFILE-COMPATIBILITY+ :compatibility))))

(defmethod version ((context context))
  (list (sdl2:gl-get-attr :context-major-version)
        (sdl2:gl-get-attr :context-minor-version)))

(defmethod current-monitor ((context context))
  (find (sdl2-ffi.functions:sdl-get-window-display-index (window context))
        (list-monitors context) :key #'monitor-id))

(defmethod list-monitors ((context context))
  (or (monitors context)
      (setf (monitors context)
            (loop for i from 0 below (sdl2-ffi.functions:sdl-get-num-video-displays)
                  collect (make-instance 'monitor :id i)))))

(defmethod list-video-modes ((monitor monitor))
  (cffi:with-foreign-object (mode '(:struct display-mode))
    (setf (cffi:foreign-slot-value mode '(:struct display-mode) 'data) (cffi:null-pointer))
    (loop with id = (monitor-id monitor)
          for i from 0 below (sdl2-ffi.functions:sdl-get-num-display-modes id)
          do (sdl2::check-rc (sdl2-ffi.functions:sdl-get-display-mode id i mode))
          collect (list (cffi:foreign-slot-value mode '(:struct display-mode) 'width)
                        (cffi:foreign-slot-value mode '(:struct display-mode) 'height)
                        (cffi:foreign-slot-value mode '(:struct display-mode) 'refresh-rate)
                        (name monitor)))))

(defun current-video-mode (monitor)
  (cffi:with-foreign-object (mode '(:struct display-mode))
    (sdl2-ffi.functions:sdl-get-current-display-mode (monitor-id monitor) mode)
    (list (cffi:foreign-slot-value mode '(:struct display-mode) 'width)
          (cffi:foreign-slot-value mode '(:struct display-mode) 'height)
          (cffi:foreign-slot-value mode '(:struct display-mode) 'refresh-rate)
          (name monitor))))

(defmethod clipboard ((context context))
  (sdl2-ffi.functions:sdl-get-clipboard-text))

(defmethod (setf clipboard) (value (context context))
  (sdl2-ffi.functions:sdl-set-clipboard-text value)
  value)

(defmethod (setf cursor-position) (pos (context context))
  (sdl2:warp-mouse-in-window (window context) (round (vx pos)) (- (height context) (round (vy pos))))
  (v<- (mouse-pos context) pos)
  pos)

(defmethod local-key-string ((context context) (name symbol))
  (sdl2:get-key-name (key->sdl2-key name)))

(defmethod local-key-string ((context context) (code integer))
  (let ((name (sdl2:get-key-from-scancode code)))
    (when name (sdl2:get-key-name name))))

(defmethod poll-input ((context context))
  ;; TODO: implement
  )

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
                           (sdl2-event-loop main))
                      (finalize main))))
             (sdl2-ffi.functions:sdl-quit))))
    #+darwin
    (tmt:with-body-in-main-thread ()
      (thunk))
    #-darwin
    (thunk)))

(defun sdl2-event-loop (main)
  (let ((context (trial:context main))
        (quit NIL))
    (sdl2:with-sdl-event (ev)
      (loop until quit
            for event = (sdl2:next-event ev)
            do (poll-input main)
               (when event
                 (with-simple-restart (abort "Don't handle the event.")
                   (case (sdl2:get-event-type ev)
                     (:windowevent
                      (case (plus-c:c-ref ev sdl2-ffi:sdl-event :window :event)
                        (#.sdl2-ffi:+SDL-WINDOWEVENT-SHOWN+
                         (handle (make-instance 'window-shown) (handler context)))
                        (#.sdl2-ffi:+SDL-WINDOWEVENT-HIDDEN+
                         (handle (make-instance 'window-hidden) (handler context)))
                        (#.sdl2-ffi:+SDL-WINDOWEVENT-FOCUS-GAINED+
                         (handle (make-instance 'gain-focus) (handler context)))
                        (#.sdl2-ffi:+SDL-WINDOWEVENT-FOCUS-LOST+
                         (handle (make-instance 'lose-focus) (handler context)))
                        (#.sdl2-ffi:+SDL-WINDOWEVENT-SIZE-CHANGED+
                         (let ((w (plus-c:c-ref ev sdl2-ffi:sdl-event :window :data1))
                               (h (plus-c:c-ref ev sdl2-ffi:sdl-event :window :data2)))
                           (setf (width context) w)
                           (setf (height context) h)
                           (handle (make-instance 'resize :width w :height h) (handler context))))))
                     (:keydown
                      (let* ((keysym (plus-c:c-ref ev sdl2-ffi:sdl-event :key :keysym))
                             (sym (sdl2:scancode-value keysym))
                             (mod (sdl2:mod-keywords (sdl2:mod-value keysym))))
                        (handle (make-instance 'key-press
                                               :key (sdl2-key->key sym)
                                               :repeat (< 0 (plus-c:c-ref ev sdl2-ffi:sdl-event :key :repeat))
                                               :modifiers (mapcar #'sdl2-mod->mod mod))
                                (handler context))))
                     (:keyup
                      (let* ((keysym (plus-c:c-ref ev sdl2-ffi:sdl-event :key :keysym))
                             (sym (sdl2:scancode-value keysym))
                             (mod (sdl2:mod-keywords (sdl2:mod-value keysym))))
                        (handle (make-instance 'key-release
                                               :key (sdl2-key->key sym)
                                               :modifiers (mapcar #'sdl2-mod->mod mod))
                                (handler context))))
                     (:mousebuttondown
                      (let ((button (plus-c:c-ref ev sdl2-ffi:sdl-event :button :button))
                            (x (plus-c:c-ref ev sdl2-ffi:sdl-event :button :x))
                            (y (- (height context) (plus-c:c-ref ev sdl2-ffi:sdl-event :button :y))))
                        (vsetf (mouse-pos context) x y)
                        (handle (make-instance 'mouse-press
                                               :pos (mouse-pos context)
                                               :button (sdl2-button->button button))
                                (handler context))))
                     (:mousebuttonup
                      (let ((button (plus-c:c-ref ev sdl2-ffi:sdl-event :button :button))
                            (x (plus-c:c-ref ev sdl2-ffi:sdl-event :button :x))
                            (y (- (height context) (plus-c:c-ref ev sdl2-ffi:sdl-event :button :y))))
                        (vsetf (mouse-pos context) x y)
                        (handle (make-instance 'mouse-release
                                               :pos (mouse-pos context)
                                               :button (sdl2-button->button button))
                                (handler context))))
                     (:mousemotion
                      (let* ((new (vec2 (plus-c:c-ref ev sdl2-ffi:sdl-event :motion :x)
                                        (- (height context) (plus-c:c-ref ev sdl2-ffi:sdl-event :motion :y))))
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
                      (let ((text (cffi:foreign-string-to-lisp
                                   (plus-c:c-ref ev sdl2-ffi:sdl-event :text :text plus-c:&)
                                   :encoding :utf-8)))
                        (handle (make-instance 'text-entered
                                               :text text)
                                (handler context))))
                     (:quit
                      (setf quit T)))))
            (sleep 0.001)))))

(defun sdl2-button->button (button)
  button)

(defun sdl2-key->key (key)
  (aref *sdl2-scancode-map* key))

(defun key->sdl2-code (key)
  (gethash key *sdl2-keycode-map*))

(defun key->sdl2-key (key)
  (let ((code (gethash key *sdl2-keycode-map*)))
    (when code (sdl2:get-key-from-scancode code))))

(defun sdl2-mod->mod (mod)
  (case mod
    ((:lshift :rshift) :shift)
    ((:lctrl :rctrl) :ctrl)
    ((:lalt :ralt) :alt)
    ((:lgui :rgui) :super)
    (T mod)))
