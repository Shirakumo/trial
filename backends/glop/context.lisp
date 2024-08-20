(in-package #:org.shirakumo.fraf.trial.glop)

(defun make-context (&optional handler &rest initargs)
  (apply #'make-instance 'context :handler handler initargs))

(defclass context (trial:context glop:window)
  ((context :initform NIL :accessor context)
   (initargs :initform NIL :accessor initargs)
   (profile :initarg :profile :accessor profile)
   (version :initarg :version :accessor version)
   (mouse-pos :initform (vec 0 0) :accessor mouse-pos)
   (previous-size :initform (vec 0 0) :accessor previous-size)
   (modifiers :initform () :accessor modifiers)
   (closing :initform NIL :accessor closing)))

(defmethod initialize-instance ((context context) &key)
  (call-next-method)
  (create-context context))

(defmethod shared-initialize :after ((context context) slots
                                     &key (width NIL width-p)
                                          (height NIL height-p)
                                          (title NIL title-p)
                                          (double-buffering NIL double-buffering-p)
                                          (stereo-buffer NIL stereo-buffer-p)
                                          (vsync NIL vsync-p)
                                     ;; Extra
                                          (accumulation-buffer NIL accumulation-buffer-p)
                                          (alpha-buffer NIL alpha-buffer-p)
                                          (depth-buffer NIL depth-buffer-p)
                                          (stencil-buffer NIL stencil-buffer-p))
  (macrolet ((maybe-set (var)
               `(when ,(let ((*print-case* (readtable-case *readtable*)))
                         (intern (format NIL "~a-~a" var 'p)))
                  (setf (getf (initargs context) ,(intern (string var) :keyword))
                        ,var))))
    (maybe-set width)
    (maybe-set height)
    (maybe-set title)
    (maybe-set double-buffering)
    (maybe-set accumulation-buffer)
    (maybe-set alpha-buffer)
    (maybe-set depth-buffer)
    (maybe-set stencil-buffer)
    (maybe-set stereo-buffer)
    (maybe-set vsync)))

(defmethod create-context ((context context))
  (flet ((g (item &optional default)
           (getf (initargs context) item default)))
    (when (shared-with context)
      (error "GLOP does not support context sharing! Go bugger the devs about it."))
    (glop:open-window context
                      (g :title) (g :width) (g :height)
                      :x (g :x 0) :y (g :y 0)
                      :double-buffer (g :double-buffering)
                      :stereo (g :stereo-buffer)
                      :accum-buffer (g :accumulation-buffer)
                      :stencil-buffer (g :stencil-buffer))
    (with-cleanup-on-failure (destroy-context context)
      (setf (context context)
            (glop:create-gl-context context
                                    :major (first (version context))
                                    :minor (second (version context))
                                    :profile (profile context)
                                    :make-current T))
      (vsync context (g :vsync)))))

(defmethod vsync ((context context) mode)
  (let ((mode (ecase mode
                (:on 1) (:off 0) (:adaptive -1))))
    #+windows
    (wgl-swap-interval mode)
    #+linux
    (glx-swap-interval
     (glop::glx-context-display (context context))
     (glop::x11-window-id context)
     mode)
    #+darwin
    (cffi:with-foreign-object (mode-ptr :int)
      (setf (cffi:mem-ref mode-ptr :int) mode)
      (cgl-set-parameter (context context) :swap-interval mode-ptr))))

#+windows
(cffi:defcfun (wgl-swap-interval "wglSwapIntervalEXT") :boolean
  (interval :int))

#+linux
(cffi:defcfun (glx-swap-interval "glXSwapIntervalEXT") :void
  (display :pointer)
  (drawable glop-glx::drawable)
  (interval :int))

;; http://mirror.informatimago.com/next/developer.apple.com/documentation/GraphicsImaging/Conceptual/OpenGL/chap5/chapter_5_section_40.html#//apple_ref/doc/c_ref/CGLContextParameter
#+darwin
(cffi:defcenum cgl-context-parameter
  (:swap-rectangle 200)
  (:swap-interval= 222)
  (:client-storage 226))

;; http://mirror.informatimago.com/next/developer.apple.com/documentation/GraphicsImaging/Conceptual/OpenGL/chap5/chapter_5_section_20.html
#+darwin
(cffi:defcfun (cgl-set-parameter "CGLSetParameter") glop-bridge::cg-error
  (context :pointer)
  (property cgl-context-parameter)
  (params :pointer))

(defmethod destroy-context ((context context))
  (glop:destroy-window context)
  (setf context NIL))

(defmethod valid-p ((context context))
  (not (null (context context))))

(defmethod make-current ((context context))
  (glop:attach-gl-context context (context context)))

(defmethod done-current ((context context))
  (glop:detach-gl-context (context context)))

(defmethod hide ((context context))
  (glop:hide-window context))

(defmethod show ((context context) &key (fullscreen NIL fullscreen-p))
  (glop:show-window context)
  (when fullscreen-p
    (glop:set-fullscreen context fullscreen)))

(defmethod quit ((context context))
  (setf (closing context) T))

(defmethod resize ((context context) width height)
  (setf (glop:window-width context) width)
  (setf (glop:window-height context) height))

(defmethod swap-buffers ((context context))
  (glop:swap-buffers context))

(defmethod show-cursor ((context context))
  (glop:show-cursor context))

(defmethod hide-cursor ((context context))
  (glop:hide-cursor context))

;; FIXME
(defmethod lock-cursor ((context context))
  )

(defmethod unlock-cursor ((context context))
  )

(defmethod title ((context context))
  (glop::window-title context))

(defmethod (setf title) (value (context context))
  (glop:set-window-title context value))

(defmethod width ((context context))
  (glop:window-width context))

(defmethod height ((context context))
  (glop:window-height context))

(defmethod poll-input ((context context))
  (glop:dispatch-events context :blocking T :on-foo NIL))

(defun launch-with-context (&optional (main 'main) &rest initargs)
  #+linux (cffi:foreign-funcall "XInitThreads" :int)
  (let* ((main (apply #'make-instance main initargs))
         (context (trial:context main))
         (glop:*ignore-auto-repeat* t))
    (flet ((body ()
             (unwind-protect
                  (catch 'escape
                    (start main)
                    (loop (glop:dispatch-events context :blocking T :on-foo NIL)
                          (poll-input main)))
               (finalize main))))
      #+darwin
      (tmt:with-body-in-main-thread (:blocking T)
        (body))
      #-darwin
      (body))))

(defmethod glop:on-event ((context context) event)
  (typecase event
    (glop:key-press-event
     (v:debug :trial.input "Key pressed: ~a" (glop:keysym event))
     (case (glop:keysym event)
       ((:shift-l :shift-r)
        (pushnew :shift (modifiers context)))
       ((:control-l :control-r)
        (pushnew :control (modifiers context)))
       ((:meta-l :meta-r)
        (pushnew :meta (modifiers context)))
       ((:super-l :super-r)
        (pushnew :super (modifiers context)))
       ((:hyper-l :hyper-r)
        (pushnew :hyper (modifiers context))))
     (handle (make-instance 'key-press :key (glop-key->key (glop:keysym event))
                                       :modifiers (modifiers context))
             (handler context)))
    (glop:key-release-event
     (v:debug :trial.input "Key released: ~a" (glop:keysym event))
     (case (glop:keysym event)
       ((:shift-l :shift-r)
        (setf (modifiers context) (delete :shift (modifiers context))))
       ((:control-l :control-r)
        (setf (modifiers context) (delete :control (modifiers context))))
       ((:meta-l :meta-r)
        (setf (modifiers context) (delete :meta (modifiers context))))
       ((:super-l :super-r)
        (setf (modifiers context) (delete :super (modifiers context))))
       ((:hyper-l :hyper-r)
        (setf (modifiers context) (delete :hyper (modifiers context)))))
     (handle (make-instance 'key-release :key (glop-key->key (glop:keysym event))
                                         :modifiers (modifiers context))
             (handler context))
     (when (and (glop:text event) (string/= "" (glop:text event)))
       (handle (make-instance 'text-entered :text (glop:text event))
               (handler context))))
    (glop:button-press-event
     (case (glop:button event)
       (4 (v:debug :trial.input "Mouse wheel: ~a" 1)
        (handle (make-instance 'mouse-scroll :delta 1
                                             :pos (mouse-pos context))
                (handler context)))
       (5 (v:debug :trial.input "Mouse wheel: ~a" -1)
        (handle (make-instance 'mouse-scroll :delta -1
                                             :pos (mouse-pos context))
                (handler context)))
       (T (v:debug :trial.input "Mouse pressed: ~a" (glop-button->symbol
                                                     (glop:button event)))
        (handle (make-instance 'mouse-press :button (glop-button->symbol
                                                     (glop:button event))
                                            :pos (mouse-pos context))
                (handler context)))))
    (glop:button-release-event
     (v:debug :trial.input "Mouse released: ~a" (glop-button->symbol
                                                 (glop:button event)))
     (handle (make-instance 'mouse-release :button (glop-button->symbol
                                                    (glop:button event))
                                           :pos (mouse-pos context))
             (handler context)))
    (glop:mouse-motion-event
     (let ((current (vec (+ (glop:x event) (glop:dx event))
                         (- (glop:window-height context) (+ (glop:y event) (glop:dy event))))))
       (handle (make-instance 'mouse-move :old-pos (or (mouse-pos context) current)
                                          :pos current)
               (handler context))
       (setf (mouse-pos context) current)))
    (glop:resize-event
     (let ((previous-size (previous-size context)))
       (when (or (/= (glop:width event) (vx previous-size))
                 (/= (glop:height event) (vy previous-size)))
         (setf (vx previous-size) (glop:width event))
         (setf (vy previous-size) (glop:height event))
         (handle (make-instance 'resize :width (glop:width event)
                                        :height (glop:height event))
                 (handler context)))))
    (glop:expose-event)
    (glop:visibility-event)
    (glop:focus-in-event
     (handle (make-instance 'gain-focus) (handler context)))
    (glop:focus-out-event
     (handle (make-instance 'lose-focus) (handler context)))
    (glop:close-event
     (setf (closing context) T)))
  (when (closing context)
    (throw 'escape NIL)))

;; FIXME: match this up with the GLFW backend.
(defun glop-key->key (key)
  (case key
    (T key)))

(defun glop-button->symbol (button)
  (case button
    (1 :left)
    (2 :middle)
    (3 :right)
    (T :x1)))
