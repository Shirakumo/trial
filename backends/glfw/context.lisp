#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.glfw)

(defvar *window-table* (tg:make-weak-hash-table :test 'eq :weakness :value))

(cffi:defcvar (optimus "NvOptimusEnablement") :uint32)
(cffi:defcvar (xpress "AmdPowerXpressRequestHighPerformance") :int)
(cffi:defcfun (wait-events-timeout "glfwWaitEventsTimeout") :void
  (timeout :double))
(cffi:defcfun (window-hint-string "glfwWindowHintString") :void
  (target :int) (hint :string))

(defclass context (trial:context)
  ((title :initarg :title :accessor title)
   (cursor-visible :initform T :accessor cursor-visible)
   (mouse-pos :initform (vec 0 0) :accessor mouse-pos)
   (initargs :initform NIL :accessor initargs)
   (window :initform NIL :accessor window)
   (vsync :initarg :vsync :accessor vsync))
  (:default-initargs
   :resizable T
   :visible T
   :decorated T
   :robustness :no-robustness
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
                                          (title NIL title-p)
                                          (double-buffering NIL double-buffering-p)
                                          (stereo-buffer NIL stereo-buffer-p)
                                          (vsync NIL vsync-p)
                                     ;; Extra options
                                          (fullscreen NIL fullscreen-p)
                                          (resizable NIL resizable-p)
                                          (visible NIL visible-p)
                                          (decorated NIL decorated-p)
                                          (robustness NIL robustness-p)
                                          (forward-compat NIL forward-compat-p)
                                          (debug-context NIL debug-context-p)
                                          (api #-arm :opengl-api #+arm :opengl-es-api))
  #+windows (ignore-errors (setf optimus 1))
  #+windows (ignore-errors (setf xpress 1))
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
      (maybe-set robustness :context-robustness)
      (maybe-set forward-compat :opengl-forward-compat)
      (maybe-set debug-context :opengl-debug-context)
      (setf (g :client-api) api)
      (setf (g :refresh-rate)
            (ecase vsync ((NIL :off) 0) ((T :on) 1) (:adaptive -1)))
      (when version-p
        (setf (g :context-version-major) (first version))
        (setf (g :context-version-minor) (second version)))
      (when profile-p
        (setf (g :opengl-profile) (ecase profile
                                    ((NIL) :opengl-any-profile)
                                    (:core :opengl-core-profile)
                                    (:compatibility :opengl-compat-profile)))))))

(defmethod create-context ((context context))
  (let ((initargs (initargs context)))
    (macrolet ((output-hints (&rest hints)
                 `(progn
                    ,@(loop for (name type attrib) in hints
                            collect `(%glfw:window-hint
                                      ,(or attrib name)
                                      (cffi:convert-to-foreign
                                       (getf initargs ,name) ,type))))))
      (output-hints
       (:resizable :boolean)
       (:visible :boolean)
       (:decorated :boolean)
       (:refresh-rate :int)
       (:stereo-buffer :boolean :stereo)
       (:context-version-major :int)
       (:context-version-minor :int)
       (:context-robustness '%glfw::robustness)
       (:opengl-forward-compat :boolean)
       (:opengl-debug-context :boolean)
       (:opengl-profile '%glfw::opengl-profile)
       (:client-api '%glfw::opengl-api)
       ;; This option is not in cl-glfw3 for some reason.
       (:double-buffering :boolean #x00021010))
      (window-hint-string 147457 "trial")
      (window-hint-string 147458 trial:+app-system+)
      (v:info :trial.backend.glfw "Creating context ~a" context)
      (float-features:with-float-traps-masked T
        (let ((window (%glfw:create-window (getf initargs :width)
                                           (getf initargs :height)
                                           (title context)
                                           (if (getf initargs :fullscreen)
                                               (%glfw:get-primary-monitor)
                                               (cffi:null-pointer))
                                           (if (shared-with context)
                                               (window (shared-with context))
                                               (cffi:null-pointer)))))
          (when (cffi:null-pointer-p window)
            (error "Error creating context."))
          (setf (gethash (cffi:pointer-address window) *window-table*) context)
          (setf (window context) window)
          (cl-glfw3:make-context-current window)
          (cl-glfw3:swap-interval (getf initargs :refresh-rate))
          (cl-glfw3:set-window-size-callback 'ctx-size window)
          (cl-glfw3:set-window-focus-callback 'ctx-focus window)
          (cl-glfw3:set-window-iconify-callback 'ctx-iconify window)
          (cl-glfw3:set-key-callback 'ctx-key window)
          (cl-glfw3:set-char-callback 'ctx-char window)
          (cl-glfw3:set-mouse-button-callback 'ctx-button window)
          (cl-glfw3:set-cursor-position-callback 'ctx-pos window)
          (cl-glfw3:set-scroll-callback 'ctx-scroll window))))))

(defmethod destroy-context ((context context))
  (cl-glfw3:destroy-window (window context))
  (setf (window context) NIL))

(defmethod valid-p ((context context))
  (not (null (window context))))

(defmethod make-current ((context context))
  (%glfw:make-context-current (window context)))

(defmethod done-current ((context context))
  (%glfw:make-context-current (cffi:null-pointer)))

(defmethod hide ((context context))
  (cl-glfw3:hide-window (window context)))

(defmethod show ((context context) &key (fullscreen NIL f-p) mode)
  (cl-glfw3:show-window (window context))
  (cond (f-p
         (destructuring-bind (w h &optional (r %glfw:+dont-care+)) (or mode (cl-glfw3:get-window-size (window context)))
           (cl-glfw3:set-window-monitor (when fullscreen (cl-glfw3:get-primary-monitor))
                                        w h :window (window context) :refresh-rate r))
         (unless fullscreen
           (center-window (window context))))
        ((print mode)
         (resize context (first mode) (second mode)))))

(defmethod resize ((context context) width height)
  (v:info :trial.backend.glfw "Resizing window to ~ax~a" width height)
  (cl-glfw3:set-window-size width height (window context))
  (center-window (window context)))

(defmethod quit ((context context))
  (cl-glfw3:set-window-should-close (window context) T))

(defmethod swap-buffers ((context context))
  (cl-glfw3:swap-buffers (window context)))

(defmethod show-cursor ((context context))
  (cl-glfw3:set-input-mode :cursor :normal (window context))
  (setf (cursor-visible context) T))

(defmethod hide-cursor ((context context))
  (cl-glfw3:set-input-mode :cursor :hidden (window context))
  (setf (cursor-visible context) NIL))

(defmethod lock-cursor ((context context))
  (cl-glfw3:set-input-mode :cursor :disabled (window context)))

(defmethod unlock-cursor ((context context))
  (if (cursor-visible context)
      (show-cursor context)
      (hide-cursor context)))

(defmethod (setf title) :before (value (context context))
  (cl-glfw3:set-window-title value (window context)))

(defmethod (setf vsync) :before (value (context context))
  (cl-glfw3:swap-interval (ecase value ((NIL :off) 0) ((:on T) 1) (:adaptive -1))))

(defmethod width ((context context))
  (cffi:with-foreign-objects ((w :int) (h :int))
    (cffi:foreign-funcall "glfwGetFramebufferSize"
		          %glfw::window (window context) :pointer w :pointer h :void)
    (cffi:mem-ref w :int)))

(defmethod height ((context context))
  (cffi:with-foreign-objects ((w :int) (h :int))
    (cffi:foreign-funcall "glfwGetFramebufferSize"
		          %glfw::window (window context) :pointer w :pointer h :void)
    (cffi:mem-ref h :int)))

(defmethod profile ((context context))
  (ecase (cl-glfw3:get-window-attribute :opengl-profile (window context))
    (:opengl-any-profile NIL)
    (:opengl-core-profile :core)
    (:opengl-compat-profile :compatibility)))

(defmethod version ((context context))
  (list (cl-glfw3:get-window-attribute :context-version-major (window context))
        (cl-glfw3:get-window-attribute :context-version-minor (window context))))

(defmethod list-video-modes ((context context))
  (flet ((mode> (a b)
           (destructuring-bind (aw ah ar) a
             (destructuring-bind (bw bh br) b
               (if (= aw bw)
                   (if (= ah bh)
                       (> ar br)
                       (> ah bh))
                   (> aw bw))))))
    (sort (delete-duplicates
           (loop for mode in (cl-glfw3:get-video-modes (cl-glfw3:get-primary-monitor))
                 collect (list (getf mode '%CL-GLFW3:WIDTH)
                               (getf mode '%CL-GLFW3:HEIGHT)
                               (getf mode '%CL-GLFW3::REFRESH-RATE)))
           :test #'equal)
          #'mode>)))

(defmethod clipboard ((context context))
  (glfw:get-clipboard-string (window context)))

(defmethod (setf clipboard) ((text string) (context context))
  (glfw:set-clipboard-string text (window context)))

(defun make-context (&optional handler &rest initargs)
  (apply #'make-instance 'context :handler handler initargs))

(defun launch-with-context (&optional main &rest initargs)
  (declare (optimize speed))
  (flet ((body ()
           (cl-glfw3:initialize)
           (cl-glfw3:set-error-callback 'ctx-error)
           (let ((main (apply #'make-instance main initargs)))
             (start main)
             (trial:rename-thread "input-loop")
             (unwind-protect
                  (loop with window = (window (trial:context main))
                        until (cl-glfw3:window-should-close-p window)
                        do (wait-events-timeout 0.005d0)
                           (poll-input main))
               (finalize main)
               (%glfw:terminate)))))
    #+darwin
    (tmt:with-body-in-main-thread ()
      (handler-bind ((error #'trial:standalone-error-handler))
        (body)))
    #-darwin
    (body)))

(defmacro %with-context (&body body)
  `(let ((context (gethash (cffi:pointer-address window) *window-table*)))
     ,@body))

(cl-glfw3:def-error-callback ctx-error (message)
  (v:severe :trial.backend.glfw "~a" message))

(cl-glfw3:def-framebuffer-size-callback ctx-size (window w h)
  (%with-context
    (v:debug :trial.input "Framebuffer resized to ~ax~a" w h)
    (handle (make-instance 'resize
                           :width w
                           :height h)
            (handler context))))

(cl-glfw3:def-window-focus-callback ctx-focus (window focusedp)
  (%with-context
    (handle (make-instance (if focusedp 'gain-focus 'lose-focus))
            (handler context))))

(cl-glfw3:def-window-iconify-callback ctx-iconify (window iconifiedp)
  (%with-context
    (handle (make-instance (if iconifiedp 'window-hidden 'window-shown))
            (handler context))))

(cl-glfw3:def-key-callback ctx-key (window key scancode action modifiers)
  (declare (ignore scancode))
  (%with-context
    (case action
      (:press
       (v:debug :trial.input "Key pressed: ~a" key)
       (handle (make-instance 'key-press
                              :key (glfw-key->key key)
                              :modifiers modifiers)
               (handler context)))
      (:release
       (v:debug :trial.input "Key released: ~a" key)
       (handle (make-instance 'key-release
                              :key (glfw-key->key key)
                              :modifiers modifiers)
               (handler context))))))

(cffi:defcallback ctx-char :void ((window :pointer) (char :unsigned-int))
  (when (< char #x110000)
    (let ((char (code-char char)))
      (%with-context
        (handle (make-instance 'text-entered :text (string char))
                (handler context))))))

(cl-glfw3:def-mouse-button-callback ctx-button (window button action modifiers)
  (declare (ignore modifiers))
  (%with-context
    (case action
      (:press
       (v:debug :trial.input "Mouse pressed: ~a" (glfw-button->button button))
       (handle (make-instance 'mouse-press
                              :pos (mouse-pos context)
                              :button (glfw-button->button button))
               (handler context)))
      (:release
       (v:debug :trial.input "Mouse released: ~a" (glfw-button->button button))
       (handle (make-instance 'mouse-release
                              :pos (mouse-pos context)
                              :button (glfw-button->button button))
               (handler context))))))

(cl-glfw3:def-scroll-callback ctx-scroll (window x y)
  (%with-context
    (v:debug :trial.input "Mouse wheel: ~a ~a" x y)
    (handle (make-instance 'mouse-scroll
                           :pos (mouse-pos context)
                           :delta y)
            (handler context))))

(cl-glfw3:def-cursor-pos-callback ctx-pos (window x y)
  (%with-context
    (glfw:get-window-content-scale window)
    (cffi:with-foreign-objects ((x-scale :float) (y-scale :float))
      (cffi:foreign-funcall "glfwGetWindowContentScale" :pointer window :pointer x-scale :pointer y-scale :void)
      (let ((current (vec (* (cffi:mem-ref x-scale :float) x)
                          (* (cffi:mem-ref y-scale :float) (- (second (cl-glfw3:get-window-size window)) y)))))
        (handle (make-instance 'mouse-move
                               :pos current
                               :old-pos (mouse-pos context))
                (handler context))
        (setf (mouse-pos context) current)))))

(defun glfw-button->button (button)
  (case button
    ((:1 :left) :left)
    ((:2 :right) :right)
    (:3 :middle)
    (:4 :x1)
    (:5 :x2)
    (:6 :x3)
    (:7 :x4)
    (:8 :x5)
    (T button)))

(defun glfw-key->key (key)
  (case key
    (:grave-accent :section)
    (T key)))

(defun find-best-monitor (window)
  (let* ((monitors (glfw:get-monitors))
         (best (first monitors)))
    (destructuring-bind (ww wh) (glfw:get-window-size window)
      (destructuring-bind (wx wy) (glfw:get-window-position window)
        (flet ((monitor-area (monitor)
                 (destructuring-bind (mx my) (glfw:get-monitor-position monitor)
                   (let* ((mode (glfw:get-video-mode monitor))
                          (x- (max wx mx))
                          (y- (max wy my))
                          (x+ (min (+ wx ww) (+ mx (getf mode '%glfw:width))))
                          (y+ (min (+ wy wh) (+ my (getf mode '%glfw:height)))))
                     (* (- x+ x-) (- y+ y-))))))
          (dolist (monitor (rest monitors) best)
            (when (< (monitor-area best) (monitor-area monitor))
              (setf best monitor))))))))

(defun center-window (window &optional (monitor (find-best-monitor window)))
  (let ((mode (glfw:get-video-mode monitor)))
    (destructuring-bind (x y) (glfw:get-monitor-position monitor)
      (destructuring-bind (w h) (glfw:get-window-size window)
        (glfw:set-window-position (+ x (floor (- (getf mode '%glfw:width) w) 2))
                                  (+ y (floor (- (getf mode '%glfw:height) h) 2))
                                  window)))))

;; Runtime support for Wayland and X11
#+linux
(progn
  (cffi:define-foreign-library glfw-x11
    (T "libglfw-x11.so"))
  (cffi:define-foreign-library glfw-wayland
    (T "libglfw-wayland.so"))
  
  (deploy:define-library %glfw::glfw
    :dont-open T
    :dont-deploy T)
  (deploy:define-library glfw-x11
    :dont-open T
    :dont-deploy T)
  (deploy:define-library glfw-wayland
    :dont-open T
    :dont-deploy T)

  (deploy:define-hook (:deploy copy-glfw) (directory)
    (let ((path (deploy:library-path (deploy:ensure-library '%glfw::glfw)))
          (target (make-pathname :name (if (deploy:env-set-p "WAYLAND_DISPLAY")
                                           "libglfw-wayland"
                                           "libglfw-x11")
                                 :type "so"
                                 :defaults directory)))
      (unless (uiop:file-exists-p target)
        (uiop:copy-file path target))))

  (deploy:define-hook (:boot load-glfw) ()
    (cond ((deploy:env-set-p "WAYLAND_DISPLAY")
           (deploy:status 1 "Detected Wayland, loading GLFW3-Wayland.")
           (deploy:open-library 'glfw-wayland))
          (T
           (deploy:status 1 "Assuming X11, loading GLFW3-X11.")
           (deploy:open-library 'glfw-x11)))))
