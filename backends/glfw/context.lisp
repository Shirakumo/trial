#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.glfw)

(defvar *window-table* (tg:make-weak-hash-table :test 'eq :weakness :value))

(defclass context (trial:context)
  ((title :initform "" :accessor title)
   (cursor-visible :initform T :accessor cursor-visible)
   (mouse-pos :initform (vec 0 0) :accessor mouse-pos)
   (initargs :initform NIL :accessor initargs)
   (window :initform NIL :accessor window))
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
                                          (resizable NIL resizable-p)
                                          (visible NIL visible-p)
                                          (decorated NIL decorated-p)
                                          (robustness NIL robustness-p)
                                          (forward-compat NIL forward-compat-p)
                                          (debug-context NIL debug-context-p))
  (flet (((setf g) (value name) (setf (getf (initargs context) name) value)))
    (macrolet ((maybe-set (var &optional (name (intern (string var) :keyword)))
                 `(when ,(intern (format NIL "~a-~a" var 'p))
                    (setf (g ,name) ,var))))
      (maybe-set width)
      (maybe-set height)
      (maybe-set title)
      (maybe-set double-buffering)
      (maybe-set stereo-buffer)
      (maybe-set resizable)
      (maybe-set visible)
      (maybe-set decorated)
      (maybe-set robustness :context-robustness)
      (maybe-set forward-compat :opengl-forward-compat)
      (maybe-set debug-context :opengl-debug-context)
      (when vsync-p
        (setf (g :refresh-rate)
              (ecase vsync (:off 0) (:on 1) (:adaptive -1))))
      (when version-p
        (setf (g :context-version-major) (first version))
        (setf (g :context-version-minor) (second version)))
      (when profile-p
        (setf (g :opengl-profile) (ecase profile
                                    ((NIL) :opengl-any-profile)
                                    (:core :opengl-core-profile)
                                    (:compatibility :opengl-compatibility-profile)))))))

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
       ;; This option is not in cl-glfw3 for some reason.
       (:double-buffering :boolean #x00021010))
      (let ((window (%glfw:create-window (getf initargs :width)
                                         (getf initargs :height)
                                         (title context)
                                         (cffi:null-pointer)
                                         (if (shared-with context)
                                             (window (shared-with context))
                                             (cffi:null-pointer)))))
        (when (cffi:null-pointer-p window)
          (error "Error creating context."))
        (setf (gethash (cffi:pointer-address window) *window-table*) context)
        (setf (window context) window)
        (cl-glfw3:make-context-current window)
        (cl-glfw3:set-window-size-callback 'ctx-size window)
        (cl-glfw3:set-window-focus-callback 'ctx-focus window)
        (cl-glfw3:set-key-callback 'ctx-key window)
        (cl-glfw3:set-char-callback 'ctx-char window)
        (cl-glfw3:set-mouse-button-callback 'ctx-button window)
        (cl-glfw3:set-cursor-position-callback 'ctx-pos window)
        (cl-glfw3:set-scroll-callback 'ctx-scroll window)))))

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

(defmethod show ((context context) &key (fullscreen NIL f-p))
  (cl-glfw3:show-window (window context))
  (when f-p
    (destructuring-bind (w h) (cl-glfw3:get-window-size (window context))
      (cl-glfw3:set-window-monitor (when fullscreen (cl-glfw3:get-primary-monitor))
                                   w h :window (window context)))))

(defmethod resize ((context context) width height)
  (cl-glfw3:set-window-size width height (window context)))

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

(defmethod width ((context context))
  (first (cl-glfw3:get-window-size (window context))))

(defmethod height ((context context))
  (second (cl-glfw3:get-window-size (window context))))

(defmethod profile ((context context))
  (ecase (cl-glfw3:get-window-attribute :opengl-profile (window context))
    (:opengl-any-profile NIL)
    (:opengl-core-profile :core)
    (:opengl-compat-profile :compatibility)))

(defmethod version ((context context))
  (list (cl-glfw3:get-window-attribute :context-version-major (window context))
        (cl-glfw3:get-window-attribute :context-version-minor (window context))))

(defun make-context (&optional handler &rest initargs)
  (apply #'make-instance 'context :handler handler initargs))

(defun launch-with-context (&optional main &rest initargs)
  (flet ((body ()
           (cl-glfw3:with-init
             (let ((main (apply #'make-instance main initargs)))
               (start main)
               (unwind-protect
                    (loop with window = (window (trial:context main))
                          until (cl-glfw3:window-should-close-p window)
                          do (cl-glfw3:poll-events)
                             ;; Apparently bt:thread-yield is a no-op sometimes,
                             ;; making this loop consume the core. Sleep instead.
                             (sleep 0.001))
                 (finalize main))))))
    #+darwin
    (tmt:with-body-in-main-thread ()
      (body))
    #-darwin
    (body)))

(defmacro %with-context (&body body)
  `(let ((context (gethash (cffi:pointer-address window) *window-table*)))
     ,@body))

(cl-glfw3:def-window-size-callback ctx-size (window w h)
  (%with-context
    (handle (make-instance 'resize
                           :width w
                           :height h)
            (handler context))))

(cl-glfw3:def-window-focus-callback ctx-focus (window focusedp)
  (%with-context
    (handle (make-instance (if focusedp 'gain-focus 'lose-focus))
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

(cl-glfw3:def-char-callback ctx-char (window char)
  (%with-context
    (handle (make-instance 'text-entered
                           :text (string char))
            (handler context))))

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
    (let ((current (vec x (- (second (cl-glfw3:get-window-size (window context))) y))))
      (handle (make-instance 'mouse-move
                             :pos current
                             :old-pos (mouse-pos context))
              (handler context))
      (setf (mouse-pos context) current))))

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
