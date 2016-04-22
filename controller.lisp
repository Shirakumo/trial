#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(define-subject controller ()
  (;; Has to be a double to avoid bignums after ~3.8 hours of runtime.
   (tick-count :initform 0.0d0 :accessor tick-count)
   (update-thread :initform NIL :accessor update-thread)
   (last-pause :initform 0 :accessor last-pause)
   (fps :initarg :fps :accessor fps)
   (selection :initarg :selection :accessor selection :finalized T))
  (:default-initargs
   :name :controller
   :fps 30.0f0
   :selection NIL))

(defmethod initialize-instance :after ((controller controller) &key)
  (setf (update-thread controller)
        (bt:make-thread (lambda () (update-loop controller))
                        :name "controller update-thread"
                        :initial-bindings `((*standard-output* . ,*standard-output*)
                                            (*error-output* . ,*error-output*)))))

(defmethod finalize ((controller controller))
  (let ((thread (update-thread controller)))
    (when thread
      (setf (update-thread controller) NIL)
      (loop for i from 0
            while (bt:thread-alive-p thread)
            do (sleep 0.1)
               (with-simple-restart (continue "Continue waiting.")
                 (when (= 10 i)
                   (restart-case
                       (error "Update thread did not exit after 1s.")
                     (abort ()
                       :report "Kill the thread and exit, risking corrupting the image."
                       (bt:destroy-thread thread)
                       (return)))))))))

(defun pause-time (fps start)
  (let* ((duration (max 0 (- (current-time) start)))
         (remainder (- (/ fps) (/ duration *time-units*))))
    (max 0 remainder)))

(defmacro with-frame-pause ((fps) &body body)
  (let ((pause (gensym "PAUSE"))
        (start (gensym "START")))
    `(let ((,start (current-time)))
       ,@body
       (let ((,pause (pause-time ,fps ,start)))
         (sleep ,pause)
         ,pause))))

(defun update-loop (controller)
  (let* ((main *main*)
         (scene (scene main)))
    (acquire-context main :reacquire T :force T)
    (cl-gamepad:init)
    (setup-rendering main)
    (setup-scene scene)
    (start scene)

    (setf (selection controller)
          (make-instance 'selection-buffer :width (q+:width main)
                                           :height (q+:height main)))
    
    (with-slots-bound (controller controller)
      (unwind-protect
           (loop while (update-thread controller)
                 do (with-simple-restart (abort "Abort the update and retry.")
                      (setf (last-pause controller)
                            (with-frame-pause ((fps controller))
                              ;; Potentially release context every time to allow
                              ;; other threads to grab it.
                              (with-context (main)
                                (issue scene 'tick)
                                (process scene)
                                (render controller main)
                                (render-hud controller main)
                                (q+:swap-buffers main)))))))
      (stop scene)))
  (v:info :trial.controller "Exiting update-loop."))

(define-handler (controller mouse-release) (ev pos)
  (let* ((buffer (selection controller))
         (x (round (vx pos)))
         (y (- (height buffer) (round (vy pos)))))
    (render (first (loops controller)) buffer)
    (v:info :test "CLICK: ~a/~a => ~a" x y (object-at-point buffer x y))))

(defun setup-rendering (main)
  (v:info :trial.controller "Running GL~a.~a with ~a buffer~:p / ~a sample~:p, max texture size ~a."
          (gl:get* :major-version)
          (gl:get* :minor-version)
          (gl:get* :sample-buffers)
          (gl:get* :samples)
          (gl:get* :max-texture-size))
  (v:debug :trial.controller "GL info report:~%~
                             GL Vendor:     ~a~%~
                             GL Renderer:   ~a~%~
                             GL Version:    ~a~%~
                             GL Shader:     ~a~%~
                             GL Extensions: ~a"
           (gl:get-string :vendor)
           (gl:get-string :renderer)
           (gl:get-string :version)
           (gl:get-string :shading-language-version)
           (gl:get-string :extensions))
  (gl:depth-mask T)
  (gl:depth-func :lequal)
  (gl:clear-depth 1.0)
  (gl:alpha-func :greater 0)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:shade-model :smooth)
  (gl:front-face :ccw)
  (gl:cull-face :back)
  (gl:hint :perspective-correction-hint :nicest)
  (gl:hint :line-smooth-hint :nicest)
  (gl:hint :polygon-smooth-hint :nicest))

(defmethod render ((controller controller) (main main))
  (q+:qgl-clear-color main (slot-value main 'background))
  (let ((width (width main)) (height (height main)))
    (gl:clear :color-buffer :depth-buffer)
    (gl:enable :blend :cull-face :texture-2d :multisample
               :line-smooth :polygon-smooth
               :depth-test :depth-clamp :alpha-test)
    (with-pushed-matrix
      (paint (first (loops controller)) main))
    (gl:load-identity)))

(defun render-hud (controller main)
  (gl:with-pushed-matrix* (:projection)
    (gl:load-identity)
    (gl:ortho 0 (q+:width main) (q+:height main) 0 -1 10)
    (gl:matrix-mode :modelview)
    (gl:load-identity)
    (gl:disable :cull-face)
    (gl:clear :depth-buffer)

    #+trial-debug-selection-buffer
    (paint (selection controller) main)
    
    (let ((font (get-resource 'font :trial :debug-hud)))
      (q+:render-text main 20 30 (format NIL "Pause: ~,10f" (last-pause controller))
                      (data font))
      (q+:render-text main 20 50 (format NIL "Time:  ~2,'0d:~6,3,,,'0f"
                                         (floor (/ (round (clock (scene main))) 60))
                                         (mod (clock (scene main)) 60))
                      (data font))))
  (gl:matrix-mode :modelview))

;; FIXME: proper LOADing of a map
(defun setup-scene (scene)
  (enter (make-instance 'skybox) scene)
  (enter (make-instance 'space-axes) scene)
  (enter (make-instance 'player) scene)
  (enter (make-instance 'pivot-camera :name :camera :target (unit :player scene)) scene))

(define-handler (controller resize resize) (ev width height)
  (v:info :trial.controller "Resizing to ~ax~a" width height)
  (reinitialize-instance (selection controller) :width width :height height))

(define-handler (controller tick tick 100) (ev)
  (incf (tick-count controller))
  (when (= 0 (mod (tick-count controller) (fps controller)))
    (cl-gamepad:detect-devices))
  (cl-gamepad:process-events))

(define-handler (controller mapping T 100) (ev)
  (map-event ev *loop*))

(define-handler (controller launch-editor) (ev)
  (signal! *main* (launch-editor)))

(define-handler (controller reload-assets reload-assets 99) (ev)
  (dolist (pool (pools))
    (mapc #'reload (assets pool))))

(define-handler (controller reload-scene reload-scene 99) (ev)
  ;; gross!
  (let ((scene (first (loops controller))))
    (dolist (obj (flare-indexed-set:coerce-set (objects scene) 'list))
      (unless (eql obj controller)
        (leave obj scene)
        (finalize obj)))
    (setup-scene scene)))

(defclass acquire-context (event)
  ())

(define-handler (controller acquire-context) (ev)
  (acquire-context *main*))

(defclass execute (event)
  ((func :initarg :func :reader func)
   (bindings :initarg :bindings :reader bindings)
   (result :initform NIL :accessor result))
  (:default-initargs
   :func (error "FORM required.")
   :bindings ()))

(defmethod execute :around ((execute execute))
  (with-slots (bindings result) execute
    (handler-case
        (handler-bind ((error (lambda (err)
                                (v:error :trial.controller "Error attempting to process execute event: ~a" err)
                                (v:debug :trial.controller err))))
          (progv (mapcar #'first bindings)
              (mapcar #'second bindings)
            (setf result (cons :success (multiple-value-list
                                         (call-next-method))))))
      (error (err)
        (setf result (cons :failure err))))))

(defmethod execute ((execute execute))
  (funcall (func execute)))

(define-handler (controller execute) (ev)
  (execute ev))

(defun funcall-in-scene (scene func &key bindings (want-results T))
  (let ((event (make-instance 'execute :func func :bindings bindings)))
    (issue scene event)
    (when want-results
      (values-list
       (loop for result = (result event)
             do (sleep 0.01)
                (case (car result)
                  (:failure (error (cdr result)))
                  (:success (return (cdr result)))))))))
