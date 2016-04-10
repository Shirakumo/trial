#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(define-subject controller ()
  (;; Has to be a double to avoid bignums after ~3.8 hours of runtime.
   (tickcount :initform 0.0d0 :accessor tickcount)
   (update-thread :initform NIL :accessor update-thread)
   (last-pause :initform 0 :accessor last-pause)
   (fps :initarg :fps :accessor fps))
  (:default-initargs
   :name :controller
   :fps 30.0f0))

(defmethod initialize-instance :after ((controller controller) &key)
  (setf (update-thread controller)
        (bt:make-thread (lambda () (update-loop controller))
                        :initial-bindings `((*standard-output* . ,*standard-output*)
                                            (*error-output* . ,*error-output*)))))

(defmethod finalize ((controller controller))
  (let ((thread (update-thread controller)))
    (setf (update-thread controller) NIL)
    (loop for i from 0
          while (bt:thread-alive-p thread)
          do (sleep 0.1)
             (when (< 10 i)
               (v:warn :trial.controller "Update loop did not exit gracefully.")
               (bt:destroy-thread thread)
               (return)))))

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
  (let ((main *main-window*))
    (q+:make-current main)

    (q+:qgl-clear-color main (q+:make-qcolor 0 0 0))
    (gl:enable :depth-test :blend :cull-face :texture-2d)
    (gl:depth-mask T)
    (gl:depth-func :lequal)
    (gl:clear-depth 1.0)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (gl:shade-model :smooth)
    (gl:front-face :ccw)
    (gl:cull-face :back)
    (gl:hint :perspective-correction-hint :nicest)
    (cl-gamepad:init)
    (setup-scene (scene main))
    
    (with-slots-bound (controller controller)
      (loop while (update-thread controller)
            do (with-simple-restart (abort "Abort the update and retry.")
                 (setf (last-pause controller)
                       (with-frame-pause ((fps controller))
                         (dolist (scene (loops controller))
                           (issue scene 'tick)
                           (process scene)
                           (render scene main))
                         (render-hud controller main)
                         (q+:swap-buffers main)))))))
  (v:debug :trial.controller "Exiting update-loop."))

(define-handler (controller resize resize) (ev width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (perspective-view 45 (/ width (max 1 height)) 0.01 1000.0)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:viewport 0 0 width height))

(define-handler (controller tick tick 100) (ev)
  (incf (tickcount controller))
  (when (= 0 (mod (tickcount controller) (fps controller)))
    (cl-gamepad:detect-devices))
  (cl-gamepad:process-events))

(define-handler (controller mapping T 100) (ev)
  (map-event ev *loop*))

(define-handler (controller launch-editor) (ev)
  (signal! *main-window* (launch-editor)))

(defun render (scene main)
  (gl:clear :color-buffer :depth-buffer)
  (gl:load-identity)
  (gl:enable :depth-test :blend :cull-face :texture-2d)
  ;; FIXME: Move into camera code
  (gl:translate 0 -30 -200)
  (paint scene main))

(defun render-hud (controller main)
  (gl:with-pushed-matrix* (:projection)
    (gl:load-identity)
    (gl:ortho 0 (q+:width main) (q+:height main) 0 -1 10)
    (gl:matrix-mode :modelview)
    (gl:load-identity)
    (gl:disable :cull-face)
    (gl:clear :depth-buffer)

    (q+:render-text main 20 20 (format NIL "Pause: ~,10f" (last-pause controller))))
  (gl:matrix-mode :modelview))

(defun perspective-view (fovy aspect z-near z-far)
  ;; http://nehe.gamedev.net/article/replacement_for_gluperspective/21002/
  (let* ((fh (* (tan (* (/ fovy 360) PI)) z-near))
         (fw (* fh aspect)))
    (gl:frustum (- fw) fw (- fh) fh z-near z-far)))


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

(defun funcall-in-scene (scene func &optional bindings)
  (let ((event (make-instance 'execute :func func :bindings bindings)))
    (issue scene event)
    (values-list
     (loop for result = (result event)
           do (sleep 0.01)
              (case (car result)
                (:failure (error (cdr result)))
                (:success (return (cdr result))))))))
