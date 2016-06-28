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
   (display :initarg :display :accessor display))
  (:default-initargs
   :name :controller
   :fps 30.0f0
   :display (error "DISPLAY required.")))

(defmethod enter :after ((controller controller) (scene scene))
  (setf (update-thread controller)
        (with-thread ("controller update-thread")
          (update-loop controller))))

(defmethod finalize :after ((controller controller))
  (let ((thread (update-thread controller)))
    (with-thread-exit (thread)
      (setf (update-thread controller) NIL))))

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
  (let* ((display (display controller))
         (scene (scene display)))
    (v:info :test "~a ~a" display *context*)
    (with-slots-bound (controller controller)
      (unwind-protect
           (loop while (update-thread controller)
                 do (with-simple-restart (abort "Abort the update and retry.")
                      (setf (last-pause controller)
                            (with-frame-pause ((fps controller))
                              ;; Potentially release context every time to allow
                              ;; other threads to grab it.
                              (with-context (display :reentrant T)
                                (issue scene 'tick :tick-count (tick-count controller))
                                (process scene)
                                (render display display)
                                (render-hud display display)
                                (q+:swap-buffers display))))))
        
        (stop scene))))
  (v:info :trial.controller "Exiting update-loop."))

(defmethod render-hud ((controller controller) (display display))
  #+trial-debug-selection-buffer
  (paint (unit :selection-buffer (scene display)) :hud)
  
  (let ((font (get-resource 'font :trial :debug-hud)))
    (q+:render-text display 20 30 (format NIL "Pause: ~,10f" (last-pause controller))
                    (data font))
    (q+:render-text display 20 50 (format NIL "Time:  ~2,'0d:~6,3,,,'0f"
                                       (floor (/ (round (clock (scene display))) 60))
                                       (mod (clock (scene display)) 60))
                    (data font))))

(define-handler (controller tick tick 100) (ev)
  (incf (tick-count controller)))

(define-handler (controller mapping T 100) (ev)
  (map-event ev *loop*)
  (retain-event ev))

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

(define-handler (controller execute) (ev)
  (execute ev))

(defun funcall-in-scene (scene func &key bindings (return-values T))
  (with-execution (return-values event 'execute-request :func func :bindings bindings )
    (issue scene event)))
