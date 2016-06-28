#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(define-subject controller ()
  (;; Has to be a double to avoid bignums after ~3.8 hours of runtime.
   (tick-count :initform 0.0d0 :accessor tick-count))
  (:default-initargs
   :name :controller))

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

(defmacro with-body-in-scene ((scene &key bindings (return-values T)) &body body)
  `(funcall-in-scene ,scene (lambda () ,@body) :bindings ,bindings :return-values ,return-values))
