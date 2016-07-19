#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(define-subject controller (hud-entity)
  (;; Has to be a double to avoid bignums after ~3.8 hours of runtime.
   (tick-count :initform 0.0d0 :accessor tick-count)
   (display :initform NIL :accessor display))
  (:default-initargs
   :name :controller))

(defmethod paint-hud ((controller controller) target)
  (when (typep target 'amin)
    (let ((font (get-resource 'font :trial :debug-hud))
          (clock (clock (scene (display controller)))))
      (q+:render-text target 20 30 (format NIL "Pause: ~,10f" (last-pause controller))
                      (data font))
      (q+:render-text target 20 50 (format NIL "Time:  ~2,'0d:~6,3,,,'0f"
                                           (floor (/ (round clock) 60))
                                           (mod clock 60))
                      (data font)))))

(define-handler (controller tick tick 100) (ev)
  (incf (tick-count controller)))

(define-handler (controller mapping T 100) (ev)
  (map-event ev *loop*)
  (retain-event ev))

(define-handler (controller reload-assets reload-assets 99) (ev)
  (dolist (pool (pools))
    (mapc #'reload (assets pool))))

(define-handler (controller reload-scene reload-scene 99) (ev)
  (let ((scene (first (loops controller))))
    (dolist (obj (flare-indexed-set:coerce-set (objects scene) 'list))
      (unless (eql obj controller)
        (leave obj scene)
        (finalize obj))))
  (setup-scene (display controller)))

(define-handler (controller execute-request) (ev)
  (execute ev))

(define-handler (controller launch-editor) (ev)
  (when (find-package '#:org.shirakumo.fraf.trial.editor)
    (with-body-in-gui (main :return-values NIL)
      (funcall (find-symbol (string '#:launch) '#:org.shirakumo.fraf.trial.editor) main))))
