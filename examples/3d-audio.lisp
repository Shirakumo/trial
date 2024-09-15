(in-package #:org.shirakumo.fraf.trial.examples)

(defmethod trial-harmony:server-initargs append ((main main))
  (list :mixers '((:effect mixed:space-mixer))))

(define-example 3d-audio
  :title "3D-Audio"
  (enter (make-instance 'vertex-entity :vertex-array (// 'trial 'unit-grid)) scene)
  (enter (make-instance 'audio-source) scene)
  (enter (make-instance 'audio-camera) scene)
  (enter (make-instance 'render-pass) scene))

(defclass audio-camera (fps-camera)
  ())

(define-handler (audio-camera tick :after) ()
  (setf (harmony:location :effect) (location audio-camera))
  (setf (harmony:direction :effect) ()))

(defclass audio-source (listener located-entity)
  ())

(defmethod stage :after ((source audio-source) (area staging-areaf))
  (stage (assets:// :step-rocks) area))

(define-handler (audio-source tick) (dt)
  (let ((loc (location audio-source)))
    (debug-sphere loc 0.5)
    (play (assets:// :step-rocks) :location loc)))
