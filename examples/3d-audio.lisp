(in-package #:org.shirakumo.fraf.trial.examples)

(define-example 3d-audio
  :title "3D-Audio"
  ;; NOTE: For audio setup in a regular game you'll probably want to use the
  ;;       trial-harmony:settings-main and a method on trial-harmony:server-initargs
  ;;       instead.
  (trial-harmony:initialize-audio-backend
   NIL :mixers '((:effect mixed:space-mixer)))
  (enter (make-instance 'vertex-entity :vertex-array (// 'trial 'grid)) scene)
  (enter (make-instance 'audio-source) scene)
  (enter (make-instance 'audio-camera) scene)
  (enter (make-instance 'render-pass) scene))

(defmethod finalize :before ((scene 3d-audio-scene))
  (when harmony:*server*
    (trial:finalize harmony:*server*)))

(defmethod setup-ui ((scene 3d-audio-scene) panel)
  (let* ((layout (make-instance 'alloy:grid-layout :col-sizes '(200 200 T) :row-sizes '(30)))
         (focus (make-instance 'alloy:vertical-focus-list))
         (row 0))
    (macrolet ((row (label repr input &rest args)
                 `(prog2 (alloy:enter ,label layout :row row :col 0)
                      (alloy:represent ,repr ,input ,@args :focus-parent focus :layout-parent layout)
                    (incf row))))
      (row "Minimum Distance" (mixed:min-distance :effect) 'alloy:wheel)
      (row "Maximum Distance" (mixed:max-distance :effect) 'alloy:wheel)
      (row "Soundspeed" (mixed:soundspeed :effect) 'alloy:wheel)
      (row "Doppler Factor" (mixed:doppler-factor :effect) 'alloy:ranged-slider :range '(0.0 . 1.0))
      (row "Rolloff" (mixed:rolloff :effect) 'alloy:ranged-slider :range '(0.0 . 1.0))
      (row "Attenuation" (mixed:attenuation :effect) 'alloy:combo-set :value-set '(:none :inverse :linear :exponential)))
    (alloy:finish-structure panel layout focus)))

(defclass audio-camera (editor-camera)
  ((move-speed :initform 0.1)
   (last-location :initform (vec 0 1 10) :accessor last-location)
   (location :initform (vec 0 1 10))))

(define-handler (audio-camera tick :after) ()
  (setf (harmony:location :effect) (location audio-camera))
  (let ((quat (qfrom-mat (view-matrix)))
        (vec (vec3 0 0 -1)))
    (declare (dynamic-extent quat vec))
    (nvunit (!q* vec (nqinv quat) vec))
    (setf (harmony:direction :effect) vec))
  (let ((vel (v- (location audio-camera) (last-location audio-camera))))
    (declare (dynamic-extent vel))
    (setf (harmony:velocity :effect) vel)
    (v<- (last-location audio-camera) (location audio-camera))))

(defclass audio-source (listener located-entity)
  ())

(defmethod stage :after ((source audio-source) (area staging-area))
  (stage (assets:// :step-rocks) area))

(define-handler (audio-source tick) ()
  (let ((loc (location audio-source)))
    (trial::debug-sphere loc 0.5)
    (harmony:play (assets:// :step-rocks) :location loc)))
