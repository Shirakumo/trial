(in-package #:org.shirakumo.fraf.trial.examples)

(define-example selection
  :title "Object Selection by Mouse"
  :description "Showcases the selection buffer feature, which allows efficient object selection by mouse."
  :slots ((selection-buffer :initform (make-instance 'selection-buffer) :accessor selection-buffer))
  (enter (make-instance 'render-pass) scene)
  (enter (make-instance 'editor-camera :location (vec3 0.0 1.0 15) :fov 50 :move-speed 0.1) scene)
  (enter (make-instance 'vertex-entity :vertex-array (// 'trial 'grid)) scene)
  (loop for i from 0 to 10
        for box = (make-instance 'ubox :location (vrand 0.0 10.0) :orientation (qrand))
        do (enter box scene)
           (enter box (selection-buffer scene))))

(defmethod handle :after ((event mouse-press) (scene selection-scene))
  (let ((selected (select (pos event) (selection-buffer scene))))
    (when selected
      (vsetf (color selected) 0 1 0 1))))

(defmethod stage :after ((scene selection-scene) (area staging-area))
  (stage (selection-buffer scene) area))
