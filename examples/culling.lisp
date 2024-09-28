(in-package #:org.shirakumo.fraf.trial.examples)

(defclass culling-camera (editor-camera)
  ((visible-count :initform 0 :accessor visible-count)
   (culled-count :initform 0 :accessor culled-count)))

(defmethod map-visible (function (camera culling-camera) (container container))
  (setf (visible-count camera) 0)
  (setf (culled-count camera) 0)
  (do-scene-graph (object container)
    (cond ((in-view-p object camera)
           (incf (visible-count camera))
           (funcall function object))
          (T
           (incf (culled-count camera))))))

(define-shader-entity basic-sphere (vertex-entity transformed-entity global-bounds-cached-entity)
  ((vertex-array :initform (// 'trial 'unit-sphere))))

(defmethod bsize ((_ basic-sphere)) (vec3 1))
(defmethod bradius ((_ basic-sphere)) 1f0)

(define-example culling
  :title "Frustum Culling"
  :description "Test workbench for camera frustum culling."
  (enter (make-instance 'display-controller) scene)
  (enter (make-instance 'culling-camera) scene)
  (enter (make-instance 'render-pass) scene)
  (observe! (visible-count (camera scene)) :title "Visible")
  (observe! (culled-count (camera scene)) :title "Culled")
  (dotimes (i 1000)
    (enter (make-instance 'basic-sphere :location (vrand 0f0 1000.0)) scene)))
