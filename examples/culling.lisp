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
  (enter (make-instance 'culling-camera :near-plane 1 :far-plane 100) scene)
  (enter (make-instance 'render-pass) scene)
  (observe! (visible-count (camera scene)) :title "Visible")
  (observe! (culled-count (camera scene)) :title "Culled")
  (dotimes (i 1000)
    (enter (make-instance 'basic-sphere :location (vrand 0f0 1000.0)) scene)))

(defmethod setup-ui ((scene culling-scene) panel)
  (let ((layout (make-instance 'alloy:grid-layout :col-sizes '(T 120 200) :row-sizes '(30)))
        (focus (make-instance 'alloy:vertical-focus-list))
        (camera (camera scene)))
    (alloy:enter "Near Plane" layout :col 1 :row 0)
    (alloy:represent (near-plane camera) 'alloy:ranged-wheel :range '(0.1 . 100.0) :layout-parent layout :focus-parent focus)
    (alloy:enter "Far Plane" layout :col 1 :row 1)
    (alloy:represent (far-plane camera) 'alloy:ranged-wheel :range '(10.0 . 10000.0)  :layout-parent layout :focus-parent focus)
    (alloy:enter "FoVy" layout :col 1 :row 2)
    (alloy:represent (fov camera) 'alloy:ranged-wheel :range '(1.0 . 89.0)  :layout-parent layout :focus-parent focus)
    (alloy:finish-structure panel layout focus)))
