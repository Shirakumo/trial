(in-package #:org.shirakumo.fraf.trial.examples)

(defclass culling-camera (editor-camera)
  ((visible-count :initform 0 :accessor visible-count)
   (culled-count :initform 0 :accessor culled-count)))

(defmethod map-visible (function (camera culling-camera) (container container))
  (setf (visible-count camera) 0)
  (setf (culled-count camera) 0)
  (do-scene-graph (object container)
    (cond ((in-view-p object camera)
           (funcall function object)
           (incf (visible-count camera))
           (when (typep object 'basic-sphere)
             (setf (color object) #.(vec 1 1 1 1))))
          (T
           (when (feature-enabled-p :depth-clamp)
             (funcall function object))
           (incf (culled-count camera))
           (when (typep object 'basic-sphere)
             (setf (color object) #.(vec 1 0 0 1)))))))

(define-shader-entity basic-sphere (vertex-entity colored-entity transformed-entity global-bounds-cached-entity)
  ((vertex-array :initform (// 'trial 'unit-sphere))))

(define-class-shader (basic-sphere :fragment-shader)
  "out vec4 color;

void main@after(){
  color.rgb *= clamp(1-pow(gl_FragCoord.z,20), 0.1, 1.0);
}")

(defmethod bsize ((_ basic-sphere)) (vec3 1))
(defmethod bradius ((_ basic-sphere)) 1f0)

(define-example culling
  :title "Frustum Culling"
  :description "Test workbench for camera frustum culling."
  (enter (make-instance 'display-controller) scene)
  (enter (make-instance 'culling-camera :near-plane 1 :far-plane 500) scene)
  (enter (make-instance 'render-pass) scene)
  (observe! (visible-count (camera scene)) :title "Visible")
  (observe! (culled-count (camera scene)) :title "Culled")
  (dotimes (i 10000)
    (enter (make-instance 'basic-sphere :location (vrand 0f0 1000.0) :scaling (vec3 (random* 1.0 3.0))) scene)))

(defmethod setup-ui ((scene culling-scene) panel)
  (let ((layout (make-instance 'alloy:grid-layout :col-sizes '(T 120 200) :row-sizes '(30)))
        (focus (make-instance 'alloy:vertical-focus-list))
        (camera (camera scene))
        (clamp NIL))
    (setf (clamp-depth-p (render-state *context*)) NIL)
    (alloy:enter "Near Plane" layout :col 1 :row 0)
    (alloy:represent (near-plane camera) 'alloy:ranged-wheel :range '(0.1 . 100.0) :layout-parent layout :focus-parent focus)
    (alloy:enter "Far Plane" layout :col 1 :row 1)
    (alloy:represent (far-plane camera) 'alloy:ranged-wheel :range '(101.0 . 10000.0)  :layout-parent layout :focus-parent focus)
    (alloy:enter "FoVy" layout :col 1 :row 2)
    (alloy:represent (fov camera) 'alloy:ranged-wheel :range '(1.0 . 89.0)  :layout-parent layout :focus-parent focus)
    (alloy:enter "Show Culled" layout :col 1 :row 3)
    (let ((a (alloy:represent clamp 'alloy:switch :layout-parent layout :focus-parent focus)))
      (alloy:on alloy:activate (a)
        (setf (clamp-depth-p (render-state *context*)) clamp)))
    (alloy:finish-structure panel layout focus)))
