#|
 This file is a part of trial
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.animation)

(defun node-transform (node)
  (let ((matrix (gltf:matrix node))
        (translation (gltf:translation node))
        (scale (gltf:scale node))
        (rotation (gltf:rotation node)))
    (let ((transform (if matrix
                         (tfrom-mat (mat4 matrix))
                         (transform))))
      (when translation
        (vsetf (tlocation transform)
               (aref translation 0)
               (aref translation 1)
               (aref translation 2)))
      (when scale
        (vsetf (tscaling transform)
               (aref scale 0)
               (aref scale 1)
               (aref scale 2)))
      (when rotation
        (qsetf (trotation transform)
               (aref rotation 0)
               (aref rotation 1)
               (aref rotation 2)
               (aref rotation 3)))
      transform)))

(defun load-rest-pose (json)
  (let* ((nodes (g json "nodes"))
         (pose (make-instance 'pose :size (length nodes))))
    (flet ((find-parent (child)
             (loop for i from 0 below (length nodes)
                   do (when (find child (gethash "children" (aref nodes i)))
                        (return i)))))
      (loop for i from 0 below (length nodes)
            for node = (aref nodes i)
            do (setf (elt pose i) (node-transform node))
               (setf (parent-joint pose i) (find-parent i))))
    pose))

(defun access (json accessor)
  (let ((view (g json "bufferViews" (g accessor "bufferView")))
        (buff (g json "buffers" (g view "buffer"))))))

(defun load-track (json channel)
  (let* ((sampler (g json "animations" "samplers" (g channel "sampler")))
         (interpolation (g sampler "interpolation"))
         (interpolation (cond ((string-equal interpolation "STEP") :constant)
                              ((string-equal interpolation "LINEAR") :linear)
                              ((string-equal interpolation "CUBICSPLINE") :hermite)
                              (T (error "Unknown interpolation type: ~s" interpolation)))))))
