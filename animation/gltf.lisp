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

(defmethod gltf:construct-element-reader ((element-type (eql :scalar)) (component-type (eql :float)))
  (lambda (ptr)
    (values (cffi:mem-ref ptr :float)
            (cffi:incf-pointer ptr 4))))

(defmethod gltf:construct-element-reader ((element-type (eql :vec3)) (component-type (eql :float)))
  (lambda (ptr)
    (values (vec (cffi:mem-ref ptr :float)
                 (cffi:mem-ref (cffi:incf-pointer ptr 4) :float)
                 (cffi:mem-ref (cffi:incf-pointer ptr 4) :float))
            (cffi:incf-pointer ptr 4))))

(defmethod gltf:construct-element-reader ((element-type (eql :vec4)) (component-type (eql :float)))
  (lambda (ptr)
    (values (quat (cffi:mem-ref ptr :float)
                  (cffi:mem-ref (cffi:incf-pointer ptr 4) :float)
                  (cffi:mem-ref (cffi:incf-pointer ptr 4) :float)
                  (cffi:mem-ref (cffi:incf-pointer ptr 4) :float))
            (cffi:incf-pointer ptr 4))))

(defmethod gltf:construct-element-reader ((element-type (eql :mat4)) (component-type (eql :float)))
  (lambda (ptr)
    (let ((elements (make-array 16 :element-type 'single-float)))
      (dotimes (i (length elements))
        (setf (aref elements i) (cffi:mem-aref ptr :float i)))
      (values (mat4 elements)
              (cffi:inc-pointer ptr (* 4 16))))))

(defun load-joint-names (gltf)
  (map 'vector #'gltf:name (gltf:nodes gltf)))

(defun load-rest-pose (gltf)
  (let* ((nodes (gltf:nodes gltf))
         (pose (make-instance 'pose :size (length nodes))))
    (loop for i from 0 below (length nodes)
          for node = (aref nodes i)
          do (setf (elt pose i) (node-transform node))
             (setf (parent-joint pose i) (if (gltf:parent node)
                                             (gltf:idx (gltf:parent node))
                                             -1)))
    pose))

(defun access (json accessor)
  (let ((view (g json "bufferViews" (g accessor "bufferView")))
        (buff (g json "buffers" (g view "buffer"))))))

(defun load-track (track sampler)
  (setf (interpolation track) (ecase (gltf:interpolation sampler)
                                (:step :constant)
                                (:linear :linear)
                                (:cubicspline :hermite)))
  (setf (frames track) (cons (gltf:input sampler) (gltf:output sampler))))

(defun load-clip (animation)
  (let ((clip (make-instance 'clip :name (gltf:name animation))))
    (loop for channel across (gltf:channels animation)
          for sampler = (svref (gltf:samplers animation) (gltf:sampler channel))
          for track = (find-track clip (gltf:idx (gltf:node (gltf:target channel))) :if-does-not-exist :create)
          do (ecase (gltf:path (gltf:target channel))
               (:translation (load-track (location track) sampler))
               (:scale (load-track (scaling track) sampler))
               (:rotation (load-track (rotation track) sampler))))
    clip))

(defun load-clips (gltf)
  (map 'vector #'load-clip (gltf:animations gltf)))

(defun load-bind-pose (gltf)
  (let* ((rest-pose (load-rest-pose gltf))
         (world-bind-pose (make-array (length rest-pose))))
    (dotimes (i (length world-bind-pose))
      (setf (svref world-bind-pose i) (global-transform rest-pose i)))
    (loop for skin across (gltf:skins gltf)
          for joints = (gltf:joints skin)
          for acc = (gltf:inverse-bind-matrices skin)
          do (loop for i from 0 below (length joints)
                   for inv-bind-matrix = (elt acc i)
                   do (setf (aref world-bind-pose (gltf:idx (svref joints i)))
                            (tfrom-mat (minv inv-bind-matrix)))))
    (let ((bind-pose rest-pose))
      (loop for i from 0 below (length world-bind-pose)
            for current = (svref world-bind-pose i)
            for p = (parent-joint bind-pose i)
            do (setf (elt bind-pose i)
                     (if (<= 0 p)
                         (t+ (tinv (svref world-bind-pose p)) current)
                         current)))
      bind-pose)))

(defun load-skeleton (gltf)
  (make-instance 'skeleton :rest-pose (load-rest-pose gltf)
                           :bind-pose (load-bind-pose gltf)
                           :joint-names (load-joint-names gltf)))
