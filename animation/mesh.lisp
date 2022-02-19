#|
 This file is a part of trial
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.animation)

(defclass skeleton ()
  ((rest-pose :initarg :rest-pose :accessor rest-pose)
   (bind-pose :initarg :bind-pose :accessor bind-pose)
   (inv-bind-pose :accessor inv-bind-pose)
   (joint-names :initarg :joint-names :accessor joint-names)))

(defmethod shared-initialize :after ((skeleton skeleton) slots &key)
  (update-bind-pose skeleton))

(defmethod (setf bind-pose) :after (pose (skeleton skeleton))
  (update-bind-pose skeleton))

(defun update-bind-pose (skeleton)
  (let* ((pose (bind-pose skeleton))
         (inv (make-array (length pose))))
    (dotimes (i (length inv) (setf (inv-bind-pose skeleton) inv))
      (setf (svref inv i) (minv (tmat4 (global-transform pose i)))))))

(defclass mesh ()
  ((name :initarg :name :initform NIL :accessor trial:name)
   (position-normals :initform (make-array 0 :element-type 'single-float) :accessor position-normals)
   (vertex-data :initform (make-array 0 :element-type 'single-float) :accessor vertex-data)
   (index-data :initform NIL :accessor index-data)))

(defmethod (setf vertex-data) :after (data (mesh mesh))
  (let ((vertices (truncate (length data) (+ 3 3 2 4 4))))
    (setf (position-normals mesh) (adjust-array (position-normals mesh) (* vertices (+ 3 3))
                                                :initial-element 0f0))))

(defmethod cpu-skin ((mesh mesh) pose)
  (let ((pos-normal (position-normals mesh))
        (vertex-data (vertex-data mesh)))
    (flet ((transform (mat out-i in-i w)
             (let ((vec (vec (aref vertex-data (+ in-i 0))
                             (aref vertex-data (+ in-i 1))
                             (aref vertex-data (+ in-i 2))
                             w)))
               (n*m mat vec)
               (setf (aref pos-normal (+ out-i 0)) (vx vec))
               (setf (aref pos-normal (+ out-i 1)) (vy vec))
               (setf (aref pos-normal (+ out-i 2)) (vz vec)))))
      (loop for i from 0 below (length pos-normal) by (+ 3 3)
            for j from 0 by (+ 3 3 2 4 4)
            for mat = (meye 4)
            do (loop for idx from 0 below 4
                     for joint = (floor (aref vertex-data (+ j idx 3 3 2)))
                     for weight = (aref vertex-data (+ j idx 3 3 2 4))
                     do (nm+ mat (m* (svref pose joint) weight)))
               (transform mat (+ i 0) (+ j 0) 1.0)
               (transform mat (+ i 3) (+ j 3) 0.0)))))

(defmethod make-vertex-array ((mesh mesh) vao)
  (let ((vertex-data (make-instance 'trial:vertex-buffer :buffer-data (vertex-data mesh)))
        (position-normals (make-instance 'trial:vertex-buffer :buffer-data (position-normals mesh)))
        (index-data (index-data mesh)))
    (trial:ensure-instance vao 'trial:vertex-array
                           :bindings (list* `(,position-normals :size 3 :offset 0 :stride 12)
                                            `(,position-normals :size 3 :offset 12 :stride 12)
                                            `(,vertex-data :size 2 :offset 24 :stride 64)
                                            `(,vertex-data :size 4 :offset 32 :stride 64)
                                            `(,vertex-data :size 4 :offset 48 :stride 64)
                                            (when index-data
                                              (list (make-instance 'trial:vertex-buffer :buffer-data index-data :buffer-type :element-array-buffer))))
                           :size (if index-data
                                     (length index-data)
                                     (truncate vertex-data (+ 3 3 2 4 4))))))

(defmethod trial:update-buffer-data ((vao trial:vertex-array) (mesh mesh) &key)
  (let ((buffer (caar (trial:bindings vao))))
    (trial:update-buffer-data buffer (position-normals mesh))))
