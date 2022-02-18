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
  ())
