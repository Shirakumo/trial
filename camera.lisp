#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(define-subject camera (located-subject oriented-subject)
  ((target :initform (vec 0 0 0) :accessor target)))

(defmethod initialize-instance :after ((camera camera) &key)
  (setf (location camera) (vec 0 0 3)
        (direction camera) (nvunit (v- (location camera) (target camera)))))

(defun lookat (target up)
  (let* ((forward (vunit target))
         (side (vunit (v* up forward)))
         (up (v* forward side))
         (matrix (make-array (* 3 3))))
    (setf (elt matrix 0) (vx side) ;; first row
          (elt matrix 1) (vy side)
          (elt matrix 2) (vz side)
          (elt matrix 3) (vx up)   ;; second row
          (elt matrix 4) (vy up)
          (elt matrix 5) (vz up)
          (elt matrix 6) (vx forward) ;; third row
          (elt matrix 7) (vy forward)
          (elt matrix 8) (vz forward))))
