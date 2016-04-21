#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defun perspective-view (fovy aspect z-near z-far)
  ;; http://nehe.gamedev.net/article/replacement_for_gluperspective/21002/
  (let* ((fh (* (tan (* (/ fovy 360) PI)) z-near))
         (fw (* fh aspect)))
    (gl:frustum (- fw) fw (- fh) fh z-near z-far)))

(define-subject camera (located-entity)
  ((target :initarg :target :accessor target)
   (up :initarg :up :accessor up)
   (fov :initarg :fov :accessor fov))
  (:default-initargs
   :target (vec 0 0 0)
   :up (vec 0 1 0)
   :location (vec 0 30 200)
   :fov 75))

(define-generic-handler (camera project-view tick -100))

(defmethod project-view :before ((camera camera) ev)
  (let ((width (width *main*))
        (height (height *main*)))
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (perspective-view (fov camera) (/ width (max 1 height)) 0.01 100000.0)
    (gl:matrix-mode :modelview)
    (gl:viewport 0 0 width height)))

(defmethod project-view ((camera camera) ev)
  (look-at (location camera) (target camera) (up camera)))

(defun look-at (eye target up)
  (let* ((z (nvunit (v- eye target)))
         (x (nvunit (vc up z)))
         (y (nvunit (vc z x))))
    (gl:mult-matrix
     (matrix-4x4
      (vx x) (vx y) (vx z) 0.0f0
      (vy x) (vy y) (vy z) 0.0f0
      (vz x) (vz y) (vz z) 0.0f0
       0.0f0  0.0f0  0.0f0 1.0f0))
    (gl:translate (- (vx eye)) (- (vy eye)) (- (vz eye)))))

(define-subject pivot-camera (camera)
  ()
  (:default-initargs
   :target NIL))

(defmethod project-view ((camera pivot-camera) ev)
  (when (target camera)
    (look-at (location camera)
             (location (target camera))
             (up camera))))

(define-subject following-camera (camera)
  ()
  (:default-initargs
   :target NIL))

(defmethod project-view ((camera following-camera) ev)
  (when (target camera)
    (look-at (v+ (location camera)
                 (location (target camera)))
             (location (target camera))
             (up camera))))
