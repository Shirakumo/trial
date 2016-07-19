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

(define-subject camera (located-entity)
  ((near-plane :initarg :near-plane :accessor near-plane)
   (far-plane :initarg :far-plane :accessor far-plane))
  (:default-initargs
   :location (vec 0 30 200)
   :near-plane 0.01f0
   :far-plane 1000000.0f0))

(define-generic-handler (camera project-view tick -100))
(define-generic-handler (camera setup-perspective resize))

(defmethod (setf near-plane) :after (val (camera camera))
  (setup-perspective camera))

(defmethod (setf far-plane) :after (val (camera camera))
  (setup-perspective camera))

(defmethod paint :around ((camera camera) target))

(defmethod setup-perspective :before ((camera camera) ev)
  (gl:matrix-mode :projection)
  (gl:load-identity))

(defmethod setup-perspective :after ((camera camera) ev)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:viewport 0 0 (width ev) (height ev)))

(define-subject 2d-camera (camera)
  ()
  (:default-initargs
   :near-plane most-negative-double-float
   :far-plane most-positive-double-float))

(defmethod setup-perspective ((camera 2d-camera) ev)
  (gl:ortho 0 (width ev) (height ev) 0
            (near-plane 2d-camera) (far-plane 2d-camera)))

(define-subject 3d-camera (camera)
  ((fov :initarg :fov :accessor fov))
  (:default-initargs
   :fov 75))

(defmethod (setf fov) :after (val (camera 3d-camera))
  (setup-perspective camera))

(defmethod setup-perspective  ((camera 3d-camera) ev)
  (perspective-view (fov camera) (/ (width ev) (max 1 (height ev)))
                    (near-plane camera) (far-plane camera)))

(define-subject target-camera (3d-camera)
  ((target :initarg :target :accessor target)
   (up :initarg :up :accessor up))
  (:default-initargs
   :target (vec 0 0 0)
   :up (vec 0 1 0)))

(defmethod project-view ((camera target-camera) ev)
  (look-at (location camera) (target camera) (up camera)))

(define-subject pivot-camera (target-camera)
  ()
  (:default-initargs
   :target NIL))

(defmethod project-view ((camera pivot-camera) ev)
  (when (target camera)
    (look-at (location camera)
             (location (target camera))
             (up camera))))

(define-subject following-camera (target-camera)
  ()
  (:default-initargs
   :target NIL))

(defmethod project-view ((camera following-camera) ev)
  (when (target camera)
    (look-at (v+ (location camera)
                 (location (target camera)))
             (location (target camera))
             (up camera))))

(define-subject fps-camera (3d-camera)
  ((rotation :initarg :rotation :accessor rotation)
   (acceleration :initarg :acceleration :accessor acceleration))
  (:default-initargs
   :rotation (vec 0 0 0)
   :acceleration 0.5))

(defmethod project-view ((camera fps-camera) ev)
  (gl:rotate (vx (rotation camera)) 1.0 0.0 0.0)
  (gl:rotate (vy (rotation camera)) 0.0 1.0 0.0)
  (gl:rotate (vz (rotation camera)) 0.0 0.0 1.0)
  (gl:translate (- (vx (location camera)))
                (- (vy (location camera)))
                (- (vz (location camera)))))

(define-handler (fps-camera mouse-move) (ev pos old-pos)
  (nv+ (rotation fps-camera) (nv* (nvorder (v- pos old-pos) :y :x :z)
                                  (acceleration fps-camera))))
