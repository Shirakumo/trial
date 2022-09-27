#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass camera (located-entity listener)
  ((near-plane :initarg :near-plane :accessor near-plane)
   (far-plane :initarg :far-plane :accessor far-plane))
  (:default-initargs
   :name :camera
   :location (vec 0 30 200)
   :near-plane 1f0
   :far-plane 1000000.0f0))

(defgeneric project-view (camera))
(defgeneric setup-perspective (camera width height))
(defgeneric map-visible (function camera container))
(defgeneric in-view-p (object camera))

(defmethod handle ((ev tick) (camera camera))
  (project-view camera))

(defmethod handle ((ev resize) (camera camera))
  (setup-perspective camera (width ev) (height ev)))

(defmethod (setf near-plane) :after (val (camera camera))
  (setup-perspective camera))

(defmethod (setf far-plane) :after (val (camera camera))
  (setup-perspective camera))

(defmethod setup-perspective :before ((camera camera) w h)
  (reset-matrix *projection-matrix*))

(defmethod project-view :before ((camera camera))
  (reset-matrix))

(defmethod map-visible (function (camera camera) (container flare:container))
  (for:for ((object over container))
    (when (in-view-p object camera)
      (funcall function object))))

(defmethod map-visible (function (camera null) (container flare:container))
  (for:for ((object over container))
    (funcall function object)))

(defmacro do-visible ((entity camera container &optional return) &body body)
  `(block NIL
     (map-visible (lambda (,entity) ,@body) ,camera ,container)
     ,return))

(defmethod in-view-p (object (camera camera)) T)

(defclass 2d-camera (camera)
  ()
  (:default-initargs
   :near-plane 0.0
   :far-plane 100.0
   :location (vec 0 0 200)))

(defmethod setup-perspective ((camera 2d-camera) width height)
  (orthographic-projection 0 (max 1 width) 0 (max 1 height) (near-plane camera) (far-plane camera)))

(defmethod project-view ((camera 2d-camera))
  (reset-matrix *view-matrix*)
  (translate (v- (location camera)) *view-matrix*))

(defclass sidescroll-camera (2d-camera)
  ((zoom :initarg :zoom :accessor zoom)
   (target :initarg :target :accessor target))
  (:default-initargs
   :zoom 1.0
   :target NIL))

(defmethod project-view ((camera sidescroll-camera))
  (let ((z (zoom camera)))
    (reset-matrix *view-matrix*)
    (scale-by z z z *view-matrix*)
    (translate (v- (location camera) (location (target camera))) *view-matrix*)))

(defclass 3d-camera (camera)
  ((fov :initarg :fov :accessor fov))
  (:default-initargs
   :fov 75))

(defmethod (setf fov) :after (val (camera 3d-camera))
  (setup-perspective camera (max 1 (width *context*)) (max 1 (height *context*))))

(defmethod setup-perspective ((camera 3d-camera) width height)
  (perspective-projection (fov camera) (/ (max 1 width) (max 1 height)) (near-plane camera) (far-plane camera)))

(defclass target-camera (3d-camera)
  ((target :initarg :target :accessor target)
   (up :initarg :up :accessor up))
  (:default-initargs
   :target (vec 0 0 0)
   :up (vec 0 1 0)))

(defmethod project-view ((camera target-camera))
  (look-at (location camera) (target camera) (up camera)))

(defclass pivot-camera (target-camera)
  ()
  (:default-initargs
   :target NIL))

(defmethod project-view ((camera pivot-camera))
  (when (target camera)
    (look-at (location camera)
             (location (target camera))
             (up camera))))

(defclass following-camera (target-camera)
  ()
  (:default-initargs
   :target NIL))

(defmethod project-view ((camera following-camera))
  (when (target camera)
    (look-at (v+ (location camera)
                 (location (target camera)))
             (location (target camera))
             (up camera))))

(defclass fps-camera (3d-camera)
  ((rotation :initarg :rotation :accessor rotation)
   (acceleration :initarg :acceleration :accessor acceleration)
   (x-inverted :initarg :x-inverted :accessor x-inverted)
   (y-inverted :initarg :y-inverted :accessor y-inverted))
  (:default-initargs
   :rotation (vec 0 0 0)
   :acceleration 0.01
   :x-inverted NIL
   :y-inverted NIL))

(defmethod project-view ((camera fps-camera))
  (reset-matrix (view-matrix))
  (rotate +vx+ (vx (rotation camera)) (view-matrix))
  (rotate +vy+ (vy (rotation camera)) (view-matrix))
  (translate (v- (the vec3 (location camera))) (view-matrix)))

(defun do-fps-movement (camera old-pos pos)
  (let ((delta (v- pos old-pos)))
    (when (x-inverted camera) (setf (vx delta) (- (vx delta))))
    (when (y-inverted camera) (setf (vy delta) (- (vy delta))))
    (nv+ (rotation camera) (nv* (vyx_ delta)
                                (acceleration camera)))
    (nvmod (rotation camera) (* 2 PI))))

(define-handler (fps-camera mouse-move) (old-pos pos)
  (do-fps-movement fps-camera old-pos pos))

(defclass freeroam-camera (fps-camera)
  ((move-speed :initarg :move-speed :accessor move-speed))
  (:default-initargs
   :move-speed 1))

(define-handler (freeroam-camera tick :after) ()
  (let* ((loc (location freeroam-camera))
         (rot (rotation freeroam-camera))
         (speed (* (move-speed freeroam-camera)
                   (if (retained :left-shift) 5 1)
                   (if (retained :left-alt) 1/5 1))))
    (cond ((retained :a)
           (decf (vx loc) (* speed (cos (vy rot))))
           (decf (vz loc) (* speed (sin (vy rot)))))
          ((retained :d)
           (incf (vx loc) (* speed (cos (vy rot))))
           (incf (vz loc) (* speed (sin (vy rot))))))
    (cond ((retained :w)
           (incf (vx loc) (* speed (sin (vy rot))))
           (decf (vz loc) (* speed (cos (vy rot))))
           (decf (vy loc) (* speed (sin (vx rot)))))
          ((retained :s)
           (decf (vx loc) (* speed (sin (vy rot))))
           (incf (vz loc) (* speed (cos (vy rot))))
           (incf (vy loc) (* speed (sin (vx rot))))))
    (cond ((retained :space)
           (incf (vy loc) speed))
          ((retained :c)
           (decf (vy loc) speed)))))

(defclass editor-camera (freeroam-camera)
  ())

(define-handler (editor-camera mouse-move) (old-pos pos)
  (when (or (retained :middle)
            (retained :left-control))
    (do-fps-movement editor-camera old-pos pos)))
