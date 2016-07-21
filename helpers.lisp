#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defclass located-entity (entity)
  ((location :initarg :location :accessor location))
  (:default-initargs
   :location (vec 0 0 0)))

(define-saved-slots located-entity location)

(defmethod paint :around ((obj located-entity) target)
  (with-pushed-matrix
    (let ((location (location obj)))
      (gl:translate (vx location) (vy location) (vz location))
      (call-next-method))))

(defclass oriented-entity (entity)
  ((orientation :initarg :orientation :accessor orientation)
   (up :initarg :up :accessor up))
  (:default-initargs
   :orientation (vec 1 0 0)
   :up (vec 0 1 0)))

(define-saved-slots oriented-entity orientation up)

(defmethod paint :around ((obj oriented-entity) target)
  (with-pushed-matrix
    (let ((axis (vc (up obj) (orientation obj)))
          (angle (* 180 (/ (acos (v. (up obj) (orientation obj))) PI))))
      (gl:rotate angle (vx axis) (vy axis) (vz axis))
      (call-next-method))))

(defclass rotated-entity (entity)
  ((axis :initarg :axis :accessor axis)
   (angle :initarg :angle :accessor angle))
  (:default-initargs
   :axis (vec 0 1 0)
   :angle 0))

(define-saved-slots rotated-entity axis angle)

(defmethod paint :around ((obj rotated-entity) target)
  (with-pushed-matrix
    (let ((axis (axis obj)))
      (gl:rotate (angle obj) (vx axis) (vy axis) (vz axis))
      (call-next-method))))

(defclass pivoted-entity (entity)
  ((pivot :initarg :pivot :accessor pivot))
  (:default-initargs
   :pivot (vec 0 0 0)))

(define-saved-slots pivoted-entity pivot)

(defmethod paint :around ((obj pivoted-entity) target)
  (with-pushed-matrix
    (let ((pivot (pivot obj)))
      (gl:translate (vx pivot) (vy pivot) (vz pivot))
      (call-next-method))))

(defclass bound-entity (located-entity)
  ((bounds :initarg :bounds :accessor bounds))
  (:default-initargs
   :bounds (vec 0 0 0)))

(define-saved-slots bound-entity bounds)

(defmethod min-bound ((entity bound-entity) &key relative)
  (if relative (vec 0 0 0) (location entity)))

(defmethod max-bound ((entity bound-entity) &key relative)
  (v+ (if relative (vec 0 0 0) (location entity)) (bounds entity)))

(defmethod contains ((this bound-entity) (other bound-entity))
  (and (v<= (min-bound this) (min-bound other)) (v<= (max-bound other) (max-bound this))))

(defmethod intersects ((this bound-entity) (other bound-entity) &key ignore-y)
  (let* ((this-min (min-bound this))
         (other-min (min-bound other))
         (this-max (max-bound this))
         (other-max (max-bound other)))
    (when ignore-y
      (setf (vy this-min) 0
            (vy other-min) 0
            (vy this-max) 1
            (vy other-max) 1))
    ;; TODO: Make this better. It's essentially the same thing twice.
    (cond ((and (v<= this-min other-min) (v<= other-min this-max))
           (make-instance 'intersection-entity
                          :location other-min
                          :bounds (v+ other-min this-max)
                          :first-object this
                          :second-object other))
          ((and (v<= other-min this-min) (v<= this-min other-max))
           (make-instance 'intersection-entity
                          :location this-min
                          :bounds (v+ this-min other-max)
                          :first-object other
                          :second-object this)))))

#+trial-debug-bound-entity
(defmethod paint :after ((entity bound-entity) target)
  (when (draw-bounds-p entity)
    (gl:line-width 1.0)
    (gl:color 0.0 0.8 0.0)
    (gl:disable :cull-face)
    (gl:polygon-mode :front-and-back :line)
    (with-primitives :quads
      (let ((min (min-bound entity :relative T))
            (max (max-bound entity :relative T)))
        (gl:vertex (vx min) (vy min) (vz min))
        (gl:vertex (vx max) (vy min) (vz min))
        (gl:vertex (vx max) (vy max) (vz min))
        (gl:vertex (vx min) (vy max) (vz min))

        (gl:vertex (vx min) (vy min) (vz max))
        (gl:vertex (vx max) (vy min) (vz max))
        (gl:vertex (vx max) (vy max) (vz max))
        (gl:vertex (vx min) (vy max) (vz max))
        
        (gl:vertex (vx min) (vy min) (vz min))
        (gl:vertex (vx min) (vy max) (vz min))
        (gl:vertex (vx min) (vy max) (vz max))
        (gl:vertex (vx min) (vy min) (vz max))
        
        (gl:vertex (vx max) (vy min) (vz min))
        (gl:vertex (vx max) (vy max) (vz min))
        (gl:vertex (vx max) (vy max) (vz max))
        (gl:vertex (vx max) (vy min) (vz max))
        
        (gl:vertex (vx min) (vy min) (vz min))
        (gl:vertex (vx max) (vy min) (vz min))
        (gl:vertex (vx max) (vy min) (vz max))
        (gl:vertex (vx min) (vy min) (vz max))
        
        (gl:vertex (vx min) (vy max) (vz min))
        (gl:vertex (vx max) (vy max) (vz min))
        (gl:vertex (vx max) (vy max) (vz max))
        (gl:vertex (vx min) (vy max) (vz max))))
    (gl:enable :cull-face)
    (gl:polygon-mode :front-and-back :fill)
    (gl:color 1.0 1.0 1.0)))

(defclass intersection-entity (bound-entity)
  ((first-object :initarg :first-object :accessor first-object)
   (second-object :initarg :second-object :accessor second-object)
   (distance :initarg :distance :accessor distance)
   (normal :initarg :normal :accessor normal)
   (ray :initarg :ray :accessor ray))
  (:default-initargs
   :distance most-positive-single-float
   :normal (vec 0 0 0)))

(defclass collidable-entity (bound-entity)
  ((old-location :initform NIL :accessor old-location)))

(defmethod handle-collision ((entity collidable-entity) intersection))

(defclass colored-entity (entity)
  ((color :initarg :color :accessor color))
  (:default-initargs
   :color (vec 0.0 0.0 1.0)))

(define-saved-slots colored-entity color)

(defmethod paint :before ((entity colored-entity) target)
  (let ((c (color entity)))
    (gl:color (vx c) (vy c) (vz c))))

(defclass textured-entity (entity)
  ((texture :initform NIL :accessor texture)))

(defmethod initialize-instance :after ((entity textured-entity) &key (texture NIL t-p) &allow-other-keys)
  (when t-p (setf (texture entity) texture)))

(defmethod reinitialize-instance :after ((entity textured-entity) &key (texture NIL t-p) &allow-other-keys)
  (when t-p (setf (texture entity) texture)))

(defmethod (setf texture) ((resource resource) (entity textured-entity))
  (setf (slot-value entity 'texture) resource))

(defmethod (setf texture) ((null null) (entity textured-entity))
  (setf (slot-value entity 'texture) NIL))

(defmethod (setf texture) ((id list) (entity textured-entity))
  (setf (texture entity) (get-resource 'texture (first id) (second id))))

(defmethod paint :around ((obj textured-entity) target)
  (let* ((tex (texture obj))
         (target (slot-value tex 'target)))
    (when tex
      (gl:bind-texture target (data tex))
      (call-next-method)
      (gl:bind-texture target 0))))

(defclass mesh-entity (entity)
  ((mesh :initform NIL :accessor mesh)))

(defmethod initialize-instance :after ((entity mesh-entity) &key (mesh NIL t-p) &allow-other-keys)
  (when t-p (setf (mesh entity) mesh)))

(defmethod reinitialize-instance :after ((entity mesh-entity) &key (mesh NIL t-p) &allow-other-keys)
  (when t-p (setf (mesh entity) mesh)))

(defmethod (setf mesh) ((resource resource) (entity mesh-entity))
  (setf (slot-value entity 'mesh) resource))

(defmethod (setf mesh) ((null null) (entity mesh-entity))
  (setf (slot-value entity 'mesh) NIL))

(defmethod (setf mesh) ((id list) (entity mesh-entity))
  (setf (mesh entity) (get-resource 'model (first id) (second id))))

(defmethod paint ((entity mesh-entity) target)
  (loop for mesh across (data (mesh entity))
        do (wavefront-loader:draw mesh)))

(defclass shader-entity (entity)
  ((shader-program :accessor shader-program)))

(defmethod initialize-instance :after ((entity shader-entity) &key shader-program)
  (setf (shader-program entity) shader-program))

(defmethod initialize-instance :after ((entity shader-entity) &key (shader-program NIL s-p))
  (when s-p (setf (shader-program entity) shader-program)))

(defmethod (setf shader-program) ((resource resource) (entity shader-entity))
  (setf (slot-value entity 'shader-program) resource))

(defmethod (setf shader-program) ((null null) (entity shader-entity))
  (setf (slot-value entity 'shader-program) NIL))

(defmethod (setf shader-program) ((id list) (entity shader-entity))
  (setf (shader-program entity) (get-resource 'shader-program (first id) (second id))))

(defmethod paint :around ((entity shader-entity) target)
  (when (shader-program entity)
    (gl:use-program (data (shader-program entity))))
  (unwind-protect
       (call-next-method)
    (gl:use-program 0)))

(defclass face-entity (textured-entity bound-entity)
  ((tex-location :initarg :tex-location :accessor tex-location)
   (tex-bounds :initarg :tex-bounds :accessor tex-bounds))
  (:default-initargs
   :tex-location (vec 0 0 0)
   :tex-bounds (vec 1 1 0)))

(defmethod paint ((entity face-entity) target)
  (with-slots ((tl tex-location) (tb tex-bounds) bounds) entity
    (gl:disable :cull-face)
    (with-primitives :quads
      (gl:tex-coord (vx tl) (vy tl))
      (gl:vertex 0 0)
      (gl:tex-coord (vx tb) (vy tl))
      (gl:vertex (vx bounds) 0)
      (gl:tex-coord (vx tb) (vy tb))
      (gl:vertex (vx bounds) (vy bounds))
      (gl:tex-coord (vx tl) (vy tb))
      (gl:vertex 0 (vy bounds)))
    (gl:enable :cull-face)))
