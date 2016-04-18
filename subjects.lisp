#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(define-subject textured-subject ()
  ((texture :initform NIL :accessor texture :finalized T)))

(defmethod initialize-instance :after ((subject textured-subject) &key (texture NIL t-p) &allow-other-keys)
  (when t-p (setf (texture subject) texture)))

(defmethod reinitialize-instance :after ((subject textured-subject) &key (texture NIL t-p) &allow-other-keys)
  (when t-p (setf (texture subject) texture)))

(defmethod (setf texture) ((texture texture) (subject textured-subject))
  (setf (slot-value subject 'texture) texture))

(defmethod (setf texture) ((null null) (subject textured-subject))
  (setf (slot-value subject 'texture) NIL))

(defmethod (setf texture) (thing (subject textured-subject))
  (setf (texture subject) (asset thing 'texture)))

(defmethod (setf texture) ((id list) (subject textured-subject))
  (setf (texture subject) (apply #'asset (first id) 'texture (rest id))))

(defmethod paint :around ((obj textured-subject) target)
  (let ((tex (texture obj)))
    (when tex
      (gl:bind-texture (target tex) (content tex))
      (call-next-method)
      (gl:bind-texture (target tex) 0))))

(defmethod save-form-args append ((subject textured-subject))
  `(:texture ,(texture subject)))

(define-subject located-subject ()
  ((location :initarg :location :accessor location))
  (:default-initargs
   :location (vec 0 0 0)))

(defmethod paint :around ((obj located-subject) target)
  (with-pushed-matrix
    (let ((location (location obj)))
      (gl:translate (vx location) (vy location) (vz location))
      (call-next-method))))

(defmethod save-form-args append ((subject located-subject))
  `(:location ,(location subject)))

(define-subject pivoted-subject ()
  ((pivot :initarg :pivot :accessor pivot))
  (:default-initargs
   :pivot (vec 0 0 0)))

(defmethod paint :around ((obj pivoted-subject) target)
  (with-pushed-matrix
    (let ((pivot (pivot obj)))
      (gl:translate (vx pivot) (vy pivot) (vz pivot))
      (call-next-method))))

(defmethod save-form-args append ((subject pivoted-subject))
  `(:pivot ,(pivot subject)))

(define-subject bound-subject (located-subject)
  ((bounds :initarg :bounds :accessor bounds)
   (draw-bounds-p :initform NIL :accessor draw-bounds-p))
  (:default-initargs
   :bounds (vec 0 0 0)))

(defmethod min-bound ((subject bound-subject) &key relative)
  (if relative (vec 0 0 0) (location subject)))

(defmethod max-bound ((subject bound-subject) &key relative)
  (v+ (if relative (vec 0 0 0) (location subject)) (bounds subject)))

(defmethod contains ((this bound-subject) (other bound-subject))
  (and (v<= (min-bound this) (min-bound other)) (v<= (max-bound other) (max-bound this))))

(defmethod intersects ((this bound-subject) (other bound-subject) &key ignore-y)
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
           (make-instance 'intersection-subject
                          :location other-min
                          :bounds (v+ other-min this-max)
                          :first-object this
                          :second-object other))
          ((and (v<= other-min this-min) (v<= this-min other-max))
           (make-instance 'intersection-subject
                          :location this-min
                          :bounds (v+ this-min other-max)
                          :first-object other
                          :second-object this)))))

(defmethod paint ((subject bound-subject) target)
  (when (draw-bounds-p subject)
    (gl:line-width 1.0)
    (gl:color 0.0 0.8 0.0)
    (gl:disable :cull-face)
    (gl:polygon-mode :front-and-back :line)
    (with-primitives :quads
      (let ((min (min-bound subject :relative T))
            (max (max-bound subject :relative T)))
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
    (gl:color 1.0 1.0 1.0))
  (call-next-method))

(define-subject intersection-subject (bound-subject)
  ((first-object :initarg :first-object :accessor first-object)
   (second-object :initarg :second-object :accessor second-object)
   (distance :initarg :distance :accessor distance)
   (normal :initarg :normal :accessor normal)
   (ray :initarg :ray :accessor ray))
  (:default-initargs
   :distance most-positive-single-float
   :normal (vec 0 0 0)))

(define-subject collidable-subject (bound-subject)
  ((old-location :initform NIL :accessor old-location)))

(defmethod handle-collision ((subject collidable-subject) intersection))

(define-subject oriented-subject ()
  ((orientation :initarg :orientation :accessor orientation)
   (up :initarg :up :accessor up))
  (:default-initargs
   :orientation (vec 1 0 0)
   :up (vec 0 1 0)))

(defmethod paint :around ((obj oriented-subject) target)
  (with-pushed-matrix
    (let ((axis (vc (up obj) (orientation obj)))
          (angle (* 180 (/ (acos (v. (up obj) (orientation obj))) PI))))
      (gl:rotate angle (vx axis) (vy axis) (vz axis))
      (call-next-method))))

(defmethod save-form-args append ((subject oriented-subject))
  `(:orientation ,(orientation subject)
    :up ,(up subject)))

(define-subject rotated-subject ()
  ((axis :initarg :axis :accessor axis)
   (angle :initarg :angle :accessor angle))
  (:default-initargs
   :axis (vec 0 1 0)
   :angle 0))

(defmethod paint :around ((obj rotated-subject) target)
  (with-pushed-matrix
    (let ((axis (axis obj)))
      (gl:rotate (angle obj) (vx axis) (vy axis) (vz axis))
      (call-next-method))))

(defmethod save-form-args append ((subject rotated-subject))
  `(:axis ,(axis subject)
    :angle ,(angle subject)))

(define-subject mesh-subject ()
  ((mesh :initform NIL :accessor mesh)))

(defmethod initialize-instance :after ((subject mesh-subject) &key (mesh NIL t-p) &allow-other-keys)
  (when t-p (setf (mesh subject) mesh)))

(defmethod reinitialize-instance :after ((subject mesh-subject) &key (mesh NIL t-p) &allow-other-keys)
  (when t-p (setf (mesh subject) mesh)))

(defmethod (setf mesh) ((asset asset) (subject mesh-subject))
  (setf (slot-value subject 'mesh) asset))

(defmethod (setf mesh) ((null null) (subject mesh-subject))
  (setf (slot-value subject 'mesh) NIL))

(defmethod (setf mesh) (thing (subject mesh-subject))
  (setf (mesh subject) (asset thing 'model)))

(defmethod paint ((subject mesh-subject) target)
  (loop for mesh across (content (mesh subject))
        do (wavefront-loader:draw mesh)))

(defmethod save-form-args append ((subject mesh-subject))
  `(:mesh ,(mesh subject)))

(define-subject space-axes ()
  ((size :initarg :size :accessor size)
   (grid :initarg :grid :accessor grid))
  (:default-initargs
   :size 10
   :grid 10))

(defmethod paint ((subject space-axes) target)
  (let* ((s (size subject))
         (g (* (/ (grid subject) 2) s)))
    (gl:line-width 1.0)
    (gl:color 0.3 0.3 0.3)
    (with-primitives :lines
      (loop for i from (- g) to g by s
            do (gl:vertex (- g) 0.0 i)
               (gl:vertex g 0.0 i)
               (gl:vertex i 0.0 (- g))
               (gl:vertex i 0.0 g)))
    (gl:line-width 2.0)
    (with-primitives :lines
      (gl:color 1.0 0 0)
      (gl:vertex 0 0 0)
      (gl:vertex s 0 0)
      (gl:color 0 1.0 0)
      (gl:vertex 0 0 0)
      (gl:vertex 0 s 0)
      (gl:color 0 0 1.0)
      (gl:vertex 0 0 0)
      (gl:vertex 0 0 s)))
  (gl:color 1.0 1.0 1.0))

(defmethod save-form-args append ((subject space-axes))
  `(:size ,(size subject)
    :grid ,(grid subject)))

(define-subject clocked-subject (clock)
  ())

(define-handler (clocked-subject advance-time tick) (ev)
  (update clocked-subject))

(defmethod save-form-args append ((subject clocked-subject))
  `(:clock ,(clock subject)
    :running ,(running subject)))

(define-subject shader-subject ()
  ((shader-program :accessor shader-program)))

(defmethod initialize-instance :after ((subject shader-subject) &key shader-program)
  (setf (shader-program subject) shader-program))

(defmethod initialize-instance :after ((subject shader-subject) &key (shader-program NIL s-p))
  (when s-p (setf (shader-program subject) shader-program)))

(defmethod (setf shader-program) ((program shader-program) (subject shader-subject))
  (setf (slot-value subject 'shader-program) program))

(defmethod (setf shader-program) ((null null) (subject shader-subject))
  (setf (slot-value subject 'shader-program) NIL))

(defmethod (setf shader-program) (name (subject shader-subject))
  (setf (shader-program subject) (asset name 'shader-program)))

(defmethod (setf shader-program) ((id list) (subject shader-subject))
  (setf (shader-program subject) (apply #'asset (first id) 'shader-program (rest id))))

(defmethod paint :around ((subject shader-subject) target)
  (when (shader-program subject)
    (gl:use-program (content (shader-program subject))))
  (unwind-protect
       (call-next-method)
    (gl:use-program 0)))
