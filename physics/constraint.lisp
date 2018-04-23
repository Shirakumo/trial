#|
This file is a part of trial
(c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.trial.physics)

(defclass distance-constraint () ())
(defclass pin-constraint () ())
(defclass angle-constraint () ())
(defclass frame-constraint () ())

(defgeneric relax (constraint &optional preserve-impulse)
  (:documentation "Relaxes the constraint towards its resting position."))

(defclass distance-constraint ()
  ((point-a :initarg :point-a :reader point-a)
   (point-b :initarg :point-b :reader point-b)
   (tear :initarg :tear :accessor tear)
   (target :initform NIL :accessor target)
   (stiffness :initarg :stiffness :accessor stiffness))
  (:default-initargs :point-a (error "POINT-A required.")
                     :point-b (error "POINT-B required.")
                     :tear NIL
                     :stiffness 2.1))

(defmethod initialize-instance :after ((constraint distance-constraint) &key)
  (let* ((point-a (location (point-a constraint)))
         (point-b (location (point-b constraint)))
         (target (square-distance point-a point-b)))
    (unless (< 0.1 target) (error "Point locations are the same!"))
    (setf (target constraint) target)))

(defmethod relax ((constraint distance-constraint) &optional preserve-impulse)
  (let* ((point-a (point-a constraint))
         (point-b (point-b constraint))
         (loc-a (location point-a))
         (loc-b (location point-b))
         (target (target constraint))
         (move (v- loc-b loc-a))
         (square (square-distance loc-a loc-b)))
    (when (/= square target)
      (nv* move (- (/ target (+ square target)) 0.5))
      (nv- (location point-a) move)
      (nv+ (location point-b) move)
      (when preserve-impulse
        (nv* move 0.98) ;; 0.98 for dampening
        (nv- (old-location point-a) move)
        (nv+ (old-location point-b) move)))))


(defclass pin-constraint ()
  ((point :initarg :point :reader point)
   (location :initarg :location :accessor location))
  (:default-initargs :point (error "POINT required.")))

(defmethod relax ((constraint pin-constraint) &optional preserve-impulse)
  (declare (ignore preserve-impulse))
  (let ((point (point constraint))
        (pin-to (location constraint)))
    (setf (location point) pin-to
          (old-location point) pin-to)))

(defclass angle-constraint ()
  ((point-a :initarg :point-a :accessor point-a)
   (point-b :initarg :point-b :accessor point-b)
   (point-c :initarg :point-c :accessor point-c)
   (stiffness :initarg :stiffness :accessor stiffness)
   (angle :initform NIL :accessor angle))
  (:default-initargs :point-a (error "POINT-A required.")
                     :point-b (error "POINT-B required.")
                     :point-c (error "POINT-C required.")
                     :stiffness 1.0))

(defmethod initialize-instance :after ((constraint angle-constraint) &key angle)
  (setf (angle constraint) (or angle (vangle (location (point-a constraint))
                                             (location (point-b constraint))
                                             (location (point-c constraint))))))


(defmethod relax ((constraint angle-constraint) &optional preserve-impulse)
  (declare (ignore preserve-impulse))
  (let* ((point-a (location (point-a constraint)))
         (point-b (location (point-b constraint)))
         (point-c (location (point-c constraint)))
         (angle (vangle point-a point-b point-c))
         (diff (- angle (angle constraint))))
    (cond
      ((<= diff (- PI))
       (incf diff (* 2 PI)))
      ((<= PI diff)
       (incf diff (* -2 PI))))
    (unless (< 0.01 (abs diff)) ;; Ignore tiny sub-pixels
      (let ((diff (* diff (stiffness constraint))))
        (setf (location point-a) (vrot (location point-a)
                                       (location point-b)
                                       diff)
              (location point-c) (vrot (location point-c)
                                       (location point-b)
                                       (- diff))
              (location point-b) (vrot (location point-b)
                                       (location point-a)
                                       diff)
              (location point-b) (vrot (location point-b)
                                       (location point-c)
                                       (- diff)))))))

(defclass frame-constraint ()
  ((point :initarg :point :accessor point)
   (min-point :initform NIL :accessor min-point)
   (max-point :initform NIL :accessor max-point))
  (:default-initargs :point (error "POINT required.")))

(defmethod initialize-instance :after ((constraint frame-constraint) &key point min max)
  (let* ((loc (location point))
         (negative (v* loc +negative-infinity+))
         (positive (v* loc +positive-infinity+))
         (min (or min negative))
         (max (or max positive)))
    (setf (min-point constraint) (ensure-vector-type min (type-of loc) negative)
          (max-point constraint) (ensure-vector-type max (type-of loc) positive))))

(defmethod relax ((constraint frame-constraint) &optional preserve-impulse)
  (let* ((point (point constraint))
         (loc (location point))
         (min (min-point constraint))
         (max (max-point constraint))
         (move-to 
           (etypecase loc
             (vec2 (vec2 (min (vx max) (max (vx min) (vx loc)))
                         (min (vy max) (max (vy min) (vy loc)))))
             (vec3 (vec3 (min (vx max) (max (vx min) (vx loc)))
                         (min (vy max) (max (vy min) (vy loc)))
                         (min (vz max) (max (vz min) (vz loc)))))
             (vec4 (vec4 (min (vx max) (max (vx min) (vx loc)))
                         (min (vy max) (max (vy min) (vy loc)))
                         (min (vz max) (max (vz min) (vz loc)))
                         (min (vw max) (max (vw min) (vw loc)))))))
         (diff (v- move-to loc)))
    (setf (location point) move-to)
    (when preserve-impulse
      (nv+ (old-location point) (nv* diff 0.98))))) ;; 0.98 for dampening


(defun ensure-constraint (type)
  (etypecase type
    (keyword (ecase type
               (:distance 'distance-constraint)
               (:pin 'pin-constraint)
               (:angle 'angle-constraint)
               (:frame 'frame-constraint)))
    (symbol (ecase type
              (distance 'distance-constraint)
              (pin 'pin-constraint)
              (angle 'angle-constraint)
              (frame 'frame-constraint)
              (T type)))))

(defun square-distance (vec-a vec-b)
  (declare (type vec vec-a vec-b))
  (let ((x (- (vx vec-a) (vx vec-b)))
        (y (- (vy vec-a) (vy vec-b)))
        (z (if (and (or (vec3-p vec-a) (vec4-p vec-a))
                    (or (vec3-p vec-b) (vec4-p vec-b)))
               (- (vz vec-a) (vz vec-b))
               0.0))
        (w (if (and (vec4-p vec-a) (vec4-p vec-b))
               (- (vw vec-a) (vw vec-b))
               0.0)))
    (+ (* x x) (* y y) (* z z) (* w w))))
