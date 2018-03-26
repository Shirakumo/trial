#|
This file is a part of trial
(c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.trial.physics)


(defgeneric relax (constraint delta)
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
                     :stiffness 1.0))

(defmethod initialize-instance :after ((constraint distance-constraint) &key target)
  (when (v= (location (point-a constraint)) (location (point-b constraint)))
    (error "Point locations are the same!"))
  (setf (target constraint) (or target (vlength (v- (location (point-b constraint))
                                                    (location (point-a constraint)))))))

(defvar *bluh* T)

(defmethod relax ((constraint distance-constraint) delta)
  (let* ((point-a (point-a constraint))
         (point-b (point-b constraint))
         (loc-a (location point-a))
         (loc-b (location point-b))
         (normal (v- loc-a loc-b))
         (diff (- (vlength normal) (target constraint)))
         (mass-a (if (and (= 0 (mass point-a))
                          (= 0 (mass point-b)))
                     1/2
                     (/ (mass point-a) (+ (mass point-a) (mass point-b)))))
         (mass-b (if (and (= 0 (mass point-a))
                          (= 0 (mass point-b)))
                     1/2
                     (/ (mass point-b) (+ (mass point-a) (mass point-b))))))
    (when (and (/= 0 diff) (/= 0 (vlength normal)))
      (let ((move (v* (vunit normal) diff)))
        (setf (location point-a) (v+ loc-a (v* move mass-a))
              (location point-b) (v- loc-b (v* move mass-b)))))))

(defclass pin-constraint ()
  ((point :initarg :point :reader point)
   (location :initarg :location :accessor location))
  (:default-initargs :point (error "POINT required.")))

(defmethod relax ((constraint pin-constraint) delta)
  (declare (ignore delta))
  (setf (location (point constraint)) (location constraint)))

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


(defmethod relax ((constraint angle-constraint) delta)
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
    (unless (= 0 diff)
      (let ((diff (* diff delta (stiffness constraint))))
        (nvrot (location point-a) (location point-b) diff)
        (nvrot (location point-c) (location point-b) (- diff))
        (nvrot (location point-b) (location point-a) diff)
        (nvrot (location point-b) (location point-c) (- diff))))))

(defclass frame-constraint ()
  ((point :initarg :point :accessor point)
   (min-point :initarg :min :accessor min-point)
   (max-point :initarg :max :accessor max-point))
  (:default-initargs :point (error "POINT required.")
                     :min NIL
                     :max NIL))

(defmethod relax ((constraint frame-constraint) delta)
  (declare (ignore delta))
  (let* ((point (point constraint))
         (loc (location point))
         (min (ensure-vector-type (or (min-point constraint) loc)
                                  (type-of loc)
                                  loc))
         (max (ensure-vector-type (or (max-point constraint) loc)
                                  (type-of loc)
                                  loc)))
    (setf (location point)
          (etypecase loc
            (vec2 (vec2 (min (vx max) (max (vx min) (vx loc)))
                        (min (vy max) (max (vy min) (vy loc)))))
            (vec3 (vec3 (min (vx max) (max (vx min) (vx loc)))
                        (min (vy max) (max (vy min) (vy loc)))
                        (min (vz max) (max (vz min) (vz loc)))))
            (vec4 (vec4 (min (vx max) (max (vx min) (vx loc)))
                        (min (vy max) (max (vy min) (vy loc)))
                        (min (vz max) (max (vz min) (vz loc)))
                        (min (vw max) (max (vw min) (vw loc)))))))))
