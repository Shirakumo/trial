#|
 This file is a part of trial
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defun %adjust-array (array length constructor)
  (let* ((old (length array)))
    (setf array (adjust-array array length))
    (loop for i from old below length
          do (setf (svref array i) (funcall constructor)))
    array))

(defclass ik-constraint ()
  ())

(defgeneric apply-constraint (constraint solver i))

(defclass ik-solver (sequences:sequence standard-object)
  ((ik-chain :initform #() :accessor ik-chain)
   (constraints :initform #() :accessor constraints)
   (iterations :initarg :iterations :initform 15 :accessor iterations)
   (threshold :initarg :threshold :initform 0.00001 :accessor threshold)))

(defgeneric solve-for (target solver))

(defmethod sequences:length ((solver ik-solver))
  (length (ik-chain solver)))

(defmethod sequences:adjust-sequence ((solver ik-solver) length &rest args)
  (declare (ignore args))
  (setf (ik-chain solver) (%adjust-array (ik-chain solver) length #'transform))
  (setf (constraints solver) (%adjust-array (constraints solver) length (constantly NIL)))
  solver)

(defmethod sequences:elt ((solver ik-solver) index)
  (svref (ik-chain solver) index))

(defmethod (setf sequences:elt) ((transform transform) (solver ik-solver) index)
  (setf (svref (ik-chain solver) index) transform)
  transform)

(defmethod (setf sequences:elt) ((constraint ik-constraint) (solver ik-solver) index)
  (setf (svref (constraints solver) index) constraint)
  constraint)

(defmethod global-transform ((solver ik-solver) i)
  (let* ((chain (ik-chain solver))
         (result (svref chain i)))
    ;; FIXME: optimize to only allocate one transform
    (loop for parent downfrom (1- i) to 0
          do (setf result (t+ (svref chain parent) result)))
    result))

(defmethod solve-for ((target transform) (solver ik-solver))
  (solve-for (tlocation target) solver))

(defclass ccd-solver (ik-solver)
  ())

(defmethod solve-for ((goal vec3) (solver ccd-solver))
  (let* ((chain (ik-chain solver))
         (constraints (constraints solver))
         (size (length chain))
         (last (1- size))
         (threshold2 (expt (threshold solver) 2)))
    (when (<= 0 last)
      (dotimes (i (iterations solver))
        (when (< (vsqrdistance goal (tlocation (global-transform solver last))) threshold2)
          (return T))
        (loop for i downfrom (- size 2) to 0
              do (let* ((effector (tlocation (global-transform solver last)))
                        (local (svref chain i))
                        (world (global-transform solver i))
                        (location (tlocation world))
                        (rotation (trotation world))
                        (to-goal (v- goal location))
                        (effector-to-goal (if (< 0.00001 (vsqrlength to-goal))
                                              (qtowards (v- effector location) to-goal)
                                              (quat)))
                        (world-rotated (q* rotation effector-to-goal))
                        (local-rotate (q* world-rotated (qinv rotation))))
                   (q<- (trotation local) (q* local-rotate (trotation local)))
                   (let ((constraint (svref constraints i)))
                     (when constraint
                       (apply-constraint constraint solver i)))
                   (when (< (vsqrdistance goal (tlocation (global-transform solver last))) threshold2)
                     (return T))))))))

(defclass fabrik-solver (ik-solver)
  ((world-chain :initform #() :accessor world-chain)
   (lengths :initform #() :accessor lengths)))

(defmethod sequences:adjust-sequence :after ((solver fabrik-solver) length &rest args)
  (declare (ignore args))
  (setf (world-chain solver) (%adjust-array (world-chain solver) length #'vec3))
  (setf (lengths solver) (%adjust-array (lengths solver) length (constantly 0.0))))

(defun %ik-chain-to-world (solver)
  (let ((chain (ik-chain solver))
        (world-chain (world-chain solver))
        (lengths (lengths solver)))
    (dotimes (i (length chain) solver)
      (let ((world (global-transform solver i)))
        (v<- (aref world-chain i) (tlocation world))
        (when (<= 1 i)
          (setf (aref lengths i) (vdistance (tlocation world) (aref world-chain (1- i)))))))))

(defun %world-to-ik-chain (solver)
  (let ((chain (ik-chain solver))
        (world-chain (world-chain solver)))
    (loop for i from 0 below (1- (length chain))
          for next = (global-transform solver (1+ i))
          for world = (global-transform solver 0) then next
          do (let* ((location (tlocation world))
                    (rinv (qinv (trotation world)))
                    (to-next (q*v rinv (v- (tlocation next) location)))
                    (to-desired (q*v rinv (v- (aref world-chain (1+ i)) location)))
                    (delta (qtowards to-next to-desired)))
               (q<- (trotation (aref chain i)) (q* delta (trotation (aref chain i))))))))

(defmethod solve-for ((goal vec3) (solver fabrik-solver))
  (let* ((chain (ik-chain solver))
         (constraints (constraints solver))
         (world-chain (world-chain solver))
         (lengths (lengths solver))
         (size (length chain))
         (last (1- size))
         (threshold2 (expt (threshold solver) 2))
         (base (vcopy (aref world-chain 0)))
         (effector (aref world-chain last)))
    (%ik-chain-to-world solver)
    (dotimes (i (iterations solver))
      (when (< (vsqrdistance goal effector) threshold2)
        (return))
      ;; Backwards iteration
      (v<- (aref world-chain last) goal)
      (loop for i downfrom (- size 2) to 0
            for offset = (nv* (nvunit (v- (aref world-chain i) (aref world-chain (1+ i))))
                              (aref lengths (1+ i)))
            do (v<- (aref world-chain i) (nv+ offset (aref world-chain (1+ i)))))
      ;; Forwards iteration
      (v<- (aref world-chain 0) base)
      (loop for i from 1 below size
            for offset = (nv* (nvunit (v- (aref world-chain i) (aref world-chain (1- i))))
                              (aref lengths i))
            do (v<- (aref world-chain i) (nv+ offset (aref world-chain (1- i)))))
      ;; Apply constraints
      (dotimes (i size)
        (let ((constraint (svref constraints i)))
          (when constraint
            (%world-to-ik-chain solver)
            (apply-constraint constraint solver i)
            (%ik-chain-to-world solver)))))
    (%world-to-ik-chain solver)
    (< (vsqrdistance goal (tlocation (global-transform solver last))) threshold2)))

(defclass ball-socket-constraint (ik-constraint)
  ((axis :initarg :axis :initform +vz3+ :accessor axis)
   (limit :initarg :limit :initform (/ PI 4) :accessor limit)))

(defmethod apply-constraint ((constraint ball-socket-constraint) solver i)
  (when (< 0 i)
    (let* ((parent (trotation (global-transform solver (1- i))))
           (this (trotation (global-transform solver i)))
           (parent-dir (q*v parent (axis constraint)))
           (this-dir (q*v this (axis constraint)))
           (angle (vangle parent-dir this-dir)))
      (when (< (limit constraint) angle)
        (let* ((correction (vc parent-dir this-dir))
               (world-space (q* parent (qfrom-angle correction (limit constraint)))))
          (q<- (trotation (elt solver i)) (q* world-space (qinv parent))))))))

(defclass hinge-constraint (ik-constraint)
  ((axis :initarg :axis :initform +vx3+ :accessor axis)
   (min-angle :initarg :min-angle :initform 0.0 :accessor min-angle)
   (max-angle :initarg :max-angle :initform (float (* 2 PI) 0f0) :accessor max-angle)))

(defmethod apply-constraint ((constraint hinge-constraint) solver i)
  (when (< 0 i)
    (let* ((current (q*v (trotation (global-transform solver i)) (axis constraint)))
           (desired (q*v (trotation (global-transform solver (1- i))) (axis constraint)))
           (rot (trotation (elt solver i))))
      ;; FIXME: allow constraining the angle as well
      (nq* rot (qtowards current desired))
      (setf (qangle rot) (clamp-angle (min-angle solver) (qangle rot) (max-angle solver))))))
