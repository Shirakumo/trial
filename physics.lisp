#|
This file is a part of trial
(c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defvar *physics-gravity* 0.0d0
  "Downways.")
(defvar *physics-viscosity* 1.0d0
  "How hard is it to move horizontally when falling on a surface.")
(defvar *physics-iterations* 5
  "Number of physics calculation iterations. Increases accuracy of calculations.")

;; TODO: use a proper vector geometry from somewhere else, maybe

(defun pv2 (x y)
  (cons x y))

(defun pv2x (a)
  (car a))

(defun (setf pv2x) (value a)
  (setf (car a) value))

(defun pv2y (a)
  (cdr a))

(defun (setf pv2y) (value a)
  (setf (cdr a) value))

(defun pv2-neg (a)
  (pv2 (- (pv2x a)) (- (pv2y a))))

(defun pv2-add (a &rest rest)
  (let ((x (pv2x a))
        (y (pv2y a)))
    (for:for ((b in rest))
      (incf x (pv2x b))
      (incf y (pv2y b))
      (returning (pv2 x y)))))

(defun pv2-sub (a &rest rest)
  (let ((x (pv2x a))
        (y (pv2y a)))
    (for:for ((b in rest))
      (incf x (- (pv2x b)))
      (incf y (- (pv2y b)))
      (returning (pv2 x y)))))

(defun pv2-mul (a &rest rest)
  (let ((x (pv2x a))
        (y (pv2y a)))
    (for:for ((b in rest)
              (xd = (if (typep b 'number) b (pv2x b)))
              (yd = (if (typep b 'number) b (pv2y b))))
      (setf x (* x xd)
            y (* y yd))
      (returning (pv2 x y)))))

(defun pv2-div (a &rest rest)
  (let ((x (pv2x a))
        (y (pv2y a)))
    (for:for ((b in rest)
              (xd = (if (typep b 'number) b (pv2x b)))
              (yd = (if (typep b 'number) b (pv2y b))))
      (setf x (/ x xd)
            y (/ y yd))
      (returning (pv2 x y)))))

(defun pv2-dotp (a b)
  (+ (* (pv2x a) (pv2x b)) (* (pv2y a) (pv2y b))))

(defun pv2-length (a)
  (sqrt (pv2-dotp a a)))

(defun pv2-normal (a)
  (let* ((x (pv2x a))
         (y (pv2y a))
         (sum (+ (abs x) (abs y))))
    (pv2 (/ x sum) (/ y sum))))

(defclass physical-point ()
  ((location :initarg :location :accessor location)
   (old-location :initform NIL :accessor location)
   (acceleration :initform NIL :accessor acceleration))
  (:default-initargs :location (error "Must define a location for a point!")))

(defmethod initialize-instance :after ((point physical-point) &key old-location acceleration)
  (setf (old-location point) (or old-location (location point))
        (acceleration point) (or acceleration (pv2 0.0d0 0.0d0))))

(defclass physical-edge ()
  ((parent :initarg :parent :reader parent)
   (point-a :initarg :point-a :accessor point-a)
   (point-b :initarg :point-b :accessor point-b)
   (original-length :initarg :length :reader original-length)))

(defclass physical-entity (located-entity rotated-entity pivoted-entity)
  ((vertices :initform NIL :accessor vertices)
   (edges :initform NIL :accessor edges)
   (center :initform NIL :accessor center)
   (static-p :initarg :static-p :accessor static-p)
   (mass :initarg :mass :accessor mass))
  (:default-initargs :mass 1.0
                     :static-p NIL))

(defmethod initialize-instance :after ((entity physical-entity)
                                       &key points edges)
  "Argument points is assumed to be a list of cons where they are location values in order (x . y).
Argument edges is a list of cons where the pairs are indexes in the points list. These two points will form the edge.

Example to make a triangle:
(make-instance 'physical-entity :points '((-1 . 2) (0 . 0) (1 . 2)) :edges '((0 . 1) (1 . 2) (2 . 0)))

This is terrible and should be made more sensible someday."
  (let* ((point-count (length points))
         (edge-point-count (length edges))
         (vertices (make-array point-count :initial-element NIL))
         (edge-arr (make-array edge-point-count :initial-element NIL)))
    (for:for ((point in points)
              (i counting point)
              (x = (car point))
              (y = (cdr point)))
      (setf (aref vertices i) (make-instance 'physical-point :location (pv2 x y))))
    (for:for ((edge in edges)
              (i counting edge)
              (p1 = (car edge))
              (p2 = (cdr edge)))
      (setf (aref edge-arr i) (make-instance 'physical-edge :parent entity
                                                            :point-a (aref (vertices entity) p1)
                                                            :point-a (aref (vertices entity) p2))))
    (setf (vertices entity) vertices
          (edges entity) edge-arr)))

(defmethod calculate-center ((entity physical-entity))
  "Calculates the average of the points that form the entity's bounding box."
  (setf (center entity) (for:for ((point across (vertices entity))
                                  (i count point)
                                  (location = (location point))
                                  (sum-x summing (pv2x location))
                                  (sum-y summing (pv2y location)))
                          (returning (pv2 (/ sum-x i) (/ sum-y i))))))

(defmethod apply-forces ((entity physical-entity))
  "Movement causing effects from input also go here. And things like wind.
TODO: Fix it up to read them from somewhere."
  (let ((viscosity *physics-viscosity*)
        (gravity *physics-gravity*))
    (for:for ((point in (vertices entity))
              (loc = (location point))
              (old = (old-location point)))
      (setf (location point) (pv2 (- (* viscosity (pv2x loc)) (* viscosity (pv2x old)))
                                  (+ (- (* viscosity (pv2x loc)) (* viscosity (pv2x old))) gravity))
            (old-location point) loc))))

(defmethod update-edges ((entity physical-entity))
  "Keeps things rigid."
  (for:for ((edge in (edges entity))
            (point-a = (location (point-a edge)))
            (point-b = (location (point-b edge)))
            (a-to-b = (pv2-sub point-b point-a))
            (length = (pv2-length a-to-b))
            (diff = (- length (original-length edge)))
            (normal = (pv2-normal a-to-b)))
    (setf (location (point-a edge)) (pv2-add point-a (pv2-mul normal diff 0.5d0))
          (location (point-b edge)) (pv2-sub point-b (pv2-mul normal diff 0.5d0)))))

(defmethod project-to-axis ((entity physical-entity) axis)
  "Gets the nearest and furthest point along an axis." ;; Think of it like casting a shadow on a wall.
  (let ((min) (max))
    (for:for ((point in (vertices entity))
              (dotp = (pv2-dotp axis (location point))))
      (unless (and min (< dotp min))
        (setf min dotp))
      (unless (and max (< max dotp))
        (setf max dotp)))
    (values min max)))

(defmethod collides-p ((entity physical-entity) (other physical-entity))
  "Collision test between two entities. Does not return T or NIL, as the name would hint, but rather gives you multiple values,
depth: length of the collision vector, or how deep the objects overlap
mass-a: mass of the first entity [0,1]
mass-b: mass of the second entity (- 1 mass-a)
normal: direction of the collision vector
edge: edge that is pierced
vertex: point that pierces furthest in"
  (unless (and (static-p entity) (static-p other))
    (let ((depth) (normal) (col-edge)
          (edge-count-a (length (edges entity)))
          (edge-count-b (length (edges other))))
      (for:for ((index ranging 0 (1- (+ edge-count-a edge-count-b)))
                (edge = (if (< index edge-count-a)
                            (aref (edges entity) index)
                            (aref (edges other) (- index edge-count-a))))
                (point-a = (location (point-a edge)))
                (point-b = (location (point-b edge)))
                (axis = (pv2-normal (pv2 (- (pv2y point-a) (pv2y point-b)) (- (pv2x point-a) (pv2x point-b))))))
        (multiple-value-bind (min-a max-a)
            (project-to-axis entity axis)
          (multiple-value-bind (min-b max-b)
              (project-to-axis other axis)
            (let ((dist (if (< min-a min-b) (- min-b max-a) (- min-a max-b))))
              (when (< 0 dist) ;; Projections don't overlap
                (return-from collides-p (values)))
              (when (or (not depth) (< (abs dist) depth))
                (setf depth (abs dist) ;; This gets us these three values
                      normal axis
                      col-edge edge))))))
      (let* ((ent1 (if (eql (parent col-edge) other) entity other))
             (ent2 (if (eql (parent col-edge) other) other entity))
             (center (pv2-sub (center ent1) (center ent2))) ;; Already calculated in update-physics
             (sign (pv2-dotp normal center))
             (mass1 (cond ((static-p ent1) 0.0d0) ((static-p ent2) 1.0d0) (T (mass ent1))))
             (mass2 (cond ((static-p ent2) 0.0d0) ((static-p ent1) 1.0d0) (T (mass ent2))))
             (total-mass (+ mass1 mass2))
             (smallest-dist)
             (vertex))
        (when (< sign 0)
          (setf normal (pv2-neg normal)))
        (for:for ((point in (vertices ent1))
                  (loc = (location point))
                  (v = (pv2-sub loc (center ent2)))
                  (dist = (pv2-dotp normal v)))
          (when (or (null smallest-dist) (< dist smallest-dist))
            (setf smallest-dist dist
                  vertex point))) ;; And here we find the piercing point
        (values depth (/ mass1 total-mass) (/ mass2 total-mass) normal col-edge vertex)))))

(defun update-physics (entities)
  (for:for ((entity in entities))
    (apply-forces entity)
    (update-edges entity))
  (dotimes (i *physics-iterations*) ;; More you do it, better it gets
    (loop for list = entities then (rest list)
          for entity = (first list)
          for rest = (rest list)
          while (and entity rest)
          do (update-edges entity)
          do (calculate-center entity)
          do (for:for ((other in rest))
               (update-edges other)
               (calculate-center other)
               (multiple-value-bind (depth mass-a mass-b normal edge vertex)
                   (collides-p entity other)
                 (when depth (resolve-collision depth mass-a mass-b normal edge vertex)))))))

(defun resolve-collision (depth mass-a mass-b normal edge vertex)
  "Pushes back the two entities from one another. The normal always points towards the piercing entity."
  (let ((response (pv2-mul normal depth))) ;; Pushback for the piercing entity
    (setf (location vertex) (pv2-add (location vertex) (pv2-mul response mass-a)))
    (let* ((point-a (location (point-a edge)))
           (point-b (location (point-b edge))) ;; Pushback for the edging entity
           ;; t-point is the factor that determines where on the edge the vertex lies, [0, 1]
           ;; It has to do the if-else check so we don't accidentally divide by zero
           (t-point (if (< (abs (- (pv2y point-a) (pv2y point-b))) (abs (- (pv2x point-a) (pv2x point-b))))
                        (/ (- (pv2x (location vertex)) (pv2x response) (pv2x point-a))
                           (- (pv2x point-b) (pv2x point-a)))
                        (/ (- (pv2y (location vertex)) (pv2y response) (pv2y point-a))
                           (- (pv2y point-b) (pv2y point-a)))))
           ;; Now lambda here. It's the scaling factor for ensuring that the collision vertex lies on
           ;; the collision edge. I have no idea who came up with it but it's just
           ;; lambda = 1 / (t^2 + (1 - t)^2)
           (lmba (/ (+ (* t-point t-point) (* (- 1 t-point) (- 1 t-point))))))
      ;; And here we just reduce it for pushback
      ;; Note the (- 1 t-point) and t-point multipliers.
      ;; It causes a bit of spin if edge wasn't hit in the middle.
      ;; ... I really hope the masses are right way around.
      (setf (location (point-a edge)) (pv2-sub point-a (pv2-mul response (- 1 t-point) mass-b lmba))
            (location (point-b edge)) (pv2-sub point-b (pv2-mul response t-point mass-b lmba))))))
