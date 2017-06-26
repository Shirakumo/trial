#|
This file is a part of trial
(c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.trial.physics)
(defpackage #:trial-verlet
  (:nicknames #:org.shirakumo.fraf.trial.physics.verlet)
  (:shadow #:scene #:entity #:load #:update)
  (:use #:cl #:3d-vectors #:3d-matrices #:flare #:trial #:trial-physics)
  (:export #:verlet-entity #:viscosity #:update-physics))
(in-package #:org.shirakumo.fraf.trial.physics.verlet)

(defvar *default-viscosity* 1.0
  "How hard is it to move horizontally when falling on a surface.")
(defvar *iterations* 5
  "Number of physics calculation iterations. Increases accuracy of calculations.")

(defclass verlet-point ()
  ((location :initarg :location :accessor location)
   (old-location :initform NIL :accessor old-location)
   (acceleration :initform NIL :accessor acceleration))
  (:default-initargs :location (error "Must define a location for a point!")))

(defmethod initialize-instance :after ((point verlet-point) &key old-location acceleration)
  (setf (old-location point) (or old-location (location point))
        (acceleration point) (or acceleration (vec 0 0))))

(defclass verlet-edge ()
  ((parent :initarg :parent :reader parent)
   (point-a :initarg :point-a :accessor point-a)
   (point-b :initarg :point-b :accessor point-b)
   (original-length :initarg :length :reader original-length)))

(defclass verlet-entity (physical-entity)
  ((vertices :initform NIL :accessor vertices)
   (edges :initform NIL :accessor edges)
   (center :initform NIL :accessor center)
   (viscosity :initarg :viscosity :accessor viscosity))
  (:default-initargs :viscosity *default-viscosity*))

(defmethod initialize-instance :after ((entity verlet-entity)
                                       &key points edges)
"
  Argument points is assumed to be a list of cons where they are location values in order (x . y).
  Argument edges is a list of cons where the pairs are indexes in the points list. These two points will form the edge.

  Example to make a triangle:
  (make-instance 'verlet-entity :points '((-1 . 2) (0 . 0) (1 . 2)) :edges '((0 . 1) (1 . 2) (2 . 0)))

  This is terrible and should be made more sensible someday.
"
  (unless (< 3 (length points))
    (error "Must define a minimum of three points"))
  (when (< (length edges) (length points))
    (error "Must define enough edges for all points")) ;; TODO: Should we allow missing the final edge?
  (let* ((point-count (length points))
         (edge-point-count (length edges))
         (vertices (make-array point-count :initial-element NIL))
         (edge-arr (make-array edge-point-count :initial-element NIL)))
    (for:for ((point in points)
              (i counting point)
              (x = (car point))
              (y = (cdr point)))
      (setf (aref vertices i) (make-instance 'verlet-point :location (vec x y))))
    (for:for ((edge in edges)
              (i counting edge)
              (p1 = (car edge))
              (p2 = (cdr edge)))
      (setf (aref edge-arr i) (make-instance 'verlet-edge :parent entity
                                                            :point-a (aref (vertices entity) p1)
                                                            :point-a (aref (vertices entity) p2))))
    (setf (vertices entity) vertices
          (edges entity) edge-arr)))

(defmethod calculate-center ((entity verlet-entity))
  "Calculates the average of the points that form the entity's bounding box."
  (setf (center entity) (for:for ((point across (vertices entity))
                                  (i count point)
                                  (location = (location point))
                                  (sum-x summing (vx location))
                                  (sum-y summing (vy location)))
                          (returning (vec (/ sum-x i) (/ sum-y i))))))

(defmethod apply-forces ((entity verlet-entity))
  "Movement causing effects from input also go here. And things like wind."
  ;; TODO: Fix it up to read the forces from somewhere.
  (let ((viscosity (viscosity entity)))
    (for:for ((point in (vertices entity))
              (loc = (location point))
              (old = (old-location point)))
      (setf (location point) (v+ (vec (- (* viscosity (vx loc)) (* viscosity (vx old)))
                                      (- (* viscosity (vx loc)) (* viscosity (vx old))))
                                 (forces entity))
            (old-location point) loc))))

(defmethod update-edges ((entity verlet-entity))
  "Keeps things rigid."
  (for:for ((edge in (edges entity))
            (point-a = (location (point-a edge)))
            (point-b = (location (point-b edge)))
            (a-to-b = (v- point-b point-a))
            (length = (vlength a-to-b))
            (diff = (- length (original-length edge)))
            (normal = (vnormal a-to-b))
            (diff-force = (v* normal diff 0.5)))
    (setf (location (point-a edge)) (v+ point-a diff-force)
          (location (point-b edge)) (v- point-b diff-force))))

(defmethod project-to-axis ((entity verlet-entity) axis)
  "Gets the nearest and furthest point along an axis." ;; Think of it like casting a shadow on a wall.
  (let ((min) (max))
    (for:for ((point in (vertices entity))
              (dotp = (v. axis (location point))))
      (unless (and min (< dotp min))
        (setf min dotp))
      (unless (and max (< max dotp))
        (setf max dotp)))
    (values min max)))

(defmethod collides-p ((entity verlet-entity) (other verlet-entity))
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
                (axis = (vnormal (vec (- (vy point-a) (vy point-b)) (- (vx point-a) (vx point-b))))))
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
             (center (v- (center ent1) (center ent2))) ;; Already calculated in update-physics
             (sign (v. normal center))
             (mass1 (cond ((static-p ent1) 0.0) ((static-p ent2) 1.0) (T (mass ent1))))
             (mass2 (cond ((static-p ent2) 0.0) ((static-p ent1) 1.0) (T (mass ent2))))
             (total-mass (+ mass1 mass2))
             (smallest-dist)
             (vertex))
        (when (< sign 0)
          (setf normal (v- normal)))
        (for:for ((point in (vertices ent1))
                  (loc = (location point))
                  (v = (v- loc (center ent2)))
                  (dist = (v. normal v)))
          (when (or (null smallest-dist) (< dist smallest-dist))
            (setf smallest-dist dist
                  vertex point))) ;; And here we find the piercing point
        (values depth (/ mass1 total-mass) (/ mass2 total-mass) normal col-edge vertex)))))

(defun resolve-collision (depth mass-a mass-b normal edge vertex)
  "Pushes back the two entities from one another. The normal always points towards the piercing entity."
  (when (and depth mass-a mass-b normal edge vertex)
    (let ((response (v* normal depth))) ;; Pushback for the piercing entity
      (setf (location vertex) (v+ (location vertex) (v* response mass-a)))
      (let* ((point-a (location (point-a edge)))
             (point-b (location (point-b edge))) ;; Pushback for the edging entity
             ;; t-point is the factor that determines where on the edge the vertex lies, [0, 1]
             ;; It has to do the if-else check so we don't accidentally divide by zero
             (t-point (if (< (abs (- (vy point-a) (vy point-b))) (abs (- (vx point-a) (vx point-b))))
                          (/ (- (vx (location vertex)) (vx response) (vx point-a))
                             (- (vx point-b) (vx point-a)))
                          (/ (- (vy (location vertex)) (vy response) (vy point-a))
                             (- (vy point-b) (vy point-a)))))
             ;; Now lambda here. It's the scaling factor for ensuring that the collision vertex lies on
             ;; the collision edge. I have no idea who came up with it but it's just
             ;; lambda = 1 / (t^2 + (1 - t)^2)
             (lmba (/ (+ (* t-point t-point) (* (- 1 t-point) (- 1 t-point))))))
        ;; And here we just reduce it for pushback
        ;; Note the (- 1 t-point) and t-point multipliers.
        ;; It causes a bit of spin if edge wasn't hit in the middle.
        ;; ... I really hope the masses are right way around.
        ;; TODO: Should we consider kinetic energy as well as the mass?
        ;;       We do not store velocity with verlet physics. Leave it for the force application?
        (setf (location (point-a edge)) (v- point-a (v* response (- 1 t-point) mass-b lmba))
              (location (point-b edge)) (v- point-b (v* response t-point mass-b lmba)))))))

(defun update-physics (entities)
  (for:for ((entity in entities))
    (apply-forces entity)
    (update-edges entity))
  (dotimes (i *iterations*) ;; More you do it, better it gets
    (loop for list = entities then (rest list)
          for entity = (first list)
          for rest = (rest list)
          while (and entity rest)
          do (update-edges entity)
          do (calculate-center entity)
          do (for:for ((other in rest))
               (update-edges other)
               (calculate-center other)
               (multiple-value-call #'resolve-collision (collides-p entity other))))))
