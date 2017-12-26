#|
This file is a part of trial
(c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.trial.physics)
(defpackage #:trial-verlet
  (:nicknames #:org.shirakumo.fraf.trial.physics.verlet)
  (:shadow #:scene #:entity #:load #:update)
  (:use #:cl+trial #:3d-vectors #:3d-matrices #:flare #:trial-physics)
  (:export #:verlet-entity #:viscosity))
(in-package #:org.shirakumo.fraf.trial.physics.verlet)

(defvar *default-viscosity* 1.0
  "How hard is it to move horizontally when falling on a surface.")
(defvar *iterations* 5
  "Number of physics calculation iterations. Increases accuracy of calculations.
In general, 4 is minimum for an alright accuracy, 8 is enough for a good accuracy.")

(defclass verlet-point ()
  ((location :initarg :location :accessor location)
   (old-location :initform NIL :accessor old-location)
   (acceleration :initarg :acceleration :accessor acceleration)
   (fixed-p :initarg :fixed-p :accessor fixed-p))
  (:default-initargs :location (error "Must define a location for a point!")
                     :acceleration (vec 0 0 0)
                     :fixed-p NIL))

(defmethod initialize-instance :after ((point verlet-point) &key old-location)
  (setf (old-location point) (or old-location (location point))))

(defmethod accelerate ((point verlet-point) vector)
  (unless (fixed-p point)
    (nv+ (acceleration point) vector)))

(defmethod correct ((point verlet-point) vector)
  (unless (fixed-p point)
    (nv+ (location point) vector)))

(defmethod simulate ((point verlet-point) delta)
  (unless (fixed-p point)
    (nv* (acceleration point) (* delta delta))
    (let ((location (v+ (v- (v* (location point) 2) (old-location point))
                        (acceleration point))))
      (setf (old-location point) (location point)
            (location point) location)
      (nv- (acceleration point) (acceleration point)))))

(defclass verlet-edge ()
  ((parent :initarg :parent :reader parent)
   (point-a :initarg :point-a :accessor point-a)
   (point-b :initarg :point-b :accessor point-b)
   (target :initform NIL :reader target)
   (elasticity :initarg :elasticity :accessor elasticity))
  (:default-initargs :elasticity 0
                     :point-a (error "Must define point A")
                     :point-b (error "Must define point B")))

(defmethod initialize-instance :after ((edge verlet-edge) &key)
  (setf (slot-value edge 'target) (vlength (v- (location (point-b edge))
                                               (location (point-a edge))))))

(defmethod resolve ((edge verlet-edge))
  (let* ((pos-a (location (point-a edge)))
         (pos-b (location (point-b edge)))
         (direction (vunit (v- (vunit pos-b) (vunit pos-a))))
         (length (vlength direction))
         (factor (/ (- length (target edge)) length))
         (correction (v* direction factor)))
    (correct (point-a edge) correction)
    (correct (point-b edge) (v- correction))))

(define-shader-entity verlet-entity (physical-entity vertex-entity)
  ((vertices :initform NIL :accessor vertices)
   (edges :initform NIL :accessor edges)
   (center :initform NIL :accessor center)
   (viscosity :initarg :viscosity :accessor viscosity))
  (:default-initargs :viscosity *default-viscosity*))

(defmethod initialize-instance :after ((entity verlet-entity) &key points vertex-array location)
  "Unless points are defined the vertices of the vertex-array in vertex-entity class are used."
  (let* ((point-array (or points vertex-array))
         (vertices (etypecase point-array
                     (mesh (vertices (first (inputs vertex-array))))
                     (vertex-mesh (vertices point-array))
                     (array point-array)
                     (list point-array))))
    (unless (< 3 (length vertices))
      (error "There are no vertices in the entity to form a shape."))
    (let ((vertices (quick-hull (for:for ((vertex over vertices)
                                          (v collecting (v+ location
                                                            (if (typep vertex 'textured-vertex)
                                                                (location vertex)
                                                                vertex))))))))
      (setf (vertices entity) (mapcar #'(lambda (v)
                                          (make-instance 'verlet-point :location v))
                                      vertices)))
    (let ((start (first (vertices entity)))
          (edges ())
          (last NIL))
      (loop for points = (vertices entity) then (rest points)
            while points
            for point = (first points)
            for next = (second points)
            for new-edge = (make-instance 'verlet-edge :point-a point
                                                       :point-b (or next start))
            do (if last
                   (setf (cdr last) (cons new-edge NIL)
                         last (cdr last))
                   (setf last (cons new-edge NIL)
                         edges last)))
      (setf (edges entity) edges
            (center entity) (calculate-center entity)))))

(defmethod simulate-step ((entity verlet-entity) delta)
  (for:for ((edge in (edges entity)))
    (resolve edge))
  (let ((forces (apply #'v+ (forces entity))))
    (for:for ((point in (vertices entity)))
      (accelerate point forces)
      (simulate point delta))))

(defmethod simulate ((entity verlet-entity) delta)
  (let ((delta (/ delta *iterations*)))
    (dotimes (i *iterations*)
      (simulate-step entity delta)))
  (setf (location entity) (calculate-center entity)))

(defmethod calculate-center ((entity verlet-entity))
  "Calculates the average of the points that form the entity's bounding box."
  (let ((center (vcopy (location (first (vertices entity))))))
    (for:for ((point over (rest (vertices entity)))
              (i count point))
      (nv+ center (location point))
      (returning (v/ center (1+ i))))))

(defmethod update-edges ((entity verlet-entity))
  "Keeps things rigid."
  (for:for ((edge in (edges entity))
            (point-a = (location (point-a edge)))
            (point-b = (location (point-b edge)))
            (a-to-b = (v- point-b point-a))
            (length = (vlength a-to-b))
            (diff = (- length (original-length edge)))
            (normal = (vunit a-to-b))
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
                (axis = (vunit (vec (- (vy point-a) (vy point-b)) (- (vx point-a) (vx point-b))))))
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

(defun in-between-point (p0 r0 p1 r1)
  "Calculates the point in between two points in space that is in 90 degrees from a point that is r0 distance away from p0 and r1 distance away from p1.
Values returned are the aforementioned point as a VEC and the distance from the point that the r0->r1 distances are for."
  (let* ((d (vlength (v- p1 p0)))
         (a (/ (+ (- (* r0 r0) (* r1 r1)) (* d d)) (* 2 d))))
    (values (v+ p0 (v/ (v* (v- p1 p0) a) d))
            (sqrt (+ (* a a) (* r0 r0))))))

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

#|
(defun simulate (entities &key (iterations *iterations*))
  (for:for ((entity in entities))
    (apply-forces entity)
    (update-edges entity))
  (dotimes (i iterations) ;; More you do it, better it gets
    (loop for list = entities then (rest list)
          for entity = (first list)
          for rest = (rest list)
          while (and entity rest)
          do (update-edges entity)
          do (calculate-center entity)
          do (for:for ((other in rest))
               (update-edges other)
               (calculate-center other)
               (multiple-value-call #'resolve-collision (collides-p entity other))
               (ensure-location other))
          do (ensure-location entity))))
|#
