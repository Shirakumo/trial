#|
This file is a part of trial
(c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.trial.physics)

(defvar *iterations* 5
  "Number of physics calculation iterations. Increases accuracy of calculations.
In general, 4 is minimum for an alright accuracy, 8 is enough for a good accuracy.")


(define-shader-entity verlet-point (located-entity) ())
(define-shader-entity verlet-entity (physical-entity vertex-entity) ())


(define-shader-entity verlet-point (located-entity)
  ((old-location :initform NIL :accessor old-location)
   (mass :initarg :mass :accessor mass))
  (:default-initargs :mass 1))

(defmethod initialize-instance :after ((point verlet-point) &key old-location)
  (setf (old-location point) (or old-location (location point))))

(defmethod simulate ((point verlet-point) delta &key forces)
  (let ((velocity (v- (location point) (old-location point))))
    (setf (old-location point) (location point))
    (nv+ (location point) (v* (if forces (v+ forces velocity) velocity)
                              delta))))

(define-shader-entity verlet-entity (physical-entity vertex-entity)
  ((mass-points :initform NIL :accessor mass-points)
   (constraints :initform NIL :accessor constraints)
   (center :initform NIL :accessor center)))

(defmethod initialize-instance :after ((entity verlet-entity) &key mass-points vertex-array location)
  (let* ((point-array (or mass-points vertex-array))
         (mass-points (etypecase point-array
                        (mesh (vertices (first (inputs vertex-array))))
                        (vertex-mesh (mass-points point-array))
                        ((or list array) point-array))))
    (unless (< 3 (length mass-points))
      (error "MASS-POINTS or VERTEX-ARRAY required"))
    (let ((mass-points (quick-hull (for:for ((point over mass-points)
                                             (v collecting (v+ location
                                                               (if (typep point 'textured-vertex)
                                                                   (location point)
                                                                   point))))))))
      (setf (mass-points entity) (mapcar #'(lambda (v) (make-instance 'verlet-point :location v))
                                         mass-points)
            (constraints entity)
            (loop with first = (first (mass-points entity))
                  for points = (mass-points entity) then (rest points)
                  while points
                  for current = (first points)
                  for next = (or (second points) first)
                  collect (make-instance 'distance-constraint :point-a current
                                                              :point-b next))))
    (let* ((points (mass-points entity))
           (center (calculate-center entity))
           (point-a (first points))
           (point-b (second points))
           (point-c (third points)))
      (setf (center entity) (list point-a (vlength (v- center (location point-a)))
                                  point-b (vlength (v- center (location point-b)))
                                  point-c (vlength (v- center (location point-c))))))
    (setf (slot-value entity 'location) location)))

(defmethod location :around ((entity verlet-entity))
  (let ((center (triangulate-center entity)))
    (unless (v= center (call-next-method))
      (setf (slot-value entity 'location) center))
    center))

(defmethod (setf location) (value (entity verlet-entity))
  (unless (typep value 'vec) (error "Invalid VALUE."))
  (let ((diff (v- value (triangulate-center entity))))
    (for:for ((point in (mass-points entity)))
      (nv+ (location point) diff)))
  (call-next-method))

(defmethod triangulate-center ((entity verlet-entity))
  ;; TODO: is this even potentially faster than CALCULATE-CENTER?
  (let ((point-a (location (first (center entity))))
        (center (apply #'triangulate (mapcar #'(lambda (p)
                                                 (if (typep p 'verlet-point)
                                                     (location p)
                                                     p))
                                             (center entity)))))
    (etypecase point-a
      (vec2 center)
      (vec3 (vec3 (vx center) (vy center) (vz point-a)))
      (vec4 (vec4 (vx center) (vy center) (vz point-a) (vw point-a))))))

(defmethod calculate-center ((entity verlet-entity))
  "Calculates the average of the points that form the entity's bounding box."
  (let ((center (v* (location (first (mass-points entity))) 0)))
    (for:for ((point in (mass-points entity))
              (i count point))
      (nv+ center (location point))
      (returning (v/ center i)))))


(defun verlet-simulation (entities delta &key forces (iterations *iterations*))
    ;; Simulations
  (for:for ((entity in entities)
            (static-forces = (static-forces entity))
            (all-forces = (when (or forces static-forces)
                            (apply #'v+ (append (if (vec-p forces)
                                                    (list forces)
                                                    forces)
                                                (if (vec-p static-forces)
                                                    (list static-forces)
                                                    static-forces))))))
    (for:for ((point in (mass-points entity)))
      (simulate point delta :forces all-forces)))
  (let ((dlt (/ delta iterations)))
    ;; Relax constraints and fix rotation
    (for:for ((entity in entities))
      (dotimes (i iterations)
        (for:for ((constraint in (constraints entity)))
          (relax constraint dlt)))
      (setf (slot-value entity 'location) (triangulate-center entity)))))

#|
(when (= 0 *foo*) (v:warn :simulate "~a,~a"
                          (location (first (mass-points (first entities))))
                          (triangulate-center (first entities))))
(when (<= 1 (incf *foo* delta))
  (setf *foo* 0))
(defvar *foo* 0)

(defmethod min-max-point ((entity verlet-entity))
  "Finds the minimum and maximum point that is theoretically within the entity's area."
  (values-list
   (for:for ((point in (mass-points entity))
             (location = (location point))
             (max-x maximize (vx location))
             (max-y maximize (vy location))
             (max-z when (or (vec3-p location) (vec4-p location))
                    maximize (vz location))
             (max-w when (vec4-p location) maximize (vw location))
             (min-x minimize (vx location))
             (min-y minimize (vy location))
             (min-z when (or (vec3-p location) (vec4-p location))
                    minimize (vz location))
             (min-w when (vec4-p location) minimize (vw location)))
     (returning (list (cond
                        (min-w (vec4 min-x min-y min-z min-w))
                        (min-z (vec3 min-x min-y min-z))
                        (T (vec2 min-x min-y)))
                      (cond
                        (max-w (vec4 max-x max-y max-z max-w))
                        (max-z (vec3 max-x max-y max-z))
                        (T (vec2 max-x max-y))))))))

(defmethod project-to-axis ((entity verlet-entity) axis)
  "Gets the nearest and furthest point along an axis." ;; Think of it like casting a shadow on a wall.
  (declare (type vec2 axis))
  (for:for ((point in (mass-points entity))
            (location = (location point))
            (dotp = (v. axis (vec2 (vx location) (vy location)))) ;; 2D
            (min minimize dotp)
            (max maximize dotp))
    (returning (values min max))))

(defmethod collides-p ((entity verlet-entity) (other verlet-entity))
  "Collision test between two entities. Does not return T or NIL, as the name would hint, but rather gives you multiple values,
depth: length of the collision vector, or how deep the objects overlap
normal: direction of the collision vector
col-edge: edge that is pierced"
  (unless (and (static-p entity) (static-p other))
    ;; AABB vs. AABB first
    (multiple-value-bind (min-a max-a)
        (min-max-point entity)
      (multiple-value-bind (min-b max-b)
          (min-max-point other)
        (when (and (<= (vx min-a) (vx max-b))
                   (<= (vx min-b) (vx max-a))
                   (<= (vy min-a) (vy max-b))
                   (<= (vy min-b) (vy max-a))
                   (if (or (and (vec3-p max-a) (vec3-p max-b))
                           (and (vec4-p max-a) (vec4-p max-b)))
                       (and (<= (vz min-a) (vz max-b))
                            (<= (vz min-b) (vz max-a)))
                       T)
                   (if (and (vec4-p max-a) (vec4-p max-b))
                       (and (<= (vw min-a) (vw max-b))
                            (<= (vw min-b) (vw max-a)))
                       T))
          ;; AABB v AABB success
          (let ((edge-count (length (edges entity)))
                depth normal col-edge entity-1 entity-2)
            ;; FIXME: We're assuming 2D from here on out, points of interest are marked "2D"
            (for:for ((edge in (append (edges entity) (edges other)))
                      (i counting edge)
                      (vector = (v- (location (point-b edge))
                                    (location (point-a edge))))
                      (axis = (vunit (vec2 (vy vector) (vx vector))))) ;; 2D
              (multiple-value-bind (min-a max-a)
                  (project-to-axis entity axis)
                (multiple-value-bind (min-b max-b)
                    (project-to-axis other axis)
                  (let ((dist (if (< min-a min-b) (- min-b max-a) (- min-a max-b))))
                    ;; Test projection overlap
                    (when (< 0 dist) (return-from collides-p (values)))
                    (when (or (not depth) (< (abs dist) depth))
                      ;; This gets us these five values
                      (setf depth (abs dist)
                            normal axis
                            col-edge edge
                            entity-1 (if (< edge-count i) entity other)
                            entity-2 (if (< edge-count i) other entity)))))))
            (let* ((center-1 (location entity-1))
                   (center-2 (location entity-2))
                   (center (v- center-1 center-2))
                   (sign (v. normal (vec2 (vx center) (vy center)))) ;; 2D
                   (total-mass (+ (mass entity-1) (mass entity-2)))
                   (mass-1 (cond
                             ((static-p entity-1) 0.0)
                             ((static-p entity-2) 1.0)
                             (T (/ (mass entity-1) total-mass))))
                   (mass-2 (cond
                             ((static-p entity-2) 0.0)
                             ((static-p entity-1) 1.0)
                             (T (/ (mass entity-2) total-mass)))))
              (when (< sign 0) (setf normal (v- normal)))
              (values-list 
               (for:for ((point in (mass-points entity-1))
                         (loc = (location point))
                         (vec = (v- loc center-2))
                         (distance = (v. normal (vec2 (vx vec) (vy vec)))) ;; 2D
                         (smallest-dist minimizing distance)
                         (smallest-point when (= smallest-dist distance) = point))
                 (returning (list depth mass-1 mass-2
                                  normal col-edge
                                  smallest-point)))))))))))

(defun resolve-collision (&optional depth mass-a mass-b normal edge point)
  "Pushes back the two entities from one another. The normal always points towards the piercing entity."
  (when (and depth mass-a mass-b normal edge point)
    (v:warn :resolve "physics.")
    (let* ((point-loc (location point))
           (normal (etypecase point-loc
                     (vec2 (vec2 (vx normal) (vy normal)))
                     (vec3 (vec3 (vx normal) (vy normal) 0))
                     (vec4 (vec4 (vx normal) (vy normal) 0 0))))
           (response (v* normal depth))) ;; Pushback for the piercing entity
      (nv+ (location point) (v* response mass-a))
      (let* ((point-a (location (point-a edge)))
             (point-b (location (point-b edge))) ;; Pushback for the edging entity
             ;; t-point is the factor that determines where on the edge the point lies, [0, 1]
             ;; It has to do the if-else check so we don't accidentally divide by zero
             (t-point (if (< (abs (- (vy point-a) (vy point-b))) (abs (- (vx point-a) (vx point-b))))
                          (/ (- (vx (location point)) (vx response) (vx point-a))
                             (- (vx point-b) (vx point-a)))
                          (/ (- (vy (location point)) (vy response) (vy point-a))
                             (- (vy point-b) (vy point-a)))))
             ;; Now lambda here. It's the scaling factor for ensuring that the collision point lies on
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

(defun verlet-simulation (entities delta &key forces)
  (for:for ((entity in entities))
    (simulate entity delta :forces forces)) ;; Move to the new spots and apply forces
  (let ((delta (/ delta *iterations*)))
    (dotimes (i *iterations*)
      (loop for remaining = entities then (rest remaining)
            for rest = (rest remaining)
            while (and remaining rest)
            for entity = (first remaining)
            do (for:for ((point in (mass-points entity)))
                 (simulate point delta))
            do (for:for ((other in rest))
                 (for:for ((point in (mass-points other)))
                   (simulate point delta))
                 (multiple-value-call #'resolve-collision (collides-p entity other)))))))
|#
