#|
This file is a part of trial
(c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.trial.physics)
(defpackage #:trial-verlet
  (:nicknames #:org.shirakumo.fraf.trial.physics.verlet)
  (:shadow #:screen-edge-2d-constraint)
  (:use #:cl+trial #:3d-vectors #:3d-matrices #:trial-physics)
  (:export #:verlet-entity #:viscosity))
(in-package #:org.shirakumo.fraf.trial.physics.verlet)

(defvar *default-viscosity* 1.0
  "How hard is it to move horizontally when falling on a surface.")
(defvar *iterations* 5
  "Number of physics calculation iterations. Increases accuracy of calculations.
In general, 4 is minimum for an alright accuracy, 8 is enough for a good accuracy.")

(define-shader-entity verlet-point () ())
(defclass verlet-link () ())
(define-shader-entity verlet-entity () ())

(define-shader-entity verlet-point (located-entity)
  ((id :initform (gensym "VERLET-POINT") :reader id)
   (old-location :initform NIL)
   (mass :initarg :mass :accessor mass)
   (acceleration :initform NIL :accessor acceleration)
   (links :initarg :links :accessor links)
   (dampen :initarg :dampen :accessor dampen)
   (pinned :initarg :pinned :accessor pinned))
  (:default-initargs :location (error "LOCATION required.")
                     :mass 1
                     :links NIL
                     :dampen 0.99
                     :pinned NIL))

(defmethod initialize-instance :after ((point verlet-point) &key old-location acceleration)
  (setf (old-location point) (or old-location (location point))
        (acceleration point) (or acceleration (v* (location point) 0)))
  (let ((old-mass (mass point)))
    (unless (and (typep old-mass 'rational) (= old-mass (round old-mass)))
      (setf (mass point) (rationalize (round old-mass)))
      (v:warn :verlet-point "MASS was not a round rational number, converting it from ~a => ~a"
              old-mass (mass point)))
    (when (<= (mass point) 0)
      (setf old-mass (mass point)
            (mass point) (if (= 0 old-mass) 1 (abs old-mass)))
      (v:warn :verlet-point "MASS was not a positive number, converting it from ~a => ~a"
              old-mass (mass point)))))

(defmethod location ((point verlet-point))
  (if (pinned-p point) (pinned point) (call-next-method)))

(defmethod old-location ((point verlet-point))
  (if (pinned-p point) (pinned point) (slot-value point 'old-location)))

(defmethod (setf old-location) (value (point verlet-point))
  (setf (slot-value point 'old-location) value))

(defmethod pinned-p ((point verlet-point))
  (when (pinned point) T))

(defmethod pin ((point verlet-point) pin-to)
  ;; FIXME: There's got to be a better way to do this
  (let ((cur-loc (location point)))
    (setf (pinned point) (etypecase pin-to
                           (vec2 (etypecase cur-loc
                                   (vec2 pin-to)
                                   (vec3 (vec (vx pin-to) (vy pin-to)
                                              (vz cur-loc)))
                                   (vec4 (vec (vx pin-to) (vy pin-to)
                                              (vz cur-loc) (vw cur-loc)))))
                           (vec3 (etypecase cur-loc
                                   (vec2 (vec (vx pin-to) (vy pin-to)))
                                   (vec3 pin-to)
                                   (vec4 (vec (vx pin-to) (vy pin-to)
                                              (vz pin-to) (vw cur-loc)))))
                           (vec4 (etypecase cur-loc
                                   (vec2 (vec (vx pin-to) (vy pin-to)))
                                   (vec3 (vec (vx pin-to) (vy pin-to) (vz pin-to)))
                                   (vec4 pin-to)))))))

(defmethod apply-forces ((point verlet-point) forces)
  (unless (pinned-p point)
    (nv+ (acceleration point) forces)))

(defmethod simulate ((point verlet-point) delta &key (forces 0))
  (unless (pinned-p point)
    (let ((forces (etypecase forces
                    (number (v+ (v* (acceleration point) 0) forces))
                    (vec forces))))
      (apply-forces point (v* forces (mass point) delta)))
    (let* ((velocity (v* (v- (location point) (old-location point))
                         (dampen point)))
           (next-loc (v+ (location point) (v* velocity 1/2
                                              (acceleration point)
                                              (* delta delta)))))
      (setf (old-location point) (location point)
            (location point) next-loc
            (acceleration point) (v* (acceleration point) 0)))))

(defmethod solve-links ((point verlet-point))
  (for:for ((link in (links point)))
    (resolve link)))

(defmethod add-link ((point verlet-point) (link verlet-link))
  (let ((other (id (point-b link))))
    (unless (for:for ((point-link in (links point)))
              (thereis (eql (id (point-b point-link)) other)))
      (setf (point-a link) point)
      (push link (links point))
      link)))

(defmethod link ((point verlet-point) (other verlet-point) &key tear stiffness)
  (add-link point (make-instance 'verlet-link :tear tear
                                              :stiffness stiffness
                                              :point-a point
                                              :point-b other)))

(defmethod remove-link ((point verlet-point) (link verlet-link))
  (let ((other (id (point-b link))))
    (setf (links point)
          (for:for ((point-link in (links point))
                    (new-links unless (eql (id (point-b point-link))
                                           other)
                               collecting point-link))
            (returning new-links)))))

(defmethod 2d-constraint ((point located-entity))
  (let ((original-loc (location point)))
    #'(lambda ()
        (let ((current-loc (location point)))
          (setf (location point) (etypecase original-loc
                                   (vec2 current-loc)
                                   (vec3 (vec (vx current-loc)
                                              (vy current-loc)
                                              (vz original-loc)))
                                   (vec4 (vec (vx current-loc)
                                              (vy current-loc)
                                              (vz original-loc)
                                              (vw original-loc)))))))))

(defmethod fixed-location-constraint ((point located-entity))
  (let ((original-loc (location point)))
    (setf (fixed point) T)
    #'(lambda ()
        (when (v/= (location point) original-loc)
          (setf (location point) original-loc)))))

(defclass verlet-link ()
  ((target :initform NIL :accessor target)
   (tear :initform NIL :accessor tear)
   (stiffness :initarg :stiffness :accessor stiffness)
   (point-a :initarg :point-a :accessor point-a)
   (point-b :initarg :point-b :accessor point-b))
  (:default-initargs :stiffness 1
                     :point-a (error "POINT-A required")
                     :point-b (error "POINT-B required")))

(defmethod initialize-instance :after ((link verlet-link) &key tear)
  (setf (target link) (vlength (v- (location (point-b link))
                                   (location (point-a link))))
        (tear link) (when tear (+ (target link) tear))))

(defmethod resolve ((link verlet-link))
  (let ((point (point-a link))
        (other (point-b link)))
    (unless (and (pinned-p point) (pinned-p other))
      (let* ((stiffness (stiffness link))
             (target (target link))
             (point-mass (/ (mass point)))
             (other-mass (/ (mass other)))
             ;; Masses scale which point are moved more towards
             (point-scalar (* (/ point-mass (+ point-mass other-mass)) stiffness))
             (other-scalar (- stiffness point-scalar))
             (point-loc (location point))
             (other-loc (location other))
             (diff-loc (v- point-loc other-loc))
             (delta (vlength diff-loc))
             (difference (/ (- target delta) delta))
             (translate (v* diff-loc difference)))
        (when (and (tear link) (< (tear link) delta))
          (remove-link point-a link))
        (setf (location point) (v+ point-loc (v* translate point-scalar))
              (location other) (v- other-loc (v* translate other-scalar)))))))

(define-shader-entity verlet-entity (physical-entity vertex-entity)
  ((mass-points :initform NIL :accessor mass-points)
   (edges :initform NIL :accessor edges)
   (center :initform NIL :accessor center)
   (viscosity :initarg :viscosity :accessor viscosity))
  (:default-initargs :viscosity *default-viscosity*))

(defmethod initialize-instance :after ((entity verlet-entity) &key points edges vertex-array location)
  "Unless points are defined the mass-points of the vertex-array in vertex-entity class are used."
  (let* ((point-array (or points vertex-array))
         (mass-points (etypecase point-array
                        (mesh (mass-points (first (inputs vertex-array))))
                        (vertex-mesh (mass-points point-array))
                        (array point-array)
                        (list point-array))))
    (unless (< 3 (length mass-points))
      (error "POINT-ARRAY or VERTEX-ARRAY required"))
    (let ((mass-points (quick-hull (for:for ((vertex over mass-points)
                                             (v collecting (v+ location
                                                               (if (typep vertex 'textured-vertex)
                                                                   (location vertex)
                                                                   vertex))))))))
      (setf (mass-points entity) (mapcar #'(lambda (v)
                                             (make-instance 'verlet-point :location v))
                                         mass-points)))
    (setf (center entity) (calculate-center entity)
          (edges entity) (or edges (loop for mass-points = (mass-points entity) then (rest mass-points)
                                         while mass-points
                                         for current = (first mass-points)
                                         for next = (or (second mass-points)
                                                        (first (mass-points entity)))
                                         for link = (link current next)
                                         when link collect link)))))

(defmethod simulate-step ((entity verlet-entity) delta &key forces)
  (let ((forces (apply #'v+ (forces entity))))
    (for:for ((point in (mass-points entity)))
      (when forces
        (apply-forces point forces))
      (simulate point delta :forces (forces entity)))))

(defmethod simulate ((entity verlet-entity) delta &key forces)
  (let ((delta (/ delta *iterations*)))
    (dotimes (i *iterations*)
      (simulate-step entity delta :forces forces)))
  (setf (location entity) (calculate-center entity)))

(defmethod calculate-center ((entity verlet-entity))
  "Calculates the average of the points that form the entity's bounding box."
  (let ((center (vcopy (location (first (mass-points entity))))))
    (for:for ((point over (rest (mass-points entity)))
              (i count point))
      (nv+ center (location point))
      (returning (v/ center (1+ i))))))

(defmethod project-to-axis ((entity verlet-entity) axis)
  "Gets the nearest and furthest point along an axis." ;; Think of it like casting a shadow on a wall.
  (let ((min) (max))
    (for:for ((point in (mass-points entity))
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
        (for:for ((point in (mass-points ent1))
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
