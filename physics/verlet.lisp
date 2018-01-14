#|
This file is a part of trial
(c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:org.shirakumo.fraf.trial.physics)

(defvar *default-viscosity* 1.0
  "How hard is it to move horizontally when falling on a surface.")
(defvar *iterations* 5
  "Number of physics calculation iterations. Increases accuracy of calculations.
In general, 4 is minimum for an alright accuracy, 8 is enough for a good accuracy.")

(define-shader-entity verlet-point (located-entity) ())
(defclass verlet-link () ())
(define-shader-entity verlet-entity (physical-entity vertex-entity) ())

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
                                   (vec3 (vec3 (vx pin-to) (vy pin-to)
                                               (vz cur-loc)))
                                   (vec4 (vec4 (vx pin-to) (vy pin-to)
                                               (vz cur-loc) (vw cur-loc)))))
                           (vec3 (etypecase cur-loc
                                   (vec2 (vec2 (vx pin-to) (vy pin-to)))
                                   (vec3 pin-to)
                                   (vec4 (vec4 (vx pin-to) (vy pin-to)
                                               (vz pin-to) (vw cur-loc)))))
                           (vec4 (etypecase cur-loc
                                   (vec2 (vec2 (vx pin-to) (vy pin-to)))
                                   (vec3 (vec3 (vx pin-to) (vy pin-to) (vz pin-to)))
                                   (vec4 pin-to)))))))

(defmethod apply-forces ((point verlet-point) forces)
  (unless (pinned-p point)
    (nv+ (acceleration point) forces)))

(defmethod simulate ((point verlet-point) delta &key forces)
  (unless (pinned-p point)
    (let ((forces (typecase forces
                    (number (v+ (v* (acceleration point) 0) forces))
                    (vec forces)
                    (T (v* (acceleration point) 0)))))
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
                                              :stiffness (or stiffness 1)
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
                                   (vec3 (vec3 (vx current-loc)
                                               (vy current-loc)
                                               (vz original-loc)))
                                   (vec4 (vec4 (vx current-loc)
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
          (remove-link point link))
        (setf (location point) (v+ point-loc (v* translate point-scalar))
              (location other) (v- other-loc (v* translate other-scalar)))))))

(define-shader-entity verlet-entity (physical-entity vertex-entity)
  ((mass-points :initform NIL :accessor mass-points)
   (edges :initform NIL :accessor edges)
   (viscosity :initarg :viscosity :accessor viscosity)
   (center :initform NIL :accessor center))
  (:default-initargs :viscosity *default-viscosity*))

(defmethod initialize-instance :after ((entity verlet-entity) &key points edges vertex-array location)
  "Unless points are defined the mass-points of the vertex-array in vertex-entity class are used."
  (let* ((point-array (or points vertex-array))
         (mass-points (etypecase point-array
                        (mesh (vertices (first (inputs vertex-array))))
                        (vertex-mesh (mass-points point-array))
                        ((or list array) point-array))))
    (unless (< 3 (length mass-points))
      (error "POINT-ARRAY or VERTEX-ARRAY required"))
    (let ((mass-points (quick-hull (for:for ((point over mass-points)
                                             (v collecting (v+ location
                                                               (if (typep point 'textured-vertex)
                                                                   (location point)
                                                                   point))))))))
      (setf (mass-points entity) (mapcar #'(lambda (v)
                                             (make-instance 'verlet-point :location v))
                                         mass-points)))
    (setf (edges entity) (or edges (loop for mass-points = (mass-points entity) then (rest mass-points)
                                         while mass-points
                                         for current = (first mass-points)
                                         for next = (or (second mass-points)
                                                        (first (mass-points entity)))
                                         for link = (link current next)
                                         when link collect link)))
    (let* ((points (mass-points entity))
           (center (calculate-center entity))
           (point-a (location (first points)))
           (point-b (location (second points)))
           (point-c (location (third points))))
      (setf (center entity) (list point-a (vlength (v- center point-a))
                                  point-b (vlength (v- center point-b))
                                  point-c (vlength (v- center point-c)))))))

(defmethod location ((entity verlet-entity))
  (triangulate-center entity))

(defmethod (setf location) (value (entity verlet-entity))
  (unless (typep value 'vec) (error "VALUE not VEC"))
  (let ((diff (v- value (triangulate-center entity))))
    (for:for ((point in (mass-points entity)))
      (setf (location point) (v+ (location point) diff))))
  value)

(defmethod simulate-step ((entity verlet-entity) delta &key forces)
  (let ((static-forces (when (static-forces entity)
                         (apply #'v+ (static-forces entity)))))
    (for:for ((point in (mass-points entity)))
      (when static-forces
        (apply-forces point static-forces))
      (simulate point delta :forces forces)
      (solve-links point))))

(defmethod simulate ((entity verlet-entity) delta &key forces)
  (let ((delta (/ delta *iterations*)))
    (dotimes (i *iterations*)
      (simulate-step entity delta :forces forces))))

(defmethod triangulate-center ((entity verlet-entity))
  ;; TODO: is this even potentially faster than CALCULATE-CENTER?
  (let ((point-a (first (center entity)))
        (center (apply #'triangulate (center entity))))
    (etypecase point-a
      (vec2 center)
      (vec3 (vec3 (vx center) (vy center) (vz point-a)))
      (vec4 (vec4 (vx center) (vy center) (vz point-a) (vw point-a))))))

(defmethod calculate-center ((entity verlet-entity))
  "Calculates the average of the points that form the entity's bounding box."
  (let ((center (vcopy (location (first (mass-points entity))))))
    (for:for ((point in (rest (mass-points entity)))
              (i count point))
      (nv+ center (location point))
      (returning (v/ center (1+ i))))))

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
