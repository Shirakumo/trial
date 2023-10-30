(in-package #:org.shirakumo.fraf.trial.examples)

(define-shader-entity gjk-body (vertex-entity colored-entity rigidbody listener)
  ())

(defmethod initialize-instance :after ((body gjk-body) &key (primitive (make-box)))
  (setf (physics-primitives body) (trial:coerce-object primitive 'trial:convex-mesh))
  (setf (vertex-array body) (trial::make-vertex-array (aref (physics-primitives body) 0) NIL)))

(defmethod render :around ((body gjk-body) (program shader-program))
  (gl:polygon-mode :front-and-back :line)
  (gl:disable :cull-face)
  (call-next-method)
  (gl:enable :cull-face)
  (gl:polygon-mode :front-and-back :fill))

(define-handler (gjk-body pre-tick) ()
  (start-frame gjk-body))

(define-handler (gjk-body tick) ()
  (setf (color gjk-body) #.(vec 1 1 1 1))
  (for:for ((entity over (scene +main+)))
    (when (and (not (eq entity gjk-body)) (typep entity 'gjk-body)
               (trial::intersects-p gjk-body entity))
      (setf (color gjk-body) #.(vec 1 0 0 1))
      (return))))

(define-shader-entity gjk-player (gjk-body)
  ())

(define-handler (gjk-player tick :after) (dt)
  (let ((spd 3.0))
    (cond ((retained :shift)
           (when (or (retained :left) (retained :a))
             (nq* (orientation gjk-player) (qfrom-angle +vy3+ (* +1.0 spd dt))))
           (when (or (retained :right) (retained :d))
             (nq* (orientation gjk-player) (qfrom-angle +vy3+ (* -1.0 spd dt))))
           (when (or (retained :up) (retained :w))
             (nq* (orientation gjk-player) (qfrom-angle +vx3+ (* +1.0 spd dt))))
           (when (or (retained :down) (retained :s))
             (nq* (orientation gjk-player) (qfrom-angle +vx3+ (* -1.0 spd dt)))))
          (T
           (when (or (retained :left) (retained :a))
             (incf (vx (location gjk-player)) (* +1.0 spd dt)))
           (when (or (retained :right) (retained :d))
             (incf (vx (location gjk-player)) (* -1.0 spd dt)))
           (when (or (retained :up) (retained :w))
             (incf (vy (location gjk-player)) (* +1.0 spd dt)))
           (when (or (retained :down) (retained :s))
             (incf (vy (location gjk-player)) (* -1.0 spd dt)))))))

(define-example gjk
  :title "GJK Collision Detection"
  (enter (make-instance '3d-camera :location (vec 0 0 -5)) scene)
  (enter (make-instance 'gjk-body :primitive (make-box)) scene)
  (enter (make-instance 'gjk-player :primitive (make-cylinder)) scene)
  (enter (make-instance 'render-pass) scene))
