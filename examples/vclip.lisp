(in-package #:org.shirakumo.fraf.trial.examples)

(define-shader-entity vclip-body (vertex-entity colored-entity rigidbody listener)
  ())

(defmethod initialize-instance :after ((body vclip-body) &key)
  (setf (physics-primitives body) (coerce-object (make-box) 'org.shirakumo.fraf.trial.v-clip:mesh))
  (setf (vertex-array body) (trial::make-vertex-array (aref (physics-primitives body) 0) NIL)))

(defmethod render :around ((body vclip-body) (program shader-program))
  (gl:polygon-mode :front-and-back :line)
  (gl:disable :cull-face)
  (call-next-method)
  (gl:enable :cull-face)
  (gl:polygon-mode :front-and-back :fill))

(define-handler (vclip-body pre-tick) ()
  (start-frame vclip-body))

(define-handler (vclip-body tick) ()
  (setf (color vclip-body) #.(vec 1 1 1 1))
  (for:for ((entity over (scene +main+)))
    (when (and (not (eq entity vclip-body)) (typep entity 'vclip-body)
               (trial::intersects-p vclip-body entity))
      (setf (color vclip-body) #.(vec 1 0 0 1))
      (return))))

(define-shader-entity vclip-player (vclip-body)
  ())

(define-handler (vclip-player tick :after) (dt)
  (let ((spd 3.0))
    (cond ((retained :shift)
           (when (retained :a)
             (nq* (orientation vclip-player) (qfrom-angle +vy3+ (* +1.0 spd dt))))
           (when (retained :d)
             (nq* (orientation vclip-player) (qfrom-angle +vy3+ (* -1.0 spd dt))))
           (when (retained :w)
             (nq* (orientation vclip-player) (qfrom-angle +vx3+ (* +1.0 spd dt))))
           (when (retained :s)
             (nq* (orientation vclip-player) (qfrom-angle +vx3+ (* -1.0 spd dt)))))
          (T
           (when (retained :a)
             (incf (vx (location vclip-player)) (* +1.0 spd dt)))
           (when (retained :d)
             (incf (vx (location vclip-player)) (* -1.0 spd dt)))
           (when (retained :w)
             (incf (vy (location vclip-player)) (* +1.0 spd dt)))
           (when (retained :s)
             (incf (vy (location vclip-player)) (* -1.0 spd dt)))))))

(define-example vclip
  :title "V-Clip Collision Detection"
  (enter (make-instance '3d-camera :location (vec 0 0 -5)) scene)
  (enter (make-instance 'vclip-body) scene)
  (enter (make-instance 'vclip-player) scene)
  (enter (make-instance 'render-pass) scene))
