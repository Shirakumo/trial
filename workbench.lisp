(in-package #:trial)

(define-pool workbench
  :base 'trial)

(define-asset (workbench box) mesh
    (make-cube '(10 10 10)))

(define-subject box-collection ()
  ((boxes :initform NIL :accessor boxes))
  (:default-initargs :name :box-collection))

(define-shader-subject box (colored-entity trial-physics:collidable-verlet-entity) ()
  (:default-initargs
   :vertex-array (asset 'workbench 'box)))

(define-handler (box-collection tick) (ev)
  (trial-physics:verlet-simulation (boxes box-collection)))

(progn
  (defmethod setup-scene ((main main) scene)
    (setf (clear-color main) (vec 0.11 0.1 0.1 1.0))
    (let ((box-collection (make-instance 'box-collection))
          (min-loc (vec 5 5 10))
          (max-loc (vec (- (width main) 5) (- (height main) 5) 10)))
      (for:for ((i repeat 20)
                (box = (make-instance 'box :location (vec (floor (+ (vx min-loc)
                                                                    (random (- (vx max-loc)
                                                                               (vx min-loc)))))
                                                          (floor (+ (vy min-loc)
                                                                    (random (- (vy max-loc)
                                                                               (vy min-loc)))))
                                                          10)
                                           :static-forces (vec 0.0 -0.004 0.0))))
        (setf (color box)
              (case (mod i 6)
                (1 (vec 0 1 0 1)) ;; Green
                (2 (vec 1 0 1 1)) ;; Magenta
                (3 (vec 0 1 1 1)) ;; Cyan
                (4 (vec 1 1 0 1)) ;; Yellow
                (5 (vec 0 0 1 1)) ;; Blue
                (0 (vec 1 0 0 1)))) ;; Red
        (trial-physics:constrain-to-frame box min-loc max-loc)
        (push box (boxes box-collection))
        (enter box scene))
      (enter box-collection scene))
    (enter (make-instance '2d-camera) scene)
    (enter (make-instance 'render-pass) scene))

  (maybe-reload-scene))
