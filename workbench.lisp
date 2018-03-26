(in-package #:trial)

(define-pool workbench
  :base 'trial)

(define-asset (workbench box) mesh
    (make-cube '(10 10 2)))

(define-subject box-collection ()
  ((boxes :initform NIL :accessor boxes))
  (:default-initargs :name :box-collection))

(define-shader-subject box (colored-entity trial-physics:verlet-entity) ()
  (:default-initargs
   :vertex-array (asset 'workbench 'box)))

(define-handler (box-collection tick) (ev)
  (trial-physics:verlet-simulation (boxes box-collection) (dt ev)))

(progn
  (defmethod setup-scene ((main main) scene)
    (setf (clear-color main) (vec 0.11 0.1 0.1 1.0))
    (let ((box-collection (make-instance 'box-collection)))
      (for:for ((i repeat 100)
                (box = (make-instance 'box :location (v+ (vec 0 0 10)
                                                         (v* (vec 1 1 0)
                                                             (vec3-random 5 (- (min (width main)
                                                                                    (height main)) 
                                                                               10))))
                                           :static-forces (vec 0 -10 0)
                                           :constrain `(:frame :min ,(vec 0 0 10)
                                                               :max ,(vec (width main)
                                                                          (height main)
                                                                          10)))))
        (setf (color box)
              (case (mod i 7)
                (1 (vec 0 1 0 1)) ;; Green
                (2 (vec 1 0 1 1)) ;; Magenta
                (3 (vec 0 1 1 1)) ;; Cyan
                (4 (vec 1 1 0 1)) ;; Yellow
                (5 (vec 1 1 1 1)) ;; White
                (6 (vec 0 0 1 1)) ;; Blue
                (0 (vec 1 0 0 1)))) ;; Red
        (push box (boxes box-collection))
        (enter box scene))
      (enter box-collection scene))
    (enter (make-instance '2d-camera) scene)
    (enter (make-instance 'render-pass) scene))

  (maybe-reload-scene))
