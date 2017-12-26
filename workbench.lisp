(in-package #:trial)

(define-pool workbench
  :base 'trial)

(define-asset (workbench box) mesh
    ((make-rectangle 10 10)))

(defclass wb-main (main)
  ()
  (:default-initargs
   :clear-color (vec 0.1 0.1 0.1 1)))

(defmethod initialize-instance :after ((main wb-main) &key)
  (harmony-simple:initialize)
  (setf (harmony:min-distance (harmony-simple:segment :sfx)) 32))

(defmethod finalize :after ((main wb-main))
  (harmony-simple:stop))

(define-shader-subject box (colored-entity trial-verlet:verlet-entity)
  ()
  (:default-initargs
   :vertex-array (asset 'workbench 'box)
   :name :box))

(define-handler (box tick) (ev)
  (trial-physics:simulate box (dt ev)))

(progn
  (defmethod setup-scene ((main wb-main) scene)
    (for:for ((i repeat 100)
              (box = (make-instance 'box :location (v+ (vec 0 0 10)
                                                       (v* (vec 1 1 0)
                                                           (vec3-random 5 (- (min (width main)
                                                                                  (height main)) 
                                                                             10)))))))
      (setf (color box)
            (case (mod i 7)
              (1 (vec 0 1 0 1)) ;; Green
              (2 (vec 1 0 1 1)) ;; Magenta
              (3 (vec 0 1 1 1)) ;; Cyan
              (4 (vec 1 1 0 1)) ;; Yellow
              (5 (vec 1 1 1 1)) ;; White
              (6 (vec 0 0 1 1)) ;; Blue
              (0 (vec 1 0 0 1)))) ;; Red
      (enter box scene))
    (enter (make-instance '2d-camera) scene)
    (enter (make-instance 'render-pass) scene))

  (maybe-reload-scene))
