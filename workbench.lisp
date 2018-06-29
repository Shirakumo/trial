(in-package #:trial)

(define-pool workbench
  :base 'trial)

(define-asset (workbench cube) mesh
    (make-cube 20))

(define-asset (workbench cat) image
    #p"cat.png")

(define-shader-entity tester (vertex-entity textured-entity)
  ())

(defmethod update :after ((main main) tt dt)
  (let ((clipmap (unit :clipmap (scene main))))
    (when clipmap
      (case 0
        (0 (setf (location clipmap) (load-time-value (vec 0 0 0))))
        (1 (maybe-show-region clipmap (- (mod (* 100 tt) 1024) 512) 0))
        (2 (maybe-show-region clipmap (* 64 (sin (* 2 tt))) (* 64 (cos (* 2 tt)))))))))

(progn
  (defmethod setup-scene ((main main) scene)
    (enter (make-instance 'tester :vertex-array (asset 'workbench 'cube) :texture (asset 'workbench 'cat)) scene)
    (enter (make-instance 'target-camera :location (vec 50 50 0)) scene)
    (enter (make-instance 'render-pass) scene))

  (maybe-reload-scene))
