(in-package #:trial)

(define-pool workbench
  :base 'trial)

(define-asset (workbench cube) mesh
    (make-cube 10))

(define-asset (workbench cat) image
    #p"cat.png")

(define-shader-subject player (vertex-entity textured-entity located-entity)
  ()
  (:default-initargs
   :vertex-array (asset 'workbench 'cube)
   :texture (asset 'workbench 'cat)))

(define-handler (player mouse-move) (ev pos)
  ;; (let ((world (screen->vec pos (width *context*) (height *context*))))
  ;;   (setf (vx (location player)) (* 30000 (vx world)))
  ;;   (setf (vz (location player)) (vy world)))
  )

(defmethod update :after ((main main) tt dt)
  (let ((clipmap (unit :clipmap (scene main)))
        (camera (unit :camera (scene main))))
    (when clipmap
      (let ((loc (case 2
                   (0 (vec 0 0))
                   (1 (vec (- (mod (* 10 tt) 100) 512) (- (mod (* 20 tt) 100) 512)))
                   (2 (vec (* 512 (sin (* 0.5 tt))) (* 512 (cos (* 0.5 tt)))))
                   (3 (vec (vx (location player)) (vz (location player)))))))
        (maybe-show-region clipmap (vx loc) (vy loc))
        ;(setf (vx (location camera)) (vx loc))
        ;(setf (vz (location camera)) (vy loc))
        ))))

(progn
  (defmethod setup-scene ((main main) scene)
    (enter (make-instance 'geometry-clipmap :name :clipmap
                                            :data-directory #p"~/clipmaps/") scene)
    (enter (make-instance 'player :name :player) scene)
    (enter (make-instance 'target-camera :name :camera
                                         :location (vec 0 1000 0.0001)) scene)
    (enter (make-instance 'render-pass) scene))

  (maybe-reload-scene))
