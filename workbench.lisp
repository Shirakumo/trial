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
      (let ((loc (case 3
                   (0 (vec 0 0))
                   (1 (vec 0 (- (- (mod (* 2 tt) 2048) 1024))))
                   (2 (vec (* 512 (sin (* 0.5 tt))) (* 512 (cos (* 0.5 tt)))))
                   (3 (vec (vx (location camera)) (vz (location camera)))))))
        (maybe-show-region clipmap (vx loc) (vy loc))))))

(progn
  (defmethod setup-scene ((main main) scene)
    (enter (make-instance 'geometry-clipmap :name :clipmap
                                            :map-scale (vec 2048 (* 12 2048) 2048)
                                            :data-directory #p"~/clipmaps/") scene)
    (enter (make-instance 'player :name :player) scene)
    (enter (make-instance 'editor-camera :name :camera
                                         :location (vec 0 (* 8 2048) 0)) scene)
    (enter (make-instance 'render-pass) scene))

  (maybe-reload-scene))
