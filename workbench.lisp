(in-package #:trial)

(define-pool workbench
  :base 'trial)

(define-asset (workbench skybox) image
    (list #p"masko-naive/posx.jpg"
          #p"masko-naive/negx.jpg"
          #p"masko-naive/posy.jpg"
          #p"masko-naive/negy.jpg"
          #p"masko-naive/posz.jpg"
          #p"masko-naive/negz.jpg")
  :target :texture-cube-map
  :min-filter :linear)

(defmethod update :after ((main main) tt dt)
  (let ((clipmap (unit :clipmap (scene main)))
        (camera (unit :camera (scene main))))
    (when clipmap
      (let ((loc (case 3
                   (0 (vec 0 0))
                   (1 (vec 0 (- (- (mod (* 2 tt) 2048) 1024))))
                   (2 (vec (* 512 (sin (* 0.5 tt))) (* 512 (cos (* 0.5 tt)))))
                   (3 (vec (vx (location camera)) (vz (location camera)))))))
        (setf (vx (location clipmap)) (vx loc)
              (vz (location clipmap)) (vy loc))))))

(progn
  (defmethod setup-scene ((main main) scene)
    (enter (make-instance 'skybox :texture (asset 'workbench 'skybox)) scene)
    (enter (make-instance 'geometry-clipmap :name :clipmap
                                            :map-scale (vec 2048 (* 12 2048) 2048)
                                            :resolution 1024
                                            :data-directory #p"~/mountain/") scene)
    (enter (make-instance 'editor-camera :name :camera
                                         :move-speed 0.1
                                         :location (vec 0 (* 4 2048) 0)) scene)
    (enter (make-instance 'render-pass) scene))

  (maybe-reload-scene))
