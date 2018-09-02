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
    (setf (vx (location clipmap)) (vx (location camera))
          (vz (location clipmap)) (vz (location camera)))
    ;; (setf (vy (location camera)) (+ 10 (current-height clipmap)))
    ))

(progn
  (defmethod setup-scene ((main main) scene)
    (setf (clear-color main) (vec 112/255 175/255 224/255))
    ;; (enter (make-instance 'skybox :texture (asset 'workbench 'skybox)) scene)
    (enter (make-instance 'geometry-clipmap :name :clipmap
                                            :map-scale (vec 1024 2048 1024)
                                            :maps '("height" "splat")
                                            :data-directory #p"~/simple-mountain/") scene)
    (enter (make-instance 'editor-camera :name :camera
                                         :move-speed 0.1
                                         :location (vec -2048 2048 -2048)) scene)
    (enter (make-instance 'render-pass) scene))
  (maybe-reload-scene))
