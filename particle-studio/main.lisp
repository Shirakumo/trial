(in-package #:org.shirakumo.fraf.trial.particle-studio)

(defclass main (trial:main)
  ()
  (:default-initargs :context '(:vsync T :version (3 3))))

(defmethod setup-scene ((main main) (scene scene))
  (preload (find-pool 'trial) scene)
  (enter (make-instance 'vertex-entity :vertex-array (// 'trial 'grid)) scene)
  (enter (make-instance 'editor-camera :location (vec3 0.0 2.3 7.3) :fov 50 :move-speed 0.1) scene)
  (enter (make-instance 'basic-node :name 'emitters) scene)
  (let ((render (make-instance 'phong-render-pass))
        (ui (make-instance 'ui))
        (combine (make-instance 'blend-pass)))
    (connect (port render 'color) (port combine 'a-pass) scene)
    (connect (port ui 'color) (port combine 'b-pass) scene)
    (setup-ui ui)))

(defun launch (&rest args)
  (apply #'trial:launch 'main args))

(defun main ()
  (command-line-toplevel)
  (launch))
