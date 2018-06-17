(in-package #:trial)

(define-pool workbench
  :base 'trial)

(define-asset (workbench cube) mesh
    (make-cube 200))

(define-asset (workbench cat) image
    #p"cat.png")

(define-shader-entity tester (vertex-entity textured-entity)
  ()
  (:default-initargs
   :vertex-array (asset 'workbench 'cube))
  (:inhibit-shaders (textured-entity :fragment-shader)))

(define-class-shader (tester :fragment-shader)
  "in vec2 texcoord;
out vec4 color;
uniform sampler2D texture_image;

void main(){
  color = texture(texture_image, texcoord);
}")

(defmethod update :after ((main main) tt dt)
  (let ((clipmap (unit :clipmap (scene main))))
    (when clipmap
      ;(show-region clipmap -512 -512)
      ;(show-region clipmap (- (mod (* 100 tt) 1024) 1024) -512)
      (show-region clipmap (+ -512 (* 64 (sin (* 2 tt)))) (+ -512 (* 64 (cos (* 2 tt)))))
      )))

(progn
  (defmethod setup-scene ((main main) scene)
    (cond ((= 0 1)
           (enter (make-instance '2d-camera) scene)
           (enter (make-instance 'geometry-clipmap-updater) scene))
          (T
           (enter (make-instance 'editor-camera :move-speed 0.5
                                                :rotation (VEC3 0.63999623 1.4305115e-6 0.0)
                                                :location (VEC3 -0.79987687 497.58453 775.5049)) scene)
           (let ((clipmap (make-instance 'geometry-clipmap :data-directory #p"~/clipmaps/" :name :clipmap)))
             (enter clipmap scene)
             ;;(enter (make-instance 'tester :texture (texture-buffer clipmap)) scene)
             )))
    (enter (make-instance 'render-pass) scene))

  (maybe-reload-scene))
