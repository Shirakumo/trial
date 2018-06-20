(in-package #:trial)

(define-pool workbench
  :base 'trial)

(define-asset (workbench cube) mesh
    (make-cube 200))

(define-asset (workbench cat) image
    #p"~/clipmaps/4096/0,0.png"
  :min-filter :linear)

(define-shader-entity tester (vertex-entity textured-entity)
  ()
  (:default-initargs
   :name :tester
   :vertex-array (asset 'workbench 'cube)
   :texture (asset 'workbench 'cat))
  (:inhibit-shaders (textured-entity :fragment-shader)))

(defmethod update :after ((main main) tt dt)
  (let ((clipmap (unit :clipmap (scene main)))
        (tester (unit :tester (scene main))))
    (when clipmap
      (case 0
        (0 (setf (location clipmap) (load-time-value (vec 0 0 0))))
        (1 (maybe-show-region clipmap (- (mod (* 100 tt) 1024) 512) 0))
        (2 (maybe-show-region clipmap (* 64 (sin (* 2 tt))) (* 64 (cos (* 2 tt)))))))))

(define-class-shader (tester :fragment-shader)
  "in vec2 texcoord;
out vec4 color;
uniform sampler2D texture_image;

void main(){
  color = vec4(texture(texture_image, texcoord).r, 0, 0, 1);
}")

(progn
  (defmethod setup-scene ((main main) scene)
    (cond ((= 1 0)
           (enter (make-instance 'tester :texture (make-instance 'texture :pixel-format :red
                                                                          :min-filter :linear
                                                                          :pixel-type (infer-pixel-type 16)
                                                                          :internal-format (infer-internal-format 16 :red)
                                                                          :width (* 2 512)
                                                                          :height (* 2 512))) scene)
           (enter (make-instance 'target-camera :location (vec 400 400 0)) scene))
          (T
           (enter (make-instance 'editor-camera :move-speed 0.5
                                                :rotation (VEC3 0.63999623 1.4305115e-6 0.0)
                                                :location (VEC3 -0.79987687 497.58453 775.5049)) scene)
           (let ((clipmap (make-instance 'geometry-clipmap
                                         :data-directory #p"~/clipmaps/"
                                         :levels 5
                                         :name :clipmap)))
             (enter clipmap scene)
             ;(enter (make-instance 'tester :texture (texture-buffer clipmap)) scene)
             )))
    (enter (make-instance 'render-pass) scene))

  (maybe-reload-scene))
