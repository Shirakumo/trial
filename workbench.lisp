(in-package #:trial)

(defclass workbench (main) ()
  (:default-initargs :clear-color (vec 0 0 0 0)))

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

;; Wood house
(define-asset (workbench wood-house) mesh
    #p"/home/linus/models/wood_house/hatka_local_.obj"
  :geometry-name "fence")

(define-asset (workbench wood-house-ambient) image
    #p"/home/linus/models/wood_house/home_hatka_Base_Color.png"
  :internal-format :srgb)

(define-asset (workbench wood-house-specular) image
    #p"/home/linus/models/wood_house/home_hatka_Metallic.png")

(define-asset (workbench wood-ground) mesh
    #p"/home/linus/models/wood_house/hatka_local_.obj"
  :geometry-name "grount")

(define-asset (workbench wood-ground-ambient) image
    #p"/home/linus/models/wood_house/grunt_Base_Color.png"
  :internal-format :srgb)

(define-asset (workbench wood-ground-specular) image
    #p"/home/linus/models/wood_house/grunt_Metallic.png")

;; Extra
(define-asset (workbench sphere) mesh
    (make-sphere 1))

(define-asset (workbench big-sphere) mesh
    (make-sphere 1))

(defparameter *scene-size* 800)

(define-shader-subject point-light (geometry-shaded located-entity)
  ((index :initarg :index :initform 0 :accessor index)
   (color :initarg :color :initform (vec 0 0 0) :accessor color)
   (attenuation :initarg :attenuation :initform '(0.07 0.017) :accessor attenuation)
   (direction :initarg :direction :accessor direction))
  (:default-initargs
   :direction (vec3-random -3 +3)
   :diffuse-map (asset 'workbench 'white)
   :specular-map (asset 'workbench 'black)
   :vertex-array (asset 'workbench 'sphere)))

(define-handler (point-light tick) (ev dt tt)
  (let ((buffer (asset 'trial 'light-block))
        (i (index point-light)))
    (flet ((field (i field)
             (format NIL "LightBlock.lights[~d].~(~a~)" i field)))
      (setf (buffer-field buffer (field i 'type)) 2)
      (setf (buffer-field buffer (field i 'position)) (location point-light))
      (setf (buffer-field buffer (field i 'color)) (color point-light))
      (setf (buffer-field buffer (field i 'attenuation_linear)) (first (attenuation point-light)))
      (setf (buffer-field buffer (field i 'attenuation_quadratic)) (second (attenuation point-light))))
    (let ((dir (direction point-light))
          (loc (location point-light)))
      (setf (vx dir) (random (* 30 dt)))
      (setf (vy dir) (+ (vy dir) (random (* 2 dt))))
      (setf (vz dir) (+ (vz dir) (random (* 2 dt))))
      (flet ((wrap (x)
               (- (mod (+ x *scene-size*) (* 2 *scene-size*)) *scene-size*)))
        (vsetf loc
               (wrap (+ (vx loc) (* (vx dir) (sin (vy dir)) (cos (vz dir)))))
               (mod (+ (vy loc) (* (vx dir) (sin (vy dir)) (sin (vz dir)))) *scene-size*)
               (wrap (+ (vz loc) (* (vx dir) (cos (vy dir))))))))))

(defmethod paint ((point-light point-light) (pass shadow-map-pass)))

(define-shader-pass deferred+shadow-pass (high-color-pass
                                          hdr-output-pass
                                          deferred-render-pass
                                          shadow-render-pass)
  ())

(define-class-shader (deferred+shadow-pass :fragment-shader 5)
  (gl-source (asset 'trial 'light-block))
  "in vec2 tex_coord;

uniform sampler2D position_map;
uniform sampler2D normal_map;
uniform sampler2D albedo_map;

float lighting_strength = 1.0;
void main(){
  vec3 position = texture(position_map, tex_coord).rgb;
  vec3 normal = texture(normal_map, tex_coord).rgb;
  vec3 light_direction = light_block.lights[0].position-position;
  float bias = shadow_bias(normal, light_direction);
  float shadow = shadow_factor(position, bias);
  lighting_strength = 1-shadow;
}")

(progn
  (defmethod setup-scene ((workbench workbench) scene)
    (enter (make-instance 'editor-camera :location (VEC3 982.9328 651.59686 -957.8825)
                                         :rotation (VEC3 0.4599998 3.9599707 0.0))
           scene)
    (enter (make-instance 'skybox :texture (asset 'workbench 'skybox)) scene)
    (flet ((add (vert diff spec &rest initargs)
             (enter (apply #'make-instance 'geometry-shaded
                           :specular-map (asset 'workbench spec)
                           :diffuse-map (asset 'workbench diff)
                           :vertex-array (asset 'workbench vert)
                           initargs)
                    scene)))
      (add 'wood-ground 'wood-ground-ambient 'wood-ground-specular)
      (add 'wood-house 'wood-house-ambient 'wood-house-specular))
    (dotimes (i (1- MAX-LIGHTS))
      (enter (make-instance 'point-light :index (1+ i)
                                         :location (vec3-random (- *scene-size*) *scene-size*)
                                         :color (nv* (nvunit (vec3-random 1 10)) 10)
                                         :attenuation '(0.07 0.017))
             scene))
    (let* ((shadow (make-instance 'shadow-map-pass :projection-matrix (mortho -800 800 -800 800 1.0 1500)
                                                   :view-matrix (mlookat (vec 600 600 -600) (vec 0 0 0) (vec 0 1 0))
                                                   :name :shadow-map-pass))
           (geometry (make-instance 'geometry-pass))
           (lighting (make-instance 'deferred+shadow-pass :shadow-map-pass shadow))
           (h-blur (make-instance 'gaussian-blur-pass :uniforms `(("dir" ,(vec 1 0)))))
           (v-blur (make-instance 'gaussian-blur-pass :uniforms `(("dir" ,(vec 0 1)))))
           (h-blur2 (make-instance 'gaussian-blur-pass :uniforms `(("dir" ,(vec 1 0)))))
           (v-blur2 (make-instance 'gaussian-blur-pass :uniforms `(("dir" ,(vec 0 1)))))
           (tone-map (make-instance 'bloom-pass)))
      ;;(connect (port shadow 'shadow) (port (make-instance 'copy-pass) 'previous-pass) scene)
      (connect (port shadow 'shadow) (port lighting 'shadow-map) scene)
      (connect (port geometry 'position) (port lighting 'position-map) scene)
      (connect (port geometry 'normal) (port lighting 'normal-map) scene)
      (connect (port geometry 'albedo) (port lighting 'albedo-map) scene)
      (connect (port lighting 'high-pass) (port h-blur 'previous-pass) scene)
      (connect (port h-blur 'color) (port v-blur 'previous-pass) scene)
      (connect (port v-blur 'color) (port h-blur2 'previous-pass) scene)
      (connect (port h-blur2 'color) (port v-blur2 'previous-pass) scene)
      (connect (port v-blur2 'color) (port tone-map 'high-pass) scene)
      (connect (port lighting 'color) (port tone-map 'previous-pass) scene)))
  
  (defmethod change-scene :after ((workbench workbench) scene &key old)
    (declare (ignore old))
    (let ((buffer (asset 'trial 'light-block)))
      (flet ((field (i field)
               (format NIL "LightBlock.lights[~d].~(~a~)" i field)))
        (setf (buffer-field buffer (field 0 'type)) 1)
        (setf (buffer-field buffer (field 0 'direction)) (nvunit (nv- (vec 600 600 -600))))
        (setf (buffer-field buffer (field 0 'color)) (vec 1.2 1.1 1)))
      (setf (buffer-field buffer "LightBlock.count") MAX-LIGHTS)))
  (maybe-reload-scene))

(defmethod update :after ((workbench workbench) tt dt)
  (let ((buffer (asset 'trial 'light-block))
        (shadow (unit :shadow-map-pass (scene workbench)))
        (light (unit :sunlight (scene workbench)))
        (camera (unit :camera (scene workbench))))
    ;;(setf (buffer-field buffer "LightBlock.lights[1].color") (vec 15 15 15))
    ;;(setf (buffer-field buffer "LightBlock.lights[1].position") light)
    ;;(setf (shadow-view-matrix shadow) (mlookat (location light) (vec 0 0 0) (vec 0 1 0)))
    ;;(vsetf (location camera) (* (sin (/ tt -1)) 20) 20 (* (cos (/ tt 1)) 20))
    ))
