(in-package #:trial)

(defclass workbench (main) ()
  (:default-initargs :clear-color (vec 0 0 0 0)))

(define-pool workbench
  :base 'trial)

(define-asset (workbench coffee) image
    #p"/home/linus/models/ark_coffee/ark_coffee.png"
  :internal-format :srgb)

(define-asset (workbench coffee-spec) image
    #p"/home/linus/models/ark_coffee/ark_coffee_specular.png")

(define-asset (workbench white) image
    #p"/home/linus/models/ark_coffee/ark_cup.png")

(define-asset (workbench gray) image
    #p"/home/linus/models/ark_coffee/ark_spoon.png")

(define-asset (workbench black) image
    #p"/home/linus/models/ark_coffee/ark_black.png")

(define-asset (workbench wood) image
    #p"wood.jpg"
  :internal-format :srgb)

(define-asset (workbench cappuccino) mesh
    #p"/home/linus/models/ark_coffee/ARK_COFFEE_CUP.obj"
  :geometry-name "cappuccino")

(define-asset (workbench cup) mesh
    #p"/home/linus/models/ark_coffee/ARK_COFFEE_CUP.obj"
  :geometry-name "cup")

(define-asset (workbench plate) mesh
    #p"/home/linus/models/ark_coffee/ARK_COFFEE_CUP.obj"
  :geometry-name "plate")

(define-asset (workbench spoon) mesh
    #p"/home/linus/models/ark_coffee/ARK_COFFEE_CUP.obj"
  :geometry-name "spoon")

(define-asset (workbench floor) mesh
    (make-cube '(30 30 1)))

(define-shader-pass deferred+shadow-pass (high-color-pass
                                          hdr-output-pass
                                          deferred-render-pass
                                          shadow-render-pass)
  ())

(define-class-shader (deferred+shadow-pass :fragment-shader)
  "out vec4 color;
void main(){
  vec3 position = texture(position_map, tex_coord).rgb;
  vec3 normal = texture(normal_map, tex_coord).rgb;
  vec3 light_direction = light_block.lights[1].position-position;
  float bias = shadow_bias(normal, light_direction);
  float shadow = shadow_factor(position, bias);
  color *= (1-min(0.2,shadow));
}")

(progn
  (defmethod setup-scene ((workbench workbench) scene)
    (enter (make-instance 'editor-camera :location (VEC3 -4.5441413 12.507906 8.965087)
                                         :rotation (VEC3 0.8199995 0.5300003 0.0)
                                         :move-speed 0.1) scene)
    (flet ((add (diff spec vert &rest initargs)
             (enter (apply #'make-instance 'geometry-shaded
                           :specular-map (asset 'workbench spec)
                           :diffuse-map (asset 'workbench diff)
                           :vertex-array (asset 'workbench vert)
                           initargs)
                    scene)))
      (add 'white 'gray 'cup)
      (add 'white 'gray 'plate)
      (add 'gray 'white 'spoon)
      (add 'coffee 'black 'cappuccino)
      (add 'wood 'wood 'floor))
    (let* ((shadow (make-instance 'shadow-map-pass :projection-matrix (mortho -20 20 -20 20 1.0 50)
                                                   :view-matrix (mlookat (vec -20 20 -20) (vec 0 0 0) (vec 0 1 0))
                                                   :name :shadow-map-pass))
           (geometry (make-instance 'geometry-pass))
           (lighting (make-instance 'deferred+shadow-pass :shadow-map-pass shadow))
           (h-blur (make-instance 'gaussian-blur-pass :uniforms `(("dir" ,(vec 1 0)))))
           (v-blur (make-instance 'gaussian-blur-pass :uniforms `(("dir" ,(vec 0 1)))))
           (h-blur2 (make-instance 'gaussian-blur-pass :uniforms `(("dir" ,(vec 1 0)))))
           (v-blur2 (make-instance 'gaussian-blur-pass :uniforms `(("dir" ,(vec 0 1)))))
           (tone-map (make-instance 'bloom-pass)))
      (connect (port geometry 'position) (port lighting 'position-map) scene)
      (connect (port geometry 'normal) (port lighting 'normal-map) scene)
      (connect (port geometry 'albedo) (port lighting 'albedo-map) scene)
      (connect (port shadow 'shadow) (port lighting 'shadow-map) scene)
      (connect (port lighting 'high-pass) (port h-blur 'previous-pass) scene)
      (connect (port h-blur 'color) (port v-blur 'previous-pass) scene)
      (connect (port v-blur 'color) (port h-blur2 'previous-pass) scene)
      (connect (port h-blur2 'color) (port v-blur2 'previous-pass) scene)
      (connect (port v-blur2 'color) (port tone-map 'high-pass) scene)
      (connect (port lighting 'color) (port tone-map 'previous-pass) scene)))
  
  (defmethod change-scene :after ((workbench workbench) scene &key old)
    (let ((buffer (asset 'trial 'light-block)))
      (setf (buffer-field buffer "LightBlock.count") 1)
      (setf (buffer-field buffer "LightBlock.lights[0].type") 2)
      (setf (buffer-field buffer "LightBlock.lights[0].position") (vec -20 20 -20))
      (setf (buffer-field buffer "LightBlock.lights[0].color") (vec 10 10 10))))
  (maybe-reload-scene))

(defmethod update :after ((workbench workbench) tt dt)
  (let ((buffer (asset 'trial 'light-block))
        (shadow (unit :shadow-map-pass (scene workbench)))
        (camera (unit :camera (scene workbench)))
        (tt (/ tt 2))
        (light (vec (* (sin tt) 25) 10 (* (cos tt) 25))))
    ;;(setf (buffer-field buffer "LightBlock.lights[1].color") (vec 15 15 15))
    ;;(setf (buffer-field buffer "LightBlock.lights[1].position") light)
    ;;(setf (shadow-view-matrix shadow) (mlookat light (vec 0 0 0) (vec 0 1 0)))
    ;;(vsetf (location camera) (* (sin (/ tt -1)) 20) 20 (* (cos (/ tt 1)) 20))
    ))
