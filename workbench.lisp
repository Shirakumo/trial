(in-package #:trial)

(defclass workbench (main) ())

(define-pool workbench
  :base 'trial)

(define-asset (workbench box) mesh
    (make-cube 10))

(define-asset (workbench floor) mesh
    (make-cube '(20 20 10) :y -10))

(define-asset (workbench box-diffuse) image
    #p"box-diffuse.png"
  :internal-format :srgb)

(define-asset (workbench box-specular) image
    #p"box-specular.png")

(define-asset (workbench brickwall-diffuse) image
    #p"brickwall-diffuse.jpg"
  :internal-format :srgb)

(define-asset (workbench brickwall-specular) image
    #p"brickwall-specular.jpg")

(define-shader-subject test-entity (geometry-shaded rotated-entity)
  ())

(define-handler (test-entity tick) (ev dt tt)
  ;(incf (vx (rotation test-entity)) dt)
  ;(incf (vz (rotation test-entity)) dt)
  )

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
  color *= (1-min(0.75,shadow));
}")

(progn
  (defmethod setup-scene ((workbench workbench) scene)
    (enter (make-instance 'target-camera :location (vec 0 20 20)) scene)
    (enter (make-instance 'test-entity :specular-map (asset 'workbench 'box-specular)
                                       :diffuse-map (asset 'workbench 'box-diffuse)
                                       :vertex-array (asset 'workbench 'box))
           scene)
    (enter (make-instance 'geometry-shaded :specular-map (asset 'workbench 'brickwall-specular)
                                           :diffuse-map (asset 'workbench 'brickwall-diffuse)
                                           :vertex-array (asset 'workbench 'floor))
           scene)
    (let* ((shadow (make-instance 'shadow-map-pass :projection-matrix (mortho -20 20 -20 20 1.0 50)
                                                   :view-matrix (mlookat (vec 20 10 5) (vec 0 0 0) (vec 0 1 0))
                                  :name :shadow-map-pass))
           (geometry (make-instance 'geometry-pass))
           (lighting (make-instance 'deferred+shadow-pass :shadow-map-pass shadow))
           (h-blur (make-instance 'gaussian-blur-pass :uniforms `(("dir" ,(vec 1 0)))))
           (v-blur (make-instance 'gaussian-blur-pass :uniforms `(("dir" ,(vec 0 1)))))
           (tone-map (make-instance 'bloom-pass)))
      (connect (port geometry 'position) (port lighting 'position-map) scene)
      (connect (port geometry 'normal) (port lighting 'normal-map) scene)
      (connect (port geometry 'albedo) (port lighting 'albedo-map) scene)
      (connect (port shadow 'shadow) (port lighting 'shadow-map) scene)
      (connect (port lighting 'high-pass) (port h-blur 'previous-pass) scene)
      (connect (port h-blur 'color) (port v-blur 'previous-pass) scene)
      (connect (port v-blur 'color) (port tone-map 'high-pass) scene)
      (connect (port lighting 'color) (port tone-map 'previous-pass) scene)))
  
  (defmethod change-scene :after ((workbench workbench) scene &key old)
    (let ((buffer (asset 'trial 'light-block)))
      (setf (buffer-field buffer "LightBlock.count") 3)
      (setf (buffer-field buffer "LightBlock.lights[0].type") 1)
      (setf (buffer-field buffer "LightBlock.lights[0].direction") (vunit (vec -20 -20 -10)))
      (setf (buffer-field buffer "LightBlock.lights[0].color") (vec 0 0 0))
      (setf (buffer-field buffer "LightBlock.lights[1].type") 2)
      (setf (buffer-field buffer "LightBlock.lights[1].position") (vec 20 10 5))
      (setf (buffer-field buffer "LightBlock.lights[1].color") (vec 50 50 50))
      (setf (buffer-field buffer "LightBlock.lights[2].type") 2)
      (setf (buffer-field buffer "LightBlock.lights[2].position") (vec 0 20 20))
      (setf (buffer-field buffer "LightBlock.lights[2].color") (vec 0 20 0))))
  (maybe-reload-scene))

(defmethod update :after ((workbench workbench) tt dt)
  (let ((buffer (asset 'trial 'light-block))
        (shadow (unit :shadow-map-pass (scene workbench)))
        (camera (unit :camera (scene workbench)))
        (light (vec (* (sin tt) 20) 10 (* (cos tt) 20))))
    ;;(setf (buffer-field buffer "LightBlock.lights[1].position") light)
    ;;(setf (shadow-view-matrix shadow) (mlookat light (vec 0 0 0) (vec 0 1 0)))
    ;;(vsetf (location camera) (* (sin (/ tt -1)) 20) 20 (* (cos (/ tt 1)) 20))
    ))
