(in-package #:org.shirakumo.fraf.trial)

(define-shader-entity image-renderer (standalone-shader-entity)
  ()
  (:inhibit-shaders (shader-entity :fragment-shader)))

(defmethod stage :after ((entity image-renderer) (area staging-area))
  (stage (// 'trial 'fullscreen-square) area))

(defmethod render ((entity image-renderer) (program shader-program))
  (declare (optimize speed))
  (render (// 'trial 'fullscreen-square) program))

(defmethod render ((renderer image-renderer) (texture texture))
  (let ((fbo (gl:gen-framebuffer)))
    (gl:bind-framebuffer :framebuffer fbo)
    (unwind-protect
         (progn
           (gl:viewport 0 0 (width texture) (height texture))
           (%gl:framebuffer-texture :framebuffer :color-attachment0 (gl-name texture) 0)
           (render renderer NIL))
      (gl:bind-framebuffer :framebuffer 0)
      (gl:delete-framebuffers (list fbo)))))

(define-class-shader (image-renderer :vertex-shader)
  "
layout (location = TRIAL_V_LOCATION) in vec3 in_position;
layout (location = TRIAL_V_UV) in vec2 in_uv;
out vec2 uv;
out vec3 world_position;

void main(){
  gl_Position = vec4(in_position, 1.0f);
  world_position = in_position;
  uv = in_uv;
}")

(define-class-shader (image-renderer :fragment-shader)
  "in vec2 uv;
in vec3 world_position;")

(define-shader-entity dynamic-image-renderer (image-renderer dynamic-shader-entity)
  ())

(define-shader-entity cubemap-renderer (vertex-entity textured-entity standalone-shader-entity)
  ((texture :initarg :texture :accessor texture)
   (vertex-array :initform (// 'trial 'unit-cube)))
  (:inhibit-shaders (textured-entity :vertex-shader)
                    (textured-entity :fragment-shader)))

(defmethod render ((renderer cubemap-renderer) (texture texture))
  (with-pushed-matrix ((model-matrix :identity)
                       (view-matrix :zero)
                       (projection-matrix :zero))
    (perspective-projection 90 1.0 0.1 10.0)
    (activate (shader-program renderer))
    (let ((fbo (gl:gen-framebuffer))
          (program (shader-program renderer)))
      (gl:bind-framebuffer :framebuffer fbo)
      (gl:front-face :cw)
      (unwind-protect
           (flet ((render (level)
                    (setf (uniform program "mip_level") level)
                    (gl:viewport 0 0 (* (width texture) (expt 0.5 level)) (*  (height texture) (expt 0.5 level)))
                    (loop for i from 0 below 6
                          for (target up) in '((#.+vx3+ #.-vy3+)
                                               (#.-vx3+ #.-vy3+)
                                               (#.+vy3+ #.+vz3+)
                                               (#.-vy3+ #.-vz3+)
                                               (#.+vz3+ #.-vy3+)
                                               (#.-vz3+ #.-vy3+))
                          do (%gl:framebuffer-texture-2d :framebuffer :color-attachment0
                                                         (+ i (cffi:foreign-enum-value '%gl::enum :texture-cube-map-positive-x))
                                                         (gl-name texture) level)
                             (look-at #.(vec 0 0 0) target up)
                             (render renderer NIL))))
             (case (min-filter texture)
               ((:linear-mipmap-linear :linear-mipmap-nearest)
                (destructuring-bind (min max) (mipmap-levels texture)
                  (setf (uniform program "max_mip_levels") max)
                  (loop for level from min below max
                        do (render level))))
               (T
                (render 0))))
        (gl:front-face :ccw)
        (gl:bind-framebuffer :framebuffer 0)
        (gl:delete-framebuffers (list fbo))))))

(define-class-shader (cubemap-renderer :vertex-shader)
  "out vec3 world_position;

void main@after(){
  world_position = vec3(position);
}")

(defclass shader-image-generator (resource-generator)
  ())

(defmethod generate-resources ((generator shader-image-generator) input &rest texture-args &key (resource (resource generator T)) (texture-class 'texture) &allow-other-keys)
  (let* ((loader (make-instance 'loader))
         (shaders (glsl-toolkit:preprocess (resolve-shader-include input) :include-resolution #'resolve-shader-include))
         (renderer (make-instance 'dynamic-image-renderer :shaders (list :fragment-shader (getf shaders :global)))))
    (apply #'ensure-instance resource texture-class (remf* texture-args :resource :texture-class))
    (with-cleanup-on-failure (finalize loader)
      (commit (list renderer resource) loader)
      (render renderer resource)
      (commit (list resource) loader :unload T))))

(defclass shader-image (single-resource-asset
                        shader-image-generator)
  ())

(defmethod generate-resources :around ((image shader-image) input &key)
  (let* ((*default-pathname-defaults* (pool-path (pool image) NIL)))
    (call-next-method)))

;; KLUDGE: This really sucks, man.
(defmethod generate-resources ((image shader-image) input &key)
  (setf (loaded-p image) T)
  (with-cleanup-on-failure (setf (loaded-p image) NIL)
    (call-next-method)))
