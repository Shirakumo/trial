#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

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
                          for (target up) in '((#.(v+ +vx3+) #.(v- +vy3+))
                                               (#.(v- +vx3+) #.(v- +vy3+))
                                               (#.(v+ +vy3+) #.(v+ +vz3+))
                                               (#.(v- +vy3+) #.(v- +vz3+))
                                               (#.(v+ +vz3+) #.(v- +vy3+))
                                               (#.(v- +vz3+) #.(v- +vy3+)))
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
  "out vec3 world_pos;

void main@after(){
  world_pos = vec3(position);
}")

(define-shader-entity environment-map-renderer (cubemap-renderer)
  ()
  (:shader-file (trial "environment-map-renderer.glsl")))

(define-shader-entity irradiance-map-renderer (cubemap-renderer)
  ()
  (:shader-file (trial "irradiance-map-renderer.glsl")))

(define-shader-entity prefiltered-environment-map-renderer (cubemap-renderer)
  ()
  (:shader-file (trial "prefiltered-environment-map-renderer.glsl")))

(defclass environment-map-generator (resource-generator)
  ())

(defmethod generate-resources ((generator environment-map-generator) input &key (width 512) (height width))
  (let* ((loader (make-instance 'loader))
         (hdr (allocate (generate-resources 'image-loader input)))
         (envmap (resource generator :environment-map))
         (irrmap (resource generator :irradiance-map))
         (pfemap (resource generator :prefiltered-environment-map))
         (envmap-renderer (make-instance 'environment-map-renderer :texture hdr))
         (irrmap-renderer (make-instance 'irradiance-map-renderer :texture envmap))
         (pfemap-renderer (make-instance 'prefiltered-environment-map-renderer :texture envmap)))
    (ensure-instance envmap 'texture :internal-format :rgb16f
                                     :target :texture-cube-map
                                     :width width :height height
                                     :min-filter :linear-mipmap-linear
                                     :mag-filter :linear)
    (ensure-instance irrmap 'texture :internal-format :rgb16f
                                     :target :texture-cube-map
                                     :width (ceiling width 16) :height (ceiling height 16)
                                     :min-filter :linear
                                     :mag-filter :linear)
    (ensure-instance pfemap 'texture :internal-format :rgb16f
                                     :target :texture-cube-map
                                     :width (ceiling width 4) :height (ceiling height 4)
                                     :mipmap-levels '(0 5)
                                     :min-filter :linear-mipmap-linear
                                     :mag-filter :linear)
    (with-cleanup-on-failure (finalize loader)
      (commit (list envmap-renderer irrmap-renderer pfemap-renderer irrmap pfemap) loader)
      ;; We do a lil' switcheroo to prevent the cubemap renderer from rendering mips,
      ;; then we let GL bake the mips for the envmap.
      (setf (min-filter envmap) :linear)
      (render envmap-renderer envmap)
      (gl:bind-texture (target envmap) (gl-name envmap))
      (setf (min-filter envmap) :linear-mipmap-linear)
      ;; Now map the irradiance and prefiltered maps as usual.
      (render irrmap-renderer irrmap)
      (render pfemap-renderer pfemap)
      (commit (list envmap irrmap pfemap) loader :unload T))))

(defclass environment-map (file-input-asset
                           multi-resource-asset
                           environment-map-generator)
  ())
