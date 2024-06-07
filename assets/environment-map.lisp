(in-package #:org.shirakumo.fraf.trial)

(define-shader-entity environment-map-renderer (cubemap-renderer)
  ()
  (:shader-file (trial "renderer/environment-map-renderer.glsl")))

(define-shader-entity irradiance-map-renderer (cubemap-renderer)
  ()
  (:shader-file (trial "renderer/irradiance-map-renderer.glsl")))

(define-shader-entity prefiltered-environment-map-renderer (cubemap-renderer)
  ()
  (:shader-file (trial "renderer/prefiltered-environment-map-renderer.glsl")))

(defclass environment-map-generator (resource-generator)
  ())

;; TODO: Cache
;; TODO: implement dynamic environment map generation based on current scene

(defmethod generate-resources ((generator environment-map-generator) input &key (width 512) (height width) (irradiance-map-scale 1/16) (prefiltered-environment-map-scale 1/4))
  (let* ((loader (make-instance 'loader))
         (hdr (generate-resources 'image-loader input))
         (envmap (resource generator :environment-map))
         (irrmap (resource generator :irradiance-map))
         (pfemap (resource generator :prefiltered-environment-map))
         (envmap-renderer (make-instance 'environment-map-renderer :texture hdr))
         (irrmap-renderer (make-instance 'irradiance-map-renderer :texture envmap))
         (pfemap-renderer (make-instance 'prefiltered-environment-map-renderer :texture envmap)))
    (ensure-instance envmap 'texture :internal-format :rgb16f
                                     :target :texture-cube-map
                                     :width width :height height
                                     :min-filter :linear
                                     :mag-filter :linear)
    (ensure-instance irrmap 'texture :internal-format :rgb16f
                                     :target :texture-cube-map
                                     :width (ceiling (* width irradiance-map-scale)) 
                                     :height (ceiling (* height irradiance-map-scale))
                                     :min-filter :linear
                                     :mag-filter :linear)
    (ensure-instance pfemap 'texture :internal-format :rgb16f
                                     :target :texture-cube-map
                                     :width (ceiling (* width prefiltered-environment-map-scale)) 
                                     :height (ceiling (* height prefiltered-environment-map-scale))
                                     :mipmap-levels '(0 5)
                                     :min-filter :linear-mipmap-linear
                                     :mag-filter :linear)
    (with-cleanup-on-failure (finalize loader)
      (commit (list envmap-renderer irrmap-renderer pfemap-renderer envmap irrmap pfemap) loader)
      ;; We do a lil' switcheroo to prevent the cubemap renderer from rendering mips,
      ;; then we let GL bake the mips for the envmap.
      (render envmap-renderer envmap)
      (setf (min-filter envmap) :linear-mipmap-linear)
      ;; Now map the irradiance and prefiltered maps as usual.
      (render irrmap-renderer irrmap)
      (render pfemap-renderer pfemap)
      (commit (list envmap irrmap pfemap) loader :unload T)
      (list-resources generator))))

(defclass environment-map (file-input-asset
                           multi-resource-asset
                           environment-map-generator)
  ())

;; KLUDGE: This really sucks, man.
(defmethod generate-resources ((map environment-map) input &key)
  (setf (loaded-p map) T)
  (with-cleanup-on-failure (setf (loaded-p map) NIL)
    (call-next-method)))
