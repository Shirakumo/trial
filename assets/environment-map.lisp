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

(defun luminance (r g b)
  (+ (* r 0.299)
     (* g 0.587)
     (* b 0.114)))

(defun pixel-luminance (source ptr x y)
  (cffi:incf-pointer ptr (* (pixel-data-stride (pixel-type source) (pixel-format source))
                            (+ (* y (width source)) x)))
  (flet ((ref (o)
           (cffi:mem-aref ptr (pixel-type source) o)))
    (ecase (pixel-format source)
      ((:r :red) (ref 0))
      ((:rg) (* (ref 0) (ref 1)))
      ((:rgb) (luminance (ref 0) (ref 1) (ref 2)))
      ((:rgba) (* (luminance (ref 0) (ref 1) (ref 2)) (ref 3))))))

(defun maximize-pixel-luminance (sources)
  (let ((map (first sources))
        (xm 0) (ym 0) (luminance 0.0))
    (dolist (source sources (values map xm ym luminance))
      (mem:with-pointer-to-array-data (ptr (pixel-data source))
        (dotimes (y (height source))
          (dotimes (x (width source))
            (let ((i (pixel-luminance source ptr x y)))
              (when (< luminance i)
                (setf map source xm x ym y luminance i)))))))))

(defun cubemap-direction (target u v)
  (let ((u (* 2 (- u 0.5)))
        (v (* 2 (- v 0.5))))
    (nvunit
     (ecase target
       ((:+x :texture-cube-map-positive-x) (vec +1 v (- u)))
       ((:-x :texture-cube-map-negative-x) (vec -1 v u))
       ((:+y :texture-cube-map-positive-y) (vec u +1 (- v)))
       ((:-y :texture-cube-map-negative-y) (vec u -1 v))
       ((:+z :texture-cube-map-positive-z) (vec u v +1))
       ((:-z :texture-cube-map-negative-z) (vec (- u) v -1))))))

(defun envmap-brightest-direction (envmap)
  (multiple-value-bind (map x y intensity)
      (maximize-pixel-luminance (download-buffer-data (etypecase envmap
                                                        (environment-map (resource envmap 'irrmap))
                                                        (texture envmap))
                                                      NIL :pixel-type :float))
    (let ((u (/ x (width map)))
          (v (/ y (height map))))
      (values (cubemap-direction (target map) u v) intensity))))
