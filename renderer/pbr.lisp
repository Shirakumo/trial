(in-package #:org.shirakumo.fraf.trial)

(define-asset (trial brdf-lut) shader-image
    #p"renderer/brdf-lut-renderer.glsl"
  :width 512
  :height 512
  :internal-format :rg16
  :min-filter :linear)

(define-gl-struct (pbr-material :include material)
  (textures NIL :initform (vector (// 'trial 'white) (// 'trial 'neutral-mro) (// 'trial 'black) (// 'trial 'neutral-normal)))
  (albedo-factor :vec4 :initform (vec 1 1 1 1) :initarg :albedo-factor :accessor albedo-factor)
  (emission-factor :vec3 :initform (vec 1 1 1) :initarg :emission-factor :accessor emission-factor)
  (metalness-factor :float :initform 1.0 :initarg :metalness-factor :accessor metalness-factor)
  (roughness-factor :float :initform 1.0 :initarg :roughness-factor :accessor roughness-factor)
  (occlusion-factor :float :initform 1.0 :initarg :occlusion-factor :accessor occlusion-factor)
  (alpha-cutoff :float :initform 0.5 :initarg :alpha-cutoff :accessor alpha-cutoff))

(defmethod albedo-texture ((material pbr-material))
  (aref (textures material) 0))

(defmethod metal-rough-occlusion-texture ((material pbr-material))
  (aref (textures material) 1))

(defmethod emission-texture ((material pbr-material))
  (aref (textures material) 2))

(defmethod normal-texture ((material pbr-material))
  (aref (textures material) 3))

(defmethod shared-initialize :after ((material pbr-material) slots &key metalness-texture roughness-texture occlusion-texture metal-rough-texture emission-texture)
  (when emission-texture
    (setf (aref (textures material) 2) emission-texture))
  ;; Reshuffle arguments when the textures are badly specified.
  (when (and metalness-texture (eq metalness-texture roughness-texture))
    (setf metal-rough-texture metalness-texture)
    (setf metalness-texture NIL roughness-texture NIL))
  (when (and metal-rough-texture (eq metal-rough-texture occlusion-texture))
    (setf (aref (textures material) 1) metal-rough-texture)
    (setf metal-rough-texture NIL occlusion-texture NIL))
  (cond ((and metalness-texture roughness-texture occlusion-texture)
         (loop for source in (sources (ensure-generated metalness-texture))
               do (setf (target source) :r))
         (loop for source in (sources (ensure-generated roughness-texture))
               do (setf (target source) :g))
         (loop for source in (sources (ensure-generated occlusion-texture))
               do (setf (target source) :b))
         (setf (aref (textures material) 1) (merge-textures (list metalness-texture roughness-texture occlusion-texture))))
        ((and metalness-texture roughness-texture)
         (loop for source in (sources (ensure-generated metalness-texture))
               do (setf (target source) :r))
         (loop for source in (sources (ensure-generated roughness-texture))
               do (setf (target source) :g))
         (let ((tex (merge-textures (list metalness-texture roughness-texture))))
           (setf (swizzle tex) '(:r :g 1 1))
           (setf (aref (textures material) 1) tex)))
        ((and metal-rough-texture occlusion-texture)
         (loop for source in (sources (ensure-generated metal-rough-texture))
               do (setf (target source) :rg))
         (loop for source in (sources (ensure-generated occlusion-texture))
               do (setf (target source) :b))
         (setf (aref (textures material) 1) (merge-textures (list metal-rough-texture occlusion-texture))))
        ((and metal-rough-texture)
         (let ((tex (ensure-generated metal-rough-texture)))
           (setf (swizzle tex) '(:r :g 1 1))
           (setf (aref (textures material) 1) tex)))
        ((and metalness-texture)
         (let ((tex (ensure-generated metalness-texture)))
           (setf (swizzle tex) '(:r 1 1 1))
           (setf (aref (textures material) 1) tex)))
        ((and occlusion-texture)
         (let ((tex (ensure-generated occlusion-texture)))
           (setf (swizzle tex) '(0 1 :r 1))
           (setf (aref (textures material) 1) tex)))))

(defmethod texture-names ((material pbr-material))
  #(:albedo-texture :metal-rough-occlusion-texture :emission-texture :normal-texture))

(define-material (missing pbr-material))
(define-material (none pbr-material)
  :albedo-texture (// 'trial 'white))

(define-gl-struct pbr-material-block
  (size NIL :initarg :size :initform 64 :reader size :reader sequences:length)
  (materials (:array (:struct pbr-material) size) :accessor materials))

(defmethod <- progn ((target pbr-material) (material pbr-material))
  (setf (albedo-factor target) (albedo-factor material))
  (setf (emission-factor target) (emission-factor material))
  (setf (metalness-factor target) (metalness-factor material))
  (setf (roughness-factor target) (roughness-factor material))
  (setf (occlusion-factor target) (occlusion-factor material))
  (setf (alpha-cutoff target) (alpha-cutoff material)))

(define-shader-pass pbr-render-pass (standard-shadows-pass light-cache-render-pass)
  ((environment :initform NIL :accessor environment))
  (:shader-file (trial "renderer/standard-render-pbr.glsl")))

(defmethod render-with ((pass pbr-render-pass) (material pbr-material) program)
  (enable material pass)
  (let ((textures (textures material)))
    (setf (uniform program "material_id") (local-id material pass))
    (setf (uniform program "albedo_tex") (local-id (aref textures 0) pass))
    (setf (uniform program "metal_rough_occlusion_tex") (local-id (aref textures 1) pass))
    (setf (uniform program "emission_tex") (local-id (aref textures 2) pass))
    (setf (uniform program "normal_tex") (local-id (aref textures 3) pass)))
  ;; We have to re-enable them here to ensure they don't get pushed out.
  ;; KLUDGE: This is obviously inefficient since we keep rebinding the textures
  ;;         instead of keeping them once. We could possibly fix this by somehow
  ;;         reserving their IDs and keeping them from being evicted from the LRU
  ;;         cache.
  (let ((env (environment pass)))
    (when env
      (setf (uniform program "irradiance_map") (enable (irradiance-map env) pass))
      (setf (uniform program "environment_map") (enable (environment-map env) pass))
      (setf (uniform program "brdf_lut") (enable (// 'trial 'brdf-lut) pass)))))

(defmethod prepare-pass-program :after ((pass pbr-render-pass) program)
  (unless (environment pass)
    ;; KLUDGE: Just set them to bogus values to get the AMD driver to shut up.
    (let ((max (1- (gl:get-integer :max-combined-texture-image-units))))
      (setf (uniform program "irradiance_map") max)
      (setf (uniform program "environment_map") max))
    (setf (uniform program "brdf_lut") 0)))

(defmethod object-renderable-p ((material pbr-material) (pass pbr-render-pass)) T)

(defmethod material-block-type ((pass pbr-render-pass))
  'pbr-material-block)

(defclass environment-light (ambient-light)
  ((irradiance-map :initarg :irradiance-map :accessor irradiance-map)
   (environment-map :initarg :environment-map :accessor environment-map)))

(define-transfer environment-light irradiance-map environment-map)

(defmethod shared-initialize :after ((light environment-light) slots &key asset)
  (when asset (setf (environment-asset light) asset)))

(defmethod (setf environment-asset) ((map environment-map) (light environment-light))
  (setf (irradiance-map light) (resource map :irradiance-map))
  (setf (environment-map light) (resource map :prefiltered-environment-map)))

(defmethod stage :after ((light environment-light) (area staging-area))
  (stage (irradiance-map light) area)
  (stage (environment-map light) area)
  (stage (// 'trial 'brdf-lut) area))

(defmethod <- progn ((target standard-light) (light environment-light))
  (setf (light-type target) 250))

(defmethod enable :after ((light environment-light) (pass standard-render-pass))
  (setf (environment pass) light))

(defmethod disable :after ((light environment-light) (pass standard-render-pass))
  (setf (environment pass) NIL))

(defmethod shadow-map ((light environment-light)) NIL)
