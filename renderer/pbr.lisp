(in-package #:org.shirakumo.fraf.trial)

(define-asset (trial brdf-lut) shader-image
    #p"brdf-lut-renderer.glsl"
  :width 512
  :height 512
  :internal-format :rg16
  :min-filter :linear)

(define-gl-struct (pbr-material :include material)
  (textures NIL :initform (vector (// 'trial 'white) (// 'trial 'black) (// 'trial 'black) (// 'trial 'neutral-normal)))
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
         (setf (occlusion-factor material) 0.0)
         (loop for source in (sources (ensure-generated metalness-texture))
               do (setf (target source) :r))
         (loop for source in (sources (ensure-generated roughness-texture))
               do (setf (target source) :g))
         (setf (aref (textures material) 1) (merge-textures (list metalness-texture roughness-texture))))
        ((and metal-rough-texture occlusion-texture)
         (loop for source in (sources (ensure-generated metal-rough-texture))
               do (setf (target source) :rg))
         (loop for source in (sources (ensure-generated occlusion-texture))
               do (setf (target source) :b))
         (setf (aref (textures material) 1) (merge-textures (list metal-rough-texture occlusion-texture))))
        ((and metal-rough-texture)
         (setf (occlusion-factor material) 0.0)
         (setf (aref (textures material) 1) (ensure-generated metal-rough-texture)))
        ((and metalness-texture)
         (setf (occlusion-factor material) 0.0)
         (setf (roughness-factor material) 0.0)
         (setf (aref (textures material) 1) (ensure-generated metalness-texture)))))

(defmethod texture-names ((material pbr-material))
  #(:albedo-texture :metal-rough-occlusion-texture :emission-texture :normal-texture))

(define-material (missing pbr-material))
(define-material (none pbr-material)
  :albedo-texture (// 'trial 'white))

(define-gl-struct pbr-material-block
  (size NIL :initarg :size :initform 64 :reader size)
  (materials (:array (:struct pbr-material) size) :accessor materials))

(defmethod transfer-to progn ((target pbr-material) (material pbr-material))
  (setf (albedo-factor target) (albedo-factor material))
  (setf (emission-factor target) (emission-factor material))
  (setf (metalness-factor target) (metalness-factor material))
  (setf (roughness-factor target) (roughness-factor material))
  (setf (occlusion-factor target) (occlusion-factor material))
  (setf (alpha-cutoff target) (alpha-cutoff material)))

(define-shader-pass pbr-render-pass (standard-shadows-pass light-cache-render-pass)
  ((environment :initform NIL :accessor environment))
  (:shader-file (trial "standard-render-pbr.glsl")))

(defmethod render-with ((pass pbr-render-pass) (material pbr-material) program)
  (enable material pass)
  (let ((textures (textures material)))
    (setf (uniform program "material_id") (local-id material pass))
    (setf (uniform program "albedo_tex") (local-id (aref textures 0) pass))
    (setf (uniform program "metal_rough_occlusion_tex") (local-id (aref textures 1) pass))
    (setf (uniform program "emission_tex") (local-id (aref textures 2) pass))
    (setf (uniform program "normal_tex") (local-id (aref textures 3) pass))))

(defmethod prepare-pass-program :after ((pass pbr-render-pass) program)
  (cond ((environment pass)
         (setf (uniform program "irradiance_map") (local-id (irradiance-map (environment pass)) pass))
         (setf (uniform program "environment_map") (local-id (environment-map (environment pass)) pass))
         (setf (uniform program "brdf_lut") (local-id (// 'trial 'brdf-lut) pass)))
        (T
         ;; KLUDGE: Just set them to bogus values to get the AMD driver to shut up.
         (let ((max (1- (gl:get-integer :max-combined-texture-image-units))))
           (setf (uniform program "irradiance_map") max)
           (setf (uniform program "environment_map") max))
         (setf (uniform program "brdf_lut") 0))))

(defmethod material-block-type ((pass pbr-render-pass))
  'pbr-material-block)

(defclass environment-light (ambient-light)
  ((irradiance-map :initarg :irradiance-map :accessor irradiance-map)
   (environment-map :initarg :environment-map :accessor environment-map)))

(defmethod shared-initialize :after ((light environment-light) slots &key asset)
  (when asset (setf (environment-asset light) asset)))

(defmethod (setf environment-asset) ((map environment-map) (light environment-light))
  (setf (irradiance-map light) (resource map :irradiance-map))
  (setf (environment-map light) (resource map :prefiltered-environment-map)))

(defmethod stage :after ((light environment-light) (area staging-area))
  (stage (irradiance-map light) area)
  (stage (environment-map light) area)
  (stage (// 'trial 'brdf-lut) area))

(defmethod transfer-to progn ((target standard-light) (light environment-light))
  (setf (light-type target) 250))

(defmethod enable :after ((light environment-light) (pass standard-render-pass))
  (enable (irradiance-map light) pass)
  (enable (environment-map light) pass)
  (enable (// 'trial 'brdf-lut) pass)
  (setf (environment pass) light))

(defmethod disable :after ((light environment-light) (pass standard-render-pass))
  (setf (environment pass) NIL))
