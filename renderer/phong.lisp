(in-package #:org.shirakumo.fraf.trial)

(define-gl-struct (phong-material :include material)
  (textures NIL :initform (vector (// 'trial 'missing) (// 'trial 'black) (// 'trial 'neutral-normal)))
  (diffuse-factor :vec4 :initarg :diffuse-factor :initform (vec 1 1 1 1) :accessor diffuse-factor)
  (specular-factor :float :initarg :specular-factor :initform 1.0 :accessor specular-factor)
  (alpha-cutoff :float :initarg :alpha-cutoff :initform 0.5 :accessor alpha-cutoff))

(defmethod diffuse-texture ((material phong-material))
  (aref (textures material) 0))

(defmethod specular-texture ((material phong-material))
  (aref (textures material) 1))

(defmethod normal-texture ((material phong-material))
  (aref (textures material) 2))

(defmethod texture-names ((material phong-material))
  #(:diffuse-texture :specular-texture :normal-texture))

(define-gl-struct phong-material-block
  (size NIL :initarg :size :initform 64 :reader size)
  (materials (:array (:struct phong-material) size) :accessor materials))

(defmethod <- progn ((target phong-material) (material phong-material))
  (setf (diffuse-factor target) (diffuse-factor material))
  (setf (specular-factor target) (specular-factor material))
  (setf (alpha-cutoff target) (alpha-cutoff material)))

(defmethod <- progn ((target phong-material) (material pbr-material))
  (setf (diffuse-factor target) (albedo-factor material))
  (setf (specular-factor target) (metalness-factor material))
  (setf (alpha-cutoff target) (alpha-cutoff material)))

(define-shader-pass phong-render-pass (standard-shadows-pass light-cache-render-pass)
  ()
  (:shader-file (trial "renderer/standard-render-phong.glsl")))

(defmethod render-with ((pass phong-render-pass) (material phong-material) program)
  (enable material pass)
  (let ((textures (textures material)))
    (setf (uniform program "material_id") (local-id material pass))
    (setf (uniform program "diffuse_tex") (local-id (aref textures 0) pass))
    (setf (uniform program "specular_tex") (local-id (aref textures 1) pass))
    (setf (uniform program "normal_tex") (local-id (aref textures 2) pass))))

(defmethod render-with ((pass phong-render-pass) (material pbr-material) program)
  (enable material pass)
  (let ((textures (textures material)))
    (setf (uniform program "material_id") (local-id material pass))
    (setf (uniform program "diffuse_tex") (local-id (aref textures 0) pass))
    (setf (uniform program "specular_tex") (local-id (aref textures 1) pass))
    (setf (uniform program "normal_tex") (local-id (aref textures 3) pass))))

(defmethod material-block-type ((pass phong-render-pass))
  'phong-material-block)

(defmethod coerce-object ((material pbr-material) (type (eql 'phong-material)) &key)
  (make-instance 'phong-material
                 :diffuse-texture (albedo-texture material)
                 :specular-texture (metal-rough-occlusion-texture material)
                 :normal-texture (normal-texture material)
                 :diffuse-factor (diffuse-factor material)
                 :specular-factor (vx (metalness-factor material))
                 :alpha-cutoff (alpha-cutoff material)))
