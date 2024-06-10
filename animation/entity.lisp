(in-package #:org.shirakumo.fraf.trial)

(defclass base-animated-entity (mesh-entity)
  ((animation-controller :initform (make-instance 'animation-controller) :accessor animation-controller)))

(define-transfer base-animated-entity animation-controller)

(define-accessor-wrapper-methods clip (base-animated-entity (animation-controller base-animated-entity)))
(define-accessor-wrapper-methods skeleton (base-animated-entity (animation-controller base-animated-entity)))
(define-accessor-wrapper-methods pose (base-animated-entity (animation-controller base-animated-entity)))
(define-accessor-wrapper-methods palette (base-animated-entity (animation-controller base-animated-entity)))
(define-accessor-wrapper-methods palette-texture (base-animated-entity (animation-controller base-animated-entity)))

(defmethod (setf mesh-asset) :after ((asset asset) (entity base-animated-entity))
  (setf (model (animation-controller entity)) asset))

(defmethod stage :after ((entity base-animated-entity) (area staging-area))
  (register-load-observer area (animation-controller entity) (mesh-asset entity))
  (stage (animation-controller entity) area))

(defmethod play (thing (entity base-animated-entity))
  (play thing (animation-controller entity)))

(defmethod fade-to (thing (entity base-animated-entity) &rest args &key &allow-other-keys)
  (apply #'fade-to thing (animation-controller entity) args))

(defmethod add-layer (thing (entity base-animated-entity) &rest args)
  (apply #'add-layer thing (animation-controller entity) args))

(defmethod find-clip (thing (entity base-animated-entity) &optional (errorp T))
  (find-clip thing (animation-controller entity) errorp))

(defmethod list-clips ((entity base-animated-entity))
  (list-clips entity))

(define-shader-entity armature (base-animated-entity lines)
  ((color :initarg :color :initform (vec 0 0 0 1) :accessor color)))

(define-handler ((entity armature) (ev tick) :after) ()
  (replace-vertex-data entity (pose entity) :default-color (color entity)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant SIMULTANEOUS-MORPHS 8))

(define-gl-struct morph-data
  (count :int :initform 0 :accessor morph-count :reader sequence:length)
  (weights (:array :float #.SIMULTANEOUS-MORPHS) :reader weights)
  (indices (:array :int #.SIMULTANEOUS-MORPHS) :reader indices))

(defclass morph ()
  ((texture :initform NIL :accessor texture)
   (weights :initform NIL :accessor weights)
   (morph-data :initform (make-instance 'uniform-buffer :data-usage :dynamic-draw :binding NIL :struct 'morph-data) :accessor morph-data)))

(defmethod stage ((morph morph) (area staging-area))
  (stage (texture morph) area)
  (stage (morph-data morph) area))

(defmethod shared-initialize :after ((morph morph) slots &key animation-data)
  (when animation-data
    (let* ((attributes
             ;; TODO: compute reduced or expanded set of attributes from targets.
             #++
             (loop for target across targets
                   for attributes = (vertex-attributes target) then (union attributes (vertex-attributes target))
                   finally (return attributes))
             '(location normal uv))
           (vertex-count (vertex-count animation-data))
           (morph-count (length (morphs animation-data)))
           ;; The stride is 9, 3 for every color. This wastes space for the UV, since it only needs RG.
           (stride 9)
           (data (make-array (* vertex-count morph-count stride) :element-type 'single-float))
           (texture (make-instance 'texture :target :texture-1d-array
                                            :internal-format :rgb32f
                                            :width morph-count
                                            :height stride
                                            :pixel-data data
                                            :pixel-type :float
                                            :pixel-format :rgb)))
      ;; Compact the targets into a slice per target
      (loop for target across (morphs animation-data)
            for src-data = (vertex-data target)
            for src-stride = (vertex-attribute-stride target)
            for slice from 0 by (* vertex-count stride)
            do (unless (= (vertex-count target) vertex-count)
                 (error "Not all morph targets have the same number of vertices!"))
               (loop for attribute in attributes
                     for src-offset = (vertex-attribute-offset attribute target)
                     for dst-offset = (vertex-attribute-offset attribute attributes)
                     do (when src-offset
                          (loop for src from src-offset below (length src-data) by src-stride
                                for dst from dst-offset by stride
                                do (setf (aref data (+ slice dst 0)) (aref src-data (+ src 0)))
                                   (setf (aref data (+ slice dst 1)) (aref src-data (+ src 1)))
                                   (unless (eq attribute 'uv)
                                     (setf (aref data (+ dst 2)) (aref src-data (+ src 2))))))))
      (setf (texture morph) texture)
      (setf (weights morph) (let ((weights (make-array morph-count :element-type 'single-float :initial-element 0f0)))
                              (replace weights (initial-weights animation-data)))))))

(defmethod update-morph-data ((morph morph))
  (with-buffer-tx (struct (morph-data morph))
    (let ((all-weights (weights morph))
          (weights (weights struct))
          (indices (indices struct))
          (count 0))
      ;; Fill the first 8 directly
      (loop for i from 0 below (min SIMULTANEOUS-MORPHS (length all-weights))
            for weight = (aref all-weights i)
            do (setf (elt indices i) i)
               (setf (elt weights i) weight)
               (when (< 0 weight)
                 (incf count)))
      ;; Now search for bigger ones
      (flet ((find-smallest-index ()
               (let ((small 0))
                 (loop for i from 1 below SIMULTANEOUS-MORPHS
                       do (when (< (elt weights i) (elt weights small))
                            (setf small i)))
                 small)))
        ;; This is basically N^2 but we don't expect to have *that* many
        ;; simultaneous targets anyhow, so it shouldn't be bad in practise
        (loop with smallest = (find-smallest-index)
              for i from SIMULTANEOUS-MORPHS below (length all-weights)
              for weight = (aref all-weights i)
              do (when (< (elt weights smallest) weight)
                   (incf count)
                   (setf (elt weights smallest) weight)
                   (setf (elt indices smallest) i)
                   (setf smallest (find-smallest-index))))
        (setf (morph-count struct) (min SIMULTANEOUS-MORPHS count))))))

(define-shader-entity morphed-entity (base-animated-entity listener)
  ()
  (:shader-file (trial "renderer/morph.glsl")))

(defmethod find-morph (vao (entity morphed-entity) &optional (errorp T))
  (or (gethash vao (morphs (animation-controller entity)))
      (when errorp (error "No morph for ~a found on ~a" vao entity))))

(define-shader-entity skinned-entity (base-animated-entity)
  ((mesh :initarg :mesh :initform NIL :accessor mesh))
  (:shader-file (trial "renderer/skin-matrix.glsl")))

(defmethod (setf mesh-asset) :after ((asset asset) (entity skinned-enity))
  (unless (loaded-p asset)
    (setf (palette entity) #(#.(meye 4)))))

(defmethod render :before ((entity skinned-entity) (program shader-program))
  (when (palette-texture entity)
    (bind (palette-texture entity) :texture5)
    (setf (uniform program "pose") 5)))

(define-shader-entity quat2-skinned-entity (base-animated-entity)
  ()
  (:shader-file (trial "renderer/skin-dquat.glsl")))

(defmethod render :before ((entity quat2-skinned-entity) (program shader-program))
  (when (palette-texture entity)
    (bind (palette-texture entity) :texture5)
    (setf (uniform program "pose") 5)))

(define-shader-entity animated-entity (skinned-entity morphed-entity)
  ())

(defmethod render :before ((entity animated-entity) (program shader-program))
  (setf (uniform program "animation")
        (+ (if (palette-texture entity) 2 0)
           (if (morphs entity) 1 0))))

(define-class-shader (animated-entity :vertex-shader)
  "layout (location = 0) in vec3 in_position;
layout (location = 1) in vec3 in_normal;
layout (location = 2) in vec2 in_uv;
layout (location = 5) in vec4 in_joints;
layout (location = 6) in vec4 in_weights;
uniform int animation = 0;

out vec3 normal;
out vec4 world_pos;
out vec2 uv;

void main(){
  vec3 position = in_position;
  normal = in_normal;
  uv = in_uv;

  if(0 < (animation & 1)) morph_vertex(position, normal, uv);
  if(0 < (animation & 2)) skin_vertex(position, normal, in_joints, in_weights);

  world_pos = model_matrix * vec4(position, 1.0f);
  normal = vec3(model_matrix * vec4(normal, 0.0f));
  gl_Position = projection_matrix * view_matrix * world_pos;
}")
