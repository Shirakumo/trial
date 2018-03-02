#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

;; LATIN-1
(defparameter *default-charset*
  #.(with-output-to-string (out)
      (loop for i from #x0000 to #x00FF
            do (write-char (code-char i) out))))

(defclass font (gl-asset)
  ((charset :initarg :charset :accessor charset)
   (size :initarg :size :accessor size))
  (:default-initargs
   :charset *default-charset*
   :size 24))

(defmethod finalize-resource ((type (eql 'font)) resource)
  (cl-fond:free resource))

(defmethod load ((font font))
  (setf (gl-name font)
        (cl-fond:make-font (coerce-asset-input font T)
                           (charset font)
                           :size (size font)
                           :oversample 2)))

(defmethod text-extent ((font font) text)
  (if (allocated-p font)
      (cl-fond:compute-extent (gl-name font) text)
      '(:l 0 :r 0 :t 0 :b 0 :gap 0)))

(define-shader-entity text (asset located-entity)
  ((font :initarg :font :accessor font)
   (text :initarg :text :accessor text)
   (color :initarg :color :initform (vec 0 0 0 1) :accessor color)
   (size :initarg :size :accessor size)
   (vbo) (ebo) (vao))
  (:default-initargs
   :text ""
   :size 24.0))

(defmethod initialize-instance :after ((text text) &key)
  (let* ((vbo (make-instance 'vertex-buffer :buffer-type :array-buffer
                                            :data-usage :dynamic-draw
                                            :size 0))
         (ebo (make-instance 'vertex-buffer :buffer-type :element-array-buffer
                                            :data-usage :dynamic-draw
                                            :size 0))
         (vao (make-instance 'vertex-array :buffers `((,vbo :size 2 :stride 16 :offset 0)
                                                      (,vbo :size 2 :stride 16 :offset 8)
                                                      ,ebo))))
    (setf (slot-value text 'vbo) vbo)
    (setf (slot-value text 'ebo) ebo)
    (setf (slot-value text 'vao) vao)))

(defmethod load ((text text))
  (setf (text text) (text text)))

(defmethod paint ((text text) (pass shader-pass))
  (let ((program (shader-program-for-pass pass text))
        (vao (slot-value text 'vao))
        (tex (cl-fond:texture (gl-name (font text))))
        (r (/ (size text) (size (font text)))))
    (gl:active-texture :texture0)
    (gl:bind-texture :texture-2d tex)
    (with-pushed-matrix (*model-matrix*)
      (scale-by r r r)
      (setf (uniform program "model_matrix") (model-matrix))
      (setf (uniform program "view_matrix") (view-matrix))
      (setf (uniform program "projection_matrix") (projection-matrix))
      (setf (uniform program "text_color") (color text))
      (gl:bind-vertex-array (gl-name vao))
      (%gl:draw-elements :triangles (size vao) :unsigned-int 0)
      (gl:bind-vertex-array 0))
    (gl:bind-texture :texture-2d 0)))

(define-class-shader (text :vertex-shader)
  "uniform mat4 model_matrix;
uniform mat4 view_matrix;
uniform mat4 projection_matrix;

layout (location = 0) in vec2 position;
layout (location = 1) in vec2 in_texcoord;
out vec2 texcoord;

void main(){
  texcoord = in_texcoord;
  gl_Position = projection_matrix * view_matrix * model_matrix * vec4(position, 0.0f, 1.0f);
}")

(define-class-shader (text :fragment-shader)
  "uniform vec4 text_color;
uniform sampler2D texture_image;

in vec2 texcoord;
out vec4 color;

void main(){
   float intensity = texture(texture_image, texcoord).r;
   color = text_color*intensity;
}")

(defmethod (setf font) :after (font (entity text))
  (when (allocated-p font)
    (setf (text entity) (text entity))))

(defmethod (setf text) :before (text (entity text))
  (let ((vao (slot-value entity 'vao))
        (vbo (slot-value entity 'vbo))
        (ebo (slot-value entity 'ebo))
        (font (gl-name (font entity))))
    (when font
      (setf (size vao) (cl-fond:update-text font text (gl-name vbo) (gl-name ebo))))))

(defmethod extent ((entity text))
  (text-extent entity (text entity)))

(defmethod text-extent ((entity text) text)
  (destructuring-bind (&key l r ((:t u)) b gap) (text-extent (font entity) text)
    (let ((s (/ (size entity) (size (font entity)))))
      (list :l (* l s) :r (* r s) :t (* u s) :b (* b s) :gap (* gap s)))))
