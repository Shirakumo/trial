#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-shader-entity text (located-entity vertex-entity colored-entity textured-entity)
  ((texture :initarg :font :accessor font)
   (text :initarg :text :accessor text)
   (size :initarg :size :accessor size)
   (vbo) (ebo))
  (:default-initargs
   :color (vec 0 0 0 1)
   :text "")
  (:inhibit-shaders (colored-entity :fragment-shader)))

(defmethod initialize-instance :after ((text text) &key size)
  (let* ((vbo (make-instance 'vertex-buffer :buffer-type :array-buffer
                                            :data-usage :dynamic-draw
                                            :size 0))
         (ebo (make-instance 'vertex-buffer :buffer-type :element-array-buffer
                                            :data-usage :dynamic-draw
                                            :size 0))
         (vao (make-instance 'vertex-array :bindings `((,vbo :size 2 :stride 16 :offset 0)
                                                       (,vbo :size 2 :stride 16 :offset 8)
                                                       ,ebo))))
    (setf (slot-value text 'vbo) vbo)
    (setf (slot-value text 'ebo) ebo)
    (setf (vertex-array text) vao)
    (unless size (setf (size text) (size (font text))))))

(defmethod paint :around ((text text) (pass shader-pass))
  (let ((r (/ (size text) (size (font text)))))
    (with-pushed-matrix (*model-matrix*)
      (scale-by r r r)
      (call-next-method))))

(define-class-shader (text :fragment-shader)
  "uniform vec4 objectcolor;
out vec4 color;

void main(){
   float intensity = color.r;
   color = objectcolor*intensity;
}")

(defmethod (setf font) :after (font (text text))
  (when (allocated-p font)
    (setf (text text) (text text))))

(defmethod (setf text) :before (string (text text))
  (let ((vao (vertex-array text))
        (vbo (slot-value text 'vbo))
        (ebo (slot-value text 'ebo))
        (font (font text)))
    (when (allocated-p font)
      (setf (size vao) (cl-fond:update-text font string (gl-name vbo) (gl-name ebo))))))

(defmethod extent ((text text))
  (text-extent text (text text)))

(defmethod text-extent ((text text) string)
  (destructuring-bind (&key l r ((:t u)) b gap) (text-extent (font text) string)
    (let ((s (/ (size text) (size (font text)))))
      (list :l (* l s) :r (* r s) :t (* u s) :b (* b s) :gap (* gap s)))))
