#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

;; ASCII
(defparameter *default-charset*
  #.(with-output-to-string (out)
      (loop for i from 32 to 126
            do (write-char (code-char i) out))
      (write-string "öäüçèàéê§°" out)
      (write-char #\Return out)
      (write-char #\Linefeed out)))

(defclass font (asset)
  ((charset :initarg :charset :accessor charset)
   (size :initarg :size :accessor size))
  (:default-initargs
   :charset *default-charset*
   :size 24))

(defmethod coerce-input ((asset font) (file pathname))
  file)

(defmethod coerce-input ((asset font) (file string))
  (pathname file))

(defmethod finalize-resource ((type (eql 'font)) resource)
  (cl-fond:free resource))

(defmethod load progn ((asset font))
  (setf (resource asset)
        (cl-fond:make-font (first (coerced-inputs asset))
                           (charset asset)
                           :size (size asset)))
  (v:debug :trial.asset "Loaded font ~a" (first (coerced-inputs asset))))

(define-shader-entity text (vertex-entity textured-entity located-entity)
  ((font :initarg :font :accessor font)
   (text :initarg :text :accessor text)
   (color :initarg :color :initform (vec 0 0 0 1) :accessor color)
   (size :initarg :size :accessor size)
   (vbo)
   (ebo))
  (:default-initargs
   :text ""
   :vertex-array NIL
   :texture (make-instance 'texture :resource T)
   :size 24.0))

(defmethod initialize-instance :after ((text text) &key)
  (let ((vbo (make-instance 'vertex-buffer :buffer-type :array-buffer
                                           :data-usage :dynamic-draw
                                           :inputs (list (cffi:null-pointer))
                                           :size 0))
        (ebo (make-instance 'vertex-buffer :buffer-type :element-array-buffer
                                           :data-usage :dynamic-draw
                                           :inputs (list (cffi:null-pointer))
                                           :size 0)))
    (setf (slot-value text 'vbo) vbo)
    (setf (slot-value text 'ebo) ebo)
    (setf (vertex-array text)
          (make-instance 'vertex-array :inputs `((,vbo :size 2 :stride 16 :offset 0)
                                                 (,vbo :size 2 :stride 16 :offset 8)
                                                 ,ebo)))))

(defmethod paint :before ((entity text) (pass shader-pass))
  (let ((program (shader-program-for-pass pass entity)))
    (setf (uniform program "text_color")
          (color entity)))
  (let ((r (/ (size entity) (size (font entity)))))
    (scale-by r r r)))

(define-class-shader (text :fragment-shader)
  "uniform vec4 text_color;

void main(){
   float intensity = color.r;
   color = text_color*intensity;
}")

(defmethod load progn ((entity text))
  (load (slot-value entity 'vbo))
  (load (slot-value entity 'ebo))
  (load (vertex-array entity))
  (setf (font entity) (load (font entity))))

(defmethod offload progn ((entity text))
  (offload (vertex-array entity)))

(defmethod (setf font) :after (font (entity text))
  (when (resource font)
    (setf (resource (texture entity)) (cl-fond:texture (resource font)))
    (setf (text entity) (text entity))))

(defmethod (setf text) :before (text (entity text))
  (let ((vao (vertex-array entity))
        (vbo (slot-value subject 'vbo))
        (ebo (slot-value subject 'ebo))
        (font (resource (font subject))))
    (when font
      (setf (size vao) (cl-fond:update-text font text
                                            (resource vbo)
                                            (resource ebo))))))

(defmethod extent ((subject text))
  (if (resource (font subject))
      (cl-fond:compute-extent (resource (font subject))
                              (text subject))
      '(:l 0 :r 0 :t 0 :b 0 :gap 0)))
