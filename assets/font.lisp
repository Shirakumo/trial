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
      (write-string "öäüçèàéê§°" out)))

(defclass font (asset)
  ((charset :initarg :charset :accessor charset)
   (size :initarg :size :accessor size))
  (:default-initargs
   :charset *default-charset*
   :size 20))

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
                           :size (size asset))))

(define-shader-subject text (vertex-subject textured-subject located-entity)
  ((font :initarg :font :accessor font)
   (text :initarg :text :accessor text)
   (color :initarg :color :initform (vec 0 0 0 1) :accessor color)
   (size :initarg :size :accessor size))
  (:default-initargs
   :vertex-array (make-instance 'vertex-array :resource T)
   :texture (make-instance 'texture :resource T)
   :size 20.0))

(defmethod paint :before ((subject text) (pass shader-pass))
  (let ((program (shader-program-for-pass pass subject)))
    (setf (uniform program "text_color")
          (color subject)))
  (let ((r (/ (size subject) (size (font subject)))))
    (scale-by r r r)))

(define-class-shader text :fragment-shader
  "uniform vec4 text_color;

void main(){
   float intensity = color.r;
   color = text_color*intensity;
}")

(defmethod load progn ((subject text))
  (setf (font subject) (load (font subject)))
  (setf (text subject) (text subject)))

(defmethod offload progn ((subject text))
  (offload (vertex-array subject)))

(defmethod (setf font) :after (font (subject text))
  (when (resource font)
    (setf (resource (texture subject)) (cl-fond:texture (resource font)))))

(defmethod (setf text) :before (text (subject text))
  (let ((vao (vertex-array subject))
        (font (resource (font subject))))
    (when font
      ;; FIXME: would be nice if cl-fond allowed re-using a VAO/VBO somehow...
      (multiple-value-bind (resource size) (cl-fond:compute-text font text)
        (unless (eql T (resource vao))
          (finalize-resource 'vertex-array (resource vao)))
        (setf (resource vao) resource)
        (setf (size vao) size)))))
