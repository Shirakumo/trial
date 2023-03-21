#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-asset (trial ascii) image
    #p"ascii.png"
  :min-filter :nearest
  :mag-filter :nearest)

(defun print-ascii-text (text array &key (glyph-width 9) (glyph-height 17) (adjust T) (start 0) (x 0) (y 0) (z 0) (scale 1.0))
  (let ((i start)
        (xi (float x 0f0))
        (yi (float y 0f0))
        (z (float z 0f0))
        (max-x 0.0)
        (gw (float glyph-width))
        (gh (float glyph-height)))
    (when adjust
      (adjust-array array (+ start (* 5 6 (length text)))))
    (macrolet ((vertex (&rest vals)
                 `(progn ,@(loop for val in vals
                                 collect `(progn (setf (aref array i) ,val)
                                                 (incf i))))))
      (flet ((print-letter (char)
               (let* ((c (clamp 0 (- (char-code char) (char-code #\Space)) 95))
                      (u0 (* gw (+ 0 c)))
                      (u1 (* gw (+ 1 c))))
                 (vertex (* scale (+ xi 0.0)) (* scale (+ yi 0.0)) z u0 0.0)
                 (vertex (* scale (+ xi  gw)) (* scale (+ yi 0.0)) z u1 0.0)
                 (vertex (* scale (+ xi  gw)) (* scale (+ yi  gh)) z u1  gh)
                 (vertex (* scale (+ xi  gw)) (* scale (+ yi  gh)) z u1  gh)
                 (vertex (* scale (+ xi 0.0)) (* scale (+ yi  gh)) z u0  gh)
                 (vertex (* scale (+ xi 0.0)) (* scale (+ yi 0.0)) z u0 0.0)
                 (incf xi gw))))
        (loop for char across text
              do (case char
                   (#\Linefeed
                    (when (< max-x xi)
                      (setf max-x xi))
                    (setf xi x)
                    (decf yi gh))
                   (#\Return)
                   (T (print-letter char))))))
    (values i (- max-x x) (- y))))

(define-shader-entity debug-text (located-entity vertex-entity textured-entity standalone-shader-entity)
  ((texture :initarg :font :initform (// 'trial 'ascii) :accessor font)
   (text :initarg :text :initform "" :accessor text)
   (size :initform (vec 0 0) :accessor size)
   (font-size :initform 17.0 :accessor font-size)
   (foreground :initarg :foreground :initform (vec4 0 0 0 1) :accessor foreground)
   (background :initarg :background :initform (vec4 0 0 0 0) :accessor background))
  (:inhibit-shaders (textured-entity :fragment-shader)))

(defmethod initialize-instance :after ((text debug-text) &key)
  (let* ((array (make-array 0 :element-type 'single-float :adjustable T))
         (vbo (make-instance 'vertex-buffer :buffer-data array))
         (vao (make-instance 'vertex-array :bindings `((,vbo :size 3 :offset 0 :stride 20)
                                                       (,vbo :size 2 :offset 12 :stride 20)))))
    (setf (vertex-array text) vao)
    (setf (text text) (text text))))

(defmethod (setf text) :after (_ (text debug-text))
  (let* ((vao (vertex-array text))
         (vbo (caar (bindings vao)))
         (array (buffer-data vbo)))
    (multiple-value-bind (i w h) (print-ascii-text (text text) array :scale (/ (font-size text) 17))
      (vsetf (size text) w h)
      (setf (size vao) (truncate i 5)))
    (when (allocated-p vao)
      (resize-buffer vbo (* 4 (length array)) :data array))))

(defmethod render :before ((text debug-text) (program shader-program))
  (setf (uniform program "foreground") (foreground text))
  (setf (uniform program "background") (background text)))

(define-class-shader (debug-text :vertex-shader)
  "out vec2 texcoord;
uniform sampler2D texture_image;

void main(){
  texcoord /= textureSize(texture_image, 0).rg;
}")

(define-class-shader (debug-text :fragment-shader)
  "in vec2 texcoord;
out vec4 color;
uniform sampler2D texture_image;
uniform vec4 foreground;
uniform vec4 background;

void main(){
  float fg_bg = texture(texture_image, texcoord, 0).r;
  color = mix(foreground, background, fg_bg);
}")
