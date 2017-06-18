#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

;;; Sprite sheets define an animation for every row
;;; and a frame in the animation for every column.

(define-shader-subject sprite-subject (vertex-subject textured-subject)
  ((frame :initform 0 :accessor frame)
   (animation :initarg :animation :accessor animation)
   (size :initarg :size :accessor size))
  (:default-initargs
   :size (vec2 32 32)
   :animation 0)
  (:inhibit-shaders
   (textured-subject :vertex-shader)))

(defmethod shared-initialize :after ((subject sprite-subject) slots &key size)
  (when size
    (setf (vertex-array subject) (change-class (make-rectangle (vx size) (vy size)) 'vertex-array))))

(defmethod paint :before ((subject sprite-subject) (pass shader-pass))
  (let ((shader (shader-program-for-pass pass subject)))
    (setf (uniform shader "size") (size subject))
    (setf (uniform shader "animation") (vec2 (frame subject) (animation subject)))))

(define-class-shader sprite-subject :vertex-shader
  "layout (location = 1) in vec2 in_texcoord;
out vec2 texcoord;
uniform sampler2D texture_image;
uniform vec2 animation = vec2(0, 0);
uniform vec2 size = vec2(32, 32);

void main(){
  // Determine size of a single sprite in the sheet.
  vec2 sprite_size = size / textureSize(texture_image, 0);
  // Determine position of the \"start\" coordinates for this frame.
  vec2 frame_start = sprite_size * animation;
  // Maybe add 1 if we're at the other edges.
  texcoord = frame_start + in_texcoord * sprite_size;
}")

(define-shader-subject animated-sprite-subject (sprite-subject)
  ((animations :initarg :animations :initform NIL :accessor animations)
   (clock :initform 0.0d0 :accessor clock)))

(defmethod (setf animation) :around (value (subject animated-sprite-subject))
  (when (/= value (animation subject))
    (call-next-method)
    (setf (frame subject) 0)))

(define-handler (animated-sprite-subject update-sprite-animation tick) (ev dt)
  (destructuring-bind (duration start-frame frames &optional next)
      (elt (animations animated-sprite-subject)
           (animation animated-sprite-subject))
    (let ((per-frame-duration (/ duration frames)))
      (incf (clock animated-sprite-subject) dt)
      (when (<= per-frame-duration (clock animated-sprite-subject))
        (decf (clock animated-sprite-subject) per-frame-duration)
        (incf (frame animated-sprite-subject)))
      (when (<= frames (frame animated-sprite-subject))
        (setf (frame animated-sprite-subject)
              (if next
                  (nth 1 (elt (animations animated-sprite-subject) next))
                  start-frame))))))
