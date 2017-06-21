#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

;;; Sprite sheets define an animation for every row
;;; and a frame in the animation for every column.

(define-shader-subject sprite-subject (vertex-subject textured-subject)
  ((tile :initarg :tile :accessor tile)
   (size :initarg :size :accessor size))
  (:default-initargs
   :size (vec2 32 32)
   :tile (vec2 0 0))
  (:inhibit-shaders
   (textured-subject :vertex-shader)))

(defmethod shared-initialize :after ((subject sprite-subject) slots &key size)
  (when size
    (setf (vertex-array subject) (change-class (make-rectangle (vx size) (vy size)) 'vertex-array))))

(defmethod paint :before ((subject sprite-subject) (pass shader-pass))
  (let ((shader (shader-program-for-pass pass subject)))
    (setf (uniform shader "size") (size subject))
    (setf (uniform shader "tile") (tile subject))))

(define-class-shader sprite-subject :vertex-shader
  "layout (location = 1) in vec2 in_texcoord;
out vec2 texcoord;
uniform sampler2D texture_image;
uniform vec2 tile = vec2(0, 0);
uniform vec2 size = vec2(32, 32);

void main(){
  // Determine size of a single sprite in the sheet.
  vec2 sprite_size = size / textureSize(texture_image, 0);
  // Determine position of the \"start\" coordinates for this frame.
  vec2 frame_start = sprite_size * tile;
  // Maybe add 1 if we're at the other edges.
  texcoord = frame_start + in_texcoord * sprite_size;
}")

(define-shader-subject animated-sprite-subject (sprite-subject)
  ((animations :initform NIL :accessor animations)
   (clock :initform 0.0d0 :accessor clock)))

(defmethod shared-initialize :after ((subject animated-sprite-subject) slots &key animation frame animations)
  (when animation (setf (animation subject) animation))
  (when frame (setf (frame subject) frame))
  (setf (animations subject) animations))

(defmethod frame ((subject animated-sprite-subject))
  (vx (tile subject)))

(defmethod (setf frame) (value (subject animated-sprite-subject))
  (setf (vx (tile subject)) value))

(defmethod animation ((subject animated-sprite-subject))
  (vy (tile subject)))

(defmethod (setf animation) (value (subject animated-sprite-subject))
  (when (/= value (animation subject))
    (setf (vy (tile subject)) value)
    (setf (vx (tile subject)) (first (nth value (animations subject))))))

(defmethod (setf animations) (value (subject animated-sprite-subject))
  (setf (slot-value subject)
        (loop for spec in value
              for i from 0
              collect (destructuring-bind (duration frames &key (start 0) (next i) (loop-to start))
                          spec
                        (list start duration frames next loop-to)))))

(define-handler (animated-sprite-subject update-sprite-animation tick) (ev dt)
  (let ((tile (tile animated-sprite-subject)))
    (destructuring-bind (duration frames next-anim loop-to)
        (rest (nth (round (vy tile)) (animations animated-sprite-subject)))
      (let ((per-frame-duration (/ duration frames)))
        (incf (clock animated-sprite-subject) dt)
        (when (<= per-frame-duration (clock animated-sprite-subject))
          (decf (clock animated-sprite-subject) per-frame-duration)
          (incf (vx tile)))
        (when (<= frames (frame animated-sprite-subject))
          (cond ((= (vy tile) next-anim)
                 (setf (vx tile) loop-to))
                (T
                 (setf (vy tile) next-anim))))))))
