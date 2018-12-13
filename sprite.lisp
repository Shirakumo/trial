#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

;;; Sprite sheets define an animation for every row
;;; and a frame in the animation for every column.

(define-shader-entity sprite-entity (vertex-entity textured-entity)
  ((tile :initarg :tile :initform (vec 0 0) :accessor tile)
   (size :initarg :size :initform (error "SIZE required.") :accessor size))
  (:inhibit-shaders
   (textured-entity :vertex-shader)))

(defmethod paint :before ((entity sprite-entity) (pass shader-pass))
  (let ((shader (shader-program-for-pass pass entity)))
    (setf (uniform shader "size") (size entity))
    (setf (uniform shader "tile") (tile entity))))

(define-class-shader (sprite-entity :vertex-shader)
  "layout (location = 1) in vec2 in_texcoord;
out vec2 texcoord;
uniform sampler2D texture_image;
uniform vec2 tile = vec2(0, 0);
uniform vec2 size = vec2(32, 32);

void main(){
  // Determine size of a single sprite in the sheet.
  vec2 sprite_size = size / textureSize(texture_image, 0);
  // Determine position of the \"start\" coordinates for this frame.
  vec2 frame_start = vec2(sprite_size.x*tile.x, sprite_size.y*tile.y);
  // Maybe add 1 if we're at the other edges.
  texcoord = frame_start + in_texcoord * sprite_size;
}")

(define-shader-subject animated-sprite-subject (sprite-entity)
  ((animations :accessor animations)
   (animation :initform 0 :accessor animation)
   (clock :initform 0.0d0 :accessor clock)))

(defmethod shared-initialize :after ((subject animated-sprite-subject) slots &key animation frame animations)
  (when animations (setf (animations subject) animations))
  (when animation (setf (animation subject) animation))
  (when frame (setf (frame subject) frame)))

(defmethod frame ((subject animated-sprite-subject))
  (- (vx (tile subject))
     (first (svref (animations subject) (animation subject)))))

(defmethod (setf frame) (value (subject animated-sprite-subject))
  (setf (vx (tile subject)) (+ value (first (svref (animations subject) (animation subject))))))

(defmethod (setf animation) :before (value (subject animated-sprite-subject))
  (when (/= value (animation subject))
    (setf (vx (tile subject)) (first (svref (animations subject) value)))))

(defmethod (setf animations) (value (subject animated-sprite-subject))
  (setf (slot-value subject 'animations)
        (coerce
         (loop with idx = 0
               for spec in value
               for i from 0
               collect (destructuring-bind (duration frames &key start (next i) loop-to) spec
                         (let ((start (or start idx)))
                           (prog1 (list start duration frames next (+ start (or loop-to 0)))
                             (setf idx (+ start frames))))))
         'simple-vector)))

(define-handler (animated-sprite-subject update-sprite-animation tick) (ev dt)
  (let ((tile (tile animated-sprite-subject)))
    (destructuring-bind (start duration frames next-anim loop-to)
        (svref (animations animated-sprite-subject) (animation animated-sprite-subject))
      (let ((per-frame-duration (/ duration frames)))
        (incf (clock animated-sprite-subject) dt)
        (when (<= per-frame-duration (clock animated-sprite-subject))
          (decf (clock animated-sprite-subject) per-frame-duration)
          (incf (vx tile)))
        (when (<= (+ start frames) (vx tile))
          (cond ((= (animation animated-sprite-subject) next-anim)
                 (setf (vx tile) loop-to))
                (T
                 (setf (animation animated-sprite-subject) next-anim))))))))
