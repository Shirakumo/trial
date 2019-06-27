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
   (animation :initform NIL :accessor animation)
   (clock :initform 0.0d0 :accessor clock)
   (playback-direction :initform +1 :accessor playback-direction)
   (playback-speed :initform 1.0 :accessor playback-speed)))

(defmethod shared-initialize :after ((subject animated-sprite-subject) slots &key animation frame animations)
  (when animations (setf (animations subject) animations))
  (when animation (setf (animation subject) animation))
  (when frame (setf (frame subject) frame)))

(defstruct (sprite-animation (:constructor make-sprite-animation (name start end step next loop)))
  (name NIL :type symbol)
  (start 0 :type (unsigned-byte 32))
  (end 0 :type (unsigned-byte 32))
  (step 0 :type single-float)
  (next 0 :type (unsigned-byte 32))
  (loop 0 :type (unsigned-byte 32)))

(defmethod frame ((subject animated-sprite-subject))
  (vx (tile subject)))

(defmethod (setf animations) (value (subject animated-sprite-subject))
  (setf (slot-value subject 'animations)
        (coerce
         (loop for spec in value
               for i from 0
               collect (destructuring-bind (name start end &key duration step (next i) loop-to) spec
                         (assert (< start end))
                         (let ((step (coerce (cond (step step)
                                                   (duration (/ duration (- end start)))
                                                   (T 0.1))
                                             'single-float))
                               (next (etypecase next
                                       ((integer 0) next)
                                       (symbol (position next value :key #'first)))))
                           (make-sprite-animation name start end step next (or loop-to start)))))
         'simple-vector))
  (setf (animation subject) 0))

(defmethod (setf frame) (value (subject animated-sprite-subject))
  (setf (vx (tile subject)) value))

(defmethod reset-animation ((subject animated-sprite-subject))
  (setf (vx (tile subject)) (sprite-animation-start (animation subject)))
  (setf (clock subject) 0.0d0)
  (setf (playback-speed subject) 1.0)
  (setf (playback-direction subject) +1))

(defmethod (setf animation) ((index integer) (subject animated-sprite-subject))
  (setf (animation subject) (aref (animations subject) index)))

(defmethod (setf animation) ((name symbol) (subject animated-sprite-subject))
  (setf (animation subject) (find name (animations subject) :key #'sprite-animation-name)))

(defmethod (setf animation) ((animation sprite-animation) (subject animated-sprite-subject))
  (unless (eql animation (animation subject))
    (setf (slot-value subject 'animation) animation)
    (reset-animation subject))
  animation)

(define-handler (animated-sprite-subject update-sprite-animation tick) (ev dt)
  (let* ((tile (tile animated-sprite-subject))
         (animations (animations animated-sprite-subject))
         (animation (animation animated-sprite-subject)))
    (incf (clock animated-sprite-subject) (* (playback-speed animated-sprite-subject) dt))
    (when (<= (sprite-animation-step animation) (clock animated-sprite-subject))
      (decf (clock animated-sprite-subject) (sprite-animation-step animation))
      (incf (vx tile) (playback-direction animated-sprite-subject)))
    (cond ((<= (sprite-animation-end animation) (vx tile))
           (let ((next (aref animations (sprite-animation-next animation))))
             (cond ((eq animation next)
                    (setf (vx tile) (sprite-animation-loop animation)))
                   (T
                    (setf (animation animated-sprite-subject) next)))))
          ((< (vx tile) (sprite-animation-start animation))
           (let ((next (aref animations (sprite-animation-next animation))))
             (cond ((eq animation next)
                    (setf (vx tile) (1- (sprite-animation-end animation))))
                   (T
                    (setf (animation animated-sprite-subject) next))))))))
