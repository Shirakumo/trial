#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass sprite-frame ()
  ((xy :initarg :xy :initform (error "XY required") :accessor xy)
   (uv :initarg :uv :initform (error "UV required") :accessor uv)
   (duration :initarg :duration :initform 0.1 :accessor duration)))

(defclass sprite-animation ()
  ((name :initarg :name :initform (error "NAME required") :reader name)
   (start :initarg :start :initform (error "START required") :accessor start)
   (end :initarg :end :initform (error "END required") :accessor end)
   (next-animation :initarg :next-animation :initform NIL :accessor next-animation)
   (loop-to :initarg :loop-to :initform NIL :accessor loop-to)))

(defmethod initialize-instance :after ((animation sprite-animation) &key)
  (unless (next-animation animation)
    (setf (next-animation animation) (name animation)))
  (unless (loop-to animation)
    (setf (loop-to animation) (start animation))))

(define-shader-entity sprite-entity (vertex-entity textured-entity)
  ((frame-idx :initarg :frame-idx :initform 0 :accessor frame-idx)
   (frames :initarg :frames :initform #() :accessor frames)
   (vertex-array :initform NIL)))

(defun make-sprite-frame-mesh (frames &key mesh)
  (with-vertex-filling ((or mesh (make-instance 'vertex-mesh :vertex-type 'textured-vertex)))
    (loop for frame across frames
          for xy = (xy frame)
          for uv = (uv frame)
          do (vertex :position (vec (- (vx xy) (vz xy)) (- (vy xy) (vw xy)) 0) :uv (vxy uv))
             (vertex :position (vec (+ (vx xy) (vz xy)) (- (vy xy) (vw xy)) 0) :uv (vzy uv))
             (vertex :position (vec (- (vx xy) (vz xy)) (+ (vy xy) (vw xy)) 0) :uv (vxw uv))
             (vertex :position (vec (+ (vx xy) (vz xy)) (+ (vy xy) (vw xy)) 0) :uv (vzw uv)))))

(defmethod frame ((entity sprite-entity))
  (aref (frames entity) (frame-idx entity)))

(defmethod (setf frame) ((idx integer) (entity sprite-entity))
  (setf (frame-idx entity) idx))

(defmethod (setf frame) ((frame sprite-frame) (entity sprite-entity))
  (setf (frame-idx entity) (or (position frame (frames entity))
                               (error "The frame~%  ~a~%is not part of the sprite-entity." frame))))

(defmethod paint ((entity sprite-entity) (pass shader-pass))
  (let ((program (shader-program-for-pass pass entity)))
    (setf (uniform program "model_matrix") (model-matrix))
    (setf (uniform program "view_matrix") (view-matrix))
    (setf (uniform program "projection_matrix") (projection-matrix)))
  (let ((vao (vertex-array entity)))
    (gl:bind-vertex-array (gl-name vao))
    (%gl:draw-arrays :triangle-strip (* 4 (frame-idx entity)) 4)
    (gl:bind-vertex-array 0)))

(define-shader-subject animated-sprite-subject (sprite-entity)
  ((clock :initform 0f0 :accessor clock)
   (animations :initform #() :accessor animations)
   (animation :initform NIL :accessor animation)
   (playback-speed :initarg :playback-speed :initform 1.0 :accessor playback-speed)
   (playback-direction :initarg :playback-direction :initform +1 :accessor playback-direction)))

(defmethod initialize-instance :after ((sprite animated-sprite-subject) &key sprite-data)
  (when sprite-data
    (register-load-observer sprite sprite-data)))

(defmethod observe-load ((sprite animated-sprite-subject) (data sprite-data))
  (setf (animations sprite) (animations sprite-data))
  (setf (frames sprite) (frames sprite-data))
  (setf (texture sprite) (texture sprite-data))
  (setf (vertex-array sprite) (vertex-array sprite-data)))

(defmethod reset-animation ((subject animated-sprite-subject))
  (setf (clock subject) 0.0d0)
  (ecase (playback-direction subject)
    (+1 (setf (frame subject) (start (animation subject))))
    (-1 (setf (frame subject) (1- (end (animation subject)))))))

(defmethod (setf animation) ((animation sprite-animation) (subject animated-sprite-subject))
  (unless (eql animation (animation subject))
    (call-next-method)
    (reset-animation subject))
  animation)

(defmethod (setf animation) ((index integer) (subject animated-sprite-subject))
  (setf (animation subject) (aref (animations subject) index)))

(defmethod (setf animation) ((name symbol) (subject animated-sprite-subject))
  (setf (animation subject) (or (find name (animations subject) :key #'name)
                                (error "No animation named ~s found." name))))

(defmethod (setf animations) :after (animations (subject animated-sprite-subject))
  (setf (animation subject) 0))

(defmethod switch-animation ((subject animated-sprite-subject) animation)
  (setf (playback-speed subject) 1.0)
  (setf (playback-direction subject) +1)
  (setf (clock subject) 0.0d0)
  (setf (animation subject) animation))

(define-handler (animated-sprite-subject update-sprite-animation tick) (ev dt)
  (let* ((idx (frame-idx animated-sprite-subject))
         (frame (aref (frames animated-sprite-subject) idx))
         (animation (animation animated-sprite-subject)))
    (incf (clock animated-sprite-subject) (* (playback-speed animated-sprite-subject) dt))
    (when (<= (duration frame) (clock animated-sprite-subject))
      (decf (clock animated-sprite-subject) (duration frame))
      (incf idx (playback-direction animated-sprite-subject)))
    (cond ((<= (end animation) idx)
           (let ((next (next-animation animation)))
             (cond ((eq next (name animation))
                    (setf (frame-idx animated-sprite-subject) (loop-to animation)))
                   (T
                    (switch-animation animated-sprite-subject next)))))
          ((< idx (start animation))
           (let ((next (next-animation animation)))
             (cond ((eq next (name animation))
                    (setf (frame-idx animated-sprite-subject) (1- (end animation))))
                   (T
                    (switch-animation animated-sprite-subject next)))))
          (T
           (setf (frame-idx animated-sprite-subject) idx)))))

