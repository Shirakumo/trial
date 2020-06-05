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

(defmethod print-object ((animation sprite-animation) stream)
  (print-unreadable-object (animation stream :type T)
    (format stream "~s" (name animation))))

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

(defmethod render ((entity sprite-entity) (program shader-program))
  (setf (uniform program "model_matrix") (model-matrix))
  (setf (uniform program "view_matrix") (view-matrix))
  (setf (uniform program "projection_matrix") (projection-matrix))
  (let ((vao (vertex-array entity)))
    (gl:bind-vertex-array (gl-name vao))
    (%gl:draw-arrays :triangle-strip (* 4 (frame-idx entity)) 4)
    (gl:bind-vertex-array 0)))

(define-shader-entity animated-sprite (listener sprite-entity)
  ((clock :initform 0f0 :accessor clock)
   (animations :initform #() :accessor animations)
   (animation :initform NIL :accessor animation)
   (playback-speed :initarg :playback-speed :initform 1.0 :accessor playback-speed)
   (playback-direction :initarg :playback-direction :initform +1 :accessor playback-direction)))

(defmethod initialize-instance :after ((sprite animated-sprite) &key sprite-data)
  (when sprite-data
    (setf (texture sprite) (resource data 'texture))
    (setf (vertex-array sprite) (resource data 'vertex-array))
    (register-load-observer sprite sprite-data)))

(defmethod observe-generation ((sprite animated-sprite) (data sprite-data) result)
  (setf (animations sprite) (animations data))
  (setf (frames sprite) (frames data))
  (setf (animation sprite) 0))

(defmethod reset-animation ((sprite animated-sprite))
  (setf (clock sprite) 0.0d0)
  (ecase (playback-direction sprite)
    (+1 (setf (frame sprite) (start (animation sprite))))
    (-1 (setf (frame sprite) (1- (end (animation sprite)))))))

(defmethod (setf animation) ((animation sprite-animation) (sprite animated-sprite))
  (unless (eql animation (animation sprite))
    (call-next-method)
    (reset-animation sprite))
  animation)

(defmethod (setf animation) ((index integer) (sprite animated-sprite))
  (setf (animation sprite) (aref (animations sprite) index)))

(defmethod (setf animation) ((name symbol) (sprite animated-sprite))
  (setf (animation sprite) (or (find name (animations sprite) :key #'name)
                                (error "No animation named ~s found." name))))

(defmethod (setf animations) :after (animations (sprite animated-sprite))
  (setf (animation sprite) 0))

(defmethod switch-animation ((sprite animated-sprite) animation)
  (setf (playback-speed sprite) 1.0)
  (setf (playback-direction sprite) +1)
  (setf (clock sprite) 0.0d0)
  (setf (animation sprite) animation))

(defmethod handle ((ev tick) (sprite animated-sprite))
  (let* ((idx (frame-idx sprite))
         (frame (aref (frames sprite) idx))
         (animation (animation sprite)))
    (incf (clock sprite) (* (playback-speed sprite) (dt ev)))
    (when (<= (duration frame) (clock sprite))
      (decf (clock sprite) (duration frame))
      (incf idx (playback-direction sprite)))
    (cond ((<= (end animation) idx)
           (let ((next (next-animation animation)))
             (cond ((eq next (name animation))
                    (setf (frame-idx sprite) (loop-to animation)))
                   (T
                    (switch-animation sprite next)))))
          ((< idx (start animation))
           (let ((next (next-animation animation)))
             (cond ((eq next (name animation))
                    (setf (frame-idx sprite) (1- (end animation))))
                   (T
                    (switch-animation sprite next)))))
          (T
           (setf (frame-idx sprite) idx)))))

