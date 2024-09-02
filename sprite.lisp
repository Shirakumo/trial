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

(defun make-sprite-frame-mesh (frames)
  (with-mesh-construction (v finalize (location uv))
    (loop for frame across frames
          for xy = (xy frame)
          for uv = (uv frame)
          do (v (- (vx xy) (vz xy)) (- (vy xy) (vw xy)) 0 (vx uv) (vy uv))
             (v (+ (vx xy) (vz xy)) (- (vy xy) (vw xy)) 0 (vz uv) (vy uv))
             (v (- (vx xy) (vz xy)) (+ (vy xy) (vw xy)) 0 (vx uv) (vw uv))
             (v (+ (vx xy) (vz xy)) (+ (vy xy) (vw xy)) 0 (vz uv) (vw uv)))
    (finalize-data)))

(defmethod frame ((entity sprite-entity))
  (aref (frames entity) (frame-idx entity)))

(defmethod (setf frame) ((idx integer) (entity sprite-entity))
  (setf (frame-idx entity) idx))

(defmethod (setf frame) ((frame sprite-frame) (entity sprite-entity))
  (setf (frame-idx entity) (or (position frame (frames entity))
                               (error "The frame~%  ~a~%is not part of the sprite-entity." frame))))

(defmethod render ((entity sprite-entity) (program shader-program))
  (declare (optimize speed))
  (setf (uniform program "model_matrix") (model-matrix))
  (setf (uniform program "view_matrix") (view-matrix))
  (setf (uniform program "projection_matrix") (projection-matrix))
  (render-array (vertex-array entity) :vertex-start (* 4 (the (unsigned-byte 32) (frame-idx entity)))
                                      :vertex-count 4
                                      :vertex-form :triangle-strip))

(define-shader-entity animated-sprite (listener sprite-entity)
  ((clock :initform 0f0 :accessor clock)
   (animations :initform #() :accessor animations)
   (animation :initform NIL :accessor animation)
   (playback-speed :initarg :playback-speed :initform 1.0 :accessor playback-speed)
   (playback-direction :initarg :playback-direction :initform +1 :accessor playback-direction)
   (sprite-data :initarg :sprite-data :initform NIL :accessor sprite-data)))

(defmethod initialize-instance :after ((sprite animated-sprite) &key)
  (let ((sprite-data (sprite-data sprite)))
    (when sprite-data
      (setf (sprite-data sprite) sprite-data))))

(defmethod (setf sprite-data) :after ((data sprite-data) (sprite animated-sprite))
  (setf (texture sprite) (resource data 'texture))
  (setf (vertex-array sprite) (resource data 'vertex-array))
  (when (loaded-p data)
    (setf (frames sprite) (frames data))
    (setf (animations sprite) (animations data))
    (unless (animation sprite)
      (setf (animation sprite) 0))))

(defmethod stage :before ((sprite animated-sprite) (area staging-area))
  (register-load-observer area sprite (sprite-data sprite)))

(defmethod observe-load-state ((sprite animated-sprite) (data sprite-data) (state (eql :loaded)) (area staging-area))
  (setf (frames sprite) (frames data))
  (setf (animations sprite) (animations data))
  (unless (animation sprite)
    (setf (animation sprite) 0)))

(defmethod reset-animation ((sprite animated-sprite))
  (setf (clock sprite) 0.0f0)
  (ecase (playback-direction sprite)
    (+1 (setf (frame sprite) (start (animation sprite))))
    (-1 (setf (frame sprite) (1- (end (animation sprite)))))))

(defmethod find-animation (name (sprite animated-sprite) &optional (errorp T))
  (or (find name (animations sprite) :key #'name)
      (when errorp (error "No animation named ~s found." name))))

(defmethod (setf animation) ((animation sprite-animation) (sprite animated-sprite))
  (unless (eql animation (animation sprite))
    (call-next-method)
    (reset-animation sprite)
    animation))

(defmethod (setf animation) ((index integer) (sprite animated-sprite))
  (setf (animation sprite) (aref (animations sprite) index)))

(defmethod (setf animation) ((name symbol) (sprite animated-sprite))
  (let ((animation (animation sprite)))
    (unless (and animation (eql name (name animation)))
      (let ((animation (find name (animations sprite) :key #'name)))
        (if animation
            (setf (animation sprite) animation)
            #-trial-release
            (with-simple-restart (continue "Ignore the animation")
              (error "No animation named ~s found." name)))))))

(defmethod (setf animations) :after (animations (sprite animated-sprite))
  (setf (animation sprite) 0))

(defmethod play (animation (sprite animated-sprite))
  (when (setf (animation sprite) animation)
    (setf (playback-speed sprite) 1.0)
    (setf (playback-direction sprite) +1)))

(defmethod switch-animation ((sprite animated-sprite) animation)
  (setf (playback-speed sprite) 1.0)
  (setf (playback-direction sprite) +1)
  (setf (clock sprite) 0.0f0)
  (setf (animation sprite) animation))

(defmethod handle ((ev tick) (sprite animated-sprite))
  (declare (optimize speed))
  (let* ((idx (frame-idx sprite))
         (frame (svref (frames sprite) idx))
         (animation (animation sprite))
         (clock (clock sprite))
         (duration (duration frame))
         (end (end animation)))
    (declare (type (unsigned-byte 32) idx end))
    (declare (type single-float clock duration))
    (incf clock (* (the single-float (playback-speed sprite))
                   (the single-float (dt ev))))
    (when (<= duration clock)
      (decf clock duration)
      (incf idx (the (member +1 -1) (playback-direction sprite))))
    (setf (clock sprite) clock)
    (cond ((<= end idx)
           (let ((next (next-animation animation)))
             (cond ((eq next (name animation))
                    (setf (frame-idx sprite) (loop-to animation)))
                   (T
                    (switch-animation sprite next)))))
          ((< idx (the (unsigned-byte 32) (start animation)))
           (let ((next (next-animation animation)))
             (cond ((eq next (name animation))
                    (setf (frame-idx sprite) (1- end)))
                   (T
                    (switch-animation sprite next)))))
          (T
           (setf (frame-idx sprite) idx)))))

