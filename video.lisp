(in-package #:org.shirakumo.fraf.trial)

(defclass video ()
  ((generator :initarg :generator :accessor generator)))

;; (defgeneric width (video))
;; (defgeneric height (video))
;; (defgeneric update (video dt tt fc))
;; (defgeneric clock (video))
(defgeneric duration (video))
(defgeneric seek (video to))
(defgeneric done-p (video))
(defgeneric framerate (video))

(defmethod resource ((video video) name)
  (resource (generator video) name))

;; TODO: allow switching video formats. We should at least
;;       properly support straight RGB and SRGB as well.
(define-shader-entity video-display (vertex-entity)
  ((textures :initform NIL :accessor textures)
   (asset :initarg :asset :initform NIL)
   (video :accessor video)
   (video-format :initarg :video-format :initform 0 :constant "VIDEO_FORMAT"))
  (:shader-file (trial "renderer/video.glsl")))

(defmethod shared-initialize :after ((entity video-display) slots &key video)
  (when video (setf (video entity) video)))

(defmethod (setf video) :after ((video video) (entity video-display))
  (setf (textures entity) (list (resource video :y) (resource video :u) (resource video :v)))
  (setf (vertex-array entity) (resource video :mesh)))

(defmethod stage :after ((entity video-display) (area staging-area))
  (when (slot-value entity 'asset)
    (register-load-observer area entity (slot-value entity 'asset))
    (stage (slot-value entity 'asset) area))
  (stage (textures entity) area))

(defmethod observe-load-state ((entity video-display) asset (state (eql :loaded)) (area staging-area))
  (setf (video entity) (video asset))
  (restage entity area))

(defmethod bind-textures :after ((entity video-display))
  (destructuring-bind (x y z) (textures entity)
    (bind x :texture0)
    (bind y :texture1)
    (bind z :texture2)))

(defmethod render :before ((entity video-display) (program shader-program))
  (setf (uniform program "plane[0]") 0)
  (setf (uniform program "plane[1]") 1)
  (setf (uniform program "plane[2]") 2))

(defmethod clock ((entity video-display))
  (clock (video entity)))

(defmethod duration ((entity video-display))
  (duration (video entity)))

(defmethod seek ((entity video-display) to)
  (seek (video entity) to))

(defmethod done-p ((entity video-display))
  (done-p (video entity)))

(define-shader-entity video-player (video-display listener)
  ((state :initarg :state :initform :paused :accessor state)
   (loop-p :initform NIL :accessor loop-p)))

(define-handler ((entity video-player) tick) (tt dt fc)
  (when (eql :playing (state entity))
    (update (video entity) tt dt fc)
    (when (done-p entity)
      (typecase (loop-p entity)
        (integer
         (if (<= 0 (decf (loop-p entity)))
             (seek entity 0.0)
             (setf (state entity) :paused)))
        (null
         (setf (state entity) :paused))
        (T
         (seek entity 0.0))))))
