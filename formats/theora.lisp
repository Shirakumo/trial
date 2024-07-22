(defpackage #:org.shirakumo.fraf.trial.theora
  (:use #:cl+trial)
  (:shadow #:video)
  (:export #:video))
(in-package #:org.shirakumo.fraf.trial.theora)

(defclass video (trial:video org.shirakumo.fraf.theora:file)
  ((org.shirakumo.fraf.theora:width :reader width)
   (org.shirakumo.fraf.theora:height :reader height)
   (org.shirakumo.fraf.theora:framerate :reader framerate)
   (clock :initform 0f0 :accessor clock)
   (frame :initform 0 :accessor frame)
   (generator :initarg :generator :reader generator)))

(defmethod load-video (input (type (eql :ogv)) &key generator)
  (let* ((file (make-instance 'video :source input :generator generator))
         (width (org.shirakumo.fraf.theora:width file))
         (height (org.shirakumo.fraf.theora:height file))
         (uv-width width)
         (uv-height height))
    (ecase (org.shirakumo.fraf.theora:pixel-format file)
      (:420 (setf uv-width (truncate width 2)
                  uv-height (truncate height 2)))
      (:422 (setf uv-width (truncate width 2)))
      (:444))
    (ensure-instance (resource generator :yuv-data) 'memory
                     :size (+ (* width height)
                              (* uv-width uv-height)
                              (* uv-width uv-height)))
    (ensure-instance (resource generator :y) 'texture
                     :sources (list (make-texture-source :pixel-data (resource generator :yuv-data)
                                                         :pixel-format :red))
                     :width width
                     :height height
                     :min-filter :linear
                     :mag-filter :linear
                     :internal-format :r8
                     :dependencies (list (resource generator :yuv-data)))
    (ensure-instance (resource generator :u) 'texture
                     :width uv-width
                     :height uv-height
                     :min-filter :linear
                     :mag-filter :linear
                     :internal-format :r8
                     :dependencies (list (resource generator :yuv-data)))
    (ensure-instance (resource generator :v) 'texture
                     :width uv-width
                     :height uv-height
                     :min-filter :linear
                     :mag-filter :linear
                     :internal-format :r8
                     :dependencies (list (resource generator :yuv-data)))
    (generate-resources 'mesh-loader (make-rectangle-mesh 1.0 (/ height width))
                        :resource (resource generator :mesh))
    file))

(defmethod finalize ((video video))
  (org.shirakumo.fraf.theora:free video))

(defmethod update ((video video) tt dt fc)
  (let* ((framerate (org.shirakumo.fraf.theora:framerate video))
         (generator (generator video))
         (clock (incf (clock video) dt))
         (frame (frame video))
         (new-frame (truncate (* clock framerate)))
         (old-frame frame)
         (data (resource generator :yuv-data)))
    (loop while (< frame new-frame)
          do (let ((read (org.shirakumo.fraf.theora:read-video data video)))
               (when (< read 1) (return))
               (incf frame read)))
    (when (/= frame old-frame)
      (let ((mem (data-pointer data)))
        (flet ((update (texture)
                 (trial:update-buffer-data texture mem)
                 (cffi:incf-pointer mem (* (width texture) (height texture)))))
          (update (resource generator :y))
          (update (resource generator :u))
          (update (resource generator :v))))
      (setf (frame video) frame))))

(defmethod done-p ((video video))
  (org.shirakumo.fraf.theora:done-p video))

(defmethod duration ((video video))
  (implement!))

(defmethod seek ((video video) to)
  (cond ((= 0 to)
         (org.shirakumo.fraf.theora:reset video)
         0.0)
        (T
         (implement!)))
  (setf (clock video) (float to 0f0))
  (setf (frame video) 0))
