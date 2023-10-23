(defpackage #:org.shirakumo.fraf.trial.theora
  (:use #:cl+trial)
  (:shadow #:asset)
  (:export #:asset))
(in-package #:org.shirakumo.fraf.trial.theora)

(defclass asset (file-input-asset multi-resource-asset)
  ((file :accessor file)))

(defmethod generate-resources ((asset asset) input &key)
  (let* ((file (org.shirakumo.fraf.theora:open input))
         (width (org.shirakumo.fraf.theora:width file))
         (height (org.shirakumo.fraf.theora:height file))
         (uv-width width)
         (uv-height height)
         data)
    (setf (file asset) file)
    (ecase (org.shirakumo.fraf.theora:pixel-format file)
      (:420 (setf uv-width (truncate width 2)
                  uv-height (truncate height 2)))
      (:422 (setf uv-width (truncate width 2)))
      (:444))
    (setf data (ensure-instance (resource asset :yuv-data) 'memory
                                :size (+ (* width height)
                                         (* uv-width uv-height)
                                         (* uv-width uv-height))))
    (ensure-instance (resource asset :y) 'texture
                     :width width
                     :height height
                     :min-filter :linear
                     :mag-filter :linear
                     :internal-format :r8)
    (ensure-instance (resource asset :u) 'texture
                     :width uv-width
                     :height uv-height
                     :min-filter :linear
                     :mag-filter :linear
                     :internal-format :r8)
    (ensure-instance (resource asset :v) 'texture
                     :width uv-width
                     :height uv-height
                     :min-filter :linear
                     :mag-filter :linear
                     :internal-format :r8)
    (generate-resources 'mesh-loader (make-rectangle-mesh 1.0 (/ height width))
                        :resource (resource asset :mesh))))

(defmethod load :after ((asset asset))
  (unless (allocated-p (resource asset :yuv-data))
    (allocate (resource asset :yuv-data))))

(defmethod unload :after ((asset asset))
  (when (file asset)
    (org.shirakumo.fraf.theora:free (file asset))
    (setf (file asset) NIL))
  (when (allocated-p (resource asset :yuv-data))
    (deallocate (resource asset :yuv-data))))

(defmethod update ((asset asset) tt dt fc)
  (let* ((file (file asset))
         (framerate (org.shirakumo.fraf.theora:framerate file))
         (frame (truncate (* tt framerate)))
         (data (resource asset :yuv-data))
         (old-fc fc))
    (loop while (< fc frame)
          do (let ((read (org.shirakumo.fraf.theora:read-video data file)))
               (when (< read 1) (return))
               (incf fc read)))
    (when (/= old-fc fc)
      (let ((mem (data-pointer data)))
        (flet ((update (texture)
                 (gl:bind-texture :texture-2d (gl-name texture))
                 (gl:tex-sub-image-2d :texture-2d 0 0 0 (width texture) (height texture) :red :unsigned-byte mem)
                 (cffi:incf-pointer mem (* (width texture) (height texture)))))
          (update (resource asset :y))
          (update (resource asset :u))
          (update (resource asset :v)))))
    fc))
