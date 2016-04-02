#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defvar *assets* (make-hash-table :test 'equal))
(defvar *type-table*
  (alexandria:alist-hash-table
   '(("png" . image-asset)
     ("jpg" . image-asset)
     ("jpeg" . image-asset)
     ("bmp" . image-asset)
     ("gif" . image-asset)
     ("tiff" . image-asset)
     )))

(defclass asset ()
  ((state :initarg :state :accessor state)
   (data :initarg :data :accessor data))
  (:default-initargs
   :state :offloaded))

(defmethod restore ((asset asset))
  (error "Don't know how to restore ~a" asset))

(defmethod restore :around ((asset asset))
  (unless (eql (state asset) :restored)
    (call-next-method))
  asset)

(defmethod restore :after ((asset asset))
  (setf (state asset) :restored))

(defmethod offload ((asset asset))
  (finalize asset))

(defmethod offload :around ((asset asset))
  (unless (eql (state asset) :offloaded)
    (call-next-method))
  asset)

(defmethod offload :after ((asset asset))
  (setf (state asset) :offloaded))

(defmethod finalize ((asset asset))
  (finalize (data asset))
  (setf (data asset) NIL))

(defclass file-asset (asset)
  ((file :initarg :file :accessor file))
  (:default-initargs
   :file (error "FILE required.")))

(defmethod restore :before ((asset file-asset))
  (unless (probe-file file)
    (error "Invalid file path ~s." file)))

(define-asset image-asset (file-asset)
  (:types bmp gif jpg jpeg png pbm pgm ppm tiff xbm xpm))

(defmethod restore ((asset image-asset))
  (let ((image (q+:make-qimage (uiop:native-namestring (file asset)))))
    (when (q+:is-null image)
      (error "Invalid file ~s." file))
    (setf (data asset) image)))

(define-asset texture-asset (image-asset)
  (:types bmp gif jpg jpeg png pbm pgm ppm tiff xbm xpm))

(defmethod restore ((asset image-asset))
  (call-next-method)
  (gl:disable :cull-face)
  (with-finalizing ((format (q+:make-qglframebufferobjectformat))
                    (image (data asset)))
    (setf (q+:attachment format) (q+:qglframebufferobject.combined-depth-stencil))
    (setf (q+:mipmap format) T)
    (let ((buffer (q+:make-qglframebufferobject (q+:size image) format)))
      (with-finalizing ((painter (q+:make-qpainter buffer)))
        (q+:draw-image painter 0 0 image)
        (setf (data asset) buffer)))))

(define-asset sound-asset (file-asset)
  (:types wav ogg mp3))

(define-asset model-asset (file-asset)
  (:types obj))

(defmethod restore ((asset model-asset))
  )

(defmethod asset ((asset asset))
  asset)
