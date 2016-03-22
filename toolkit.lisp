#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defun resource-pathname (pathname)
  (let ((pathname (pathname pathname)))
    (if (eql :absolute (first (pathname-directory pathname)))
        pathname
        (asdf:system-relative-pathname :trial (merge-pathnames "data/" pathname)))))

(defun image->framebuffer (image)
  (with-finalizing ((format (q+:make-qglframebufferobjectformat)))
    (setf (q+:attachment format) (q+:qglframebufferobject.combined-depth-stencil))
    (setf (q+:mipmap format) T)
    (let ((buffer (q+:make-qglframebufferobject (q+:size image) format)))
      (with-finalizing ((painter (q+:make-qpainter buffer)))
        (q+:draw-image painter 0 0 image)
        buffer))))

(defun file->image (file)
  (unless (probe-file file)
    (error "Invalid file path ~s." file))
  (let ((image (q+:make-qimage (uiop:native-namestring file))))
    (when (q+:is-null image)
      (error "Invalid file ~s." file))
    image))

(defun load-image-buffer (image)
  (with-finalizing ((image (file->image image)))
    (image->framebuffer image)))

(qt::def-unmarshal (variable "GLuint" type)
  (cffi:mem-ref variable :uint))

;; THIS IS BROKEN. FIX
(qt::defmarshal (value (:|GLuint|) :around NIL :type integer)
  (cffi:make-pointer value))
