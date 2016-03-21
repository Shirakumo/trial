#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defun resource-pathname (pathname)
  (if (eql :absolute (first (pathname-directory pathname)))
      pathname
      (asdf:system-relative-pathname :trial (merge-pathnames "data/" pathname))))

(defun load-image-buffer (image)
  (let ((file (resource-pathname image)))
    (unless (probe-file file)
      (error "Invalid file path ~s." file))
    (with-finalizing ((image (q+:make-qimage (uiop:native-namestring file)))
                      (format (q+:make-qglframebufferobjectformat)))
      (when (q+:is-null image)
        (error "Invalid file ~s." file))
      (setf (q+:attachment format) (q+:qglframebufferobject.combined-depth-stencil))
      (let ((buffer (q+:make-qglframebufferobject (q+:size image) format)))
        (with-finalizing ((painter (q+:make-qpainter buffer)))
          (q+:draw-image painter 0 0 image)
          buffer)))))
