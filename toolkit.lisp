#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(progn
  (defparameter *fps* 30)
  (defparameter *max-steptime* (float (/ 1000 *fps*)))
  (defparameter *relative-time* (float (/ internal-time-units-per-second 1000))))

(defun pause-miliseconds (start)
  (let* ((duration (- (get-internal-real-time) start))
         (secs (/ duration *relative-time*))
         (remainder (- *max-steptime* secs)))
    (max 0 remainder)))

(defun resource-pathname (pathname)
  (let ((pathname (pathname pathname)))
    (if (eql :absolute (first (pathname-directory pathname)))
        pathname
        (asdf:system-relative-pathname :trial (merge-pathnames "data/" pathname)))))

(defun image->framebuffer (image)
  (gl:disable :cull-face)
  (with-finalizing ((format (q+:make-qglframebufferobjectformat)))
    (setf (q+:attachment format) (q+:qglframebufferobject.combined-depth-stencil))
    (setf (q+:mipmap format) T)
    (let ((buffer (q+:make-qglframebufferobject (q+:size image) format)))
      (with-finalizing ((painter (q+:make-qpainter buffer)))
        (q+:draw-image painter 0 0 image)
        (gl:enable :cull-face)
        buffer))))

(defun file->image (file)
  (v:info :trial.toolkit "Loading image ~s" file)
  (unless (probe-file file)
    (error "Invalid file path ~s." file))
  (let ((image (q+:make-qimage (uiop:native-namestring file))))
    (when (q+:is-null image)
      (error "Invalid file ~s." file))
    image))

(defun load-image-buffer (image)
  (with-finalizing ((image (file->image image)))
    (image->framebuffer image)))
