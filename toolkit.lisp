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

(defun make-matrix (width height)
  (make-array (* width height) :initial-element 0))

(defun matrix-el (matrix x y width)
  (elt matrix (+ x (* y width))))

(defun set-matrix-el (matrix x y width new-value)
  (unless (null matrix)
    (setf (elt matrix (+ x (* y width))) new-value))
  new-value)

(defsetf matrix-el set-matrix-el)

(defun cot (x)
  "Cotangent"
  (tan (- (/ PI 2) x)))
