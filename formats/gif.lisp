#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defun %composite-gif-image (image pixels w)
  (let ((data (skippy:image-data image))
        (table (skippy:color-table image))
        (iw (skippy:width image))
        (transparent (skippy:transparency-index image))
        (yo (skippy:top-position image))
        (xo (skippy:left-position image)))
    (loop for y from 0 below (skippy:height image)
          do (loop for x from 0 below iw
                   do (let ((index (aref data (+ x (* iw y))))
                            (idx (* (+ x xo (* w (+ y yo))) 4)))
                        (when (/= index transparent)
                          (multiple-value-bind (r g b) (skippy:color-rgb (skippy:color-table-entry table index))
                            (setf (aref pixels (+ idx 0)) r)
                            (setf (aref pixels (+ idx 1)) g)
                            (setf (aref pixels (+ idx 2)) b)
                            (setf (aref pixels (+ idx 3)) 255))))))))

(defmethod load-image (path (type (eql :gif)) &key)
  (let* ((stream (skippy:load-data-stream path))
         (w (skippy:width stream))
         (h (skippy:height stream))
         (pixels (make-array (* w h 4) :element-type '(unsigned-byte 8) :initial-element 0)))
    (%composite-gif-image (skippy:last-image stream) pixels w)
    (values pixels w h :unsigned-byte :rgba)))
