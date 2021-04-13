#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass sprite-data (multi-resource-asset file-input-asset)
  ((animations :initform #() :accessor animations)
   (frames :initform #() :accessor frames)))

(defun decode-json-vec (data)
  (cond ((and (jsown:keyp data "x") (jsown:keyp data "w"))
         (vec (jsown:val data "x") (jsown:val data "y") (jsown:val data "w") (jsown:val data "h")))
        ((jsown:keyp data "x")
         (vec (jsown:val data "x") (jsown:val data "y")))
        ((jsown:keyp data "w")
         (vec (jsown:val data "w") (jsown:val data "h")))
        (T (error "No vector found in~%  ~s" data))))

(defun decode-aseprite-frame (data atlas-size)
  (let ((uv (decode-json-vec (jsown:val data "frame")))
        (source (decode-json-vec (jsown:val data "spriteSourceSize")))
        (frame (decode-json-vec (jsown:val data "sourceSize"))))
    (make-instance 'sprite-frame ;; Normalised XY coordinates, centered.
                   :xy (vec (- (+ (vx source) (/ (vz source) 2)) (/ (vx frame) 2))
                            (- (vy frame) (vy source) (/ (vw source) 2))
                            (/ (vz source) 2)
                            (/ (vw source) 2))
                   ;; Normalised UV coordinates, bottom left and top right.
                   :uv (v/ (vec (vx uv)
                                (- (vy atlas-size) (vy uv) (vw uv))
                                (+ (vx uv) (vz uv))
                                (- (vy atlas-size) (vy uv)))
                           (vxyxy atlas-size))
                   ;; Durations are given in milliseconds
                   :duration (/ (jsown:val data "duration") 1000f0))))

(defun decode-aseprite-name (name)
  (intern (with-output-to-string (out)
            (loop for char across name
                  do (case char
                       ((#\  #\_) (write-char #\- out))
                       (T (write-char (char-upcase char) out)))))))

(defun decode-aseprite-tag (data)
  (make-instance 'sprite-animation
                 :name (decode-aseprite-name (jsown:val data "name"))
                 :start (jsown:val data "from")
                 :end (1+ (jsown:val data "to"))
                 :next-animation (when (jsown:keyp data "next")
                                   (intern (string-upcase (jsown:val data "next"))))
                 :loop-to (when (jsown:keyp data "loop")
                            (jsown:val data "loop"))))

(defmethod generate-resources ((sprite sprite-data) (path pathname) &key (min-filter :nearest) (mag-filter :nearest))
  (let* ((data (jsown:parse (alexandria:read-file-into-string path)))
         (meta (jsown:val data "meta"))
         (size (decode-json-vec (jsown:val meta "size"))))
    (setf (frames sprite) (map 'vector (lambda (f) (decode-aseprite-frame f size)) (jsown:val data "frames")))
    (setf (animations sprite) (map 'vector #'decode-aseprite-tag (jsown:val meta "frameTags")))
    (generate-resources 'image-loader (merge-pathnames (jsown:val meta "image") path)
                        :resource (resource sprite 'texture)
                        :min-filter min-filter
                        :mag-filter mag-filter)
    (generate-resources 'mesh-loader (make-sprite-frame-mesh (frames sprite))
                        :resource (resource sprite 'vertex-array))
    (list (resource sprite 'texture)
          (resource sprite 'vertex-array))))
