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
  (cond ((and (gethash "x" data) (gethash "w" data))
         (vec (gethash "x" data) (gethash "y" data) (gethash "w" data) (gethash "h" data)))
        ((gethash "x" data)
         (vec (gethash "x" data) (gethash "y" data)))
        ((gethash "w" data)
         (vec (gethash "w" data) (gethash "h" data)))
        (T (error "No vector found in~%  ~s" data))))

(defun decode-aseprite-frame (data atlas-size)
  (let ((uv (decode-json-vec (gethash "frame" data)))
        (source (decode-json-vec (gethash "spriteSourceSize" data)))
        (frame (decode-json-vec (gethash "sourceSize" data))))
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
                   :duration (/ (gethash "duration" data) 1000f0))))

(defun decode-aseprite-name (name)
  (intern (with-output-to-string (out)
            (loop for char across name
                  do (case char
                       ((#\  #\_) (write-char #\- out))
                       (T (write-char (char-upcase char) out)))))))

(defun decode-aseprite-tag (data)
  (make-instance 'sprite-animation
                 :name (decode-aseprite-name (gethash "name" data))
                 :start (gethash "from" data)
                 :end (1+ (gethash "to" data))
                 :next-animation (when (gethash "next" data)
                                   (intern (string-upcase (gethash "next" data))))
                 :loop-to (when (gethash "loop" data)
                            (gethash "loop" data))))

(defmethod generate-resources ((sprite sprite-data) (path pathname) &key (min-filter :nearest) (mag-filter :nearest))
  (let* ((data (com.inuoe.jzon:parse path))
         (meta (gethash "meta" data))
         (size (decode-json-vec (gethash "size" meta))))
    (setf (frames sprite) (map 'vector (lambda (f) (decode-aseprite-frame f size)) (gethash "frames" data)))
    (setf (animations sprite) (map 'vector #'decode-aseprite-tag (gethash "frameTags" meta)))
    (generate-resources 'image-loader (merge-pathnames (gethash "image" meta) path)
                        :resource (resource sprite 'texture)
                        :min-filter min-filter
                        :mag-filter mag-filter)
    (generate-resources 'mesh-loader (make-sprite-frame-mesh (frames sprite))
                        :resource (resource sprite 'vertex-array))
    (list (resource sprite 'texture)
          (resource sprite 'vertex-array))))
