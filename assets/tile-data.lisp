#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass tileset (texture)
  ((tile-size :initform (vec 16 16) :initarg :tile-size :accessor tile-size)
   (min-filter :initform :nearest)
   (mag-filter :initform :nearest)))

(defclass tilemap (texture)
  ((target :initform :texture-2d)
   (pixel-type :initform :unsigned-byte)
   (pixel-format :initform :rg-integer)
   (internal-format :initform :rg8ui)
   (min-filter :initform :nearest)
   (mag-filter :initform :nearest)
   (tileset :initarg :tileset :accessor tileset)))

(defmethod dependencies ((tilemap tilemap))
  (list (tileset tilemap)))

(defclass tile-data (multi-resource-asset file-input-asset)
  ())

(defun decode-tiled-gid (gid tilesets)
  (if (= 0 gid)
      (list 0 0 NIL)
      (let* ((id (ldb (byte 28 0) gid))
             (tileset (loop with min = (first tilesets)
                            for tileset in (rest tilesets)
                            for first-id = (getf tileset :first-id)
                            do (when (and (<= first-id id)
                                          (< (getf min :first-id) first-id))
                                 (setf min tileset))
                            finally (return min))))
        (multiple-value-bind (y x) (floor (- id (getf tileset :first-id))
                                          (getf tileset :columns))
          (list x y (getf tileset :tileset))))))

(defun decode-tiled-layer (data tilesets asset)
  (let* ((width (gethash "width"  data))
         (height (gethash "height"  data))
         (pixel-data (make-array (* width height 2) :element-type '(unsigned-byte 8) :initial-element 0))
         (tileset NIL))
    (loop for gid across (gethash "data"  data)
          for i from 0
          do (destructuring-bind (x y new-tileset) (decode-tiled-gid gid tilesets)
               (when new-tileset
                 (when (and tileset (not (eq new-tileset tileset)))
                   (error "Mixed tilesets in the same map are not supported."))
                 (setf tileset new-tileset)
                 (setf (aref pixel-data (+ 0 (* 2 i))) x)
                 (setf (aref pixel-data (+ 1 (* 2 i))) (- (floor (height tileset) (vy (tile-size tileset))) 1 y)))))
    (flip-image-vertically pixel-data width height 2)
    (ensure-instance (resource asset (gethash "id"  data)) 'tilemap
                     :width width
                     :height height
                     :pixel-data pixel-data
                     :tileset tileset)))

(defun decode-tiled-tileset (data path asset)
  (list :tileset (generate-resources 'image-loader (merge-pathnames (gethash "image"  data) path)
                                     :resource (resource asset (gethash "name"  data))
                                     :texture-class 'tileset
                                     :width (gethash "imagewidth"  data)
                                     :height (gethash "imageheight"  data)
                                     :tile-size (vec (gethash "tileheight"  data)
                                                     (gethash "tilewidth"  data)))
        :first-id (gethash "firstgid"  data)
        :columns (gethash "columns"  data)))

(defmethod generate-resources ((tile tile-data) (path pathname) &key)
  (let* ((data (com.inuoe.jzon:parse path))
         (tilesets (map 'list (lambda (f) (decode-tiled-tileset f path tile)) (gethash "tilesets"  data))))
    (map NIL (lambda (f) (decode-tiled-layer f tilesets tile)) (gethash "layers"  data))
    (list-resources tile)))
