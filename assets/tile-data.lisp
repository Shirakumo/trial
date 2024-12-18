(in-package #:org.shirakumo.fraf.trial)

(defclass tileset (texture)
  ((tile-size :initform (vec 16 16) :initarg :tile-size :accessor tile-size)
   (min-filter :initform :nearest)
   (mag-filter :initform :nearest)))

(defclass tilemap (texture)
  ((target :initform :texture-2d)
   (internal-format :initform :rg8ui)
   (min-filter :initform :nearest)
   (mag-filter :initform :nearest)
   (tileset :initarg :tileset :accessor tileset)))

(defmethod dependencies ((tilemap tilemap))
  (list* (tileset tilemap)
         (call-next-method)))

(defclass tile-data (multi-resource-asset file-input-asset compiled-generator)
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
  (let* ((width (gethash "width" data))
         (height (gethash "height" data))
         (pixel-data (make-array (* width height 2) :element-type '(unsigned-byte 8) :initial-element 0))
         (tileset NIL))
    (loop for gid across (gethash "data" data)
          for i from 0
          do (destructuring-bind (x y new-tileset) (decode-tiled-gid gid tilesets)
               (when new-tileset
                 (when (and tileset (not (eq new-tileset tileset)))
                   (error "Mixed tilesets in the same map are not supported."))
                 (setf tileset new-tileset)
                 (setf (aref pixel-data (+ 0 (* 2 i))) x)
                 (setf (aref pixel-data (+ 1 (* 2 i))) (- (floor (height tileset) (vy (tile-size tileset))) 1 y)))))
    (flip-image-vertically pixel-data width height 2)
    (ensure-instance (resource asset (gethash "id" data)) 'tilemap
                     :width width
                     :height height
                     :sources (list (make-image-source pixel-data width height :unsigned-byte :rg-integer))
                     :tileset tileset)))

(defun tileset-source (tileset)
  (let ((source (gethash "source" tileset)))
    (when source
      (let* ((as-path (pathname-utils:pathname* source))
             (ext (pathname-type as-path)))
        (if (or (string-equal "tsj" ext)
                (string-equal "json" ext))
            as-path
            (error "Embedded tileset found, but not JSON format: ~a" as-path))))))

(defun decode-tiled-tileset (raw-tileset path asset)
  (let ((tileset (let ((unembedded-ts-path (tileset-source raw-tileset)))
                   (if unembedded-ts-path
                       (com.inuoe.jzon:parse (merge-pathnames unembedded-ts-path path))
                       raw-tileset))))
    (list :tileset (generate-resources 'image-loader (merge-pathnames (gethash "image" tileset) path)
                                       :resource (resource asset (gethash "name" tileset))
                                       :texture-class 'tileset
                                       :width (gethash "imagewidth" tileset)
                                       :height (gethash "imageheight" tileset)
                                       :tile-size (vec (gethash "tilewidth" tileset)
                                                       (gethash "tileheight" tileset)))
          :first-id (gethash "firstgid" raw-tileset)
          :columns (gethash "columns" tileset))))

(defun load-tiled-data (tile source)
  (let* ((data (com.inuoe.jzon:parse source))
         (tilesets (map 'list (lambda (tileset) (decode-tiled-tileset tileset source tile)) (gethash "tilesets" data)))
         (layers (map 'list (lambda (f) (decode-tiled-layer f tilesets tile)) (gethash "layers" data))))
    (when (string= "isometric" (gethash "orientation" data))
      (dolist (layer layers) (setf (isometric-p layer) T)))
    (values layers tilesets)))

(defmethod compile-resources ((tile tile-data) target &key (source-file-type "tmj"))
  (let ((source (make-pathname :type source-file-type :defaults target)))
    (when (and (not (equal target source))
               (probe-file source)
               (trial:recompile-needed-p target source))
      (multiple-value-bind (layers tilesets) (load-tiled-data tile source)
        (with-trial-io-syntax ()
          (with-open-file (stream target :direction :output :if-exists :supersede)
            (format stream "~s ~s~%" :tilesets
                    (loop for map in tilesets
                          for path = (make-pathname :name (name map) :type "png" :defaults target)
                          do (save-image map path :png)
                          collect (list (name map) :tile-size (list (vx (tile-size map)) (vy (tile-size map))) :source (enough-namestring path target))))
            (format stream "~s ~s~%" :layers
                    (loop for layer in layers
                          for path = (make-pathname :name (princ-to-string (name layer)) :type "raw" :defaults target)
                          do (save-image layer path :raw)
                          collect (list (name layer) :tilemap (name (tilemap layer)) :source (enough-namestring path target))))))))))

(defmethod generate-resources ((asset tile-data) (path pathname) &key)
  (cond ((or (string= "json" (pathname-type path))
             (string= "tmj" (pathname-type path)))
         (load-tiled-data asset path))
        ((string= "lisp" (pathname-type path))
         (destructuring-bind (&key tilesets layers)
             (with-open-file (stream path)
               (with-trial-io-syntax () (parse-sexps stream)))
           (dolist (tileset tilesets)
             (destructuring-bind (name &key tile-size source) tileset
               (generate-resources 'image-loader (merge-pathnames source path)
                                   :resource (resource asset name)
                                   :texture-class 'tileset
                                   :tile-size (vec (first tile-size) (second tile-size)))))
           (dolist (layer layers)
             (destructuring-bind (name &key tileset source) layer
               (generate-resources 'image-loader (merge-pathnames source path)
                                   :resource (resource asset name)
                                   :texture-class 'tilemap
                                   :tileset (resource asset tileset))))))
        (T
         (error "Unsupported file type ~s" (pathname-type path))))
  (list-resources asset))
