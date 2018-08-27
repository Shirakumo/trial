#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass image (gl-asset texture)
  ())

(defgeneric load-image (path type &key width height pixel-type pixel-format &allow-other-keys))

(defmethod load-image (path (type (eql :tga)) &key)
  (let* ((tga (tga:read-tga path))
         (buffer (make-static-vector (length (tga:image-data tga))
                                     :initial-contents (tga:image-data tga))))
    (with-cleanup-on-failure (maybe-free-static-vector buffer)
      (values buffer
              (tga:image-width tga)
              (tga:image-height tga)
              (infer-pixel-type (tga:image-bpp tga) :unsigned)
              (ecase (tga:image-channels tga)
                (3 :bgr)
                (4 :bgra))))))

(defmethod load-image (path (type (eql :png)) &key)
  (let ((png (pngload:load-file path :flatten T :flip-y T :static-vector T)))
    (mark-static-vector (pngload:data png))
    (with-cleanup-on-failure (maybe-free-static-vector (pngload:data png))
      (values (pngload:data png)
              (pngload:width png)
              (pngload:height png)
              (infer-pixel-type (pngload:bit-depth png) :unsigned)
              (ecase (pngload:color-type png)
                (:greyscale :red)
                (:greyscale-alpha :rg)
                (:truecolour :rgb)
                (:truecolour-alpha :rgba))))))

(defmethod load-image (path (type (eql :tiff)) &key)
  (let* ((tiff (retrospectiff:read-tiff-file path))
         (bits (aref (retrospectiff:tiff-image-bits-per-sample tiff) 1))
         (buffer (make-static-vector (length (retrospectiff:tiff-image-data tiff))
                                     :initial-contents (retrospectiff:tiff-image-data tiff))))
    ;; FIXME: higher bittage than 8 still returns an ub8 array, but GL doesn't like it.
    (with-cleanup-on-failure (maybe-free-static-vector buffer)
      (values buffer
              (retrospectiff:tiff-image-width tiff)
              (retrospectiff:tiff-image-length tiff)
              (infer-pixel-type bits :unsigned)
              (ecase (retrospectiff:tiff-image-samples-per-pixel tiff)
                (1 :red)
                (3 :rgb)
                (4 :rgba))))))

(defmethod load-image (path (type (eql :tif)) &rest args)
  (apply #'load-image path :tiff args))

(defmethod load-image (path (type (eql :jpeg)) &key)
  (multiple-value-bind (height width components) (jpeg:jpeg-file-dimensions path)
    (values (jpeg:decode-image path)
            width
            height
            :unsigned-byte
            (ecase components
              (1 :red)
              (2 :rg)
              (3 :bgr)
              (4 :bgra)))))

(defmethod load-image (path (type (eql :jpg)) &rest args)
  (apply #'load-image path :jpeg args))

(defmethod load-image (path (type (eql :raw)) &key width height pixel-type pixel-format)
  (declare (optimize speed))
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let* ((data (make-static-vector (file-length stream) :element-type (ecase pixel-type
                                                                          (:float 'single-float)
                                                                          (:byte '(signed-byte 8))
                                                                          (:short '(signed-byte 16))
                                                                          (:int '(signed-byte 32))
                                                                          (:unsigned-byte '(unsigned-byte 8))
                                                                          (:unsigned-short '(unsigned-byte 16))
                                                                          (:unsigned-int '(unsigned-byte 32)))))
           (c (internal-format-components pixel-format))
           (width (or width (when height (/ (length data) height c)) (floor (sqrt (/ (length data) c)))))
           (height (or height (when width (/ (length data) width c)) (floor (sqrt (/ (length data) c)))))
           (reader (ecase pixel-type
                     (:float (lambda (b) (ieee-floats:decode-float32 (fast-io:readu32-le b))))
                     (:byte #'fast-io:read8-le)
                     (:short #'fast-io:read16-le)
                     (:int #'fast-io:read32-le)
                     (:unsigned-byte #'fast-io:readu8-le)
                     (:unsigned-short #'fast-io:readu16-le)
                     (:unsigned-int #'fast-io:readu32-le))))
      (declare (type (unsigned-byte 8) c))
      (declare (type (simple-array * (*)) data))
      (fast-io:with-fast-input (buffer NIL stream)
        (loop for i from 0 below (length data)
              do (setf (aref data i) (funcall reader buffer))))
      (values data
              width
              height
              pixel-type
              pixel-format))))

(defmethod load-image (path (type (eql :r16)) &rest args)
  (apply #'load-image path :raw :pixel-type :half-float args))

(defmethod load-image (path (type (eql :r32)) &rest args)
  (apply #'load-image path :raw :pixel-type :float args))

(defmethod load-image (path (type (eql :ter)) &key)
  (let ((terrain (terrable:read-terrain path)))
    (tg:cancel-finalization terrain)
    (values (terrable:data terrain)
            (terrable:width terrain)
            (terrable:height terrain)
            :signed-short
            :red)))

(defmethod load-image (path (type (eql T)) &rest args)
  (let ((type (pathname-type path)))
    (apply #'load-image path (intern (string-upcase type) "KEYWORD") args)))

(defun free-image-data (data)
  (etypecase data
    (cffi:foreign-pointer
     (cffi:foreign-free data))
    (vector
     (maybe-free-static-vector data))))

(defmethod load ((image image))
  (flet ((load-image (path)
           (with-new-value-restart (path) (new-path "Specify a new image path.")
             (with-retry-restart (retry "Retry loading the image path.")
               (load-image path T)))))
    (let ((input (coerce-asset-input image T)))
      (multiple-value-bind (bits width height pixel-type pixel-format) (load-image (unlist input))
        (assert (not (null bits)))
        (with-unwind-protection (mapcar #'free-image-data (enlist (pixel-data image)))
          ;; FIXME: Maybe attempt to reconcile/compare user-provided data?
          ;; FIXME: This whole crap needs to be revised to allow updates anyway
          (setf (pixel-data image) bits)
          (when width
            (setf (width image) width))
          (when height
            (setf (height image) height))
          (when pixel-format
            (setf (pixel-format image) pixel-format))
          (when pixel-type
            (setf (pixel-type image) pixel-type))
          (when (and pixel-format pixel-type)
            (setf (internal-format image) (infer-internal-format pixel-type pixel-format)))
          (when (listp input)
            (setf (pixel-data image) (list (pixel-data image)))
            (dolist (input (rest input))
              (multiple-value-bind (bits width height pixel-type pixel-format) (load-image input)
                (assert (not (null bits)))
                (when width
                  (assert (= width (width image))))
                (when height
                  (assert (= height (height image))))
                (when pixel-format
                  (assert (eq pixel-format (pixel-format image))))
                (when pixel-type
                  (assert (eq pixel-type (pixel-type image))))
                (push bits (pixel-data image))))
            (setf (pixel-data image) (nreverse (pixel-data image))))
          (allocate image))))))

(defmethod resize ((image image) width height)
  (error "Resizing is not implemented for images."))
