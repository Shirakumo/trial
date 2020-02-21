#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass image (gl-asset texture)
  ())

(defmethod shared-initialize :after ((image image) slots &key &allow-other-keys)
  (dolist (file (enlist (coerce-asset-input image T)))
    (unless (probe-file file)
      (alexandria:simple-style-warning "Input image file ~s does not exist." file))))

(defun flip-image-vertically (image width height components)
  (let ((stride (* width components)))
    (loop for y1 from 0 below (floor height 2)
          for y2 downfrom (1- height)
          do (loop for x1 from (* y1 stride)
                   for x2 from (* y2 stride)
                   repeat stride
                   do (rotatef (aref image x1) (aref image x2))))))

(defgeneric load-image (path type &key width height pixel-type pixel-format &allow-other-keys))

(defmethod load-image (path (type (eql :tga)) &key)
  (let* ((tga (tga:read-tga path))
         (buffer (make-static-vector (length (tga:image-data tga))
                                     :initial-contents (tga:image-data tga))))
    (with-cleanup-on-failure (maybe-free-static-vector buffer)
      (values buffer
              (tga:image-width tga)
              (tga:image-height tga)
              (infer-pixel-type (/ (tga:image-bpp tga)
                                   (tga:image-channels tga))
                                :unsigned)
              (ecase (tga:image-channels tga)
                (3 :bgr)
                (4 :bgra))))))

(defmethod load-image (path (type (eql :png)) &key)
  (let ((png (pngload-fast:load-file path :flatten T :flip-y T)))
    (values (pngload-fast:data png)
            (pngload-fast:width png)
            (pngload-fast:height png)
            (infer-pixel-type (pngload-fast:bit-depth png) :unsigned)
            (ecase (pngload-fast:color-type png)
              (:greyscale :red)
              (:greyscale-alpha :rg)
              (:truecolour :rgb)
              (:truecolour-alpha :rgba)))))

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
  (multiple-value-bind (image height width components) (jpeg:decode-image path)
    (flip-image-vertically image width height components)
    (values image
            width
            height
            :unsigned-byte
            (ecase components
              (1 :red)
              (2 :gr)
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

(defmethod load ((image image))
  (flet ((load-image (path)
           (with-new-value-restart (path) (new-path "Specify a new image path.")
             (with-retry-restart (retry "Retry loading the image path.")
               (load-image path T)))))
    (let ((input (coerce-asset-input image T)))
      (multiple-value-bind (bits width height pixel-type pixel-format) (load-image (unlist input))
        (assert (not (null bits)))
        (with-unwind-protection (mapcar #'free-data (enlist (pixel-data image)))
          ;; FIXME: This whole crap needs to be revised to allow updates.
          ;;        Maybe instead of setting things, we should pass an arglist
          ;;        to ALLOCATE instead.
          (setf (pixel-data image) bits)
          (macrolet ((maybe-set (var)
                       `(when ,var (setf (,var image) ,var))))
            ;; These values /must/ be set in order to actually be able to load the image
            ;; If the user had specified incompatible values the loading would not produce
            ;; expected values.
            (maybe-set width)
            (maybe-set height)
            (maybe-set pixel-format)
            (maybe-set pixel-type)
            ;; The internal format on the other hand should be user-customisable.
            (when (and pixel-format pixel-type (not (slot-boundp image 'internal-format)))
              (setf (internal-format image) (infer-internal-format pixel-type pixel-format))))
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
