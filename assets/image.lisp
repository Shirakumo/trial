#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass image (gl-asset texture)
  ())

(defmethod load-image (path (type (eql :tga)))
  (let* ((tga (tga:read-tga path))
         (buffer (make-static-vector (length (tga:image-data tga))
                                     :initial-contents (tga:image-data tga))))
    (with-cleanup-on-failure (maybe-free-static-vector buffer)
      (values buffer
              (tga:image-width tga)
              (tga:image-height tga)
              (/ (tga:image-bpp tga)
                 (tga:image-channels tga))
              (ecase (tga:image-channels tga)
                (3 :bgr)
                (4 :bgra))))))

(defmethod load-image (path (type (eql :png)))
  (let ((png (pngload:load-file path :flatten T :flip-y T :static-vector T)))
    (mark-static-vector (pngload:data png))
    (with-cleanup-on-failure (maybe-free-static-vector (pngload:data png))
      (values (pngload:data png)
              (pngload:width png)
              (pngload:height png)
              (pngload:bit-depth png)
              (ecase (pngload:color-type png)
                (:greyscale :red)
                (:greyscale-alpha :rg)
                (:truecolour :rgb)
                (:truecolour-alpha :rgba)
                (:indexed-colour
                 (error "FIXME: Can't deal with indexed colour.")))))))

(defmethod load-image (path (type (eql :tiff)))
  (let* ((tiff (retrospectiff:read-tiff-file path))
         (bits (aref (retrospectiff:tiff-image-bits-per-sample tiff) 1))
         (buffer (make-static-vector (length (retrospectiff:tiff-image-data tiff))
                                     :initial-contents (retrospectiff:tiff-image-data tiff))))
    ;; FIXME: higher bittage than 8 still returns an ub8 array, but GL doesn't like it.
    (with-cleanup-on-failure (maybe-free-static-vector buffer)
      (values buffer
              (retrospectiff:tiff-image-width tiff)
              (retrospectiff:tiff-image-length tiff)
              bits
              (ecase (retrospectiff:tiff-image-samples-per-pixel tiff)
                (1 :red)
                (3 :rgb)
                (4 :rgba))))))

(defmethod load-image (path (type (eql :jpeg)))
  (multiple-value-bind (height width components) (jpeg:jpeg-file-dimensions path)
    (let ((buffer (make-static-vector (* height width components) :element-type '(unsigned-byte 8))))
      (with-cleanup-on-failure (maybe-free-static-vector buffer)
        (let ((buf (jpeg:decode-image path)))
          (dotimes (i height)
            (dotimes (j width)
              (dotimes (k components)
                (setf (aref buffer (+ (* i width) (* j components) k))
                      (aref buf (+ (* i height) (* j components) k)))))))
        (values buffer
                width
                height
                8
                (ecase components
                  (1 :red)
                  (2 :rg)
                  (3 :bgr)
                  (4 :bgra)))))))

(defmethod load-image (path (type (eql :jpg)))
  (load-image path :jpeg))

(defmethod load-image (path (type (eql :raw)))
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let ((data (make-static-vector (file-length stream))))
      (loop for reached = 0 then (read-sequence data stream :start reached)
            while (< reached (length data))
            finally (return data)))))

(defmethod load-image (path (type (eql :r16)))
  (values (load-image path :raw)
          NIL
          NIL
          :f16))

(defmethod load-image (path (type (eql :r32)))
  (values (load-image path :raw)
          NIL
          NIL
          :f32))

(defmethod load-image (path (type (eql :ter)))
  (let ((terrain (terrable:read-terragen path)))
    (tg:cancel-finalization terrain)
    (values (terrable:data terrain)
            (terrable:width terrain)
            (terrable:height terrain)
            :s16
            :red)))

(defmethod load-image (path (type (eql T)))
  (let ((type (pathname-type path)))
    (load-image path (intern (string-upcase type) "KEYWORD"))))

(defun free-image-data (data)
  (etypecase data
    (cffi:foreign-pointer
     (cffi:foreign-free data))
    (vector
     (maybe-free-static-vector data))))

(defun infer-internal-format (bittage pixel-format)
  (intern
   (format NIL "~a~a"
           (ecase pixel-format
             ((:r :red) :r)
             ((:rg :gr) :rg)
             ((:rgb :bgr) :rgb)
             ((:rgba :bgra) :rgba))
           (ecase bittage
             ((:u8 8) :8)
             ((:u16 16) :16)
             ((:u32 32) :32)
             ((:s8) :8i)
             ((:s16) :16i)
             ((:s32) :32i)
             ((:f16) :16f)
             ((:f32) :32f)))))

(defun infer-pixel-type (bittage)
  (ecase bittage
    ((:u8 8) :unsigned-byte)
    ((:u16 16) :unsigned-short)
    ((:u32 32) :unsigned-int)
    ((:s8) :byte)
    ((:s16) :short)
    ((:s32) :int)
    ((:f16) :half-float)
    ((:f32) :float)))

(defmethod load ((image image))
  ;; FIXME: Convert pixel data to raw buffer.
  (flet ((load-image (path)
           (with-new-value-restart (path) (new-path "Specify a new image path.")
             (with-retry-restart (retry "Retry loading the image path.")
               (load-image path T)))))
    (let ((input (coerce-asset-input image T)))
      (multiple-value-bind (bits width height bittage pixel-format) (load-image (unlist input))
        (assert (not (null bits)))
        (with-unwind-protection (mapcar #'free-image-data (enlist (pixel-data image)))
          ;; FIXME: Maybe attempt to reconcile user-provided data?
          (setf (pixel-data image) bits)
          (when pixel-format (setf (pixel-format image) pixel-format))
          (when (and bittage pixel-format) (setf (internal-format image) (infer-internal-format bittage pixel-format)))
          (when bittage (setf (pixel-type image) (infer-pixel-type bittage)))
          (when width (setf (width image) width))
          (when height (setf (height image) height))
          (when (listp input)
            (setf (pixel-data image) (list (pixel-data image)))
            (dolist (input (rest input))
              (multiple-value-bind (bits width height bittage pixel-format) (load-image input)
                (assert (or (null width) (= width (width image))))
                (assert (or (null height) (= height (height image))))
                (assert (or (null pixel-format) (eq pixel-format (pixel-format image))))
                (assert (or (null bittage) (null pixel-format) (eq (infer-internal-format bittage pixel-format) (internal-format image))))
                (push bits (pixel-data image))))
            (setf (pixel-data image) (nreverse (pixel-data image))))
          (allocate image))))))

(defmethod resize ((image image) width height)
  (error "Resizing is not implemented for images."))
