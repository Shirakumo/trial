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
  (ecase pixel-format
    ((:red)
     (ecase bittage
       (8 :r8)
       (16 :r16)
       (32 :r32)))
    ((:rg)
     (ecase bittage
       (8 :r8)
       (16 :r16)
       (32 :r32)))
    ((:rgb :bgr)
     (ecase bittage
       (8 :rgb8)
       (16 :rgb16)
       (32 :rgb32)))
    ((:rgba :bgra)
     (ecase bittage
       (8 :rgba8)
       (16 :rgba16)
       (32 :rgba32)))))

(defun infer-pixel-type (bittage)
  (ecase bittage
    (8 :unsigned-byte)
    (16 :unsigned-short)
    (32 :unsigned-int)))

(defmethod load ((image image))
  ;; FIXME: Convert pixel data to raw buffer.
  (flet ((load-image (path)
           (with-new-value-restart (path) (new-path "Specify a new image path.")
             (with-retry-restart (retry "Retry loading the image path.")
               (load-image path T)))))
    (let ((input (coerce-asset-input image T)))
      (multiple-value-bind (bits width height bittage pixel-format) (load-image (unlist input))
        (with-unwind-protection (mapcar #'free-image-data (enlist (pixel-data image)))
          ;; FIXME: Maybe attempt to reconcile user-provided data?
          (setf (pixel-data image) bits)
          (setf (pixel-format image) pixel-format)
          (setf (internal-format image)
                (infer-internal-format bittage pixel-format))
          (setf (pixel-type image) (infer-pixel-type bittage))
          (setf (width image) width)
          (setf (height image) height)
          (when (listp input)
            (setf (pixel-data image) (list (pixel-data image)))
            (dolist (input (rest input))
              (multiple-value-bind (bits width height bittage pixel-format) (load-image input)
                (assert (= width (width image)))
                (assert (= height (height image)))
                (assert (eq pixel-format (pixel-format image)))
                (assert (eq (infer-internal-format bittage pixel-format) (internal-format image)))
                (push bits (pixel-data image))))
            (setf (pixel-data image) (nreverse (pixel-data image))))
          (allocate image))))))

(defmethod resize ((image image) width height)
  (error "Resizing is not implemented for images."))
