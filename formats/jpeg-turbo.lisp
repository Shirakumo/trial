#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defmethod load-image ((path pathname) (type (eql :jpeg)))
  (jpeg-turbo:with-decompressor (handle)
    (multiple-value-bind (image width height) (jpeg-turbo:decompress handle path :pixel-format :rgb :flags '(:bottomup))
      (make-image-source image width height :unsigned-byte :rgb))))

(defun decompress-header-from-pointer (handle ptr size)
  (cffi:with-foreign-objects ((width      :int 1)
                              (height     :int 1)
                              (subsamp    :int 1)
                              (colorspace :int 1))
    (let ((code (cffi:foreign-funcall "tjDecompressHeader3"
                                      :pointer handle
                                      :pointer ptr
                                      :ulong size
                                      :pointer width
                                      :pointer height
                                      :pointer subsamp
                                      :pointer colorspace
                                      :int)))
      (if (zerop code)
          (values (cffi:mem-aref width  :int) (cffi:mem-aref height :int))
          (error 'jpeg-turbo:jpeg-error :error-string (jpeg-turbo::last-error handle))))))

(defun decompress-from-pointer (handle ptr size &key (scaling-factor 1) (pixel-format :rgb) flags)
  (multiple-value-bind (width height) (decompress-header-from-pointer handle ptr size)
    (setq width  (jpeg-turbo::scale width  scaling-factor)
          height (jpeg-turbo::scale height scaling-factor))
    (let ((region (mem:allocate NIL (* width height (jpeg-turbo::pixel-size pixel-format)))))
      (let ((code (cffi:foreign-funcall "tjDecompress2"
                                        :pointer handle
                                        :pointer ptr
                                        :ulong size
                                        :pointer (memory-region-pointer region)
                                        :int width
                                        :int 0
                                        :int height
                                        jpeg-turbo::pixel-format pixel-format
                                        jpeg-turbo::jpeg-turbo-flags (cons :no-realloc flags)
                                        :int)))
        (if (zerop code)
            (values region width height)
            (error 'jpeg-turbo:jpeg-error :error-string (jpeg-turbo::last-error handle)))))))

(defmethod load-image (source (type (eql :jpeg)))
  (mem:with-memory-region (region source)
    (jpeg-turbo:with-decompressor (handle)
      (multiple-value-bind (image width height) (decompress-from-pointer handle (memory-region-pointer region) (memory-region-size region)
                                                                         :pixel-format :rgb :flags '(:bottomup))
        (make-image-source image width height :unsigned-byte :rgb)))))

(defmethod load-image (path (type (eql :jpg)))
  (load-image path :jpeg))

;; (defmethod save-image ((source texture-source) (path pathname) (type (eql :jpeg)) &key)
;;   (destructuring-bind (x y z w h d) (texture-source-src source)
;;     (declare (ignore x y z d))
;;     ))

;; (defmethod save-image (source target (type (eql :jpg)) &rest args)
;;   (apply #'save-image source target :jpeg args))
