(in-package #:org.shirakumo.fraf.trial)

#+trial-release
(deploy:define-hook (:boot devil) ()
  (cl-devil:init))

#-trial-release
(cl-devil:init)

(defun %devil-image-to-source (image)
  (cl-devil:with-bound-image image
    (when (eql :origin-upper-left (cl-devil:image-origin))
      (cl-devil:flip-image))
    (let* ((size (* (cl-devil:image-width)
                    (cl-devil:image-height)
                    (cl-devil:image-bytes-per-pixel)))
           (region (mem:allocate NIL size)))
      (mem:replace region (memory-region (cl-devil:get-data) size))
      (make-image-source region
                         (cl-devil:image-width)
                         (cl-devil:image-height)
                         (cl-devil:image-type)
                         (case (cl-devil:image-format)
                           (:luminance :red)
                           (:luminance-alpha :rg)
                           (T (cl-devil:image-format)))))))

(defmethod load-image ((path pathname) (type symbol) &key)
  (cl-devil:with-images ((image path))
    (%devil-image-to-source image)))

(defmethod load-image (source (type symbol) &key)
  (mem:with-memory-region (region source)
    (cl-devil:with-images ((image type (memory-region-pointer region) (memory-region-size region)))
      (%devil-image-to-source image))))

(defmethod save-image ((source texture-source) (path pathname) type &key)
  (mem:with-memory-region (region (pixel-data source))
    (cl-devil:with-images (image)
      (cl-devil:with-bound-image image
        (destructuring-bind (x y z w h d) (texture-source-src source)
          (declare (ignore x y z))
          (let ((format (case (pixel-format source)
                          (:red :luminance)
                          (:rg :luminance-alpha)
                          (T (pixel-format source))))
                (bpp (pixel-data-stride (pixel-type source) (pixel-format source))))
            (cl-devil:tex-image w h d bpp format (pixel-type source) (memory-region-pointer region))))
        (cl-devil:save type path)))))
