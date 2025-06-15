(in-package #:org.shirakumo.fraf.trial)

(defun flip-image-vertically (image width height components)
  (declare (optimize speed (safety 1)))
  (declare (type (unsigned-byte 32) width height))
  (declare (type (unsigned-byte 8) components))
  (macrolet ((typexpand (&body body)
               `(etypecase image
                  ((simple-array (unsigned-byte 8) (*))
                   ,@body)
                  ((simple-array single-float (*))
                   ,@body)
                  (T
                   ,@body))))
    (let ((stride (* width components)))
      (typexpand
       (loop for y1 of-type (unsigned-byte 32) from 0 below (floor height 2)
             for y2 of-type (unsigned-byte 32) downfrom (1- height)
             do (loop for x1 of-type (unsigned-byte 32) from (* y1 stride)
                      for x2 of-type (unsigned-byte 32) from (* y2 stride)
                      repeat stride
                      do (rotatef (aref image x1) (aref image x2)))))
      image)))

(defun convert-image-data (data-in width-in height-in &key (pixel-type-in :unsigned-byte) (pixel-format-in :rgba) (swizzle '(:r :g :b :a)) (pixel-type-out pixel-type-in) (pixel-format-out pixel-format-in)
                                                           (width-out width-in) (height-out height-in))
  (cond ((and (eql pixel-type-in pixel-type-out)
              (eql pixel-format-in pixel-format-out)
              (eql width-in width-out)
              (eql height-in height-out)
              (equal swizzle '(:r :g :b :a)))
         data-in)
        (T
         ;; TODO: implement convert-image-data
         (implement!))))

(define-standard-load-function load-image)
(define-standard-save-function save-image)

(defmethod save-image ((region memory-region) target type &rest args &key &allow-other-keys)
  (let ((vector (make-array (memory-region-size region) :element-type '(unsigned-byte 8))))
    (mem:replace vector region)
    (apply #'save-image vector target type args)))

(defmethod save-image ((vector vector) target type &rest args &key width height pixel-type pixel-format)
  (let ((texture-source (make-image-source vector width height pixel-type pixel-format)))
    (apply #'save-image texture-source target type args)))

(defmethod load-image ((source texture-source) type &rest args &key &allow-other-keys)
  (merge-texture-sources (apply #'load-image (texture-source-pixel-data source) type args) source))

(defmethod load-image ((sources cons) (type (eql T)) &rest args &key &allow-other-keys)
  (loop for source in sources collect (apply #'load-image source T args)))

(defun %load-image (source type &rest args)
  (with-new-value-restart (source) (use-value "Specify a new image source.")
    (with-retry-restart (retry "Retry loading the image source.")
      (apply #'load-image source type args))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass image-loader (compiled-generator)
    ())

  (defclass image (single-resource-asset file-input-asset image-loader)
    ()))

(defmethod generate-resources ((generator image-loader) sources &rest texture-args &key (type T) target swizzle internal-format (resource (resource generator T)) (texture-class 'texture) &allow-other-keys)
  (multiple-value-bind (sources source-swizzle) (normalize-texture-sources (enlist (%load-image sources type)) target)
    (destructuring-bind (width height depth) (texture-sources->texture-size sources)
      (apply #'ensure-instance resource texture-class
             :sources sources :width width :height height :depth depth :target (or target (texture-sources->target sources))
             :internal-format (or internal-format (infer-internal-format (pixel-type (first sources)) (pixel-format (first sources))))
             :swizzle (or swizzle source-swizzle (infer-swizzle-format (pixel-format (first sources))))
             (remf* texture-args :type :target :swizzle :internal-format :resource :texture-class)))))

(defmethod compile-resources ((generator image-loader) sources &rest args &key (source-file-type "png") force)
  (loop for target in (enlist sources)
        for source = (make-pathname :type source-file-type :defaults target)
        do (when (or force (and (probe-file source) (trial:recompile-needed-p target source)))
             (apply #'transcode source T target T args))))

(defmacro define-native-image-transcoder (type)
  `(defmethod transcode (source (source-type symbol) target (target-type (eql ,type)) &rest args &key &allow-other-keys)
     (apply #'save-image (load-image source source-type) target target-type args)))

;; FIXME: Once texture loaded, unload sources to free static memory!
#++
(defmethod load :after ((image image))
  (deallocate (sources (resource image T))))

(define-asset (trial cat) image
    #p"cat.png")

(define-asset (trial logo) image
    #p"logo.png")
