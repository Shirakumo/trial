#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defun flip-image-vertically (image width height components)
  (let ((stride (* width components)))
    (loop for y1 from 0 below (floor height 2)
          for y2 downfrom (1- height)
          do (loop for x1 from (* y1 stride)
                   for x2 from (* y2 stride)
                   repeat stride
                   do (rotatef (aref image x1) (aref image x2))))
    image))

(defun downscale-image (image width height components width2 height2)
  (let ((target (make-array (* width2 height2 components) :element-type '(unsigned-byte 8) :initial-element 0))
        (x-ratio (float (/ width width2) 0f0))
        (y-ratio (float (/ height height2) 0f0))
        (offset 0))
    (declare (optimize speed))
    (declare (type (unsigned-byte 32) width height width2 height2 offset))
    (declare (type (unsigned-byte 8) components))
    (declare (type (array (unsigned-byte 8)) image))
    (dotimes (i height2 target)
      (dotimes (j width2)
        (dotimes (c components)
          (let ((avg 0) (count 0))
            (declare (type (unsigned-byte 32) avg count))
            (loop for fi from (max 0 (floor (* y-ratio (- i 0.5)))) to (min (1- height) (ceiling (* y-ratio (+ i 0.5))))
                  do (loop for fj from (max 0 (floor (* x-ratio (- j 0.5)))) to (min (1- width) (ceiling (* x-ratio (+ j 0.5))))
                           do (incf avg (aref image (+ c (* components (+ fj (* fi width))))))
                              (incf count)))
            (setf (aref target offset) (floor (/ avg count)))
            (incf offset)))))))

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
         (error "IMPLEMENT"))))

(defgeneric load-image (path type &key width height pixel-type pixel-format &allow-other-keys))

(defmethod load-image (source (type (eql T)) &rest args &key mime-type)
  (if mime-type
      (cl-ppcre:register-groups-bind (type) ("^[^/]*/([^+/]+)" mime-type)
        (remf args :mime-type)
        (apply #'load-image source (kw type) args))
      (call-next-method)))

(defmethod load-image ((path pathname) (type (eql T)) &rest args)
  (let ((type (pathname-type path)))
    (apply #'load-image path (kw type) args)))

(defmethod load-image ((paths cons) type &rest args)
  (let ((data (loop for path in paths
                    collect (multiple-value-list (apply #'load-image path type args)))))
    #-trial-release
    (loop with first = (first data)
          for other in (rest data)
          for path in (rest paths)
          do (unless (equal (rest first) (rest other))
               (error "Images are not congruent! File~%  ~a~%has attributes~%  ~a~%but file~%  ~a~%has attributes~%  ~a"
                      (first paths) (rest first) path (rest other))))
    (values-list (list* (mapcar #'first data) (rest (first data))))))

(defclass image-loader (resource-generator)
  ())

(defmethod generate-resources ((generator image-loader) path &rest texture-args &key (type T) internal-format pixel-format (resource (resource generator T)) (texture-class 'texture) mime-type &allow-other-keys)
  (multiple-value-bind (bits width height pixel-type inferred-pixel-format swizzle)
      (with-new-value-restart (path) (new-path "Specify a new image path.")
        (with-retry-restart (retry "Retry loading the image path.")
          (load-image path type :mime-type mime-type)))
    (assert (not (null bits)))
    (let ((pixel-format (or pixel-format inferred-pixel-format)))
      (with-unwind-protection (free-data bits)
        (apply #'ensure-instance resource texture-class
               :width width :height height
               :pixel-data bits
               :pixel-type pixel-type
               :pixel-format pixel-format
               :internal-format (or internal-format
                                    (infer-internal-format pixel-type pixel-format))
               :swizzle (or swizzle (infer-swizzle-format pixel-format))
               (remf* texture-args :mime-type :type :resource :texture-class))))))

(defclass image (single-resource-asset file-input-asset image-loader)
  ())

;; FIXME: multi-image textures such as cube maps
