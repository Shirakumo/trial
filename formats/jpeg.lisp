(in-package #:org.shirakumo.fraf.trial)

(defmethod load-image ((path pathname) (type (eql :jpeg)))
  (multiple-value-bind (image height width components) (jpeg:decode-image path)
    (flip-image-vertically image width height components)
    (make-image-source image
                       width
                       height
                       :unsigned-byte
                       (ecase components
                         (1 :red)
                         (2 :gr)
                         (3 :bgr)
                         (4 :bgra)))))

(defmethod load-image (source (type (eql :jpeg)))
  (mem:with-memory-region (region source)
    (multiple-value-bind (image height width components) (jpeg:decode-stream (mem:to-stream region))
      (flip-image-vertically image width height components)
      (make-image-source image
                         width
                         height
                         :unsigned-byte
                         (ecase components
                           (1 :red)
                           (2 :gr)
                           (3 :bgr)
                           (4 :bgra))))))

(defmethod load-image (path (type (eql :jpg)))
  (load-image path :jpeg))

(defmethod save-image ((source texture-source) (path pathname) (type (eql :jpeg)) &key)
  (let ((c (ecase (pixel-format source)
             (:rgba 4) (:rgb 3) (:rg 2) ((:r :red) 1))))
    (destructuring-bind (x y z w h d) (texture-source-src source)
      (declare (ignore x y z d))
      ;; FIXME: need to flip, since jpeg encodes as bgra, rather than rgba. Sigh.
      (jpeg:encode-image path (flip-image-vertically (pixel-data source) w h c) c h w))))

(defmethod save-image (source target (type (eql :jpg)) &rest args)
  (apply #'save-image source target :jpeg args))

(define-native-image-transcoder :jpeg)
(define-native-image-transcoder :jpg)
