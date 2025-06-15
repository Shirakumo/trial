(in-package #:org.shirakumo.fraf.trial)

(defmethod load-image ((stream stream) (type (eql :ktx)) &key)
  (let ((ktx (org.shirakumo.ktx:read-file stream)))
    (loop for mip across (org.shirakumo.ktx:mipmaps ktx)
          for level from 0
          collect (make-image-source (org.shirakumo.ktx:data mip)
                                     (org.shirakumo.ktx:width ktx)
                                     (org.shirakumo.ktx:height ktx)
                                     (org.shirakumo.ktx:gl-type ktx)
                                     (org.shirakumo.ktx:gl-format ktx)
                                     :level level))))

(defmethod save-image ((source texture-source) (stream stream) (type (eql :ktx)) &key)
  (destructuring-bind (x y z w h d) (texture-source-src source)
    (declare (ignore x y z))
    (let ((ktx (org.shirakumo.ktx:create-file
                (vector (pixel-data source))
                (pixel-type source)
                (pixel-format source)
                :width w :height (or h 0) :depth (or d 0))))
      (org.shirakumo.ktx:write-file ktx stream))))

(define-native-image-transcoder :ktx)

;; TODO: ICO to decode to mips? maybe?
