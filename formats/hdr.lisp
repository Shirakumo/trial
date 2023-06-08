#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defmethod load-image (path (type (eql :hdr)) &key)
  (let ((file (3b-hdr:read-hdr-file path :y-up T)))
    (make-image-source (3b-hdr:data file)
                       (3b-hdr:width file)
                       (3b-hdr:height file)
                       (3b-hdr:gl-pixel-type file)
                       (3b-hdr:gl-pixel-format file))))
