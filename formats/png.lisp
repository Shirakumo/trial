#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defmethod load-image (source (type (eql :png)) &key)
  (let ((png (etypecase source
               ((or string pathname)
                (pngload:load-file source :flatten T :flip-y T))
               (vector
                (pngload:load-vector source :flatten T :flip-y T))
               (memory-region
                ;; FIXME: upstream this
                (let ((png (pngload::make-png))
                      (state (pngload::make-state :decode-data T
                                                  :flatten T
                                                  :flip-y T
                                                  :use-static-vector NIL
                                                  :unknown-chunk-warnings NIL)))
                  (setf (pngload::state png) state)
                  (3bz:with-octet-pointer (pointer-binding (memory-region-pointer source)
                                                           (memory-region-size source))
                    (let ((source (make-instance 'pngload::octet-pointer-source
                                                 :data (memory-region-pointer source)
                                                 :end (memory-region-size source))))
                      (setf (pngload::state-source state) source
                            (pngload::state-mmap-pointer state) pointer-binding
                            (pngload::parse-tree png) (pngload::parse-datastream png))))
                  png)))))
    (make-image-source (pngload:data png) (pngload:width png) (pngload:height png)
                       (infer-pixel-type (pngload:bit-depth png) :unsigned)
                       (ecase (pngload:color-type png)
                         (:greyscale :red)
                         (:greyscale-alpha :rg)
                         (:truecolour :rgb)
                         (:truecolour-alpha :rgba)
                         (:indexed-colour :rgb)))))
