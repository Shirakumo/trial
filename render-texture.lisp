#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass render-texture (pipeline)
  ((width :initarg :width :accessor width)
   (height :initarg :height :accessor height))
  (:default-initargs
   :width (error "WIDTH required.")
   :height (error "HEIGHT required.")))

(defmethod pack ((render-texture render-texture))
  (pack-pipeline render-texture render-texture))

(defmethod texture ((render-texture render-texture))
  (let ((pass (aref (passes render-texture) (1- (length (passes render-texture))))))
    (texture (find :color-attachment0 (flow:ports pass)
                   :key #'attachment))))

(defmethod paint ((entity entity) (render-texture render-texture))
  (gl:viewport 0 0 (width render-texture) (height render-texture))
  (register-object-for-pass render-texture entity)
  (paint render-texture entity))
