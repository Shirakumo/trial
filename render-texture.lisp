#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass render-texture (pipeline)
  ((width :initarg :width :accessor width)
   (height :initarg :height :accessor height)
   (clear-color :initarg :clear-color :accessor clear-color))
  (:default-initargs
   :width (error "WIDTH required.")
   :height (error "HEIGHT required.")
   :clear-color (vec4 0 0 0 0)))

(defmethod pack ((render-texture render-texture))
  (pack-pipeline render-texture render-texture))

(defmethod resize :after ((render-texture render-texture) width height)
  (setf (width render-texture) width)
  (setf (height render-texture) height))

(defmethod texture ((render-texture render-texture))
  (let ((pass (aref (passes render-texture) (1- (length (passes render-texture))))))
    (texture (find :color-attachment0 (flow:ports pass)
                   :key #'attachment))))

(defmethod paint-with :before ((target render-texture) source)
  (gl:viewport 0 0 (width target) (height target))
  (let ((c (clear-color target)))
    (gl:clear-color (vx c) (vy c) (vz c) (if (vec4-p c) (vw c) 0.0))))
