#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass scene-buffer (pipeline scene)
  ((render-pass :initform (make-instance 'render-pass) :accessor render-pass)
   (width :initarg :width :accessor width)
   (height :initarg :height :accessor height))
  (:default-initargs
   :width (error "WIDTH required.")
   :height (error "HEIGHT required.")))

(defmethod initialize-instance :after ((buffer scene-buffer) &key)
  (register (render-pass buffer) buffer)
  (pack buffer))

(defmethod pack ((buffer scene-buffer))
  (pack-pipeline buffer buffer))

(defmethod texture ((buffer scene-buffer))
  (let ((pass (aref (passes buffer) (1- (length (passes buffer))))))
    (texture (find :color-attachment0 (flow:ports pass)
                   :key #'attachment))))

(defmethod enter :after ((subject shader-subject) (buffer scene-buffer))
  (register-object-for-pass buffer subject))

(defmethod paint :before ((buffer scene-buffer) (target scene-buffer))
  (gl:viewport 0 0 (width target) (height target)))

(defmethod paint ((pass shader-pass) (buffer scene-buffer))
  (for:for ((object over buffer))
    (paint object pass)))
