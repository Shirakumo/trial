#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass scene-buffer (render-texture scene)
  ((render-pass :initform (make-instance 'render-pass) :accessor render-pass))
  (:default-initargs
   :width (error "WIDTH required.")
   :height (error "HEIGHT required.")))

(defmethod initialize-instance :after ((buffer scene-buffer) &key)
  (register (render-pass buffer) buffer)
  (pack buffer))

(defmethod enter :after ((subject shader-subject) (buffer scene-buffer))
  (register-object-for-pass buffer subject))

(defmethod paint ((pass shader-pass) (buffer scene-buffer))
  (for:for ((object over buffer))
    (paint object pass)))
