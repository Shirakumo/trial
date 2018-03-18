#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass scene-buffer (render-texture pipelined-scene)
  ((render-pass :initform (make-instance 'render-pass) :accessor render-pass))
  (:default-initargs
   :width (error "WIDTH required.")
   :height (error "HEIGHT required.")))

(defmethod initialize-instance :after ((buffer scene-buffer) &key)
  (enter (render-pass buffer) buffer)
  (pack buffer))
