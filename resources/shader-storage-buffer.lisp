#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass shader-storage-buffer (struct-buffer bindable-buffer)
  ((buffer-type :initform :shader-storage-buffer)))

(defmethod shared-initialize :after ((buffer shader-storage-buffer) slots &key)
  (unless (slot-boundp buffer 'binding)
    (setf (binding buffer) (cffi:translate-underscore-separated-name
                            (class-name (class-of (struct buffer)))))))

(defmethod binding-target ((buffer shader-storage-buffer)) :shader-storage-buffer)

(defmethod allocate :after ((buffer shader-storage-buffer))
  (unless (binding-point buffer) (setf (binding-point buffer) T)))

(defmethod bind ((buffer shader-storage-buffer) (program shader-program))
  ;; TODO: Once we can do shared/packed, load offsets here.
  (load buffer)
  ;; Bind the buffer to the program's specified binding point.
  (%gl:shader-storage-block-binding
   (gl-name program)
   (cffi:with-foreign-string (var (gl-type buffer))
     (%gl:get-program-resource-index (gl-name program) :shader-storage-block var))
   (binding-point buffer)))
