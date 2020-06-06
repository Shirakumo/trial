#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass font (single-resource-asset file-input-asset)
  ())

(defmethod generate-resources ((font font) input &key (charset *default-charset*) (index 0) :size 24 :oversample NIL :width 64 :height 64)
  (ensure-instance (resource font T) 'font-atlas
                   :file input
                   :charset charset
                   :index index
                   :size size
                   :oversample oversample
                   :width width
                   :height height))
