(defpackage #:org.shirakumo.fraf.trial.fbx
  (:use #:cl+trial)
  (:shadow #:asset)
  (:local-nicknames
   (#:fbx #:org.shirakumo.fraf.fbx))
  (:export
   #:asset))
(in-package #:org.shirakumo.fraf.trial.fbx)

(defclass asset (file-input-asset
                 multi-resource-asset
                 animation-asset
                 trial::full-load-asset)
  ())

(defmethod generate-resources ((asset asset) input &key)
  (fbx:with-freeing ((axes (make-instance 'fbx:coordinate-axes
                                          :right :positive-x
                                          :up :positive-y
                                          :front :negative-z))
                     (file (fbx:parse input :generate-missing-normals T
                                            :normalize-normals T
                                            :target-axes axes)))
    ;; FIXME: do this some day.
    ))
