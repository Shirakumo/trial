(defpackage #:org.shirakumo.fraf.trial.fbx
  (:use #:cl+trial)
  (:shadow #:asset)
  (:local-nicknames
   (#:fbx #:org.shirakumo.fraf.fbx))
  (:export))
(in-package #:org.shirakumo.fraf.trial.fbx)

(defmethod load-model (input (type (eql :fbx)) &key generator (model (make-instance 'model)))
  (fbx:with-freeing ((axes (make-instance 'fbx:coordinate-axes
                                          :right :positive-x
                                          :up :positive-y
                                          :front :negative-z))
                     (file (fbx:parse input :generate-missing-normals T
                                            :normalize-normals T
                                            :target-axes axes)))
    ;; FIXME: do this some day.
    (implement!)))
