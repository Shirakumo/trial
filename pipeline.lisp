#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defclass pipeline ()
  ((passes :initarg :passes :initform () :accessor passes)))

(defmethod register ((pass shader-pass) (pipeline pipeline))
  )

(defmethod deregister ((pass shader-pass) (pipeline pipeline))
  )

(defmethod connect-pass (source-pass source-output target-pass target-input pipeline)
  )

(defmethod pack-pipeline ((pipeline pipeline))
  )

(defmethod paint ((pipeline pipeline) target)
  )
