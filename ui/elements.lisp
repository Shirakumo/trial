#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.ui)

(define-shader-subject flat-ui-element (vertex-subject colored-subject ui-element)
  ()
  (:default-initargs :vertex-array NIL))

(defmethod shared-initialize :after ((ui-element flat-ui-element) slots &key extent)
  (when extent
    (note-extent-change ui-element NIL)))

(defmethod note-extent-change :after ((ui-element flat-ui-element) (other null))
  (finalize (vertex-array ui-element))
  (setf (vertex-array ui-element)
        (change-class (make-rectangle (width ui-element) (height ui-element) :align :topleft)
                      'vertex-array :load T)))

(defmethod paint :around ((ui-element ui-element) target)
  (with-pushed-matrix (((model-matrix) :identity))
    (translate (vxy_ (extent ui-element)))
    (call-next-method)))

(defclass spacer (ui-element)
  ())

(defmethod note-extent-change ((ui-element spacer) (other null)))

(define-shader-subject label (flat-ui-element)
  ((text :initarg :text :accessor text)))

(defmethod note-extent-change ((ui-element label) (other null)))

(define-shader-subject ui-window (flat-ui-element rectangular-pane)
  ())

(defmethod paint :after ((window ui-window) target)
  (loop for e across (children window)
        do (paint e target)))
