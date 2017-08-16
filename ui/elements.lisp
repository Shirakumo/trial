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
  ((text-asset :accessor text-asset)
   (text-offset :initform (vec2 0 0) :accessor text-offset)
   (align :initarg :align :accessor align))
  (:default-initargs
   :text ""
   :text-color (vec 0 0 0 1)
   :font (asset 'trial 'trial::noto-sans)
   :align (list :center :center)))

(defmethod initialize-instance :after ((label label) &key text text-color font)
  (setf (text-asset label) (make-instance 'text :text text :color text-color :font font)))

(defmethod load progn ((label label))
  (load (text-asset label)))

(defmethod offload progn ((label label))
  (offload (text-asset label)))

(defmethod text ((label label))
  (text (text-asset label)))

(defmethod (setf text) (text (label label))
  (setf (text (text-asset label)) text)
  (note-extent-change label NIL))

(defmethod note-extent-change ((label label) (other null))
  (destructuring-bind (halign valign) (align label)
    (let* ((bounds (extent (text-asset label)))
           (x (ecase halign
                (:left 0)
                (:right (- (width label) (getf bounds :r)))
                (:center (/ (- (width label) (getf bounds :r)) 2))))
           (y (ecase valign
                (:bottom 0)
                (:top (- (height label) (getf bounds :t)))
                (:center (/ (- (height label) (getf bounds :t)) 2)))))
      (vsetf (text-offset label) x y))))

(defmethod paint :after ((label label) target)
  (let ((offset (text-offset label)))
    (translate-by (vx2 offset) (vy2 offset) 0)
    (paint (text-asset label) target)))

(define-shader-subject ui-window (flat-ui-element rectangular-pane)
  ())

(defmethod paint :after ((window ui-window) target)
  (loop for e across (children window)
        do (paint e target)))
