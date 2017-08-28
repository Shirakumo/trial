#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.ui)

(defclass pane (widget array-container)
  ((layout :initform NIL :initarg :layout :accessor layout)))

(defmethod shared-initialize :after ((pane pane) slots &key (children () c-p))
  (when c-p
    (dolist (child children)
      (enter child pane))
    (when (layout pane)
      (apply-layout (layout pane) pane))))

(defmethod (setf layout) :after (layout (pane pane))
  (apply-layout (layout pane) pane))

(defmethod note-extent-change ((pane pane) (child widget))
  (apply-layout (layout pane) pane))

(defmethod note-extent-change ((pane pane) (other null))
  (apply-layout (layout pane) pane))

(defmethod register-object-for-pass :after (pass (pane pane))
  (loop for e across (objects pane)
        do (register-object-for-pass pass e)))

(defmethod enter :after ((widget widget) (pane pane))
  (setf (parent widget) pane))

(defmethod leave :after ((widget widget) (pane pane))
  (setf (parent widget) NIL))

(defclass inactive-element (widget)
  ())

(defmethod (setf focus) (value (widget inactive-element))
  NIL)

(defvar *ui-layer* 0)

(defclass complex-pane (pane)
  ())

(defmethod paint :around ((pane complex-pane) target)
  (with-pushed-attribs ()
    (let ((*ui-layer* (1+ *ui-layer*)))
      (enable :stencil-test)
      (call-next-method))))

(defmethod paint :before ((pane complex-pane) target)
  (gl:stencil-mask #xFF)
  (gl:stencil-func :always *ui-layer* #xFF)
  (gl:stencil-op :keep :keep :replace))

(defmethod paint :after ((pane complex-pane) target)
  (gl:stencil-func :equal *ui-layer* #xFF)
  (gl:stencil-op :keep :keep :keep)
  (loop for e across (objects pane)
        do (paint e target)))

(defclass rectangular-pane (pane)
  ())

(defmethod paint :around ((pane rectangular-pane) target)
  (with-pushed-attribs ()
    (let ((prev (gl:get-integer :scissor-box 4)))
      (enable :scissor-test)
      (with-vec4 (x y w h) (extent pane)
        (gl:scissor x y w h))
      (call-next-method)
      (gl:scissor (aref prev 0) (aref prev 1) (aref prev 2) (aref prev 3)))))
