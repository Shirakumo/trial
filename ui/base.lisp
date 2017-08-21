#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.ui)

(defclass ui-element (entity)
  ((focused-p :initform NIL :accessor focused-p)
   (active-p :initform NIL :accessor active-p)
   (parent :initform NIL :initarg :parent :accessor parent)
   (extent :initform (vec4 0 0 0 0) :initarg :extent :accessor extent)
   (preferred-size :initform (vec2 1 1) :initarg :preferred-size :accessor preferred-size)
   (visible-p :initform T :initarg :visible-p :accessor visible-p)))

(defmethod (setf visible-p) :after ((visibility null) (ui-element ui-element))
  (note-extent-change ui-element ui-element))

(defmethod (setf active-p) :after ((active null) (ui-element ui-element))
  (when (focused-p ui-element)
    (setf (focused-p ui-element) NIL)))

(defmethod (setf active-p) :after ((active (eql T)) (ui-element ui-element))
  (unless (focused-p ui-element)
    (setf (focused-p ui-element) T)))

(defmethod (setf extent) :after (extent (ui-element ui-element))
  (note-extent-change ui-element ui-element))

(defmethod width ((ui-element ui-element))
  (vz4 (extent ui-element)))

(defmethod (setf width) (value (ui-element ui-element))
  (prog1 (setf (vz4 (extent ui-element)) value)
    (note-extent-change ui-element ui-element)))

(defmethod height ((ui-element ui-element))
  (vw4 (extent ui-element)))

(defmethod (setf height) (value (ui-element ui-element))
  (prog1 (setf (vw4 (extent ui-element)) value)
    (note-extent-change ui-element ui-element)))

(defmethod preferred-width ((ui-element ui-element))
  (vx2 (preferred-size ui-element)))

(defmethod (setf preferred-width) (value (ui-element ui-element))
  (prog1 (setf (vx2 (preferred-size ui-element)) value)
    (note-extent-change ui-element ui-element)))

(defmethod preferred-height ((ui-element ui-element))
  (vy2 (preferred-size ui-element)))

(defmethod (setf preferred-height) (value (ui-element ui-element))
  (prog1 (setf (vy2 (preferred-size ui-element)) value)
    (note-extent-change ui-element ui-element)))

(defmethod note-extent-change ((ui-element ui-element) (other ui-element))
  (when (parent ui-element)
    (note-extent-change (parent ui-element) ui-element)))

(defclass pane (ui-element)
  ((children :initform (make-array 0 :adjustable T :fill-pointer 0) :accessor children)
   (layout :initform NIL :initarg :layout :accessor layout)))

(defmethod shared-initialize :after ((pane pane) slots &key (children () c-p))
  (when c-p
    (adjust-array (children pane) (length children))
    (loop for i from 0
          for child in children
          do (setf (aref (children pane) i) child)
             (setf (parent child) pane))
    (setf (fill-pointer (children pane)) (length children))
    (when (layout pane)
      (apply-layout (layout pane) pane))))

(defmethod (setf focused-p) :after ((value null) (pane pane))
  (loop for child across (children pane)
        do (setf (focused-p child) NIL)))

(defmethod (setf layout) :after (layout (pane pane))
  (apply-layout (layout pane) pane))

(defmethod insert-child ((child ui-element) (pane pane) &key position)
  (if position
      (array-utils:vector-push-extend-position child (children pane) position)
      (vector-push-extend child (children pane)))
  (setf (parent child) pane))

(defmethod insert-child :after ((child ui-element) (pane pane) &key)
  (apply-layout (layout pane) pane))

(defmethod enter ((child ui-element) (pane pane))
  (insert-child child pane))

(defmethod remove-child ((child ui-element) (pane pane))
  (array-utils:vector-pop-position (children pane) (position child (children pane))))

(defmethod remove-child ((position integer) (pane pane))
  (array-utils:vector-pop-position (children pane) position))

(defmethod remove-child :after ((child ui-element) (pane pane))
  (apply-layout (layout pane) pane))

(defmethod leave ((child ui-element) (pane pane))
  (remove-child child pane))

(defmethod paint ((pane pane) target)
  (loop for e across (children pane)
        do (paint e target)))

(defmethod note-extent-change ((pane pane) (child ui-element))
  (apply-layout (layout pane) pane))

(defmethod note-extent-change ((pane pane) (other null))
  (apply-layout (layout pane) pane))

(defmethod load progn ((pane pane))
  (map NIL #'load (children pane)))

(defmethod offload progn ((pane pane))
  (map NIL #'offload (children pane)))

(defmethod finalize ((pane pane))
  (map NIL #'finalize (children pane)))

(defmethod register-object-for-pass :after (pass (pane pane))
  (loop for e across (children pane)
        do (register-object-for-pass pass e)))

(defclass inactive-element (ui-element)
  ())

(defmethod (setf focused-p) (value (ui-element inactive-element))
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
  (loop for e across (children pane)
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
