#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.ui)

(define-shader-entity flat-widget (vertex-entity colored-entity widget bakable)
  ((background-color :initarg :background-color :accessor background-color))
  (:default-initargs
   :background-color (vec 0.5 0.5 0.5 1)
   :vertex-array NIL))

(defmethod shared-initialize :after ((widget flat-widget) slots &key extent background-color)
  (when background-color
    (setf (color widget) background-color))
  (when extent
    (note-extent-change widget NIL)))

(defmethod bake ((widget flat-widget))
  (note-extent-change widget NIL))

(defmethod note-extent-change :after ((widget flat-widget) (other null))
  (finalize (vertex-array widget))
  (setf (vertex-array widget)
        (change-class (make-rectangle (width widget) (height widget) :align :topleft)
                      'vertex-array :load T)))

(defmethod paint :around ((widget widget) target)
  (with-pushed-matrix (((model-matrix) :identity))
    (translate (vxy_ (extent widget)))
    (call-next-method)))

(define-shader-entity highlightable-widget (flat-widget control)
  ((highlight-color :initarg :highlight-color :accessor highlight-color)
   (active-color :initarg :active-color :accessor active-color))
  (:default-initargs
   :highlight-color (vec 0.8 0.8 0.8 1)
   :active-color (vec 1.0 1.0 1.0 1)))

(defmethod (setf status) :after (value (widget highlightable-widget))
  (setf (color widget) (ecase value
                         (:background (background-color widget))
                         (:highlighted (highlight-color widget))
                         (:active (active-color widget)))))

(define-shader-entity bordered-widget (flat-widget)
  ((border-size :initarg :border-size :accessor border-size)
   (border-color :initarg :border-color :accessor border-color))
  (:default-initargs
   :border-size 1
   :border-color (vec4 0.3 0.3 0.3 1)))

(defclass spacer (inactive-element)
  ())

(defmethod note-extent-change ((widget spacer) (other null)))

(define-shader-entity text-element ()
  ((text-asset :accessor text-asset)
   (text-offset :initform (vec2 0 0) :accessor text-offset)
   (align :initarg :align :accessor align))
  (:default-initargs
   :text ""
   :text-color (vec 0 0 0 1)
   :font (asset 'trial 'trial::noto-sans)
   :align (list :center :center)))

(defmethod initialize-instance :after ((text-element text-element) &key text text-color font)
  (setf (text-asset text-element) (make-instance 'text :text text :color text-color :font font)))

(defmethod text ((text-element text-element))
  (text (text-asset text-element)))

(defmethod (setf text) (text (text-element text-element))
  (setf (text (text-asset text-element)) text)
  (note-extent-change text-element NIL))

(defmethod note-extent-change ((text-element text-element) (other null))
  (destructuring-bind (halign valign) (align text-element)
    (let* ((bounds (extent (text-asset text-element)))
           (x (ecase halign
                (:left (getf bounds :l))
                (:right (- (+ (width text-element) (getf bounds :l)) (getf bounds :r)))
                (:center (/ (- (+ (width text-element) (getf bounds :l)) (getf bounds :r)) 2))))
           (y (ecase valign
                (:bottom (getf bounds :b))
                (:top (- (+ (height text-element) (getf bounds :b)) (getf bounds :t)))
                (:center (/ (- (+ (height text-element) (getf bounds :b)) (getf bounds :t)) 2)))))
      (vsetf (text-offset text-element) x y))))

(defmethod paint :after ((text-element text-element) target)
  (let ((offset (text-offset text-element)))
    (translate-by (vx2 offset) (vy2 offset) 0)
    (paint (text-asset text-element) target)))

(define-shader-entity label (flat-widget text-element inactive-element)
  ())
