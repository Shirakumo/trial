#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.ui)

(define-shader-entity flat-ui-element (vertex-entity colored-entity ui-element)
  ()
  (:default-initargs
   :color (vec 0.5 0.5 0.5 1)
   :vertex-array NIL))

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

(defclass spacer (inactive-element)
  ())

(defmethod note-extent-change ((ui-element spacer) (other null)))

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

(defmethod load progn ((text-element text-element))
  (load (text-asset text-element)))

(defmethod offload progn ((text-element text-element))
  (offload (text-asset text-element)))

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

(define-shader-entity label (flat-ui-element text-element inactive-element)
  ())

(define-shader-entity text-field (flat-ui-element text-element)
  ((cursor :initform 0 :accessor cursor)
   (vtext :initarg :text :accessor vtext))
  (:default-initargs
   :align (list :left :center)))

(defun string-remove-pos (string pos)
  (let ((new (make-array (1- (length string)) :element-type 'character)))
    (replace new string :end1 pos)
    (replace new string :start1 pos :start2 (1+ pos))
    new))

(defun string-insert-pos (string pos stuff)
  (let ((new (make-array (+ (length string) (length stuff)) :element-type 'character)))
    (replace new string :end1 pos)
    (replace new stuff :start1 pos)
    (replace new string :start1 (+ pos (length stuff)) :start2 pos)
    new))

;; This is fucking stupid. Do something more sensible.
(defmethod (setf cursor) :after (pos (text-field text-field))
  (setf (text text-field) (string-insert-pos (vtext text-field) pos "|")))

(defmethod handle ((event key-release) (text-field text-field))
  (let ((key (key event)))
    (case key
      (:backspace
       (when (<= 1 (cursor text-field) (length (text text-field)))
         (setf (vtext text-field) (string-remove-pos (vtext text-field) (1- (cursor text-field))))
         (decf (cursor text-field))))
      (:delete
       (when (< -1 (cursor text-field) (length (vtext text-field)))
         (setf (vtext text-field) (string-remove-pos (vtext text-field) (cursor text-field)))
         (setf (cursor text-field) (cursor text-field))))
      (:left
       (when (< 0 (cursor text-field))
         (decf (cursor text-field))))
      (:right
       (when (< (cursor text-field) (length (vtext text-field)))
         (incf (cursor text-field))))
      (:home
       (setf (cursor text-field) 0))
      (:end
       (setf (cursor text-field) (length (vtext text-field))))
      ((:left-control :right-control :left-alt :right-alt :left-shift :right-shift :shift))
      (T (cond ((find :left-control (modifiers event))
                (case key
                  (:c)
                  (:v)
                  (:x)
                  (:k
                   (setf (vtext text-field) "")
                   (setf (cursor text-field) 0))))
               (T
                (setf (vtext text-field) (string-insert-pos (vtext text-field) (cursor text-field) (text event)))
                (incf (cursor text-field) (length (text event)))))))))

(define-shader-entity ui-window (flat-ui-element rectangular-pane)
  ())

(defmethod paint :after ((window ui-window) target)
  (loop for e across (children window)
        do (paint e target)))
