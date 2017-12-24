#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.ui)

(define-shader-entity text-field-cursor (vertex-entity)
  ((text-field :initarg :text-field :accessor text-field)
   (text-position :initform 0 :accessor text-position)
   (cursor-size :initform (vec4 0 0 0.5 1) :reader cursor-size))
  (:default-initargs
   :text-field (error "TEXT-FIELD required.")
   :vertex-array (asset 'trial 'fullscreen-square)))

(defmethod initialize-instance :after ((cursor text-field-cursor) &key text-field)
  (let* ((extent (extent (text-asset text-field)))
         (u (getf extent :t))
         (b (getf extent :b)))
    (setf (vy (cursor-size cursor)) 0)
    (setf (vz (cursor-size cursor)) (/ (size (text-asset text-field))
                                       (size (font (text-asset text-field)))
                                       2))
    (setf (vw (cursor-size cursor)) (/ (- u b) 2))))

(defmethod (setf text-position) :around (value (cursor text-field-cursor))
  (call-next-method (min (length (text (text-field cursor))) (max 0 value)) cursor))

(defmethod (setf text-position) :after (value (cursor text-field-cursor))
  (setf (vx (cursor-size cursor))
        (getf (text-extent (text-asset (text-field cursor))
                           (subseq (text (text-field cursor)) 0 (text-position cursor)))
              :r)))

(defmethod paint :before ((cursor text-field-cursor) target)
  (let ((size (cursor-size cursor)))
    (translate-by (vx size) (+ (vy size) (vw size)) 0)
    (scale-by (vz size) (vw size) 1)))

(define-class-shader (text-field-cursor :fragment-shader)
  "out vec4 color;

void main(){
    color = vec4(0,0,0,1);
}")

(define-shader-entity text-field (highlightable-widget text-element control)
  ((cursor :accessor cursor))
  (:default-initargs
   :align (list :left :center)))

(defmethod initialize-instance :after ((text-field text-field) &key)
  (setf (cursor text-field) (make-instance 'text-field-cursor :text-field text-field))
  (setf (text-position (cursor text-field)) (length (text text-field))))

(defmethod register-object-for-pass :after (pass (text-field text-field))
  (register-object-for-pass pass (cursor text-field)))

(defmethod paint :after ((text-field text-field) target)
  (paint (cursor text-field) target))

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

(defmethod handle ((event key-release) (text-field text-field))
  (let ((key (key event))
        (cursor (cursor text-field)))
    (with-accessors ((text-position text-position)) cursor
      (case key
        (:backspace
         (when (<= 1 text-position (length (text text-field)))
           (setf (text text-field) (string-remove-pos (text text-field) (1- text-position)))
           (decf text-position)))
        (:delete
         (when (< -1 text-position (length (text text-field)))
           (setf (text text-field) (string-remove-pos (text text-field) text-position))))
        (:left
         (decf text-position))
        (:right
         (incf text-position))
        (:home
         (setf text-position 0))
        (:end
         (setf text-position (length (text text-field))))))))

(defmethod handle ((event text-entered) (text-field text-field))
  (setf (text text-field) (string-insert-pos (text text-field) (text-position (cursor text-field)) (text event)))
  (incf (text-position (cursor text-field)) (length (text event))))
