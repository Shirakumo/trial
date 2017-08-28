#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.ui)

(define-shader-entity text-field (highlightable-widget text-element control)
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
       (setf (cursor text-field) (length (vtext text-field)))))))

(defmethod handle ((event text-entered) (text-field text-field))
  (setf (vtext text-field) (string-insert-pos (vtext text-field) (cursor text-field) (text event)))
  (incf (cursor text-field) (length (text event))))
