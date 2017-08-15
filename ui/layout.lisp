#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.ui)

(defclass layout ()
  ())

(defclass horizontal-layout ()
  ((alignment :initarg :alignment :initform :center :accessor alignment)))

(defmethod apply-layout ((layout horizontal-layout) (pane pane))
  (with-vec4 (left top total-width total-height) (extent pane)
    (let* ((children (children pane))
           (alignment (alignment layout))
           (scale (/ 1 (loop for e across children sum (preferred-width e)))))
      (loop for e across children
            for x = left then (+ x w)
            for w = (* scale (preferred-width e) total-width)
            for h = (*       (preferred-height e) total-height)
            do (vsetf (the vec4 (extent e))
                      x
                      (ecase alignment
                        (:top top)
                        (:center (+ top (/ (- total-height h) 2)))
                        (:bottom (+ top (- total-height h))))
                      w
                      h)
               (note-extent-change e NIL)))))

(defclass vertical-layout ()
  ((alignment :initarg :alignment :initform :center :accessor alignment)))

(defmethod apply-layout ((layout vertical-layout) (pane pane))
  (with-vec4 (left top total-width total-height) (extent pane)
    (let* ((children (children pane))
           (alignment (alignment layout))
           (scale (/ 1 (loop for e across children sum (preferred-height e)))))
      (loop for e across children
            for y = top then (+ y h)
            for w = (*       (preferred-width e) total-width)
            for h = (* scale (preferred-height e) total-height)
            do (vsetf (the vec4 (extent e))
                      (ecase alignment
                        (:left left)
                        (:center (+ left (/ (- total-width w) 2)))
                        (:right (+ left (- total-width w))))
                      y
                      w
                      h)
               (note-extent-change e NIL)))))
