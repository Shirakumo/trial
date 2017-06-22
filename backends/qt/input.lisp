#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.qt)
(in-readtable :qtools)

(define-override (context key-press-event) (ev)
  (unless (q+:is-auto-repeat ev)
    (let ((key (qt-key->symbol (q+:key ev))))
      (v:debug :trial.input "Key pressed: ~a" key)
      (handle (make-instance 'key-press :key key
                                        :text (q+:text ev)
                                        :modifiers (qt-modifiers->list (q+:modifiers ev)))
              (handler context)))))

(define-override (context key-release-event) (ev)
  (unless (q+:is-auto-repeat ev)
    (let ((key (qt-key->symbol (q+:key ev))))
      (v:debug :trial.input "Key released: ~a ~a" key (qt-modifiers->list (q+:modifiers ev)))
      (handle (make-instance 'key-release :key key
                                          :text (q+:text ev)
                                          :modifiers (qt-modifiers->list (q+:modifiers ev)))
              (handler context)))))

(define-override (context mouse-press-event) (ev)
  (let ((button (qt-button->symbol (q+:button ev)))
        (position (vec (q+:x (q+:pos-f ev)) (q+:y (q+:pos-f ev)))))
    (v:debug :trial.input "Mouse pressed: ~a" button)
    (handle (make-instance 'mouse-press :button button :pos position)
            (handler context))))

(define-override (context mouse-release-event) (ev)
  (let ((button (qt-button->symbol (q+:button ev)))
        (position (vec (q+:x (q+:pos-f ev)) (q+:y (q+:pos-f ev)))))
    (v:debug :trial.input "Mouse released: ~a" button)
    (handle (make-instance 'mouse-release :button button :pos position)
            (handler context))))

(define-override (context wheel-event) (ev)
  (let ((delta (/ (q+:delta ev) 8 15))
        (position (vec (q+:x (q+:pos ev)) (q+:y (q+:pos ev)))))
    (v:debug :trial.input "Mouse wheel: ~a" delta)
    (handle (make-instance 'mouse-scroll :delta delta :pos position)
            (handler context))))

(define-override (context mouse-move-event) (ev)
  (let ((position (vec (q+:x (q+:pos-f ev)) (q+:y (q+:pos-f ev)))))
    (handle (make-instance 'mouse-move :old-pos (or previous-pos position) :pos position)
            (handler context))
    (setf previous-pos position)))

(define-override (context focus-in-event) (ev)
  (handle (make-instance 'gain-focus) (handler context)))

(define-override (context focus-out-event) (ev)
  (handle (make-instance 'lose-focus) (handler context)))
