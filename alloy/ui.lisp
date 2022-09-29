#|
 This file is a part of trial
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.alloy)

(defclass event-bridge ()
  ())

(defmacro define-event-translator (trial-type alloy-type &body args)
  `(defmethod trial:handle ((ev ,trial-type) (bridge event-bridge))
     (alloy:handle (make-instance ',alloy-type ,@args) bridge)))

(defun vec->point (vec)
  (alloy:px-point (3d-vectors:vx vec) (3d-vectors:vy vec)))

(define-event-translator trial:mouse-move alloy:pointer-move
  :old-location (vec->point (trial:old-pos ev))
  :location (vec->point (trial:pos ev)))

(define-event-translator trial:mouse-press alloy:pointer-down
  :kind (trial:button ev)
  :location (vec->point (trial:pos ev)))

(define-event-translator trial:mouse-release alloy:pointer-up
  :kind (trial:button ev)
  :location (vec->point (trial:pos ev)))

(define-event-translator trial:mouse-scroll alloy:scroll
  :dy (trial:delta ev)
  :dx 0
  :location (vec->point (trial:pos ev)))

(define-event-translator trial:key-press alloy:key-down
  :modifiers (trial:modifiers ev)
  :key (trial:key ev)
  :code 0)

(define-event-translator trial:key-release alloy:key-up
  :modifiers (trial:modifiers ev)
  :key (trial:key ev)
  :code 0)

(define-event-translator trial:gamepad-press alloy:button-down
  :device (trial:device ev)
  :button (trial:button ev))

(define-event-translator trial:gamepad-release alloy:button-up
  :device (trial:device ev)
  :button (trial:button ev))

(defmethod trial:handle ((ev trial:text-entered) (bridge event-bridge))
  (when (trial:replace-p ev)
    (alloy:handle (make-instance 'alloy:reset-event) bridge))
  (alloy:handle (make-instance 'alloy:text-event :text (trial:text ev)) bridge))

(defclass ui (renderer event-bridge alloy:ui trial:entity)
  ((first-hold-time :initform (cons NIL 0.0) :accessor first-hold-time)))

(defmethod org.shirakumo.alloy.renderers.opengl.msdf:fontcache-directory ((ui ui))
  (trial:pool-path 'trial:trial "font-cache/"))

(defmethod trial:render ((ui ui) target)
  (alloy:render ui ui))

(defmethod trial:register :after ((ui ui) (loop trial:event-loop))
  (trial:add-listener ui loop))

(defmethod trial:handle ((ev trial:event) (ui ui)))

(defmethod trial:handle ((ev trial:tick) (ui ui))
  (animation:update ui (float (trial:dt ev) 0f0)))

(defmethod trial:handle ((ev trial:resize) (ui ui))
  (alloy:suggest-size (alloy:px-size (trial:width ev) (trial:height ev)) ui))

(defmethod trial:stage ((ui ui) (area trial:staging-area))
  (trial:stage (alloy:layout-tree ui) area))

(defmethod alloy:clipboard ((ui ui))
  (trial:clipboard trial:*context*))

(defmethod (setf alloy:clipboard) (value (ui ui))
  (setf (trial:clipboard trial:*context*) value))

(defmethod trial:handle :after ((ev trial:key-release) (ui ui))
  (case (trial:key ev)
    (:insert
     (when (find :shift (trial:modifiers ev))
       (alloy:handle (make-instance 'alloy:paste-event :content (alloy:clipboard ui)) ui)))
    (:v
     (when (find :control (trial:modifiers ev))
       (alloy:handle (make-instance 'alloy:paste-event :content (alloy:clipboard ui)) ui)))
    (:c
     (when (find :control (trial:modifiers ev))
       (alloy:handle (make-instance 'alloy:copy-event) ui)))
    (:x
     (when (find :control (trial:modifiers ev))
       (alloy:handle (make-instance 'alloy:cut-event) ui)))))

(trial:define-shader-pass ui-pass (ui)
  ((name :initform 'ui)
   (trial:color :port-type trial:output :attachment :color-attachment0)
   (trial:depth :port-type trial:output :attachment :depth-stencil-attachment)))

(defmethod render :around ((pass ui-pass) target)
  (trial:with-pushed-attribs
    (gl:enable :depth-test)
    (gl:clear-color 0 0 0 0)
    (call-next-method)))

;; KLUDGE: No idea why this is necessary, fuck me.
(defmethod simple:request-font :around ((pass ui-pass) font &key)
  (let ((font (call-next-method)))
    (unless (and (alloy:allocated-p font)
                 (trial:allocated-p (org.shirakumo.alloy.renderers.opengl.msdf:atlas font)))
      (trial:commit font (trial:loader trial:+main+) :unload NIL))
    font))

(defmethod trial:object-renderable-p ((renderable trial:renderable) (pass ui-pass)) NIL)

(trial:define-shader-pass base-ui (ui-pass
                                   alloy:fixed-scaling-ui
                                   presentations:default-look-and-feel)
  ((alloy:target-resolution :initform (alloy:px-size 1280 720))
   (alloy:scales :initform '((3840 T 2.0)
                             (2800 T 1.5)
                             (1920 T 1.25)
                             (1280 T 1.0)
                             (1000 T 0.8)
                             (T T 0.5)))))
