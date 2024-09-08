(in-package #:org.shirakumo.fraf.trial.alloy)

(defclass event-bridge ()
  ())

(defmacro define-event-translator (trial-type alloy-type &body args)
  `(defmethod trial:handle ((ev ,trial-type) (bridge event-bridge))
     (alloy:handle (make-instance ',alloy-type ,@args) bridge)))

(defun vec->point (vec)
  (alloy:px-point (math:vx vec) (math:vy vec)))

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

(define-event-translator trial:file-drop-event alloy:drop-event
  :location (vec->point (trial:pos ev))
  :paths (trial:paths ev))

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
  (alloy:suggest-size (alloy:px-size (trial:width ev) (trial:height ev)) ui)
  (loop for framebuffer across (framebuffers ui)
        do (trial:resize framebuffer (trial:width ev) (trial:height ev))))

(defmethod trial:stage :after ((ui ui) (area trial:staging-area))
  (trial:stage (alloy:layout-tree ui) area))

(defmethod alloy:clipboard ((ui ui))
  (when trial:*context*
    (trial:clipboard trial:*context*)))

(defmethod (setf alloy:clipboard) (value (ui ui))
  (when trial:*context*
    (setf (trial:clipboard trial:*context*) value)))

(defmethod alloy:cursor ((ui ui))
  (when trial:*context*
    (trial:cursor trial:*context*)))

(defmethod (setf alloy:cursor) (value (ui ui))
  (when trial:*context*
    (setf (trial:cursor trial:*context*) value)))

(defmethod alloy:key-text (key (ui ui))
  (trial:local-key-string trial:*context* key))

(defmethod trial:handle :after ((ev trial:key-release) (ui ui))
  (case (trial:key ev)
    (:insert
     (when (find :shift (trial:modifiers ev))
       (alloy:handle (make-instance 'alloy:paste-event :content (alloy:clipboard ui)) ui)))
    (T
     (when trial:*context*
       (case (char (or (trial:local-key-string trial:*context* (trial:key ev)) " ") 0)
         (#\v
          (when (find :control (trial:modifiers ev))
            (alloy:handle (make-instance 'alloy:paste-event :content (alloy:clipboard ui)) ui)))
         (#\c
          (when (find :control (trial:modifiers ev))
            (alloy:handle (make-instance 'alloy:copy-event) ui)))
         (#\x
          (when (find :control (trial:modifiers ev))
            (alloy:handle (make-instance 'alloy:cut-event) ui))))))))

(trial:define-shader-pass ui-pass (ui)
  ((trial:name :initform 'ui)
   (trial:color :port-type trial:output :attachment :color-attachment0)
   (trial:depth :port-type trial:output :attachment :depth-stencil-attachment)))

(defmethod trial:enter (thing (pass ui-pass)))
(defmethod trial:leave (thing (pass ui-pass)))

(defmethod trial:stage :after ((pass ui-pass) (area trial:staging-area))
  (trial:stage (trial:framebuffer pass) area))

(defmethod trial:render :around ((pass ui-pass) target)
  (trial:activate (trial:framebuffer pass))
  (trial:with-pushed-features
    (trial:enable-feature :depth-test :stencil-test)
    (trial:disable-feature :cull-face)
    (call-next-method)))

;; KLUDGE: No idea why this is necessary, fuck me.
(defmethod simple:request-font :around ((pass ui-pass) font &key)
  (let ((font (call-next-method)))
    (unless (and (alloy:allocated-p font)
                 (trial:allocated-p (org.shirakumo.alloy.renderers.opengl.msdf:atlas font)))
      (trial:commit font (trial:loader trial:+main+) :unload NIL))
    font))

(defmethod trial:object-renderable-p ((renderable trial:renderable) (pass ui-pass)) NIL)

(defun ui (&optional (scene T))
  (when trial:+main+
    (trial:node 'ui scene)))

(defmethod (setf alloy:focus) :after (value (text alloy:text-input-component))
  (case value
    ((NIL :weak)
     (trial:clear-retained)
     (setf trial:+map-key-events+ T))
    (:strong
     (setf trial:+map-key-events+ NIL))))

(trial:define-setting-observer font :display :font ()
  (when (ui) (alloy:refresh (ui))))

(trial:define-setting-observer ui-scale :display :ui-scale ()
  (when (ui) (alloy:refresh (ui))))

(trial:define-language-change-hook ui ()
  (when (ui) (alloy:refresh (ui))))
