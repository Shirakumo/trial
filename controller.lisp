#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-action system-action ())

(define-action launch-editor (system-action)
  (key-press (eql key :section)))

(define-action save-game (system-action)
  (key-press (eql key :f2)))

(define-action load-game (system-action)
  (key-press (eql key :f3)))

(define-action reload-assets (system-action)
  (key-press (eql key :f5)))

(define-action reload-scene (system-action)
  (key-press (eql key :f6)))

(define-subject controller (persistent)
  (;; Has to be a double to avoid bignums after ~3.8 hours of runtime.
   (tick-count :initform 0.0d0 :accessor tick-count)
   (display :initform NIL :accessor display))
  (:default-initargs
   :name :controller))

(define-handler (controller resize) (ev width height)
  (let ((pipeline (pipeline (display controller))))
    (when pipeline (resize pipeline width height))))

(define-handler (controller tick tick 100) (ev)
  (incf (tick-count controller)))

(define-handler (controller mapping T 100) (ev)
  (map-event ev *loop*)
  (retain-event ev))

(define-handler (controller reload-assets reload-assets 99) (ev)
  (loop for asset being the hash-keys of (assets *context*)
        do (load (offload asset))))

(define-handler (controller reload-scene reload-scene 99) (ev)
  (loop for asset being the hash-keys of (assets *context*)
        do (offload asset))
  (clear (scene (display controller)))
  (clear (pipeline (display controller)))
  (setup-scene (display controller)))

(define-handler (controller load-request) (ev asset action)
  (ecase action
    (offload (offload asset))
    (load    (load asset))
    (reload  (load (offload asset)))))

(define-handler (controller save-game) (ev)
  (save-scene (event-loop controller) #p"~/test.sav.lisp"))

(define-handler (controller load-game) (ev)
  (load-scene (event-loop controller) #p"~/test.sav.lisp"))

(define-handler (controller key-release) (ev key)
  (when (eql key :escape)
    (issue (event-loop controller) 'pause)))

(defun maybe-reload-scene (&optional (window (or *context* (window :main))))
  (when window
    (issue (scene window) 'reload-scene)))
