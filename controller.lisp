#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

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

(define-subject controller (hud-entity persistent)
  (;; Has to be a double to avoid bignums after ~3.8 hours of runtime.
   (tick-count :initform 0.0d0 :accessor tick-count)
   (display :initform NIL :accessor display))
  (:default-initargs
   :name :controller))

(defmethod paint ((controller controller) (hud hud))
  (let ((font (get-resource 'font :trial :debug-hud))
        (clock (clock (scene (display controller)))))
    ;; FIXME: Some way to draw text without having to call Qt methods explicitly.
    ;;        Generally some kinda UI framework...
    (q+:render-text (display controller) 20 30 (format NIL "Pause: ~,10f" (last-pause (display controller)))
                    (data font))
    (q+:render-text (display controller) 20 50 (format NIL "FPS:   ~,2f" (actual-fps (display controller)))
                    (data font))
    (q+:render-text (display controller) 20 70 (format NIL "Time:  ~2,'0d:~6,3,,,'0f"
                                           (floor (/ (round clock) 60))
                                           (mod clock 60))
                    (data font))))

(define-handler (controller tick tick 100) (ev)
  (incf (tick-count controller)))

(define-handler (controller mapping T 100) (ev)
  (map-event ev *loop*)
  (retain-event ev))

(define-handler (controller reload-assets reload-assets 99) (ev)
  (dolist (pool (pools))
    (mapc #'reload (assets pool))))

(define-handler (controller reload-scene reload-scene 99) (ev)
  (let ((scene (first (loops controller))))
    (dolist (obj (flare-indexed-set:coerce-set (objects scene) 'list))
      (unless (eql obj controller)
        (leave obj scene)
        (finalize obj))))
  (setup-scene (display controller)))

(define-handler (controller save-game) (ev)
  (save-scene (first (loops controller)) #p"~/test.sav.lisp"))

(define-handler (controller load-game) (ev)
  (load-scene (first (loops controller)) #p"~/test.sav.lisp"))

(define-handler (controller execute-request) (ev)
  (execute ev))

(define-handler (controller launch-editor) (ev)
  (when (asdf:find-system :trial-editor)
    (asdf:load-system :trial-editor))
  (when (find-package '#:org.shirakumo.fraf.trial.editor)
    (with-body-in-gui ((display controller) :return-values NIL)
      (funcall (find-symbol (string '#:launch) '#:org.shirakumo.fraf.trial.editor)
               (display controller)))))
