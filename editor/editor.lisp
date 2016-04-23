#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:trial-editor
  (:nicknames #:org.shirakumo.fraf.trial.editor)
  (:use #:cl+qt)
  (:export #:editor #:launch))
(in-package #:org.shirakumo.fraf.trial.editor)
(in-readtable :qtools)

(define-widget status-label (QLabel)
  ())

(define-initializer (status-label setup)
  (update status-label))

(defmethod update :around ((label status-label))
  (setf (q+:text label) (call-next-method)))

(define-widget memory-label (QLabel status-label)
  ())

(defun allocated-mbs ()
  #+sbcl (/ (sb-kernel:dynamic-usage) 1024 1024)
  #-sbcl :unknown)

(defun available-mbs ()
  #+sbcl (/ (sb-ext:dynamic-space-size) 1024 1024)
  #-sbcl :unknown)

(defun mem-percentage ()
  (let ((allocated (allocated-mbs))
        (available (available-mbs)))
    (if (and (numberp allocated) (numberp available))
        (round (* 100 (/ (allocated-mbs) (available-mbs))))
        :unknown)))

(defmethod update ((label memory-label))
  (format NIL "Memory: ~,2f / ~,2f mb (~d%)"
          (allocated-mbs) (available-mbs) (mem-percentage)))

(define-widget scene-label (QLabel status-label)
  ((main :initarg :main :accessor main)))

(defmethod update ((label scene-label))
  (let ((scene (trial:scene (main label))))
    (format NIL "Scene: ~a ~2,'0d:~6,3,,,'0f"
            (if (flare:running scene) :running :stopped)
            (floor (/ (round (flare:clock scene)) 60))
            (mod (flare:clock scene) 60))))

(define-widget editor (QMainWindow)
  ((main :initarg :main :accessor main))
  (:default-initargs
    :main (error "MAIN required.")))

(define-subwidget (editor area) (q+:make-qmdiarea editor)
  (setf (q+:central-widget editor) area))

(define-subwidget (editor game-view) (q+:make-qmdisubwindow editor)
  (setf (q+:widget game-view) main)
  (setf (parent main) game-view)
  (setf (q+:window-title game-view) "Game View")
  (q+:add-sub-window area game-view))

(define-subwidget (editor console) (q+:make-qmdisubwindow editor)
  (setf (q+:widget console) (make-instance 'trial-editor-console:console :main main))
  (setf (q+:window-title console) "Console")
  (q+:add-sub-window area console))

(define-subwidget (editor status-memory) (make-instance 'memory-label))

(define-subwidget (editor status-scene) (make-instance 'scene-label :main main))

(define-subwidget (editor status) (q+:make-qstatusbar editor)
  (setf (q+:status-bar editor) status)
  (q+:add-widget status status-memory)
  (q+:add-widget status status-scene))

(define-subwidget (editor updater) (q+:make-qtimer editor)
  (setf (q+:single-shot updater) NIL)
  (q+:start updater 500))

(define-initializer (editor setup)
  (setf (q+:window-title editor) "Trial Editor"))

(define-override (editor close-event) (ev)
  (setf (parent main) NIL)
  (q+:accept ev)
  (q+:hide editor)
  (finalize editor))

(define-slot (editor update) ()
  (declare (connected updater (timeout)))
  (update status-memory)
  (update status-scene))

(define-menu (editor File)
  (:item ("Open..." (ctrl o)))
  (:item ("Save" (ctrl s)))
  (:item ("Save As..." (ctrl alt s)))
  (:separator)
  (:item "Exit Editor"
         (q+:close editor))
  (:item "Quit"
         (q+:close main)
         (q+:close editor)))

(define-menu (editor View)
  (:item "Asset Browser")
  (:item "Object Tree")
  (:item "Game View"
         (setf (q+:visible game-view) (not (q+:is-visible game-view))))
  (:item "Console"
         (setf (q+:visible console) (not (q+:is-visible console)))))

(defun launch (main)
  (q+:show (make-instance 'editor :main main)))
