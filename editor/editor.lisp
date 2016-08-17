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

(define-widget subwindow (QMdiSubWindow)
  ())

(define-override (subwindow close-event) (ev)
  (q+:hide subwindow)
  (q+:ignore ev))

(define-widget game-view (QMdiSubWindow subwindow)
  ())

(define-override (game-view resize-event) (ev)
  (q+:resize-event (q+:widget game-view) ev)
  (stop-overriding))

(define-widget editor (QMainWindow trial:unsavable trial:container-unit trial:window)
  ((main :initarg :main :accessor main)
   (file :initform NIL :accessor file))
  (:default-initargs
    :main (error "MAIN required.")))

(define-subwidget (editor area) (q+:make-qmdiarea editor)
  (setf (q+:central-widget editor) area))

(define-subwidget (editor game-view) (make-instance 'game-view)
  (setf (q+:widget game-view) main)
  (setf (parent main) game-view)
  (setf (q+:window-title game-view) "Game View")
  (q+:add-sub-window area game-view))

(define-subwidget (editor console) (make-instance 'subwindow)
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

;; (define-subwidget (editor toolkit) (make-instance 'trial-editor-tools::toolkit)
;;   (trial:enter toolkit editor))

(define-initializer (editor setup)
  (setf (q+:window-title editor) "Trial Editor")
  (trial:enter editor (trial:scene main)))

(define-finalizer (editor teardown)
  (trial:leave editor (trial:scene main)))

(define-override (editor close-event) (ev)
  (setf (parent main) NIL)
  (q+:accept ev)
  (q+:hide editor)
  (finalize editor))

(define-slot (editor update) ()
  (declare (connected updater (timeout)))
  (update status-memory)
  (update status-scene))

(defun editor-save (editor &optional query)
  (flet ((save ()
           (trial::with-body-in-scene ((trial::scene (main editor)) :return-values NIL)
             (trial::save-scene (trial::scene (main editor)) (file editor)))))
    (if (or query (not (file editor)))
        (with-finalizing ((dialog (q+:make-qfiledialog)))
          (when (file editor)
            (setf (q+:directory dialog) (uiop:native-namestring (pathname-utils:to-directory (file editor))))
            (q+:select-file dialog (pathname-utils:file-name (file editor))))
          (setf (q+:default-suffix dialog) "sav")
          (setf (q+:accept-mode dialog) (q+:qfiledialog.accept-save))
          (when (q+:exec dialog)
            (setf (file editor) (first (q+:selected-files dialog)))
            (save)))
        (save))))

(defun editor-open (editor)
  (with-finalizing ((dialog (q+:make-qfiledialog)))
    (when (file editor)
      (setf (q+:directory dialog) (uiop:native-namestring (pathname-utils:to-directory (file editor))))
      (q+:select-file dialog (pathname-utils:file-name (file editor))))
    (setf (q+:default-suffix dialog) "sav")
    (setf (q+:accept-mode dialog) (q+:qfiledialog.accept-open))
    (when (q+:exec dialog)
      (setf (file editor) (first (q+:selected-files dialog)))
      (trial::with-body-in-scene ((trial::scene (main editor)) :return-values NIL)
        (trial::load-scene (trial::scene (main editor)) (file editor))))))

(define-menu (editor File)
  (:item ("Open..." (ctrl o))
         (editor-open editor))
  (:item ("Save" (ctrl s))
         (editor-save editor))
  (:item ("Save As..." (ctrl alt s))
         (editor-save editor T))
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
