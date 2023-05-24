#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass main (display gamepad-input-handler)
  ((scene :initform (make-instance 'pipelined-scene) :accessor scene)
   (loader :initform (make-instance 'loader) :accessor loader)))

(defmethod initialize-instance :before ((main main) &key)
  (clear-retained)
  (setf +main+ main))

(defmethod initialize-instance :after ((main main) &key)
  (when (string= "true" (uiop:getenv "TRIAL_QUIT_AFTER_INIT"))
    (quit (context main))))

(defmethod finalize ((main main))
  (v:info :trial.main "RAPTURE")
  (when (context main)
    (acquire-context (context main) :force T))
  (finalize (loader main))
  (finalize (scene main)))

(defmethod finalize :after ((main main))
  (setf +main+ NIL)
  (setf +input-source+ :keyboard))

#+windows
(cffi:define-foreign-library secur32
  (T (:default "Secur32")))

(flet ((fallback-username ()
         (or
          #+windows
          (cffi:with-foreign-objects ((size :ulong)
                                      (name :uint16 128))
            (unless (cffi:foreign-library-loaded-p 'secur32)
              (cffi:load-foreign-library 'secur32))
            (setf (cffi:mem-ref size :ulong) 128)
            ;; Constant 3 here specifies a "display name".
            (cond ((< 0 (cffi:foreign-funcall "GetUserNameExW" :int 13 :pointer name :pointer size :int))
                   (org.shirakumo.com-on:wstring->string name (cffi:mem-ref size :ulong)))
                  (T
                   (setf (cffi:mem-ref size :ulong) 128)
                   (when (< 0 (cffi:foreign-funcall "GetUserNameW" :pointer name :pointer size :int))
                     (org.shirakumo.com-on:wstring->string name (cffi:mem-ref size :ulong))))))
          #+unix
          (cffi:foreign-funcall "getlogin" :string)
          (pathname-utils:directory-name (user-homedir-pathname)))))
  (defmethod username ((main main))
    (fallback-username))

  (defmethod username ((default (eql T)))
    (if +main+
        (username +main+)
        (fallback-username))))

(defmethod scene ((default (eql T)))
  (scene +main+))

(defmethod handle (event (main main))
  (issue (scene main) event))

(defmethod handle :before (event (main main))
  (typecase event
    ((or mouse-event keyboard-event)
     (setf +input-source+ :keyboard))
    (gamepad-move
     (when (< 0.1 (pos event))
       (setf +input-source+ (device event))))
    (gamepad-press
     (setf +input-source+ (device event)))))

(defmethod handle ((window window-close) (main main))
  (quit (context main)))

(defmethod update ((main main) tt dt fc)
  (issue (scene main) 'tick :tt tt :dt dt :fc fc)
  (process (scene main)))

(defmethod commit (thing (main main) &rest args)
  (apply #'commit thing (loader main) args))

(defmethod setup-rendering :after ((main main))
  (restart-case
      (change-scene main (scene main) :old NIL)
    (abort ()
      :report "Don't set up the scene, leaving it empty."
      (clear (scene main)))))

(defmethod setup-scene :around ((main main) (scene scene))
  (v:info :trial.main "Setting up ~a" scene)
  (with-timing-report (info :trial.main "Scene setup took ~fs run time, ~fs clock time.")
    (call-next-method))
  ;; Cause camera to refresh
  (issue scene 'resize :width (width main) :height (height main))
  scene)

(defmethod setup-scene ((main main) scene)
  ())

(defmethod setup-scene :after ((main main) (scene scene))
  (unless (node :controller scene)
    (enter (make-instance 'controller) scene)))

(defmethod change-scene ((main main) (new scene) &key (old (scene main)))
  (unless (eq old new)
    (setup-scene main new)
    (with-timing-report (info :trial.main "Commit took ~fs run time, ~fs clock time.")
      (when (commit new (loader main))
        (setf (scene main) new))))
  (values new old))

(defmethod enter-and-load ((object renderable) (container container) (main main))
  (let ((area (make-instance 'staging-area)))
    (stage object area)
    (enter object container)
    (loop for pass across (passes (scene main))
          do (when (object-renderable-p object pass)
               (loop for program being the hash-keys of (program-table pass)
                     do (stage program area))))
    (unless (commit area (loader main) :unload NIL)
      (loop for pass across (passes (scene main))
            do (when (object-renderable-p object pass)
                 (leave object pass)))
      (leave object container))
    object))

(defmethod enter-and-load ((object entity) (container container) (main main))
  (let ((area (make-instance 'staging-area)))
    (stage object area)
    (enter object container)
    (unless (commit area (loader main) :unload NIL)
      (leave object container))
    object))

(defmethod render ((source main) (target main))
  (render (scene source) NIL)
  ;; KLUDGE: This assumes a pipelined scene
  (when (visible-p (context target))
    (blit-to-screen (scene source))))

(defmethod launch ((main symbol) &rest initargs)
  (standalone-logging-handler)
  (v:output-here)
  (v:info :trial.main "GENESIS")
  (v:info :trail.main "Launching version ~a" (version :app))
  (when (string= "true" (uiop:getenv "TRIAL_QUIT_AFTER_INIT"))
    (let ((context (getf initargs :context)))
      (setf (getf initargs :context) (list* :visible NIL context))))
  (handler-bind ((error #'standalone-error-handler))
    (flet ((thunk ()
             (apply #'launch-with-context main initargs)))
      (if (or (find :darwin *features*) (deploy:deployed-p))
          (float-features:with-float-traps-masked T
            (thunk))
          (thunk))))
  (setf *context* NIL)
  (tg:gc :full T))
