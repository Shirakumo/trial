(in-package #:org.shirakumo.fraf.trial)

(define-event scene-changed () scene old-scene)

(defclass main (#+thread-support display
                #-thread-support single-threaded-display
                gamepad-input-handler)
  ((scene :initform (make-instance 'pipelined-scene) :accessor scene)
   (loader :initform (make-instance 'loader) :accessor loader)))

(defmethod initialize-instance :before ((main main) &key)
  (clear-retained)
  (setf +main+ main))

(defmethod start ((main main))
  (cond ((deploy:env-set-p "TRIAL_QUIT_AFTER_INIT")
         (quit (context main)))
        (T
         (call-next-method))))

(defmethod finalize ((main main))
  (v:info :trial.main "RAPTURE")
  (when (context main)
    (acquire-context (context main) :force T))
  (finalize (loader main))
  (finalize (scene main)))

(defmethod finalize :after ((main main))
  (setf +main+ NIL)
  (setf +input-source+ :keyboard)
  (setf +map-key-events+ T))

(defmethod username ((main main))
  (system-username))

(defmethod username ((default (eql T)))
  (if +main+
      (username +main+)
      (system-username)))

(defmethod scene ((default (eql T)))
  (scene +main+))

(defmethod handle (event (main main))
  (when (scene main)
    (issue (scene main) event)))

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
  (let ((scene (scene main)))
    (issue scene 'pre-tick :tt tt :dt dt :fc fc)
    (issue scene 'tick :tt tt :dt dt :fc fc)
    (issue scene 'post-tick :tt tt :dt dt :fc fc)
    (process scene)))

(defmethod commit (thing (main main) &rest args)
  (apply #'commit thing (loader main) args))

(defmethod setup-rendering :after ((main main))
  (restart-case
      (change-scene main (scene main) :old NIL)
    (abort ()
      :report "Don't set up the scene, leaving it empty."
      (unless (scene main)
        (setf (scene main) (make-instance 'scene)))
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

;; FIXME: When transitioning between scenes we should try to re-use existing textures
;;        and fbos to reduce the amount of unnecessary allocation.
(defmethod change-scene ((main main) (new scene) &key (old (scene main)))
  (unless (eq old new)
    (setf (scene main) new)
    (with-cleanup-on-failure (setf (scene main) old)
      (setup-scene main new)
      (with-timing-report (info :trial.main "Commit took ~fs run time, ~fs clock time.")
        (commit new (loader main)))
      (handle (make-event 'scene-changed :scene new :old-scene old) new)))
  (values new old))

(defmethod enter-and-load ((object renderable) (container container) (main main))
  (let ((area (make-instance 'staging-area)))
    (stage object area)
    (enter object container)
    (with-cleanup-on-failure (loop for pass across (passes (scene main))
                                   do (when (object-renderable-p object pass)
                                        (leave object pass))
                                   finally (leave object container))
      (loop for pass across (passes (scene main))
            do (when (object-renderable-p object pass)
                 (loop for program being the hash-keys of (program-table pass)
                       do (stage program area))))
      (commit area (loader main) :unload NIL))
    object))

(defmethod enter-and-load ((object entity) (container container) (main main))
  (let ((area (make-instance 'staging-area)))
    (stage object area)
    (enter object container)
    (with-cleanup-on-failure (leave object container)
      (commit area (loader main) :unload NIL))
    object))

(defmethod render ((source main) (target main))
  (when (visible-p (context target))
    (render (scene source) NIL)
    ;; KLUDGE: This assumes a pipelined scene
    (blit-to-screen (scene source))))

(defmethod launch ((main symbol) &rest initargs)
  #+nx (setf (v:repl-level) :trace)
  (standalone-logging-handler)
  (v:output-here)
  (v:info :trial.main "GENESIS")
  (v:info :trail.main "Launching version ~a (~a)" (version :app) (version :binary))
  (when (deploy:env-set-p "TRIAL_QUIT_AFTER_INIT")
    (let ((context (getf initargs :context)))
      (setf (getf initargs :context) (list* :visible NIL context))))
  (handler-bind ((error #'standalone-error-handler)
                 #+trial-release (style-warning #'muffle-warning))
    (flet ((thunk ()
             (apply #'launch-with-context main initargs)))
      (if (or (find :darwin *features*) (deploy:deployed-p))
          (float-features:with-float-traps-masked T
            (thunk))
          (thunk))))
  (v:info :trial.main "We're done")
  (setf *context* NIL)
  #-nx (tg:gc :full T))
