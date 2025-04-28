(in-package #:org.shirakumo.fraf.trial)

(defclass gamepad-input-handler ()
  ((last-device-probe :initform 0 :accessor last-device-probe)))

(defmacro with-gamepad-failure-handling ((&key (ignore-error T)) &body body)
  `(catch 'bail-input
     (handler-bind ((gamepad:gamepad-error
                      (lambda (e)
                        (declare (ignore e))
                        (when (find-restart 'gamepad:drop-device)
                          (invoke-restart 'gamepad:drop-device))
                        ,(when ignore-error
                           `(throw 'bail-input NIL)))))
       ,@body)))

(defun describe-gamepad (dev)
  (format NIL "Vendor: ~a Product: ~a Version: ~a Driver: ~a Name: ~a"
          (gamepad:vendor dev) (gamepad:product dev) (gamepad:version dev)
          (gamepad:driver dev) (gamepad:name dev)))

(defmethod start :before ((handler gamepad-input-handler))
  (with-gamepad-failure-handling (:ignore-error #-trial-release NIL #+trial-release T)
    (let ((devices (gamepad:init)))
      (v:info :trial.input "~:[No controllers detected.~;Detected the following controllers:~:*~{~%  ~a~}~]"
              (mapcar #'describe-gamepad devices))
      (dolist (device devices)
        (handle (make-event 'gamepad-added :device device) handler)))))

(defmethod stop :before ((handler gamepad-input-handler))
  (with-gamepad-failure-handling ()
    (gamepad:shutdown)))

(defmethod poll-input :after ((handler gamepad-input-handler))
  (with-gamepad-failure-handling ()
    (labels ((process (event)
               (typecase event
                 (gamepad:button-down
                  (handle (make-event 'gamepad-press
                                      :button (or (gamepad:event-label event)
                                                  (gamepad:event-code event))
                                      :device (gamepad:event-device event))
                          handler))
                 (gamepad:button-up
                  (handle (make-event 'gamepad-release
                                      :button (or (gamepad:event-label event)
                                                  (gamepad:event-code event))
                                      :device (gamepad:event-device event))
                          handler))
                 (gamepad:axis-move
                  (handle (make-event 'gamepad-move
                                      :pos (gamepad:event-value event)
                                      :old-pos (gamepad:event-old-value event)
                                      :axis (or (gamepad:event-label event)
                                                (gamepad:event-code event))
                                      :device (gamepad:event-device event))
                          handler))))
             (poll (device)
               (gamepad:poll-events device #'process)))
      (declare (dynamic-extent #'process #'poll))
      (gamepad:call-with-devices #'poll))
    (when (< internal-time-units-per-second
             (- (get-internal-real-time) (last-device-probe handler)))
      (labels ((process (action device)
                 (ecase action
                   (:add (v:info :trial.input "New controller:~%  ~a" (describe-gamepad device))
                    (handle (make-event 'gamepad-added :device device) handler))
                   (:remove (v:info :trial.input "Lost controller:~%  ~a" (describe-gamepad device))
                    (handle (make-event 'gamepad-removed :device device) handler)))))
        (declare (dynamic-extent #'process))
        (setf (last-device-probe handler) (get-internal-real-time))
        (gamepad:poll-devices :function #'process)))))

(defun maybe-load-gamepad-device-maps (&optional (file (pathname-utils:merge-pathnames*
                                                        "default-device-mappings.lisp"
                                                        (or #+nx (tempdir)
                                                            (if (deploy:deployed-p)
                                                                (self)
                                                                (user-homedir-pathname))))))
  (when (and (deploy:deployed-p) (probe-file file))
    (v:info :trial.input "Loading default device mappings from ~a" file)
    (cl:load file)))
