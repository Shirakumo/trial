(in-package #:org.shirakumo.fraf.trial)

(define-global +powersave-timer+ 0.0d0)
#+darwin
(progn
  (define-global +mac-sleep-reason-name+ NIL)
  (define-global +mac-power-id+ 0))

(defun prevent-powersave ()
  (ignore-errors
   (v:info :trial.power "Preventing powersaving.")
   (setf +powersave-timer+ -100d0)
   #+nx
   (cffi:foreign-funcall "nxgl_prevent_powersave")
   #+darwin
   (progn
     (unless +mac-sleep-reason-name+
       (setf +mac-sleep-reason-name+ (org.shirakumo.fraf.gamepad.impl::cfstr "TrialGameRunning")))
     (setf +mac-power-id+ 0))
   ;; FIXME: Implement this for Linux via DBUS messaging.
   ))

(defun ping-powersave (tt)
  (let ((tt (float tt 0d0)))
    (when (< (+ 10d0 +powersave-timer+) tt)
      (setf +powersave-timer+ tt)
      #+nx
      (cffi:foreign-funcall "nxgl_ping_powersave")
      #+windows
      (cffi:foreign-funcall "SetThreadExecutionState"
                            :uint #x80000003 :int)
      #+darwin
      (when +mac-sleep-reason-name+
        (cffi:with-foreign-object (id :uint32)
          (setf (cffi:mem-ref id :uint32) +mac-power-id+)
          (cffi:foreign-funcall "IOPMAssertionDeclareUserActivity"
                                :pointer +mac-sleep-reason-name+
                                :uint 0
                                :pointer id
                                :int)
          (setf +mac-power-id+ (cffi:mem-ref id :uint32)))))))

(defun restore-powersave ()
  (ignore-errors
   (v:info :trial.power "Restoring powersaving.")
   #+nx
   (cffi:foreign-funcall "nxgl_restore_powersave")
   #+windows
   (cffi:foreign-funcall "SetThreadExecutionState"
                         :uint #x80000000 :int)
   #+darwin
   (setf +mac-power-id+ 0)))
