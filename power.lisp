(in-package #:org.shirakumo.fraf.trial)

(define-global +powersave-timer+ 0.0d0)
#+darwin
(progn
  (define-global +mac-sleep-reason-name+ NIL)
  (define-global +mac-power-id+ 0))

(defun prevent-powersave ()
  #-nx
  (ignore-errors
   (v:info :trial.power "Preventing powersaving.")
   (setf +powersave-timer+ -100.0)
   #+darwin
   (progn
     (unless +mac-sleep-reason-name+
       (setf +mac-sleep-reason-name+ (org.shirakumo.fraf.gamepad.impl::cfstr "TrialGameRunning")))
     (setf +mac-power-id+ 0))
   #+linux
   (uiop:run-program (list "xset" "s" "off" "-dpms") :ignore-error-status T)))

(defun ping-powersave (tt)
  #-nx
  (when (< (+ 10 +powersave-timer+) tt)
    (setf +powersave-timer+ tt)
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
        (setf +mac-power-id+ (cffi:mem-ref id :uint32))))))

(defun restore-powersave ()
  #-nx
  (ignore-errors
   (v:info :trial.power "Restoring powersaving.")
   #+windows
   (cffi:foreign-funcall "SetThreadExecutionState"
                         :uint #x80000000 :int)
   #+darwin
   (setf +mac-power-id+ 0)))
