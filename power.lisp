#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-global +powersave-timer+ 0.0d0)
#+darwin
(progn
  (org.shirakumo.fraf.gamepad.impl::define-lazy-constant SLEEP-REASON-NAME
      (org.shirakumo.fraf.gamepad.impl::cfstr "TrialGameRunning"))
  (define-global +mac-power-id+ 0))
#+linux
(progn
  (define-global +X11-display+ NIL))

(defun prevent-powersave (tt)
  (when (< (+ 10 +powersave-timer+) tt)
    (setf +powersave-timer+ tt)
    #+windows
    (cffi:foreign-funcall "SetThreadExecutionState"
                          :uint #x80000003 :int)
    #+darwin
    (cffi:with-foreign-object (id :uint32)
      (setf (cffi:mem-ref id :uint32) +mac-power-id+)
      (cffi:foreign-funcall "IOPMAssertionDeclareUserActivity"
                            :pointer SLEEP-REASON-NAME
                            :uint 0
                            :pointer id
                            :int)
      (setf +mac-power-id+ (cffi:mem-ref id :uint32)))
    #+linux
    (unless +X11-display+
      (setf +X11-display+ (xlib:open-default-display))
      (xlib/dpms:dpms-disable +X11-display+))
    #+linux
    (xlib:reset-screen-saver +X11-display+)))

(defun restore-powersave ()
  #+windows
  (cffi:foreign-funcall "SetThreadExecutionState"
                        :uint #x80000000 :int)
  #+darwin
  (setf +mac-power-id+ 0)
  #+linux
  (when +X11-display+
    (xlib/dpms:dpms-enable +X11-display+)
    (xlib:close-display +X11-display+)
    (setf +X11-display+ NIL)))
