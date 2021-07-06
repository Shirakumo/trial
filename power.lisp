#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

#+darwin
(progn
  (org.shirakumo.fraf.gamepad.impl::define-lazy-constant PREVENT-SLEEP-NAME
      (org.shirakumo.fraf.gamepad.impl::cfstr "PreventUserIdleDisplaySleep"))
  (org.shirakumo.fraf.gamepad.impl::define-lazy-constant SLEEP-REASON-NAME
      (org.shirakumo.fraf.gamepad.impl::cfstr "TrialGameRunning"))
  (define-global +mac-power-id+ NIL))
#+linux
(progn
  (define-global +X11-display+ NIL)
  (define-global +powersave-timer+ 0.0d0))

(defun prevent-powersave (tt)
  (declare (ignorable tt))
  #+windows
  (cffi:foreign-funcall "SetThreadExecutionState"
                        :int #x80000003 :int)
  #+darwin
  (unless +mac-power-id+
    (cffi:with-foreign-object (id :uint32)
      (cffi:foreign-funcall "IOPMAssertionCreateWithName"
                            :pointer PREVENT-SLEEP-NAME
                            :uint32 255
                            :pointer SLEEP-REASON-NAME
                            :pointer id
                            :int)
      (setf +mac-power-id+ (cffi:mem-ref id :uint32))))
  #+linux
  (unless +X11-display+
    (setf +X11-display+ (cffi:foreign-funcall "XOpenDisplay" :pointer (cffi:null-pointer) :pointer)))
  (when (< (+ 10 +powersave-timer+) tt)
    (cffi:foreign-funcall "XResetScreenSaver" :pointer +X11-display+ :int)
    (setf +powersave-timer+ tt)))

(defun restore-powersave ()
  #+windows
  (cffi:foreign-funcall "SetThreadExecutionState"
                        :int #x80000000 :int)
  #+darwin
  (when +mac-power-id+
    (cffi:foreign-funcall "IOPMAssertionRelease"
                          :uint32 +mac-power-id+
                          :int)
    (setf +mac-power-id+ NIL))
  #+linux
  (when +X11-display+
    (cffi:foreign-funcall "XCloseDisplay" :pointer +X11-display+ :void)
    (setf +X11-display+ NIL)))
