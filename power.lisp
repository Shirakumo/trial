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

(defun prevent-powersave ()
  (v:info :trial.power "Preventing powersaving.")
  (setf +powersave-timer+ -100.0)
  #+darwin
  (setf +mac-power-id+ 0)
  #+linux
  (unless +X11-display+
    (setf +X11-display+ (xlib:open-default-display))
    (xlib/dpms:dpms-disable +X11-display+)
    (multiple-value-bind (timeout interval blank exposure) (xlib:screen-saver +X11-display+)
      (declare (ignore timeout))
      (xlib:set-screen-saver +X11-display+ 0 interval blank exposure))))

(defun ping-powersave (tt)
  (when (< (+ 10 +powersave-timer+) tt)
    (setf +powersave-timer+ tt)
    #+windows
    (cffi:foreign-funcall "SetThreadExecutionState"
                          :uint #x80000003 :int)
    #+darwin
    (cffi:with-foreign-object (id :uint32)'q
      (setf (cffi:mem-ref id :uint32) +mac-power-id+)
      (cffi:foreign-funcall "IOPMAssertionDeclareUserActivity"
                            :pointer SLEEP-REASON-NAME
                            :uint 0
                            :pointer id
                            :int)
      (setf +mac-power-id+ (cffi:mem-ref id :uint32)))
    #+linux
    (xlib:reset-screen-saver +X11-display+)))

(defun restore-powersave ()
  (v:info :trial.power "Restoring powersaving.")
  #+windows
  (cffi:foreign-funcall "SetThreadExecutionState"
                        :uint #x80000000 :int)
  #+darwin
  (setf +mac-power-id+ 0)
  #+linux
  (when +X11-display+
    (xlib/dpms:dpms-enable +X11-display+)
    (multiple-value-bind (timeout interval blank exposure) (xlib:screen-saver +X11-display+)
      (declare (ignore timeout))
      (xlib:set-screen-saver +X11-display+ -1 interval blank exposure))
    (xlib:close-display +X11-display+)
    (setf +X11-display+ NIL)))
