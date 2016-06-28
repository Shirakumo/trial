#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defclass fullscreenable (QWidget)
  ((original-mode :initform NIL :accessor original-mode)
   (resolution :accessor resolution)
   (fullscreen :accessor fullscreen))
  (:default-initargs
   :resolution (list 800 600)
   :fullscreen NIL))

(defmethod initialize-instance :after ((widget fullscreenable) &key resolution fullscreen)
  (setf (original-mode widget) (cl-monitors:mode (cl-monitors:monitor resolution)))
  (setf (resolution widget) resolution)
  (setf (fullscreen widget) fullscreen))

(define-finalizer (fullscreenable restore-monitor)
  (setf (resolution fullscreenable) NIL))

(defmethod (setf resolution) :before (resolution (widget fullscreenable))
  (etypecase resolution
    (null
     (cl-monitors:make-current (original-mode fullscreenable)))
    (list
     (setf (q+:fixed-size widget) (values (first resolution)
                                          (second resolution))))
    (cl-monitors:mode
     (setf (q+:fixed-size widget) (values (cl-monitors:width resolution)
                                          (cl-monitors:height resolution)))
     (cl-monitors:make-current resolution))))

(defmethod (setf fullscreen) :before (fullscreen (widget fullscreenable))
  (if fullscreen
      (q+:show-full-screen widget)
      (q+:show-normal widget)))

(cl-monitors:init)
(pushnew #'cl-monitors:deinit qtools:*build-hooks*)
(pushnew #'cl-monitors:init qtools:*boot-hooks*)
(pushnew #'cl-monitors:deinit qtools:*quit-hooks*)
