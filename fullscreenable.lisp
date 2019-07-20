#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass fullscreenable (display)
  ((original-mode :initform NIL :accessor original-mode)
   (resolution :initarg :resolution :accessor resolution)
   (fullscreen :initarg :fullscreen :accessor fullscreen))
  (:default-initargs
   :resolution (list 800 600)
   :fullscreen NIL))

(defmethod start :after ((fullscreenable fullscreenable))
  (cl-monitors:init)
  (setf (original-mode fullscreenable)
        (cl-monitors:mode
         (dolist (monitor (cl-monitors:detect))
           (when (cl-monitors:primary-p monitor)
             (return monitor)))))
  (setf (resolution fullscreenable) (resolution fullscreenable))
  (setf (fullscreen fullscreenable) (fullscreen fullscreenable)))

(defmethod stop :after ((fullscreenable fullscreenable))
  (cl-monitors:deinit))

(defmethod finalize :after ((fullscreenable fullscreenable))
  (setf (resolution fullscreenable) NIL))

(defmethod (setf resolution) :before (resolution (fullscreenable fullscreenable))
  (etypecase resolution
    (null
     (cl-monitors:make-current (original-mode fullscreenable)))
    (list
     (resize (context fullscreenable)
             (first resolution)
             (second resolution)))
    (cl-monitors:mode
     (resize (context fullscreenable)
             (cl-monitors:width resolution)
             (cl-monitors:height resolution))
     (cl-monitors:make-current resolution))))

(defmethod (setf fullscreen) :before (fullscreen (fullscreenable fullscreenable))
  (show (context fullscreenable) :fullscreen fullscreen))
