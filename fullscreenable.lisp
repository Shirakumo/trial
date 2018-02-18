#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass fullscreenable (display)
  ((original-mode :initform NIL :accessor original-mode)
   (resolution :accessor resolution)
   (fullscreen :accessor fullscreen))
  (:default-initargs
   :resolution (list 800 600)
   :fullscreen NIL))

(defmethod initialize-instance :after ((fullscreenable fullscreenable) &key resolution fullscreen)
  (setf (original-mode fullscreenable) (cl-monitors:mode
                                (dolist (monitor (cl-monitors:detect))
                                  (when (cl-monitors:primary-p monitor)
                                    (return monitor)))))
  (setf (resolution fullscreenable) resolution)
  (setf (fullscreen fullscreenable) fullscreen))

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

(cl-monitors:init)
