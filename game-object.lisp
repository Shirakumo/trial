#|
 This file is a part of simple-tasks
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defclass game-object (QWidget)
  "Object that appears in the game."
  ((file-path :initarg :path :initform NIL :accessor path)
   (image :initform NIL :accessor image)))

(defmethod initialize-instance :after ((obj game-object) &key)
  "Loads up the sprite data."
  (unless (path obj) (error "Missing file path."))
  (setf (image obj) (q+:make-qimage (path obj)))
  (unless (image obj) (error "Invalid file path.")))
