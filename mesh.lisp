#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(define-subject mesh-subject ()
  ((mesh :initform NIL :accessor mesh)
   (textures :initform () :accessor textures :finalized T)))

(defmethod initialize-instance :after ((subject mesh-subject) &key (mesh NIL t-p) &allow-other-keys)
  (when t-p (setf (mesh subject) mesh)))

(defmethod reinitialize-instance :after ((subject mesh-subject) &key (mesh NIL t-p) &allow-other-keys)
  (when t-p (setf (mesh subject) mesh)))

(defmethod (setf mesh) ((path pathname) (subject mesh-subject))
  (setf (mesh subject) (elt (wavefront-loader::load-obj (resource-pathname path)) 0))
  (prepare subject))

(defmethod (setf mesh) ((path string) (subject mesh-subject))
  (setf (mesh subject) (uiop:parse-native-namestring path)))

(defmethod (setf mesh) ((null null) (subject mesh-subject))
  (setf (slot-value subject 'mesh) NIL))

(defmethod prepare ((subject mesh-subject))
  (let ((diffuse (wavefront-loader::diffuse
                  (wavefront-loader::material
                   (mesh subject)))))
    (when (typep diffuse 'pathname)
      (let ((buffer (load-image-buffer diffuse)))
        (setf (wavefront-loader::diffuse
               (wavefront-loader::material
                (mesh subject)))
              (q+:texture buffer))
        (push buffer (textures subject))))))

(defmethod draw ((subject mesh-subject))
  (wavefront-loader::draw (mesh subject)))
