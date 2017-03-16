#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:trial-editor-tools
  (:nicknames #:org.shirakumo.fraf.trial.editor.tools)
  (:use #:cl+qt :trial :3d-vectors)
  (:shadowing-import-from #:flare #:slot)
  (:export #:tools))
(in-package #:org.shirakumo.fraf.trial.editor.tools)
(in-readtable :qtools)

(define-subject toolkit (trial::global-selection-buffer)
  ((mode :initform :move :accessor mode)))

(defmethod (setf selected) :after ((entity located-entity) (toolkit toolkit))
  (when (eql (mode toolkit) :move)
    (enter (make-instance 'move-arrow :direction +vx+ :location (v+ (location entity))) (event-loop toolkit))
    (enter (make-instance 'move-arrow :direction +vy+ :location (v+ (location entity))) (event-loop toolkit))
    (enter (make-instance 'move-arrow :direction +vz+ :location (v+ (location entity))) (event-loop toolkit))))

(define-subject move-arrow (draggable-entity cube colored-entity located-entity)
  ((direction :initarg :direction :accessor direction))
  (:default-initargs
   :direction +vx+))

(defmethod drag ((move-arrow move-arrow) from to)
  (let* ((loc (vec->main (location move-arrow) (window :main)))
         (end (vec->main (v+ (location move-arrow) (direction move-arrow)) (window :main)))
         (dir (nvunit (v- end loc)))
         (pos (vlength (nv* dir (v. (v- to loc) dir)))))
    (nv+ (location move-arrow) (v* (direction move-arrow) pos))))
