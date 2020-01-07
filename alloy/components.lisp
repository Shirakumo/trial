#|
 This file is a part of trial
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.alloy)

(defclass vec (alloy:grid-layout
               alloy:focus-list
               alloy:value-component)
  ((alloy::cell-margins :initform (alloy:margins)))
  (:default-initargs :row-sizes '(20)))

(defclass vec2 (vec)
  ()
  (:default-initargs :col-sizes '(20 T 20 T)))

(defmethod initialize-instance :after ((vec vec2) &key)
  (let ((object (alloy:value vec)))
    (alloy:enter (alloy:represent "X" 'alloy:label :style '((:label :halign :middle))) vec)
    (alloy:enter (alloy:represent (3d-vectors:vx2 object) 'alloy:wheel) vec)
    (alloy:enter (alloy:represent "Y" 'alloy:label :style '((:label :halign :middle))) vec)
    (alloy:enter (alloy:represent (3d-vectors:vy2 object) 'alloy:wheel) vec)))

(defmethod alloy:component-class-for-object ((_ 3d-vectors:vec2)) (find-class 'vec2))

(defclass vec3 (vec)
  ()
  (:default-initargs :col-sizes '(20 T 20 T 20 T)))

(defmethod initialize-instance :after ((vec vec3) &key)
  (let ((object (alloy:value vec)))
    (alloy:enter (alloy:represent "X" 'alloy:label :style '((:label :halign :middle))) vec)
    (alloy:enter (alloy:represent (3d-vectors:vx3 object) 'alloy:wheel) vec)
    (alloy:enter (alloy:represent "Y" 'alloy:label :style '((:label :halign :middle))) vec)
    (alloy:enter (alloy:represent (3d-vectors:vy3 object) 'alloy:wheel) vec)
    (alloy:enter (alloy:represent "Z" 'alloy:label :style '((:label :halign :middle))) vec)
    (alloy:enter (alloy:represent (3d-vectors:vz3 object) 'alloy:wheel) vec)))

(defmethod alloy:component-class-for-object ((_ 3d-vectors:vec3)) (find-class 'vec3))

(defclass vec4 (vec)
  ()
  (:default-initargs :col-sizes '(20 T 20 T 20 T 20 T)))

(defmethod initialize-instance :after ((vec vec4) &key)
  (let ((object (alloy:value vec)))
    (alloy:enter (alloy:represent "X" 'alloy:label :style '((:label :halign :middle))) vec)
    (alloy:enter (alloy:represent (3d-vectors:vx4 object) 'alloy:wheel) vec)
    (alloy:enter (alloy:represent "Y" 'alloy:label :style '((:label :halign :middle))) vec)
    (alloy:enter (alloy:represent (3d-vectors:vy4 object) 'alloy:wheel) vec)
    (alloy:enter (alloy:represent "Z" 'alloy:label :style '((:label :halign :middle))) vec)
    (alloy:enter (alloy:represent (3d-vectors:vz4 object) 'alloy:wheel) vec)
    (alloy:enter (alloy:represent "W" 'alloy:label :style '((:label :halign :middle))) vec)
    (alloy:enter (alloy:represent (3d-vectors:vw4 object) 'alloy:wheel) vec)))

(defmethod alloy:component-class-for-object ((_ 3d-vectors:vec4)) (find-class 'vec4))

(defclass asset (alloy:button)
  ())

(defmethod activate :after ((asset asset))
  ())

(defmethod alloy:component-class-for-object ((_ trial:asset)) (find-class 'asset))

