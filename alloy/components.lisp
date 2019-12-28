#|
 This file is a part of trial
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.alloy)

(defclass vec2 (alloy:structure)
  ())

(defmethod initialize-instance :after ((structure vec2) &key object layout-parent focus-parent)
  (let ((layout (make-instance 'alloy:grid-layout :col-sizes '(10 T 10 T) :row-sizes '(20) :layout-parent layout-parent))
        (focus (make-instance 'alloy:focus-list :focus-parent focus-parent)))
    (alloy:enter "X" layout)
    (alloy:represent (3d-vectors:vx2 object) 'alloy:wheel :layout-parent layout :focus-parent focus)
    (alloy:enter "Y" layout)
    (alloy:represent (3d-vectors:vy2 object) 'alloy:wheel :layout-parent layout :focus-parent focus)
    (alloy:finish-structure structure layout focus)))

(defmethod alloy:component-class-for-object ((_ 3d-vectors:vec2)) (find-class 'vec2))

(defclass vec3 (alloy:structure)
  ())

(defmethod initialize-instance :after ((structure vec3) &key object layout-parent focus-parent)
  (let ((layout (make-instance 'alloy:grid-layout :col-sizes '(10 T 10 T 10 T) :row-sizes '(20) :layout-parent layout-parent))
        (focus (make-instance 'alloy:focus-list :focus-parent focus-parent)))
    (alloy:enter "X" layout)
    (alloy:represent (3d-vectors:vx3 object) 'alloy:wheel :layout-parent layout :focus-parent focus)
    (alloy:enter "Y" layout)
    (alloy:represent (3d-vectors:vy3 object) 'alloy:wheel :layout-parent layout :focus-parent focus)
    (alloy:enter "Z" layout)
    (alloy:represent (3d-vectors:vz3 object) 'alloy:wheel :layout-parent layout :focus-parent focus)
    (alloy:finish-structure structure layout focus)))

(defmethod alloy:component-class-for-object ((_ 3d-vectors:vec3)) (find-class 'vec3))

(defclass vec4 (alloy:structure)
  ())

(defmethod initialize-instance :after ((structure vec4) &key object layout-parent focus-parent)
  (let ((layout (make-instance 'alloy:grid-layout :col-sizes '(10 T 10 T 10 T 10 T) :row-sizes '(20) :layout-parent layout-parent))
        (focus (make-instance 'alloy:focus-list :focus-parent focus-parent)))
    (alloy:enter "X" layout)
    (alloy:represent (3d-vectors:vx4 object) 'alloy:wheel :layout-parent layout :focus-parent focus)
    (alloy:enter "Y" layout)
    (alloy:represent (3d-vectors:vy4 object) 'alloy:wheel :layout-parent layout :focus-parent focus)
    (alloy:enter "Z" layout)
    (alloy:represent (3d-vectors:vz4 object) 'alloy:wheel :layout-parent layout :focus-parent focus)
    (alloy:enter "W" layout)
    (alloy:represent (3d-vectors:vw4 object) 'alloy:wheel :layout-parent layout :focus-parent focus)
    (alloy:finish-structure structure layout focus)))

(defmethod alloy:component-class-for-object ((_ 3d-vectors:vec4)) (find-class 'vec4))
