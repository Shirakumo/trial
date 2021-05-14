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

(flet ((handle (event vec)
         (alloy:do-elements (element vec :result (alloy:decline) :from-end T)
           (when (alloy:contained-p (alloy:location event) (alloy:bounds element))
             (if (alloy:handle event element)
                 (return)
                 (alloy:decline))))))
  (defmethod alloy:handle ((event alloy:pointer-move) (vec vec))
    (handle event vec))
  (defmethod alloy:handle ((event alloy:pointer-down) (vec vec))
    (handle event vec))
  (defmethod alloy:handle ((event alloy:pointer-up) (vec vec))
    (handle event vec)))

(defmethod alloy:value-changed ((vec vec))
  (let ((els '(3d-vectors:vx 3d-vectors:vy 3d-vectors:vz 3d-vectors:vw)))
    (alloy:do-elements (element vec)
      (setf (alloy:value element) (funcall (pop els) (alloy:value vec))))))

(defclass vec2 (vec)
  ())

(defmethod initialize-instance :after ((vec vec2) &key labels (step 0.1))
  (let ((object (alloy:value vec)))
    (setf (alloy:col-sizes vec) (if labels '(20 T 20 T) '(T T)))
    (when labels (alloy:enter (alloy:represent "X" 'alloy:label :style '((:label :halign :middle))) vec))
    (alloy:enter (alloy:represent (3d-vectors:vx2 object) 'alloy:wheel :step step) vec)
    (when labels (alloy:enter (alloy:represent "Y" 'alloy:label :style '((:label :halign :middle))) vec))
    (alloy:enter (alloy:represent (3d-vectors:vy2 object) 'alloy:wheel :step step) vec)))

(defmethod alloy:component-class-for-object ((_ 3d-vectors:vec2)) (find-class 'vec2))

(defclass vec3 (vec)
  ())

(defmethod initialize-instance :after ((vec vec3) &key labels (step 0.1))
  (let ((object (alloy:value vec)))
    (setf (alloy:col-sizes vec) (if labels '(20 T 20 T 20 T) '(T T T)))
    (when labels (alloy:enter (alloy:represent "X" 'alloy:label :style '((:label :halign :middle))) vec))
    (alloy:enter (alloy:represent (3d-vectors:vx3 object) 'alloy:wheel :step step) vec)
    (when labels (alloy:enter (alloy:represent "Y" 'alloy:label :style '((:label :halign :middle))) vec))
    (alloy:enter (alloy:represent (3d-vectors:vy3 object) 'alloy:wheel :step step) vec)
    (when labels (alloy:enter (alloy:represent "Z" 'alloy:label :style '((:label :halign :middle))) vec))
    (alloy:enter (alloy:represent (3d-vectors:vz3 object) 'alloy:wheel :step step) vec)))

(defmethod alloy:component-class-for-object ((_ 3d-vectors:vec3)) (find-class 'vec3))

(defclass vec4 (vec)
  ())

(defmethod initialize-instance :after ((vec vec4) &key labels (step 0.1))
  (let ((object (alloy:value vec)))
    (setf (alloy:col-sizes vec) (if labels '(20 T 20 T 20 T 20 T) '(T T T T)))
    (when labels (alloy:enter (alloy:represent "X" 'alloy:label :style '((:label :halign :middle))) vec))
    (alloy:enter (alloy:represent (3d-vectors:vx4 object) 'alloy:wheel :step step) vec)
    (when labels (alloy:enter (alloy:represent "Y" 'alloy:label :style '((:label :halign :middle))) vec))
    (alloy:enter (alloy:represent (3d-vectors:vy4 object) 'alloy:wheel :step step) vec)
    (when labels (alloy:enter (alloy:represent "Z" 'alloy:label :style '((:label :halign :middle))) vec))
    (alloy:enter (alloy:represent (3d-vectors:vz4 object) 'alloy:wheel :step step) vec)
    (when labels (alloy:enter (alloy:represent "W" 'alloy:label :style '((:label :halign :middle))) vec))
    (alloy:enter (alloy:represent (3d-vectors:vw4 object) 'alloy:wheel :step step) vec)))

(defmethod alloy:component-class-for-object ((_ 3d-vectors:vec4)) (find-class 'vec4))

(defclass asset-item (alloy:combo-item)
  ())

(defmethod alloy:text ((asset asset-item))
  (format NIL "~a / ~a"
          (trial:name (trial:pool (alloy:value asset)))
          (trial:name (alloy:value asset))))

(defclass asset (alloy:combo)
  ())

(defmethod (setf alloy:value) :before ((value trial:asset) (asset asset))
  (trial:load value))

(defmethod alloy:value-set ((asset asset))
  (let ((type (if (alloy:value asset) (type-of (alloy:value asset)) 'trial:asset)))
    (loop for pool in (trial:list-pools)
          nconc (loop for asset in (trial:list-assets pool)
                      when (typep asset type)
                      collect asset))))

(defmethod alloy:text ((asset asset))
  (format NIL "~a / ~a"
          (trial:name (trial:pool (alloy:value asset)))
          (trial:name (alloy:value asset))))

(defmethod alloy:combo-item (item (asset asset))
  (make-instance 'asset-item :value item))

(defmethod alloy:component-class-for-object ((_ trial:asset)) (find-class 'asset))

(defclass resource-item (alloy:combo-item)
  ())

(defmethod alloy:text ((resource resource-item))
  (let ((resource (alloy:value resource)))
    (cond ((trial:generator resource)
           (format NIL "~a / ~a~@[ / ~a~]"
                   (trial:name (trial:pool (trial:generator resource)))
                   (trial:name (trial:generator resource))
                   (unless (eq T (trial:name resource))
                     (trial:name resource))))
          ((trial:name resource)
           (format NIL "~a" (trial:name resource)))
          (T
           (format NIL "<~a>" (type-of resource))))))

(defclass resource (alloy:combo)
  ())

(defmethod alloy:value-set ((resource resource))
  (let ((type (if (alloy:value resource) (type-of (alloy:value resource)) 'trial:resource))
        (values ()))
    (when (eql type 'trial:placeholder-resource)
      (setf type 'trial:resource))
    (flet ((resource< (a b)
             (let ((ag (trial:generator a))
                   (bg (trial:generator b)))
               (if (and ag bg)
                   (if (eq (trial:pool ag) (trial:pool bg))
                       (string< (trial:name ag) (trial:name bg))
                       (string< (trial:name (trial:pool ag)) (trial:name (trial:pool bg))))
                   (string< (trial:name a) (trial:name b))))))
      (dolist (pool (trial:list-pools) (sort values #'resource<))
        (dolist (asset (trial:list-assets pool))
          (dolist (resource (trial:list-resources asset))
            (when (typep resource type)
              (push resource values))))))))

(defmethod alloy:text ((resource resource))
  (call-next-method))

(defmethod alloy:combo-item (item (resource resource))
  (make-instance 'resource-item :value item))

(defmethod alloy:component-class-for-object ((_ trial:resource)) (find-class 'resource))

(defclass video-mode (alloy:combo)
  ())

(defmethod alloy:value-set ((video-mode video-mode))
  (trial:list-video-modes trial:*context*))

(defmethod alloy:combo-item (item (video-mode video-mode))
  (make-instance 'video-mode-item :value item))

(defclass video-mode-item (alloy:combo-item)
  ())

(defmethod alloy:text ((item video-mode-item))
  (destructuring-bind (w h r) (alloy:value item)
    (format NIL "~a x ~a @ ~aHz" w h r)))
