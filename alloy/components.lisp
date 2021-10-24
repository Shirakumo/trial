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

(defmacro define-set-representation (name &body value-set)
  (form-fiddle:with-body-options (body options item-text represents) value-set
    (declare (ignore options))
    (let ((item (trial::mksym *package* name '-item)))
      `(progn
         (defclass ,item (alloy:combo-item)
           ())

         (defmethod alloy:text ((item ,item))
           (let ((alloy:value (alloy:value item)))
             ,item-text))

         (defclass ,name (alloy:combo)
           ())

         (defmethod alloy:value-set ((,name ,name))
           ,@body)

         (defmethod alloy:text ((,name ,name))
           (let ((alloy:value (alloy:value ,name)))
             ,item-text))

         (defmethod alloy:combo-item (item (,name ,name))
           (make-instance ',item :value item))

         ,(when represents
            `(defmethod alloy:component-class-for-object ((_ ,represents))
               (find-class ',name)))))))

(define-set-representation asset
  :represents trial:asset
  :item-text (format NIL "~a / ~a"
                     (trial:name (trial:pool alloy:value))
                     (trial:name alloy:value))
  (let ((type (if (alloy:value asset) (type-of (alloy:value asset)) 'trial:asset)))
    (loop for pool in (trial:list-pools)
          nconc (loop for asset in (trial:list-assets pool)
                      when (typep asset type)
                      collect asset))))

(defmethod (setf alloy:value) :before ((value trial:asset) (asset asset))
  (trial:load value))

(define-set-representation resource
  :represents trial:resource
  :item-text (cond ((trial:generator alloy:value)
                    (format NIL "~a / ~a~@[ / ~a~]"
                            (trial:name (trial:pool (trial:generator alloy:value)))
                            (trial:name (trial:generator alloy:value))
                            (unless (eq T (trial:name alloy:value))
                              (trial:name alloy:value))))
                   ((trial:name alloy:value)
                    (format NIL "~a" (trial:name alloy:value)))
                   (T
                    (format NIL "<~a>" (type-of alloy:value))))
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

(define-set-representation monitor
  :represents trial:monitor
  :item-text trial:name
  (trial:list-monitors trial:*context*))

(defclass video-mode-item (alloy:combo-item)
  ())

(defmethod alloy:text ((item video-mode-item))
  (destructuring-bind (w h &optional r monitor) (alloy:value item)
    (declare (ignore monitor))
    (format nil "~a x ~a~@[ @ ~aHz~]" w h r)))

(defclass video-mode (alloy:combo)
  ((monitor :initarg :monitor :initform trial:*context* :accessor monitor)))

(defmethod (setf monitor) :after (monitor (mode video-mode))
  (alloy:notify-observers 'value-set mode monitor mode))

(defmethod alloy:value-set ((video-mode video-mode))
  (trial:list-video-modes (trial:monitor video-mode)))

(defmethod alloy:text ((video-mode video-mode))
  (destructuring-bind (w h &optional r monitor) (alloy:value video-mode)
    (declare (ignore monitor))
    (format nil "~a x ~a~@[ @ ~aHz~]" w h r)))

(defmethod alloy:combo-item (item (video-mode video-mode))
  (make-instance 'video-mode-item :value item))
