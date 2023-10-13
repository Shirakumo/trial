(in-package #:org.shirakumo.fraf.trial)

(defclass staging-area ()
  ((staged :initform (make-hash-table :test 'eq) :reader staged)))

(defgeneric dependencies (object))
(defgeneric stage (object staging-area))
(defgeneric unstage (object staging-area))
(defgeneric mark-dependent (dependency object staging-area))

(defmethod dependencies (object) ())

(defmethod stage :around (object (area staging-area))
  (unless (gethash object (staged area))
    (call-next-method)))

(defmethod stage :before (object (area staging-area))
  (dolist (dependency (dependencies object))
    (mark-dependent dependency object area)))

(defmethod stage :after ((container container) (area staging-area))
  (for:for ((child over container))
    (stage child area)))

(defmethod deallocate ((area staging-area))
  (loop for object being the hash-keys of (staged area)
        do (deallocate object)
           (remhash object area)))

(defclass loader ()
  ((loaded :initform (make-hash-table :test 'eq) :reader loaded)))

(defgeneric commit (staging-area loader &key unload))
(defgeneric abort-commit (loader))
(defgeneric progress (loader so-far total))

(defmethod finalize ((loader loader))
  (loop for resource being the hash-keys of (loaded loader) using (hash-value status)
        do (case status
             ((:to-unload :to-keep :loaded :allocated)
              (deallocate resource))))
  (clrhash (loaded loader)))

(defmethod progress ((loader loader) so-far total))
