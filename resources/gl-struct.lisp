#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defvar *gl-structs* (make-hash-table :test 'eq))

(defclass gl-declaration ()
  ((name :initarg :name :accessor name)
   (gl-name :initarg :gl-name :accessor gl-name)
   (gl-type :initarg :type :accessor gl-type)
   (qualifiers :initarg :qualifiers :accessor qualifiers)
   (layout :initarg :layout :accessor layout))
  (:default-initargs
   :name (error "NAME required.")
   :type (error "TYPE required.")
   :qualifiers ()
   :layout NIL))

(defmethod initialize-instance :after ((gl-declaration gl-declaration) &key name gl-name)
  (unless gl-name
    (setf (gl-name gl-declaration) (cffi:translate-underscore-separated-name name))))

(defmethod print-object ((gl-declaration gl-declaration) stream)
  (print-unreadable-object (gl-declaration stream :type T)
    (format stream "~s" (name gl-declaration))))

(defmethod gl-source ((gl-declaration gl-declaration))
  `(glsl-toolkit:struct-declarator
    (glsl-toolkit:type-qualifier
     ,@(when (layout gl-declaration)
         `((glsl-toolkit:layout-qualifier
            ,@(loop for id in (enlist (layout gl-declaration))
                    collect `(glsl-toolkit:layout-qualifier-id ,@(enlist id))))))
     ,@(qualifiers gl-declaration))
    (glsl-toolkit:type-specifier
     ,(let ((type (gl-type gl-declaration)))
        (if (and (listp type) (eq (first type) :struct))
            (gl-type (gl-struct (second type)))
            type)))
    ,(gl-name gl-declaration)))

(defclass gl-struct-field (gl-declaration)
  ((array-size :initarg :count :initarg :array-size :accessor array-size))
  (:default-initargs
   :type (error "TYPE required.")
   :array-size NIL))

(defmethod gl-source ((gl-struct-field gl-struct-field))
  (let ((source (call-next-method gl-struct-field)))
    (when (array-size gl-struct-field)
      (setf (third source) (append (third source) `((glsl-toolkit:array-specifier ,(array-size gl-struct-field))))))
    source))

(defclass gl-struct ()
  ((name :initarg :name :accessor name)
   (gl-type :initarg :type :accessor gl-type)
   (fields :initarg :fields :accessor fields))
  (:default-initargs
   :name (error "NAME required.")
   :gl-type NIL
   :fields (error "FIELDS required.")))

(defmethod initialize-instance :after ((gl-struct gl-struct) &key name gl-type)
  (unless gl-type
    (setf (gl-type gl-struct) (format NIL "~@(~a~)" (cffi:translate-underscore-separated-name name)))))

(defmethod print-object ((gl-struct gl-struct) stream)
  (print-unreadable-object (gl-struct stream :type T)
    (format stream "~s" (name gl-struct))))

(defmethod gl-source ((gl-struct gl-struct))
  `(glsl-toolkit:struct-declaration
    ,(gl-type gl-struct)
    ,@(mapcar #'gl-source (fields gl-struct))))

(defun gl-struct (name &optional (errorp T))
  (or (gethash name *gl-structs*)
      (when errorp (error "No gl-struct named ~s is defined." name))))

(defun (setf gl-struct) (struct name)
  (check-type struct gl-struct)
  (setf (gethash name *gl-structs*) struct))

(defun remove-gl-struct (name)
  (remhash name *gl-structs*))

(defun translate-gl-struct-field-info (fields)
  (loop for field in fields
        collect (destructuring-bind (name type &rest args) field
                  `(make-instance 'gl-struct-field :name ',name :type ',type ,@args))))

(defmacro define-gl-struct (name/options &body fields)
  (destructuring-bind (name &rest initargs &key (type 'gl-struct))
      (enlist name/options)
    (let ((initargs (copy-list initargs)))
      (remf initargs :type)
      `(setf (gl-struct ',name)
             (make-instance ',type
                            :name ',name
                            :fields (list ,@(translate-gl-struct-field-info fields))
                            ,@initargs)))))

;; (defmethod compute-offsets ((struct gl-struct) (layout (eql :std140)))
;;   )

;; (defmethod compute-offsets ((struct gl-struct) (layout (eql :std430)))
;;   )
