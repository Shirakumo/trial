#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defpackage trial-save-area
  (:use #:cl #:trial)
  (:export))

(defparameter *compile-save* NIL)
(defvar trial-save-area::*scene*)

(defgeneric make-save-form (object))
(defgeneric save-form-initargs (object)
  (:method-combination append))
(defgeneric save-form-objects (object))
(defgeneric create-save (scene target))
(defgeneric load-save (scene source))

(defmacro trial-save-area::insert (container &body objects)
  (let ((cont (gensym "CONT")))
    `(let ((,cont ,container))
       ,@(loop for object in objects
               collect `(enter ,object ,cont))
       ,cont)))

(defmacro trial-save-area::make (class &rest initargs &key)
  `(make-instance ',class ,@initargs))

(defmacro trial-save-area::alloc (class &rest initargs &key)
  `(allocate-instance ',class ,@initargs))

(defmacro trial-save-area::with-container ((class &rest initargs) &body objects)
  `(trial-save-area::insert (trial-save-area::make ,class ,@initargs)
                            ,@objects))

(defmethod make-save-form (object)
  (let* ((inner (save-form-objects object))
         (forms (remove-if #'null (mapc #'make-save-form inner))))
    (if forms
        `(trial-save-area::with-container (,(class-name (class-of object)) ,@(save-form-initargs object))
           ,@forms)
        `(trial-save-area::make ,(class-name (class-of object))
                                ,@(save-form-initargs object)))))

(defmethod save-form-initargs append (object)
  NIL)

(defmethod save-form-objects (object)
  NIL)

(defclass unsavable ()
  ())

(defmethod make-save-form ((unsavable unsavable))
  ())

(defclass persistent (unsavable)
  ())

;; We have to do it like this to make it evaluated at load-time. During
;; compile time the class is not available and so we cannot inspect it
;; for slots.
(defun %create-save-form-initargs-method (class-name &rest slot-names)
  (let ((class (find-class class-name)))
    (c2mop:finalize-inheritance class)
    (let ((slots (c2mop:class-slots class)))
      (eval
       `(defmethod save-form-initargs append ((,class-name ,class-name))
          (list ,@(loop for name in slot-names
                        for slot = (find name slots :key #'c2mop:slot-definition-name)
                        for arg = (or (first (c2mop:slot-definition-initargs slot))
                                      (error "No initarg for class slot ~a." name))
                        collect arg collect `(slot-value ,class-name ',name))))))))

(defmacro define-saved-slots (class-name &rest slot-names)
  `(%create-save-form-initargs-method ',class-name ,@(loop for name in slot-names collect `',name)))

(defun write-save-object (thing stream)
  (write thing :stream stream
               :escape T
               :radix NIL
               :base 10
               :circle T
               :pretty T
               :level NIL
               :length NIL
               :case :downcase
               :array T
               :gensym T
               :readably NIL
               :right-margin NIL
               :miser-width NIL
               :lines NIL)
  (terpri stream))

(defun clear-for-reload (container)
  (for:for ((item over container))
    (unless (typep item 'persistent)
      (when (typep item 'container)
        (clear-for-reload item))
      (leave item container))))

(defmethod create-save (object (target string))
  (create-save object (uiop:parse-native-namestring target)))

(defmethod create-save (scene (target pathname))
  (flet ((save-to (file)
           (with-open-file (stream file :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create)
             (create-save scene stream)
             file)))
    (v:info :test "Saving ~a to ~a ." scene target)
    (if *compile-save*
        (let ((temp (merge-pathnames (format NIL "~a.tmp" (pathname-name target)))))
          (unwind-protect
               (compile-file (save-to temp) :output-file target)
            (uiop:delete-file-if-exists temp)))
        (save-to target)))
  target)

(defmethod create-save (scene (stream stream))
  (let ((*package* (find-package '#:trial-save-area)))
    (stop scene)
    (process scene) ; Process pending events
    (write-save-object `(cl:in-package '#:trial-save-area)
                       stream)
    (write-save-object `(trial-save-area::insert
                         trial-save-area::*scene*
                         ,@(for:for ((object over scene)
                                     (form = (make-save-form object))
                                     (forms when form collect form))))
                       stream)
    (start scene)))

(defmethod load-save (scene (source string))
  (load-save scene (uiop:parse-native-namestring source)))

(defmethod load-save (scene (source pathname))
  (let ((*package* (find-package '#:trial-save-area))
        (trial-save-area::*scene* scene))
    (stop scene)
    (process scene) ; Process pending events
    (clear-for-reload scene)
    (load source)
    (start scene)))
