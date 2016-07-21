#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defpackage trial-save-area
  (:use #:cl #:trial)
  (:export #:build))

(defparameter *compile-save* T)
(defvar *save-object-counter* 0)

(defgeneric make-save-form (object))
(defgeneric save-form-initargs (object)
  (:method-combination append))
(defgeneric save-form-objects (object))
(defgeneric create-save (object target))
(defgeneric load-save (source))

(defmethod make-save-form :around (object)
  (let ((result (call-next-method)))
    (when result (incf *save-object-counter*))
    result))

(defmethod save-form-initargs append (object)
  NIL)

(defmethod save-form-objects (object)
  NIL)

(defclass unsavable ()
  ())

(defmethod make-save-form ((unsavable unsavable))
  ())

(defclass savable ()
  ())

(defmethod make-save-form ((savable savable))
  `(trial-save-area::instantiate
    ,(class-name (class-of savable))
    ,(save-form-initargs savable)
    ,@(save-form-objects savable)))

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

(defmacro trial-save-area::instantiate (name initargs &rest objects)
  (if objects
      (let ((instance (gensym "INSTANCE")))
        `(let ((,instance (make-instance ',name ,@initargs)))
           ,@(loop for object in objects
                   when object collect `(enter ,object ,instance))))
      `(make-instance ',name ,@initargs)))

(defmacro trial-save-area::allocate (name initargs &rest objects)
  (if (every #'null objects)
      `(make-instance ',name ,@initargs)
      (let ((instance (gensym "INSTANCE")))
        `(let ((,instance (allocate-instance ',name ,@initargs)))
           ,@(loop for object in objects
                   when object collect `(enter ,object ,instance))))))

(defun write-save-object (thing stream)
  (let ((*package* (find-package '#:trial-save-area)))
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
    (terpri stream)))

(defmethod create-save (object (target string))
  (create-save object (uiop:parse-native-namestring target)))

(defmethod create-save (object (target pathname))
  (flet ((save-to (file)
           (with-open-file (stream file :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create)
             (create-save object stream)
             file)))
    (v:info :test "Saving ~a to ~a ." object target)
    (if *compile-save*
        (let ((temp (merge-pathnames (format NIL "~a.tmp" (pathname-name target)))))
          (unwind-protect
               (compile-file (save-to temp) :output-file target)
            (uiop:delete-file-if-exists temp)))
        (save-to target)))
  target)

(defmethod create-save (object (target stream))
  (let ((*save-object-counter* 0))
    (write-save-object `(cl:in-package #:trial-save-area) target)
    (write-save-object `(cl:defun trial-save-area:build ()
                          ,(make-save-form object)) target)
    (v:info :trial.savestate "Saved ~a object~:p." *save-object-counter*)))

(defmethod load-save ((source string))
  (load-save (uiop:parse-native-namestring source)))

(defmethod load-save ((source pathname))
  (let ((*package* (find-package '#:trial-save-area)))
    (load source)
    (prog1 (trial-save-area:build)
      (fmakunbound 'trial-save-area:build))))
