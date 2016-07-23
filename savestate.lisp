#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defparameter *compile-savestate* T)
(defvar *scene*)

(defgeneric serialize (object))
(defgeneric restoration-slots (unit)
  (:method-combination append))
(defgeneric restoration-initargs (unit)
  (:method-combination append))
(defgeneric make-restore-form (object))
(defgeneric @=> (type &rest args))

(defmethod restoration-slots append (object)
  ())

(defmethod restoration-initargs append (object)
  ())

(defmethod serialize (object)
  object)

(defmethod serialize ((symbol symbol))
  (typecase symbol
    ((or keyword (eql T) (eql NIL)) symbol)
    (T `',symbol)))

(defmethod serialize ((list list))
  `(list ,@(loop for i in list
                 collect (serialize i))))

(defmethod serialize ((string string))
  string)

(defmethod serialize ((vector vector))
  `(vector ,@(loop for i across vector
                   collect (serialize i))))

(defmethod serialize ((array array))
  `(mkarray ',(array-dimensions array)
            ,@(loop for i from 0 below (array-total-size array)
                    collect (serialize (row-major-aref array i)))))

(defmethod serialize ((hash-table hash-table))
  `(mktable ',(hash-table-test hash-table)
            ,@(loop for key being the hash-keys of hash-table
                    for val being the hash-values of hash-table
                    collect (serialize key) collect (serialize val))))

(defmethod serialize ((object standard-object))
  `(mkobject ',(class-name (class-of object))
             ,@(loop for slot in (c2mop:class-slots object)
                     for name = (c2mop:slot-definition-name slot)
                     collect `',name collect (serialize (slot-value object name)))))

(defmacro @ (type &rest args)
    `(@=> ',type ,@(loop for arg in args collect `',arg)))

(defun make-partial-instance (class &rest slots)
  (let* ((class (ensure-class class))
         (skeleton (allocate-instance class)))
    ;; Initialise slots as by make-instance, bypassing initialize-instance.
    (let ((default-initargs (loop for (name form func) in (c2mop:class-default-initargs class)
                                  collect name collect (funcall func))))
      (apply #'shared-initialize skeleton T default-initargs))
    ;; initialise explicit slots
    (apply #'update-slots skeleton slots)))

(defun finish-partial-instance (skeleton slots &rest initargs)
  (apply #'update-slots skeleton slots)
  (apply #'reinitialize-instance skeleton initargs))

(defmacro with-allocation-in (scene &body units)
  `(let ((*scene* ,scene))
     ,@(loop for (name type) in units
             collect `(register (make-partial-instance ',(unlist type) 'name ',name) *scene*))
     ,@(loop for (name type . initargs) in units
             collect `(finish-partial-instance (@ unit ,name) (list ,@(rest (enlist type))) ,@initargs))))

(defmethod make-restore-form ((unit unit))
  (let ((slots (loop for (key val) on (restoration-slots unit) by #'cddr
                     collect `',key collect (serialize val)))
        (initargs (loop for (key val) on (restoration-initargs unit) by #'cddr
                        collect key collect (serialize val)))
        (type (class-name (class-of unit))))
    `(,(name unit)
      ,(if slots `(,type ,@slots) type)
      ,@initargs)))

(defmacro define-saved-slots (class-name &rest slot-names)
  `(defmethod restoration-slots append ((,class-name ,class-name))
     (list ,@(loop for name in slot-names collect `',name collect `(slot-value ,class-name ',name)))))

(defun slot-initarg-alist (class)
  (loop for slot in (c2mop:class-slots class)
        for initargs = (c2mop:slot-definition-initargs slot)
        when initargs
        collect (cons (c2mop:slot-definition-name slot) (first initargs))))

(defmacro define-saved-initargs (class-name &rest slot-names)
  (let ((slot (gensym "SLOT"))
        (alist (gensym "ALIST")))
    `(defmethod restoration-initargs append ((,class-name ,class-name))
       (let ((,alist (slot-initarg-alist (find-class ',class-name))))
         (loop for ,slot in ',slot-names
               for initarg = (or (and (listp ,slot) (second ,slot))
                                 (cdr (assoc ,slot ,alist))
                                 (error "No initarg for slot ~s on ~a." ,slot ,class-name))
               collect initarg collect (slot-value ,class-name ,slot))))))

(defun clear-for-reload (scene)
  (loop for item being the hash-values of (name-map scene)
        do (unless (typep item 'persistent)
             (leave item scene))))

(defun structure-scene (scene structure)
  (labels ((find-item (name)
             (or (unit name scene)
                 (error "Referenced unknown object ~s." name)))
           (struc (parent structure)
             (cond ((consp structure)
                    (let ((item (find-item (first structure))))
                      (enter item parent)
                      (struc item (rest structure))))
                   (T
                    (enter (find-item structure) parent)))))
    (dolist (item structure scene)
      (struc scene item))))

(defun compile-structure (container)
  (flet ((unit-structure (unit)
           (etypecase unit
             (container (list* (name unit) (compile-structure unit)))
             (unit (name unit)))))
    (for:for ((unit over container)
              (c collecting (unit-structure unit))))))

(defun scene-save-form (scene)
  (let ((scenesym (gensym "SCENE")))
    `(defun restore-scene (,scenesym)
       (update-slots ,scenesym ,@(mapcar #'serialize (restoration-slots scene)))
       (with-allocation-in ,scenesym
         ,@(loop for unit being the hash-values of (name-map scene)
                 for form = (make-restore-form unit)
                 when form collect form))
       (structure-scene ,scenesym ',(compile-structure scene)))))

(defclass unsavable () ())

(defmethod make-restore-form ((unsavable unsavable))
  NIL)

(defclass persistent (unsavable) ())

(defun save-scene (scene file)
  (v:info :trial.savestate "Saving ~a to ~a ." scene file)
  (stop scene)
  (process scene)
  (let ((temp (make-pathname :type "tmp" :defaults file))
        (*package* #.*package*))
    (unwind-protect
         (progn
           (with-open-file (stream temp :direction :output :if-exists :supersede)
             (let ((*package* #.*package*))
               (write `(in-package #:trial) :stream stream :case :downcase)
               (terpri stream)
               (write (scene-save-form scene) :stream stream :case :downcase :circle T)))
           (if *compile-savestate*
               (compile-file temp :output-file file)
               (uiop:copy-file temp file)))
      (uiop:delete-file-if-exists temp)))
  (start scene))

(defun load-scene (scene file)
  (let ((*package* #.*package*))
    (v:info :trial.savestate "Loading ~a from ~a ." scene file)
    (stop scene)
    (process scene)
    (clear-for-reload scene)
    ;; FIXME: Updating references from persistent objects to units within the scene
    (tg:gc :full T)
    (load file)
    (restore-scene scene)
    (fmakunbound 'restore-scene)
    (discard-events scene)
    (start scene)))
