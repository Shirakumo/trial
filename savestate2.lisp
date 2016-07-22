#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defparameter *compile-savestate* NIL)

(defgeneric serialize (object))
(defgeneric allocation-slots (unit)
  (:method-combination append))
(defgeneric make-allocation-form (object))
(defgeneric dereference (reference))
(defgeneric traverse (object function))

(defmethod serialize (object)
  object)

(defmethod serialize ((symbol symbol))
  ;; FIXME: dereferencing gensym named units
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

(defclass reference ()
  ((id :initarg :id :accessor id)
   (referenced-type :allocation :class :reader referenced-type)))

(defmethod print-object ((reference reference) stream)
  (print `(@ ,(referenced-type reference) ,(serialize (id reference))) stream))

(defmethod dereference ((reference reference))
  (error "Don't know how to dereference ~s." reference))

(defun find-reference-for-type (type)
  (labels ((traverse (class)
             (dolist (class (c2mop:class-direct-subclasses class))
               (c2mop:finalize-inheritance class)
               (when (eql type (referenced-type (c2mop:class-prototype class)))
                 (return-from find-reference-for-type (class-name class)))
               (traverse class))))
    (traverse (find-class 'reference))))

(defmacro @ (type id)
  `(make-instance (find-reference-for-type ',type) :id ,id))

(defun enter-all (scene &rest entities)
  (dolist (entity entities scene)
    (enter entity scene)))

(defmacro with-allocation-in (scene &body units)
  `(enter-all ,scene
              ,@(loop for (type . args) in units
                      collect `(mkobject ',type ,@(loop for (key val) on args by #'cddr collect `',key collect val)))))

(defmethod make-allocation-form ((unit unit))
  `(,(class-name (class-of unit))
    ,@(loop for (key val) on (allocation-slots unit) by #'cddr
            collect key collect (serialize val))))

(defmacro define-saved-slots (class-name &rest slot-names)
  `(defmethod allocation-slots append ((,class-name ,class-name))
     (list ,@(loop for name in slot-names collect `',name collect `(slot-value ,class-name ',name)))))

(defvar *traversal-stack* NIL)
(defmethod traverse :around (object function)
  (unless (find object *traversal-stack*)
    (let ((*traversal-stack* (cons object *traversal-stack*)))
      (call-next-method))))

(defmethod traverse (object function)
  object)

(defmethod traverse ((list list) function)
  (loop for cons on list
        for new-val = (funcall function (car cons))
        do (unless (eq (car cons) new-val) (setf (car cons) new-val)))
  list)

(defmethod traverse ((array array) function)
  (loop for i from 0 below (array-total-size array)
        for val = (row-major-aref array i)
        for new-val = (funcall function val)
        do (unless (eq val new-val) (setf (row-major-aref array i) new-val)))
  array)

(defmethod traverse ((table hash-table) function)
  (let ((pending ()))
    (loop for k being the hash-keys of table
          for v being the hash-values of table
          for new-k = (funcall function k)
          for new-v = (funcall function v)
          do (cond ((not (eql new-k k))
                    (remhash k table)
                    (push (cons new-k new-v) pending))
                   ((not (eql new-v v))
                    (setf (gethash k table) v))))
    (dolist (cons pending table)
      (setf (gethash (car cons) table) (cdr cons)))))

(defmethod traverse ((object standard-object) function)
  (loop for slot in (c2mop:class-slots (class-of object))
        for name = (c2mop:slot-definition-name slot)
        for val = (slot-value object name)
        for new-val = (funcall function val)
        do (unless (eq val new-val) (setf (slot-value object name) new-val)))
  object)

(defmethod traverse ((queue flare-queue:queue) function)
  (for:for ((cell flare-queue:of-queue queue)
            (val = (flare-queue:value cell))
            (new-val = (funcall function val)))
    (unless (eq val new-val) (setf (flare-queue:value cell) new-val)))
  queue)

(defun resolve-references (scene)
  (labels ((maybe-dereference (object)
             (if (typep object 'reference)
                 (dereference object)
                 (traverse object #'maybe-dereference))))
    (loop for val being the hash-values of (name-map scene)
          do (traverse val #'maybe-dereference))))

(defun insert-references (scene)
  (labels ((maybe-reference (object)
             (if (typep object 'unit)
                 (make-instance 'unit-reference :id (name object))
                 (traverse object #'maybe-reference))))
    (loop for val being the hash-values of (name-map scene)
          do (traverse val #'maybe-reference))))

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
       (update-slots ,scenesym ,@(mapcar #'serialize (allocation-slots scene)))
       (with-allocation-in ,scenesym
         ,@(loop for unit being the hash-values of (name-map scene)
                 for form = (make-allocation-form unit)
                 when form collect form))
       (structure-scene ,scenesym ',(compile-structure scene)))))

(defclass unsavable () ())

(defmethod make-allocation-form ((unsavable unsavable))
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
    (insert-references scene)
    (tg:gc :full T)
    (load file)
    (restore-scene scene)
    (resolve-references scene)
    (discard-events scene)
    (start scene)))
