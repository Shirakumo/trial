#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defgeneric serialize (object))

(defmethod serialize (object)
  object)

(defmethod serialize (symbol)
  ;; FIXME: dereferencing gensym named units
  (if (keywordp symbol) symbol `',symbol))

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
  ((id :initarg :id :accessor id)))

(defmethod print-object ((reference reference) stream)
  (print `(@ ,(class-name (class-of reference)) ',(id reference)) stream))

(defmethod dereference ((reference reference))
  (error "Don't know how to dereference ~s." reference))

(defmacro @ (type id)
    (let ((type (or (find-symbol (format NIL "~a-~a" type 'reference) #.*package*)
                    (error "No such reference type ~s." type))))
      `(make-instance ',type :id ,id)))

(defclass unit-reference () ())

(defmethod dereference ((reference unit-reference))
  (unit (id reference) *scene*))

(defmethod serialize ((unit unit))
  (@ unit (name unit)))

(defclass asset-reference () ())

(defmethod dereference ((reference asset-reference))
  (apply #'asset (id reference)))

(defmethod serialize ((asset asset))
  (@ asset (list (class-name (class-of asset))
                (name (pool asset))
                (name asset))))

(defclass resource-reference () ())

(defmethod dereference ((reference resource-reference))
  (resource (apply #'asset (id reference))))

(defmethod serialize ((resource resource))
  (let ((asset (resource-asset resource)))
    (@ resource (list (class-name (class-of asset))
                      (name (pool asset))
                      (name asset)))))

(defun allocate-into (scene &rest forms)
  (dolist (form forms scene)
    (enter (apply #'allocate-instance (first form) (rest form)))))

(defmacro with-allocation-in (scene &body units)
  `(allocate-into ,scene
                  ,@(loop for (type . args) in units
                          collect `(list ',type ,@args))))

(defmethod create-allocation-form ((unit unit))
  `(,(class-name (class-of unit))
    ,@(loop for (key val) on (allocation-initargs unit) by #'cddr
            collect key collect (serialize val))))

(defgeneric allocation-initargs (unit)
  (:method-combination append))

(defmethod allocation-initargs append ((unit unit))
  (list :name (name unit)))

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
  (for:for ((cell of-queue queue)
            (val = (flare-queue:value cell))
            (new-val = (funcall function val)))
    (unless (eq val new-val) (setf (flare-queue:value cell) new-val)))
  queue)

(defun resolve-references (scene)
  (labels ((maybe-dereference (object)
             (if (typep object 'reference)
                 (dereference object)
                 (traverse object #'maybe-dereference))))
    (loop val being the hash-values of (name-map scene)
          do (traverse val #'maybe-dereference))))

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
  `(defun restore-scene (scene)
     (reinitialize-instance scene ,@(allocation-initargs scene))
     (with-allocation-in scene
       ,@(for:for ((unit over scene)
                   (forms collecting (create-allocation-form unit)))))
     (resolve-references scene)
     (structure-scene scene ',(compile-structure scene))))
