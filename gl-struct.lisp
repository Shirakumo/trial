#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass gl-struct ()
  ((storage-ptr :initarg :storage-ptr :accessor storage-ptr)))

(defun compound-struct-slot-initform (slot standard storage-ptr)
  (case (first (gl-type slot))
    (:struct
     (make-instance (second (gl-type slot))))
    (:array
     (destructuring-bind (identifier type count) (gl-type slot)
       (declare (ignore identifier))
       (if (listp type)
           (loop with vector = (make-array count)
                 with size = (buffer-field-stride (second type) standard)
                 for i from 0 below count
                 for offset = (base-offset slot) then (+ offset size)
                 do (setf (aref vector i) (make-instance (second type)
                                                         :storage-ptr storage-ptr
                                                         :base-offset offset))
                 finally (return vector))
           (make-instance 'gl-vector
                          :storage-ptr storage-ptr
                          :base-offset (base-offset slot)
                          :element-type type
                          :element-count count
                          :stride (buffer-field-stride type standard)))))))

(defmethod initialize-instance :after ((struct gl-struct) &key base-offset)
  (when base-offset (cffi:incf-pointer (storage-ptr struct) base-offset))
  ;; TODO: optimise with precompiled ctors
  (loop with standard = (layout-standard (class-of struct))
        for slot in (c2mop:class-slots (class-of struct))
        when (and (typep slot 'gl-struct-effective-slot)
                  (not (typep slot 'gl-struct-immediate-slot)))
        do (setf (slot-value struct (c2mop:slot-definition-name slot))
                 (compound-struct-slot-initform slot standard (storage-ptr struct)))))

(defmethod compute-dependent-types ((struct gl-struct))
  (compute-dependent-types (class-of struct)))

(defmethod gl-source ((struct gl-struct))
  (gl-source (class-of struct)))

(defclass gl-struct-class (standard-class)
  ((gl-type :initarg :gl-type :accessor gl-type)
   (layout-standard :initform :std140 :reader layout-standard)))

(defmethod initialize-instance :after ((class gl-struct-class) &key)
  (unless (slot-boundp class 'gl-type)
    (setf (gl-type class) (cffi:translate-camelcase-name (class-name class)
                                                         :upper-initial-p T))))

(defmethod shared-initialize :after ((class gl-struct-class) slots &key layout-standard)
  (when layout-standard
    (setf (slot-value class 'layout-standard)
          (unlist layout-standard))))

(defmethod c2mop:validate-superclass ((a gl-struct-class) (b T))  NIL)
(defmethod c2mop:validate-superclass ((a gl-struct-class) (b standard-class)) T)
(defmethod c2mop:validate-superclass ((a T) (b gl-struct-class)) NIL)

(defmethod struct-fields ((class gl-struct-class))
  (unless (c2mop:class-finalized-p class)
    (c2mop:finalize-inheritance class))
  (loop for slot in (c2mop:class-slots class)
        when (typep slot 'gl-struct-slot)
        collect slot))

(defmethod compute-dependent-types ((name symbol))
  (compute-dependent-types (find-class name)))

(defmethod compute-dependent-types ((class gl-struct-class))
  (let ((deps ()))
    (flet ((add (dep)
             (setf deps (list* dep (compute-dependent-types dep)))))
      (loop for slot in (struct-fields class)
            for type = (gl-type slot)
            do (when (listp type)
                 (ecase (first type)
                   (:struct (add (second type)))
                   (:array (when (listp (second type))
                             (add (second (second type))))))))
      (delete-duplicates deps))))

(defmethod gl-source ((class gl-struct-class))
  `(glsl-toolkit:struct-declaration
    ,(gl-type class)
    ,@(mapcar #'gl-source (struct-fields class))))

(defmethod vertex-layout ((class gl-struct-class))
  (let ((stride (buffer-field-stride class :vertex-buffer)))
    (values (loop for offset = 0 then (+ offset size)
                  for field in (struct-fields class)
                  for size = (buffer-field-size (gl-type field) :vertex-buffer 0)
                  collect (list :offset offset
                                :size (ecase (gl-type field)
                                        ((:float :int) 1)
                                        ((:vec2) 2)
                                        ((:vec3) 3)
                                        ((:vec4) 4)
                                        ((:mat2) 4)
                                        ((:mat3) 9)
                                        ((:mat4) 16))
                                :stride stride))
            stride)))

(defmethod c2mop:compute-slots ((class gl-struct-class))
  (let ((slots (call-next-method))
        (standard (slot-value class 'layout-standard))
        (offset 0))
    ;; Compute discrete slot offsets.
    (loop for slot in slots
          when (typep slot 'gl-struct-slot)
          do (let ((base (round-to (buffer-field-base (gl-type slot) standard) offset)))
               (setf (slot-value slot 'base-offset) base)
               (setf offset (+ base (buffer-field-size (gl-type slot) standard base)))))
    slots))

(defmethod c2mop:finalize-inheritance :after ((class gl-struct-class))
  ;; FIXME: remove dependents again (how?)
  (dolist (field (struct-fields class))
    (when (listp (gl-type field))
      (ecase (first (gl-type field))
        (:struct (c2mop:add-dependent (find-class (second (gl-type field))) class))
        (:array (when (listp (second (gl-type field)))
                  (c2mop:add-dependent (find-class (second (second (gl-type field)))) class)))))))

(defmethod c2mop:update-dependent ((updated gl-struct-class) (dependent gl-struct-class) &rest initargs)
  ;; If a referenced struct changed, we need to re-finalise in order to recompute field offsets.
  (c2mop:finalize-inheritance dependent))

(defmethod buffer-field-base ((class gl-struct-class) (standard (eql :std140)))
  (round-to (buffer-field-base :vec4 :std140)
            (loop for field in (struct-fields class)
                  maximize (buffer-field-base (gl-type field) standard))))

(defmethod buffer-field-base ((class gl-struct-class) (standard (eql :vertex-buffer)))
  1)

(defmethod buffer-field-base ((class gl-struct-class) (standard (eql T)))
  (buffer-field-base class (layout-standard class)))

(defmethod buffer-field-size ((class gl-struct-class) standard base)
  (round-to (buffer-field-base class standard)
            (let ((field (car (last (struct-fields class)))))
              (+ (base-offset field) (buffer-field-size (gl-type field) standard base)))))

(defmethod buffer-field-size ((class gl-struct-class) (standard (eql T)) base)
  (buffer-field-size class (layout-standard class) base))

(defmethod buffer-field-stride ((class gl-struct-class) (standard (eql :std140)))
  (buffer-field-size class standard 0))

(defmethod buffer-field-stride ((class gl-struct-class) (standard (eql :vertex-buffer)))
  (buffer-field-size class standard 0))

(defmethod buffer-field-stride ((class gl-struct-class) (standard (eql T)))
  (buffer-field-stride class (layout-standard class)))

(defvar *indentation* 0)
(defmethod describe-memory-layout ((class gl-struct-class) stream offset standard)
  (let ((offset (round-to (buffer-field-base class T) offset)))
    (format stream "~5d ~v{ ~} ~a ~64t(~5dB)~%"
            offset  *indentation* 0 (gl-type class) (buffer-field-size class T offset))
    (let ((*indentation* (1+ *indentation*)))
      (loop for field in (struct-fields class)
            do (setf offset (describe-memory-layout field stream offset standard))))))

(defmethod describe-object :after ((class gl-struct-class) stream)
  (format stream "~&~%Memory Layout (~a):~%"
          (layout-standard class))
  (describe-memory-layout class stream 0 (layout-standard class)))

(defclass gl-struct-slot (c2mop:standard-slot-definition)
  ((gl-type :initarg :gl-type :accessor gl-type)
   (gl-name :initarg :gl-name :accessor gl-name)
   (qualifiers :initarg :qualifiers :accessor qualifiers)
   (layout :initarg :layout :accessor layout)))

(defmethod c2mop:slot-definition-type ((slot gl-struct-slot))
  (if (slot-boundp slot 'gl-type)
      (let ((type (gl-type slot)))
        (if (listp type)
            (ecase (first type)
              (:struct (second type))
              (:array (if (listp (second type))
                          'vector
                          'gl-vector)))
            (gl-type->cl-type type)))
      T))

(defmethod gl-source ((slot gl-struct-slot))
  `(glsl-toolkit:struct-declarator
    (glsl-toolkit:type-qualifier
     ,@(when (layout slot)
         `((glsl-toolkit:layout-qualifier
            ,@(loop for id in (enlist (layout slot))
                    collect `(glsl-toolkit:layout-qualifier-id ,@(enlist id))))))
     ,@(qualifiers slot))
    (glsl-toolkit:type-specifier
     ,@(labels ((translate-type (type)
                  (etypecase type
                    (cons
                     (ecase (first type)
                       (:struct (list (gl-type (find-class (second type)))))
                       (:array (append (translate-type (second type))
                                       `((glsl-toolkit:array-specifier ,(third type)))))))
                    (symbol (list type)))))
         (translate-type (gl-type slot))))
    ,(gl-name slot)))

(defmethod describe-memory-layout ((slot gl-struct-slot) stream offset standard)
  (let* ((offset (round-to (buffer-field-base (gl-type slot) standard) offset))
         (size (buffer-field-size (gl-type slot) standard offset)))
    (format stream "~5d ~v{ ~} ~a ~a ~64t(~5dB)~%"
            offset *indentation* 0 (gl-name slot) (gl-type slot) size)
    (when (listp (gl-type slot))
      (let ((*indentation* (1+ *indentation*)))
        (ecase (first (gl-type slot))
          (:struct
           (describe-memory-layout (gl-type slot) stream offset))
          (:array
           (loop repeat (third (gl-type slot))
                 for start from offset by (buffer-field-stride (second (gl-type slot)) standard)
                 do (if (listp (second (gl-type slot)))
                        (describe-memory-layout (find-class (second (second (gl-type slot)))) stream start standard)
                        (format stream "~5d ~v{ ~} ~a ~64t(~5dB)~%"
                                start *indentation* 0 (second (gl-type slot)) (buffer-field-size (second (gl-type slot)) standard start))))))))
    (+ offset size)))

(defclass gl-struct-direct-slot (gl-struct-slot c2mop:standard-direct-slot-definition)
  ())

(defmethod c2mop:direct-slot-definition-class ((class gl-struct-class) &rest _)
  (declare (ignore _))
  (find-class 'gl-struct-direct-slot))

(defclass gl-struct-effective-slot (gl-struct-slot c2mop:standard-effective-slot-definition)
  ((base-offset :reader base-offset))
  (:default-initargs
   :qualifiers ()
   :layout NIL))

(defmethod initialize-instance :after ((slot gl-struct-effective-slot) &key)
  (unless (slot-boundp slot 'gl-name)
    (setf (gl-name slot) (cffi:translate-underscore-separated-name
                          (c2mop:slot-definition-name slot)))))

;;; KLUDGE: Since there's no way to influence the initargs passed to EFFECTIVE-SLOT-DEFINITION-CLASS
;;          there's no way for us to actually know which effective slot definition class we need.
;;          We are also not allowed to change-class slot definition objects, so we have to use this
;;          gross hack here instead.
(defvar *effective-slot-definition-class*
  (find-class 'c2mop:standard-effective-slot-definition))

(defmethod c2mop:effective-slot-definition-class ((class gl-struct-class) &rest _)
  (declare (ignore _))
  *effective-slot-definition-class*)

;; FIXME: maybe this should be NIL during class initialisation after all so that the
;;        slot initform can do its thing.
(defmethod c2mop:slot-boundp-using-class ((class gl-struct-class) (object gl-struct) (slot gl-struct-effective-slot))
  T)

(defmethod c2mop:slot-makunbound-using-class ((class gl-struct-class) (object gl-struct) (slot gl-struct-effective-slot))
  (error "Cannot unbind struct slots."))

(defmethod c2mop:compute-effective-slot-definition ((class gl-struct-class) name direct-slots)
  (let (effective)
    (flet ((maybe-init (type)
             (unless effective
               (let ((*effective-slot-definition-class* (find-class type)))
                 (setf effective (call-next-method))))))
      (dolist (direct direct-slots)
        (when (eql name (c2mop:slot-definition-name direct))
          (cond ((typep direct 'gl-struct-direct-slot)
                 (maybe-init 'gl-struct-effective-slot)
                 ;; Copy over slots from the direct definitions.
                 (flet ((maybe-copy-slots (slots &optional (source direct))
                          (dolist (slot slots)
                            (when (and (slot-boundp source slot)
                                       (not (slot-boundp effective slot)))
                              (setf (slot-value effective slot) (slot-value source slot))))))
                   (when (and (slot-boundp direct 'gl-type)
                              (not (slot-boundp effective 'gl-type)))
                     (setf (slot-value effective 'gl-type) (slot-value direct 'gl-type))
                     ;; Now that we know the type we might have to change class again, so
                     ;; recreate the instance and copy over all known fields.
                     (when (keywordp (gl-type effective))
                       (let ((old (shiftf effective NIL)))
                         (maybe-init 'gl-struct-immediate-slot)
                         (maybe-copy-slots '(gl-type gl-name qualifiers layout) old))))
                   (maybe-copy-slots '(gl-name qualifiers layout))))
                (T
                 (maybe-init 'c2mop:standard-effective-slot-definition)))))
      ;; Check effective slot for completeness.
      (when (and (typep effective 'gl-struct-effective-slot)
                 (not (slot-boundp effective 'gl-type)))
        (error (format NIL "The slot ~s on ~s is a struct slot, but no GL-TYPE is specified."
                       name (class-name class))))
      effective)))

(defclass gl-struct-immediate-slot (gl-struct-effective-slot)
  ())

;; TODO: generate optimised accessor functions

(defmethod c2mop:slot-value-using-class ((class gl-struct-class) (struct gl-struct) (slot gl-struct-immediate-slot))
  (gl-memref (cffi:inc-pointer (storage-ptr struct) (base-offset slot))
             (gl-type slot)))

(defmethod (setf c2mop:slot-value-using-class) (value (class gl-struct-class) (struct gl-struct) (slot gl-struct-immediate-slot))
  (setf (gl-memref (cffi:inc-pointer (storage-ptr struct) (base-offset slot))
                   (gl-type slot))
        value))

(defmethod c2mop:slot-definition-allocation ((slot gl-struct-immediate-slot))
  :none)
;; FIXME: Figure out direct accessor functions

(defmacro define-gl-struct (name &body slots)
  (destructuring-bind (name . options) (enlist name)
    `(defclass ,name (,@(cdr (assoc :include options)) gl-struct)
       ,(loop for (slot type . args) in slots
              collect (list* slot :gl-type type args))
       (:metaclass gl-struct-class)
       ,@(remove :include options :key #'car))))

;;; Only for primitive types.
;;; FIXME: factor out into trivial-* library
(defclass gl-vector (#+(or abcl sbcl) sequence)
  ((storage-ptr :initarg :storage-ptr :accessor storage-ptr)
   (element-count :initarg :element-count :reader sb-sequence:length)
   (element-type :initarg :element-type :reader element-type)
   (stride :initarg :stride :accessor stride)))

(defmethod initialize-instance :after ((vector gl-vector) &key base-offset)
  (when base-offset (cffi:incf-pointer (storage-ptr vector) base-offset)))

(defmethod print-object ((vector gl-vector) stream)
  (print-unreadable-object (vector stream :type T :identity T)
    (format stream "~s ~s" (element-type vector) (length vector))))

#+sbcl
(defmethod sb-sequence:elt ((vector gl-vector) index)
  (gl-memref (cffi:inc-pointer (storage-ptr vector)
                               (* index (stride vector)))
             (element-type vector)))

#+sbcl
(defmethod (setf sb-sequence:elt) (value (vector gl-vector) index)
  (setf (gl-memref (cffi:inc-pointer (storage-ptr vector)
                                     (* index (stride vector)))
                   (element-type vector))
        value))

#+sbcl
(defmethod sb-sequence:make-sequence-iterator ((vector gl-vector) &key from-end start end)
  (let* ((start (or start 0))
         (end (or end (sb-sequence:length vector)))
         (state (if from-end (1- end) start))
         (limit (if from-end (1- start) end))
         (ptr (storage-ptr vector))
         (type (element-type vector))
         (stride (stride vector)))
    (values state limit from-end
            (if from-end
                (lambda (vector state from-end)
                  (declare (ignore vector from-end))
                  (1- state))
                (lambda (vector state from-end)
                  (declare (ignore vector from-end))
                  (1+ state)))
            (lambda (vector state limit from-end)
              (declare (ignore vector from-end))
              (= state limit))
            (lambda (vector state)
              (declare (ignore vector))
              (gl-memref (cffi:inc-pointer ptr (* state stride))
                         type))
            (lambda (value vector state)
              (declare (ignore vector))
              (setf (gl-memref (cffi:inc-pointer ptr (* state stride))
                               type)
                    value))
            (lambda (vector state)
              (declare (ignore vector))
              state)
            (lambda (vector state)
              (declare (ignore vector))
              state))))

;;; FIXME: Add a way to allow SHARED and PACKED layouts.
