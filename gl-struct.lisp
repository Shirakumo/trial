(in-package #:org.shirakumo.fraf.trial)

(defclass gl-struct ()
  ((storage :accessor storage :reader org.shirakumo.memory-regions:to-memory-region)
   (base-offset :initform 0 :initarg :base-offset :accessor base-offset)))

(defmethod mem:call-with-memory-region ((function function) (struct gl-struct) &key (start 0))
  (if (= 0 start (base-offset struct))
      (funcall function (storage struct))
      (let* ((region (storage struct))
             (region (memory-region (cffi:inc-pointer (memory-region-pointer region) (+ (base-offset struct) start))
                                    (max 0 (- (memory-region-size region) (base-offset struct) start)))))
        (declare (dynamic-extent region))
        (funcall function region))))

(defun compound-struct-slot-initform (struct slot standard storage)
  (case (first (gl-type slot))
    (:struct
     (make-instance (second (gl-type slot)) :storage storage
                                            :base-offset (base-offset slot)))
    (:array
     (destructuring-bind (identifier type count) (gl-type slot)
       (declare (ignore identifier))
       (when (symbolp count)
         (setf count (slot-value struct count)))
       (if (listp type)
           (loop with vector = (make-array count)
                 with size = (buffer-field-stride standard (second type))
                 for i from 0 below count
                 for offset = (base-offset slot) then (+ offset size)
                 do (setf (aref vector i) (make-instance (second type)
                                                         :storage storage
                                                         :base-offset offset))
                 finally (return vector))
           (make-instance 'gl-vector
                          :storage storage
                          :base-offset (base-offset slot)
                          :element-type type
                          :element-count count
                          :stride (buffer-field-stride standard type)))))))

(defmethod shared-initialize ((struct gl-struct) slots &rest initargs &key (storage NIL storage-p))
  ;; TODO: optimise in class because this is dumb as heck
  ;; KLUDGE: we have to init standard slots first because the size computation
  ;;         may depend on the value of those standard slots
  (let ((class (class-of struct)))
    (flet ((maybe-init (slot)
             ;; THis suuuuucks.
             (when (and (not (typep slot 'gl-struct-immediate-slot))
                        (not (c2mop:slot-boundp-using-class class struct slot)))
               (loop for initarg in (c2mop:slot-definition-initargs slot)
                     for val = (getf initargs initarg #1='#:no-value)
                     do (unless (eq val #1#)
                          (setf (slot-value struct (c2mop:slot-definition-name slot)) val)
                          (return))
                     finally (when (c2mop:slot-definition-initfunction slot)
                               (setf (slot-value struct (c2mop:slot-definition-name slot))
                                     (funcall (c2mop:slot-definition-initfunction slot))))))))
      (etypecase slots
        (null)
        ((eql T)
         (dolist (slot (c2mop:class-slots class))
           (maybe-init slot)))
        (cons
         (dolist (name slots)
           (let ((slot (find name (c2mop:class-slots class) :key #'c2mop:slot-definition-name)))
             (unless (typep slot 'gl-struct-slot)
               (maybe-init slot)))))))
    (let ((size (buffer-field-size (layout-standard struct) struct 0)))
      (if (not storage-p)
          (unless (slot-boundp struct 'storage)
            (setf (slot-value struct 'storage) (mem:static-vector-memory-region size)))
          (etypecase storage
            ((eql T)
             (setf (slot-value struct 'storage) (mem:static-vector-memory-region size)))
            (null
             (setf (slot-value struct 'storage) NIL))
            (memory-region
             (assert (<= size (memory-region-size storage)))
             (setf (slot-value struct 'storage) storage)))))
    (call-next-method)))

(defmethod shared-initialize :after ((struct gl-struct) slots &key)
  ;; TODO: use the slot-definition-initfunction instead somehow
  (loop with standard = (layout-standard struct)
        for slot in (c2mop:class-slots (class-of struct))
        for slot-name = (c2mop:slot-definition-name slot)
        do (when (and (typep slot 'gl-struct-effective-slot)
                      (not (typep slot 'gl-struct-immediate-slot)))
             (setf (slot-value struct slot-name)
                   (compound-struct-slot-initform struct slot standard (storage struct))))))

(defmethod finalize ((struct gl-struct))
  (when (storage struct)
    (finalize (storage struct))
    (setf (storage struct) NIL)))

(defmethod (setf storage) :after (pointer (struct gl-struct))
  (loop with standard = (layout-standard struct)
        for slot in (c2mop:class-slots (class-of struct))
        do (when (and (typep slot 'gl-struct-effective-slot)
                      (not (typep slot 'gl-struct-immediate-slot)))
             (let ((value (slot-value struct (c2mop:slot-definition-name slot))))
               (typecase value
                 (vector
                  (loop for v across value
                        do (setf (storage v) pointer)))
                 ((or gl-vector gl-struct)
                  (setf (storage value) pointer)))))))

(defmethod compute-dependent-types ((struct gl-struct))
  (compute-dependent-types (class-of struct)))

(defmethod gl-source ((struct gl-struct))
  (let ((*dynamic-context* struct))
    (gl-source (class-of struct))))

(defmethod gl-type ((struct gl-struct))
  (gl-type (class-of struct)))

(defmethod struct-fields ((struct gl-struct))
  (struct-fields (class-of struct)))

(defmethod layout-standard ((struct gl-struct))
  (layout-standard (class-of struct)))

(defmethod buffer-field-base (standard (struct gl-struct))
  (let ((*dynamic-context* struct))
    (buffer-field-base standard (class-of struct))))

(defmethod buffer-field-size (standard (struct gl-struct) base)
  (let ((*dynamic-context* struct))
    (buffer-field-size standard (class-of struct) base)))

(defmethod buffer-field-stride (standard (struct gl-struct))
  (let ((*dynamic-context* struct))
    (buffer-field-stride standard (class-of struct))))

(defclass gl-struct-class (standard-class)
  ((gl-type :accessor gl-type)
   (layout-standard :initform 'std140 :reader layout-standard)))

(defmethod shared-initialize :after ((class gl-struct-class) slots &key layout-standard gl-type)
  (when layout-standard
    (setf (slot-value class 'layout-standard) (unlist layout-standard)))
  (when gl-type
    (setf (gl-type class) (unlist gl-type))))

(defmethod initialize-instance :after ((class gl-struct-class) &key)
  (unless (slot-boundp class 'gl-type)
    (setf (gl-type class) (cffi:translate-camelcase-name (class-name class)
                                                         :upper-initial-p T))))

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
    NIL
    ,@(mapcar #'gl-source (struct-fields class))))

(defmethod resolve-shader-include ((source gl-struct-class))
  (list 'glsl-toolkit:shader (gl-source source)))

(defmethod vertex-layout ((class gl-struct-class))
  (let ((stride (buffer-field-stride 'vertex-buffer class)))
    (values (loop for offset = 0 then (+ offset size)
                  for field in (struct-fields class)
                  for size = (buffer-field-size 'vertex-buffer (gl-type field) 0)
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
          do (let ((base (round-to (buffer-field-base standard (gl-type slot)) offset)))
               (setf (slot-value slot 'base-offset) base)
               (ignore-errors (setf offset (+ base (buffer-field-size standard (gl-type slot) base))))))
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

(defmethod buffer-field-base ((standard std140) (class gl-struct-class))
  (round-to (buffer-field-base standard :vec4)
            (loop for field in (struct-fields class)
                  maximize (buffer-field-base standard (gl-type field)))))

(defmethod buffer-field-base ((standard vertex-buffer) (class gl-struct-class))
  1)

(defmethod buffer-field-base ((standard (eql T)) (class gl-struct-class))
  (buffer-field-base (layout-standard class) class))

(defmethod buffer-field-size (standard (class gl-struct-class) base)
  (round-to (buffer-field-base standard class)
            (let ((field (car (last (struct-fields class)))))
              (+ (base-offset field) (buffer-field-size standard (gl-type field) base)))))

(defmethod buffer-field-size ((standard (eql T)) (class gl-struct-class) base)
  (buffer-field-size (layout-standard class) class base))

(defmethod buffer-field-stride ((standard std140) (class gl-struct-class))
  (buffer-field-size standard class 0))

(defmethod buffer-field-stride ((standard vertex-buffer) (class gl-struct-class))
  (buffer-field-size standard class 0))

(defmethod buffer-field-stride ((standard (eql T)) (class gl-struct-class))
  (buffer-field-stride (layout-standard class) class))

(defvar *indentation* 0)
(defmethod describe-memory-layout ((class gl-struct-class) stream offset standard)
  (let ((offset (round-to (buffer-field-base T class) offset)))
    (format stream "~5d ~v{ ~} ~a ~64t(~5dB)~%"
            offset  *indentation* 0 (gl-type class) (buffer-field-size T class offset))
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
  (flet ((resolve (value)
           (if (symbolp value)
               (slot-value *dynamic-context* value)
               value)))
    (let ((array ()))
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
                           (:array
                            (let ((size (resolve (third type))))
                              ;; KLUDGE: Trying to emit large arrays into source causes the driver to
                              ;;         spend an *absolutely insane* amount of time trying to compile
                              ;;         it to do whatever, so we just truncate it here.
                              (if (<= size 256)
                                  (push `(glsl-toolkit:array-specifier ,size) array)
                                  (push `(glsl-toolkit:array-specifier) array))
                              (translate-type (second type))))))
                        (symbol (list type)))))
             (translate-type (gl-type slot))))
        ,(gl-name slot)
        ,@array))))

(defmethod describe-memory-layout ((slot gl-struct-slot) stream offset standard)
  (let* ((offset (round-to (buffer-field-base standard (gl-type slot)) offset))
         (size (buffer-field-size standard (gl-type slot) offset)))
    (format stream "~5d ~v{ ~} ~a ~a ~64t(~5dB)~%"
            offset *indentation* 0 (gl-name slot) (gl-type slot) size)
    (when (listp (gl-type slot))
      (let ((*indentation* (1+ *indentation*)))
        (ecase (first (gl-type slot))
          (:struct
           (describe-memory-layout (gl-type slot) stream offset))
          (:array
           (loop repeat (third (gl-type slot))
                 for start from offset by (buffer-field-stride standard (second (gl-type slot)))
                 do (if (listp (second (gl-type slot)))
                        (describe-memory-layout (find-class (second (second (gl-type slot)))) stream start standard)
                        (format stream "~5d ~v{ ~} ~a ~64t(~5dB)~%"
                                start *indentation* 0 (second (gl-type slot)) (buffer-field-size standard (second (gl-type slot)) start))))))))
    (+ offset size)))

(defclass gl-struct-direct-slot (gl-struct-slot c2mop:standard-direct-slot-definition)
  ())

(defmethod c2mop:direct-slot-definition-class ((class gl-struct-class) &rest _ &key gl-type)
  (declare (ignore _))
  (if gl-type
      (find-class 'gl-struct-direct-slot)
      (call-next-method)))

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

(defun gl-struct-ref (struct slot &optional container)
  (let* ((class (class-of struct))
         (slot (find slot (c2mop:class-slots class) :key #'c2mop:slot-definition-name)))
    (gl-memref (cffi:inc-pointer (memory-region-pointer (storage struct)) (+ (base-offset struct) (base-offset slot)))
               (gl-type slot) :layout (layout-standard class) :container container)))

(defun (setf gl-struct-ref) (value struct slot &optional container)
  (let* ((class (class-of struct))
         (slot (find slot (c2mop:class-slots class) :key #'c2mop:slot-definition-name)))
    (setf (gl-memref (cffi:inc-pointer (memory-region-pointer (storage struct)) (+ (base-offset struct) (base-offset slot)))
                     (gl-type slot) :layout (layout-standard class) :container container)
          value)))

(defun gl-struct-ref* (class struct slot &optional container)
  (let* ((class (find-class class))
         (slot (find slot (c2mop:class-slots class) :key #'c2mop:slot-definition-name)))
    (gl-memref (cffi:inc-pointer (memory-region-pointer (storage struct)) (+ (base-offset struct) (base-offset slot)))
               (gl-type slot) :layout (layout-standard class) :container container)))

(defun (setf gl-struct-ref*) (value class struct slot &optional container)
  (let* ((class (find-class class))
         (slot (find slot (c2mop:class-slots class) :key #'c2mop:slot-definition-name)))
    (setf (gl-memref (cffi:inc-pointer (memory-region-pointer (storage struct)) (+ (base-offset struct) (base-offset slot)))
                     (gl-type slot) :layout (layout-standard class) :container container)
          value)))

(define-compiler-macro gl-struct-ref* (&whole whole &environment env class struct slot &optional container)
  (if (and (constantp class env) (constantp slot env))
      (let ((structg (gensym "STRUCT")))
        `(let ((,structg ,struct))
           (gl-memref (cffi:inc-pointer (memory-region-pointer (storage struct))
                                        (+ (base-offset struct)
                                           (load-time-value
                                            (base-offset (find ,slot (c2mop:class-slots (find-class ,class)) :key #'c2mop:slot-definition-name)))))
                      (load-time-value (gl-type (find ,slot (c2mop:class-slots (find-class ,class)) :key #'c2mop:slot-definition-name)))
                      :layout (load-time-value (layout-standard (find-class ,class)))
                      :container ,container)))
      whole))

(define-compiler-macro (setf gl-struct-ref*) (&whole whole &environment env value class struct slot &optional container)
  (if (and (constantp class env) (constantp slot env))
      (let ((structg (gensym "STRUCT")))
        `(let ((,structg ,struct))
           (setf (gl-memref (cffi:inc-pointer (memory-region-pointer (storage struct))
                                              (+ (base-offset struct)
                                                 (load-time-value
                                                  (base-offset (find ,slot (c2mop:class-slots (find-class ,class)) :key #'c2mop:slot-definition-name)))))
                            (load-time-value (gl-type (find ,slot (c2mop:class-slots (find-class ,class)) :key #'c2mop:slot-definition-name)))
                            :layout (load-time-value (layout-standard (find-class ,class)))
                            :container ,container)
                 ,value)))
      whole))

(defmacro with-gl-slots ((class &rest slots) struct &body body)
  (let* ((ptr (gensym "PTR"))
         (structg (gensym "STRUCT"))
         (class (c2mop:ensure-finalized (find-class class)))
         (slots (loop for slot in slots
                      collect (list (gensym (string slot)) slot (or (find slot (c2mop:class-slots class) :key #'c2mop:slot-definition-name)
                                                                    (error "No such slot ~s on class ~s" slot class))))))
    `(let ,(loop for (sym _ slot) in slots
                 for type = (gl-type->cl-type (gl-type slot))
                 collect `(,sym ,(case type
                                   (single-float 0f0)
                                   (double-float 0d0)
                                   (T (if (subtypep type 'org.shirakumo.type-templates:type-object)
                                          `(,type)
                                          (type-prototype type))))))
       (declare (dynamic-extent ,@(loop for slot in slots collect (first slot)))
                ,@(loop for (sym _ slot) in slots
                        collect `(type ,(gl-type->cl-type (gl-type slot)) ,sym)))
       (let* ((,structg ,struct)
              (,ptr (cffi:inc-pointer (memory-region-pointer (storage ,structg)) (base-offset ,structg))))
         (symbol-macrolet ,(loop for (sym name slot) in slots
                                 collect `(,name (gl-memref (cffi:inc-pointer ,ptr ,(base-offset slot))
                                                            ,(gl-type slot) :layout ',(layout-standard class) :container ,sym)))
           ,@body)))))

(defmethod c2mop:slot-value-using-class ((class gl-struct-class) (struct gl-struct) (slot gl-struct-immediate-slot))
  (if (storage struct)
      (gl-memref (cffi:inc-pointer (memory-region-pointer (storage struct)) (+ (base-offset struct) (base-offset slot)))
                 (gl-type slot) :layout (layout-standard class))
      (call-next-method)))

(defmethod (setf c2mop:slot-value-using-class) (value (class gl-struct-class) (struct gl-struct) (slot gl-struct-immediate-slot))
  (if (storage struct)
      (setf (gl-memref (cffi:inc-pointer (memory-region-pointer (storage struct)) (+ (base-offset struct) (base-offset slot)))
                       (gl-type slot) :layout (layout-standard class))
            value)
      (call-next-method)))

(defmethod slot-offset ((struct gl-struct) (slot gl-struct-immediate-slot))
  (+ (base-offset struct) (base-offset slot)))

(defmethod slot-offset ((struct gl-struct) (slot symbol))
  (slot-offset struct (or (find slot (c2mop:class-slots (class-of struct)) :key #'c2mop:slot-definition-name)
                          (error "No slot named~%  ~s~%  on~%  ~s" slot struct))))

(defmethod slot-offset ((struct gl-struct-class) (slot symbol))
  (base-offset (or (find slot (c2mop:class-slots struct) :key #'c2mop:slot-definition-name)
                   (error "No slot named~%  ~s~%  on~%  ~s" slot struct))))

(defmethod slot-offset ((name symbol) slot)
  (slot-offset (find-class name) slot))

(defmethod c2mop:slot-definition-allocation ((slot gl-struct-immediate-slot))
  :instance)
;; FIXME: Figure out direct accessor functions

(defmacro define-gl-struct (name &body slots)
  (destructuring-bind (name . options) (enlist name)
    `(defclass ,name (,@(enlist (getf options :include)) gl-struct)
       ,(loop for (slot type . args) in slots
              collect (if type
                          (list* slot :gl-type type args)
                          (list* slot args)))
       (:metaclass gl-struct-class)
       ,@(loop for (k v) on options by #'cddr
               unless (eql k :include) collect (list k v)))))

;;; Only for primitive types.
;;; FIXME: factor out into trivial-* library
(defclass gl-vector (sequences:sequence standard-object)
  ((storage :initarg :storage :accessor storage)
   (base-offset :initform 0 :initarg :base-offset :accessor base-offset)
   (element-count :initarg :element-count :reader sequences:length)
   (element-type :initarg :element-type :reader element-type)
   (stride :initarg :stride :accessor stride)))

(defmethod print-object ((vector gl-vector) stream)
  (print-unreadable-object (vector stream :type T :identity T)
    (format stream "~s ~s" (element-type vector) (length vector))))

(defmethod sequences:elt ((vector gl-vector) index)
  (gl-memref (cffi:inc-pointer (memory-region-pointer (storage vector)) (+ (base-offset vector) (* index (stride vector))))
             (element-type vector)))

(defmethod (setf sequences:elt) (value (vector gl-vector) index)
  (setf (gl-memref (cffi:inc-pointer (memory-region-pointer (storage vector)) (+ (base-offset vector) (* index (stride vector))))
                   (element-type vector))
        value))

(defmethod sequences:make-sequence-iterator ((vector gl-vector) &key from-end start end)
  (let* ((start (or start 0))
         (end (or end (sequences:length vector)))
         (state (if from-end (1- end) start))
         (limit (if from-end (1- start) end))
         (ptr (cffi:inc-pointer (memory-region-pointer (storage vector)) (base-offset vector)))
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
