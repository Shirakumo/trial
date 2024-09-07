(in-package #:org.shirakumo.fraf.trial.alloy)

(defclass language-data (alloy:data)
  ((name :initarg :name :accessor name)))

(defmethod alloy:value ((data language-data))
  (trial:language-string (name data)))

(defmethod (setf alloy:value) (value (data language-data))
  value)

(defmethod alloy:access ((data language-data) (field (eql 'alloy:value)))
  (alloy:value data))

(defmethod (setf alloy:access) (value (data language-data) (field (eql 'alloy:value)))
  value)

(defmethod alloy:refresh ((data language-data))
  (alloy:notify-observers 'alloy:value data (trial:language-string (name data)) data))

(defmethod alloy:expand-compound-place-data ((place (eql 'trial:@)) args)
  (destructuring-bind (name) args
    `(make-instance 'language-data :name ',name)))

(defclass vec (alloy:grid-layout
               alloy:focus-list
               alloy:value-component)
  ((alloy::cell-margins :initform (alloy:margins)))
  (:default-initargs :row-sizes '(20)))

(flet ((handle (event vec)
         (alloy:do-elements (element vec :result (alloy:decline) :from-end T)
           (when (alloy:contained-p (alloy:location event) element)
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
  (let ((els '(math:vx math:vy math:vz math:vw)))
    (alloy:do-elements (element vec)
      (setf (alloy:value element) (funcall (pop els) (alloy:value vec))))))

(defmacro %vec-slot (vec accessor label &body body)
  `(let ((comp (alloy:represent (,accessor object) 'alloy:wheel :step step)))
     (when labels (alloy:enter (alloy:represent ,label 'alloy:label :style '((:label :halign :middle))) ,vec))
     (alloy:enter comp ,vec)
     (alloy:on alloy:value (v comp)
       (declare (ignore v))
       ,@body
       (alloy:notify-observers 'alloy:value vec (alloy:value vec) vec))))

(defclass vec2 (vec)
  ())

(defmethod initialize-instance :after ((vec vec2) &key labels (step 0.1))
  (let ((object (alloy:value vec)))
    (setf (alloy:col-sizes vec) (if labels '(20 T 20 T) '(T T)))
    (%vec-slot vec math:vx2 "X")
    (%vec-slot vec math:vy2 "Y")))

(defmethod alloy:component-class-for-object ((_ math:vec2)) (find-class 'vec2))

(defclass vec3 (vec)
  ())

(defmethod initialize-instance :after ((vec vec3) &key labels (step 0.1))
  (let ((object (alloy:value vec)))
    (setf (alloy:col-sizes vec) (if labels '(20 T 20 T 20 T) '(T T T)))
    (%vec-slot vec math:vx3 "X")
    (%vec-slot vec math:vy3 "Y")
    (%vec-slot vec math:vz3 "Z")))

(defmethod alloy:component-class-for-object ((_ math:vec3)) (find-class 'vec3))

(defclass vec4 (vec)
  ())

(defmethod initialize-instance :after ((vec vec4) &key labels (step 0.1))
  (let ((object (alloy:value vec)))
    (setf (alloy:col-sizes vec) (if labels '(20 T 20 T 20 T 20 T) '(T T T T)))
    (%vec-slot vec math:vx4 "X")
    (%vec-slot vec math:vy4 "Y")
    (%vec-slot vec math:vz4 "Z")
    (%vec-slot vec math:vw4 "W")))

(defmethod alloy:component-class-for-object ((_ math:vec4)) (find-class 'vec4))

(defclass quat (vec)
  ())

(defmethod initialize-instance :after ((vec quat) &key labels (step 0.1))
  (let ((object (alloy:value vec)))
    (setf (alloy:col-sizes vec) (if labels '(20 T 20 T 20 T 20 T) '(T T T T)))
    (%vec-slot vec math:qi "I" (math:nqunit* object))
    (%vec-slot vec math:qj "J" (math:nqunit* object))
    (%vec-slot vec math:qk "K" (math:nqunit* object))
    (%vec-slot vec math:qr "R" (math:nqunit* object))))

(defmethod alloy:component-class-for-object ((_ math:quat)) (find-class 'quat))

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

(define-set-representation language
  :item-text (let ((language (trial::try-find-language alloy:value)))
               (or (first (language-codes:names language)) language))
  (trial:languages))

(define-set-representation asset
  :represents trial:asset
  :item-text (format NIL "~a / ~a"
                     (trial:name (trial:pool alloy:value))
                     (trial:name alloy:value))
  (let ((type (if (alloy:value asset) (type-of (alloy:value asset)) 'trial:asset))
        (values ()))
    (flet ((asset< (a b)
             (if (eq (trial:pool a) (trial:pool b))
                 (string< (trial:name a) (trial:name b))
                 (string< (trial:name (trial:pool a)) (trial:name (trial:pool b))))))
      (dolist (pool (trial:list-pools) (sort values #'asset<))
        (dolist (asset (trial:list-assets pool))
          (when (typep asset type)
            (push asset values)))))))

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
            (when (or (typep resource type)
                      (typep resource 'trial:placeholder-resource))
              (push resource values))))))))

(defmethod (setf alloy:value) :before ((value trial:resource) (resource resource))
  (trial:load value))

(define-set-representation monitor
  :represents trial:monitor
  :item-text (typecase alloy:value (string alloy:value) (T "Default"))
  (list* T (mapcar #'trial:name (trial:list-monitors trial:*context*))))

(defclass video-mode-item (alloy:combo-item)
  ())

(defmethod alloy:text ((item video-mode-item))
  (destructuring-bind (w h &optional r monitor) (alloy:value item)
    (declare (ignore monitor))
    (format nil "~a x ~a~@[ @ ~aHz~]" w h r)))

(defclass video-mode (alloy:combo)
  ((monitor :initarg :monitor :initform trial:*context* :accessor trial:monitor)))

(defmethod (setf monitor) :after (monitor (mode video-mode))
  (alloy:notify-observers 'value-set mode monitor mode))

(defmethod alloy:value-set ((video-mode video-mode))
  (trial:list-video-modes (trial:monitor video-mode)))

(defmethod alloy:text ((video-mode video-mode))
  (destructuring-bind (w h &optional r monitor) (alloy:value video-mode)
    (declare (ignore monitor))
    (if (eql T w)
        (format NIL "Native~@[ @ ~aHz~]" r)
        (format nil "~a x ~a~@[ @ ~aHz~]" w h r))))

(defmethod alloy:combo-item (item (video-mode video-mode))
  (make-instance 'video-mode-item :value item))

(defclass localized-combo-set (alloy:combo-set)
  ((localization :initarg :localization :initform (alloy:arg! :localization) :accessor localization)))

(defmethod alloy:text ((combo localized-combo-set))
  (loop with key = (alloy:value combo)
        for (k v) on (trial:language-string (localization combo)) by #'cddr
        do (when (equal k key) (return v))
        finally (return (princ-to-string key))))

(defclass localized-combo-item (alloy:combo-item)
  ((localization :initarg :localization :initform (alloy:arg! :localization) :accessor localization)))

(defmethod alloy:text ((item localized-combo-item))
  (loop with key = (alloy:value item)
        for (k v) on (trial:language-string (localization item)) by #'cddr
        do (when (equal k key) (return v))
        finally (return (princ-to-string key))))

(defmethod alloy:combo-item (item (combo localized-combo-set))
  (make-instance 'localized-combo-item :value item :localization (localization combo)))
