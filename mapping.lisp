(in-package #:org.shirakumo.fraf.trial)

(define-global +map-key-events+ T)
(define-global +retention-table+ (make-hash-table :test 'eql))
(define-global +analog-table+ (make-hash-table :test 'eql))
(define-global +direction-table+ (make-hash-table :test 'eql))
(defvar *mapping-functions* (make-hash-table :test 'eql))
(defvar *action-mappings* ())

(defun mapping-function (name)
  (gethash name *mapping-functions*))

(defun (setf mapping-function) (mapping name)
  (setf (gethash name *mapping-functions*) mapping))

(defun remove-mapping-function (name)
  (remhash name *mapping-functions*))

(defmacro define-mapping-function (name (loop ev) &body body)
  `(setf (mapping-function ',name)
         (lambda (,loop ,ev)
           (declare (ignorable ,loop))
           ,@body)))

(defun map-event (event loop)
  (when (or +map-key-events+
            (not (typep event 'key-event)))
    (loop for function being the hash-values of *mapping-functions*
          do (funcall function loop event))))

(declaim (inline %retained (setf %retained) retained (setf retained) directional clear-retained))
(defun %retained (id)
  (gethash id +retention-table+ 0))

(defun (setf %retained) (int id)
  (setf (gethash id +retention-table+) int))

(defun retained (id)
  (< 0 (the (signed-byte 8) (gethash id +retention-table+ 0))))

(defun (setf retained) (bool id)
  (setf (gethash id +retention-table+) (if bool 1 0)))

(defun clear-retained (&optional id)
  (if id
      (setf (retained id) NIL)
      (clrhash +retention-table+)))

(defun reset-retained (&optional (scene (scene +main+)))
  (clear-retained)
  (typecase +input-source+
    ((eql :keyboard)
     ;; FIXME: how the heck do we do this?
     )
    (gamepad:device
     (loop for label across gamepad:+labels+
           for button = (gamepad:button label +input-source+)
           for axis = (gamepad:axis label +input-source+)
           do (when button
                (map-event (make-instance 'gamepad-press :button label :device +input-source+)
                           scene))
              (when (<= 0.2 (abs axis))
                (map-event (make-instance 'gamepad-move :axis label :pos axis :old-pos 0.0 :device +input-source+)
                           scene))))))

(defun analog (id)
  (the single-float
       (or (gethash id +analog-table+)
           (setf (gethash id +analog-table+) 0f0))))

(defun (setf analog) (value id)
  (setf (gethash id +analog-table+) (or value 0f0)))

(defun clear-analog (&optional id)
  (if id
      (setf (analog id) 0f0)
      (loop for analog being the hash-keys of +analog-table+
            do (setf (analog id) 0f0))))

(defun directional (id)
  (let ((value (gethash id +direction-table+)))
    (the vec4
         (or value
             (setf (gethash id +direction-table+) (vec 0 0 0 0))))))

(defun clear-directional (&optional id)
  (if id
      (vsetf (directional id) 0 0 0 0)
      (loop for directional being the hash-values of +direction-table+
            do (vsetf directional 0 0 0 0))))

(define-mapping-function retain-generic (loop ev)
  (typecase ev
    (mouse-press
     (setf (retained (button ev)) T))
    (mouse-release
     (setf (retained (button ev)) NIL))
    (key-press
     (setf (retained (key ev)) T)
     (case (key ev)
       ((:left-control :right-control) (setf (retained :control) T))
       ((:left-shift :right-shift) (setf (retained :shift) T))
       ((:left-alt :right-alt) (setf (retained :alt) T))))
    (key-release
     (setf (retained (key ev)) NIL)
     (case (key ev)
       ((:left-control :right-control) (setf (retained :control) NIL))
       ((:left-shift :right-shift) (setf (retained :shift) NIL))
       ((:left-alt :right-alt) (setf (retained :alt) NIL))))))

(defclass action-mapping ()
  ((action-type :initarg :action-type :accessor action-type)
   (event-type :initarg :event-type :accessor event-type)
   (qualifier :initarg :qualifier :accessor qualifier)
   (%action-prototype)))

(defmethod shared-initialize :after ((mapping action-mapping) slots &key)
  (setf (slot-value mapping '%action-prototype) (c2mop:class-prototype (c2mop:ensure-finalized (find-class (action-type mapping))))))

(defmethod print-object ((mapping action-mapping) stream)
  (print-unreadable-object (mapping stream :type T)
    (format stream "~s ~s -> ~s" (event-type mapping) (qualifier mapping) (action-type mapping))))

(defgeneric event-applicable-p (event mapping))
(defgeneric event-active-p (event mapping))
(defgeneric perform-event-mapping (event mapping loop))
(defgeneric from-mapping-description (type action bindings))
(defgeneric to-mapping-description (mapping))
(defgeneric event-from-action-mapping (mapping))
(defgeneric event-to-action-mapping (event action &key))
(defgeneric stratify-action-mapping (mapping))

(defmethod clear ((mapping action-mapping)))

(defmethod active-p ((mapping action-mapping))
  (active-p (slot-value mapping '%action-prototype)))

(defmethod event-applicable-p ((event event) (mapping action-mapping))
  NIL)

(defmethod event-applicable-p ((event key-event) (mapping action-mapping))
  (and (not (repeat-p event))
       (typep event (event-type mapping))
       (find (key event) (qualifier mapping))))

(defmethod event-applicable-p ((event mouse-button-event) (mapping action-mapping))
  (and (typep event (event-type mapping))
       (find (button event) (qualifier mapping))))

(defmethod event-applicable-p ((event gamepad-button-event) (mapping action-mapping))
  (and (typep event (event-type mapping))
       (find (button event) (qualifier mapping))))

(defmethod event-applicable-p ((event gamepad-move) (mapping action-mapping))
  (and (typep event (event-type mapping))
       (find (axis event) (qualifier mapping))))

(defmethod stratify-action-mapping ((mapping action-mapping))
  (loop for qualifier in (qualifier mapping)
        collect (make-instance (type-of mapping) :action-type (action-type mapping)
                                                 :event-type (event-type mapping)
                                                 :qualifier (list qualifier))))

(defmethod event-to-action-mapping (event (action symbol) &rest args &key &allow-other-keys)
  (apply #'event-to-action-mapping event (make-instance action) args))

(defclass digital-mapping (action-mapping)
  ((threshold :initarg :threshold :initform +0.5 :accessor threshold)
   (toggle-p :initarg :toggle-p :initform NIL :accessor toggle-p)))

(defmethod event-active-p ((event gamepad-move) (mapping digital-mapping))
  (let* ((threshold (threshold mapping))
         (old (old-pos event))
         (cur (pos event)))
    (if (< 0.0 threshold)
        (cond ((< old threshold cur)
               :on)
              ((< cur threshold old)
               :off))
        (cond ((< cur threshold old)
               :on)
              ((< old threshold cur)
               :off)))))

(defmethod event-active-p ((event digital-event) (mapping digital-mapping))
  (if (typep event 'press-event)
      :on :off))

(defmethod perform-event-mapping (event (mapping digital-mapping) loop)
  (let ((active-p (event-active-p event mapping))
        (action (action-type mapping)))
    (when active-p
      (let ((active-p (eql :on active-p)))
        (cond ((toggle-p mapping)
               (when active-p
                 (setf (%retained action) (if (retained action) -1 +1))))
              (T
               (when (and (not (retained action)) active-p)
                 (issue loop (make-instance action :source-event event)))
               (typecase event
                 (digital-event
                  (setf (%retained action) (max 0 (+ (%retained action) (if active-p +1 -1)))))
                 (T
                  (setf (%retained action) (if active-p +1 0))))))))))

(defun normalize-mapping-event-type (type)
  (case type
    (key 'key-event)
    (mouse 'mouse-button-event)
    (button 'gamepad-button-event)
    (axis 'gamepad-move)
    (T type)))

(defmethod from-mapping-description ((type (eql 'trigger)) action bindings)
  (when (check-action action)
    (loop for binding in bindings
          collect (destructuring-bind (type &key one-of edge threshold (toggle NIL toggle-p)) binding
                    (make-instance 'digital-mapping
                                   :action-type action
                                   :event-type (normalize-mapping-event-type type)
                                   :qualifier one-of
                                   :threshold (or threshold 0.5)
                                   :toggle-p (if toggle-p toggle (eql :rise-only edge)))))))

(defmethod to-mapping-description ((mapping digital-mapping))
  (list* 'trigger (action-type mapping)
         (append (list (case (event-type mapping)
                         ((key-event key-press) 'key)
                         ((mouse-button-event mouse-press) 'mouse)
                         ((gamepad-button-event gamepad-press) 'button)
                         (gamepad-move 'axis)
                         (T (event-type mapping)))
                       :one-of (qualifier mapping))
                 (unless (subtypep (event-type mapping) 'digital-event)
                   (list :threshold (threshold mapping)))
                 (when (toggle-p mapping) `(:toggle T)))))

(defmethod event-from-action-mapping ((mapping digital-mapping))
  (let ((qualifier (first (qualifier mapping))))
    (ecase (event-type mapping)
      ((key-event key-press key-release) (make-instance 'key-press :key qualifier))
      ((mouse-button-event mouse-press mouse-release) (make-instance 'mouse-press :button qualifier :pos #.(vec 0 0)))
      ((gamepad-button-event gamepad-press gamepad-release) (make-instance 'gamepad-press :device NIL :button qualifier))
      (gamepad-move (make-instance 'gamepad-move :device NIL :axis qualifier :old-pos 0.0 :pos (threshold mapping))))))

(defmethod event-to-action-mapping ((event gamepad-move) (action action) &key (threshold 0.5) toggle-p)
  (make-instance 'digital-mapping
                 :action-type (type-of action)
                 :event-type (type-of event)
                 :qualifier (list (axis event))
                 :threshold threshold
                 :toggle-p toggle-p))

(defmethod event-to-action-mapping ((event digital-event) (action action) &key toggle-p)
  (make-instance 'digital-mapping
                 :action-type (type-of action)
                 :event-type (typecase event
                               (key-event 'key-event)
                               (mouse-button-event 'mouse-button-event)
                               (gamepad-button-event 'gamepad-button-event)
                               (T (type-of event)))
                 :qualifier (list (button event))
                 :toggle-p toggle-p))

(defmethod stratify-action-mapping ((mapping digital-mapping))
  (mapc (lambda (new)
          (setf (threshold new) (threshold mapping))
          (setf (toggle-p new) (toggle-p mapping)))
        (call-next-method)))

(defclass axis-analog-mapping (action-mapping)
  ((event-type :initform 'gamepad-move)
   (dead-zone :initarg :dead-zone :initform 0.1 :accessor dead-zone :accessor threshold)
   (multiplier :initarg :multiplier :initform 1.0 :accessor multiplier)))

(defmethod perform-event-mapping (event (mapping axis-analog-mapping) loop)
  (let* ((pos (pos event))
         (value (if (< (dead-zone mapping) (abs pos)) (* (multiplier mapping) pos) 0.0))
         (action (action-type mapping)))
    (setf (analog action) value)
    (issue loop action :value value)))

(defmethod stratify-action-mapping ((mapping axis-analog-mapping))
  (mapc (lambda (new)
          (setf (dead-zone new) (dead-zone mapping))
          (setf (multiplier new) (multiplier mapping)))
        (call-next-method)))

(defmethod event-from-action-mapping ((mapping axis-analog-mapping))
  (let ((qualifier (first (qualifier mapping))))
    (make-instance 'gamepad-move :device NIL :axis qualifier :old-pos 0.0 :pos (* (multiplier mapping) (dead-zone mapping)))))

(defmethod event-to-action-mapping ((event gamepad-move) (action analog-action) &key (dead-zone 0.1) (multiplier +1.0))
  (make-instance 'axis-analog-mapping
                 :action-type (type-of action)
                 :event-type (type-of event)
                 :qualifier (list (axis event))
                 :dead-zone dead-zone
                 :multiplier multiplier))

(defmethod to-mapping-description ((mapping axis-analog-mapping))
  (list* 'analog (action-type mapping)
         `(axis :one-of ,(qualifier mapping)
                :dead-zone ,(dead-zone mapping)
                :multiplier ,(multiplier mapping))))

(defclass digital-analog-mapping (action-mapping)
  ((high-value :initarg :high-value :initform 1.0 :accessor high-value)
   (low-value :initarg :low-value :initform 0.0 :accessor low-value)
   ;; KLUDGE: I kind of hate having to keep this state here.
   (pressed-p :initform NIL :accessor pressed-p)))

(defmethod clear ((mapping digital-analog-mapping))
  (setf (pressed-p mapping) NIL))

(defmethod perform-event-mapping (event (mapping digital-analog-mapping) loop)
  (let ((action (action-type mapping))
        (value 0.0))
    (cond ((typep event 'press-event)
           (setf (pressed-p mapping) T)
           (setf value (high-value mapping)))
          (T
           (setf (pressed-p mapping) NIL)
           (setf value (low-value mapping))))
    (setf (analog action) value)
    (issue loop action :value value)))

(defmethod stratify-action-mapping ((mapping digital-analog-mapping))
  (mapc (lambda (new)
          (setf (high-value new) (high-value mapping))
          (setf (low-value new) (low-value mapping)))
        (call-next-method)))

(defmethod event-from-action-mapping ((mapping digital-analog-mapping))
  (let ((qualifier (first (qualifier mapping))))
    (ecase (event-type mapping)
      ((key-event key-press key-release) (make-instance 'key-press :key qualifier))
      ((mouse-button-event mouse-press mouse-release) (make-instance 'mouse-press :button qualifier :pos #.(vec 0 0)))
      ((gamepad-button-event gamepad-press gamepad-release) (make-instance 'gamepad-press :device NIL :button qualifier)))))

(defmethod event-to-action-mapping ((event digital-event) (action analog-action) &key (high-value 1.0) (low-value 0.0))
  (make-instance 'digital-analog-mapping
                 :action-type (type-of action)
                 :event-type (type-of event)
                 :qualifier (list (button event))
                 :high-value high-value
                 :low-value low-value))

(defmethod to-mapping-description ((mapping digital-analog-mapping))
  (list* 'analog (action-type mapping)
         `(,(case (event-type mapping)
              ((key-event key-press) 'key)
              ((mouse-button-event mouse-press) 'mouse)
              ((gamepad-button-event gamepad-press) 'button)
              (gamepad-move 'axis)
              (T (event-type mapping)))
           :one-of ,(qualifier mapping)
           :high-value ,(high-value mapping)
           :low-value ,(low-value mapping))))

(defmethod from-mapping-description ((type (eql 'analog)) action bindings)
  (when (check-action action)
    (let ((mappings ()))
      (flet ((add (type &rest initargs)
               (push (apply #'make-instance type :action-type action initargs) mappings)))
        (dolist (binding bindings mappings)
          (destructuring-bind (type &key (multiplier +1.0) one-of (dead-zone 0.1) (low-value 0.0) (high-value 1.0)) binding
            (ecase type
              (axis
               (add 'axis-analog-mapping
                    :qualifier one-of
                    :multiplier multiplier
                    :dead-zone dead-zone
                    :multiplier multiplier))
              ((key button mouse)
               (add 'digital-analog-mapping
                    :event-type (normalize-mapping-event-type type)
                    :qualifier one-of
                    :high-value high-value
                    :low-value low-value))
              #++(point
               (add 'mouse-directional-mapping
                    :scaling (vec (first scaling) (second scaling))
                    :dead-zone dead-zone
                    :timeout timeout))
              (buttons
               (loop for (u d) in one-of
                     do (add 'digital-analog-mapping :event-type 'gamepad-button-event :qualifier (list u) :high-value (+ high-value) :low-value (+ low-value))
                        (add 'digital-analog-mapping :event-type 'gamepad-button-event :qualifier (list d) :high-value (- high-value) :low-value (- low-value))))
              (keys
               (loop for (u d) in one-of
                     do (add 'digital-analog-mapping :event-type 'key-event :qualifier (list u) :high-value (+ high-value) :low-value (+ low-value))
                        (add 'digital-analog-mapping :event-type 'key-event :qualifier (list d) :high-value (- high-value) :low-value (- low-value)))))))))))

(defclass axis-directional-mapping (action-mapping)
  ((event-type :initform 'gamepad-move)
   (dead-zone :initarg :dead-zone :initform 0.1 :accessor dead-zone :accessor threshold)
   (axis :initarg :axis :initform :x :accessor axis)))

(defmethod perform-event-mapping (event (mapping axis-directional-mapping) loop)
  (let* ((pos (pos event))
         (value (if (< (dead-zone mapping) (abs pos)) pos 0.0))
         (action (action-type mapping))
         (vec (directional action)))
    (ecase (axis mapping)
      (:x (setf (vx vec) value))
      (:y (setf (vy vec) value))
      (:z (setf (vz vec) value))
      (:w (setf (vw vec) value)))
    (let ((event (make-event action)))
      (v<- (direction event) vec)
      (issue loop event))))

(defmethod stratify-action-mapping ((mapping axis-directional-mapping))
  (mapc (lambda (new)
          (setf (dead-zone new) (dead-zone mapping))
          (setf (axis new) (axis mapping)))
        (call-next-method)))

(defmethod event-from-action-mapping ((mapping axis-directional-mapping))
  (let ((qualifier (first (qualifier mapping))))
    (make-instance 'gamepad-move :device NIL :axis qualifier :old-pos 0.0 :pos (dead-zone mapping))))

(defmethod event-to-action-mapping ((event gamepad-move) (action directional-action) &key (dead-zone 0.1) (axis :x))
  (make-instance 'axis-directional-mapping
                 :action-type (type-of action)
                 :event-type (type-of event)
                 :qualifier (list (axis event))
                 :dead-zone dead-zone
                 :axis axis))

(defmethod to-mapping-description ((mapping axis-directional-mapping))
  (list* 'directional (action-type mapping)
         `(axis :axis ,(axis mapping) :one-of ,(qualifier mapping) :dead-zone ,(dead-zone mapping))))

(defclass digital-directional-mapping (action-mapping)
  ((high-value :initarg :high-value :initform 1.0 :accessor high-value)
   (low-value :initarg :low-value :initform 0.0 :accessor low-value)
   (axis :initarg :axis :initform :x :accessor axis)
   ;; KLUDGE: I kind of hate having to keep this state here.
   (pressed-p :initform NIL :accessor pressed-p)))

(defmethod clear ((mapping digital-directional-mapping))
  (setf (pressed-p mapping) NIL))

(defmethod maybe-reactivate-other-mapping ((like digital-directional-mapping))
  ;; KLUDGE: This kind of blows since we need to scan all mappings explicitly.
  (dolist (mapping *action-mappings* NIL)
    (when (and (not (eq mapping like))
               (typep mapping 'digital-directional-mapping)
               (eq (action-type mapping) (action-type like))
               (eq (axis mapping) (axis like))
               (pressed-p mapping))
      (setf (aref (varr (directional (action-type mapping)))
                  (ecase (axis mapping)
                    (:x 0) (:y 1) (:z 2) (:w 3)))
            (high-value mapping))
      (return T))))

(defmethod perform-event-mapping (event (mapping digital-directional-mapping) loop)
  (let* ((action (action-type mapping))
         (vec (directional action))
         (axis (ecase (axis mapping)
                 (:x 0) (:y 1) (:z 2) (:w 3))))
    (cond ((typep event 'press-event)
           (setf (pressed-p mapping) T)
           (setf (aref (varr vec) axis) (high-value mapping)))
          ((maybe-reactivate-other-mapping mapping)
           (setf (pressed-p mapping) NIL))
          (T
           (setf (pressed-p mapping) NIL)
           (setf (aref (varr vec) axis) (low-value mapping))))
    (let ((event (make-event action)))
      (v<- (direction event) vec)
      (issue loop event))))

(defmethod stratify-action-mapping ((mapping digital-directional-mapping))
  (mapc (lambda (new)
          (setf (high-value new) (high-value mapping))
          (setf (low-value new) (low-value mapping))
          (setf (axis new) (axis mapping)))
        (call-next-method)))

(defmethod event-from-action-mapping ((mapping digital-directional-mapping))
  (let ((qualifier (first (qualifier mapping))))
    (ecase (event-type mapping)
      ((key-event key-press key-release) (make-instance 'key-press :key qualifier))
      ((mouse-button-event mouse-press mouse-release) (make-instance 'mouse-press :button qualifier :pos #.(vec 0 0)))
      ((gamepad-button-event gamepad-press gamepad-release) (make-instance 'gamepad-press :device NIL :button qualifier)))))

(defmethod event-to-action-mapping ((event digital-event) (action directional-action) &key (high-value 1.0) (low-value 0.0) (axis :x))
  (make-instance 'digital-directional-mapping
                 :action-type (type-of action)
                 :event-type (type-of event)
                 :qualifier (list (button event))
                 :high-value high-value
                 :low-value low-value
                 :axis axis))

(defmethod to-mapping-description ((mapping digital-directional-mapping))
  (list* 'directional (action-type mapping)
         `(,(case (event-type mapping)
              ((key-event key-press) 'key)
              ((mouse-button-event mouse-press) 'mouse)
              ((gamepad-button-event gamepad-press) 'button)
              (gamepad-move 'axis)
              (T (event-type mapping)))
           :axis ,(axis mapping)
           :one-of ,(qualifier mapping)
           :high-value ,(high-value mapping)
           :low-value ,(low-value mapping))))

(defclass mouse-directional-mapping (action-mapping)
  ((event-type :initform 'mouse-move)
   (qualifier :initform ())
   (scaling :initarg :scaling :initform (vec 1 1) :accessor scaling)
   (dead-zone :initarg :dead-zone :initform 0.1 :accessor dead-zone)
   (timeout :initarg :timeout :initform 0.02 :accessor timeout)
   (clock :initarg :clock :initform 0.0 :accessor clock)))

;; TODO: implement mouse smoothing

(defmethod event-applicable-p ((event mouse-move) (mapping mouse-directional-mapping))
  T)

(defmethod event-applicable-p ((event tick) (mapping mouse-directional-mapping))
  T)

(defmethod perform-event-mapping (event (mapping mouse-directional-mapping) loop)
  (let* ((action (action-type mapping))
         (vec (directional action))
         (scaling (scaling mapping))
         (delta (load-time-value (vec 0 0)))
         (dead-zone (dead-zone mapping)))
    (v<- delta (pos event))
    (nv- delta (old-pos event))
    (setf (vx vec) (* (vx scaling) (if (< dead-zone (abs (vx delta))) (vx delta) 0.0)))
    (setf (vy vec) (* (vy scaling) (if (< dead-zone (abs (vy delta))) (vy delta) 0.0)))
    (setf (clock mapping) (timeout mapping))
    (let ((event (make-event action)))
      (v<- (direction event) vec)
      (issue loop event))))

;; KLUDGE: This is necessary to "unstick" the mouse once it stops moving.
(defmethod perform-event-mapping ((event tick) (mapping mouse-directional-mapping) loop)
  (let ((clock (clock mapping)))
    (when (and (< 0.0 clock)
               (<= (decf clock (dt event)) 0.0))
      (let* ((action (action-type mapping))
             (vec (directional action)))
        (setf (vx vec) 0.0)
        (setf (vy vec) 0.0)
        (let ((event (make-event action)))
          (v<- (direction event) vec)
          (issue loop event))))
    (setf (clock mapping) clock)))

(defmethod stratify-action-mapping ((mapping mouse-directional-mapping))
  (mapc (lambda (new)
          (setf (scaling new) (scaling mapping))
          (setf (dead-zone new) (dead-zone mapping)))
        (call-next-method)))

(defmethod event-from-action-mapping ((mapping mouse-directional-mapping))
  (make-instance 'mouse-move :device NIL :old-pos (vec 0 0) :pos (v/ (dead-zone mapping) (scaling mapping))))

(defmethod event-to-action-mapping ((event mouse-move) (action directional-action) &key (scaling (vec 1 1)) (dead-zone 0.1) (timeout 0.02))
  (make-instance 'mouse-directional-mapping
                 :action-type (type-of action)
                 :event-type (type-of event)
                 :qualifier (list (button event))
                 :scaling scaling
                 :dead-zone dead-zone
                 :timeout timeout))

(defmethod to-mapping-description ((mapping mouse-directional-mapping))
  (list* 'directional (action-type mapping)
         `(point :scaling (,(vx (scaling mapping)) ,(vy (scaling mapping))) :timeout ,(timeout mapping) :dead-zone ,(dead-zone mapping))))

(defmethod from-mapping-description ((type (eql 'directional)) action bindings)
  (when (check-action action)
    (let ((mappings ()))
      (flet ((add (type &rest initargs)
               (push (apply #'make-instance type :action-type action initargs) mappings)))
        (dolist (binding bindings mappings)
          (destructuring-bind (type &key (axis :x) one-of (timeout 0.02) (dead-zone 0.1) (low-value 0.0) (high-value 1.0) (scaling '(1 1))) binding
            (ecase type
              (axis
               (add 'axis-directional-mapping
                    :qualifier one-of
                    :axis axis
                    :dead-zone dead-zone))
              ((key button mouse)
               (add 'digital-directional-mapping
                    :event-type (normalize-mapping-event-type type)
                    :qualifier one-of
                    :axis axis
                    :high-value high-value
                    :low-value low-value))
              (point
               (add 'mouse-directional-mapping
                    :scaling (vec (first scaling) (second scaling))
                    :dead-zone dead-zone
                    :timeout timeout))
              (stick
               (loop for (x y) in one-of
                     do (add 'axis-directional-mapping :qualifier (list x) :axis :x :dead-zone dead-zone)
                        (add 'axis-directional-mapping :qualifier (list y) :axis :y :dead-zone dead-zone)))
              (buttons
               (loop for (u l d r) in one-of
                     do (add 'digital-directional-mapping :event-type 'gamepad-button-event :qualifier (list u) :axis :y :high-value (+ high-value) :low-value (+ low-value))
                        (add 'digital-directional-mapping :event-type 'gamepad-button-event :qualifier (list l) :axis :x :high-value (- high-value) :low-value (- low-value))
                        (add 'digital-directional-mapping :event-type 'gamepad-button-event :qualifier (list r) :axis :x :high-value (+ high-value) :low-value (+ low-value))
                        (add 'digital-directional-mapping :event-type 'gamepad-button-event :qualifier (list d) :axis :y :high-value (- high-value) :low-value (- low-value))))
              (keys
               (loop for (u l d r) in one-of
                     do (add 'digital-directional-mapping :event-type 'key-event :qualifier (list u) :axis :y :high-value (+ high-value) :low-value (+ low-value))
                        (add 'digital-directional-mapping :event-type 'key-event :qualifier (list l) :axis :x :high-value (- high-value) :low-value (- low-value))
                        (add 'digital-directional-mapping :event-type 'key-event :qualifier (list r) :axis :x :high-value (+ high-value) :low-value (+ low-value))
                        (add 'digital-directional-mapping :event-type 'key-event :qualifier (list d) :axis :y :high-value (- high-value) :low-value (- low-value)))))))))))

(define-mapping-function input-maps (loop event)
  ;; TODO: This is slow, as we keep iterating over and testing for events that will
  ;;       very likely not change for a long time (comparatively). We should cache
  ;;       the set of applicable mappings depending on active action-sets whenever
  ;;       those change.
  (if (typep event 'lose-focus)
      (mapc #'clear *action-mappings*)
      (dolist (mapping *action-mappings*)
        (when (and (active-p mapping)
                   (event-applicable-p event mapping))
          (perform-event-mapping event mapping loop)))))

(defun compile-mapping (input)
  (let ((mappings ()))
    (dolist (description input)
      (destructuring-bind (type action &rest bindings) description
        (dolist (mapping (from-mapping-description type action bindings))
          (push mapping mappings))))
    (setf *action-mappings* mappings)))

(defun load-mapping (input &key (package *package*))
  (etypecase input
    ((or pathname string)
     (load-mapping (depot:from-pathname input) :package package))
    (depot:entry
     (depot:with-open (tx input :input 'character)
       (load-mapping (depot:to-stream tx) :package package)))
    (stream
     (load-mapping (loop with *package* = package
                         for form = (read input NIL '#1=#:END)
                         until (eq form '#1#)
                         collect form)))
    (list
     (compile-mapping input))))

(defun save-mapping (output &key (package *package*))
  (etypecase output
    (null
     (with-output-to-string (stream)
       (save-mapping stream :package package)))
    ((or pathname string)
     (save-mapping (depot:from-pathname output) :package package))
    (depot:entry
     (depot:with-open (tx output :output 'character)
       (save-mapping (depot:to-stream tx) :package package)))
    (stream
     (let ((descriptions (mapcar #'to-mapping-description *action-mappings*))
           (cache (make-hash-table :test 'equal))
           (*print-case* :downcase)
           (*package* package))
       (dolist (description descriptions)
         (push (cddr description) (gethash (list (first description) (second description)) cache)))
       ;; FIXME: collect based on matching :one-of.
       (let ((descriptions (loop for preamble being the hash-keys of cache
                                 for bindings being the hash-values of cache
                                 collect (append preamble bindings))))
         (loop for (type event . bindings) in descriptions
               do (format output "(~s ~s~{~%  ~s~})~%~%"
                          type event bindings))
         *action-mappings*)))))

(defun find-action-mappings (action &optional (input-event-type 'input-event))
  (let ((triggers ()))
    (loop for mapping in *action-mappings*
          do (when (and (eql action (action-type mapping))
                        (subtypep (event-type mapping) input-event-type))
               (push mapping triggers)))
    triggers))

(defun update-action-mappings (new-mappings &key (prune-event-type T))
  (let ((mappings (append new-mappings
                          (remove-if (lambda (mapping)
                                       (and (find (action-type mapping) new-mappings :key #'action-type)
                                            (subtypep (event-type mapping) prune-event-type)))
                                     *action-mappings*))))
    (mapc #'clear mappings)
    (setf *action-mappings* mappings)))

#| Keymap should have the following syntax:

keymap    ::= mapping*
mapping   ::= (type action trigger*)
type      ::= retain | trigger
trigger   ::= (key one-of edge?)
            | (mouse one-of edge?)
            | (button one-of edge?)
            | (axis one-of edge? threshold?)
one-of    ::= :one-of label
edge      ::= :edge :rise | :edge :fall
threshold ::= :threshold number
action    --- a symbol naming an action event
label     --- a keyword naming a key or button label

Examples:

(trigger quicksave
(label :english "Quick Save")
(key :one-of (:f5)))

(retain dash
(label :english "Dash")
(axis :one-of (:r2) :threshold 0.2))
|#
