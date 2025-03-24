(in-package #:org.shirakumo.fraf.trial)

(defclass ik-constraint ()
  ())

(defgeneric apply-constraint (constraint solver i))

(defclass ik-solver (sequences:sequence standard-object)
  ((pose :initform NIL :initarg :pose :accessor pose)
   (ik-chain :initform (make-array 0 :element-type '(unsigned-byte 8)) :accessor ik-chain :reader joints)
   (constraints :initform #() :accessor constraints)
   (iterations :initarg :iterations :initform 15 :accessor iterations)
   (threshold :initarg :threshold :initform 0.00001 :accessor threshold)))

(defmethod shared-initialize :after ((solver ik-solver) slots &key (joints NIL joints-p) (constraints NIL constraints-p))
  (when joints-p (setf (joints solver) joints))
  (when constraints-p (replace (constraints solver) constraints)))

(defmethod print-object ((solver ik-solver) stream)
  (print-unreadable-object (solver stream :type T :identity T)
    (format stream "~s ~s" :length (length (ik-chain solver)))))

(defmethod describe-object :after ((solver ik-solver) stream)
  (terpri stream)
  (loop for i from 0
        for chain across (ik-chain solver)
        for constraint across (constraints solver)
        do (format stream "~d ~3d ~a~%" i chain constraint)))

(defmethod ik-from-skeleton ((skeleton skeleton) leaf-joint &key (type 'fabrik-solver) root-joint length pose constraints)
  (let* ((pose (or pose (rest-pose* skeleton)))
         (parents (parents (rest-pose skeleton)))
         (names (joint-names skeleton))
         (leaf-joint (etypecase leaf-joint
                       (string (position leaf-joint names :test #'string-equal))
                       (integer leaf-joint)))
         (root-joint (etypecase root-joint
                       (string (position root-joint names :test #'string-equal))
                       (integer root-joint)
                       (null (if length
                                 (loop repeat (1+ length)
                                       for parent = leaf-joint then (aref parents parent)
                                       finally (return parent))
                                 0))))
         (chain ())
         (straints ()))
    (loop for parent = leaf-joint then (aref parents parent)
          do (push parent chain)
             (push (or (getf* parent constraints)
                       (getf* (aref names parent) constraints :test #'equal))
                   straints)
          until (= root-joint parent))
    (make-instance type :joints chain
                        :constraints straints
                        :pose pose)))

(defmethod (setf joints) (joints (solver ik-solver))
  (sequences:adjust-sequence solver (length joints))
  (replace (ik-chain solver) joints)
  joints)

(defgeneric solve-for (target solver))

(defmethod sequences:length ((solver ik-solver))
  (length (ik-chain solver)))

(defmethod sequences:adjust-sequence ((solver ik-solver) length &rest args)
  (declare (ignore args))
  (setf (ik-chain solver) (%adjust-array (ik-chain solver) length (constantly 0)))
  (setf (constraints solver) (%adjust-array (constraints solver) length (constantly NIL)))
  solver)

(defmethod sequences:elt ((solver ik-solver) index)
  (svref (joints (pose solver)) (aref (ik-chain solver) index)))

(defmethod (setf sequences:elt) ((transform transform) (solver ik-solver) index)
  (setf (elt (pose solver) (aref (ik-chain solver) index)) transform))

(defmethod (setf sequences:elt) ((joint integer) (solver ik-solver) index)
  (setf (aref (ik-chain solver) index) joint)
  joint)

(defmethod (setf sequences:elt) ((constraint ik-constraint) (solver ik-solver) index)
  (setf (svref (constraints solver) index) constraint)
  constraint)

(defmethod global-transform ((solver ik-solver) i &optional (transform (transform)))
  (global-transform (pose solver) (aref (ik-chain solver) i) transform))

(defmethod solve-for ((target transform) (solver ik-solver))
  (solve-for (tlocation target) solver))

(defclass ccd-solver (ik-solver)
  ())

(defmethod solve-for ((goal vec3) (solver ccd-solver))
  (let* ((chain (ik-chain solver))
         (joints (joints (pose solver)))
         (constraints (constraints solver))
         (size (length chain))
         (last (1- size))
         (threshold2 (expt (threshold solver) 2)))
    (when (< 0 last)
      (flet ((test-goal ()
               (when (<= (vsqrdistance goal (tlocation (global-transform solver last))) threshold2)
                 (return-from solve-for T))))
        (dotimes (i (iterations solver))
          (test-goal)
          (loop for j downfrom (- size 2) to 0
                do (let* ((effector (tlocation (global-transform solver last)))
                          (local (svref joints (aref chain j)))
                          (world (global-transform solver j))
                          (location (tlocation world))
                          (rotation (trotation world))
                          (to-goal (v- goal location))
                          (effector-to-goal (if (< 0.00001 (vsqrlength to-goal))
                                                (qtowards (v- effector location) to-goal)
                                                (quat)))
                          (world-rotated (q* rotation effector-to-goal))
                          (local-rotate (q* world-rotated (qinv rotation))))
                     (q<- (trotation local) (q* local-rotate (trotation local)))
                     (let ((constraint (svref constraints j)))
                       (when constraint
                         (apply-constraint constraint solver j)))
                     (test-goal))))))))

(defclass fabrik-solver (ik-solver)
  ((world-chain :initform #() :accessor world-chain)
   (lengths :initform #() :accessor lengths)))

(defmethod sequences:adjust-sequence :after ((solver fabrik-solver) length &rest args)
  (declare (ignore args))
  (setf (world-chain solver) (%adjust-array (world-chain solver) length #'vec3))
  (setf (lengths solver) (%adjust-array (lengths solver) length (constantly 0.0))))

(defun %ik-chain-to-world (solver)
  (let ((chain (ik-chain solver))
        (world-chain (world-chain solver))
        (lengths (lengths solver)))
    (dotimes (i (length chain) solver)
      (let ((world (global-transform solver i)))
        (v<- (aref world-chain i) (tlocation world))
        (when (<= 1 i)
          (setf (aref lengths i) (vdistance (tlocation world) (aref world-chain (1- i)))))))))

(defun %world-to-ik-chain (solver)
  (let ((chain (ik-chain solver))
        (joints (joints (pose solver)))
        (world-chain (world-chain solver)))
    (loop for i from 0 below (1- (length chain))
          for joint = (aref chain i)
          for next = (global-transform solver (1+ i))
          for world = (global-transform solver i)
          do (let* ((location (tlocation world))
                    (rinv (qinv (trotation world)))
                    (to-next (q* rinv (v- (tlocation next) location)))
                    (to-desired (q* rinv (v- (aref world-chain (1+ i)) location)))
                    (delta (qtowards to-next to-desired)))
               (q<- (trotation (svref joints joint)) (q* delta (trotation (svref joints joint))))))))

(defmethod solve-for ((goal vec3) (solver fabrik-solver))
  (let* ((constraints (constraints solver))
         (world-chain (world-chain solver))
         (lengths (lengths solver))
         (size (length constraints))
         (last (1- size))
         (threshold2 (expt (threshold solver) 2))
         (base (vcopy (aref world-chain 0)))
         (effector (aref world-chain last)))
    (%ik-chain-to-world solver)
    (dotimes (i (iterations solver))
      (when (< (vsqrdistance goal effector) threshold2)
        (return))
      ;; Backwards iteration
      (v<- (aref world-chain last) goal)
      (loop for i downfrom (- size 2) to 0
            for offset = (nv* (nvunit* (v- (aref world-chain i) (aref world-chain (1+ i))))
                              (aref lengths (1+ i)))
            do (v<- (aref world-chain i) (nv+ offset (aref world-chain (1+ i)))))
      ;; Forwards iteration
      (v<- (aref world-chain 0) base)
      (loop for i from 1 below size
            for offset = (nv* (nvunit* (v- (aref world-chain i) (aref world-chain (1- i))))
                              (aref lengths i))
            do (v<- (aref world-chain i) (nv+ offset (aref world-chain (1- i)))))
      ;; Apply constraints
      (dotimes (i size)
        (let ((constraint (svref constraints i)))
          (when constraint
            (%world-to-ik-chain solver)
            (apply-constraint constraint solver i)
            (%ik-chain-to-world solver)))))
    (%world-to-ik-chain solver)
    (< (vsqrdistance goal (tlocation (global-transform solver last))) threshold2)))

(defclass ball-socket-constraint (ik-constraint)
  ((axis :initarg :axis :initform +vz3+ :accessor axis)
   (limit :initarg :limit :initform (/ PI 4) :accessor limit)))

(defmethod apply-constraint ((constraint ball-socket-constraint) solver i)
  (when (< 0 i)
    (let* ((parent (trotation (global-transform solver (1- i))))
           (this (trotation (global-transform solver i)))
           (parent-dir (q* parent (axis constraint)))
           (this-dir (q* this (axis constraint)))
           (angle (vangle parent-dir this-dir)))
      (when (< (limit constraint) angle)
        (let* ((correction (vc parent-dir this-dir))
               (world-space (q* parent (qfrom-angle correction (limit constraint)))))
          (q<- (trotation (elt solver i)) (q* world-space (qinv parent))))))))

(defclass hinge-constraint (ik-constraint)
  ((axis :initarg :axis :initform +vx3+ :accessor axis)
   (min-angle :initarg :min-angle :initform 0.0 :accessor min-angle)
   (max-angle :initarg :max-angle :initform (float (* 2 PI) 0f0) :accessor max-angle)))

(defmethod apply-constraint ((constraint hinge-constraint) solver i)
  (when (< 0 i)
    (let* ((current (q* (trotation (global-transform solver i)) (axis constraint)))
           (desired (q* (trotation (global-transform solver (1- i))) (axis constraint)))
           (rot (trotation (elt solver i))))
      ;; FIXME: allow constraining the angle as well
      (nq* rot (qtowards current desired))
      (setf (qangle rot) (clamp-angle (min-angle constraint) (qangle rot) (max-angle constraint))))))

(defclass ik-system ()
  ((solver :initarg :solver :accessor solver)
   (strength :initarg :strength :initform 1.0 :accessor strength)
   (active-p :initarg :active-p :initform T :accessor active-p)
   (target :initform (vec 0 0 0) :accessor target)
   (transform :initarg :transform :initform (transform) :accessor tf)))

(defmethod print-object ((system ik-system) stream)
  (print-unreadable-object (system stream :type T)
    (format stream "~s ~s ~a ~:[~; ACTIVE~]"
            (length (solver system)) (strength system) (target system) (active-p system))))

(defmethod pose ((system ik-system))
  (pose (solver system)))

(defmethod (setf pose) (pose (system ik-system))
  (setf (pose (solver system)) pose))

(defmethod update ((system ik-system) tt dt fc)
  (let ((target (target system)))
    ;; The target is in global space, transform into local for IK.
    (t*p-inv (transform system) target)
    (solve-for target (solver system))))

(defclass global-ik-system (ik-system)
  ((global-target :initarg :target :initform (vec3) :accessor global-target)))

(defmethod update ((system global-ik-system) tt dt fc)
  (v<- (target system) (global-target system))
  (call-next-method))

(defclass clip-ik-system (ik-system)
  ((clip :initarg :clip :accessor clip)
   (clock :initform 0.0 :accessor clock)))

(defmethod update ((layer clip-ik-system) tt dt fc)
  (setf (clock layer) (sample layer (clip layer) (+ (clock layer) dt) :loop-p T))
  (call-next-method))

(defclass entity-target-ik-system (ik-system)
  ((entity :initarg :entity :accessor entity)
   (offset :initarg :offset :accessor offset)))

(defmethod update ((layer entity-target-ik-system) tt dt fc)
  (v<- (target layer) (location (entity layer)))
  (nv+ (target layer) (offset layer))
  (call-next-method))

(defclass ik-controller ()
  ((ik-systems :initform (make-hash-table :test 'equalp) :accessor ik-systems)
   (target :accessor target)))

(defmethod shared-initialize :after ((controller ik-controller) slots &key skeleton ik-systems)
  (loop for (name . args) in ik-systems
        do (apply #'add-ik-system (or skeleton (skeleton controller)) controller 
                  :name name args)))

(defmethod describe-object :after ((controller ik-controller) stream)
  (terpri stream)
  (format stream "IK Systems:~%")
  (let ((systems (sort (alexandria:hash-table-keys (ik-systems controller)) #'string<)))
    (if systems
        (loop for name in systems
              for system = (ik-system name controller)
              do (format stream "  [~:[ ~;x~]] ~24a ~2d ~4f ~a~%"
                         (active-p system) name (length (solver system)) (strength system) (target system)))
        (format stream "  No IK systems.~%"))))

(defmethod update ((controller ik-controller) tt dt fc)
  (when (next-method-p) (call-next-method))
  (loop for system being the hash-values of (ik-systems controller)
        when (active-p system)
        do (pose<- (pose (solver system)) (target controller))
           (update system tt dt fc)
           (blend-into (target controller) (target controller) (pose system) (strength system))))

(defmethod add-ik-system ((system ik-system) (controller ik-controller) &key (name (arg! :name)))
  (setf (ik-system name controller) system))

(defmethod add-ik-system ((solver ik-solver) (controller ik-controller) &rest args &key (system-type 'global-ik-system) (name (arg! :name)) &allow-other-keys)
  (let ((system (apply #'make-instance system-type :solver solver
                      (remf* args :name :system-type))))
    (setf (ik-system name controller) system)))

(defmethod add-ik-system ((skeleton skeleton) (controller ik-controller) &rest args &key (name (arg! :name)) (joint name) (system-type 'global-ik-system) (solver-type 'fabrik-solver) root-joint length constraints &allow-other-keys)
  (let* ((solver (ik-from-skeleton skeleton joint :type solver-type :root-joint root-joint :length length :constraints constraints))
         (system (apply #'make-instance system-type :solver solver
                       (remf* args :name :joint :system-type :solver-type :root-joint :length :constraints))))
    (setf (ik-system name controller) system)))

(defmethod add-ik-system ((name string) (controller ik-controller) &rest args &key &allow-other-keys)
  (unless (getf args :joint)
    (setf (getf args :joint) name))
  (apply #'add-ik-system (skeleton controller) controller :name name args))

(defmethod remove-ik-system (name (controller ik-controller))
  (setf (ik-system name controller) NIL))

(defmethod ik-system (name (controller ik-controller))
  (gethash name (ik-systems controller)))

(defmethod (setf ik-system) ((system ik-system) name (controller ik-controller))
  (setf (gethash name (ik-systems controller)) system))

(defmethod (setf ik-system) ((null null) name (controller ik-controller))
  (remhash name (ik-systems controller))
  null)
