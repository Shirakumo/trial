(in-package #:org.shirakumo.fraf.trial)

(defclass skeleton-controller ()
  ((pose :accessor pose)
   (skeleton :initform NIL :accessor skeleton)))

(define-transfer skeleton-controller skeleton)

(defmethod initialize-instance :after ((controller skeleton-controller) &key)
  (setf (pose controller) (make-instance 'pose :data controller)))

(defmethod shared-initialize :after ((controller skeleton-controller) slots &key skeleton)
  (when skeleton
    (setf (skeleton controller) skeleton)))

(defmethod (setf skeleton) :around ((skeleton skeleton) (controller skeleton-controller))
  (unless (eq skeleton (skeleton controller))
    (call-next-method)
    (setf (pose controller) (rest-pose* skeleton :data controller)))
  skeleton)

(defmethod observe-load-state :after ((controller skeleton-controller) (asset model) (state (eql :loaded)) (area staging-area))
  (stage controller area))

(defmethod stage :after ((controller skeleton-controller) (area staging-area))
  (stage (skeleton controller) area))

(defmethod (setf mesh) (meshes (entity skeleton-controller)))

(defmethod (setf mesh) :after ((meshes cons) (controller skeleton-controller))
  (dolist (mesh meshes)
    (when (and (skinned-p mesh) (skeleton mesh))
      (cond ((eq (skeleton controller) (skeleton mesh)))
            ((null (skeleton controller))
             (setf (skeleton controller) (skeleton mesh)))
            (T
             (cerror "Update the controller" "Animation controller already bound to skeleton~%  ~a~%which is not the same as~%  ~a~%found on~%  ~a"
                      (skeleton controller) (skeleton mesh) mesh)
             (setf (skeleton controller) (skeleton mesh)))))))

(defstruct (animation-layer
            (:constructor %make-animation-layer (clip pose base)))
  (clip NIL :type clip)
  (pose NIL :type pose)
  (base NIL :type pose)
  (strength 0.0 :type single-float))

(defun make-animation-layer (clip skeleton &key (strength 0.0) (data skeleton))
  (let ((layer (%make-animation-layer
                clip
                (rest-pose* skeleton :data data)
                (instantiate-clip skeleton clip))))
    (setf (strength layer) strength)
    layer))

(defmethod strength ((layer animation-layer))
  (animation-layer-strength layer))

(defmethod (setf strength) (strength (layer animation-layer))
  (let ((clip (animation-layer-clip layer))
        (strength (clamp 0.0 (float strength 0f0) 1.0)))
    (when (/= strength (animation-layer-strength layer))
      (sample (animation-layer-pose layer) clip (+ (start-time clip) (* strength (duration clip))))
      (setf (animation-layer-strength layer) strength))))

(define-accessor-delegate-methods clip (animation-layer-clip animation-layer))
(define-accessor-delegate-methods pose (animation-layer-pose animation-layer))
(define-accessor-delegate-methods base (animation-layer-base animation-layer))

(defclass layer-controller (skeleton-controller)
  ((animation-layers :initform (make-hash-table :test 'equalp) :accessor animation-layers)))

(defmethod shared-initialize :after ((controller layer-controller) slots &key animation-layers)
  (loop for layer in animation-layers
        for (clip . args) = (enlist layer)
        do (apply #'add-layer clip controller args)))

(defmethod describe-object :after ((controller layer-controller) stream)
  (format stream "~&~%Layers:~%")
  (let ((layers (sort (alexandria:hash-table-keys (animation-layers controller)) #'string<)))
    (if layers
        (loop for name in layers
              for layer = (gethash name (animation-layers controller))
              do (format stream "  ~3d% ~s~%" (round (* 100 (animation-layer-strength layer))) name))
        (format stream "  None~%"))))

(defmethod update ((controller layer-controller) tt dt fc)
  (when (next-method-p) (call-next-method))
  (loop for layer being the hash-values of (animation-layers controller)
        do (when (< 0.0 (animation-layer-strength layer))
             (layer-onto (pose controller) (pose controller) (animation-layer-pose layer) (animation-layer-base layer)))))

(defmethod add-animation-layer ((layer animation-layer) (controller layer-controller) &key name strength (if-exists :error))
  (when (gethash name (animation-layers controller))
    (ecase if-exists
      ((:overwrite :supersede :replace))
      (:error
       (cerror "Replace the layer" "An animation layer with the name ~s already exists:~%  ~a"
               name (animation-layer name controller)))
      ((NIL (return-from add-animation-layer NIL)))))
  (when strength (setf (strength layer) (float strength 0f0)))
  (setf (animation-layer name controller) layer))

(defmethod add-animation-layer ((clip clip) (controller layer-controller) &key (strength 0.0) (name (name clip)) (if-exists :error))
  (add-animation-layer (make-animation-layer clip (skeleton controller)
                                             :data controller :strength (float strength 0f0))
                       controller :name name :if-exists if-exists))

(defmethod remove-animation-layer (name (controller layer-controller))
  (remhash name (animation-layers controller)))

(defmethod animation-layer (name (controller layer-controller) &key (if-does-not-exist :error))
  (let ((existing (gethash name (animation-layers controller))))
    (or existing
        (ecase if-does-not-exist
          (:error
           (cerror "Add it based on the clip of the same name." "No such layer ~s" name)
           (add-animation-layer name controller))
          (:create
           (add-animation-layer name controller))
          ((NIL))))))

(defmethod (setf animation-layer) ((layer animation-layer) name (controller layer-controller))
  (setf (gethash name (animation-layers controller)) layer))

(defmethod (setf animation-layer) ((strength real) name (controller layer-controller))
  (let ((layer (gethash name (animation-layers controller))))
    (cond (layer
           (setf (strength layer) (float strength 0f0)))
          (T
           (cerror "Add it based on the clip of the same name." "No such layer ~s" name)
           (add-animation-layer name controller :strength strength)))
    strength))

(defmethod (setf animation-layer) ((null null) name (controller layer-controller))
  (remhash name (animation-layers controller))
  null)

(defstruct (fade-target
            (:constructor make-fade-target (clip pose duration)))
  (pose NIL :type pose)
  (clip NIL :type clip)
  (clock 0.0 :type single-float)
  (duration 0.0 :type single-float)
  (elapsed 0.0 :type single-float))

(defmethod pose ((target fade-target)) (fade-target-pose target))
(defmethod clip ((target fade-target)) (fade-target-clip target))
(defmethod clock ((target fade-target)) (fade-target-clock target))
(defmethod duration ((target fade-target)) (fade-target-duration target))
(defmethod elapsed ((target fade-target)) (fade-target-elapsed target))

(defclass fade-controller (skeleton-controller)
  ((fade-targets :initform (make-array 0 :adjustable T :fill-pointer T) :accessor fade-targets)
   (clip :initarg :clip :initform NIL :accessor clip)
   (clock :initform 0.0 :accessor clock)
   (playback-speed :initarg :playback-speed :initform 1.0 :accessor playback-speed)))

(defmethod describe-object :after ((controller fade-controller) stream)
  (format stream "~&~%Current Clip:~%")
  (if (clip controller)
      (format stream "  ~4f / ~4f ~s~%"
              (clock controller) (duration (clip controller)) (name (clip controller)))
      (format stream "  None~%"))
  (format stream "~&~%Fade Targets:~%")
  (if (< 0 (length (fade-targets controller)))
      (loop for target across (fade-targets controller)
            do (format stream "  ~4f / ~4f ~s~%"
                       (fade-target-clock target) (fade-target-duration target)
                       (name (fade-target-clip target))))
      (format stream "  None~%")))

(defmethod play ((target clip) (controller fade-controller))
  (unless (eq target (clip controller))
    (setf (playback-speed controller) 1.0)
    (setf (fill-pointer (fade-targets controller)) 0)
    (setf (clip controller) target)
    (when (skeleton controller)
      (pose<- (pose controller) (rest-pose (skeleton controller))))
    (setf (clock controller) (start-time target))
    (sample (pose controller) (clip controller) (clock controller))))

(defmethod fade-to ((target clip) (controller fade-controller) &key (duration (blend-duration target)))
  (let ((targets (fade-targets controller)))
    (cond ((or (null (clip controller)) (<= duration 0))
           (play target controller))
          ((and (or (= 0 (length targets))
                    (not (eq target (fade-target-clip (aref targets (1- (length targets)))))))
                (not (eq target (clip controller))))
           (setf (playback-speed controller) 1.0)
           (vector-push-extend (make-fade-target target (rest-pose (skeleton controller)) (float duration 0f0)) targets)))))

(defmethod update ((controller fade-controller) tt dt fc)
  (when (next-method-p) (call-next-method))
  (let ((clip (clip controller)))
    (cond (clip
           (let ((targets (fade-targets controller)))
             (loop for target across targets
                   for i from 0
                   do (when (<= (fade-target-duration target) (fade-target-elapsed target))
                        (setf clip (setf (clip controller) (fade-target-clip target)))
                        (setf (clock controller) (fade-target-clock target))
                        (pose<- (pose controller) (fade-target-pose target))
                        (array-utils:vector-pop-position targets i)
                        (return)))
             (let ((time (sample (pose controller) clip (+ (clock controller) (* (playback-speed controller) dt)))))
               (setf (clock controller) time)
               (when (and (not (loop-p clip))
                          (<= (end-time clip) time)
                          (next-clip clip))
                 (fade-to (next-clip clip) controller))
               (loop for target across targets
                     do (setf (fade-target-clock target) (sample (fade-target-pose target) (fade-target-clip target) (+ (fade-target-clock target) dt)))
                        (incf (fade-target-elapsed target) dt)
                        (let ((time (min 1.0 (/ (fade-target-elapsed target) (fade-target-duration target)))))
                          (blend-into (pose controller) (pose controller) (fade-target-pose target) time))))))
          ((skeleton controller)
           ;; With no active clip, make sure we restore to the rest pose on every update,
           ;; since other controllers may want to modify the pose.
           (pose<- (pose controller) (rest-pose (skeleton controller)))))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant SIMULTANEOUS-MORPHS 8))

(define-gl-struct morph-data
  (count :int :initform 0 :accessor morph-count :reader sequences:length)
  (weights (:array :float #.SIMULTANEOUS-MORPHS) :reader weights)
  (indices (:array :int #.SIMULTANEOUS-MORPHS) :reader indices))

(defclass morph-group ()
  ((name :initform NIL :initarg :name :accessor name)
   (weights :initform #() :initarg :weights :accessor weights)
   (morph-names :initform #() :initarg :morph-names :accessor morph-names)
   (textures :initform (make-array 0 :adjustable T :fill-pointer T) :accessor textures)
   (morph-data :initform NIL :accessor morph-data)))

(defmethod print-object ((morph morph-group) stream)
  (print-unreadable-object (morph stream :type T :identity T)
    (format stream "~s" (name morph))))

(defmethod shared-initialize :after ((morph morph-group) slots &key meshes)
  (unless (morph-data morph)
    (setf (morph-data morph) (make-instance 'uniform-buffer :data-usage :dynamic-draw :binding "morph_data" :struct 'morph-data)))
  (when meshes
    (unless (name morph)
      (setf (name morph) (model-name (first meshes))))
    (when (= 0 (length (weights morph)))
      (setf (weights morph) (make-morph-weights (first meshes))))
    (when (/= (length (morph-names morph)) (length (weights morph)))
      (setf (morph-names morph) (adjust-array (morph-names morph) (length (weights morph)) :initial-element NIL)))
    (dolist (mesh meshes)
      (replace (morph-names morph) (morph-names mesh))
      (unless (find (name mesh) (textures morph) :key #'car :test #'equal)
        (vector-push-extend (cons (name mesh) (make-morph-texture mesh)) (textures morph))))))

(defmethod stage :after ((morph morph-group) (area staging-area))
  (loop for (name . texture) across (textures morph)
        do (stage texture area))
  (stage (morph-data morph) area))

(defmethod find-morph ((mesh mesh-data) (morph morph-group) &optional (errorp T))
  (or (cdr (find (name mesh) (textures morph) :key #'car))
      (when errorp (error "No morph data texture for ~a on ~a" mesh morph))))

(defmethod update-morph-data ((morph morph-group))
  (with-buffer-tx (struct (morph-data morph) :update :write)
    (let ((all-weights (weights morph))
          (weights (weights struct))
          (indices (indices struct))
          (count 0))
      ;; Fill the first 8 directly
      (loop for i from 0 below (min SIMULTANEOUS-MORPHS (length all-weights))
            for weight = (aref all-weights i)
            do (setf (elt indices i) i)
               (setf (elt weights i) weight)
               (when (< 0 weight)
                 (incf count)))
      ;; Now search for bigger ones
      (flet ((find-smallest-index ()
               (let ((small 0))
                 (loop for i from 1 below SIMULTANEOUS-MORPHS
                       do (when (< (elt weights i) (elt weights small))
                            (setf small i)))
                 small)))
        ;; This is basically N^2 but we don't expect to have *that* many
        ;; simultaneous targets anyhow, so it shouldn't be bad in practise
        (loop with smallest = (find-smallest-index)
              for i from SIMULTANEOUS-MORPHS below (length all-weights)
              for weight = (aref all-weights i)
              do (when (< (elt weights smallest) weight)
                   (incf count)
                   (setf (elt weights smallest) weight)
                   (setf (elt indices smallest) i)
                   (setf smallest (find-smallest-index))))
        (setf (morph-count struct) (min SIMULTANEOUS-MORPHS count))))))

(defmethod morph-weight (name (group morph-group))
  (let ((pos (position name (morph-names group) :test #'equal)))
    (if pos
        (aref (weights group) pos)
        (error "No such morph target ~s on ~a" name group))))

(defmethod (setf morph-weight) ((weight real) name (group morph-group))
  (let ((pos (position name (morph-names group) :test #'equal)))
    (if pos
        (setf (aref (weights group) pos) (float weight 0f0))
        (error "No such morph target ~s on ~a" name group))))

(defclass morph-group-controller ()
  ((morph-groups :initform (make-hash-table :test 'eql) :accessor morph-groups)))

(define-transfer morph-group-controller morph-groups)

(defmethod describe-object :after ((entity morph-group-controller) stream)
  (format stream "~&~%Morph Groups:~%")
  (if (< 0 (hash-table-count (morph-groups entity)))
      (loop for group being the hash-values of (morph-groups entity)
            do (format stream "  ~s~%" (name group))
               (loop for weight across (weights group)
                     for name across (morph-names group)
                     for i from 0
                     do (format stream "    ~20s ~5,3f~%" (or name i) weight)))
      (format stream "  None~%")))

(defun update-pose-weights (pose controller)
  (loop for morph being the hash-values of (morph-groups controller)
        do (setf (gethash (name morph) (weights pose)) (weights morph))))

(defmethod (setf mesh) (meshes (entity morph-group-controller)))

(defmethod (setf mesh) :after ((meshes cons) (controller morph-group-controller))
  (let ((new-groups (make-hash-table :test 'eql))
        (groups (morph-groups controller))
        (found NIL))
    (loop for mesh in meshes
          do (when (morphed-p mesh)
               (pushnew mesh (gethash (or (model-name mesh) mesh) new-groups))))
    (loop for name being the hash-keys of new-groups using (hash-value meshes)
          do (setf (gethash name groups) (ensure-instance (gethash name groups) 'morph-group
                                                          :name name :meshes meshes))
             (setf found T))
    (when (and found (slot-boundp controller 'pose))
      (update-pose-weights (pose controller) controller))
    (when (skeleton controller)
      (update-pose-weights (rest-pose (skeleton controller)) controller))))

(defmethod find-morph ((mesh animated-mesh) (controller morph-group-controller) &optional (errorp T))
  (or (gethash (or (model-name mesh) mesh) (morph-groups controller))
      (when errorp (error "No morph for ~a found on ~a" mesh controller))))

(defmethod find-morph ((name symbol) (controller morph-group-controller) &optional (errorp T))
  (or (gethash name (morph-groups controller))
      (when errorp (error "No morph for ~s found on ~a" name controller))))

(defmethod morph-weight (name (controller morph-group-controller))
  (or (loop for group being the hash-values of (morph-groups controller)
            for pos = (position name (morph-names group) :test #'equal)
            do (when pos
                 (return (aref (weights group) pos))))
      (error "No such morph target ~s on ~a" name controller)))

(defmethod (setf morph-weight) ((weight real) name (controller morph-group-controller))
  (let ((found NIL)
        (weight (float weight 0f0)))
    (loop for group being the hash-values of (morph-groups controller)
          for pos = (position name (morph-names group) :test #'equal)
          do (when pos
               (setf (aref (weights group) pos) weight)
               (setf found T)))
    (unless found
      (error "No such morph target ~s on ~a" name controller))
    weight))

(defmethod (setf skeleton) :before ((skeleton skeleton) (entity morph-group-controller))
  (update-pose-weights (rest-pose skeleton) entity))

(defmethod stage :after ((entity morph-group-controller) (area staging-area))
  (loop for morph being the hash-values of (morph-groups entity)
        do (stage morph area)))

(defmethod (setf pose) :after ((pose pose) (entity morph-group-controller))
  (update-pose-weights pose entity))

(defmethod update-morph-data ((entity morph-group-controller))
  (loop for morph-group being the hash-values of (morph-groups entity)
        do (update-morph-data morph-group)))

(defclass fk-controller (skeleton-controller)
  ())

(defgeneric fk-update (fk-controller pose tt dt fc))

(defmethod fk-update ((controller fk-controller) pose tt dt fc))

(defmethod update ((controller fk-controller) tt dt fc)
  (when (next-method-p) (call-next-method))
  (when (skeleton controller)
    (fk-update controller (pose controller) tt dt fc)))

(defclass animation-controller (morph-group-controller fk-controller ik-controller layer-controller fade-controller listener)
  ((updated-on :initform -1 :accessor updated-on)
   (palette :initform #() :accessor palette)
   (palette-texture :initform (make-instance 'texture :target :texture-1d-array :width 3 :height 1 :internal-format :rgba32f :min-filter :nearest :mag-filter :nearest) :accessor palette-texture)
   (palette-data :initform (make-array 0 :element-type 'single-float) :accessor palette-data)))

(defmethod describe-object :after ((entity animation-controller) stream)
  (format stream "~&~%Clips:~%")
  (if (list-clips entity)
      (loop for clip in (list-clips entity)
            do (format stream "  ~s~%" clip))
      (format stream "  None~%"))
  (format stream "~&~%Skeleton:~%")
  (if (skeleton entity)
      (describe-skeleton (skeleton entity) stream)
      (format stream "  No skeleton.~%")))

(defmethod observe-load-state :before ((entity animation-controller) (asset model) (state (eql :loaded)) (area staging-area))
  (dolist (clip (list-clips asset))
    (stage clip area)))

(defmethod find-clip (name (entity animation-controller) &optional (errorp T))
  (if (null (skeleton entity))
      (when errorp (error "No such clip ~s found on ~a" name entity))
      (find-clip name (skeleton entity) errorp)))

(defmethod list-clips ((entity animation-controller))
  (when (skeleton entity)
    (list-clips (skeleton entity))))

(defmethod add-animation-layer (clip-name (entity animation-controller) &rest args &key &allow-other-keys)
  (apply #'add-animation-layer (find-clip clip-name entity) entity args))

(defmethod fade-to ((name string) (entity animation-controller) &rest args &key &allow-other-keys)
  (apply #'fade-to (find-clip name entity) entity args))

(defmethod fade-to ((name symbol) (entity animation-controller) &rest args &key &allow-other-keys)
  (apply #'fade-to (find-clip name entity) entity args))

(defmethod play ((name string) (entity animation-controller))
  (play (find-clip name entity) entity))

(defmethod play ((name symbol) (entity animation-controller))
  (play (find-clip name entity) entity))

(defmethod play ((anything (eql T)) (entity animation-controller))
  (loop for clip being the hash-values of (clips (skeleton entity))
        do (return (play clip entity))))

(defmethod update ((entity animation-controller) tt dt fc)
  (when (/= (updated-on entity) fc)
    (call-next-method)
    (update-palette entity)
    (update-morph-data entity)
    (setf (updated-on entity) fc)))

(defmethod stage :after ((entity animation-controller) (area staging-area))
  (stage (palette-texture entity) area))

(defmethod (setf pose) :after ((pose pose) (entity animation-controller))
  (update-palette entity))

(defmethod (setf ik-system) :after ((system ik-system) name (entity animation-controller))
  ;; Hook up our local transform to the IK system's. Since the identity never changes
  ;; the properties "transfer".
  (setf (slot-value system 'transform) (tf entity)))

(define-handler ((entity animation-controller) (ev tick)) (tt dt fc)
  (update entity tt dt fc))

(defmethod update-palette ((entity animation-controller))
  (when (skeleton entity)
    (let* ((palette (matrix-palette (pose entity) (palette entity)))
           (texinput (%adjust-array (palette-data entity) (* 12 (length (pose entity))) (constantly 0f0)))
           (texture (palette-texture entity))
           (inv (mat-inv-bind-pose (skeleton entity))))
      (mem:with-memory-region (texptr texinput)
        (dotimes (i (length palette) (setf (palette entity) palette))
          (let ((mat (nm* (svref palette i) (svref inv i)))
                (texptr (mem:memory-region-pointer texptr)))
            (cffi:with-pointer-to-vector-data (arrptr (marr4 mat))
              (mem:replace texptr arrptr :start1 (* i 12 4) :end2 (* 12 4))))))
      (setf (palette-data entity) texinput)
      (setf (height texture) (length palette))
      (when (allocated-p texture)
        (resize-buffer-data texture texinput :pixel-type :float :pixel-format :rgba)))))

(defmethod instantiate-prefab :after ((instance animation-controller) asset)
  (do-scene-graph (child instance)
    (when (typep child 'base-animated-entity)
      (unless (skeleton instance)
        (setf (skeleton instance) (skeleton child)))
      (setf (animation-controller child) instance))))

(defclass quat2-animation-controller (animation-controller)
  ())

(defmethod update-palette ((entity quat2-animation-controller))
  ;; FIXME: Update for texture data
  (let ((palette (quat2-palette (pose entity) (palette entity)))
        (inv (quat-inv-bind-pose (skeleton entity))))
    (dotimes (i (length palette) (setf (palette entity) palette))
      (nq* (svref palette i) (svref inv i)))))

(defclass basic-animation-controller (basic-node animation-controller)
  ())
