(in-package #:org.shirakumo.fraf.trial)

(defstruct (animation-layer
            (:constructor %make-animation-layer (clip pose base)))
  (clip NIL :type clip)
  (pose NIL :type pose)
  (base NIL :type pose)
  (strength 0.0 :type single-float))

(defun make-animation-layer (clip skeleton &key (strength 0.0) (data skeleton))
  (let ((layer (%make-animation-layer
                clip
                (rest-pose* skeleton data)
                (instantiate-clip skeleton clip))))
    (setf (strength layer) strength)
    layer))

(defmethod strength ((layer animation-layer))
  (animation-layer-strength layer))

(defmethod (setf strength) (strength (layer animation-layer))
  (let ((clip (animation-layer-clip layer))
        (strength (clamp 0.0 (float strength 0f0) 1.0)))
    (sample (animation-layer-pose layer) clip (+ (start-time clip) (* strength (duration clip))))
    (setf (animation-layer-strength layer) strength)))

(defclass layer-controller ()
  ((animation-layers :initform (make-hash-table :test 'equalp) :accessor animation-layers)
   (pose :accessor pose)
   (skeleton :initform NIL :accessor skeleton)))

(defmethod shared-initialize :after ((controller layer-controller) slots &key animation-layers)
  (loop for layer in animation-layers
        for (clip . args) = (enlist layer)
        do (apply #'add-layer clip controller args)))

(defmethod describe-object :after ((controller layer-controller) stream)
  (format stream "~&~%Layers:~%")
  (let ((layers (sort (alexandria:hash-table-keys (animation-layers controller)) #'string<)))
    (if layers
        (loop for name in layers
              for layer = (layer name controller)
              do (format stream "  ~3d% ~s~%" (round (* 100 (animation-layer-strength layer))) name))
        (format stream "  None~%"))))

(defmethod update ((controller layer-controller) tt dt fc)
  (when (next-method-p) (call-next-method))
  (loop for layer being the hash-values of (animation-layers controller)
        do (layer-onto (pose controller) (pose controller) (animation-layer-pose layer) (animation-layer-base layer))))

(defmethod add-animation-layer ((layer animation-layer) (controller layer-controller) &key name)
  (setf (layer name controller) layer))

(defmethod add-animation-layer ((clip clip) (controller layer-controller) &key (strength 0.0) (name (name clip)))
  (setf (layer name controller) (make-animation-layer clip (skeleton controller)
                                                      :data controller :strength strength)))

(defmethod remove-animation-layer (name (controller layer-controller))
  (setf (layer name controller) NIL))

(defmethod animation-layer (name (controller layer-controller))
  (gethash name (animation-layers controller)))

(defmethod (setf animation-layer) ((layer animation-layer) name (controller layer-controller))
  (setf (gethash name (animation-layers controller)) layer))

(defmethod (setf animation-layer) ((null null) name (controller layer-controller))
  (remhash name (animation-layers93 controller))
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

(defclass fade-controller ()
  ((fade-targets :initform (make-array 0 :adjustable T :fill-pointer T) :accessor fade-targets)
   (clip :initarg :clip :initform NIL :accessor clip)
   (clock :initform 0.0 :accessor clock)
   (playback-speed :initarg :playback-speed :initform 1.0 :accessor playback-speed)
   (pose :accessor pose)
   (skeleton :initform NIL :accessor skeleton)))

(defmethod shared-initialize :after ((controller fade-controller) slots &key skeleton)
  (when skeleton
    (setf (skeleton controller) skeleton)))

(defmethod (setf skeleton) :after ((skeleton skeleton) (controller fade-controller))
  (setf (pose controller) (rest-pose* skeleton :data controller)))

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
    (pose<- (pose controller) (rest-pose (skeleton controller)))
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
    (when (and clip (skeleton controller))
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
                     (blend-into (pose controller) (pose controller) (fade-target-pose target) time))))))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant SIMULTANEOUS-MORPHS 8))

(define-gl-struct morph-data
  (count :int :initform 0 :accessor morph-count :reader sequence:length)
  (weights (:array :float #.SIMULTANEOUS-MORPHS) :reader weights)
  (indices (:array :int #.SIMULTANEOUS-MORPHS) :reader indices))

(defclass morph-group ()
  ((name :initform NIL :initarg :name :accessor name)
   (weights :initform #() :initarg :weights :accessor weights)
   (textures :initform #() :accessor textures)
   (morph-data :initform (make-instance 'uniform-buffer :data-usage :dynamic-draw :binding NIL :struct 'morph-data) :accessor morph-data)))

(defmethod shared-initialize :after ((morph morph-group) slots &key meshes)
  (when meshes
    (unless (name morph)
      (setf (name morph) (model-name (first meshes))))
    (when (= 0 (length (weights morph)))
      (setf (weights morph) (make-morph-weights (first meshes))))
    (setf (textures morph) (map 'vector #'make-morph-texture meshes))))

(defmethod stage :after ((morph morph-group) (area staging-area))
  (stage (textures morph) area)
  (stage (morph-data morph) area))

(defmethod update-morph-data ((morph morph-group))
  (with-buffer-tx (struct (morph-data morph) :update (if (allocated-p (morph-data morph)) :write))
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

(defclass morph-group-controller ()
  ((morph-groups :initform (make-hash-table :test 'eql) :accessor morph-groups)))

(defmethod describe-object :after ((entity morph-group-controller) stream)
  (format stream "~&~%Morph Groups:~%")
  (if (< 0 (hash-table-count (morph-groups entity)))
      (loop for group being the hash-values of (morph-groups entity)
            do (format stream "  ~20a ~a~%" (name group) (weights group)))
      (format stream "  None~%")))

(defmethod (setf model) :after ((asset asset) (entity morph-group-controller))
  (when (loaded-p asset)
    (let ((groups (make-hash-table :test 'eql)))
      (loop for mesh being the hash-values of (meshes asset)
            do (when (morphed-p mesh)
                 (push mesh (gethash (or (model-name mesh) mesh) groups))))
      (loop for name being the hash-keys of groups using (hash-value meshes)
            do (setf (gethash name groups) (make-instance 'morph-group :name name :meshes meshes)))
      (setf (morph-groups entity) groups))))

(defmethod find-morph ((mesh animated-mesh) (entity morph-group-controller) &optional (errorp T))
  (or (gethash (or (model-name mesh) mesh) (morph-groups entity))
      (when errorp (error "No morph for ~a found on ~a" mesh entity))))

(defmethod find-morph ((name symbol) (entity morph-group-controller) &optional (errorp T))
  (or (gethash name (morph-groups entity))
      (when errorp (error "No morph for ~s found on ~a" name entity))))

(defmethod (setf skeleton) :before ((skeleton skeleton) (entity morph-group-controller))
  (loop for morph being the hash-values of (morph-groups entity)
        do (setf (gethash (name morph) (weights (rest-pose skeleton))) (weights morph))))

(defmethod stage :after ((entity morph-group-controller) (area staging-area))
  (loop for morph being the hash-values of (morph-groups entity)
        do (stage morph area)))

(defmethod (setf pose) :after ((pose pose) (entity morph-group-controller))
  (loop for morph being the hash-values of (morph-groups entity)
        do (setf (gethash (name morph) (weights pose)) (weights morph))))

(defmethod update-morph-data ((entity morph-group-controller))
  (loop for morph-group being the hash-values of (morph-groups entity)
        do (update-morph-data morph-group)))

(defclass animation-controller (morph-group-controller ik-controller layer-controller fade-controller listener)
  ((model :initform NIL :accessor model)
   (updated-on :initform -1 :accessor updated-on)
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
  (describe-skeleton (skeleton entity) stream))

(defmethod observe-load-state ((entity animation-controller) (asset model) (state (eql :loaded)) (area staging-area))
  (setf (model entity) asset))

(defmethod (setf model) :after ((asset asset) (entity animation-controller))
  (when (loaded-p asset)
    (setf (skeleton entity) (skeleton asset)))
  (play (or (clip entity) T) entity))

(defmethod find-clip (name (entity animation-controller) &optional (errorp T))
  (if (null (model entity))
      (when errorp (error "No such clip ~s found on ~a" name entity))
      (find-clip name (model entity) errorp)))

(defmethod list-clips ((entity animation-controller))
  (when (model entity)
    (list-clips (model entity))))

(defmethod add-layer (clip-name (entity animation-controller) &key (name NIL name-p))
  (let ((clip (find-clip clip-name entity)))
    (add-layer clip entity :name (if name-p name (name clip)))))

(defmethod fade-to ((name string) (entity animation-controller) &rest args &key &allow-other-keys)
  (apply #'fade-to (find-clip name entity) entity args))

(defmethod fade-to ((name symbol) (entity animation-controller) &rest args &key &allow-other-keys)
  (apply #'fade-to (find-clip name entity) entity args))

(defmethod play ((name string) (entity animation-controller))
  (play (find-clip name entity) entity))

(defmethod play ((name symbol) (entity animation-controller))
  (play (find-clip name entity) entity))

(defmethod play ((anything (eql T)) (entity animation-controller))
  (loop for clip being the hash-values of (clips (model entity))
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
  (let* ((palette (matrix-palette (pose entity) (palette entity)))
         (texinput (%adjust-array (palette-data entity) (* 12 (length (pose entity))) (constantly 0f0)))
         (texture (palette-texture entity))
         (inv (mat-inv-bind-pose (skeleton (model entity)))))
    (mem:with-memory-region (texptr texinput)
      (dotimes (i (length palette) (setf (palette entity) palette))
        (let ((mat (nm* (svref palette i) (svref inv i)))
              (texptr (mem:memory-region-pointer texptr)))
          (cffi:with-pointer-to-vector-data (arrptr (marr4 mat))
            (static-vectors:replace-foreign-memory
             (cffi:inc-pointer texptr (* i 12 4)) arrptr (* 12 4))))))
    (setf (palette-data entity) texinput)
    (setf (height texture) (length palette))
    (when (allocated-p texture)
      (resize-buffer-data texture texinput :pixel-type :float :pixel-format :rgba))))

(defmethod instantiate-prefab :before ((instance animation-controller) (asset model))
  (setf (model instance) asset))

(defmethod instantiate-prefab :after ((instance animation-controller) asset)
  (do-scene-graph (child instance)
    (when (typep child 'base-animated-entity)
      (setf (animation-controller child) instance))))

(defclass quat2-animation-controller (animation-controller)
  ())

(defmethod update-palette ((entity quat2-animation-controller))
  ;; FIXME: Update for texture data
  (let ((palette (quat2-palette (pose entity) (palette entity)))
        (inv (quat-inv-bind-pose (skeleton (model entity)))))
    (dotimes (i (length palette) (setf (palette entity) palette))
      (nq* (svref palette i) (svref inv i)))))
