(in-package #:org.shirakumo.fraf.trial.gltf)

(defun load-joint-names (gltf)
  (map 'vector #'gltf-name (gltf:nodes gltf)))

(defun load-rest-pose (gltf)
  (let* ((nodes (gltf:nodes gltf))
         (pose (make-instance 'pose :size (length nodes))))
    (loop for i from 0 below (length nodes)
          for node = (aref nodes i)
          do (setf (elt pose i) (gltf-node-transform node))
             (setf (parent-joint pose i) (if (gltf:parent node)
                                             (gltf:idx (gltf:parent node))
                                             -1)))
    (check-consistent pose)
    pose))

(defun load-animation-track (track sampler)
  (setf (interpolation track) (ecase (gltf:interpolation sampler)
                                (:step :constant)
                                (:linear :linear)
                                (:cubicspline :hermite)))
  (setf (frames track) (cons (gltf:input sampler) (gltf:output sampler))))

(defgeneric translate-track-pointer (pointer track gltf))

(defmethod translate-track-pointer ((pointer string) track gltf)
  (change-class track 'trial::slot-value-track
                :slot-name (lispify-name pointer "KEYWORD")
                :name (name track)))

(defmethod translate-track-pointer ((pointer symbol) track gltf)
  (change-class track 'trial::slot-value-track
                :slot-name pointer
                :name (name track)))

(defgeneric translate-effect (name effect gltf))

(defmethod translate-effect ((name string) effect gltf)
  (translate-effect (lispify-name name) effect gltf))

(defmethod translate-effect (name effect gltf)
  (v:warn :trial.gltf "Unknown effect name: ~s, ignoring." name)
  NIL)

;; FIXME: How do we actually translate the pointer to the corresponding lisp-side object slot?
;;        it's unlikely to be what's pointed to by the json pointer, since objects are transformed
;;        to more fitting native representations that should be manipulated instead.

(defun load-clip (gltf animation)
  (let ((clip (make-instance 'clip :name (gltf-name animation))))
    (loop for channel across (gltf:channels animation)
          for target = (gltf:target channel)
          for sampler = (svref (gltf:samplers animation) (gltf:sampler channel))
          for track = (find-animation-track clip (gltf:idx (gltf:node target)) :if-does-not-exist :create)
          do (case (gltf:path target)
               (:translation
                (load-animation-track (location track) sampler))
               (:scale
                (load-animation-track (scaling track) sampler))
               (:rotation
                (load-animation-track (rotation track) sampler))
               (:weights
                (change-class track 'trial::weights-track :name (gltf-name (gltf:node target)))
                (load-animation-track track sampler))
               (:pointer
                (translate-track-pointer (gltf:pointer channel) track gltf)
                (load-animation-track track sampler))
               (T (v:warn :trial.gltf "Unknown animation channel target path: ~s on ~s, ignoring."
                          (gltf:path (gltf:target channel)) (gltf-name animation)))))
    ;; Extra handling for custom properties
    (let* ((extras (gltf:extensions animation))
           (trial (when extras (gethash "SHIRAKUMO_trial" extras)))
           (tracks (when trial (gethash "extraTracks" trial))))
      (when tracks
        (loop for field being the hash-keys of tracks using (hash-value data)
              for track = (find-animation-track clip field :if-does-not-exist :create)
              do (translate-track-pointer field track gltf)
                 (setf (interpolation track) :constant)
                 (setf (frames track) (cons (map 'vector (lambda (f) (float f 0f0)) (gethash "times" data))
                                            (map 'vector (lambda (f) (float f 0f0)) (gethash "values" data)))))))
    (trial::recompute-duration clip)
    (case (gltf:kind animation)
      (:blocking
       (setf (trial:blocking-p clip) T))
      (:physical
       (setf (trial:blocking-p clip) T)
       (change-class clip 'forward-kinematic-clip :velocity-scale (gltf:velocity-scale animation))))
    (if (gltf:next animation)
        (setf (next-clip clip) (trial:lispify-name (gltf:next animation)))
        (setf (loop-p clip) (gltf:loop-p animation)))
    (setf (blend-duration clip) (gltf:blend-duration animation))
    (setf (trial::effects clip)
          (coerce (loop for effect across (gltf:effects animation)
                        for object = (translate-effect (gltf:name effect) effect gltf)
                        when object collect (trial::make-animation-effect
                                             (float (gltf:start effect) 0f0)
                                             (float (or (gltf:end effect) (gltf:start effect)) 0f0)
                                             object))
                  'simple-vector))
    clip))

(defun load-clips (gltf &optional (table (make-hash-table :test 'equal)))
  (loop for animation across (gltf:animations gltf)
        for clip = (load-clip gltf animation)
        do (setf (gethash (name clip) table) clip))
  table)

(defun load-bind-pose (gltf)
  (let* ((rest-pose (load-rest-pose gltf))
         (world-bind-pose (make-array (length rest-pose))))
    (dotimes (i (length world-bind-pose))
      (setf (svref world-bind-pose i) (global-transform rest-pose i)))
    (loop for skin across (gltf:skins gltf)
          for joints = (gltf:joints skin)
          for acc = (gltf:inverse-bind-matrices skin)
          do (loop for i from 0 below (length joints)
                   for inv-bind-matrix = (elt acc i)
                   do (setf (aref world-bind-pose (gltf:idx (svref joints i)))
                            (tfrom-mat (minv inv-bind-matrix)))))
    (let ((bind-pose rest-pose))
      (loop for i from 0 below (length world-bind-pose)
            for current = (svref world-bind-pose i)
            for p = (parent-joint bind-pose i)
            do (setf (elt bind-pose i)
                     (if (<= 0 p)
                         (t+ (tinv (svref world-bind-pose p)) current)
                         current)))
      (check-consistent bind-pose)
      bind-pose)))

(defun load-skeleton (gltf)
  (make-instance 'skeleton :rest-pose (load-rest-pose gltf)
                           :bind-pose (load-bind-pose gltf)
                           :joint-names (load-joint-names gltf)))

