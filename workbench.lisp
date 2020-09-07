(defpackage #:workbench
  (:use #:cl+trial)
  (:shadow #:launch)
  (:local-nicknames
   (#:harmony #:org.shirakumo.fraf.harmony.user)
   (#:mixed #:org.shirakumo.fraf.mixed))
  (:export #:workbench #:launch))
(in-package #:workbench)

(defclass workbench (main) ()
  (:default-initargs :clear-color (vec 0.25 0.3 0.35 0)))

(defun launch ()
  (trial:launch 'workbench))

(define-pool workbench)

(define-asset (workbench grid) mesh
    (make-line-grid 10 100 100))

(define-asset (workbench sphere) mesh
    (make-sphere 10))

(define-asset (workbench cone) mesh
    (make-cone 10 10))

(define-shader-entity voice (listener vertex-entity located-entity)
  ((vertex-array :initform (// 'workbench 'cone))
   (voice :accessor voice)))

(defmethod initialize-instance :after ((voice voice) &key file)
  (setf (voice voice) (harmony:play file :mixer :effect :repeat T :effects '((mixed:speed-change :speed-factor 1.0))
                                         :location (list (vx (location voice)) (vy (location voice)) (vz (location voice))))))

(defmethod handle ((ev tick) (voice voice))
  (setf (harmony:location (voice voice))
        (list (vx (location voice)) (vy (location voice)) (vz (location voice)))))

(defmethod handle ((ev mouse-scroll) (voice voice))
  (incf (mixed:field :speed-factor (harmony:segment 2 (voice voice)))
        (/ (delta ev) 10)))

(define-shader-entity guy (listener vertex-entity located-entity)
  ((vertex-array :initform (// 'workbench 'sphere))))

(defmethod handle :after ((ev tick) (guy guy))
  (let ((loc (location guy))
        (spd (if (retained :shift) 2.0 0.5)))
    (setf (harmony:location harmony:*server*)
          (list (vx loc) (vy loc) (vz loc)))
    (setf (mixed:min-distance (harmony:segment :effect T)) 20)
    (setf (mixed:max-distance (harmony:segment :effect T)) 200)
    (when (retained :w) (incf (vz loc) spd))
    (when (retained :a) (incf (vx loc) spd))
    (when (retained :s) (decf (vz loc) spd))
    (when (retained :d) (decf (vx loc) spd))))

(define-shader-entity grid (vertex-entity)
  ((vertex-array :initform (// 'workbench 'grid))))

(defmethod initialize-instance :before ((workbench workbench) &key)
  (harmony:maybe-start-simple-server)
  (setf (mixed:min-distance (harmony:segment :effect T)) 20)
  (setf (mixed:max-distance (harmony:segment :effect T)) 200)
  (setf (mixed:attenuation (harmony:segment :effect T)) :linear)
  (setf (mixed:rolloff (harmony:segment :effect T)) 1.0))

(defmethod finalize ((workbench workbench))
  (dolist (voice (harmony:voices T))
    (setf (mixed:done-p voice) T))
  (harmony:end harmony:*server*))

(progn
  (defmethod setup-scene ((workbench workbench) scene)
    (enter (make-instance 'grid) scene)
    (enter (make-instance 'voice :location (vec 100 0 100) :file #p "~/Media/Celeste Original Soundtrack/Lena Raine - Celeste Original Soundtrack - 03 Resurrections.mp3") scene)
    (enter (make-instance 'voice :location (vec -100 0) :file #p "~/Media/Celeste Original Soundtrack/Lena Raine - Celeste Original Soundtrack - 04 Awake.mp3") scene)
    (enter (make-instance 'guy :name :guy) scene)
    (enter (make-instance 'following-camera :target (unit :guy scene) :location (vec 0 100 -100)) scene)
    (enter (make-instance 'render-pass) scene))

  (maybe-reload-scene))
