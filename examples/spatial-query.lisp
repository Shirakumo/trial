(in-package #:org.shirakumo.fraf.trial.examples)

(define-shader-entity ubox (vertex-entity colored-entity transformed-entity)
  ((color :initform (vec 1 0 0 1))
   (vertex-array :initform (// 'trial 'unit-cube))))

(defmethod spaces:bsize ((box ubox))
  (v* (scaling box) .5))

(defmethod spaces:ensure-region ((object ubox) &optional region)
  (unless region
    (setf region (spaces:region 0 0 0 0 0 0)))
  (v<- region (location object))
  (let ((size/2 (v* (scaling object) .5)))
    (nv- region size/2)
    (v<- (spaces:region-size region) size/2)
    (nv* (spaces:region-size region) 2))
  region)

(define-shader-entity ubox* (ubox)
  ((color :initform (vec4 1))))

(defmethod render ((ubox ubox*) (program shader-program))
  (with-pushed-features
    (disable-feature :cull-face)
    (gl:polygon-mode :front-and-back :line)
    (call-next-method)
    (gl:polygon-mode :front-and-back :fill)))

(defclass spatial-structure (trial:array-container entity listener)
  ((spatial-index :initarg :spatial-index :accessor spatial-index)
   (reoptimize :initarg :reoptimize :initform nil :accessor reoptimize)
   (query-region :initarg :query-region :initform (make-instance 'ubox* :scaling (vec3 1)) :accessor query-region)
   (fine :initarg :fine :initform t :accessor fine)
   (dirty :initform NIL :accessor dirty)
   (timing :initform '() :accessor timing)))

(defmethod initialize-instance :after ((structure spatial-structure) &key)
  (enter (query-region structure) structure))

(defmethod enter :after ((box ubox) (structure spatial-structure))
  (setf (dirty structure) T)
  (unless (eql box (query-region structure))
    (spaces:enter box (spatial-index structure))))

(defmethod leave :after ((box ubox) (structure spatial-structure))
  (setf (dirty structure) T)
  (spaces:leave box (spatial-index structure)))

(defmethod clear :after ((structure spatial-structure))
  (setf (dirty structure) T)
  (spaces:clear (spatial-index structure)))

(macrolet ((with-timing ((place key) &body body)
             (let ((nstart (gensym "START"))
                   (nelapsed (gensym "ELAPSED")))
               `(let ((,nstart (get-internal-real-time)))
                  ,@body
                  (let ((,nelapsed (/ (- (get-internal-real-time) ,nstart)
                                      internal-time-units-per-second)))
                    (setf (getf ,place ,key) ,nelapsed))))))

  (defmethod (setf spatial-index) :after ((spatial-index spaces:container) (structure spatial-structure))
    (setf (dirty structure) T)
    (loop :repeat 1
          :do (spaces:clear spatial-index)
              (with-timing ((timing structure) :enter)
                (for:for ((entity over structure))
                  (unless (eql entity (query-region structure))
                    (spaces:enter entity spatial-index))))

              #+no (let ((count 0))
                     (for:for ((entity over structure))
                       (when (< count 7500)
                         (unless (eql entity (query-region structure))
                           (spaces:leave entity spatial-index)
                           (incf count)))))
              (describe spatial-index)
          )
    #+no (if (typep spatial-index 'org.shirakumo.fraf.trial.space.kd-tree::kd-tree)
             (let ((objects '()))
               (for:for ((entity over structure))
                 (unless (eql entity (query-region structure))
                   (push entity objects)))
               (setf objects (coerce objects 'vector))
               (with-timing ((timing structure) :enter)
                 (org.shirakumo.fraf.trial.space.kd-tree::enter-all spatial-index objects)))
             )
    (with-timing ((timing structure) :reoptimize)
      (when (reoptimize structure)
        (spaces:reoptimize spatial-index)))
                                        ; (clouseau:inspect spatial-index)
    )

  (defmethod start-frame ((structure spatial-structure))
    (flet ((box-color (box)
             (if (consp box)
                 (destructuring-bind (box . id) box
                   (let ((r (/ (ldb (byte 20 0) id) (1- (ash 1 20))))
                         (g (/ (ldb (byte 20 20) id) (1- (ash 1 20))))
                         (b (/ (ldb (byte 20 40) id) (1- (ash 1 20)))))
                     (values box r g b 1)))
                 (values box 1 1 0 1))))
      (let ((spatial-index (spatial-index structure))
            (query-region (spaces:ensure-region (query-region structure)))
            (fine (fine structure))
            (timing))
        (with-timing ((timing structure) :all)
          (spaces:do-all (box spatial-index)
            (vsetf (color box) 1 0 0 .02)))
        (with-timing ((timing structure) :candidates)
          (spaces:do-candidates (box spatial-index query-region)
            (multiple-value-bind (box r g b a) (box-color box)
              (vsetf (color box) r g b .15))))
        (with-timing ((timing structure) :overlapping)
          (spaces:do-overlapping (box spatial-index query-region)
            (multiple-value-bind (box r g b a) (box-color box)
              (when (or (not fine) (spaces:region-overlaps-p box query-region))
                (vsetf (color box) 1 g 1 .5)))))
        (with-timing ((timing structure) :contained)
          (spaces:do-contained (box spatial-index query-region)
            (multiple-value-bind (box r g b a) (box-color box)
              (when (or (not fine) (spaces:region-contains-p box query-region))
                (vsetf (color box) 0 1 0 1 ; r g b a
                       )))))
        #+no (with-timing ((timing structure) :intersecting)
          (loop for s2 from 0 to 20 by 1/300
                do (let* ((ray-origin (spaces:location query-region))
                          #+no (s1 (/ (get-internal-real-time)
                                      internal-time-units-per-second))
                          #+no (s2 (mod s1 20))
                          (ray-direction (cond ((<= 0 s2 10)
                                                (let ((p (* 2 pi (/ s2 10))))
                                                  (vec (cos p) (sin p) 0)))
                                               ((<= 10 s2 20)
                                                (let ((p (* 2 pi (/ (- s2 10) 10))))
                                                  (vec (cos p) 0 (sin p)))))))
                     (spaces:do-intersecting (box spatial-index ray-origin ray-direction)
                       (when (or (not fine) (org.shirakumo.fraf.trial.space::ray-intersects-box-p
                                             (the vec3 ray-origin) ray-direction
                                             (v- (spaces:location box) (spaces:bsize box))
                                             (v+ (spaces:location box) (spaces:bsize box))))
                         (vsetf (color box) 0 0 1 1))))))))
    (setf (dirty structure) NIL)))

(defmethod bsize ((structure spatial-structure))
  (scaling (query-region structure)))

(defmethod location ((structure spatial-structure))
  (location (query-region structure)))

(defmethod (setf bsize) ((vec vec3) (structure spatial-structure))
  (setf (dirty structure) T)
  (setf (scaling (query-region structure)) vec))

(defmethod (setf location) ((vec vec3) (structure spatial-structure))
  (setf (dirty structure) T)
  (setf (location (query-region structure)) vec))

(define-handler (spatial-structure tick) ()
  (when (dirty spatial-structure)
    (start-frame spatial-structure)))

(defun make-spatial-index (kind)
  (ecase kind
    (:kd (org.shirakumo.fraf.trial.space.kd-tree:make-kd-tree :split-size 255
                                                              ))
    (:grid (org.shirakumo.fraf.trial.space.grid3:make-grid
            .75 :bsize (vec3 3)))))

(defun enter-boxes (structure shape count)
  (let ((generator (ecase shape
                     (:uniform
                      (lambda ()
                        (make-instance 'ubox :location (vrand (vec3 0) (vec3 6))
                                             :scaling (vec3 0.05))))
                     (:ring
                      (lambda ()
                        (let ((radius (* 3 (+ .5 (random .5))))
                              (angle (random (* 2 pi)))
                              (y (* 3 (+ -.3 (random .6)))))
                          (make-instance 'ubox :location (vec3 (* (cos angle) radius)
                                                               y
                                                               (* (sin angle) radius))
                                               :scaling (vec3 0.05))))))))
    (let ((boxes (map-into (make-array count) generator)))
      (sort boxes #'< :key (lambda (box)
                             (vx (location box))))
      (loop for box across boxes
            do (enter box structure)))))

(defvar *structure*)

(define-example spatial-query
  :title "Spatial Query Tests"
  (enter (make-instance 'render-pass) scene)
  (enter (make-instance 'pivot-camera :radius 7) scene)
  (let ((structure (setf *structure* (make-instance 'spatial-structure
                                                    :name :structure
                                                    :spatial-index (make-spatial-index :kd)))))
    (enter structure scene)
    (enter-boxes structure :uniform 10000)))

(defun timing-as-string (structure)
  (format nil "~:[…~;~:*~(~{~A: ~,0,3Fm~^ ~}~)~]" (timing structure)))

(defmethod setup-ui ((scene spatial-query-scene) panel)
  (let ((structure (node :structure scene))
        (layout (make-instance 'alloy:grid-layout :col-sizes '(120 140 T) :row-sizes '(30)))
        (focus (make-instance 'alloy:vertical-focus-list))
        (index-kind :kd)
        (shape :uniform)
        (cube-count 10000)
        (cube-size 0.01))
    (flet ((re-index ()
             (spaces:clear (spatial-index structure))
             (setf (spatial-index structure) (spatial-index structure))))
      (alloy:enter "index structure" layout :row 0 :col 0)
      (let ((index-kind-select (alloy:represent index-kind 'alloy:combo-set :value-set '(:kd :grid) :layout-parent layout :focus-parent focus)))
        (alloy:on alloy:value (value index-kind-select)
          (setf (spatial-index structure) (make-spatial-index index-kind))))

      (alloy:enter "reoptimize" layout :row 1 :col 0)
      (let ((reoptimize (alloy:represent-with T (make-instance 'alloy:accessor-data :object structure :accessor 'reoptimize) :layout-parent layout :focus-parent focus)))
        (alloy:on alloy:value (value reoptimize)
          (re-index)))

      (alloy:enter "query size" layout :row 2 :col 0)
      (let ((size (alloy:represent-with T (make-instance 'alloy:accessor-data :object structure :accessor #'bsize) :layout-parent layout :focus-parent focus)))
        (alloy:on alloy:value (value size)
          (setf (dirty structure) T)))

      (alloy:enter "location" layout :row 3 :col 0)
      (let ((loc (alloy:represent-with T (make-instance 'alloy:accessor-data :object structure :accessor #'location) :layout-parent layout :focus-parent focus)))
        (alloy:on alloy:value (value loc)
          (setf (dirty structure) T)))

      (alloy:enter "re-check fine" layout :row 4 :col 0)
      (let ((fine (alloy:represent-with T (make-instance 'alloy:accessor-data :object structure :accessor 'fine) :layout-parent layout :focus-parent focus)))
        (alloy:on alloy:value (value fine)
          (setf (dirty structure) T)))

      (alloy:enter "shape" layout :row 5 :col 0)
      (let ((shape-select (alloy:represent shape 'alloy:combo-set :value-set '(:uniform :ring) :layout-parent layout :focus-parent focus)))
        (alloy:on alloy:value (value shape-select)
          (clear structure)
          (enter-boxes structure shape cube-count)
          (re-index)))

      (alloy:enter "cube count" layout :row 6 :col 0)
      (let ((size (alloy:represent cube-count 'alloy:ranged-wheel :range '(10 . 50000) :step 100 :layout-parent layout :focus-parent focus)))
        (alloy:on alloy:value (value size)
          (clear structure)
          (enter-boxes structure shape cube-count)
          (re-index)))

      (alloy:enter "cube size" layout :row 7 :col 0)
      (let ((size (alloy:represent cube-size 'alloy:ranged-wheel :range '(0.001 . 0.5) :step 0.01 :layout-parent layout :focus-parent focus)))
        (alloy:on alloy:value (value size)
          (for:for ((entity over structure))
            (unless (eql entity (query-region structure))
              (setf (scaling entity) (vec3 cube-size))))
          (re-index)))

      (alloy:enter "timing" layout :row 8 :col 0)
      (alloy:represent-with T (make-instance 'alloy:accessor-data :object structure :accessor 'timing-as-string) :layout-parent layout :focus-parent focus))

    (alloy:finish-structure panel layout focus)))

(define-handler (spatial-query-scene mouse-press) (button pos)
  (if (eq button :left)
      (let* ((camera-position (vxyz (org.shirakumo.fraf.math.matrices:m* (minv (view-matrix)) (vec4 0 0 0 1))))
             (camera-forward  (org.shirakumo.fraf.math.matrices:m* (view-matrix) (vec4 0 0 1 0)))
             (camera-forward  (vec (vxy camera-forward) (- (vz camera-forward)))))
        (format *trace-output* "pos ~A~%fwd ~A~%" camera-position camera-forward)
        (spaces:do-intersecting (box (spatial-index (node :structure spatial-query-scene)) camera-position camera-forward)
          (format *trace-output* "Hit ~A~%" box)
          (vsetf (color box) 0 0 1 1)))
      (call-next-method)))
