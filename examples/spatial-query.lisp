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

(defclass spatial-structure (trial:array-container listener)
  ((spatial-index :initarg :spatial-index :accessor spatial-index)
   (reoptimize :initarg :reoptimize :initform nil :accessor reoptimize)
   (query-region :initarg :query-region :initform (make-instance 'ubox* :scaling (vec3 1)) :accessor query-region)
   (fine :initarg :fine :initform t :accessor fine)
   (dirty :initform NIL :accessor dirty)))

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

(defmethod (setf spatial-index) :after ((spatial-index spaces:container) (structure spatial-structure))
  (setf (dirty structure) T)
  (for:for ((entity over structure))
    (unless (eql entity (query-region structure))
      (spaces:enter entity spatial-index)))
  (when (reoptimize structure)
    (spaces:reoptimize spatial-index)))

(defmethod start-frame ((structure spatial-structure))
  (let ((spatial-index (spatial-index structure))
        (query-region (spaces:ensure-region (query-region structure)))
        (fine (fine structure)))
    (spaces:do-all (box spatial-index)
      (vsetf (color box) 1 0 0 .1))
    (spaces:do-overlapping (box spatial-index query-region)
      (when (or (not fine) (spaces:region-overlaps-p box query-region))
        (vsetf (color box) 1 1 0 1)))
    (spaces:do-contained (box spatial-index query-region)
      (when (or (not fine) (spaces:region-contains-p box query-region))
        (vsetf (color box) 0 1 0 1))))
  (setf (dirty structure) NIL))

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

(defclass query-panel (trial-alloy:panel) ())

(defmethod initialize-instance :after ((panel query-panel) &key structure)
  (let ((layout (make-instance 'alloy:grid-layout :col-sizes '(120 140 T) :row-sizes '(30)))
        (focus (make-instance 'alloy:vertical-focus-list))
        (index-kind :kd)
        (shape :uniform)
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
          (enter-boxes structure shape 10000)
          (re-index)))

      (alloy:enter "cube size" layout :row 6 :col 0)
      (let ((size (alloy:represent cube-size 'alloy:ranged-wheel :range '(0.001 . 0.5) :step 0.01 :layout-parent layout :focus-parent focus)))
        (alloy:on alloy:value (value size)
          (for:for ((entity over structure))
            (unless (eql entity (query-region structure))
              (setf (scaling entity) (vec3 cube-size))))
          (re-index))))

    (alloy:finish-structure panel layout focus)))

(defun make-spatial-index (kind)
  (ecase kind
    (:kd (org.shirakumo.fraf.trial.space.kd-tree:make-kd-tree))
    (:grid (org.shirakumo.fraf.trial.space.grid3:make-grid
            0.1 :bsize (vec3 3)))))

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
    (loop repeat count
          for box = (funcall generator)
          do (enter box structure))))

(define-example spatial-query
  :title "Spatial Query Tests"
  (let ((game (make-instance 'render-pass))
        (ui (make-instance 'ui))
        (combine (make-instance 'blend-pass)))
    (connect (port game 'color) (port combine 'a-pass) scene)
    (connect (port ui 'color) (port combine 'b-pass) scene))
  (enter (make-instance 'pivot-camera :radius 7) scene)
  (let ((structure (make-instance 'spatial-structure
                                  :spatial-index (make-spatial-index :kd))))
    (enter structure scene)
    (enter-boxes structure :uniform 10000)
    (trial-alloy:show-panel 'query-panel :structure structure)))
