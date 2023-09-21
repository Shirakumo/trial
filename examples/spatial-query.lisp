(in-package #:org.shirakumo.fraf.trial.examples)

(define-shader-entity ubox (vertex-entity colored-entity transformed-entity)
  ((color :initform (vec 1 0 0 1))
   (vertex-array :initform (// 'trial 'unit-cube))))

(defmethod spaces:bsize ((box ubox))
  (scaling box))

(defmethod spaces:ensure-region ((object ubox) &optional region)
  (unless region
    (setf region (spaces:region 0 0 0 0 0 0)))
  (v<- region (location object))
  (nv- region (scaling object))
  (v<- (spaces:region-size region) (scaling object))
  (nv* (spaces:region-size region) 2)
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
  ((q :initarg :q  :accessor q)
   (f :initarg :f :initform (make-instance 'ubox* :scaling (vec3 0.5)) :accessor f)
   (dirty :initform NIL :accessor dirty)))

(defmethod initialize-instance :after ((structure spatial-structure) &key)
  (enter (f structure) structure))

(defmethod enter :after ((box ubox) (structure spatial-structure))
  (setf (dirty structure) T)
  (unless (eql box (f structure))
    (spaces:enter box (q structure))))

(defmethod leave :after ((box ubox) (structure spatial-structure))
  (setf (dirty structure) T)
  (spaces:leave box (q structure)))

(defmethod clear :after ((structure spatial-structure))
  (setf (dirty structure) T)
  (spaces:clear (q structure)))

(defmethod (setf q) :after ((q spaces:container) (structure spatial-structure))
  (setf (dirty structure) T)
  (for:for ((entity over structure))
    (unless (eql entity (f structure))
      (spaces:enter entity q))))

(defmethod start-frame ((structure spatial-structure))
  (spaces:do-all (e (q structure))
    (vsetf (color e) 1 0 0 0.2))
  (spaces:do-overlapping (e (q structure) (f structure))
    (vsetf (color e) 0 1 0 0.2))
  (spaces:do-contained (e (q structure) (f structure))
    (vsetf (color e) 1 1 0 0.2))
  (setf (dirty structure) NIL))

(defmethod bsize ((structure spatial-structure))
  (scaling (f structure)))

(defmethod location ((structure spatial-structure))
  (location (f structure)))

(defmethod (setf bsize) ((vec vec3) (structure spatial-structure))
  (setf (dirty structure) T)
  (setf (scaling (f structure)) vec))

(defmethod (setf location) ((vec vec3) (structure spatial-structure))
  (setf (dirty structure) T)
  (setf (location (f structure)) vec))

(define-handler (spatial-structure tick) ()
  (when (dirty spatial-structure)
    (start-frame spatial-structure)))

(defclass query-panel (trial-alloy:panel) ())

(defmethod initialize-instance :after ((panel query-panel) &key structure)
  (let ((layout (make-instance 'alloy:grid-layout :col-sizes '(100 100 T) :row-sizes '(30)))
        (focus (make-instance 'alloy:vertical-focus-list))
        (cube-size 0.01))
    (alloy:enter "query size" layout :row 0 :col 0)
    (let ((size (alloy:represent-with T (make-instance 'alloy:accessor-data :object structure :accessor #'bsize) :layout-parent layout :focus-parent focus)))
      (alloy:on alloy:value (value size)
        (setf (dirty structure) T)))
    (alloy:enter "location" layout :row 1 :col 0)
    (let ((loc (alloy:represent-with T (make-instance 'alloy:accessor-data :object structure :accessor #'location) :layout-parent layout :focus-parent focus)))
      (alloy:on alloy:value (value loc)
        (setf (dirty structure) T)))
    (alloy:enter "cube size" layout :row 2 :col 0)
    (let ((size (alloy:represent cube-size 'alloy:ranged-wheel :range '(0.001 . 0.5) :step 0.01 :layout-parent layout :focus-parent focus)))
      (alloy:on alloy:value (value size)
        (for:for ((entity over structure))
          (unless (eql entity (f structure))
            (setf (scaling entity) (vec3 cube-size))))
        (spaces:clear (q structure))
        (setf (q structure) (q structure))))
    (alloy:finish-structure panel layout focus)))

(define-example spatial-query
  (let ((game (make-instance 'render-pass))
        (ui (make-instance 'ui))
        (combine (make-instance 'blend-pass)))
    (connect (port game 'color) (port combine 'a-pass) scene)
    (connect (port ui 'color) (port combine 'b-pass) scene))
  (enter (make-instance 'pivot-camera :radius 2.5) scene)
  (let ((structure (make-instance 'spatial-structure
                                  :q (ecase :kd
                                       (:kd (org.shirakumo.fraf.trial.space.kd-tree:make-kd-tree))
                                       (:grid (org.shirakumo.fraf.trial.space.grid3:make-grid 0.01 :bsize (vec3 1)))))))
    (enter structure scene)
    (loop for i from 0 below 1000
          for box = (make-instance 'ubox :location (vrand (vec3 0) (vec3 2)) :scaling (vec3 0.01))
          do (enter box structure))
    (trial-alloy:show-panel 'query-panel :structure structure)))
