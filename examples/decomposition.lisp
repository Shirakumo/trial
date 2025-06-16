(in-package #:org.shirakumo.fraf.trial.examples)

(define-shader-entity decomposition-entity (vertex-entity colored-entity transformed-entity)
  ((hull :initarg :hull :reader hull)
   (original-color :initarg :original-color :accessor original-color)
   (polygon-mode :initarg :polygon-mode :initform nil :accessor polygon-mode)
   (panel :initarg :panel :accessor panel)
   (scene :initarg :scene :accessor scene)
   (visible-p :initarg :visible-p :initform T :accessor visible-p))
  (:inhibit-shaders (colored-entity :fragment-shader)))

(defun debug-hull (entity)
  (check-type entity decomposition-entity)
  (let* ((hull       (hull entity))
         (debug-info (org.shirakumo.fraf.convex-covering::debug-info hull)))
    (getf debug-info :hull)))

(defun debug-patch (entity)
  (check-type entity decomposition-entity)
  (let* ((hull       (hull entity))
         (debug-info (org.shirakumo.fraf.convex-covering::debug-info hull)))
    (getf debug-info :patch)))

(defmethod spaces:location ((object decomposition-entity))
  (spaces:location (debug-hull object)))

(defmethod spaces:bsize ((object decomposition-entity))
  (spaces:bsize (debug-hull object)))

(define-class-shader (decomposition-entity :fragment-shader)
  "in vec3 v_view_position;
in vec3 v_world_position;
uniform vec4 objectcolor;
out vec4 color;

void main(){
  vec3 normal = cross(dFdx(v_view_position), dFdy(v_view_position));
  normal = normalize(normal * sign(normal.z));

  // Shitty phong diffuse lighting
  vec3 light_dir = normalize(vec3(20, 15, 0) - v_world_position);
  vec3 radiance = vec3(0.75) * (objectcolor.xyz * max(dot(normal, light_dir), 0));
  radiance += vec3(0.4) * objectcolor.xyz;
  color = vec4(radiance, objectcolor.w);
}")

(defmethod render-with :around ((pass render-pass) (entity decomposition-entity) (program shader-program))
  (when (visible-p entity)
    (gl:polygon-mode :front-and-back (or (polygon-mode entity)
                                         (polygon-mode (scene +main+))))
    (call-next-method)
    (gl:polygon-mode :front-and-back :fill)))

(defclass mesh-item (alloy:combo-item) ())
(defmethod alloy:text ((item mesh-item))
  (princ-to-string (name (alloy:value item))))

(defmethod alloy:combo-item ((item mesh-data) (combo alloy:combo))
  (make-instance 'mesh-item :value item))

(define-example decomposition
  :title "Convex Hull Decomposition"
  :description "Test workbench for the automated convex decomposition feature."
  :superclasses (alloy:observable)
  :slots ((model :initform NIL :accessor model)
          (mesh :initform NIL :accessor mesh)
          (polygon-mode :initform :fill :accessor polygon-mode)
          (show-original :initform NIL :accessor show-original)
          (show-decomposition :initform NIL :accessor show-decomposition)
          (file :initform NIL :accessor file)
          ;; Decomposition
          (prefer-symmetric-p :initform NIL :accessor prefer-symmetric-p)
          (patch-size-limit :initform 1000 :accessor patch-size-limit)
          (tolerance :initform -4.0 :accessor tolerance)
          ;; Debugging
          (index :initform (org.shirakumo.fraf.trial.space.kd-tree:make-kd-tree
                            :dimensions 3)
                 :reader index)
          (selection-buffer :initform (make-instance 'selection-buffer) :accessor selection-buffer)
          (selection1 :initform '() :accessor selection1)
          (selection2 :initform '() :accessor selection2)
          (debug-entities :initform '() :accessor debug-entities)
          (context :initform nil :accessor context))
  (enter (make-instance 'render-pass) scene)
  ;; The grid can be confusing when debugging
  ;; (enter (make-instance 'vertex-entity :vertex-array (// 'trial 'grid)) scene)
  (enter (make-instance 'editor-camera :location (VEC3 0.0 2.3 10) :fov 50 :move-speed 0.1) scene)
  (enter (make-instance 'basic-node :name :container) scene))

(defun selected-patch1 (scene)
  (first (selection1 scene)))

(defun selected-patch2 (scene)
  (first (selection2 scene)))

(alloy:define-observable (setf model) (value alloy:observable))
(alloy:define-observable (setf mesh) (value alloy:observable))

(defmethod setup-ui ((scene decomposition-scene) panel)
  (let ((layout (make-instance 'alloy:grid-layout :col-sizes '(120 140 T) :row-sizes '(30)))
        (focus (make-instance 'alloy:vertical-focus-list)))
    (alloy:enter "Load Model" layout :row 0 :col 0)
    (let ((button (alloy:represent "..." 'alloy:button :layout-parent layout :focus-parent focus)))
      (alloy:on alloy:activate (button)
        (let ((file (org.shirakumo.file-select:existing :title "Load Model File..."
                                                        :filter '(("Model file" "obj" "gltf" "glb"))
                                                        :default (file scene))))
          (when file (setf (file scene) file)))))
    (alloy:enter "Mesh" layout :row 1 :col 0)
    (let ((selector (alloy:represent (mesh scene) 'alloy:combo-set :value-set () :layout-parent layout :focus-parent focus)))
      (alloy:on model (model scene)
        (let ((meshes (if (typep model 'model) (list-meshes model) ())))
          (setf (alloy:value-set selector) meshes)
          (when meshes (setf (alloy:value selector) (first meshes)))))
      (alloy:on alloy:value (mesh selector)
        (setf (mesh scene) mesh)
        (setf (show-original scene) t
              (show-decomposition scene) nil)))
    (alloy:enter "Show Original" layout :row 2 :col 0)
    (alloy:represent (show-original scene) 'alloy:switch :layout-parent layout :focus-parent focus)
    (alloy:enter "Show Decomposition" layout :row 3 :col 0)
    (alloy:represent (show-decomposition scene) 'alloy:switch :layout-parent layout :focus-parent focus)
    (alloy:enter "Wireframe" layout :row 4 :col 0)
    (alloy:represent (polygon-mode scene) 'alloy:switch :layout-parent layout :focus-parent focus
                                                        :on :line :off :fill)

    (alloy:enter "Prefer symmetric" layout :row 5 :col 0)
    (alloy:represent (prefer-symmetric-p scene) T :layout-parent layout :focus-parent focus)

    (alloy:enter "Patch size limit" layout :row 6 :col 0)
    (alloy:represent (patch-size-limit scene) 'alloy:ranged-wheel :range '(3 . 1000) :step 1 :layout-parent layout :focus-parent focus)

    (alloy:enter "Tolerance (log)" layout :row 7 :col 0)
    (alloy:represent (tolerance scene) 'alloy:ranged-wheel :range '(-5.0 . 5.0) :step .1 :layout-parent layout :focus-parent focus)

    ;; Reduce startup time by not loading a model
    ;; (load (assets:asset :woman))
    ;; (setf (model scene) (assets:asset :woman))
    (alloy:finish-structure panel layout focus)))

(defmethod stage :after ((scene decomposition-scene) (area staging-area))
  (stage (selection-buffer scene) area))

(defmethod (setf file) :before (file (scene decomposition-scene))
  ;; Model files may use features that are not supported in this
  ;; context (animation, physics, custom classes, etc.). Try to
  ;; recover from errors related to unsupported features and continue
  ;; loading the model.
  (setf (model scene)
        (handler-bind
            ((error (lambda (condition)
                      (warn "~@<While loading ~S, encountered ~A Trying to ~
                             continue.~@:>"
                            file condition)
                      (continue))))
          (generate-resources 'model-loader file))))

(defmethod (setf show-original) :after (value (scene decomposition-scene))
  (let ((orig (node :original scene)))
    (when orig (setf (visible-p orig) value))))

(defmethod (setf show-decomposition) :after (value (scene decomposition-scene))
  (when value
    (clear (node :container scene)) ; TODO(jmoringe): keep original mesh
    (spaces:clear (index scene))
    (let* ((mesh (mesh scene))
           (all-vertices (reordered-vertex-data mesh '(location)))
           (all-faces (faces mesh)))
      (multiple-value-bind (hulls context)
          (let ((org.shirakumo.fraf.convex-covering::*debug-output* nil)
                (org.shirakumo.fraf.convex-covering::*debug-visualizations* nil))
            (org.shirakumo.fraf.convex-covering:decompose
             all-vertices all-faces
             :merge-cost (append '(org.shirakumo.fraf.convex-covering::/compactness)
                                 (when (prefer-symmetric-p scene)
                                   '(org.shirakumo.fraf.convex-covering::patch-size-symmetry))
                                 (when (< (patch-size-limit scene) 1000)
                                   (list (org.shirakumo.fraf.convex-covering::make-patch-size-limit
                                          (patch-size-limit scene)))))
             :tolerance (expt 10 (tolerance scene))))
        (setf (context scene) context)
        (loop for hull across hulls
              for vertices = (org.shirakumo.fraf.convex-covering:vertices hull)
              for faces = (trial::simplify (org.shirakumo.fraf.convex-covering:faces hull) '(unsigned-byte 16))
              for (name . color) in (apply #'alexandria:circular-list (colored:list-colors))
              for color* = (vec (colored:r color) (colored:g color) (colored:b color))
              ;; Don't destroy the vertex array of the hull since it
              ;; is needed for debugging.
              for mesh = (make-convex-mesh :vertices (copy-seq vertices) :faces faces)
              for entity = (make-instance 'decomposition-entity
                                          :transform (tfrom-mat (primitive-local-transform mesh))
                                          :hull hull
                                          :scene scene
                                          :original-color color*
                                          :color color*
                                          :visible-p (show-decomposition scene)
                                          :vertex-array (make-vertex-array mesh NIL))
              do (enter entity (node :container scene))
                 (enter entity (selection-buffer scene))
                 (enter entity (index scene)))))
    (commit (scene +main+) (loader +main+))))

(defmethod (setf mesh) :before ((mesh mesh-data) (scene decomposition-scene))
  (clear (node :container scene))
  (spaces:clear (index scene))
  (let* ((all-vertices (reordered-vertex-data mesh '(location)))
         (all-faces (faces mesh))
         (mesh (make-general-mesh :vertices all-vertices
                                  :faces (trial::simplify all-faces '(unsigned-byte 16)))))
    (enter (make-instance 'decomposition-entity
                          :name :original
                          :scene scene
                          :transform (t+ (tfrom-mat (primitive-local-transform mesh))
                                         (transform (vec -3 0 0)))
                          :color (vec 1 1 1 0.5)
                          :visible-p (show-original scene)
                          :vertex-array (make-vertex-array mesh NIL))
           (node :container scene)))
  (commit (scene +main+) (loader +main+)))

(defun clear-debug-entities (scene)
  (debug-clear)
  (let ((container (node :container scene)))
    (map nil (lambda (entity) (leave entity container))
         (debug-entities scene)))
  (setf (debug-entities scene) '()))

(defun add-debug-hull (patch color scene &key (polygon-mode :fill))
  (let ((hull (org.shirakumo.fraf.convex-covering::patch-hull patch)))
    (when hull
      (loop for (centroid . normal) in (org.shirakumo.fraf.convex-covering::hull-facet-normals
                                        hull)
            do (debug-line (vec centroid) (vec (v+ centroid (v* normal .5)))
                           :container (node :container scene)
                           :color (v* color .4))))

    (multiple-value-bind (vertices faces)
        (if hull
            (values (org.shirakumo.fraf.convex-covering::hull-vertices hull)
                    (org.shirakumo.fraf.convex-covering::hull-facets hull))
            (values (org.shirakumo.fraf.convex-covering::context-vertices (context scene))
                    (org.shirakumo.fraf.convex-covering::patch-faces patch)))
      (multiple-value-bind (vertices faces)
          (org.shirakumo.fraf.manifolds:normalize
           (map-into (make-array (length vertices) :element-type 'single-float)
                     (lambda (c) (coerce c 'single-float))
                     vertices)
           faces
           :threshold .000001)
        (let* ((mesh (make-convex-mesh :vertices (copy-seq vertices)
                                       :faces (trial::simplify faces '(unsigned-byte 16))))
               (transform (tfrom-mat (primitive-local-transform mesh)))
               (entity (make-instance 'decomposition-entity :hull patch
                                                            :scene scene
                                                            :transform transform
                                                            :original-color color
                                                            :color color
                                                            :visible-p t
                                                            :polygon-mode polygon-mode
                                                            :vertex-array (make-vertex-array mesh NIL))))
          (push entity (debug-entities scene))
          (enter entity (node :container scene))
          entity)))))

(defun patch-merged-patches (patch)
  (let* ((patch (hull patch))
         (link  (org.shirakumo.fraf.convex-covering::patch-link patch))
         (a     (org.shirakumo.fraf.convex-covering::patch-link-a link))
         (b     (org.shirakumo.fraf.convex-covering::patch-link-b link)))
    (values a b)))

(defun explain-merge (patch decomposition-panel)
  (let ((link (org.shirakumo.fraf.convex-covering::patch-link patch)))
    (when link
      (let ((a (org.shirakumo.fraf.convex-covering::patch-link-a link))
            (b (org.shirakumo.fraf.convex-covering::patch-link-b link)))
        (add-debug-hull a (vec 0 1 0 1) decomposition-panel :polygon-mode :line)
        (add-debug-hull b (vec 1 0 0 1) decomposition-panel :polygon-mode :line)
        (spaces:do-all (patch (index decomposition-panel))
          (setf (color patch) (vec .3 .3 .3)
                (polygon-mode patch) :line))))))

(define-handler (decomposition-scene mouse-press :after) (button pos)
  (case button
    (:left
     (let ((object (select pos (selection-buffer decomposition-scene))))
       (setf (selection1 decomposition-scene) (if object (list object) '()))))
    (:right
     (let ((object (select pos (selection-buffer decomposition-scene))))
       (setf (selection2 decomposition-scene) (if object (list object) '())))))
  (case button
    ((:left :right)
     (clear-debug-entities decomposition-scene)
     (let ((selected1 (selected-patch1 decomposition-scene))
           (selected2 (selected-patch2 decomposition-scene)))
       (spaces:do-all (patch (index decomposition-scene))
         (setf (visible-p patch) t)
         (cond ((or (eq patch selected1) (eq patch selected2))
                (setf (polygon-mode patch) :fill))
               (t
                (setf (color patch) (original-color patch)
                      (polygon-mode patch) nil))))))))

(defun draw-annotations (annotations scene)
  (let ((container (node :container scene)))
    (labels ((draw (function color &rest arguments)
               (let ((color (apply #'vec color)))
                 (apply function (append arguments (list :color     color
                                                         :container container)))))
             (point (position color)
               (draw #'debug-point color (vec position)))
             (line (start end color)
               (draw #'debug-line color (vec start) (vec end))))
      (loop for (kind . data) in annotations
            do (case kind
                 (:point
                  (destructuring-bind (position &key (color '(1 0 0)))
                      (if (listp data)
                          data
                          (list data))
                    (point position color)))
                 (:line
                  (destructuring-bind (start end &key (color '(1 0 0))) data
                    (line start end color)))
                 (:triangle
                  (destructuring-bind (a b c &key (color '(1 0 0))) data
                    (line a b color)
                    (line b c color)
                    (line c a color))))))))

(define-handler (decomposition-scene text-entered :after) (text)
  (labels ((inspect-patch (entity)
             (check-type entity decomposition-entity)
             (let* ((hull       (hull entity))
                    (debug-info (org.shirakumo.fraf.convex-covering::debug-info hull)))
               (break "Add inspector here")
               ;; (clouseau:inspect debug-info :new-process t)
               ))
           (validate (patch aspects)
             (let ((context (context decomposition-scene)))
               (let ((*print-circle* t)
                     (*print-level* 3)
                     (*print-length* 3))
                 (let ((org.shirakumo.fraf.convex-covering::*debug-visualizations* aspects))
                   (format *trace-output* "Valid: ~A~%"
                           (org.shirakumo.fraf.convex-covering::valid-patch-p patch context)))
                 (let ((hull (org.shirakumo.fraf.convex-covering::patch-hull patch)))
                   (when hull
                     (format *trace-output* "Flat: ~A~%" (org.shirakumo.fraf.convex-covering::hull-flat-p hull))
                     (draw-annotations
                      (org.shirakumo.fraf.convex-covering::hull-annotations hull)
                      decomposition-scene)
                     (setf (org.shirakumo.fraf.convex-covering::hull-annotations hull) '()))))))
           (find-merged-patch (patch1 patch2)
             (let ((link (find-if (lambda (link)
                                    (or (eq (org.shirakumo.fraf.convex-covering::patch-link-a link) patch2)
                                        (eq (org.shirakumo.fraf.convex-covering::patch-link-b link) patch2)))
                                  (org.shirakumo.fraf.convex-covering::patch-links patch1))))
               (org.shirakumo.fraf.convex-covering::patch-link-merge-result link)))
           (patch-of-interest ()
             (let ((patch1 (when (selected-patch1 decomposition-scene)
                             (debug-patch (selected-patch1 decomposition-scene))))
                   (patch2 (when (selected-patch2 decomposition-scene)
                             (debug-patch (selected-patch2 decomposition-scene)))))
               (cond ((and patch1 patch2)
                      (find-merged-patch patch1 patch2))
                     (patch1)
                     (patch2)))))
    (cond ;; Inspect first selected patch
          #+no ((string= text "1") ; interferes with inputting numbers
           (inspect-patch (selected-patch1 decomposition-scene)))
          ;; Inspect second selected patch
          #+no ((string= text "2")
           (inspect-patch (selected-patch2 decomposition-scene)))
          ;; Inspect patch that results from merging the two selected patches
          #+no ((string= text "3")
           (let ((patch1 (selected-patch1 decomposition-scene))
                 (patch2 (selected-patch2 decomposition-scene)))
             (unless (and patch1 patch2)
               (format *trace-output* "Selected two patches~%"))
             (let* ((patch1       (debug-patch patch1))
                    (patch2       (debug-patch patch2))
                    (merged-patch (find-merged-patch patch1 patch2)))
               (break "Add inspector here")
               ;; (clouseau:inspect merged-patch :new-process t)
               )))
          ;; "E"xplain why mergable/not mergable
          ((string= text "e")
           (let ((patch1 (selected-patch1 decomposition-scene))
                 (patch2 (selected-patch2 decomposition-scene)))
             (cond ((and patch1 (not patch2)) ; why was this created?
                    (format *trace-output* "Why merged ~A?~%" patch1)
                    (setf (polygon-mode patch1) :line)
                    (let ((patch (debug-patch patch1)))
                      (explain-merge patch decomposition-scene)))
                   ((and patch1 patch2) ; why were these not merged?
                    (format *trace-output* "Why not merged ~A and ~A?~%" patch1 patch2)
                    (setf (polygon-mode patch1) :line)
                    (setf (polygon-mode patch2) :line)
                    (let* ((patch1       (debug-patch patch1))
                           (patch2       (debug-patch patch2))
                           (merged-patch (find-merged-patch patch1 patch2)))
                      (let ((debug-hull (add-debug-hull merged-patch (vec 1 1 1) decomposition-scene)))
                        (setf (polygon-mode debug-hull) :line))
                      (explain-merge merged-patch decomposition-scene)))))
           (commit (scene +main+) (loader +main+)))
          ;; "G"reen patch
          ((string= text "g")
           (clear-debug-entities decomposition-scene)
           (let* ((selection1 (selected-patch1 decomposition-scene))
                  (patch      (patch-merged-patches selection1))
                  (entity     (add-debug-hull patch (original-color selection1) decomposition-scene)))
             (setf (polygon-mode entity) :line)
             (setf (selection1 decomposition-scene) (list entity))
             (explain-merge patch decomposition-scene))
           (commit (scene +main+) (loader +main+)))
          ;; "R"ed patch
          ((string= text "r")
           (clear-debug-entities decomposition-scene)
           (let* ((selection1 (selected-patch1 decomposition-scene))
                  (patch      (nth-value 1 (patch-merged-patches selection1)))
                  (entity     (add-debug-hull patch (original-color selection1) decomposition-scene)))
             (setf (polygon-mode entity) :line)
             (setf (selection1 decomposition-scene) (list entity))
             (explain-merge patch decomposition-scene))
           (commit (scene +main+) (loader +main+)))
          ;; Validate "n"ormals
          ((string= text "n")
           ;; (clear-debug-entities decomposition-scene)
           (validate (patch-of-interest) '(:normals)))
          ;; Validate "l"ines (since e for "edge" is taken)
          ((string= text "l")
           (clear-debug-entities decomposition-scene)
           (let ((patch (patch-of-interest)))
             (explain-merge patch decomposition-scene)
             (validate patch '(:edges)))
           (commit (scene +main+) (loader +main+))))))
