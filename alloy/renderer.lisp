#|
 This file is a part of trial
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.alloy)

(defclass renderer (org.shirakumo.alloy.renderers.opengl.msdf:renderer trial:renderable trial:resource)
  ((image-cache :initform (make-hash-table :test 'equal) :reader image-cache)))

(defmethod org.shirakumo.alloy.renderers.opengl.msdf:fontcache-directory ((renderer renderer))
  (if (deploy:deployed-p)
      (pathname-utils:subdirectory (deploy:data-directory) "pool" "font-cache")
      (call-next-method)))

(defmethod opengl:view-size ((renderer renderer))
  (alloy:px-size (trial:width trial:*context*) (trial:height trial:*context*)))

(defmethod alloy:allocate ((renderer renderer)))

(defmethod deallocate-cache ((renderer renderer))
  (loop with cache = (image-cache renderer)
        for path being the hash-keys of cache using (hash-value object)
        do (trial:deallocate object)
           (remhash path (image-cache renderer))))

(defmethod alloy:deallocate :after ((renderer renderer))
  (deallocate-cache renderer))

(defmethod trial:stage :before ((renderer renderer) (area trial:staging-area))
  ;; FIXME: This is BAD, but Alloy gives us no way of generating the resource stubs.
  (alloy:allocate renderer))

(defmethod trial:stage ((tree alloy:layout-tree) (area trial:staging-area))
  (trial:stage (alloy:root tree) area)
  (trial:stage (alloy:popups tree) area))

(defmethod trial:stage ((layout alloy:layout) (area trial:staging-area))
  (alloy:do-elements (element layout)
    (trial:stage element area)))

(defmethod trial:stage ((structure alloy:structure) (area trial:staging-area))
  (trial:stage (alloy:layout-element structure) area))

(defmethod trial:dependencies ((renderer renderer))
  (append (alexandria:hash-table-values (org.shirakumo.alloy.renderers.opengl.msdf:fontcache renderer))
          (alexandria:hash-table-values (opengl::resources renderer))))

(defmethod trial:allocate ((renderer renderer))
  (alloy:allocate renderer))

(defmethod trial:allocated-p ((renderer renderer))
  (alloy:allocated-p renderer))

(defmethod trial:deallocate ((renderer renderer))
  (alloy:deallocate renderer))

(defmethod alloy:render :before ((renderer renderer) (ui alloy:ui))
  (let ((target (simple:transform-matrix renderer)))
    (setf (aref target 0) (/ 2f0 (max 1 (trial:width trial:*context*))))
    (setf (aref target 1) 0f0)
    (setf (aref target 2) -1f0)
    
    (setf (aref target 3) 0f0)
    (setf (aref target 4) (/ 2f0 (max 1 (trial:height trial:*context*))))
    (setf (aref target 5) -1f0)

    (setf (aref target 6) 0f0)
    (setf (aref target 7) 0f0)
    (setf (aref target 8) 1f0)))

(defmethod simple:z-index ((renderer renderer))
  (- (call-next-method)))

(defmethod (setf simple:z-index) (value (renderer renderer))
  (call-next-method (- value) renderer))

(defmethod opengl:make-shader ((renderer renderer) &key vertex-shader fragment-shader)
  (let ((vert (make-instance 'trial:shader :source vertex-shader
                                           :type :vertex-shader))
        (frag (make-instance 'trial:shader :source fragment-shader
                                           :type :fragment-shader)))
    (make-instance 'trial:shader-program :shaders (list vert frag))))

(defmethod (setf opengl:uniform) (value (shader trial:shader-program) uniform)
  (let ((loc (trial:uniform-location shader uniform)))
    (etypecase value
      (colored:color
       (%gl:uniform-4f loc (colored:r value) (colored:g value) (colored:b value) (colored:a value)))
      (alloy:point
       (%gl:uniform-2f loc (alloy:pxx value) (alloy:pxy value)))
      (alloy:size
       (%gl:uniform-2f loc (alloy:pxw value) (alloy:pxh value)))
      (vector
       #+sbcl
       (sb-sys:with-pinned-objects (value)
         (%gl:uniform-matrix-3fv loc 1 T (sb-sys:vector-sap value)))
       #-sbcl
       (gl:uniform-matrix-3fv loc (marr3 value)))
      (single-float
       (%gl:uniform-1f loc value)))))

(defmethod opengl:bind ((shader trial:shader-program))
  (gl:use-program (trial:gl-name shader)))

(defmethod alloy:allocate ((shader trial:shader))
  (trial:allocate shader))

(defmethod alloy:allocate ((shader trial:shader-program))
  (trial:allocate shader))

(defmethod alloy:deallocate ((shader trial:shader-program))
  (trial:deallocate shader))

(defmethod opengl:make-vertex-buffer ((renderer renderer) contents &key (data-usage :static-draw)
                                                                        (buffer-type :array-buffer))
  (etypecase contents
    (vector
     (make-instance 'trial:vertex-buffer
                    :buffer-data contents
                    :data-usage data-usage
                    :buffer-type buffer-type))
    (integer
     (make-instance 'trial:vertex-buffer
                    :size (* 4 contents)
                    :data-usage data-usage
                    :buffer-type buffer-type))))

(defmethod opengl:update-vertex-buffer ((buffer trial:vertex-buffer) contents)
  (let ((length (* (length contents) (trial:gl-type-size :float))))
    (if (< (trial:size buffer) length)
        (trial:resize-buffer buffer length :data contents)
        (trial:update-buffer-data buffer contents))))

(defmethod alloy:allocate ((buffer trial:vertex-buffer))
  (trial:allocate buffer))

(defmethod alloy:deallocate ((buffer trial:vertex-buffer))
  (trial:deallocate buffer))

(defmethod opengl:gl-name ((buffer trial:vertex-buffer))
  (trial:gl-name buffer))

(defmethod opengl:make-vertex-array ((renderer renderer) bindings)
  (make-instance 'trial:vertex-array :bindings bindings :vertex-form NIL))

(defmethod opengl:draw-vertex-array ((array trial:vertex-array) primitive-type offset count)
  (gl:bind-vertex-array (trial:gl-name array))
  (if (loop for binding in (trial:bindings array)
            thereis (typep binding 'trial:vertex-buffer))
      (%gl:draw-elements primitive-type count :unsigned-int offset)
      (%gl:draw-arrays primitive-type offset count)))

(defclass image (trial:texture simple:image)
  ())

(defmethod opengl:make-texture ((renderer renderer) width height data &key (channels 4) (filtering :linear))
  (let ((format (ecase channels (1 :r) (2 :rg) (3 :rgb) (4 :rgba))))
    (make-instance 'image :width width :height height
                          :internal-format format
                          :pixel-format format
                          :pixel-data data
                          :min-filter filtering
                          :mag-filter filtering)))

(defmethod alloy:allocate ((array trial:vertex-array))
  (trial:allocate array))

(defmethod alloy:deallocate ((array trial:vertex-array))
  (trial:deallocate array))

(defmethod simple:size ((image trial:texture))
  (alloy:px-size (trial:width image) (trial:height image)))

(defmethod simple:data ((image trial:texture))
  (trial:pixel-data image))

(defmethod simple:channels ((image trial:texture))
  (ecase (trial:internal-format image)
    ((:red :r8) 1)
    ((:rg :rg8) 2)
    ((:rgb :rgb8) 3)
    ((:rgba :rgba8) 4)))

(defmethod simple:icon ((renderer renderer) bounds (texture trial:texture) &rest initargs)
  (apply #'make-instance 'simple:icon :image texture :bounds bounds initargs))

(defmethod simple:icon ((renderer renderer) bounds (path pathname) &rest initargs)
  (apply #'simple:icon renderer bounds (simple:request-image renderer path) initargs))

(defmethod simple:request-image ((renderer renderer) (path pathname) &key (filtering :linear) (wrapping :repeat))
  (let ((image (gethash path (image-cache renderer))))
    (unless image
      (setf image (trial:generate-resources 'trial:image-loader path
                                            :wrapping (list wrapping wrapping wrapping)
                                            :texture-class 'image
                                            :min-filter filtering
                                            :mag-filter filtering))
      (setf (gethash path (image-cache renderer)) image)
      (trial:allocate image))
    image))

(defmethod simple:request-image ((renderer renderer) (texture trial:texture) &key)
  texture)

(defmethod alloy:allocate ((texture trial:texture))
  (trial:allocate texture))

(defmethod alloy:deallocate ((texture trial:texture))
  (trial:deallocate texture))

(defmethod opengl:bind ((texture trial:texture))
  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2D (trial:gl-name texture)))

(defmethod simple:size ((image trial:image))
  (alloy:size (trial:width image) (trial:height image)))

(defmethod trial:dependencies ((font simple:font))
  (list (org.shirakumo.alloy.renderers.opengl.msdf:atlas font)))

(defmethod trial:stage :before ((font simple:font) (area trial:staging-area))
  ;; FIXME: This is BAD, but Alloy gives us no way of generating the resource stubs.
  (alloy:allocate font))

(defmethod trial:stage ((object simple:font) (area trial:staging-area))
  (setf (gethash object (trial:staged area)) (cons NIL NIL)))

(defmethod trial:load-with (loader (object simple:font)))
(defmethod trial:unload-with (loader (object simple:font)))
