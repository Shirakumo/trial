#|
 This file is a part of trial
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.alloy)

(defclass renderer (opengl:renderer trial:resource)
  ())

(defmethod alloy:allocate ((renderer renderer))
  (loop for res across (trial::topological-sort-by-dependencies
                        (loop for v being the hash-values of (opengl::resources renderer)
                              append (typecase v
                                       (trial:shader-program
                                        (list* v (trial:shaders v)))
                                       (T (list v)))))
        do (alloy:allocate res)))

(defmethod trial:allocate ((renderer renderer))
  (alloy:allocate renderer))

(defmethod trial:allocated-p ((renderer renderer))
  (alloy:allocated-p renderer))

(defmethod trial:deallocate ((renderer renderer))
  (alloy:deallocate renderer))

(defmethod alloy:render :before ((renderer renderer) (ui (eql T)))
  (let ((target (simple:transform-matrix (simple:transform renderer))))
    (setf (aref target 0) (/ 2f0 (trial:width trial:*context*)))
    (setf (aref target 1) 0f0)
    (setf (aref target 2) -1f0)
    
    (setf (aref target 3) 0f0)
    (setf (aref target 4) (/ 2f0 (trial:height trial:*context*)))
    (setf (aref target 5) -1f0)

    (setf (aref target 6) 0f0)
    (setf (aref target 7) 0f0)
    (setf (aref target 8) 1f0)))

(defmethod opengl:make-shader ((renderer renderer) &key vertex-shader fragment-shader)
  (let ((vert (make-instance 'trial:shader :source vertex-shader
                                           :type :vertex-shader))
        (frag (make-instance 'trial:shader :source fragment-shader
                                           :type :fragment-shader)))
    (make-instance 'trial:shader-program :shaders (list vert frag))))

(defmethod (setf opengl:uniform) (value (shader trial:shader-program) uniform)
  (let ((loc (trial:uniform-location shader uniform)))
    (etypecase value
      (simple:color
       (%gl:uniform-4f loc (simple:r value) (simple:g value) (simple:b value) (simple:a value)))
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

(defmethod opengl:make-vertex-buffer ((renderer renderer) contents &key (data-usage :static-draw))
  (make-instance 'trial:vertex-buffer
                 :buffer-data contents
                 :data-usage data-usage))

(defmethod opengl:update-vertex-buffer ((buffer trial:vertex-buffer) contents)
  (trial:update-buffer-data buffer contents))

(defmethod alloy:allocate ((buffer trial:vertex-buffer))
  (trial:allocate buffer))

(defmethod alloy:deallocate ((buffer trial:vertex-buffer))
  (trial:deallocate buffer))

(defmethod opengl:make-vertex-array ((renderer renderer) bindings)
  (make-instance 'trial:vertex-array :bindings bindings :vertex-form NIL))

(defmethod opengl:draw-vertex-array ((array trial:vertex-array) primitive-type count)
  (gl:bind-vertex-array (trial:gl-name array))
  (%gl:draw-arrays primitive-type 0 count))

(defmethod alloy:allocate ((array trial:vertex-array))
  (trial:allocate array))

(defmethod alloy:deallocate ((array trial:vertex-array))
  (trial:deallocate array))

(defmethod simple:request-image ((renderer renderer) (image pathname))
  (make-instance 'trial:image :input image))

(defmethod alloy:allocate ((texture trial:texture))
  (trial:allocate texture))

(defmethod alloy:deallocate ((texture trial:texture))
  (trial:deallocate texture))

(defmethod simple:size ((image trial:image))
  (alloy:size (trial:width image) (trial:height image)))

(defmethod (setf simple:font) ((font trial:font) (style simple:style))
  (setf (slot-value style 'simple:font) font))

(defmethod simple:request-font ((renderer renderer) (fontspec pathname))
  (make-instance 'trial:font :input fontspec))

(defmethod simple:request-font ((renderer renderer) (fontspec (eql :default)))
  (trial:asset 'trial:trial 'trial:noto-sans))

(defmethod alloy:allocate ((font trial:font))
  (trial:allocate font))

(defmethod alloy:deallocate ((font trial:font))
  (trial:deallocate font))

(defmethod simple:text ((renderer renderer) point string &key (font (simple:font renderer))
                                                              (size (simple:font-size renderer))
                                                              (align :start)
                                                              (direction :right)
                                                              (vertical-align :bottom))
  )
