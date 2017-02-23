#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

;; FIXME: configurable defaults

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defclass asset ()
  ((inputs :initarg :inputs :accessor inputs)
   (resource :initarg :resource :accessor resource))
  (:default-initargs :inputs () :resource NIL))

(defmethod print-object ((asset asset) stream)
  (print-unreadable-object (asset stream :type T :identity T)))

(defmethod finalize-resource ((asset asset) resource))

(defmethod coerce-input ((asset asset) input)
  (error "Incompatible input type ~s for asset of type ~s."
         (type-of input) (type-of asset)))

(defmethod coerced-inputs ((asset asset))
  (loop for input in (inputs asset)
        collect (coerce-input asset input)))

(defmethod load-asset :around ((asset asset))
  (unless (resource asset)
    (call-next-method))
  asset)

(defmethod offload-asset :around ((asset asset))
  (when (resource asset)
    (call-next-method))
  asset)

(defmethod finalize :after ((asset asset))
  (offload-asset asset))

(defun make-asset (type inputs &rest initargs)
  (apply #'make-instance type :inputs (enlist inputs) initargs))

(defmacro with-assets (asset-specs &body body)
  (let ((storage (make-hash-table :test 'eq)))
    `(let* ,(loop for (name . initargs) in asset-specs
                  collect `(,name (or (gethash ',name ,storage)
                                      (setf (gethash ',name ,storage)
                                            (make-asset ,@initargs)))))
       ,@body)))

(defclass shader-asset (asset)
  ((shader-type :initarg :type :accessor shader-type)))

(defmethod initialize-instance :before ((asset shader-asset) &key type)
  (check-shader-type type))

(defmethod coerce-input ((asset shader-asset) (file pathname))
  (alexandria:read-file-into-string file))

(defmethod coerce-input ((asset shader-asset) (source string))
  source)

(defmethod offload-asset ((asset shader-asset))
  (gl:delete-shader (resource asset)))

(defmethod load-asset ((asset shader-asset))
  (let ((source (with-output-to-string (output)
                  (dolist (source (coerced-inputs asset))
                    (write-sequence source output))))
        (shader (gl:create-shader (shader-type asset))))
    (setf (resource asset) shader)
    (with-cleanup-on-failure (offload-asset asset)
      (with-new-value-restart (source input-source) (use-source "Supply new source code directly.")
        (gl:shader-source shader source)
        (gl:compile-shader shader)
        (unless (gl:get-shader shader :compile-status)
          (error "Failed to compile ~a: ~%~a" asset (gl:get-shader-info-log shader)))))))

(defclass shader-program-asset (asset)
  ())

(defmethod coerce-input ((asset shader-program-asset) (shader shader-asset))
  shader)

(defun check-shader-compatibility (shaders)
  (loop with table = (make-hash-table :test 'eql)
        for shader in shaders
        do (if (gethash (shader-type shader) table)
               (error "Cannot compile two shaders of the same type into a single program~%  ~a~%  ~a"
                      (gethash (shader-type shader) table) shader)
               (setf (gethash (shader-type shader) table) shader))
        finally (return shaders)))

(defmethod offload-asset ((asset shader-program-asset))
  (gl:delete-program (resource asset)))

(defmethod load-asset ((asset shader-program-asset))
  (let ((shaders (coerced-inputs asset)))
    (check-shader-compatibility shaders)
    (let ((program (gl:create-program)))
      (setf (resource asset) program)
      (with-cleanup-on-failure (offload-asset asset)
        (dolist (shader shaders)
          (gl:attach-shader program (resource shader)))
        (gl:link-program program)
        (dolist (shader shaders)
          (gl:detach-shader program (resource shader)))
        (unless (gl:get-program program :link-status)
          (error "Failed to link ~a: ~%~a"
                 asset (gl:get-program-info-log program)))))))

(defmethod uniform-location ((asset shader-program-asset) (name string))
  (gl:get-uniform-location (resource asset) name))

(defmethod uniform-location ((asset shader-program-asset) (name symbol))
  (uniform-location asset (symbol->c-name name)))

(defmethod (setf uniform) (data (asset shader-program-asset) (name symbol))
  (setf (uniform asset (symbol->c-name name)) data))

(defmethod (setf uniform) (data (asset shader-program-asset) (name string))
  (let ((location (uniform-location asset name)))
    (etypecase data
      (fixnum (gl:uniformi location data))
      (single-float (gl:uniformf location data))
      (vec2 (gl:uniformf location (vx data) (vy data)))
      (vec3 (gl:uniformf location (vx data) (vy data) (vz data)))
      (vec4 (gl:uniformf location (vx data) (vy data) (vz data) (vw data)))
      (mat2 (gl:uniform-matrix-2fv location (marr2 data)))
      (mat3 (gl:uniform-matrix-3fv location (marr3 data)))
      (mat4 (gl:uniform-matrix-4fv location (marr4 data)))
      (matn (ecase (mrows data)
              (2 (ecase (mcols data)
                   (3 (gl:uniform-matrix-2x3-fv location (marrn data)))
                   (4 (gl:uniform-matrix-2x4-fv location (marrn data)))))
              (3 (ecase (mcols data)
                   (2 (gl:uniform-matrix-3x2-fv location (marrn data)))
                   (4 (gl:uniform-matrix-3x4-fv location (marrn data)))))
              (4 (ecase (mcols data)
                   (2 (gl:uniform-matrix-4x2-fv location (marrn data)))
                   (3 (gl:uniform-matrix-4x3-fv location (marrn data))))))))))

(defclass vertex-buffer-asset (asset)
  ((buffer-type :initarg :buffer-type :accessor buffer-type)
   (element-type :initarg :element-type :accessor element-type)
   (data-usage :initarg :data-usage :accessor data-usage))
  (:default-initargs
   :buffer-type :array-buffer
   :element-type :float
   :data-usage :static-draw))

(defmethod initialize-instance :before ((asset vertex-buffer-asset) &key buffer-type element-type data-usage)
  (check-vertex-buffer-type buffer-type)
  (check-vertex-buffer-element-type element-type)
  (check-vertex-buffer-data-usage data-usage))

(defmethod coerce-input ((asset vertex-buffer-asset) (vector vector))
  vector)

(defmethod coerce-input ((asset vertex-buffer-asset) (list list))
  (coerce list 'vector))

(defmethod offload-asset ((asset vertex-buffer-asset))
  (gl:delete-buffers (resource asset)))

(defmethod load-asset ((asset vertex-buffer-asset))
  (let ((buffer-data (let ((output (make-array 0 :adjustable T :fill-pointer T)))
                       (dolist (input (coerced-inputs asset) output)
                         (loop for v across input do (vector-push-extend v output))))))
    (with-slots (element-type buffer-type data-usage) asset
      (let ((buffer (gl:gen-buffer)))
        (setf (resource asset) buffer)
        (with-cleanup-on-failure (offload-asset asset)
          (let ((array (gl:alloc-gl-array element-type (length buffer-data))))
            (unwind-protect
                 (loop initially (gl:bind-buffer buffer-type buffer)
                       for i from 0
                       for el across buffer-data
                       do (setf (gl:glaref array i) (gl-coerce el element-type))
                       finally (gl:buffer-data buffer-type data-usage array))
              (gl:free-gl-array array)
              (gl:bind-buffer buffer-type 0))))))))

(defclass vertex-array-asset (asset)
  ())

(defmethod coerce-input ((asset vertex-array-asset) (buffer vertex-buffer-asset))
  (list buffer))

(defmethod coerce-input ((asset vertex-array-asset) (spec list))
  spec)

(defmethod offload-asset ((asset vertex-buffer-asset))
  (gl:delete-vertex-arrays (list (resource asset))))

(defmethod load-asset ((asset vertex-array-asset))
  (let ((array (gl:gen-vertex-array)))
    (setf (resource asset) array)
    (with-cleanup-on-failure (offload-asset asset)
      (gl:bind-vertex-array array)
      (unwind-protect
           (loop for buffer in (coerced-inputs asset)
                 for i from 0
                 do (destructuring-bind (buffer &key (index i)
                                                     (size 3)
                                                     (normalized NIL)
                                                     (stride 0)
                                                     (offset 0))
                        buffer
                      (gl:bind-buffer (buffer-type buffer) (resource buffer))
                      (gl:vertex-attrib-pointer index size (element-type buffer) normalized stride offset)
                      (gl:enable-vertex-attrib-array index)))
        (gl:bind-vertex-array 0)))))
