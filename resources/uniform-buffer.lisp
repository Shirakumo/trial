(in-package #:org.shirakumo.fraf.trial)

(defclass uniform-buffer (struct-buffer bindable-buffer)
  ((buffer-type :initform :uniform-buffer)))

(defmethod shared-initialize :after ((buffer uniform-buffer) slots &key)
  (unless (slot-boundp buffer 'binding)
    (setf (binding buffer) (cffi:translate-underscore-separated-name (struct-class buffer)))))

(defmethod binding-target ((buffer uniform-buffer)) :uniform-buffer)

(defmethod allocate :after ((buffer uniform-buffer))
  (unless (binding-point buffer) (setf (binding-point buffer) T)))

(defmethod bind ((buffer uniform-buffer) (program shader-program))
  #-elide-context-current-checks
  (check-context-current)
  ;; TODO: Once we can do shared/packed, load offsets here.
  (load buffer)
  ;; Bind the buffer to the program's specified binding point.
  (let ((index (gl:get-uniform-block-index (gl-name program) (gl-type buffer))))
    (cond ((= (1- (ash 1 32)) index)
           #-trial-release
           (error "Failed to get uniform block index for ~s in ~a" (gl-type buffer) program)
           #+trial-release
           (v:warn :trial.resource "Failed to get uniform block index for ~s in ~a"
                   (gl-type buffer) program))
          (T
           (%gl:uniform-block-binding (gl-name program) index (binding-point buffer))
           #-(or elide-buffer-access-checks trial-release)
           (let ((size (gl:get-active-uniform-block (gl-name program) index :uniform-block-data-size)))
             (assert (= size (size buffer))))))))
