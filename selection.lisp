(in-package #:org.shirakumo.fraf.trial)

(define-shader-pass selection-buffer (per-object-pass)
  ((object-table :initform (make-hash-table :test 'eq) :accessor object-table)
   (color-counter :initform 0 :accessor color-counter)
   (color :port-type output :attachment :color-attachment0 :texspec (:internal-format :rgb) :reader color)
   (depth :port-type output :attachment :depth-attachment :reader depth)))

(defmethod clear :after ((buffer selection-buffer))
  (clrhash (object-table buffer))
  (setf (color-counter buffer) 0))

(defmethod stage :before ((buffer selection-buffer) (area staging-area))
  (unless (framebuffer buffer)
    (pack-pipeline buffer (width *context*) (height *context*))))

(defmethod enter :before ((thing renderable) (buffer selection-buffer))
  (unless (gethash thing (object-table buffer))
    (let ((next (1+ (color-counter buffer))))
      (when (<= (ash 1 32) next)
        (when (<= (1- next) (hash-table-count (object-table buffer)))
          (error "Can't store 2^32 objects in the selection buffer (wtf are you doing?)"))
        ;; Recompact the table back down
        (let ((new (make-hash-table :test 'eq)))
          (setf next 1)
          (loop for old-color being the hash-keys of (object-table buffer) using (hash-value object)
                do (when (integerp old-color)
                     (setf (gethash next new) object)
                     (setf (gethash object new) next)
                     (incf next)))
          (setf (object-table buffer) new)))
      (setf (color-counter buffer) next)
      (setf (gethash thing (object-table buffer)) next)
      (setf (gethash next (object-table buffer)) thing))))

(defmethod leave :after ((thing renderable) (buffer selection-buffer))
  (let ((color (gethash thing (object-table buffer))))
    (when color
      (remhash thing (object-table buffer))
      (remhash color (object-table buffer)))))

(defmethod select (point (buffer selection-buffer))
  (resize (framebuffer buffer) (width *context*) (height *context*))
  (render buffer NIL)
  (let ((x (clamp 0 (round (vx point)) (1- (width buffer))))
        (y (clamp 0 (round (vy point)) (1- (height buffer)))))
    (cffi:with-foreign-objects ((int :uint32))
      (setf (cffi:mem-ref int :uint32) 0)
      (gl:bind-framebuffer :read-framebuffer (gl-name (framebuffer buffer)))
      (%gl:read-pixels x y 1 1 :rgb :unsigned-byte int)
      (gethash (cffi:mem-ref int :uint32) (object-table buffer)))))

(defmethod render-with :before ((buffer selection-buffer) object program)
  ;; KLUDGE: This sucks, but I couldn't make it work with an r32ui texture,
  ;;         the read-pixels call would just always fail and I don't know why.
  (let* ((int (gethash object (object-table buffer)))
         (color (vec (/ (ldb (byte 8 0) int) 255.0)
                     (/ (ldb (byte 8 8) int) 255.0)
                     (/ (ldb (byte 8 16) int) 255.0))))
    (declare (dynamic-extent color))
    (setf (uniform program "selection_color") color)))

;; TODO: allow some leniency in the selection. How do we do this best? by
;;       grabbing more pixels around the selection point, or by trying to
;;       render objects to the buffer bigger? The latter is the outline
;;       problem, which is quite hard to do generically. Just scaling the
;;       objects won't work right.

(defmethod make-pass-shader-program ((buffer selection-buffer) object)
  (let* ((program (call-next-method))
         (fragment (find :fragment-shader (shaders program) :key #'shader-type)))
    (setf (shader-source fragment) "out vec4 color;
uniform vec3 selection_color = vec3(0);

void main(){
  color = vec4(selection_color, 1);
}")
    program))
