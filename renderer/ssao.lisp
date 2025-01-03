(in-package #:org.shirakumo.fraf.trial)

(define-shader-pass z-prepass (per-object-pass standard-environment-pass)
  ((depth :port-type output :attachment :depth-attachment :reader depth)))

(defmethod object-renderable-p ((renderable renderable) (pass z-prepass)) NIL)
(defmethod object-renderable-p ((renderable standard-renderable) (pass z-prepass)) T)
(defmethod object-renderable-p ((material material) (pass z-prepass)) (not (transparent-p material)))

(defmethod compute-shader ((type (eql :fragment-shader)) (pass z-prepass) object)
  (load-time-value (list (glsl-toolkit:parse "void main(){}"))))

(defmethod make-pass-framebuffer ((pass z-prepass))
  (let ((framebuffer (call-next-method)))
    (setf (clear-bits framebuffer) '(:depth-buffer))
    framebuffer))

(define-shader-pass z-prepass-standard-render-pass (standard-render-pass)
  ((depth-map :port-type input :accessor depth-map)))

(defmethod make-pass-framebuffer ((pass z-prepass-standard-render-pass))
  (let ((framebuffer (call-next-method)))
    (setf (attachments framebuffer)
          `((:depth-attachment ,(depth-map pass))
            ,@(remove-all '(:depth-attachment :depth-stencil-attachment) (attachments framebuffer) :key #'car)))
    (setf (clear-bits framebuffer) '(:color-buffer))
    framebuffer))

(defmethod render :around ((pass z-prepass-standard-render-pass) target)
  (with-depth-mask NIL
    (call-next-method)))

(define-shader-pass z-pre-pbr-render-pass (z-prepass-standard-render-pass pbr-render-pass)
  ())

(define-shader-pass ssao-pbr-render-pass (z-pre-pbr-render-pass)
  ((ssao-radius :initform 8.0 :initarg :ssao-radius :accessor ssao-radius :uniform T)
   (ssao-bias :initform 5.0 :initarg :ssao-bias :accessor ssao-bias :uniform T)
   (ssao-kernel-size :initform 64 :initarg :ssao-kernel-size :accessor ssao-kernel-size :uniform T))
  (:shader-file (trial "renderer/standard-render-pbr-ssao.glsl")))

(define-shader-pass ssao-pass (post-effect-pass)
  ((depth-map :port-type input :accessor depth-map)
   (occlusion :port-type output :accessor occlusion
              :attachment :color-attachment0
              :texspec (:internal-format :red
                        :min-filter :nearest
                        :mag-filter :nearest))
   (ssao-radius :initform 8.0 :accessor ssao-radius :uniform T)
   (ssao-bias :initform 5.0 :accessor ssao-bias :uniform T))
  (:buffers (trial standard-environment-information)))

(define-class-shader (ssao-pass :fragment-shader)
  "#include (trial::trial \"ssao.glsl\")

in vec2 uv;
out float color;
void main(){
  color = evaluate_ssao(uv);
}")

(defun generate-ssao-noise (&optional (samples 16))
  (let ((array (make-array (* samples 3) :element-type 'single-float)))
    (dotimes (i samples array)
      (setf (aref array (+ 0 (* 3 i))) (1- (random 2.0)))
      (setf (aref array (+ 1 (* 3 i))) (1- (random 2.0)))
      (setf (aref array (+ 2 (* 3 i))) 0.0))))

(defun generate-ssao-kernel (&optional (samples 64))
  (let ((array (make-array (* 3 samples) :element-type 'single-float)))
    (dotimes (i samples array)
      (let* ((scale (lerp 0.1 1.0 (expt (/ i samples) 2)))
             (sample (vec3 (1- (random 2.0))
                           (1- (random 2.0))
                           (random 1.0)))
             (sample (nv* (nvunit sample) scale)))
        (setf (aref array (+ 0 (* i 3))) (vx sample))
        (setf (aref array (+ 1 (* i 3))) (vy sample))
        (setf (aref array (+ 2 (* i 3))) (vz sample))))))
