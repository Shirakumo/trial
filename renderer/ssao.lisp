(in-package #:org.shirakumo.fraf.trial)

(define-shader-pass z-prepass (per-object-pass)
  ((depth :port-type output :attachment :depth-attachment :texspec (:internal-format :depth-component)))
  (:buffers (trial standard-environment-information)))

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
    (let ((depth (or (find :depth-attachment (attachments framebuffer))
                     (find :depth-stencil-attachment (attachments framebuffer)))))
      (if depth
          (setf (second depth) (depth-map pass))
          (push (list :depth-attachment (depth-map pass)) (attachments framebuffer))))
    (setf (clear-bits framebuffer) '(:color-buffer))
    framebuffer))

(defmethod render :around ((pass z-prepass-standard-render-pass) target)
  (gl:depth-mask NIL)
  (call-next-method)
  (gl:depth-mask T))

(defun generate-ssao-noise (&optional (samples 16))
  (let ((array (make-array (* samples 3) :element-type 'single-float)))
    (dotimes (i samples array)
      (setf (aref array (+ 0 (* 3 i))) (1- (* 2 (random 1.0))))
      (setf (aref array (+ 1 (* 3 i))) (1- (* 2 (random 1.0))))
      (setf (aref array (+ 2 (* 3 i))) 0.0))))

(define-asset (trial ssao-noise) image
    #p"ssao-noise.raw"
  :min-filter :nearest
  :mag-filter :nearest
  :wrapping :repeat
  :internal-format :rgb32f)

(defun generate-ssao-kernel (&optional (samples 64))
  (let ((array (make-array (* 3 samples) :element-type 'single-float)))
    (dotimes (i samples array)
      (let* ((scale (lerp 0.1 1.0 (expt (/ i samples) 2)))
             (sample (vec3 (1- (* 2 (random 1.0)))
                           (1- (* 2 (random 1.0)))
                           (random 1.0)))
             (sample (nv* (nvunit sample) (random 1.0) scale)))
        (setf (aref array (+ 0 (* i 3))) (vx sample))
        (setf (aref array (+ 1 (* i 3))) (vy sample))
        (setf (aref array (+ 2 (* i 3))) (vz sample))))))

(define-asset (trial ssao-kernel) image
    #p"ssao-kernel.raw"
  :target :texture-1d
  :min-filter :nearest
  :mag-filter :nearest
  :wrapping :repeat
  :internal-format :rgb32f)

(define-shader-pass ssao-pass (post-effect-pass)
  ((depth-map :port-type input :accessor depth-map)
   (occlusion :port-type output :accessor occlusion
              :attachment :color-attachment0
              :texspec (:internal-format :red
                        :min-filter :nearest
                        :mag-filter :nearest))
   (ssao-kernel :port-type fixed-input :texture (// 'trial 'ssao-kernel))
   (ssao-noise :port-type fixed-input :texture (// 'trial 'ssao-noise))
   (radius :initform 8.0 :accessor radius :uniform T)
   (bias :initform 5.0 :accessor radius :uniform T))
  (:buffers (trial standard-environment-information))
  (:shader-file (trial "ssao.glsl")))
