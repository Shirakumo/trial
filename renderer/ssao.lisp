(in-package #:org.shirakumo.fraf.trial)

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
  ((position-map :port-type input)
   (normal-map :port-type input)
   (occlusion :port-type output
              :attachment :color-attachment0
              :texspec (:internal-format :red
                        :min-filter :nearest
                        :mag-filter :nearest))
   (ssao-kernel :port-type fixed-input :texture (// 'trial 'ssao-kernel))
   (ssao-noise :port-type fixed-input :texture (// 'trial 'ssao-noise)))
  (:buffers (trial standard-environment-information))
  (:shader-file (trial "ssao.glsl")))
