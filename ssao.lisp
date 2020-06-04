#|
 This file is a part of trial
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defun generate-ssao-noise (&optional (samples 16))
  (let ((array (make-array (* samples 3) :element-type 'single-float)))
    (dotimes (i samples array)
      (setf (aref array (+ 0 (* 3 i))) (1- (* 2 (random 1.0))))
      (setf (aref array (+ 1 (* 3 i))) (1- (* 2 (random 1.0))))
      (setf (aref array (+ 2 (* 3 i))) 0.0))))

(defun generate-ssao-kernel (&optional (samples 64))
  (let ((array (make-array samples)))
    (flet ((lerp (a b f)
             (+ a (* f (- b a)))))
      (dotimes (i samples array)
        (let* ((scale (lerp 0.1 1.0 (expt (/ i samples) 2)))
               (sample (vec3 (1- (* 2 (random 1.0)))
                             (1- (* 2 (random 1.0)))
                             (random 1.0)))
               (sample (nv* (nvunit sample) (random 1.0) scale)))
          (setf (aref array i) sample))))))

(define-shader-pass ssao-pass (post-effect-pass)
  ((position-map :port-type input)
   (normal-map :port-type input)
   (noise-map :port-type buffer
              :texspec (:width 4
                        :height 4
                        :min-filter :nearest
                        :mag-filter :nearest
                        :wrapping :repeat
                        :internal-format :rgb32f
                        :pixel-format :rgb
                        :pixel-type :float))
   (occlusion :port-type output
              :attachment :color-attachment0
              :texspec (:internal-format :rgb
                        :min-filter :nearest
                        :mag-filter :nearest))
   (kernel :initform (generate-ssao-kernel) :accessor kernel))
  (:inhibit-shaders (shader-entity :fragment-shader (generate-ssao-noise))))

(defmethod initialize-instance :after ((pass ssao-pass) &key)
  (setf (getf (texspec (port pass 'noise-map)) :pixel-data) (generate-ssao-noise)))

(defmethod render :before ((pass ssao-pass) (program shader-program))
  (let ((kernel (kernel pass)))
    (loop for i from 0 below (length kernel)
          for vec of-type vec3 = (aref kernel i)
          do (setf (uniform program (format NIL "samples[~d]" i)) vec))
    (setf (uniform program "projection_matrix") *projection-matrix*)
    (setf (uniform program "view_matrix") *view-matrix*)
    (setf (uniform program "viewport_size") (vec2 (width *context*) (height *context*)))))

(define-class-shader (ssao-pass :fragment-shader)
  ;; KLUDGE
  (asdf:system-relative-pathname :trial "data/ssao.frag"))
