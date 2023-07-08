#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-shader-pass render-pass (per-object-pass)
  ((color :port-type output :attachment :color-attachment0 :reader color)
   (depth :port-type output :attachment :depth-stencil-attachment :reader depth)))

(define-shader-pass solid-render-pass (render-pass)
  ((color :port-type output)
   (fill-color :initarg :fill-color :initform (vec 0 0 0 1) :uniform T :accessor fill-color)))

(define-class-shader (solid-render-pass :fragment-shader)
  "out vec4 color;
uniform vec4 fill_color;

void main(){
  color = fill_color;
}")

(define-shader-pass simple-post-effect-pass (post-effect-pass)
  ((previous-pass :port-type input :reader previous-pass)
   (color :port-type output :reader color)))

(defmethod (setf active-p) :before (value (pass simple-post-effect-pass))
  ;; KLUDGE: This is terrible. How do we do this cleanly?
  (flet ((previous (pass)
           (flow:left (first (flow:connections (flow:port pass 'previous-pass)))))
         (next (pass)
           (flow:right (first (flow:connections (flow:port pass 'color))))))
    (let ((pred (loop for prev = (previous pass) then (previous (flow:node prev))
                      do (when (active-p (flow:node prev)) (return prev))))
          (succ (when (flow:connections (flow:port pass 'color))
                  (loop for next = (next pass) then (next (flow:node next))
                        do (when (active-p (flow:node next)) (return next))))))
      (cond (value
             (setf (texture (flow:port pass 'previous-pass)) (texture pred))
             (when succ (setf (texture succ) (texture (flow:port pass 'color)))))
            (succ
             (setf (texture succ) (texture pred)))))))

(define-shader-pass iterative-post-effect-pass (simple-post-effect-pass)
  ((iterations :initarg :iterations :initform 1 :accessor iterations)))

(defmethod render ((pass iterative-post-effect-pass) (program shader-program))
  (let* ((color (color pass))
         (ocolor color)
         (framebuffer (framebuffer pass))
         (previous (previous-pass pass)))
    (flet ((swap-buffers ()
             (rotatef color previous)
             (bind color framebuffer)
             (bind previous 0)))
      (call-next-method)
      (loop with limit = (iterations pass)
            for i from 0
            do (call-next-method)
               (if (< (incf i) limit)
                   (swap-buffers)
                   (return)))
      ;; KLUDGE: this is wrong for even number of iterations. It essentially
      ;;         discards the last iteration, as it won't be displayed....
      (unless (eq ocolor color)
        (bind ocolor (framebuffer pass))))))

(define-shader-pass temporal-post-effect-pass (post-effect-pass)
  ((previous :port-type static-input :accessor previous)
   (color :port-type output :reader color)))

(defmethod render :after ((pass temporal-post-effect-pass) thing)
  (rotatef (gl-name (previous pass)) (gl-name (color pass)))
  (bind (color pass) (framebuffer pass)))

(define-shader-pass copy-pass (simple-post-effect-pass)
  ())

(define-class-shader (copy-pass :fragment-shader)
  "
uniform sampler2D previous_pass;
in vec2 uv;
out vec4 color;

void main(){
  color = texture(previous_pass, uv);
}")

(define-shader-pass negative-pass (simple-post-effect-pass)
  ()
  (:shader-file (trial "negative.glsl")))

(define-shader-pass box-blur-pass (iterative-post-effect-pass)
  ((intensity :initarg :intensity :initform 1.0 :uniform T :accessor intensity))
  (:shader-file (trial "box-blur.glsl")))

(define-shader-pass sobel-pass (simple-post-effect-pass)
  ((intensity :initarg :intensity :initform 1.0 :uniform T :accessor intensity))
  (:shader-file (trial "sobel.glsl")))

(define-shader-pass gaussian-blur-pass (iterative-post-effect-pass)
  ((intensity :initarg :intensity :initform 1.0 :uniform T :accessor intensity)
   (direction :initarg :direction :initform (vec 1 0) :uniform T :accessor direction))
  (:shader-file (trial "gaussian.glsl")))

(define-shader-pass radial-blur-pass (iterative-post-effect-pass)
  ((intensity :initarg :intensity :initform 0.2 :uniform T :accessor intensity)
   (exposure :initarg :exposure :initform 0.8 :uniform T :accessor exposure)
   (samples :initarg :samples :initform 12 :uniform T :accessor samples)
   (origin :initarg :origin :initform (vec 0.5 0.5) :uniform T :accessor origin))
  (:shader-file (trial "radial-blur.glsl")))

(define-shader-pass swirl-pass (simple-post-effect-pass)
  ((radius :initarg :radius :initform 1000.0 :uniform T :accessor radius)
   (angle :initarg :angle :initform 0.8 :uniform T :accessor angle))
  (:shader-file (trial "swirl.glsl")))

(define-shader-pass fxaa-pass (simple-post-effect-pass)
  ()
  (:shader-file (trial "fxaa.glsl")))

(define-shader-pass blend-pass (post-effect-pass)
  ((a-pass :port-type input)
   (b-pass :port-type input)
   (color :port-type output :reader color)
   (blend-type :initarg :blend-type :initform 0 :uniform T :accessor blend-type))
  (:shader-file (trial "blend.glsl")))

(defmethod (setf blend-type) ((value symbol) (pass blend-pass))
  ;; TODO: add more
  (setf (blend-type pass) (ecase value
                            (:b-over 0)
                            (:a-over 1)
                            (:add 2)
                            (:subtract 3)
                            (:multiply 4))))

(define-shader-pass high-pass-filter (simple-post-effect-pass)
  ((threshold :initarg :threshold :initform 1.0 :uniform T :accessor threshold))
  (:shader-file (trial "high-pass-filter.glsl")))

(define-shader-pass low-pass-filter (simple-post-effect-pass)
  ((threshold :initarg :threshold :initform 2.0 :uniform T :accessor threshold))
  (:shader-file (trial "low-pass-filter.glsl")))

(define-shader-pass chromatic-aberration-filter (simple-post-effect-pass)
  ((offset :initarg :offset :initform 3.0 :uniform T :accessor offset))
  (:shader-file (trial "aberration.glsl")))

(define-shader-pass luminance-pass (simple-post-effect-pass)
  ((color :texspec (:internal-format :r16f)))
  (:shader-file (trial "luminance.glsl")))

(define-shader-pass light-scatter-pass (post-effect-pass)
  ((previous-pass :port-type input)
   (black-render-pass :port-type input)
   (color :port-type output)
   (density :initarg :density :initform 1.0 :uniform T :accessor density)
   (weight :initarg :weight :initform 0.01 :uniform T :accessor weight)
   (decay :initarg :decay :initform 1.0 :uniform T :accessor decay)
   (exposure :initarg :exposure :initform 1.2 :uniform T :accessor exposure)
   (samples :initarg :samples :initform 100 :uniform T :accessor samples)
   (origin :initarg :origin :initform (vec 0.5 0.5) :uniform T :accessor origin))
  (:shader-file (trial "light-scatter.glsl")))

(define-shader-pass visualizer-pass (post-effect-pass)
  ((t[0] :port-type input)
   (t[1] :port-type input)
   (t[2] :port-type input)
   (t[3] :port-type input)
   (color :port-type output :texspec (:internal-format :rgba))
   (textures-per-line :initarg :textures-per-line :initform 1 :uniform T :accessor textures-per-line))
  (:shader-file (trial "visualizer.glsl")))

(defmethod check-consistent ((pass visualizer-pass))
  ;; Skip consistency checks to allow optional inputs
  T)
