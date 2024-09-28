(in-package #:org.shirakumo.fraf.trial.examples)

(define-shader-entity mandelbrot (vertex-entity listener alloy:observable-object)
  ((vertex-array :initform (// 'trial 'fullscreen-square))
   (max-iterations :initform 500 :accessor max-iterations :uniform T)
   (zoom :initform 1.0 :accessor zoom :uniform T)
   (center :initform (vec2) :accessor center :uniform T)))

(define-handler (mandelbrot tick) (dt)
  (when (retained :a) (decf (vx (center mandelbrot)) (/ dt (zoom mandelbrot))))
  (when (retained :d) (incf (vx (center mandelbrot)) (/ dt (zoom mandelbrot))))
  (when (retained :s) (decf (vy (center mandelbrot)) (/ dt (zoom mandelbrot))))
  (when (retained :w) (incf (vy (center mandelbrot)) (/ dt (zoom mandelbrot))))
  (when (retained :q) (decf (zoom mandelbrot) (* dt (zoom mandelbrot))))
  (when (retained :e) (incf (zoom mandelbrot) (* dt (zoom mandelbrot)))))

(defmethod render :before ((mandelbrot mandelbrot) (program shader-program))
  (setf (uniform program "screen_size") (size *context*)))

(define-class-shader (mandelbrot :fragment-shader)
  "out vec4 color;
uniform int max_iterations = 500;
uniform float zoom = 1.0;
uniform vec2 center = vec2(0);
uniform vec2 screen_size = vec2(1280,720);

void main(){
  float size = min(screen_size.x, screen_size.y);
  float real = (gl_FragCoord.x-(screen_size.x*0.5)) / (size*zoom) + center.x;
  float imag = (gl_FragCoord.y-(screen_size.y*0.5)) / (size*zoom) + center.y;
 
  int iterations = 0;
  float const_real = real;
  float const_imag = imag;
 
  for(; iterations < max_iterations; ++iterations){
    float tmp_real = real;
    real = (real * real - imag * imag) + const_real;
    imag = (2.0 * tmp_real * imag) + const_imag;
    
    float dist = real * real + imag * imag;
     
    if(dist > 4.0) break;
  }

  float quotient = float(iterations) / max_iterations;
  if(quotient == 1.0){
    color = vec4(1);
  }else if(quotient > 0.5){
    color = vec4(quotient, 1.0, quotient, 1.0);
  }else{
    color = vec4(0.0, quotient, 0.0, 1.0);
  }
}")

(define-example shader
  :title "Custom Shaders"
  :description "An illustration of custom shaders in Trial by way of a Mandelbrot simulation."
  (enter (make-instance 'display-controller) scene)
  (observe! "WASD to move, QE to zoom" :title "Controls")
  (enter (make-instance 'mandelbrot :name :mandelbrot) scene)
  (enter (make-instance 'render-pass) scene))

(defmethod setup-ui ((scene shader-scene) panel)
  (let* ((layout (make-instance 'alloy:grid-layout :col-sizes '(T 120 200) :row-sizes '(30)))
         (focus (make-instance 'alloy:vertical-focus-list))
         (row 0))
    (macrolet ((row (label repr input &rest args)
                 `(prog2 (alloy:enter ,label layout :row row :col 1)
                      (alloy:represent ,repr ,input ,@args :focus-parent focus :layout-parent layout)
                    (incf row))))
      (row "Zoom" (slot-value (node :mandelbrot scene) 'zoom) 'alloy:ranged-slider :range '(1.0 . 100.0))
      (row "Center" (slot-value (node :mandelbrot scene) 'center) T))
    (alloy:finish-structure panel layout focus)))
