(in-package #:org.shirakumo.fraf.trial)

(define-asset (trial ascii) image
    #p"ascii.raw"
  :internal-format :red
  :swizzle '(:r :r :r 1)
  :min-filter :nearest
  :mag-filter :nearest)

(defun print-ascii-text (text array &key (glyph-width 9) (glyph-height 17) (adjust T) (start 0) (x 0) (y 0) (z 0) (scale 1.0))
  (declare (optimize speed))
  (let ((i start)
        (scale (float scale 0f0))
        (xi (/ (float x 0f0) scale))
        (yi (/ (float y 0f0) scale))
        (z (float z 0f0))
        (max-x 0.0)
        (gw (float glyph-width 0f0))
        (gh (float glyph-height 0f0)))
    (declare (type (unsigned-byte 32) start i))
    (declare (type string text))
    (when adjust
      (adjust-array array (+ start (* 5 6 (length text)))))
    (macrolet ((vertex (&rest vals)
                 `(progn ,@(loop for val in vals
                                 collect `(progn (setf (aref array i) ,val)
                                                 (incf i))))))
      (flet ((print-letter (char)
               (let* ((c (clamp 0 (- (char-code char) (char-code #\Space)) 95))
                      (u0 (* gw (+ 0 c)))
                      (u1 (* gw (+ 1 c))))
                 (vertex (* scale (+ xi 0.0)) (* scale (+ yi 0.0)) z u0 0.0)
                 (vertex (* scale (+ xi  gw)) (* scale (+ yi 0.0)) z u1 0.0)
                 (vertex (* scale (+ xi  gw)) (* scale (+ yi  gh)) z u1  gh)
                 (vertex (* scale (+ xi  gw)) (* scale (+ yi  gh)) z u1  gh)
                 (vertex (* scale (+ xi 0.0)) (* scale (+ yi  gh)) z u0  gh)
                 (vertex (* scale (+ xi 0.0)) (* scale (+ yi 0.0)) z u0 0.0)
                 (incf xi gw))))
        (loop for char across text
              do (case char
                   (#\Linefeed
                    (when (< max-x xi)
                      (setf max-x xi))
                    (setf xi x)
                    (decf yi gh))
                   (#\Return)
                   (T (print-letter char))))))
    (values i (- max-x x) (- y))))

(define-shader-entity debug-text (located-entity vertex-entity textured-entity standalone-shader-entity)
  ((texture :initarg :font :initform (// 'trial 'ascii) :accessor font)
   (text :initarg :text :initform "" :accessor text)
   (size :initform (vec 0 0) :accessor size)
   (font-size :initarg :font-size :initform 17.0 :accessor font-size)
   (foreground :initarg :foreground :initform (vec4 0 0 0 1) :accessor foreground)
   (background :initarg :background :initform (vec4 0 0 0 0) :accessor background))
  (:inhibit-shaders (textured-entity :fragment-shader)))

(defmethod shared-initialize :after ((text debug-text) slots &key)
  (unless (slot-boundp text 'vertex-array)
    (let* ((array (make-array 0 :element-type 'single-float :adjustable T))
           (vbo (make-instance 'vertex-buffer :buffer-data array))
           (vao (make-instance 'vertex-array :bindings `((,vbo :size 3 :offset 0 :stride 20 :index 0)
                                                         (,vbo :size 2 :offset 12 :stride 20 :index 2)))))
      (setf (vertex-array text) vao)
      (setf (text text) (text text)))))

(defmethod (setf text) :after (_ (text debug-text))
  (let* ((vao (vertex-array text))
         (vbo (caar (bindings vao)))
         (array (buffer-data vbo)))
    (multiple-value-bind (i w h) (print-ascii-text (text text) array :scale (/ (font-size text) 17))
      (vsetf (size text) w h)
      (setf (size vao) (truncate i 5)))
    (when (allocated-p vao)
      (resize-buffer-data vbo (* 4 (length array)) :data array))))

(defmethod render :before ((text debug-text) (program shader-program))
  (setf (uniform program "foreground") (foreground text))
  (setf (uniform program "background") (background text)))

(define-class-shader (debug-text :vertex-shader)
  "out vec2 uv;
uniform sampler2D texture_image;

void main@after(){
  uv /= textureSize(texture_image, 0).rg;
}")

(define-class-shader (debug-text :fragment-shader)
  "in vec2 uv;
out vec4 color;
uniform sampler2D texture_image;
uniform vec4 foreground;
uniform vec4 background;

void main(){
  float fg_bg = texture(texture_image, uv, 0).r;
  color = mix(foreground, background, fg_bg);
}")

(define-shader-entity repl (debug-text listener)
  ((text :initform (make-array 4096 :adjustable T :fill-pointer T :element-type 'character))
   (history :initform (make-array 32 :adjustable T :fill-pointer 0) :accessor history)
   (history-index :initform 0 :accessor history-index)
   (line-count :initform 40 :initarg :line-count :accessor line-count)
   (input-start :initform 0 :accessor input-start)))

(defmethod initialize-instance :after ((repl repl) &key)
  (format (text repl) "~%This is ~a ~a, an implementation of ANSI Common Lisp.
as ~a @ ~a
on ~a ~a,
a ~a ~a machine
with ~a ~a~%"
          (lisp-implementation-type) (lisp-implementation-version)
          (username T) (machine-instance)
          (software-type) (software-version)
          (machine-type) (machine-version)
          +app-system+ (version :app))
  (setf (text repl) (text repl))
  (output-result () repl))

(define-handler (repl text-entered) (text)
  (loop for char across text do (vector-push-extend char (text repl)))
  (setf (text repl) (text repl)))

(define-handler (repl key-press) (key)
  (let ((text (text repl)))
    (case key
      (:up
       (decf (history-index repl)))
      (:down
       (incf (history-index repl)))
      (:backspace
       (when (< (input-start repl) (length text))
         (decf (fill-pointer text))
         (setf (text repl) text)))
      ((:enter :return)
       (if (= (input-start repl) (length text))
           (output-result () repl)
           (handler-case
               (let ((start (shiftf (input-start repl) (length text))))
                 (vector-push-extend (subseq text start) (history repl))
                 (setf (history-index repl) (length (history repl)))
                 (let* ((- (read-from-string text T NIL :start start))
                        (values (multiple-value-list (eval -))))
                   (shiftf *** ** * (first values))
                   (shiftf /// cl:// / values)
                   (shiftf +++ ++ + -)
                   (output-result values repl)))
             (error (e)
               (format (text repl) "~%ERROR: ~a~%" e)
               (output-result () repl))))))))

(defmethod (setf history-index) :around (index (repl repl))
  (call-next-method (clamp 0 index (length (history repl))) repl))

(defmethod (setf history-index) :after (index (repl repl))
  (let ((history-item (if (< index (length (history repl)))
                          (aref (history repl) index))))
    (setf (fill-pointer (text repl)) (+ (input-start repl) (length history-item)))
    (replace (text repl) history-item :start1 (input-start repl))
    (setf (text repl) (text repl))))

(defmethod output-result (values (repl repl))
  (let ((text (text repl)))
    (format text "~{~%~s~}" values)
    (format text "~%~a> " (package-abbreviation *package*))
    ;; Scroll lines
    (let ((to-remove (- (loop for char across text
                              count (char= char #\Linefeed))
                        (line-count repl))))
      (loop for i from 0 below (length text)
            do (when (<= to-remove 0)
                 (array-utils:array-shift text :n (- i))
                 (return))
               (when (char= (char text i) #\Linefeed)
                 (decf to-remove))))
    (setf (input-start repl) (length text))
    (setf (text repl) text)))
