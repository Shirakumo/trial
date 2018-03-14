#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-shader-entity text (located-entity vertex-entity colored-entity textured-entity readied)
  ((texture :initarg :font :accessor font)
   (text :initarg :text :accessor text)
   (size :initarg :size :accessor size)
   (wrap :initarg :wrap :accessor wrap)
   (width :initarg :width :accessor width)
   (height :initarg :height :accessor height)
   (vbo) (ebo))
  (:default-initargs
   :color (vec 0 0 0 1)
   :text ""
   :wrap NIL
   :width NIL
   :height NIL
   :font (error "FONT required."))
  (:inhibit-shaders (colored-entity :fragment-shader)))

(defmethod initialize-instance :after ((text text) &key size)
  (let* ((vbo (make-instance 'vertex-buffer :buffer-type :array-buffer
                                            :data-usage :dynamic-draw
                                            :size 0))
         (ebo (make-instance 'vertex-buffer :buffer-type :element-array-buffer
                                            :data-usage :dynamic-draw
                                            :size 0))
         (vao (make-instance 'vertex-array :bindings `((,vbo :size 2 :stride 16 :offset 0)
                                                       (,vbo :size 2 :stride 16 :offset 8)
                                                       ,ebo))))
    (setf (slot-value text 'vbo) vbo)
    (setf (slot-value text 'ebo) ebo)
    (setf (vertex-array text) vao)
    (unless size (setf (size text) (size (font text))))))

(defmethod paint :around ((text text) (pass shader-pass))
  (let ((r (/ (size text) (size (font text)))))
    (with-pushed-matrix (*model-matrix*)
      (scale-by r r r)
      (call-next-method))))

(define-class-shader (text :fragment-shader)
  "uniform vec4 objectcolor;
out vec4 color;

void main(){
   float intensity = color.r;
   color = objectcolor*intensity;
}")

(defmethod (setf font) :after (font (text text))
  (when (allocated-p font)
    (setf (text text) (text text))))

(defmethod resources-ready ((text text))
  (setf (text text) (text text)))

(defun wrap-text (font string width)
  ;; FIXME: This is really primitive.
  (destructuring-bind (&key l r ((:t u)) b gap) (text-extent font " ")
    (let ((s (+ r l))
          (current-width 0)
          (height (- u b))
          (buffer (make-string-output-stream))
          (vskip (/ (* (size font) (+ gap u)) (+ u b))))
      (values (with-output-to-string (out)
                (flet ((complete-word ()
                         (let* ((word (get-output-stream-string buffer))
                                (ext (text-extent font word))
                                (w (+ (getf ext :r) (getf ext :l))))
                           (when (<= width (+ current-width s w))
                             (write-char #\Linefeed out)
                             (setf current-width 0)
                             (incf height vskip))
                           (write-string word out)
                           (write-char #\Space out)
                           (incf current-width (+ s w)))))
                  (loop for c across string
                        do (case c
                             (#\Space
                              (complete-word))
                             (#\Linefeed
                              (setf current-width width)
                              (complete-word))
                             (T
                              (write-char c buffer)))
                        finally (complete-word))))
              height))))

(defmethod (setf width) :after (width (text text))
  (when (wrap text)
    (setf (text text) (text text))))

(defmethod (setf text) :before (string (text text))
  (let ((vao (vertex-array text))
        (vbo (slot-value text 'vbo))
        (ebo (slot-value text 'ebo))
        (font (font text)))
    (when (allocated-p font)
      (if (wrap text) 
          (multiple-value-bind (string height) (wrap-text font string (width text))
            (setf (height text) height)
            (setf (size vao) (cl-fond:update-text font string (gl-name vbo) (gl-name ebo))))
          (destructuring-bind (&key l r ((:t u)) b gap) (text-extent text string)
            (declare (ignore gap))
            (setf (width text) (+ l r))
            (setf (height text) (+ u b))
            (setf (size vao) (cl-fond:update-text font string (gl-name vbo) (gl-name ebo))))))))

(defmethod extent ((text text))
  (text-extent text (text text)))

(defmethod text-extent ((text text) string)
  (destructuring-bind (&key l r ((:t u)) b gap) (text-extent (font text) string)
    (let ((s (/ (size text) (size (font text)))))
      (list :l (* l s) :r (* r s) :t (* u s) :b (* b s) :gap (* gap s)))))
