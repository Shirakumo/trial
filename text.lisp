#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-shader-entity text (located-entity vertex-entity colored-entity textured-entity readied)
  ((texture :initarg :font :initform (error "FONT required.") :accessor font)
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
   :height NIL)
  (:inhibit-shaders (colored-entity :fragment-shader)
                    (textured-entity :fragment-shader)))

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

(defmethod paint ((text text) (pass shader-pass))
  (let ((r (/ (size text) (size (font text)))))
    (with-pushed-matrix (*model-matrix*)
      (scale-by r r r)
      (call-next-method))))

(define-class-shader (text :fragment-shader)
  "uniform vec4 objectcolor;
uniform sampler2D texture_image;
in vec2 texcoord;
out vec4 color;

void main(){
  float intensity = texture(texture_image, texcoord, -0.65).r;
  color = objectcolor * intensity;
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
                              (complete-word)
                              (setf current-width width))
                             (T
                              (write-char c buffer)))
                        finally (complete-word))))
              height))))

(defmethod (setf width) :after (width (text text))
  (when (wrap text)
    (setf (text text) (text text))))

(defmethod (setf text) :before ((string string) (text text))
  (let ((vao (vertex-array text))
        (vbo (slot-value text 'vbo))
        (ebo (slot-value text 'ebo))
        (font (font text)))
    (when (and (allocated-p font)
               (allocated-p vao))
      (let ((s (/ (size (font text)) (size text))))
        (if (wrap text)
            (multiple-value-bind (string height) (wrap-text font string (* (width text) s))
              (setf (height text) (/ height s))
              (setf (size vao) (cl-fond:update-text font string (gl-name vbo) (gl-name ebo))))
            (destructuring-bind (&key l r ((:t u)) b gap) (text-extent font string)
              (declare (ignore gap))
              (setf (width text) (/ (+ l r) s))
              (setf (height text) (/ (- u b) s))
              (setf (size vao) (cl-fond:update-text font string (gl-name vbo) (gl-name ebo)))))))))

(defmethod extent ((text text))
  (text-extent text (text text)))

(defmethod text-extent ((text text) string)
  (destructuring-bind (&key l r ((:t u)) b gap) (text-extent (font text) string)
    (let ((s (/ (size text) (size (font text)))))
      (list :l (* l s) :r (* r s) :t (* u s) :b (* b s) :gap (* gap s)))))

(define-shader-entity highlighted-text (text)
  ((cbo)
   (color-regions :accessor color-regions))
  (:default-initargs :color-regions ())
  (:inhibit-shaders (text :fragment-shader)))

(defmethod initialize-instance :after ((text highlighted-text) &key color-regions)
  (let* ((cbo (make-instance 'vertex-buffer :buffer-type :array-buffer
                                            :data-usage :dynamic-draw
                                            :size 0))
         (ebo (slot-value text 'ebo))
         (vbo (slot-value text 'vbo))
         (vao (vertex-array text)))
    (setf (slot-value text 'cbo) cbo)
    (setf (bindings vao) `((,vbo :size 2 :stride 16 :offset 0)
                           (,vbo :size 2 :stride 16 :offset 8)
                           (,cbo :size 4 :stride 16 :offset 0)
                           ,ebo))
    (setf (color-regions text) color-regions)))

(defun %update-highlight-buffer (text length)
  (let ((cbo (slot-value text 'cbo))
        (size (* 4 4 length))
        (unit (color text)))
    (cffi:with-foreign-object (array :float size)
      (loop with regions = (color-regions text)
            for i from 0 below length
            do (flet ((insert (c)
                        ;; FIXME: Ahead-of-time translation of colors to buffers
                        ;;        and then simply use memcpy here.
                        (loop for j from (* 4 4 i) by 4 repeat 4
                              do (setf (cffi:mem-aref array :float (+ j 0)) (vx c))
                                 (setf (cffi:mem-aref array :float (+ j 1)) (vy c))
                                 (setf (cffi:mem-aref array :float (+ j 2)) (vz c))
                                 (setf (cffi:mem-aref array :float (+ j 3)) (vw c)))))
                 (if regions
                     (destructuring-bind (s e c) (first regions)
                       (cond ((< i s)
                              (insert unit))
                             ((< i e)
                              (insert c))
                             (T
                              (pop regions)
                              (decf i))))
                     (insert unit))))
      (setf (size cbo) (* 4 size))
      (update-buffer-data/ptr cbo array (size cbo)))))

(defmethod (setf color-regions) :around (regions (text highlighted-text))
  ;; FIXME: Check for overlapping regions.
  (let ((regions (sort regions #'< :key #'first)))
    (call-next-method regions text)
    (when (allocated-p (slot-value text 'cbo))
      (%update-highlight-buffer text (length (text text))))))

(defmethod (setf text) :before (string (text highlighted-text))
  (let ((cbo (slot-value text 'cbo))
        (font (font text)))
    (when (and (allocated-p font)
               (allocated-p cbo))
      (%update-highlight-buffer text (length string)))))

(define-class-shader (highlighted-text :vertex-shader)
  "layout (location = 2) in vec4 color;
out vec4 character_color;

void main(){
  character_color = color;
}")

(define-class-shader (highlighted-text :fragment-shader)
  "uniform sampler2D texture_image;
in vec2 texcoord;
in vec4 character_color;
out vec4 color;

void main(){
  float intensity = texture(texture_image, texcoord, -0.65).r;
  color = character_color*intensity;
}")
