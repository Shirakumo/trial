#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

;; Textured Subject

(define-subject textured-subject ()
  ((texture :initform NIL :accessor texture :finalized T)))

(defmethod initialize-instance :after ((subject textured-subject) &key (texture NIL t-p) &allow-other-keys)
  (when t-p (setf (texture subject) texture)))

(defmethod reinitialize-instance :after ((subject textured-subject) &key (texture NIL t-p) &allow-other-keys)
  (when t-p (setf (texture subject) texture)))

(defmethod (setf texture) :around (texture (subject textured-subject))
  (let ((prev (finalize (texture subject))))
    (call-next-method)
    (finalize prev)))

(defmethod (setf texture) (texture (subject textured-subject))
  (setf (slot-value subject 'texture)
        (qtypecase texture
          (QImage (image->framebuffer texture))
          (QGLFramebufferObject texture)
          (T (error "Don't know how to use ~a as a texture for ~a." texture subject)))))

(defmethod (setf texture) ((path pathname) (subject textured-subject))
  (setf (texture subject) (load-image-buffer (resource-pathname path))))

(defmethod (setf texture) ((path string) (subject textured-subject))
  (setf (texture subject) (uiop:parse-native-namestring path)))

(defmethod (setf texture) ((null null) (subject textured-subject))
  (setf (slot-value subject 'texture) NIL))

(defmethod draw ((obj textured-subject))
  (let ((texture (texture obj)))
    (when texture
      (let ((size (q+:size texture)))
        (with-finalizing ((point (q+:make-qpointf (- (/ (q+:width size) 2))
                                                  (- (/ (q+:height size) 2)))))
          (q+:draw-texture *main-window* point (q+:texture texture)))))))

;; Located Subject

(define-subject located-subject ()
  ((location :initform (vec 0 0 0) :accessor location)))

(defmethod draw :around ((obj located-subject))
  (let ((pos (location obj)))
    (gl:push-matrix)
    (gl:translate (vx pos) (vy pos) (vz pos))
    (call-next-method)
    (gl:pop-matrix)))

;; Oriented Subject

(define-subject oriented-subject ()
  ((orientation :initform (vec 0 0 1) :accessor orientation)
   (angle :initform 0 :accessor angle)))

(defmethod draw ((obj oriented-subject))
  (let ((vector (orientation obj)))
    (gl:rotate (slot-value obj 'angle) (vx vector) (vy vector) (vz vector)))
  (call-next-method))

;; Cat (the test subject)

(define-subject cat (located-subject oriented-subject textured-subject)
  ((angle-delta :initform 1 :accessor angle-delta)
   (orientation-delta :initform (vec 1 2 3) :accessor orientation-delta)
   (velocity :initform (vec 0 0 0) :accessor velocity))
  (:default-initargs :texture "cat.png"))

(defmethod draw ((cat cat))
  (call-next-method))

(define-handler (cat update tick) (ev)
  (incf (angle cat) (angle-delta cat))
  (nv+ (orientation cat) (orientation-delta cat))
  (nv+ (location cat) (velocity cat)))

(define-handler (cat catty-go key-press) (ev key)
  (case key
    (:left (setf (vx (velocity cat)) -5))
    (:right (setf (vx (velocity cat)) 5))
    (:up (setf (vy (velocity cat)) -5))
    (:down (setf (vy (velocity cat)) 5))))

(define-handler (cat catty-stop key-release) (ev key)
  (case key
    (:left (setf (vx (velocity cat)) 0))
    (:right (setf (vx (velocity cat)) 0))
    (:up (setf (vy (velocity cat)) 0))
    (:down (setf (vy (velocity cat)) 0))))
