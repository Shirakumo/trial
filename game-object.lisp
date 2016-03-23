#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

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

(defmethod draw :around ((obj textured-subject))
  (when (texture obj)
    (call-next-method)))

(defmethod bind-texture ((obj textured-subject))
  (gl:bind-texture :texture-2d (q+:texture (texture obj)))
  (gl:tex-parameter :texture-2d :texture-min-filter :linear)
  (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
  (gl:tex-parameter :texture-2d :texture-wrap-s :clamp)
  (gl:tex-parameter :texture-2d :texture-wrap-t :clamp))

(define-subject located-subject ()
  ((location :initform (vec 0 0 0) :accessor location)))

(defmethod draw :around ((obj located-subject))
  (let ((pos (location obj)))
    (gl:translate (vx pos) (vy pos) (vz pos))
    (call-next-method)))

(define-subject oriented-subject ()
  ((right :initform (vec 1 0 0) :accessor right)
   (up :initform (vec 0 1 0) :accessor up)
   (direction :initform (vec 0 0 1) :accessor direction)
   (angle :initform 0 :accessor angle)))

(defmethod initialize-instance :after ((obj oriented-subject) &key)
  (setf (right obj) (normalize (cross (up obj) (direction obj)))
        (up obj) (cross (direction obj) (right obj))))

(defmethod draw :around ((obj oriented-subject))
  (let ((vector (up obj)))
    (gl:rotate (slot-value obj 'angle) (vx vector) (vy vector) (vz vector)))
  (call-next-method))

(define-subject cat (located-subject oriented-subject textured-subject)
  ((angle-delta :initform 1 :accessor angle-delta)
   (velocity :initform (vec 0 0 0) :accessor velocity))
  (:default-initargs :texture "cat.png"))

(defmethod initialize-instance :after ((cat cat) &key)
  (setf (location cat) (vec 0 0 0)))

(defmethod draw ((cat cat))
  (gl:enable :texture-2d)
  
  (bind-texture cat)
  (gl:begin :quads)
  (gl:color 1 1 1)
  (gl:tex-coord 1 1) (gl:vertex -1 1 1)
  (gl:tex-coord 1 0) (gl:vertex 1 1 1)
  (gl:tex-coord 0 0) (gl:vertex 1 1 -1)
  (gl:tex-coord 0 1) (gl:vertex -1 1 -1)
  
  (gl:tex-coord 1 0) (gl:vertex -1 -1 1)
  (gl:tex-coord 0 0) (gl:vertex -1 -1 -1)
  (gl:tex-coord 0 1) (gl:vertex 1 -1 -1)
  (gl:tex-coord 1 1) (gl:vertex 1 -1 1)
  
  (gl:tex-coord 1 0) (gl:vertex -1 1 1)
  (gl:tex-coord 0 0) (gl:vertex -1 -1 1)
  (gl:tex-coord 1 0.3) (gl:vertex 1 1 1)
  
  (gl:tex-coord 0 0.3) (gl:vertex 1 -1 1)
  
  (gl:tex-coord 1 0.5) (gl:vertex 1 1 -1)
  (gl:tex-coord 0 0.5) (gl:vertex 1 -1 -1)
  
  (gl:tex-coord 1 0.75) (gl:vertex -1 1 -1)
  (gl:tex-coord 0 0.75) (gl:vertex -1 -1 -1)
  
  (gl:tex-coord 1 0.95) (gl:vertex -1 1 1)
  (gl:tex-coord 0 0.95) (gl:vertex -1 -1 1)
  (gl:end))

(define-handler (cat update tick) (ev)
  (incf (angle cat) (angle-delta cat))
  (nv+ (location cat) (velocity cat)))

(define-handler (cat catty-go key-press) (ev key)
  (case key
    (:left (setf (vx (velocity cat)) -5))
    (:right (setf (vx (velocity cat)) 5))
    (:up (setf (vy (velocity cat)) 5))
    (:down (setf (vy (velocity cat)) -5))))

(define-handler (cat catty-stop key-release) (ev key)
  (case key
    (:left (setf (vx (velocity cat)) 0))
    (:right (setf (vx (velocity cat)) 0))
    (:up (setf (vy (velocity cat)) 0))
    (:down (setf (vy (velocity cat)) 0))))

(define-subject camera (located-subject oriented-subject)
  ((target :initform (vec 0 0 0) :accessor target)))

(defmethod initialize-instance :after ((camera camera) &key)
  (setf (location camera) (vec 0 0 3)
        (direction camera) (normalize (v- (location camera) (target camera)))))

(defun normalize (vector)
  (let ((length (sqrt (+ (* (vx vector) (vx vector))
                         (* (vy vector) (vy vector))
                         (* (vz vector) (vz vector))))))
    (if (/= length 0)
        (vec (/ (vx vector) length)
             (/ (vy vector) length)
             (/ (vz vector) length))
        vector)))

(defun cross (vector-a vector-b)
  (let ((ax (vx vector-a)) (ay (vy vector-a)) (az (vz vector-a))
        (bx (vx vector-b)) (by (vy vector-b)) (bz (vz vector-b))))
  (vec (- (* ay bz) (* az by))
       (- (* az bx) (* ax bz))
       (- (* ax by) (* ay bx))))

(defun lookat (target up)
  (let* ((forward (normalize target))
         (side (normalize (v* up forward)))
         (up (v* forward side))
         (matrix (make-array (* 3 3))))
    (setf (elt matrix 0) (vx side) ;; first row
          (elt matrix 1) (vy side)
          (elt matrix 2) (vz side)
          (elt matrix 3) (vx up)   ;; second row
          (elt matrix 4) (vy up)
          (elt matrix 5) (vz up)
          (elt matrix 6) (vx forward) ;; third row
          (elt matrix 7) (vy forward)
          (elt matrix 8) (vz forward))))
