(in-package #:org.shirakumo.fraf.trial)

(declaim (type mat4
               *view-matrix*
               *projection-matrix*
               *model-matrix*))

(declaim (inline view-matrix (setf view-matrix)
                 projection-matrix (setf projection-matrix)
                 model-matrix (setf model-matrix)
                 push-matrix pop-matrix
                 translate translate-by
                 rotate rotate-by
                 scale scale-by))

(declaim (ftype (function () mat4)
                projection-matrix
                view-matrix
                model-matrix))

(defvar *view-matrix* (meye 4))
(defvar *projection-matrix* (meye 4))
(defvar *model-matrix* (meye 4))
(define-global +matrix-index+ 0)
(define-global +matrix-stack+
    (let ((stack (make-array 30)))
      (dotimes (i (length stack) stack)
        (setf (aref stack i) (meye 4)))))

(defun view-matrix ()
  *view-matrix*)

(defun (setf view-matrix) (mat4)
  (replace (marr *view-matrix*) (marr mat4)))

(defun projection-matrix ()
  *projection-matrix*)

(defun (setf projection-matrix) (mat4)
  (replace (marr *projection-matrix*) (marr mat4)))

(defun model-matrix ()
  *model-matrix*)

(defun (setf model-matrix) (mat4)
  (replace (marr *model-matrix*) (marr mat4)))

(defun look-at (eye target up)
  (nmlookat *view-matrix* eye target up))

(defun perspective-projection (fovy aspect near far)
  (nmperspective *projection-matrix* fovy aspect near far))

(defun orthographic-projection (left right bottom top near far)
  (nmortho *projection-matrix* left right bottom top near far))

(defun push-matrix ()
  (replace (marr4 (aref +matrix-stack+ +matrix-index+)) (marr4 *model-matrix*))
  (incf +matrix-index+))

(defun pop-matrix ()
  (decf +matrix-index+)
  (replace (marr4 *model-matrix*) (marr4 (aref +matrix-stack+ +matrix-index+))))

(defmacro with-pushed-matrix (specs &body body)
  (let ((specs (loop for spec in (or specs '(((model-matrix) :copy)))
                     for (accessor fill) = (enlist spec :copy)
                     for variable = (ecase (unlist accessor)
                                      ((*view-matrix* view-matrix) '*view-matrix*)
                                      ((*projection-matrix* projection-matrix) '*projection-matrix*)
                                      ((*model-matrix* model-matrix) '*model-matrix*))
                     collect (list variable fill))))
    `(let ,(loop for (variable fill) in specs
                 collect `(,variable
                           ,(case fill
                              (:zero `(mat4))
                              (:identity `(meye 4))
                              ((:copy NIL) `(mcopy ,variable))
                              (T fill))))
       (declare (dynamic-extent ,@(mapcar #'first specs)))
       ,@body)))

(defun translate (v &optional (matrix (model-matrix)))
  (declare (type vec3 v) (type mat4 matrix))
  (nmtranslate matrix v))

(defun translate-by (x y z &optional (matrix (model-matrix)))
  (let ((vec (vec (float x 0f0) (float y 0f0) (float z 0f0))))
    (declare (dynamic-extent vec))
    (nmtranslate matrix vec)))

(defun rotate (v angle &optional (matrix (model-matrix)))
  (declare (type vec3 v) (type mat4 matrix))
  (nmrotate matrix v angle))

(defun rotate-by (x y z angle &optional (matrix (model-matrix)))
  (let ((vec (vec (float x 0f0) (float y 0f0) (float z 0f0))))
    (declare (dynamic-extent vec))
    (nmrotate matrix vec angle)))

(defun scale (v &optional (matrix (model-matrix)))
  (declare (type vec3 v) (type mat4 matrix))
  (nmscale matrix v))

(defun scale-by (x y z &optional (matrix (model-matrix)))
  (let ((vec (vec (float x 0f0) (float y 0f0) (float z 0f0))))
    (declare (dynamic-extent vec))
    (nmscale matrix vec)))

(defun reset-matrix (&optional (matrix (model-matrix)))
  (!meye matrix))

(defun vec->screen (vec)
  (let* ((clip-pos (n*m (projection-matrix) (n*m (view-matrix) (vec (vx vec)
                                                                    (vy vec)
                                                                    (vz vec)
                                                                    1))))
         (w (vw clip-pos)))
    (if (= 0.0f0 w)
        (vec -1 -1 0)
        (let* ((norm-pos (nv+ (nv* (vxy clip-pos) (/ 0.5f0 w)) 0.5f0))
               (context *context*))
          (vsetf norm-pos
                 (* (width context) (vx norm-pos))
                 (* (height context) (vy norm-pos)))))))

(defun screen->vec (vec &optional (z 0))
  ;; FIXME: this is completely useless lmao.
  ;;        we need to be able to pass a plane to project onto.
  (let* ((context *context*)
         (x (- (* 2 (/ (vx vec) (width context))) 1))
         (y (+ (* -2 (/ (vy vec) (height context))) 1))
         (inv (minv (m* (projection-matrix) (view-matrix))))
         (res (m* inv (vec4 x y z 1))))
    (vec3 (/ (vx res) (vw res))
          (/ (vy res) (vw res))
          0)))
