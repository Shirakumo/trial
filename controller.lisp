#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defvar *observers* (make-array 0 :adjustable T :fill-pointer T))

(define-action-set system-action)

(define-action reload-scene (system-action))
(define-action quit-game (system-action))
(define-action toggle-overlay (system-action))

(defclass controller (entity listener)
  ((handlers :initform NIL :accessor handlers)
   (name :initform :controller)))

(defmethod handle ((ev quit-game) (controller controller))
  (quit *context*))

(defmethod handle ((ev event) (controller controller))
  (dolist (handler (handlers controller))
    (handle ev handler))
  (map-event ev (scene +main+)))

(defmethod handle ((ev lose-focus) (controller controller))
  (clear-retained))

(defmethod handle ((ev reload-scene) (controller controller))
  (let ((old (scene +main+)))
    (change-scene +main+ (make-instance (type-of old)))))

(defmethod observe ((func function) &key title)
  (let ((title (or title (format NIL "~d" (length *observers*)))))
    (let ((position (position title *observers* :key #'car :test #'equal)))
      (if position
          (setf (aref *observers* position) (cons title func))
          (vector-push-extend (cons title func) *observers*)))
    func))

(defmethod observe (thing &rest args &key &allow-other-keys)
  (apply #'observe (compile NIL `(lambda (ev)
                                   (declare (ignorable ev))
                                   ,thing))
         args))

(defmacro observe! (form &rest args)
  (let ((ev (gensym "EV")))
    `(observe (lambda (,ev) (declare (ignore ,ev)) ,form) ,@args)))

(defmethod stop-observing (&optional title)
  (let ((observers *observers*))
    (if title
        (let ((pos (position title observers :key #'car :test #'equal)))
          (when pos (array-utils:vector-pop-position observers pos)))
        (loop for i from 0 below (array-total-size observers)
              do (setf (aref observers i) NIL)
              finally (setf (fill-pointer observers) 0)))))

(defclass load-request (event)
  ((thing :initarg :thing)))

(define-handler (controller load-request) (thing)
  (typecase thing
    (asset
     (if (loaded-p thing)
         (reload thing)
         (load thing)))
    (resource
     (unless (allocated-p thing)
       (allocate thing)))
    (T
     (commit thing (loader +main+) :unload NIL))))

(defun maybe-reload-scene (&optional (main +main+))
  (when main
    (issue (scene main) 'reload-scene)))

(defclass eval-request (event)
  ((func :initarg :func)))

(define-handler (controller eval-request) (func)
  (funcall func))

(defun call-in-render-loop (function scene)
  (issue scene 'eval-request :func function))

(defmacro with-eval-in-render-loop ((scene) &body body)
  `(call-in-render-loop (lambda () ,@body) ,scene))

(define-shader-entity display-controller (controller debug-text)
  ((fps-buffer :initform (make-array 100 :fill-pointer T :initial-element 1) :reader fps-buffer)
   (background :initform (vec4 1 1 1 0.3))
   (show-overlay :initform T :accessor show-overlay)))

(defmethod handle ((ev toggle-overlay) (controller display-controller))
  (setf (show-overlay controller) (not (show-overlay controller))))

(defun compute-fps-buffer-fps (fps-buffer)
  (/ (loop for i from 0 below (array-total-size fps-buffer)
           sum (aref fps-buffer i))
     (array-total-size fps-buffer)))

(defun compose-controller-debug-text (controller ev)
  (multiple-value-bind (gfree gtotal) (gpu-room)
    (multiple-value-bind (cfree ctotal) (cpu-room)
      (with-output-to-string (stream)
        (format stream "FPS  [Hz]: ~8,2f~%~
                        RAM  [KB]: ~8d (~2d%)~%~
                        VRAM [KB]: ~8d (~2d%)~%~
                        RESOURCES: ~8d"
                (compute-fps-buffer-fps (fps-buffer controller))
                (- ctotal cfree) (floor (/ (- ctotal cfree) ctotal 0.01))
                (- gtotal gfree) (floor (/ (- gtotal gfree) gtotal 0.01))
                (hash-table-count (loaded (loader +main+))))
        (loop with observers = *observers*
              for i from 0 below (length observers)
              for (title . func) = (aref observers i)
              when func
              do (restart-case (format stream "~%~a:~12t~a" title (funcall func ev))
                   (remove-observer ()
                     :report "Remove the offending observer."
                     (setf (aref observers i) NIL))))))))

(defmethod handle ((ev tick) (controller display-controller))
  (when (and (show-overlay controller)
             *context*)
    (setf (text controller) (compose-controller-debug-text controller ev))
    (setf (vy (location controller)) (- (vy (size controller))))
    (setf (vx (location controller)) 5)))

(defmethod apply-transforms progn ((controller display-controller))
  (orthographic-projection 0 (width *context*)
                           0 (height *context*)
                           0 10)
  (setf *view-matrix* (meye 4))
  (setf *model-matrix* (meye 4))
  (translate-by 2 (- (height *context*) 14) 0))

(defmethod render :around ((controller display-controller) (program shader-program))
  (when (show-overlay controller)
    (let ((fps-buffer (fps-buffer controller)))
      (when (= (array-total-size fps-buffer) (fill-pointer fps-buffer))
        (setf (fill-pointer fps-buffer) 0))
      (vector-push (if (= 0 (frame-time +main+))
                       1
                       (/ (frame-time +main+)))
                   fps-buffer))
    
    (call-next-method)))
