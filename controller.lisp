#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defvar *observers* (make-array 0 :adjustable T :fill-pointer T))

(defclass system-action (action-set) ())

(define-action reload-scene (system-action))

(define-action quit-game (system-action)
  (key-press (and (eql key :q) (find :control modifiers))))

(define-action toggle-overlay (system-action)
  (key-press (one-of key :section :grave)))

(defclass controller (entity listener)
  ((display :initform NIL :accessor display))
  (:default-initargs
   :name :controller))

(defmethod handle ((ev quit-game) (controller controller))
  (quit *context*))

(defmethod handle ((ev event) (controller controller))
  (map-event ev *scene*))

(defmethod handle ((ev lose-focus) (controller controller))
  (clear-retained))

(defmethod handle ((ev reload-scene) (controller controller))
  (let ((old (scene (display controller))))
    (change-scene (display controller) (make-instance (type-of old) :clock (clock old)))))

(defun find-controller ()
  (or (when *context* (unit :controller (scene (handler *context*))))
      (error "No reachable controller found.")))

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
     (commit thing (loader (display controller)) :unload NIL))))

(defun maybe-reload-scene (&optional (window (list-windows)))
  (dolist (window (enlist window))
    (issue (scene window) 'reload-scene)))

(defclass display-controller (controller renderable)
  ((text :initform NIL :accessor text)
   (fps-buffer :initform (make-array 100 :fill-pointer T :initial-element 1) :reader fps-buffer)
   (show-overlay :initform T :accessor show-overlay)))

(defmethod register-object-for-pass (pass (controller display-controller))
  (register-object-for-pass pass (text controller)))

(defmethod stage ((controller display-controller) (area staging-area))
  (stage (text controller) area))

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
        (format stream "TIME  [s]: ~8,2f~%~
                        FPS  [Hz]: ~8,2f~%~
                        RAM  [KB]: ~8d (~2d%)~%~
                        VRAM [KB]: ~8d (~2d%)~%~
                        RESOURCES: ~8d"
                (clock (scene (display controller)))
                (compute-fps-buffer-fps (fps-buffer controller))
                (- ctotal cfree) (floor (/ (- ctotal cfree) ctotal 0.01))
                (- gtotal gfree) (floor (/ (- gtotal gfree) gtotal 0.01))
                (hash-table-count (loaded (loader (display controller)))))
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
    (let ((text (text controller)))
      (setf (vy (location text))
            (- -5 (getf (text-extent text "a") :t)))
      (setf (vx (location text)) 5)
      (setf (text text) (compose-controller-debug-text controller ev)))))

(defmethod render ((controller display-controller) program)
  (when (show-overlay controller)
    (let ((fps-buffer (fps-buffer controller)))
      (when (= (array-total-size fps-buffer) (fill-pointer fps-buffer))
        (setf (fill-pointer fps-buffer) 0))
      (vector-push (if (= 0 (frame-time (display controller)))
                       1
                       (/ (frame-time (display controller))))
                   fps-buffer))
    (with-pushed-matrix ((*projection-matrix* :zero)
                         (*model-matrix* :identity)
                         (*view-matrix* :identity))
      (orthographic-projection 0 (width *context*)
                               0 (height *context*)
                               0 10)
      (translate-by 2 (- (height *context*) 14) 0)
      (render (text controller) program))))
