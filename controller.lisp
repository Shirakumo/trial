#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-action system-action ())

(define-action reload-scene (system-action))

(define-action quit-game (system-action)
  (key-press (and (eql key :q) (find :control modifiers))))

(define-action toggle-overlay (system-action)
  (key-press (one-of key :section :grave)))

(define-asset (trial noto-sans) font
    #p"noto-sans-regular.ttf")

(define-asset (trial noto-mono) font
    #p"noto-mono-regular.ttf")

(defclass controller (entity renderable listener)
  ((display :initform NIL :accessor display)
   (text :initform (make-instance 'text :font (@r 'trial 'noto-mono) :size 18) :accessor text)
   (fps-buffer :initform (make-array 100 :fill-pointer T :initial-element 1) :reader fps-buffer)
   (show-overlay :initform T :accessor show-overlay)
   (observers :initform (make-array 0 :adjustable T :fill-pointer T) :accessor observers))
  (:default-initargs
   :name :controller))

(defmethod register-object-for-pass :after (pass (controller controller))
  (register-object-for-pass pass (text controller)))

(defmethod handle ((ev toggle-overlay) (controller controller))
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
                (hash-table-count (resources *context*)))
        (loop with observers = (observers controller)
              for i from 0 below (length observers)
              for (title . func) = (aref observers i)
              when func
              do (restart-case (format stream "~%~a:~12t~a" title (funcall func ev))
                   (remove-observer ()
                     :report "Remove the offending observer."
                     (setf (aref observers i) NIL))))))))

(defmethod handle ((ev tick) (controller controller))
  (when (and (show-overlay controller)
             *context*)
    (let ((text (text controller)))
      (setf (vy (location text))
            (- -5 (getf (text-extent text "a") :t)))
      (setf (vx (location text)) 5)
      (setf (text text) (compose-controller-debug-text controller ev)))))

(defmethod register-object-for-pass ((pass per-object-pass) (controller controller))
  (register-object-for-pass pass (text controller)))

(defmethod render ((controller controller) program)
  (when (show-overlay controller)
    (let ((fps-buffer (fps-buffer controller)))
      (when (= (array-total-size fps-buffer) (fill-pointer fps-buffer))
        (setf (fill-pointer fps-buffer) 0))
      ;; FIXME: Yeesh. Don't like these (handler *context*) accesses.
      (vector-push (if (= 0 (frame-time (handler *context*)))
                       1
                       (/ (frame-time (handler *context*))))
                   fps-buffer))
    (with-pushed-matrix ((*projection-matrix* :zero)
                         (*model-matrix* :identity)
                         (*view-matrix* :identity))
      (orthographic-projection 0 (width *context*)
                               0 (height *context*)
                               0 10)
      (translate-by 0 (height *context*) 0)
      (render (text controller) program))))

(defmethod handle ((ev quit-game) (controller controller))
  (quit *context*))

(defmethod handle ((ev event) (controller controller))
  (map-event ev *scene*)
  (retain-event ev))

(defmethod handle ((ev reload-scene) (controller controller))
  (let ((old (scene (display controller))))
    (change-scene (display controller) (make-instance (type-of old) :clock (clock old)))))

(defun find-controller ()
  (or (when *context* (unit :controller (scene (handler *context*))))
      (error "No reachable controller found.")))

(defmethod observe ((func function) &key title (controller (find-controller)))
  (let ((title (or title (format NIL "~d" (length (observers controller))))))
    (vector-push-extend (cons title func) (observers controller))
    func))

(defmethod observe (thing &rest args &key &allow-other-keys)
  (apply #'observe (compile NIL `(lambda (ev)
                                   (declare (ignorable ev))
                                   ,thing))
         args))

(defmethod stop-observing (&optional controller)
  (let ((observers (observers (or controller (find-controller)))))
    (setf (fill-pointer observers) 0)
    (loop for i from 0 below (array-total-size observers)
          do (setf (aref observers i) NIL))))

(defclass load-request (event)
  ((asset :initarg :asset)
   (action :initarg :action :initform 'reload)))

(define-handler (controller load-request) (asset action)
  (ecase action
    (deallocate (deallocate asset))
    (load (load asset))
    (reload (reload asset))))

(defun maybe-reload-scene (&optional (window (list-windows)))
  (dolist (window (enlist window))
    (issue (scene window) 'reload-scene)))
