#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-action system-action ())

(define-action save-game (system-action)
  (key-press (eql key :f2)))

(define-action load-game (system-action)
  (key-press (eql key :f3)))

(define-action reload-scene (system-action)
  (key-press (eql key :f6)))

(define-action quit-game (system-action)
  (key-press (and (eql key :q) (find :control modifiers))))

(define-action toggle-overlay (system-action)
  (key-press (one-of key :section :grave)))

(define-asset (trial noto-sans) font
    #p"noto-sans-regular.ttf")

(define-asset (trial noto-mono) font
    #p"noto-mono-regular.ttf")

(define-subject controller ()
  ((display :initform NIL :accessor display)
   (text :initform (make-instance 'text :font (asset 'trial 'noto-mono) :size 18) :accessor text)
   (fps-buffer :initform (make-array 100 :fill-pointer T :initial-element 1))
   (show-overlay :initform NIL :accessor show-overlay))
  (:default-initargs
   :name :controller))

(defmethod compute-resources ((controller controller) cache)
  (compute-resources (text controller) cache))

(defmethod register-object-for-pass :after (pass (controller controller))
  (register-object-for-pass pass (text controller)))

(define-handler (controller toggle-overlay) (ev)
  (setf (show-overlay controller) (not (show-overlay controller))))

(define-handler (controller tick) (ev tt)
  (when (show-overlay controller)
    (multiple-value-bind (gfree gtotal) (gpu-room)
      (multiple-value-bind (cfree ctotal) (cpu-room)
        (with-slots (fps-buffer text) controller
          (when (= (array-total-size fps-buffer) (fill-pointer fps-buffer))
            (setf (fill-pointer fps-buffer) 0))
          ;; FIXME: Yeesh. Don't like these (handler *context*) accesses.
          (vector-push (if (= 0 (frame-time (handler *context*))) 1 (/ (frame-time (handler *context*)))) fps-buffer)
          
          (setf (vy (location text))
                (- -5 (getf (text-extent text "a") :t)))
          (setf (vx (location text)) 5)
          (setf (text text) (format NIL "TIME  [s]: ~8,2f~%~
                                         FPS  [Hz]: ~8,2f~%~
                                         RAM  [KB]: ~8d (~2d%)~%~
                                         VRAM [KB]: ~8d (~2d%)~%~
                                         RESOURCES: ~8d"
                                    (clock (scene (display controller)))
                                    (/ (loop for i from 0 below (array-total-size fps-buffer)
                                             sum (aref fps-buffer i))
                                       (array-total-size fps-buffer))
                                    (- ctotal cfree) (floor (/ (- ctotal cfree) ctotal 0.01))
                                    (- gtotal gfree) (floor (/ (- gtotal gfree) gtotal 0.01))
                                    (hash-table-count (resources *context*)))))))))

(defmethod paint ((controller controller) target)
  (when (show-overlay controller)
    (with-pushed-matrix ((*projection-matrix* :zero)
                         (*model-matrix* :identity)
                         (*view-matrix* :identity))
      (orthographic-projection 0 (width *context*)
                               0 (height *context*)
                               0 10)
      (translate-by 0 (height *context*) 0)
      (paint (text controller) target))))

(define-handler (controller quit-game) (ev)
  (quit *context*))

(define-handler (controller mapping T 100) (ev)
  (map-event ev *scene*)
  (retain-event ev))

(define-handler (controller reload-scene reload-scene 99) (ev)
  (let* ((display (display controller))
         (old (scene display)))
    (stop old)
    (restart-case
        (let ((new (make-instance (type-of old))))
          (setf (clock new) (clock old))
          (setup-scene display new)
          (transition old new)
          (setf (scene display) new))
      (abort ()
        :report "Give up reloading the scene and continue with the old."
        (start old)))))

(defclass load-request (event)
  ((asset :initarg :asset)
   (action :initarg :action :initform 'reload)))

(define-handler (controller load-request) (ev asset action)
  (ecase action
    (deallocate (deallocate asset))
    (load (load asset))
    (reload (reload asset))))

(defun maybe-reload-scene (&optional (window (list-windows)))
  (dolist (window (enlist window))
    (issue (scene window) 'reload-scene)))
