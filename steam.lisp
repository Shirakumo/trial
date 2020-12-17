#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.trial.steam
  (:use #:cl)
  (:export #:main #:steam-required-p)
  (:local-nicknames
   (#:trial #:org.shirakumo.fraf.trial)
   (#:steam #:org.shirakumo.fraf.steamworks)))
(in-package #:org.shirakumo.fraf.trial.steam)

(defun action-label (action)
  (let ((action (etypecase action
                  (symbol action)
                  (class (class-name action))
                  (trial:action (class-name (class-of action))))))
    (format NIL "~a_~a" (package-name (symbol-package action)) (symbol-name action))))

(defclass main (trial:main)
  ((analog-actions :initform #() :accessor analog-actions)
   (digital-actions :initform #() :accessor digital-actions)
   (use-steaminput :initform T :initarg :use-steaminput :accessor use-steaminput)))

(defmethod steam-required-p ((main main)) NIL)

(defmethod initialize-instance :after ((main main) &key app-id)
  (handler-bind ((error
                   (lambda (e)
                     (v:severe :trial.steam "Failed to initialise steamworks: ~a" e)
                     (v:debug :trial.steam e)
                     (when (deploy:deployed-p)
                       (cond ((steam-required-p main)
                              (invoke-restart 'steam:restart))
                             (T
                              (setf (use-steaminput main) NIL)
                              (invoke-restart 'ignore)))))))
    (when (or (steam-required-p main)
              (deploy:deployed-p))
      (with-simple-restart (ignore "Ignore the steamworks failure.")
        (v:info :trial.steam "Initialising steamworks")
        (make-instance 'steam:steamworks-client :app-id app-id)
        ;; Populate action sets
        (when (use-steaminput main)
          (let ((input (steam:interface 'steam:steaminput T))
                (analog ())
                (digital ()))
            (dolist (class (trial:list-leaf-classes (find-class 'trial:action)))
              (cond ((or (c2mop:subclassp class (find-class 'trial:analog-action))
                         (c2mop:subclassp class (find-class 'trial:directional-action)))
                     (let ((action (steam:find-analog-action input (action-label class))))
                       (when action (push (cons action class) analog))))
                    (T
                     (let ((action (steam:find-digital-action input (action-label class))))
                       (when action (push (cons action class) digital))))))
            (setf (analog-actions main) (coerce analog 'vector))
            (setf (digital-actions main) (coerce digital 'vector))))))))

(defmethod trial:finalize :after ((main main))
  (handler-case
      (steam:free (steam:steamworks))
    (steam:steamworks-not-initialized ())))

(defmethod trial:poll-input :after ((main main))
  (when (use-steaminput main)
    (let ((input (steam:interface 'steam:steaminput T)))
      (steam:run-frame input)
      (macrolet ((fire (type &rest args)
                   `(trial:handle (make-instance ,type :device controller ,@args) main)))
        (steam:do-controllers (controller input)
          (loop for (action . class) across (analog-actions main)
                do (let ((px (getf (steam:previous-action-data action) :x))
                         (py (getf (steam:previous-action-data action) :y)))
                     (destructuring-bind (&key mode x y active) (steam:action-data action controller)
                       (declare (ignore mode))
                       (when active
                         (if (c2mop:subclassp class (find-class 'trial:directional-action))
                             (when (or (/= x px) (/= y py)) (fire class :x x :y y))
                             (when (/= x px) (fire class :value x)))))))
          (loop for (action . class) across (digital-actions main)
                do (let ((previous (getf (steam:previous-action-data action) :state)))
                     (destructuring-bind (&key state active) (steam:action-data action controller)
                       (when (and active (not (eql previous state)))
                         (fire class))))))))))

(deploy:define-hook (:build check-steamworks) ()
  (unless steam::*low-level-present*
    (error "CL-STEAMWORKS has not been set up properly!
Please check the CL-STEAMWORKS setup instructions.

Refusing to deploy as the game would not launch properly anyway.")))
