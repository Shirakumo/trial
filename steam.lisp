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

(defclass main (trial:main)
  ())

(defmethod steam-required-p ((main main)) NIL)

(defmethod initialize-instance :after ((main main) &key app-id)
  (handler-bind ((error
                   (lambda (e)
                     (when (deploy:deployed-p)
                       (if (steam-required-p main)
                           (invoke-restart 'steam:restart)
                           (invoke-restart 'ignore))))))
    (when (or (steam-required-p main)
              (deploy:deployed-p))
      (with-simple-restart (ignore "Ignore the steamworks failure.")
        (make-instance 'steam:steamworks-client :app-id app-id)))))

(defmethod trial:finalize :after ((main main))
  (handler-case
      (steam:free (steam:steamworks))
    (steam:steamworks-not-initialized ())))

(deploy:define-hook (:build check-steamworks) ()
  (unless steam::*low-level-present*
    (error "CL-STEAMWORKS has not been set up properly!
Please check the CL-STEAMWORKS setup instructions.

Refusing to deploy as the game would not launch properly anyway.")))
