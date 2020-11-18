#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.trial.notify
  (:use #:cl)
  (:local-nicknames
   (#:trial #:org.shirakumo.fraf.trial)
   (#:notify #:org.shirakumo.file-notify))
  (:export
   #:watch
   #:notify
   #:files-to-watch
   #:process-changes
   #:main))
(in-package #:org.shirakumo.fraf.trial.notify)

(defvar *file-association-table* (make-hash-table :test 'equal))

(defgeneric watch (asset))
(defgeneric notify (asset file))
(defgeneric files-to-watch (asset)
  (:method-combination append))

(defmethod watch ((asset trial:asset))
  ;; First unregister from all
  (loop for file being the hash-keys of *file-association-table*
        for assets being the hash-values of *file-association-table*
        do (let ((assets (remove asset assets)))
             (if assets
                 (setf (gethash file *file-association-table*) assets)
                 (remhash file *file-association-table*))))
  ;; Then register new
  (dolist (file (files-to-watch asset))
    (let ((file (truename file)))
      (pushnew asset (gethash file *file-association-table*))
      (handler-case
          (notify:watch file :events '(:modify))
        (notify:failure ()
          (v:error :trial.notify "Failed to add watch for ~a" file))))))

(defmethod watch ((all (eql T)))
  (notify:init)
  (dolist (pool (trial:list-pools))
    (dolist (asset (trial:list-assets pool))
      (watch asset))))

(defmethod notify ((asset trial:asset) file)
  (v:info :trial.notify "Noticed file change for ~a, issuing reload." file)
  (when trial:*context*
    (trial:issue (trial:scene (trial:handler trial:*context*)) 'trial:load-request :thing asset)))

(defmethod notify ((applicable (eql T)) file)
  (trial:with-retry-restart (retry "Retry the notification")
    (let ((file (probe-file file)))
      (when file
        (dolist (asset (gethash file *file-association-table*))
          (notify asset file))))))

(defmethod files-to-watch append ((asset trial:asset))
  ())

(defmethod files-to-watch append ((asset trial:file-input-asset))
  (list (trial:input* asset)))

(defun process-changes (&key timeout)
  (notify:with-events (file change-type :timeout timeout)
    (notify T file)))

(defclass main (trial:main)
  ((file-watch-thread :initform NIL :accessor file-watch-thread)))

(defmethod initialize-instance :after ((main main) &key (watch-files (not (deploy:deployed-p))))
  (when watch-files
    (watch T)
    (setf (file-watch-thread main) T)
    (setf (file-watch-thread main)
          (trial:with-thread ("Asset notification thread")
            (loop while (file-watch-thread main)
                  do (with-simple-restart (abort "Ignore the error.")
                       (process-changes :timeout 0.1)))))))

(defmethod trial:finalize :after ((main main))
  (trial:with-thread-exit ((file-watch-thread main))
    (setf (file-watch-thread main) NIL)))

#+darwin
(trial::dont-deploy
 org.shirakumo.file-notify::corefoundation
 org.shirakumo.file-notify::coreservices)
