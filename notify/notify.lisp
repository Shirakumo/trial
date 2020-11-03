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
   #:process-changes))
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
      (notify:watch file :events '(:modify)))))

(defmethod watch ((all (eql T)))
  (notify:init)
  (dolist (pool (trial:list-pools))
    (dolist (asset (trial:list-assets pool))
      (watch asset))))

(defmethod notify ((asset trial:asset) file)
  (trial:with-context ()
    (trial:reload asset)))

(defmethod notify ((applicable (eql T)) file)
  (let ((file (truename file)))
    (dolist (asset (gethash file *file-association-table*))
      (notify asset file))))

(defmethod files-to-watch append ((asset trial:asset))
  ())

(defmethod files-to-watch append ((asset trial:file-input-asset))
  (list (trial:input* asset)))

(defun process-changes ()
  (notify:with-events (file change-type)
    (notify T file)))
