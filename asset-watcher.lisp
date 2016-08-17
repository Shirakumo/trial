#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(define-object watching-pool (QFileSystemWatcher pool)
  ())

(defmethod print-object ((pool watching-pool) stream)
  (print-unreadable-object (pool stream :type T)
    (format stream "~s ~s" (name pool) (base pool))))

(defmethod initialize-instance :after ((pool watching-pool) &key)
  (pool-watch-path pool pool))

(defmethod enter :after ((asset file-asset) (pool watching-pool))
  ;; We only need to note files that aren't within our pool base, as
  ;; everything else is caught within our directory change note.
  (unless (pathname-utils:subpath-p (file asset) (base pool))
    (pool-watch-path pool asset)))

(defmethod leave :after ((asset file-asset) (pool watching-pool))
  ;; FIXME: what if two assets have the same file path?
  (unless (pathname-utils:subpath-p (file asset) (base pool))
    (pool-unwatch-path pool asset)))

(defun pool-watch-path (pool path)
  (etypecase path
    (string
     (unless (find path (q+:files pool) :test #'string=)
       (v:info :trial.asset "~a is now watching ~a" pool path)
       (q+:add-path pool path)))
    (pathname
     (pool-watch-path pool (uiop:native-namestring path))
     (when (pathname-utils:directory-p path)
       (dolist (file (uiop:directory-files path))
         (pool-watch-path pool file))))
    (pool (pool-watch-path pool (base path)))
    (file-asset (pool-watch-path pool (file path)))))

(defun pool-unwatch-path (pool path)
  (etypecase path
    (string
     (when (find path (q+:files pool) :test #'string=)
       (v:info :trial.asset "~a is no longer watching ~a" pool path)
       (q+:remove-path pool path)))
    (pathname (pool-unwatch-path pool (uiop:native-namestring path)))
    (pool (pool-unwatch-path pool (base path)))
    (file-asset (pool-unwatch-path pool (file path)))))

(defun pool-notice-path-updated (pool path)
  (v:info :trial.asset "Noticed file update to ~a" path)
  (dolist (asset (assets pool))
    (when (and (typep asset 'file-asset)
               (pathname-utils:pathname= path (file asset)))
      (reinitialize-instance asset))))

(defun pool-notice-path-removed (pool path)
  (v:info :trial.asset "Noticed file rename or deletion of ~a" path)
  (dolist (asset (assets pool))
    (when (and (typep asset 'file-asset)
               (string= (uiop:native-namestring path)
                        (uiop:native-namestring (file asset))))
      (v:warn :trial.asset "~a may not be able to ~:[restore~;reload~] anymore!"
              asset (loaded-p asset)))))

;; FIXME: The API does not give us access to knowing whether a file has truly been
;;        deleted or merely renamed, and in the case of a rename what the new file
;;        name is going to be. We could remedy this with the directory watching, but
;;        that is inherently incredibly susceptible to race conditions and false-
;;        detections.
(define-slot (watching-pool notice-file-change) ((path string))
  (declare (connected watching-pool (file-changed string)))
  ;; This is inherently race-y, but nothing much we can do. :/
  (let ((path (uiop:parse-native-namestring path)))
    (cond ((uiop:file-exists-p path)
           (pool-notice-path-updated watching-pool path))
          (T
           (pool-notice-path-removed watching-pool path)))))

(define-slot (watching-pool notice-directory-change) ((path string))
  (declare (connected watching-pool (directory-changed string)))
  ;; This is inherently race-y, but nothing much we can do. :/
  (let ((path (uiop:parse-native-namestring path)))
    (dolist (file (uiop:directory-files path))
      (pool-watch-path watching-pool file))))

#-trial-optimize-pool-watching
(defmacro define-pool (name &body options)
  `(name (make-instance 'watching-pool :name ',name ,@options)))
