(defpackage #:org.shirakumo.fraf.trial.notify
  (:use #:cl)
  (:local-nicknames
   (#:trial #:org.shirakumo.fraf.trial)
   (#:notify #:org.shirakumo.file-notify)
   (#:v #:org.shirakumo.verbose))
  (:export
   #:list-wathers
   #:watch
   #:unwatch
   #:notify
   #:files-to-watch
   #:process-changes
   #:main))
(in-package #:org.shirakumo.fraf.trial.notify)

;;; {truename -> {object ~> T}}
(defvar *file-association-table* (make-hash-table :test 'equal))

(defgeneric watch (object))
(defgeneric unwatch (object))
(defgeneric notify (object file))
(defgeneric files-to-watch (object)
  (:method-combination append))

(defun list-watchers (file)
  (loop for key being the hash-keys of (gethash (truename file) *file-association-table*)
        collect key))

(defmethod watch (object)
  (dolist (file (files-to-watch object))
    (handler-case
        (let* ((file (truename file))
               (table (gethash file *file-association-table*)))
          (unless table
            (notify:watch file :events '(:modify))
            (setf table (tg:make-weak-hash-table :weakness :key))
            (setf (gethash file *file-association-table*) table))
          (setf (gethash object table) T))
      (error ()
        (v:error :trial.notify "Failed to add watch for ~a" file)))))

(defmethod unwatch (object)
  (loop for file being the hash-keys of *file-association-table* using (hash-value objects)
        do (remhash object objects)
           (when (= 0 (hash-table-count objects))
             (remhash file *file-association-table*)
             (notify:unwatch file))))

(defmethod watch ((pool trial:pool))
  (dolist (asset (trial:list-assets pool))
    (watch asset)))

(defmethod unwatch ((pool trial:pool))
  (dolist (asset (trial:list-assets pool))
    (unwatch asset)))

(defmethod watch ((all (eql T)))
  (dolist (pool (trial:list-pools))
    (watch pool)))

(defmethod unwatch ((all (eql T)))
  (loop for file being the hash-keys of *file-association-table*
        do (remhash file *file-association-table*)
           (notify:unwatch file))
  (clrhash *file-association-table*))

(defmethod notify ((asset trial:asset) file)
  (when trial:+main+
    (trial:handle trial:+main+ (trial:make-event 'trial:load-request :thing asset))))

(defmethod notify ((prefab trial:prefab) file)
  (when trial:+main+
    (trial:with-eval-in-render-loop ()
      (trial:reload prefab))))

(defmethod notify ((applicable (eql T)) file)
  (trial:with-retry-restart (retry "Retry the notification")
    (let ((file (probe-file file)))
      (when file
        (v:info :trial.notify "Noticed file change for ~a, issuing reload." file)
        (loop for object being the hash-keys of (gethash file *file-association-table*)
              do (notify object file))))))

(defmethod files-to-watch append ((asset trial:asset))
  ())

(defmethod files-to-watch append ((asset trial:file-input-asset))
  (trial:enlist (trial:input* asset)))

(defmethod files-to-watch append ((prefab trial:prefab))
  (files-to-watch (trial:prefab-asset prefab)))

(defun process-changes (&key timeout)
  (notify:with-events (file change-type :timeout timeout)
    (notify T file)))

(defclass main (trial:main)
  ((file-watch-thread :initform NIL :accessor file-watch-thread)))

(defmethod initialize-instance :after ((main main) &key (watch-files (not (deploy:deployed-p))))
  (when watch-files
    (notify:init)
    (setf (file-watch-thread main) T)
    (setf (file-watch-thread main)
          (trial:with-thread ("asset-watcher")
            (loop while (file-watch-thread main)
                  do (with-simple-restart (abort "Ignore the error.")
                       (process-changes :timeout 0.5)))))))

(defmethod trial:finalize :after ((main main))
  (trial:with-thread-exit ((file-watch-thread main))
    (setf (file-watch-thread main) NIL))
  (unwatch T))

#+darwin
(trial::dont-deploy
 org.shirakumo.file-notify::corefoundation
 org.shirakumo.file-notify::coreservices)
