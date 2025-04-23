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

(pushnew :trial-notify *features*)

(in-package #:org.shirakumo.fraf.trial.notify)

;;; {truename -> {object ~> T}}
(defvar *file-association-table* (make-hash-table :test 'equal))
(defvar *pending-files* (make-hash-table :test 'equal))

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
    (let ((table (gethash file *file-association-table*)))
      (unless table
        (setf table (tg:make-weak-hash-table :weakness :key))
        (setf (gethash file *file-association-table*) table))
      (setf (gethash object table) T)
      (handler-case (notify:watch file :events '(:modify :delete :move))
        (error () (setf (gethash file *pending-files*) T))))))

(defmethod unwatch (object)
  (loop for file being the hash-keys of *file-association-table* using (hash-value objects)
        do (remhash object objects)
           (when (= 0 (hash-table-count objects))
             (unwatch file))))

(defmethod unwatch ((file pathname))
  (remhash file *file-association-table*)
  (remhash file *pending-files*)
  (ignore-errors (notify:unwatch file)))

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
           (ignore-errors (notify:unwatch file)))
  (clrhash *file-association-table*))

(defmethod notify ((asset trial:asset) file)
  (when trial:+main+
    (trial:handle (trial:make-event 'trial:asset-changed :changed-asset asset) trial:+main+)))

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


(let ((queue (make-hash-table :test 'equal)))
  (defun process-changes (&key timeout)
    (let ((found NIL))
      (notify:with-events (file change-type :timeout timeout)
        (setf found T)
        (case change-type
          (:modify
           (setf (gethash file queue) file))
          ((:delete :move)
           (ignore-errors (notify:unwatch file))
           (setf (gethash file *pending-files*) T)
           (v:info :trial.notify "File disappeared: ~a" file))))
      (loop for file being the hash-keys of *pending-files*
            do (ignore-errors
                (when (probe-file file)
                  (notify:watch file :events '(:modify :delete :move))
                  (remhash file *pending-files*)
                  (v:info :trial.notify "File appeared: ~a" file)
                  (setf (gethash file queue) file))))
      (unless found ;; Now process the (hopefully) deduplicated queue
        (loop for file being the hash-keys of queue
              do (notify T file))
        (clrhash queue)))))

(defclass main (trial:main)
  ((file-watch-thread :initform NIL :accessor file-watch-thread)))

(defmethod initialize-instance :after ((main main) &key (watch-files (not (deploy:deployed-p))))
  (when watch-files
    (notify:init)
    (setf (file-watch-thread main) T)
    (setf (file-watch-thread main)
          (trial:with-thread ("asset-watcher")
            (v:info :trial.notify "Watching for file changes...")
            (loop while (file-watch-thread main)
                  do (with-simple-restart (abort "Ignore the error.")
                       (process-changes :timeout 0.5)))))))

(defmethod trial:finalize :after ((main main))
  (when (file-watch-thread main)
   (trial:with-thread-exit ((file-watch-thread main))
     (setf (file-watch-thread main) NIL))
   (unwatch T)
   (notify:shutdown)))

