(in-package #:org.shirakumo.fraf.trial)

(define-condition unsupported-save-file (error)
  ((version :initarg :version :accessor version)))

(defun save-file-path (&optional (slot 0))
  (make-pathname :name (etypecase slot
                         ((eql :wild) pathname-utils:*wild-component*)
                         ((or string symbol) (string-downcase slot))
                         (integer (princ-to-string slot)))
                 :type "save"
                 :defaults (config-directory)))

(defclass save-file ()
  ((username :initarg :username :initform (username T) :accessor username)
   (slot :initarg :slot :initform 0 :accessor slot)
   (id :initarg :id :initform (make-uuid) :accessor id)
   (start-time :initarg :start-time :initform (get-universal-time) :accessor start-time)
   (save-time :initarg :save-time :initform (get-universal-time) :accessor save-time)
   (play-duration :initarg :play-duration :initform 0 :accessor play-duration)
   (image :initarg :image :initform NIL :accessor image)
   (file :initarg :file :accessor file)))

(defmethod initialize-instance :after ((save-file save-file) &key file)
  (unless (slot-boundp save-file 'file)
    (let ((path (save-file-path (slot save-file))))
      (setf (file save-file) (if file (merge-pathnames file path) path)))))

(defmethod print-object ((save-file save-file) stream)
  (print-unreadable-object (save-file stream :type T)
    (format stream "~d ~s" (slot save-file) (file save-file))))

(defmethod initargs append ((file save-file))
  '(:username :slot :id :start-time :save-time :play-duration :image :file))

(defun list-save-files ()
  (sort
   (loop for file in (directory (save-file-path :wild))
         for state = (handler-case (minimal-load-save file)
                       (unsupported-save-file ()
                         (v:warn :trial.save-data "Save state ~s is too old, ignoring." file)
                         NIL)
                       #+trial-release
                       (error (e)
                         (v:warn :trial.save-data "Save state ~s failed to load, ignoring." file)
                         (v:debug :trial.save-data e)
                         NIL))
         when state collect state)
   #'string< :key #'slot))

(defun delete-save-files ()
  (dolist (save (directory (save-file-path :wild)))
    (delete-file save)))

(defmethod save-version-type (version)
  'save-file)

(defmethod version ((file save-file))
  1)

(defun minimal-load-save (file)
  (depot:with-depot (depot file)
    (destructuring-bind (type version . initargs)
        (parse-sexps (depot:read-from (depot:entry "manifest" depot) 'character))
      (assert (eq 'save-file type))
      (apply #'make-instance (save-version-type version)
             :image (when (depot:entry-exists-p "preview.png" depot)
                      (setf (image file) (depot:read-from (depot:entry "preview.png" depot) 'byte)))
             initargs))))

(defgeneric load-save-file (save-file depot))
(defgeneric store-save-file (save-file depot &key))

(defmethod load-save-file (thing depot)
  (load-save-file (save-file-path thing) depot))

(defmethod load-save-file ((file save-file) depot)
  (load-save-file (file file) depot))

(defmethod load-save-file ((pathname pathname) depot)
  (load-save-file (make-instance 'save-file :file pathname) depot))

(defmethod load-save-file ((file save-file) (depot (eql T)))
  (depot:with-depot (depot (file file))
    (load-save-file file depot)))

(defmethod store-save-file (thing depot &rest args &key &allow-other-keys)
  (apply #'store-save-file (save-file-path thing) depot args))

(defmethod store-save-file ((pathname pathname) depot &rest args &key &allow-other-keys)
  (apply #'store-save-file (make-instance 'save-file :file pathname) depot args))

(defmethod save-file-manifest ((file save-file))
  (with-output-to-string (out)
    (with-trial-io-syntax ()
      (format out "~s ~s~%" 'save-file (version file))
      (loop for initarg in (initargs file)
            do (format out "~s ~s~%" initarg (initarg-slot-value file initarg))))))

(defmethod store-save-file ((file save-file) (depot (eql T) &rest args &key &allow-other-keys))
  (depot:with-depot (depot (file file) :commit T)
    (apply #'store-save-file file depot args)))

(defmethod store-save-file :before ((file save-file) (depot depot:depot) &key (preview T) (preview-args '(:width 192 :height 108)) &allow-other-keys)
  (depot:write-to (depot:ensure-entry "manifest" depot) (save-file-manifest file))
  (with-ignored-errors-on-release (:trial.save-data "Failed to save preview image for save file.")
    (labels ((copy-preview (source)
               (depot:with-open (tx (depot:ensure-entry "preview.png" depot) :output '(unsigned-byte 8))
                 (with-open-file (in source :direction :input :element-type '(unsigned-byte 8))
                   (uiop:copy-stream-to-stream in (depot:to-stream tx) :element-type '(unsigned-byte 8)))))
             (save-preview (renderable)
               (with-tempfile (temp :type "png" :id (id file))
                 (apply #'save-image renderable temp :png preview-args)
                 (copy-preview temp))))
      (etypecase preview
        (null)
        ((eql T)
         (render (scene +main+) NIL)
         (save-preview (scene +main+)))
        ((or framebuffer shader-pass memory-region vector texture texture-source)
         (save-preview preview))
        (pathname
         (copy-preview preview))))))

;;; Users must provide methods for:
;;;   (load-save-file save-file depot:depot)
;;;   (store-save-file save-file depot:depot)
;;; to handle the encoding/decoding
