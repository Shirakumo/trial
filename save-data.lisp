(in-package #:org.shirakumo.fraf.trial)

(define-condition unsupported-save-version (error)
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
   (slot :initarg :slot :initform (error "SLOT required.") :accessor slot)
   (id :initarg :id :initform (make-uuid) :accessor id)
   (start-time :initarg :start-time :initform (get-universal-time) :accessor start-time)
   (save-time :initarg :save-time :initform (get-universal-time) :accessor save-time)
   (play-duration :initarg :play-duration :initform 0 :accessor play-duration)
   (image :initarg :image :initform NIL :accessor image)
   (file :initarg :file :accessor file)))

(defmethod initialize-instance :after ((save-file save-file) &key file)
  (unless (slot-boundp save-file 'file)
    (let ((path (save-file-path (slot save-file))))
      (setf (file save-file) (if file (pathname-utils:merge-pathnames* file path) path)))))

(defmethod print-object ((save-file save-file) stream)
  (print-unreadable-object (save-file stream :type T)
    (format stream "~d ~s" (slot save-file) (file save-file))))

(defmethod initargs append ((file save-file))
  '(:username :slot :id :start-time :save-time :play-duration :image :file))

(defun list-save-files ()
  (sort
   (loop for file in (directory (save-file-path :wild))
         for state = (handler-case (minimal-load-save file)
                       (unsupported-save-version ()
                         (v:warn :trial.save-data "Save state ~s is too old, ignoring." file)
                         NIL)
                       #+trial-release
                       (error (e)
                         (v:warn :trial.save-data "Save state ~s failed to load, ignoring." file)
                         (v:debug :trial.save-data e)
                         NIL))
         when state collect state)
   #'string< :key (lambda (save) (princ-to-string (slot save)))))

(defun delete-save-files ()
  (dolist (save (directory (save-file-path :wild)))
    (delete-file save)))

(defmethod save-version-type (version)
  (error 'unsupported-save-version :version version))

(defmethod version ((file save-file))
  (error "You must implement a VERSION method for ~s" (type-of file)))

(defun minimal-load-save (file)
  (depot:with-depot (depot file)
    (destructuring-bind (type version . initargs)
        (parse-sexps (depot:read-from (depot:entry "manifest" depot) 'character))
      (assert (eq 'save-file type))
      (apply #'make-instance (save-version-type version)
             :image (when (depot:entry-exists-p "preview.png" depot)
                      (load-image (depot:read-from (depot:entry "preview.png" depot) 'byte) :png))
             initargs))))

(defgeneric load-save-data (save-file depot))
(defgeneric store-save-data (save-file depot &key))

(defmethod load-save-data (thing depot)
  (load-save-data (save-file-path thing) depot))

(defmethod load-save-data ((file save-file) depot)
  (load-save-data (file file) depot))

(defmethod load-save-data ((file pathname) depot)
  (load-save-data (minimal-load-save file) depot))

(defmethod load-save-data ((file save-file) (depot (eql T)))
  (depot:with-depot (depot (file file))
    (load-save-data file depot)))

(defmethod load-save-data :around ((file save-file) depot)
  (call-next-method)
  file)

(defmethod store-save-data (thing depot &rest args &key &allow-other-keys)
  (apply #'store-save-data (make-instance (save-version-type T) :slot thing) depot args))

(defmethod store-save-data :around ((file save-file) depot &key &allow-other-keys)
  (call-next-method)
  file)

(defmethod save-file-manifest ((file save-file))
  (with-trial-io-syntax ()
    (with-output-to-string (out)
      (format out "~s ~s~%" 'save-file (version file))
      (loop for initarg in (initargs file)
            do (format out "~s ~s~%" initarg (initarg-slot-value file initarg))))))

(defmethod store-save-data ((file save-file) (depot (eql T)) &rest args &key &allow-other-keys)
  (depot:with-depot (depot (org.shirakumo.depot.zip:from-pathname (file file)) :commit T)
    (apply #'store-save-data file depot args)))

(defmethod store-save-data :before ((file save-file) (depot depot:depot) &key (preview (image file)) (preview-args '(:width 192 :height 108)) &allow-other-keys)
  (setf (save-time file) (get-universal-time))
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
;;;   (load-save-data save-file depot:depot)
;;;   (store-save-data save-file depot:depot)
;;;   (save-version-type T)
;;;   (save-version-type version)
;;;   (version save-file)
;;; to handle the encoding/decoding

(defmacro define-simple-save-file (version state &body methods)
  (destructuring-bind (version &optional (class (mksym *package* 'save-file- version))) (enlist version)
    (let ((decode (cdr (assoc :decode methods)))
          (encode (cdr (assoc :encode methods)))
          (data (gensym "DATA")))
      `(progn
         (defclass ,class (save-file)
           ())
         
         (defmethod version ((file ,class)) ',version)
         
         (defmethod save-version-type ((name (eql ',version)))
           ,(ecase state
              ((:latest :current)
               `',class)
              ((:upgradable :supported) 
               `(progn (cerror "Upgrade the version" 'unsupported-save-version :version ',version)
                                   ',class))
              (:incompatible
               `(error 'unsupported-save-version :version ',version))))
         
         ,@(when (find state '(:latest :current))
             `((defmethod save-version-type ((default (eql T))) ',class)))

         ,(when decode
            (destructuring-bind ((depot . args) &rest body) decode
              `(defmethod load-save-data ((file ,class) (,depot depot:depot))
                 (let ((,data (with-trial-io-syntax ()
                                (read-from-string (depot:read-from (depot:entry "data" ,depot) 'character)))))
                   (destructuring-bind ,args ,data
                     ,@body)))))

         ,(when encode
            (destructuring-bind ((depot) &rest body) encode
              `(defmethod store-save-data ((file ,class) (,depot depot:depot) &key)
                 (let ((,data (progn ,@body)))
                   (depot:write-to (depot:ensure-entry "data" ,depot)
                                   (with-trial-io-syntax ()
                                     (prin1-to-string ,data)))))))))))

(trivial-indent:define-indentation define-simple-save-file
  (4 6 &rest (&whole 2 6 &body)))
