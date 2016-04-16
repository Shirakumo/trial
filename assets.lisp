#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

;; FIXME: How do we access extra information that is not the straight-up content?
;;        stuff like dimensions of a texture or mesh material or whatever?

(defvar *assets* (make-hash-table :test 'equal))
(defvar *root* (asdf:system-source-directory :trial))

(defun resource-pathname (pathname)
  (let ((pathname (pathname pathname)))
    (pathname-utils:normalize-pathname
     (merge-pathnames
      (if (pathname-utils:absolute-p pathname) pathname (merge-pathnames "data/" pathname))
      *root*))))

(defun clear-assets ()
  (loop for k being the hash-keys of *assets*
        do (remove-asset k)))

(defclass asset ()
  ((state :initarg :state :accessor state)
   (data :initarg :data :accessor data))
  (:default-initargs
   :state :offloaded))

(defmethod print-object ((asset asset) stream)
  (print-unreadable-object (asset stream :type T)
    (format stream "~s" (state asset))))

(defmethod asset ((asset asset) type)
  asset)

(defmethod asset :around (id type)
  (or (gethash id *assets*)
      (setf (gethash id *assets*)
            (call-next-method))))

(defmethod remove-asset (id)
  (let ((asset (gethash id *assets*)))
    (when asset
      (offload asset)
      (remhash id *assets*))))

(defmethod content :before ((asset asset) &optional offset)
  (declare (ignore offset))
  (restore asset))

(defmethod content ((asset asset) &optional offset)
  (etypecase offset
    (null (data asset))
    (integer (elt (data asset) offset))))

(defmethod restore ((asset asset))
  (error "Don't know how to restore ~a" asset))

(defmethod restore :around ((asset asset))
  (unless (eql (state asset) :restored)
    (call-next-method))
  asset)

(defmethod restore :before ((asset asset))
  (v:info :trial.assets "Restoring ~a" asset))

(defmethod restore :after ((asset asset))
  (setf (state asset) :restored))

(defmethod offload ((asset asset))
  (finalize asset))

(defmethod offload :around ((asset asset))
  (unless (eql (state asset) :offloaded)
    (call-next-method))
  asset)

(defmethod offload :before ((asset asset))
  (v:info :trial.assets "Offloading ~a" asset))

(defmethod offload :after ((asset asset))
  (setf (state asset) :offloaded))

(defmethod finalize ((asset asset))
  (finalize (data asset))
  (setf (data asset) NIL))

(defclass named-asset (asset)
  ((name :initarg :name :accessor name))
  (:default-initargs
   :name (error "NAME required.")))

(defmethod print-object ((asset named-asset) stream)
  (print-unreadable-object (asset stream :type T)
    (format stream "~s ~s" (state asset) (name asset))))

(defmethod asset ((name string) (type symbol))
  (make-instance type :name name))

(defclass font (named-asset)
  ((size :initarg :size :accessor size))
  (:default-initargs
   :size 12))

(defmethod restore ((asset font))
  (setf (data asset) (q+:make-qfont (name asset) (size asset))))

(defclass file-asset (asset)
  ((file :initarg :file :accessor file)
   (allowed-types :initarg :allowed-types :accessor allowed-types))
  (:default-initargs
   :file (error "FILE required.")))

(defmethod initialize-instance :before ((asset file-asset) &key file allowed-types)
  (unless (or (eql allowed-types T)
              (find (pathname-type file) allowed-types :test #'string-equal))
    (error "~a does not know how to handle a file of type ~a."
           asset (pathname-type file))))

(defmethod print-object ((asset file-asset) stream)
  (if *print-readably*
      (print (make-load-form asset) stream)
      (print-unreadable-object (asset stream :type T)
        (format stream "~s ~s" (state asset) (file asset)))))

(defmethod asset :around ((pathname pathname) type)
  (call-next-method (resource-pathname pathname) type))

(defmethod asset ((pathname pathname) type)
  (make-instance type :file pathname))

(defmethod asset :around ((string string) (type symbol))
  (if (subtypep type 'file-asset)
      (asset (uiop:parse-native-namestring string) type)
      (call-next-method)))

(defmethod remove-asset ((pathname pathname))
  (call-next-method (resource-pathname pathname)))

(defmethod restore :before ((asset file-asset))
  (unless (probe-file (file asset))
    (error "Invalid file path ~s." (file asset))))

(defmethod make-load-form ((asset file-asset) &optional env)
  (declare (ignore env))
  `(asset ,(file asset) ',(class-name (class-of asset))))

(defclass image (file-asset)
  ()
  (:default-initargs
   :allowed-types '(bmp gif jpg jpeg png pbm pgm ppm tiff xbm xpm)))

(defmethod restore ((asset image))
  (let ((image (q+:make-qimage (uiop:native-namestring (file asset)))))
    (when (q+:is-null image)
      (error "Invalid file ~s." (file asset)))
    (setf (data asset) image)))

(defclass texture (image)
  ())

(defmethod restore ((asset texture))
  (call-next-method)
  (let* ((image (data asset))
         (buffer (make-instance 'framebuffer :width (q+:width image)
                                             :height (q+:height image)
                                             :mipmap T)))
    (gl:disable :cull-face)
    (with-painter (painter buffer)
      (q+:draw-image painter 0 0 image))
    (setf (data asset) buffer)
    (finalize image)))

(defmethod content ((asset texture) &optional offset)
  (declare (ignore offset))
  (texture (data asset)))

(defclass sound (file-asset)
  ()
  (:default-initargs
   :allowed-types '(wav ogg mp3)))

(defclass model (file-asset)
  ()
  (:default-initargs
   :allowed-types '(obj)))

(defmethod restore ((asset model))
  (setf (data asset) (wavefront-loader:load-obj (file asset)))
  (loop for obj across (data asset)
        for diffuse = (wavefront-loader:diffuse (wavefront-loader:material obj))
        do (when (typep diffuse 'pathname)
             (let ((texture (asset diffuse 'texture)))
               (setf (wavefront-loader:diffuse (wavefront-loader:material obj))
                     (content texture))))))
