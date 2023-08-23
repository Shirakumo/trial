(in-package #:org.shirakumo.fraf.trial)

(defclass animation-asset (asset)
  ((meshes :initform (make-hash-table :test 'equal) :accessor meshes)
   (clips :initform (make-hash-table :test 'equal) :accessor clips)
   (skeleton :initform NIL :accessor skeleton)))

(defmethod generate-resources :around ((asset animation-asset) input &key)
  (let ((meshes (meshes asset))
        (clips (clips asset)))
    (clrhash meshes)
    (clrhash clips)
    (setf (skeleton asset) NIL)
    (call-next-method)
    (loop for mesh being the hash-values of meshes
          collect (resource asset (name mesh)))))

(defmethod find-clip (name (asset animation-asset) &optional (errorp T))
  (or (gethash name (clips asset))
      (when errorp (error "No clip with name~%  ~a~%on~%  ~a"
                          name asset))))

(defmethod (setf find-clip) ((clip clip) name (asset animation-asset))
  (setf (gethash name (clips asset)) clip))

(defmethod (setf find-clip) ((null null) name (asset animation-asset))
  (remhash name (clips asset))
  null)

(defmethod list-clips ((asset animation-asset))
  (alexandria:hash-table-keys (clips asset)))

(defmethod unload :after ((asset animation-asset))
  (clrhash (meshes asset))
  (clrhash (clips asset))
  (setf (skeleton asset) NIL))
