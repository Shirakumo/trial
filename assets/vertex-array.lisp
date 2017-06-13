#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass vertex-array (asset)
  ((size :initarg :size :initform NIL :accessor size)))

(defmethod coerce-input ((asset vertex-array) (buffer vertex-buffer))
  (list buffer))

(defmethod coerce-input ((asset vertex-array) (spec list))
  spec)

(defmethod finalize-resource ((type (eql 'vertex-array)) resource)
  (gl:delete-vertex-arrays (list resource)))

(defmethod load progn ((asset vertex-array))
  (let ((array (gl:gen-vertex-array)))
    (setf (resource asset) array)
    (with-cleanup-on-failure (offload asset)
      (gl:bind-vertex-array array)
      (unwind-protect
           (loop for input in (coerced-inputs asset)
                 for i from 0
                 do (destructuring-bind (buffer &key (index i)
                                                     (size 3)
                                                     (stride 0)
                                                     (offset 0)
                                                     (normalized NIL))
                        (enlist input)
                      (load buffer)
                      (gl:bind-buffer (buffer-type buffer) (resource buffer))
                      (ecase (buffer-type buffer)
                        (:element-array-buffer
                         (unless (size asset)
                           (setf (size asset) (size buffer)))
                         (decf i))
                        (:array-buffer
                         (gl:vertex-attrib-pointer index size (element-type buffer) normalized stride offset)
                         (gl:enable-vertex-attrib-array index)))))
        (gl:bind-vertex-array 0)))))

(defclass packed-vertex-array (asset)
  ((size :initform NIL :accessor size)))

(defmethod finalize-resource ((type (eql 'packed-vertex-array)) resource)
  (finalize-resource 'vertex-array resource))

(defmethod load progn ((asset packed-vertex-array))
  (let ((buffer (make-array 0 :adjustable T :fill-pointer 0))
        (groups)
        (inputs ())
        (offset 0)
        (element (first (inputs asset)))
        (specs (rest (inputs asset))))
    (loop for index from 0
          for (size input) on specs by #'cddr
          do ;; Ensure that each array matches with all others in groups.
          (cond ((/= 0 (mod (length input) size))
                 (error "The input array~%  ~s~% with size ~a cannot be divided into even groups."
                        input size))
                ((not groups)
                 (setf groups (/ (length input) size)))
                ((/= (/ (length input) size) groups)
                 (error "The input array~%  ~s~% with size ~a does not match the number of groups ~a."
                        input size groups)))
          (push (list :index index :size size :offset offset) inputs)
          (incf offset (* size 4)))
    ;; Fill the buffer
    (dotimes (group groups)
      (loop for (size input) on specs by #'cddr
            do (loop for i from (* size group) below (* size (1+ group))
                     do (vector-push-extend (elt input i) buffer))))
    ;; Construct actual assets.
    (setf (size asset) (length element))
    (let* ((buffer (make-asset 'vertex-buffer (list buffer)
                               :element-type :float))
           (element (make-asset 'vertex-buffer (list element)
                                :type :element-array-buffer
                                :element-type :uint))
           (vao (make-asset 'vertex-array
                            (list* element
                                   (loop for spec in (nreverse inputs)
                                         collect (list* buffer :stride offset spec))))))
      (load vao)
      (setf (resource asset) (resource vao))
      (offload buffer)
      (offload element))))
