#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

;;; Everything here is really primitive.
;; TODO: - Properly respect reading types
;;       - Properly respect sizes and check

(defun translate-collada-source (source)
  (let* ((accessor (lquery:$ source "accessor" (node)))
         (stride (parse-integer (plump:attribute accessor "stride")))
         (array (lquery:$ source (inline (plump:attribute accessor "source")) (node)))
         (data (make-array (parse-integer (plump:attribute array "count"))
                           :element-type 'single-float)))
    (with-input-from-string (stream (plump:text array))
      (loop for i from 0 below (length data)
            do (setf (aref data i) (read stream))))
    (list (plump:attribute source "id")
          stride
          data)))

(defun array->vec (data stride &optional (offset 0))
  (ecase stride
    (2 (vec2 (aref data (+ 0 offset))
             (aref data (+ 1 offset))))
    (3 (vec3 (aref data (+ 0 offset))
             (aref data (+ 1 offset))
             (aref data (+ 2 offset))))
    (4 (vec4 (aref data (+ 0 offset))
             (aref data (+ 1 offset))
             (aref data (+ 2 offset))
             (aref data (+ 3 offset))))))

(defun collada-semantic->vertex-keyword (string)
  (intern string :keyword))

(defun read-collada-mesh (data)
  ;; FIXME: determine polygon vertex count.
  (let* ((mesh (make-instance 'vertex-mesh :vertex-type 'basic-vertex))
         (sources (loop for source across (lquery:$ data "source")
                        collect (translate-collada-source source)))
         (inputs (loop for input across (lquery:$ data "polygons input")
                       collect (list (parse-integer (plump:attribute input "offset"))
                                     (plump:attribute input "semantic")
                                     (plump:attribute input "source")))))
    (setf inputs (sort inputs #'< :key #'first))
    (loop for polygon across (lquery:$ data "polygons p")
          do (with-input-from-string (stream (plump:text polygon))
               (loop for i from 0 below 3
                     for initargs = (loop for (_ semantic source-id) in inputs
                                          for (stride data) = (cdr (assoc source-id sources :test #'string=))
                                          for offset = (* stride (read stream))
                                          collect (collada-semantic->vertex-keyword semantic)
                                          collect (array->vec data stride offset))
                     do (apply #'add-vertex mesh initargs))))
    mesh))

(defmethod read-geometry (file (format (eql :dae)) &key &allow-other-keys)
  (let* ((plump:*tag-dispatchers* plump:*xml-tags*)
         (collada (plump:parse file))
         (geometry (make-instance 'geometry)))
    (loop for mesh across (lquery:$ collada "mesh")
          do (setf (gethash :foo (meshes geometry))
                   (read-collada-mesh mesh)))
    geometry))
