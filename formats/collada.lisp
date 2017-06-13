#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

;;; Everything here is really primitive.
;; TODO: - Properly respect reading types
;;       - Properly respect sizes and check
;;       - Pack sources into single buffer
;;       - Tessellate polygons if necessary

(lquery:define-lquery-list-function inode (vec)
  (when (< 0 (length vec))
    (parse-integer (elt vec 0))))

(defun translate-collada-source (source)
  (let* ((accessor (lquery:$ source "accessor" (node)))
         (stride (lquery:$ accessor (attr "stride") (inode)))
         (array (lquery:$ source (inline (lquery:$ accessor (attr "source") (node))) (node)))
         (data (make-array (lquery:$ array (attr "count") (inode)))))
    (with-input-from-string (stream (lquery:$ array (text) (node)))
      (loop for i from 0 below (length data)
            do (setf (aref data i) (read stream))))
    (list (lquery:$ source (attr "id") (node))
          stride
          data)))

(defun read-list (string)
  (with-input-from-string (stream string)
    (loop for el = (read stream NIL :nothing)
          until (eql el :nothing)
          collect el)))

(defun translate-collada-input (input polygons sources mesh)
  (let ((offset (lquery:$ input (attr "offset") (inode)))
        (data (make-array (* (length polygons) 3))) ; triangles only for now.
        (semantic (lquery:$ input (attr "semantic") (node)))
        (source (lquery:$ input (attr "source") (node))))
    (loop with i = 0
          for polygon in polygons
          do (loop for j from 0 below 3
                   do (setf (aref data i) (elt polygon (+ offset (* j 3))))
                      (incf i)))
    (list semantic
          (position (subseq (if (string= semantic "VERTEX")
                                (lquery:$ mesh source "input" (attr "source") (node))
                                source)
                            1)
                    sources :key #'first :test #'string=)
          data)))

(defun read-collada-mesh (data)
  (let* ((polygons (loop for polygon across (lquery:$ data "polygons p")
                         collect (read-list (lquery:$ polygon (text) (node)))))
         (sources (loop for source across (lquery:$ data "source")
                        collect (translate-collada-source source)))
         (inputs (loop for input across (lquery:$ data "polygons input")
                       collect (translate-collada-input input polygons sources mesh))))
    (list sources
          (list (assoc "VERTEX" inputs :test #'string=)
                (assoc "TEXCOORD" inputs :test #'string=)
                (assoc "NORMAL" inputs :test #'string=)))))

(defmethod read-geometry (file (format (eql :dae)) &key &allow-other-keys)
  (let* ((plump:*tag-dispatchers* plump:*xml-tags*)
         (collada (plump:parse input))
         (geometry (make-instance 'geometry)))
    (loop for mesh across (lquery:$ collada "mesh")
          do (setf (gethash :foo (meshes geometry))
                   (read-collada-mesh mesh)))
    geometry))
