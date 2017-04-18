#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

;;; Everything here is really primitive.
;; TODO: - Properly respect reading types
;;       - Properly respect sizes and check
;;       - Pack sources into single buffer
;;       - Tessellate polygons if necessary

(lquery:define-lquery-list-function inode (vec)
  (when (< 0 (length vec))
    (parse-integer (elt vec 0))))

(defun translate-source (source)
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

(defun translate-input (input polygons sources mesh)
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

(defun translate-mesh (mesh)
  (let* ((polygons (loop for polygon across (lquery:$ mesh "polygons p")
                         collect (read-list (lquery:$ polygon (text) (node)))))
         (sources (loop for source across (lquery:$ mesh "source")
                        collect (translate-source source)))
         (inputs (loop for input across (lquery:$ mesh "polygons input")
                       collect (translate-input input polygons sources mesh))))
    (list sources inputs)))

(defun parse-collada (input)
  (let* ((plump:*tag-dispatchers* plump:*xml-tags*)
         (collada (plump:parse input)))
    (loop for mesh across (lquery:$ collada "mesh")
          collect (translate-mesh mesh))))

(defun collada->vertex-format (input output)
  (destructuring-bind (sources inputs) (first (parse-collada input))
    (write-vformat output
                   (append (loop for source in sources
                                 collect (list (third source) :array-buffer :static-draw :float))
                           (loop for input in inputs
                                 collect (list (third input) :element-array-buffer :static-draw :uint)))
                   (loop for input in inputs
                         for i from (length sources)
                         for index from 0
                         for stride = (second (elt sources (second input)))
                         collect (list (list (second input) i) index stride 0 0 NIL)))))
