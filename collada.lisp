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
    (list sources
          (list (assoc "VERTEX" inputs :test #'string=)
                (assoc "TEXCOORD" inputs :test #'string=)
                (assoc "NORMAL" inputs :test #'string=)))))

(defun parse-collada (input)
  (let* ((plump:*tag-dispatchers* plump:*xml-tags*)
         (collada (plump:parse input)))
    (loop for mesh across (lquery:$ collada "mesh")
          collect (translate-mesh mesh))))

(defun check-compress-pair-compatibility (pairs)
  (loop with size = (length (first (first pairs)))
        for (index stride buffer) in pairs
        for i from 0
        do (when (/= size (length index))
             (error "Pair ~n's index buffer is ~s elements long, rather than the expected ~s."
                    i (length index) size))))

(defun compress-indexed (pairs)
  (check-compress-pair-compatibility pairs)
  (let* ((size (length (first (first pairs))))
         (data (make-array size :fill-pointer 0))
         (indices (make-array size :element-type '(unsigned-byte 32)
                                   :initial-element 0 :fill-pointer 0)))
    ;; Fill data and remove duplicates by indexing
    (loop for i from 0 below size
          for entry = (loop for (index stride buffer) in pairs
                            collect (loop for j from (* (elt index i) stride)
                                          repeat stride
                                          collect (elt buffer j)))
          do (let ((pos (position entry data :test #'equal)))
               (cond (pos
                      (vector-push-extend pos
                                          indices))
                     (T
                      (vector-push-extend (vector-push-extend entry data)
                                          indices)))))
    ;; Compress data into a single buffer
    (let ((buffer (make-array (loop for (index stride buffer) in pairs
                                    sum (* stride (length data)))
                              :element-type 'float :initial-element 0.0s0)))
      (loop with i = 0
            for entry across data
            do (loop for list in entry
                     do (loop for number in list
                              do (setf (aref buffer i) number)
                                 (incf i))))
      (list buffer indices))))

(defun compress-collada-data (sources inputs)
  (compress-indexed
   (loop for (name ref index) in inputs
         for source = (elt sources ref)
         collect (list index (second source) (third source)))))

(defun collada->vertex-format (input output &key (if-exists :error))
  (destructuring-bind (sources inputs) (first (parse-collada input))
    (destructuring-bind (buffer index) (compress-collada-data sources inputs)
      (let ((stride (* 4 (loop for input in inputs
                               sum (second (elt sources (second input)))))))
        (write-vformat output
                       (list (list index :element-array-buffer :static-draw :uint)
                             (list buffer :array-buffer :static-draw :float))
                       (list* (list 0 0 0 0 0 NIL)
                              (loop for input in inputs
                                    for index from 0
                                    for size = (second (elt sources (second input)))
                                    for offset = 0 then (+ offset (* size 4))
                                    collect (list 1 index size stride offset NIL)))
                       :if-exists if-exists)))))

(defun load-collada (input)
  (destructuring-bind (sources inputs) (first (parse-collada input))
    (destructuring-bind (buffer index) (compress-collada-data sources inputs)
      (let* ((index (make-asset 'vertex-buffer-asset index
                                :type :element-array-buffer :element-type :uint))
             (buffer (make-asset 'vertex-buffer-asset buffer))
             (stride (* 4 (loop for input in inputs
                                sum (second (elt sources (second input))))))
             (array (make-asset 'vertex-array-asset
                                (list* index
                                       (loop for input in inputs
                                             for index from 0
                                             for size = (second (elt sources (second input)))
                                             for offset = 0 then (+ offset (* size 4))
                                             collect (list buffer :index index :size size :stride stride :offset offset))))))
        (prog1 (load array)
          (offload index)
          (offload buffer))))))
