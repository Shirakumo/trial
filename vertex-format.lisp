#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

;; FIXME: more error/integrity checks

(defun vformat-write-vector (stream array type)
  (when (<= (expt 2 32) (length array))
    (error "Array is longer than 2³² elements."))
  (fast-io:write32-be (length array) stream)
  (ecase type
    (:char
     (fast-io:writeu8 0 stream)
     (loop for value across array
           do (fast-io:write8 value stream)))
    (:int
     (fast-io:writeu8 1 stream)
     (loop for value across array
           do (fast-io:write32-be value stream)))
    (:uint
     (fast-io:writeu8 2 stream)
     (loop for value across array
           do (fast-io:writeu32-be value stream)))
    (:float
     (fast-io:writeu8 3 stream)
     (loop for value across array
           do (fast-io:writeu32-be (ieee-floats:encode-float32 value) stream)))
    (:double
     (fast-io:writeu8 4 stream)
     (loop for value across array
           do (fast-io:writeu64-be (ieee-floats:encode-float64 value) stream)))))

(defun vformat-read-vector (stream)
  (let* ((size (fast-io:read32-be stream))
         (type (ecase (fast-io:readu8 stream)
                 (0 :char)
                 (1 :int)
                 (2 :uint)
                 (3 :float)
                 (4 :double)))
         (array (cffi:foreign-alloc type :count size)))
    (ecase type
      (:char
       (loop for i from 0 below size
             do (setf (cffi:mem-aref array :char i) (fast-io:read8 stream))))
      (:int
       (loop for i from 0 below size
             do (setf (cffi:mem-aref array :int i) (fast-io:read32-be stream))))
      (:uint
       (loop for i from 0 below size
             do (setf (cffi:mem-aref array :uint i) (fast-io:readu32-be stream))))
      (:float
       (loop for i from 0 below size
             do (setf (cffi:mem-aref array :float i) (ieee-floats:decode-float32 (fast-io:readu32-be stream)))))
      (:double
       (loop for i from 0 below size
             do (setf (cffi:mem-aref array :double i) (ieee-floats:decode-float64 (fast-io:readu64-be stream))))))
    (values array
            size
            type)))

(defun vformat-write-string (stream string)
  (loop for char across string
        do (fast-io:writeu8 (char-code char) stream))
  (fast-io:writeu8 0 stream))

(defun vformat-read-string (stream)
  (let ((string (make-array 0 :element-type 'character :initial-element #\Null
                              :adjustable T :fill-pointer T)))
    (loop for code = (fast-io:readu8 stream)
          until (= 0 code)
          do (vector-push-extend (code-char code) string))
    string))

(defun vertex-buffer-type->int (type)
  (position type *vertex-buffer-type-list*))

(defun int->vertex-buffer-type (int)
  (elt *vertex-buffer-type-list* int))

(defun vertex-buffer-usage->int (type)
  (position type *vertex-buffer-data-usage-list*))

(defun int->vertex-buffer-usage (int)
  (elt *vertex-buffer-data-usage-list* int))

(defun vformat-write-buffer (stream data type usage element-type)
  (vformat-write-string stream "VBUF")
  (fast-io:writeu8 (vertex-buffer-type->int type) stream)
  (fast-io:writeu8 (vertex-buffer-usage->int usage) stream)
  (vformat-write-vector stream data element-type))

(defun vformat-read-buffer (stream)
  (let ((name (vformat-read-string stream)))
    (unless (string= name "VBUF")
      (error "Expected vertex buffer identifier, but got ~s" name)))
  (let ((type (int->vertex-buffer-type (fast-io:readu8 stream)))
        (usage (int->vertex-buffer-usage (fast-io:readu8 stream))))
    (multiple-value-bind (data size element-type) (vformat-read-vector stream)
      (values data size type usage element-type))))

(defun vformat-write-array (stream buffer-refs)
  (when (<= (expt 2 8) (length buffer-refs))
    (error "More than 2⁸ buffer refs."))
  (vformat-write-string stream "VARR")
  (fast-io:writeu8 (length buffer-refs) stream)
  (loop for (buffer index size stride offset normalized) in buffer-refs
        do (fast-io:writeu8 buffer stream)
           (fast-io:writeu8 index stream)
           (fast-io:writeu8 size stream)
           (fast-io:writeu8 stride stream)
           (fast-io:writeu8 offset stream)
           (fast-io:writeu8 (if normalized 1 0) stream)))

(defun vformat-read-array (stream)
  (let ((name (vformat-read-string stream)))
    (unless (string= name "VARR")
      (error "Expected vertex array identifier, but got ~s" name)))
  (let ((size (fast-io:readu8 stream)))
    (loop repeat size
          collect (list (fast-io:readu8 stream)
                        (fast-io:readu8 stream)
                        (fast-io:readu8 stream)
                        (fast-io:readu8 stream)
                        (fast-io:readu8 stream)
                        (if (< 0 (fast-io:readu8 stream)) T NIL)))))

(defun vformat-write-bundle (stream buffers array)
  (when (<= (expt 2 8) (length buffers))
    (error "More than 2⁸ buffers."))
  (vformat-write-string stream "VBUN")
  (fast-io:writeu8 (length buffers) stream)
  (dolist (buffer buffers)
    (etypecase buffer
      (list (apply #'vformat-write-buffer stream buffer))
      (vertex-buffer-asset
       (vformat-write-buffer stream
                             (first (inputs buffer))
                             (buffer-type buffer)
                             (data-usage buffer)
                             (element-type buffer)))))
  (etypecase array
    (list (vformat-write-array stream array))
    (vertex-array-asset
     (vformat-write-array
      stream (loop for i from 0
                   for input in (inputs array)
                   collect (destructuring-bind (bufref &key (index i)
                                                             (size 3)
                                                             (stride 0)
                                                             (offset 0)
                                                             (normalized NIL))
                               input
                             (list (or (position bufref buffers)
                                       (error "Unable to find ~a in buffer list." bufref))
                                   index size stride offset normalized)))))))

(defun vformat-read-bundle (stream)
  (let ((name (vformat-read-string stream)))
    (unless (string= name "VBUN")
      (error "Expected vertex bundle identifier, but got ~s" name)))
  (let* ((buffers (loop repeat (fast-io:readu8 stream)
                        collect (multiple-value-bind (data size type usage element-type)
                                    (vformat-read-buffer stream)
                                  (let ((asset (make-asset 'vertex-buffer-asset
                                                           (list data)
                                                           :type type
                                                           :element-type element-type
                                                           :data-usage usage
                                                           :size size)))
                                    (prog1 (load asset)
                                      (setf (inputs asset) NIL)
                                      (cffi:foreign-free data))))))
         (inputs (loop for (bufref index size stride offset normalized) in (vformat-read-array stream)
                       collect (list (elt buffers bufref)
                                     :index index
                                     :size size
                                     :stride stride
                                     :offset offset
                                     :normalized normalized))))
    (let ((asset (make-asset 'vertex-array-asset inputs)))
      (prog1 (load asset)
        (setf (inputs asset) NIL)
        (mapcar #'offload buffers)))))

(defun write-vformat (file buffers array &key (if-exists :error))
  (with-open-file (stream file :direction :output
                               :element-type '(unsigned-byte 8)
                               :if-exists if-exists)
    (when stream
      (fast-io:with-fast-output (buffer stream)
        (vformat-write-bundle buffer buffers array))
      file)))

(defun load-vformat (file &key (if-does-not-exist :error))
  (with-open-file (stream file :direction :input
                               :element-type '(unsigned-byte 8)
                               :if-does-not-exist if-does-not-exist)
    (when stream
      (fast-io:with-fast-input (buffer NIL stream)
        (vformat-read-bundle buffer)))))

(defclass vertex-format-asset (asset)
  ((size :initform NIL :accessor size)))

(defmethod coerce-input ((asset vertex-format-asset) (file string))
  (coerce-input asset (uiop:native-namestring file)))

(defmethod coerce-input ((asset vertex-format-asset) (file pathname))
  file)

(defmethod finalize-resource ((type (eql 'vertex-format-asset)) resource)
  (gl:delete-vertex-arrays (list resource)))

(defmethod load progn ((asset vertex-format-asset))
  (let* ((inputs (coerced-inputs asset))
         (array (load-vformat (first inputs))))
    (setf (resource asset) (resource array))
    (setf (size asset) (size array))))
