(in-package #:org.shirakumo.fraf.trial)

;;;; Minimal ad-hoc float pixel format:
;; FORMAT ::= CHANNELS WIDTH HEIGHT DEPTH PIXEL*
;; CHANNELS ::= ub16
;;          --- Integer in [1,4]
;; WIDTH    ::= ub16
;; HEIGHT   ::= ub16
;; DEPTH    ::= ub16
;; PIXEL    ::= float

(defmethod load-image (path (type (eql :raw)))
  (depot:with-open (tx path :input '(unsigned-byte 8))
    (let ((header (make-array (* 2 4) :element-type '(unsigned-byte 8))))
      (depot:read-from tx header)
      (let* ((c (nibbles:ub16ref/le header 0))
             (w (nibbles:ub16ref/le header 2))
             (h (nibbles:ub16ref/le header 4))
             (d (nibbles:ub16ref/le header 6))
             (data (make-static-vector (* c w h d 4))))
        (depot:read-from tx data)
        (make-texture-source :src (list NIL NIL NIL w h d)
                             :dst (list NIL NIL NIL NIL NIL NIL)
                             :pixel-data data
                             :pixel-type :float
                             :pixel-format (ecase c
                                             (1 :red)
                                             (2 :rg)
                                             (3 :rgb)
                                             (4 :rgba)))))))

(defmethod save-image ((source texture-source) path (type (eql :raw)) &key)
  (destructuring-bind (x y z w h d) (texture-source-src source)
    (declare (ignore x y z))
    (assert (eql :float (texture-source-pixel-type source)))
    (depot:with-open (tx path :output '(unsigned-byte 8))
      (let ((c (ecase (texture-source-pixel-format source)
                 (:red 1) (:rg 2) (:rgb 3) (:rgba 4)))
            (header (make-array (* 2 4) :element-type '(unsigned-byte 8))))
        (setf (nibbles:ub16ref/le header 0) c)
        (setf (nibbles:ub16ref/le header 2) w)
        (setf (nibbles:ub16ref/le header 4) h)
        (setf (nibbles:ub16ref/le header 6) d)
        (depot:write-to tx header)
        (with-static-vector (data (* c w h d 4))
          (mem:with-memory-region (region (texture-source-pixel-data source))
            (static-vectors:replace-foreign-memory
             (static-vectors:static-vector-pointer data)
             (mem:memory-region-pointer region)
             (length data)))
          (depot:write-to tx data)
          path)))))
