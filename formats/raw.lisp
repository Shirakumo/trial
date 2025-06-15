(in-package #:org.shirakumo.fraf.trial)

;;;; Minimal ad-hoc float pixel format:
;; FORMAT ::= TYPE CHANNELS WIDTH HEIGHT DEPTH PIXEL*
;; TYPE     ::= ub16
;;          --- Integer designating the pixel type:
;;            1 ub8
;;            2 ub16
;;            4 f32
;;            8 f64
;; CHANNELS ::= ub16
;;          --- Integer in [1,4]
;; WIDTH    ::= ub16
;; HEIGHT   ::= ub16
;; DEPTH    ::= ub16
;; PIXEL    ::= type

(defmethod load-image ((stream stream) (type (eql :raw)) &key)
  (let ((header (make-array (* 2 5) :element-type '(unsigned-byte 8))))
    (read-sequence header stream)
    (let* ((f (nibbles:ub16ref/le header 0))
           (c (nibbles:ub16ref/le header 2))
           (w (nibbles:ub16ref/le header 4))
           (h (nibbles:ub16ref/le header 6))
           (d (nibbles:ub16ref/le header 8))
           (data (make-array (* f c w (max 1 h) (max 1 d)) :element-type '(unsigned-byte 8))))
      (read-sequence data stream)
      (make-texture-source :src (list NIL NIL NIL w h d)
                           :dst (list NIL NIL NIL NIL NIL NIL)
                           :pixel-data data
                           :pixel-type (ecase f
                                         (1 :unsigned-byte)
                                         (2 :unsigned-short)
                                         (4 :float)
                                         (8 :double))
                           :pixel-format (ecase c
                                           (1 :red)
                                           (2 :rg)
                                           (3 :rgb)
                                           (4 :rgba))))))

(defmethod save-image ((source texture-source) (stream stream) (type (eql :raw)) &key)
  (destructuring-bind (x y z w h d) (texture-source-src source)
    (declare (ignore x y z))
    (let ((f (ecase (texture-source-pixel-type source)
               (:unsigned-byte 1) (:unsigned-short 2) (:float 4) (:double 8)))
          (c (ecase (texture-source-pixel-format source)
               (:red 1) (:rg 2) (:rgb 3) (:rgba 4)))
          (header (make-array (* 2 5) :element-type '(unsigned-byte 8))))
      (setf (nibbles:ub16ref/le header 0) f)
      (setf (nibbles:ub16ref/le header 2) c)
      (setf (nibbles:ub16ref/le header 4) w)
      (setf (nibbles:ub16ref/le header 6) h)
      (setf (nibbles:ub16ref/le header 8) d)
      (write-sequence header stream)
      (let ((data (make-array (* f c w (max 1 h) (max 1 d)) :element-type '(unsigned-byte 8))))
        (mem:with-memory-region (region (texture-source-pixel-data source))
          (mem:replace data region))
        (write-sequence data stream)))))

(define-native-image-transcoder :raw)
