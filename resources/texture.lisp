#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass texture (gl-resource)
  ((width :initarg :width :writer (setf width))
   (height :initarg :height :writer (setf height))
   (depth :initarg :depth :writer (setf depth))
   (target :initarg :target :writer (setf target))
   (levels :initarg :levels :writer (setf levels))
   (samples :initarg :samples :writer (setf samples))
   (internal-format :initarg :internal-format :writer (setf internal-format))
   (pixel-format :initarg :pixel-format :writer (setf pixel-format))
   (pixel-type :initarg :pixel-type :writer (setf pixel-type))
   (pixel-data :initarg :pixel-data :writer (setf pixel-data))
   (mag-filter :initarg :mag-filter :writer (setf mag-filter))
   (min-filter :initarg :min-filter :writer (setf min-filter))
   (mipmap-levels :initarg :mipmap-levels :writer (setf mipmap-levels))
   (mipmap-lod :initarg :mipmap-lod :writer (setf mipmap-lod))
   (anisotropy :initarg :anisotropy :writer (setf anisotropy))
   (wrapping :initarg :wrapping :writer (setf wrapping))
   (border-color :initarg :border-color :writer (setf border-color))
   (storage :initarg :storage)))

(define-unbound-reader texture width NIL)
(define-unbound-reader texture height NIL)
(define-unbound-reader texture depth NIL)
(define-unbound-reader texture target :texture-2d)
(define-unbound-reader texture levels 1)
(define-unbound-reader texture samples 1)
(define-unbound-reader texture internal-format :rgba)
(define-unbound-reader texture pixel-format NIL)
(define-unbound-reader texture pixel-type NIL)
(define-unbound-reader texture pixel-data NIL)
(define-unbound-reader texture mag-filter :linear)
(define-unbound-reader texture min-filter :linear-mipmap-linear)
(define-unbound-reader texture mipmap-levels (list 0 1000))
(define-unbound-reader texture mipmap-lod (list -1000 1000 0.0))
(define-unbound-reader texture anisotropy NIL)
(define-unbound-reader texture wrapping '(:clamp-to-edge :clamp-to-edge :clamp-to-edge))
(define-unbound-reader texture border-color (vec 0 0 0 0))
(define-unbound-reader texture storage :dynamic)

(defun texture-texspec (texture)
  (loop for slot in '(width height depth target levels samples internal-format
                      pixel-format pixel-type pixel-data
                      mag-filter min-filter mipmap-levels mipmap-lod
                      anisotropy wrapping border-color storage)
        when (slot-boundp texture slot)
        collect (intern (string slot) "KEYWORD")
        and collect (slot-value texture slot)))

(defmethod shared-initialize :around ((texture texture) slots &rest args)
  (when (getf args :wrapping)
    (setf (getf args :wrapping) (enlist (getf args :wrapping)
                                        (getf args :wrapping)
                                        (getf args :wrapping))))
  (when (and (getf args :internal-format)
             (not (getf args :pixel-format))
             (not (slot-boundp texture 'pixel-format)))
    (setf (getf args :pixel-format) (texture-internal-format->pixel-format (getf args :internal-format))))
  (when (and (getf args :pixel-format)
             (not (getf args :pixel-type))
             (not (slot-boundp texture 'pixel-type)))
    (setf (getf args :pixel-type) (pixel-format->pixel-type (getf args :pixel-format))))
  (apply #'call-next-method texture slots args))

(defmethod shared-initialize :before ((texture texture) slots &rest initargs)
  (declare (ignore slots))
  ;; (assert (< 0 width (gl:get* :max-texture-size)))
  ;; (assert (< 0 height (gl:get* :max-texture-size)))
  ;; (assert (< 0 depth (gl:get* :max-texture-size)))
  (flet ((test (func prop &optional (mod #'identity))
           (let* ((temp '#.(make-symbol "NO-VALUE"))
                  (value (getf initargs prop temp)))
             (unless (eq value temp)
               (funcall func (funcall mod (getf initargs prop)))))))
    (test (lambda (x) (check-type x vec4)) :border-color)
    (test #'check-texture-target :target)
    (test #'check-texture-internal-format :internal-format)
    (test #'check-texture-pixel-format :pixel-format)
    (test #'check-texture-pixel-type :pixel-type)
    (test #'check-texture-mag-filter :mag-filter)
    (test #'check-texture-min-filter :min-filter)
    (test #'check-texture-wrapping :wrapping #'first)
    (test #'check-texture-wrapping :wrapping #'second)
    (test #'check-texture-wrapping :wrapping #'third)))

(defmethod print-object ((texture texture) stream)
  (print-unreadable-object (texture stream :type T :identity T)
    (case (target texture)
      (:texture-1d
       (format stream "~a" (width texture)))
      ((:texture-2d :texture-1d-array :texture-cube-map :texture-2d-multisample)
       (format stream "~ax~a" (width texture) (height texture)))
      ((:texture-3d :texture-2d-array :texture-2d-multisample-array)
       (format stream "~ax~ax~a" (width texture) (height texture) (depth texture))))
    (format stream " ~a" (internal-format texture))))

(defmethod destructor ((texture texture))
  (let ((tex (gl-name texture)))
    (lambda () (when tex (gl:delete-textures (list tex))))))

(defmacro with-pixel-data-pointer ((ptr data) &body body)
  (let ((datag (gensym "DATA")))
    `(let ((,datag ,data))
       (flet ((thunk (,ptr) () ,@body))
         (etypecase ,datag
           (null
            (thunk (cffi:null-pointer)))
           (cffi:foreign-pointer
            (thunk ,datag))
           (vector
            (if (static-vector-p ,datag)
                (thunk (static-vectors:static-vector-pointer ,datag))
                (with-pointer-to-vector-data (,ptr ,datag)
                  (thunk ,ptr)))))))))

(defun allocate-texture-storage (texture)
  (with-accessors* (target storage levels internal-format width height depth
                           samples pixel-format pixel-type pixel-data) texture
    (let ((internal-format (cffi:foreign-enum-value '%gl:enum internal-format)))
      ;; FIXME: Handle array cases better, factor this out into an update routine.
      (case target
        ((:texture-1d)
         (with-pixel-data-pointer (ptr pixel-data)
           (ecase storage
             (:dynamic (%gl:tex-image-1d target 0 internal-format width 0 pixel-format pixel-type ptr))
             (:static (%gl:tex-storage-1d target levels internal-format width)))))
        ((:texture-2d :texture-1d-array)
         (with-pixel-data-pointer (ptr pixel-data)
           (ecase storage
             (:dynamic (%gl:tex-image-2d target 0 internal-format width height 0 pixel-format pixel-type ptr))
             (:static (%gl:tex-storage-2d target levels internal-format width height)))))
        ((:texture-cube-map)
         (loop for target in '(:texture-cube-map-positive-x :texture-cube-map-negative-x
                               :texture-cube-map-positive-y :texture-cube-map-negative-y
                               :texture-cube-map-positive-z :texture-cube-map-negative-z)
               for data in (if (consp pixel-data)
                               pixel-data
                               (let ((c (cons pixel-data NIL)))
                                 (setf (cdr c) c)))
               do (with-pixel-data-pointer (ptr data)
                    (ecase storage
                      (:dynamic (%gl:tex-image-2d target 0 internal-format width height 0 pixel-format pixel-type ptr))
                      (:static (%gl:tex-storage-2d target levels internal-format width height))))))
        ((:texture-3d :texture-2d-array)
         (ecase storage
           (:dynamic (%gl:tex-image-3d target 0 internal-format width height depth 0 pixel-format pixel-type (cffi:null-pointer)))
           (:static (%gl:tex-storage-3d target levels internal-format width height depth)))
         (typecase pixel-data
           (list
            (loop for z from 0
                  for data in pixel-data
                  do (with-pixel-data-pointer (ptr data)
                       (%gl:tex-sub-image-3d target 0 0 0 z width height 1 pixel-format pixel-type ptr))))
           ((not null)
            (with-pixel-data-pointer (ptr pixel-data)
              (%gl:tex-sub-image-3d target 0 0 0 0 width height depth pixel-format pixel-type ptr)))))
        ((:texture-2d-multisample)
         (%gl:tex-storage-2d-multisample target samples internal-format width height 1))
        ((:texture-2d-multisample-array)
         (%gl:tex-storage-3d-multisample target samples internal-format width height depth 1))))))

(defmethod allocate ((texture texture))
  (with-accessors* (width height depth target samples internal-format pixel-format pixel-type pixel-data
                          mag-filter min-filter mipmap-levels mipmap-lod anisotropy wrapping border-color storage)
      texture
    (let ((tex (gl:gen-texture)))
      (with-cleanup-on-failure (gl:delete-textures (list tex))
        (gl:bind-texture target tex)
        (allocate-texture-storage texture)
        (gl:tex-parameter target :texture-wrap-s (first wrapping))
        (unless (find target '(:texture-1d-array :texture-1d))
          (gl:tex-parameter target :texture-wrap-t (second wrapping)))
        (when (eql target :texture-cube-map)
          (gl:tex-parameter target :texture-wrap-r (third wrapping)))
        (when (find :clamp-to-border wrapping)
          (gl:tex-parameter target :texture-border-color
                            (list (vx border-color) (vy border-color) (vz border-color) (vw border-color))))
        (gl:tex-parameter target :texture-min-filter min-filter)
        (gl:tex-parameter target :texture-mag-filter mag-filter)
        (unless (or (eql target :texture-2d-multisample)
                    (find internal-format '(:depth-component :depth-stencil)))
          (when (find min-filter '(:linear-mipmap-linear :linear-mipmap-nearest
                                   :nearest-mipmap-linear :nearest-mipmap-nearest))
            (destructuring-bind (&optional (base 0) (max 1000)) mipmap-levels
              (gl:tex-parameter target :texture-base-level base)
              (gl:tex-parameter target :texture-max-level max))
            (destructuring-bind (&optional (min -1000) (max 1000) (bias 0.0)) mipmap-lod
              (gl:tex-parameter target :texture-min-lod min)
              (gl:tex-parameter target :texture-max-lod max)
              (gl:tex-parameter target :texture-lod-bias bias))
            (gl:generate-mipmap target))
          (when anisotropy
            (gl:tex-parameter target :texture-max-anisotropy-ext anisotropy)))
        (gl:bind-texture target 0)
        (setf (data-pointer texture) tex)))))

(defmethod resize ((texture texture) width height)
  (when (or (/= width (width texture))
            (/= height (height texture)))
    (setf (width texture) width)
    (setf (height texture) height)
    (when (allocated-p texture)
      (assert (eql :dynamic (storage texture)))
      (gl:bind-texture (target texture) (gl-name texture))
      (allocate-texture-storage texture)
      (when (find (min-filter texture) '(:linear-mipmap-linear :linear-mipmap-nearest
                                         :nearest-mipmap-linear :nearest-mipmap-nearest))
        (gl:generate-mipmap (target texture)))
      (gl:bind-texture (target texture) 0))))

;;;; Texture spec wrangling
;; The idea of this is that, in order to maximise sharing of texture resources
;; between independent parts, we need to join (in the lattice sense) two texture
;; specs together to determine a common closest supertype that can be used by
;; both. Some texture attributes are not joinable and in such a case the join will
;; simply return NIL. Others are easily joinable by maxing. Yet others are a huge
;; pain in the ass, such as the internal format. This implementation is a best-
;; effort that in theory is capable of perfectly handling every combination.
;; however, due to limits in my ability to give a shit it is currently only
;; feasible for common formats. The primary place that still needs work is the
;; restructure-texture-format, the rest should be generic enough to handle every-
;; thing, at least by my estimation.

(defun destructure-texture-format (format)
  (cl-ppcre:register-groups-bind (compression signed super r r-size r-type g g-size g-type b b-size b-type rg rg-size rg-type rgb rgb-size rgb-type rgba rgba-size rgba-type a a-size a-type e e-size d d-size d-type s s-size s-type rgtc bptc floatage snorm unorm) ("^(compressed-)?(signed-)?(s)?((?:red|r)(\\d+)?(ui|i|f)?)?(-g(\\d+)?(ui|i|f)?)?(-b(\\d+)?(ui|i|f)?)?(rg(\\d+)?(ui|i|f)?)?(rgb(\\d+)?(ui|i|f)?)?(rgba(\\d+)?(ui|i|f)?)?(-(?:a|alpha)(\\d+)?(ui|i|f)?)?(-e(\\d+)?)?(depth(?:-component-?)?(\\d+)?(f)?)?(-stencil(\\d+)?(ui|i|f)?)?(-rgtc\\d)?(-bptc)?(-signed-float|-unsigned-float)?(-snorm)?(-unorm)?$" (string-downcase format))
    (macrolet ((parse-part (part)
                 (let* ((*print-case* (readtable-case *readtable*))
                        (type (intern (format NIL "~a-~a" part 'type)))
                        (size (intern (format NIL "~a-~a" part 'size))))
                   `(when ,part
                      (list (when ,size (parse-integer ,size))
                            (cond ((or (equalp ,type "f") floatage) :float)
                                  ((or (equalp ,type "i") (equalp ,type "ui")) :integer))
                            (cond ((or (equalp ,type "i") signed (equalp ,type "-signed-float")) :signed)
                                  ((or (equalp ,type "ui") (equalp ,type "-unsigned-float")) :unsigned)))))))
      (let ((r (parse-part r))
            (g (parse-part g))
            (b (parse-part b))
            (a (parse-part a))
            (d (parse-part d))
            (s (parse-part s))
            (rg (parse-part rg))
            (rgb (parse-part rgb))
            (rgba (parse-part rgba)))
        (list :r (or r rg rgb rgba)
              :g (or g rg rgb rgba)
              :b (or b rgb rgba)
              :a (or a rgba)
              :depth d
              :stencil s
              :shared (cond (e-size (parse-integer e-size))
                            (e T))
              :features (loop for i in (list compression super rgtc bptc snorm unorm)
                              for f in (list :compression :super :rgtc :bptc :snorm :unorm)
                              when i collect f))))))

(defun restructure-texture-format (format)
  (destructuring-bind (&key r g b a depth stencil shared features) format
    ;; FIXME: This is a primitive approach to restructuring. Some of the formats
    ;;        are SERIOUSLY WEIRD, but I also expect them to not ever get used, so
    ;;        this should be "good enough" for now.
    (let ((rgba (cond ((and r g b a) "rgba")
                      ((and r g b) "rgb")
                      ((and r g) "rg")
                      ((and r) "r"))))
      (flet ((format-type (type)
               (format NIL "~@[~a~]~@[~a~]"
                       (first type) (case (second type)
                                      (:float "f")
                                      (:integer (case (third type)
                                                  (:signed "i")
                                                  (:unsigned "ui")))))))
        (values
         (find-symbol (string-upcase
                       (cond ((find :compression features)
                              (format NIL "compressed-~a~@[-rgtc~a~]~@[-~a~]~@[-~a~]"
                                      rgba (when (find :rgtc features) (length rgba))
                                      (find :bptc features) (find :unorm features)))
                             ((and depth stencil)
                              (format NIL "depth~@[~a~]-stencil~@[~a~]"
                                      (format-type depth) (format-type stencil)))
                             (depth
                              (format NIL "depth-component~@[~a~]"
                                      (format-type depth)))
                             (stencil
                              (format NIL "stencil-index~@[~a~]"
                                      (format-type stencil)))
                             (T
                              ;; FIXME: Doesn't handle different types for each component
                              (format NIL "~a~a~@[-e~a~]" rgba (format-type r) shared))))
                      "KEYWORD"))))))

(defun join-texture-format-typespec (a b)
  (flet ((same (a b)
           (cond ((eql a b) (values a T)) ((null a) (values b T)) ((null b) (values a T))))
         (max* (a b)
           (cond ((and a b) (max a b)) (a a) (b b))))
    (cond ((and a b)
           (when (and (nth-value 1 (same (second a) (second b)))
                      (nth-value 1 (same (third a) (third b))))
             (list (max* (first a) (first b))
                   (same (second a) (second b))
                   (same (third a) (third b)))))
          (a a)
          (b b))))

(defun join-texture-format (a b)
  (let ((a (destructure-texture-format a))
        (b (destructure-texture-format b)))
    (flet ((same (field)
             (equal (getf a field) (getf b field)))
           (join-texture-format-typespec* (f)
             (join-texture-format-typespec (getf a f) (getf b f))))
      (when (and (same :features)
                 (same :shared))
        (cond ((and (getf a :depth) (getf b :depth))
               (restructure-texture-format
                (list :depth (join-texture-format-typespec* :depth)
                      :stencil (join-texture-format-typespec* :stencil)
                      :features (getf a :features))))
              ((and (getf a :stencil) (getf b :stencil))
               (restructure-texture-format
                (list :stencil (join-texture-format-typespec* :stencil)
                      :features (getf a :features))))
              ((not (or (getf a :depth) (getf b :depth)))
               (restructure-texture-format
                (list :r (join-texture-format-typespec* :r)
                      :g (join-texture-format-typespec* :g)
                      :b (join-texture-format-typespec* :b)
                      :a (join-texture-format-typespec* :a)
                      :features (getf a :features)))))))))

(defun join-texspec (a b)
  (flet ((same (field)
           (equal (getf a field) (getf b field)))
         (max* (a b)
           (cond ((and a b) (max a b)) (a a) (b b))))
    (when (and (same :target)
               (same :pixel-type)
               (same :pixel-data)
               (same :mag-filter)
               (same :min-filter)
               (same :wrapping))
      ;; FIXME: width and height handling
      (let ((texspec (copy-list a)))
        (setf (getf texspec :samples)
              (max* (getf a :samples)
                    (getf b :samples)))
        (setf (getf texspec :anisotropy)
              (max* (getf a :anisotropy)
                    (getf b :anisotropy)))
        (setf (getf texspec :internal-format)
              (join-texture-format
               (getf a :internal-format)
               (getf b :internal-format)))
        (when (getf texspec :internal-format)
          texspec)))))

(defun join-texspecs (texspecs)
  (let (spec (collected ()))
    (tagbody
     next
       (setf spec (pop texspecs))
       (unless spec (go end))
     loop
       (dolist (other texspecs)
         (let ((merged (join-texspec spec other)))
           (when merged
             (setf texspecs (remove other texspecs :test #'eq))
             (setf spec merged)
             (go loop))))
       (push spec collected)
       (go next)
     end)
    collected))
