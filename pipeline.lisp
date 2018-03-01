#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass pipeline ()
  ((nodes :initform NIL :accessor nodes)
   (passes :initform #() :accessor passes)
   (textures :initform #() :accessor textures)))

(defmethod finalize ((pipeline pipeline))
  (clear pipeline))

(defmethod enter ((pass shader-pass) (pipeline pipeline))
  (pushnew pass (nodes pipeline)))

(defmethod leave ((pass shader-pass) (pipeline pipeline))
  (setf (nodes pipeline) (delete pass (nodes pipeline))))

(defmethod clear-pipeline ((pipeline pipeline))
  (loop for tex across (textures pipeline)
        do (finalize tex))
  (loop for pass across (passes pipeline)
        do (when (framebuffer pass)
             (finalize (framebuffer pass))
             (setf (framebuffer pass) NIL))
           (remove-handler pass pipeline))
  (setf (nodes pipeline) ())
  (setf (passes pipeline) #())
  (setf (textures pipeline) #()))

(defmethod connect ((source flow:port) (target flow:port) (pipeline pipeline))
  (unless (find (flow:node source) (nodes pipeline))
    (enter (flow:node source) pipeline))
  (unless (find (flow:node target) (nodes pipeline))
    (enter (flow:node target) pipeline))
  (flow:connect source target 'flow:directed-connection)
  pipeline)

(defmethod check-consistent ((pipeline pipeline))
  (dolist (node (nodes pipeline))
    (dolist (port (flow:ports node))
      (check-consistent port))))

;; FIXME: update for new asset system, take into account all
;;        sorts of texture options and compare according to
;;        them.
(defun allocate-textures (passes textures kind)
  (flow:allocate-ports passes :sort NIL :test kind :attribute :texid)
  (let* ((texture-count (loop for pass in passes
                              when (flow:ports pass)
                              maximize (loop for port in (flow:ports pass)
                                             when (and (flow:attribute port :texid)
                                                       (funcall kind port))
                                             maximize (1+ (flow:attribute port :texid)))))
         (offset (length textures)))
    (adjust-array textures (+ offset texture-count) :initial-element NIL)
    (dolist (pass passes textures)
      (dolist (port (flow:ports pass))
        (when (funcall kind port)
          ;; FIXME: Recompute the minimal upgraded texspec across all shared
          ;;        ports, as the partitioning done by the allocation mechanism
          ;;        might have broken up texspecs that were initially grouped.
          (let* ((texid (+ offset (flow:attribute port :texid)))
                 (texture (or (aref textures texid)
                              (apply #'make-instance 'texture (texspec port)))))
            (setf (aref textures texid) texture)
            (setf (texture port) texture)
            (dolist (connection (flow:connections port))
              (setf (texture (flow:right connection)) texture))))))))

(defmethod resize ((pipeline pipeline) width height)
  (gl:scissor 0 0 width height)
  ;; FIXME: keep width/height according to desired texspec
  (loop for texture across (textures pipeline)
        do (resize texture width height)))

(defun normalize-texspec (texspec width height)
  (assert (= 0 (getf texspec :level 0)))
  (assert (= :dynamic (getf texpsec :storage :dynamic)))
  (let ((initargs (c2mop:class-default-initargs (find-class 'texture))))
    (loop for (key) on initargs by #'cddr
          for val = (getf texspec key)
          collect key
          collect (cond ((eql key :width)
                         (if val
                             (eval `(let ((width ,width)
                                          (height ,height))
                                      ,val))
                             width))
                        ((eql key :height)
                         (if val
                             (eval `(let ((width ,width)
                                          (height ,height))
                                      ,val))
                             height))
                        (val
                         val)
                        (T
                         (getf normalized key))))))

(defun destructure-texture-format (format)
  (cl-ppcre:register-groups-bind (compression signed super r r-size r-type g g-size g-type b b-size b-type rg rg-size rg-type rgb rgb-size rgb-type rgba rgba-size rgba-type a a-size a-type e e-size d d-size d-type s s-size s-type rgtc bptc floatage snorm unorm) ("^(compressed-)?(signed-)?(s)?((?:red|r)(\\d+)?(ui|i|f)?)?(-g(\\d+)?(ui|i|f)?)?(-b(\\d+)?(ui|i|f)?)?(rg(\\d+)?(ui|i|f)?)?(rgb(\\d+)?(ui|i|f)?)?(rgba(\\d+)?(ui|i|f)?)?(-(?:a|alpha)(\\d+)?(ui|i|f)?)?(-e(\\d+)?)?(depth(?:-component-?)?(\\d+)?(f)?)?(-stencil(\\d+)?(ui|i|f)?)?(-rgtc\\d)?(-bptc)?(-signed-float|-unsigned-float)?(-snorm)?(-unorm)?$" (string-downcase format))
    (macrolet ((parse-part (part)
                 (let ((type (intern (format NIL "~a-~a" part 'type)))
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
                             (stencil
                              (format NIL "depth~@[~a~]-stencil~@[~a~]"
                                      (format-type depth) (format-type stencil)))
                             (depth
                              (format NIL "depth-component~@[-~a~]"
                                      (format-type depth)))
                             (T
                              ;; FIXME: Doesn't handle different types for each component
                              (format NIL "~a~a~@[-e~a~]" rgba (format-type r) shared))))
                      "KEYWORD"))))))

(defun upgraded-texture-format-typespec (a b)
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

(defun upgraded-texture-format (a b)
  (let ((a (destructure-texture-format a))
        (b (destructure-texture-format b)))
    (flet ((same (field)
             (equal (getf a field) (getf b field)))
           (upgraded-texture-format-typespec* (f)
             (upgraded-texture-format-typespec (getf a f) (getf b f))))
      (when (and (same :features)
                 (same :shared))
        (cond ((and (getf a :depth) (getf b :depth))
               (restructure-texture-format
                (list :depth (upgraded-texture-format-typespec* :depth)
                      :stencil (upgraded-texture-format-typespec* :stencil)
                      :features (getf a :features))))
              ((not (or (getf a :depth) (getf b :depth)))
               (restructure-texture-format
                (list :r (upgraded-texture-format-typespec* :r)
                      :g (upgraded-texture-format-typespec* :g)
                      :b (upgraded-texture-format-typespec* :b)
                      :a (upgraded-texture-format-typespec* :a)
                      :features (getf a :features)))))))))

(defun upgraded-texspec (a b)
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
      (let ((texspec (copy-list a)))
        (setf (getf texspec :samples)
              (max (getf a :samples)
                   (getf b :samples)))
        (setf (getf texspec :anisotropy)
              (max* (getf a :anisotropy)
                    (getf b :anisotropy)))
        (setf (getf texpsec :internal-format)
              (upgraded-texture-format
               (getf a :internal-format)
               (getf b :internal-format)))
        (when (getf texpsec :internal-format)
          texspec)))))

(defmethod pack-pipeline ((pipeline pipeline) target)
  (check-consistent pipeline)
  (v:info :trial.pipeline "~a packing for ~a (~ax~a)" pipeline target (width target) (height target))
  (let* ((passes (flow:topological-sort (nodes pipeline)))
         (textures (make-array 0 :initial-element NIL :adjustable T))
         (texspecs (mapcar #'texspec (mapcan #'flow:ports passes))))
    (clear-pipeline pipeline)
    (dolist (texspec (remove-duplicates texspecs :test #'texspec-matching-p))
      (allocate-textures passes textures (lambda (port) (texspec-matching-p (texspec port texspec)))))
    (v:info :trial.pipeline "~a pass order: ~a" pipeline passes)
    (v:info :trial.pipeline "~a texture count: ~a" pipeline (length textures))
    (v:info :trial.pipeline "~a texture allocation: ~:{~%~a~:{~%    ~a: ~a~}~}" pipeline
            (loop for pass in passes
                  collect (list pass (loop for port in (flow:ports pass)
                                           collect (list (flow:name port) (texture port))))))
    (dolist (pass passes)
      (add-handler pass pipeline)
      (setf (framebuffer pass)
            (load (make-instance 'framebuffer
                                 :attachments (loop for port in (flow:ports pass)
                                                    when (typep port '(and output (not buffer)))
                                                    collect (list (texture port) :attachment (attachment port)))))))
    (setf (passes pipeline) (coerce passes 'vector))
    (setf (textures pipeline) textures)))

(defmethod paint-with ((pipeline pipeline) source)
  (loop for pass across (passes pipeline)
        for fbo = (framebuffer pass)
        do (gl:bind-framebuffer :framebuffer (gl-name fbo))
           ;; FIXME: Figure out which to clear depending on framebuffer attachments
           (gl:clear :color-buffer :depth-buffer :stencil-buffer)
           (paint-with pass source)))

(defmethod register-object-for-pass ((pipeline pipeline) object)
  (loop for pass across (passes pipeline)
        do (register-object-for-pass pass object)))
