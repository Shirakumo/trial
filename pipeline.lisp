#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass pipeline ()
  ((nodes :initform NIL :accessor nodes)
   (passes :initform #() :accessor passes)
   (textures :initform #() :accessor textures)
   (texspecs :initform #() :accessor texspecs)))

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
           (remove-listener pass pipeline))
  (setf (nodes pipeline) ())
  (setf (passes pipeline) #())
  (setf (textures pipeline) #())
  (setf (texspecs pipeline) #()))

(defmethod connect ((source flow:port) (target flow:port) (pipeline pipeline))
  (unless (find (flow:node source) (nodes pipeline))
    (enter (flow:node source) pipeline))
  (unless (find (flow:node target) (nodes pipeline))
    (enter (flow:node target) pipeline))
  (flow:connect source target 'flow:directed-connection)
  pipeline)

(defmethod check-consistent ((pipeline pipeline))
  (dolist (node (nodes pipeline))
    (check-consistent node)))

(defun texspec-real-size (texspec width height)
  (flet ((eval-size (size)
           (eval `(let ((width ,width)
                        (height ,height))
                    (declare (ignorable width height))
                    ,size))))
    (values (eval-size (getf texspec :width))
            (eval-size (getf texspec :height)))))

(defmethod resize ((pipeline pipeline) width height)
  (let ((width (max 1 width))
        (height (max 1 height)))
    (loop for texture across (textures pipeline)
          for texspec across (texspecs pipeline)
          do (multiple-value-bind (width height) (texspec-real-size texspec width height)
               (resize texture width height)))
    (loop for pass across (passes pipeline)
          for binding = (when (framebuffer pass) (first (attachments (framebuffer pass))))
          when binding ;; We have to do it like this to prevent updating FBOs with
                       ;; texspecs that are not window-size.
          do (setf (width (framebuffer pass)) (width (second binding)))
             (setf (height (framebuffer pass)) (height (second binding))))))

(defmethod normalized-texspec ((texspec list))
  (assert (= 0 (getf texspec :level 0)))
  (assert (eql :dynamic (getf texspec :storage :dynamic)))
  (let ((texspec (copy-list texspec)))
    (unless (getf texspec :width)
      (setf (getf texspec :width) 'width))
    (unless (getf texspec :height)
      (setf (getf texspec :height) 'height))
    (unless (getf texspec :target)
      (setf (getf texspec :target) :texture-2d))
    texspec))

(defmethod normalized-texspec ((port texture-port))
  (normalized-texspec (texspec port)))

(defmethod normalized-texspec ((port output))
  (normalized-texspec
   (append (texspec port)
           ;; Default internal format for attachments
           (case (attachment port)
             (:depth-attachment
              (list :internal-format :depth-component
                    :min-filter :nearest
                    :mag-filter :nearest))
             (:stencil-attachment
              (list :internal-format :stencil-index
                    :min-filter :nearest
                    :mag-filter :nearest))
             (:depth-stencil-attachment
              (list :internal-format :depth-stencil
                    :min-filter :nearest
                    :mag-filter :nearest))
             (T
              (list :internal-format :rgba
                    :min-filter :linear
                    :mag-filter :linear))))))

(defun allocate-textures (passes textures texspec)
  (flet ((kind (port)
           ;; FIXME: This is really dumb and inefficient. If we could remember which port belongs
           ;;        to which joined texspec instead it could be much better and wouldn't need to
           ;;        recompute everything all the time.
           (and (and (typep port 'flow:out-port))
                (join-texspec texspec (normalized-texspec port)))))
    (flow:allocate-ports passes :sort NIL :test #'kind :attribute :texid)
    (let* ((texture-count (loop for pass in passes
                                when (flow:ports pass)
                                maximize (loop for port in (flow:ports pass)
                                               when (and (flow:attribute port :texid)
                                                         (kind port))
                                               maximize (1+ (flow:attribute port :texid)))))
           (offset (length textures)))
      (adjust-array textures (+ offset texture-count) :initial-element NIL)
      (dolist (pass passes textures)
        (dolist (port (flow:ports pass))
          (when (kind port)
            ;; FIXME: Recompute the minimal upgraded texspec across all shared
            ;;        ports, as the partitioning done by the allocation mechanism
            ;;        might have broken up texspecs that were initially grouped.
            (let* ((texid (+ offset (flow:attribute port :texid)))
                   (texture (or (aref textures texid)
                                (apply #'make-instance 'texture texspec))))
              (setf (aref textures texid) texture)
              (setf (texture port) texture)
              (dolist (connection (flow:connections port))
                (setf (texture (flow:right connection)) texture)))))))))

(defmethod pack-pipeline ((pipeline pipeline) target)
  (check-consistent pipeline)
  (v:info :trial.pipeline "~a packing for ~a (~ax~a)" pipeline target (width target) (height target))
  (let* ((passes (flow:topological-sort (nodes pipeline)))
         (textures (make-array 0 :initial-element NIL :fill-pointer 0 :adjustable T)))
    ;; KLUDGE: We need to do the intersection here to ensure that we remove passes
    ;;         that are not part of this pipeline, but still connected to one of the
    ;;         passes that *is* part of the pipeline.
    (flet ((node-p (node) (find node (nodes pipeline))))
      (setf passes (remove-if-not #'node-p passes)))
    ;; Compute minimised texture set
    ;; (let ((texspecs (loop for port in (mapcan #'flow:ports passes)
    ;;                       when (and (typep port 'flow:out-port)
    ;;                                 (typep port 'texture-port))
    ;;                       collect (normalized-texspec port))))
    ;;   (dolist (texspec (join-texspecs texspecs))
    ;;     (allocate-textures passes textures texspec)))
    ;; Compute full texture set
    (dolist (port (mapcan #'flow:ports passes))
      (when (typep port '(and (or static-input flow:out-port) texture-port))
        (let ((texture (apply #'make-instance 'texture (normalized-texspec port))))
          (setf (texture port) texture)
          (dolist (connection (flow:connections port))
            (setf (texture (flow:right connection)) texture))
          (vector-push-extend texture textures))))
    ;; Extract texspecs
    (let ((texspecs (make-array (length textures))))
      (loop for i from 0 below (length textures)
            for texture = (aref textures i)
            for texspec = (texture-texspec texture)
            do (setf (aref texspecs i) texspec)
               (multiple-value-bind (width height) (texspec-real-size texspec (width target) (height target))
                 (setf (width texture) width)
                 (setf (height texture) height)))
      ;; Compute frame buffers
      (dolist (pass passes)
        (when (typep pipeline 'event-loop)
          (add-listener pass pipeline))
        (let ((output (loop for port in (flow:ports pass)
                            do (when (and (typep port 'output)
                                          (eql :color-attachment0 (attachment port)))
                                 (return port)))))
          (when output
            (flet ((dimension (func)
                     (funcall func (texture output))))
              (setf (framebuffer pass)
                    (make-instance 'framebuffer
                                   :width (dimension #'width)
                                   :height (dimension #'height)
                                   :attachments (loop for port in (flow:ports pass)
                                                      when (typep port 'output)
                                                      collect (list (attachment port) (texture port)))))))))
      ;; Now re-set the activation to short-modify the pipeline as necessary.
      (dolist (pass passes)
        (setf (active-p pass) (active-p pass)))
      ;; All done.
      (v:info :trial.pipeline "~a pass order: ~a" pipeline passes)
      (v:info :trial.pipeline "~a texture count: ~a" pipeline (length textures))
      (v:info :trial.pipeline "~a texture allocation: ~:{~%~a~:{~%    ~a: ~a~}~}" pipeline
              (loop for pass in passes
                    collect (list pass (loop for port in (flow:ports pass)
                                             collect (list (flow:name port) (texture port))))))
      ;; FIXME: Replace textures with existing ones if they match to save on re-allocation.
      ;; FIXME: When transitioning between scenes we should try to re-use existing textures
      ;;        and fbos to reduce the amount of unnecessary allocation. This is separate
      ;;        from the previous issue as the scenes typically have separate pipelines.
      (clear-pipeline pipeline)
      (setf (passes pipeline) (coerce passes 'vector))
      (setf (textures pipeline) textures)
      (setf (texspecs pipeline) texspecs))))

(defmethod render ((pipeline pipeline) target)
  (loop for pass across (passes pipeline)
        do (when (active-p pass)
             (render pass target))))

(defmethod blit-to-screen ((pipeline pipeline))
  (let ((passes (passes pipeline)))
    (loop for i downfrom (1- (length passes)) to 0
          for pass = (aref passes i)
          do (when (and (active-p pass) (framebuffer pass))
               (blit-to-screen pass)
               (return)))))

(defmethod stage ((pipeline pipeline) (area staging-area))
  (loop for texture across (textures pipeline)
        do (stage texture area))
  (loop for pass across (passes pipeline)
        do (stage pass area)))
