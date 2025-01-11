(in-package #:org.shirakumo.fraf.trial)

(defclass pipeline ()
  ((nodes :initform NIL :accessor nodes)
   (passes :initform #() :accessor passes)
   (textures :initform #() :accessor textures)
   (texspecs :initform #() :accessor texspecs)))

(defmethod finalize ((pipeline pipeline))
  (clear pipeline))

(defmethod flow:sever ((pipeline pipeline))
  (loop for pass across (passes pipeline)
        do (flow:sever pass))
  (setf (nodes pipeline) NIL))

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
           (flow:sever pass)
           (remove-listener pass pipeline))
  (setf (nodes pipeline) ())
  (setf (passes pipeline) #())
  (setf (textures pipeline) #())
  (setf (texspecs pipeline) #()))

(defmethod connect ((source flow:port) (target flow:port) (pipeline pipeline))
  (flet ((enter-if-new (node)
           (unless (find node (nodes pipeline))
             (if (eq pipeline (container node))
                 (pushnew node (nodes pipeline))
                 (enter node pipeline)))))
    (enter-if-new (flow:node source))
    (enter-if-new (flow:node target)))
  (flow:connect source target 'flow:directed-connection)
  pipeline)

(defmacro connect* (pipeline &body parts)
  (let ((data (loop for data in parts
                    collect (list* (gensym "PASS") (enlist data))))
        (pipeg (gensym "PIPELINE")))
    `(let ((,pipeg ,pipeline)
           ,@(loop for (gens val) in data collect `(,gens ,val)))
       ,@(if (rest data)
             (loop for ((a a_ ai_ ao) (b b_ bi bo_)) on data
                   while b
                   collect `(connect (port ,a ',(or ao 'color)) (port ,b ',(or bi 'previous-pass)) ,pipeg))
             `((enter ,(caar data) ,pipeg)))
       ,pipeg)))

(defmacro construct-pipeline (pipeline passes &body connections)
  (let ((pipelineg (gensym "PIPELINE")))
    (labels ((process-connection (connection)
               (case (car connection)
                 (:if
                  (destructuring-bind (test then else) (rest connection)
                    `(cond (,test
                            ,@(process-connections then))
                           (T
                            ,@(process-connections else)))))
                 (:case
                  (destructuring-bind (test . cases) (rest connection)
                    `(case ,test
                       ,@(loop for (case . then) in cases
                               collect `(,case ,@(process-connections then))))))
                 (:when
                     (destructuring-bind (test . then) (rest connection)
                       `(when ,test
                          ,@(process-connections then))))
                 (:unless
                     (destructuring-bind (test . then) (rest connection)
                       `(unless ,test
                          ,@(process-connections then))))
                 (T
                  (let* ((sequence (loop for a in connection until (keywordp a) collect a))
                         (kargs (loop for a = (car connection) until (keywordp a) while connection do (pop connection)))
                         (body (loop for a in sequence
                                     for b in (rest sequence)
                                     for (a-pass a_ a-port) = (enlist a NIL 'color)
                                     for (b-pass b-port b_) = (enlist b 'previous-pass NIL)
                                     collect `(connect (port ,a-pass ',a-port) (port ,b-pass ',b-port) ,pipelineg))))
                    (if (getf kargs :when)
                        `(when ,(getf kargs :when) ,@body)
                        `(progn ,@body))))))
             (process-connections (connections)
               (loop for connection in connections
                     collect (process-connection connection))))
      `(let* ((,pipelineg ,pipeline)
              ,@(loop for pass in passes
                      for (type . args) = (enlist pass)
                      for name = (or (unquote (getf args :name)) type)
                      collect `(,name (ensure-instance (node ',name ,pipelineg) ,(if (listp type) type `',type)
                                                       :name ',name ,@args))))
         (flow:sever ,pipelineg)
         ,@(process-connections connections)))))

(defmethod check-consistent ((pipeline pipeline))
  (dolist (node (nodes pipeline))
    (check-consistent node)))

(defun texspec-real-size (texspec width height &optional depth)
  (flet ((eval-size (size)
           (when size
             (let ((res (eval `(let ((width ,width)
                                     (height ,height)
                                     (depth ,depth))
                                 (declare (ignorable width height depth))
                                 ,size))))
               (when res (ceiling res))))))
    (values (eval-size (getf texspec :width))
            (eval-size (getf texspec :height))
            (eval-size (getf texspec :depth)))))

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
    (unless (getf texspec :depth)
      (setf (getf texspec :depth) 'depth))
    (unless (getf texspec :target)
      (setf (getf texspec :target) :texture-2d))
    texspec))

(defmethod normalized-texspec ((port texture-port))
  (let ((spec (copy-list (texspec port))))
    (when (getf spec :include)
      (let ((port (flow:port (flow:node port) (getf spec :include))))
        (remf spec :include)
        (setf spec (nconc spec (texspec (texture port))))))
    (normalized-texspec spec)))

(defmethod normalized-texspec ((port output))
  (normalized-texspec
   (append (call-next-method)
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

(defun texture-texspec-matches-p (texture texspec width height)
  (and (eq (internal-format texture) (getf texspec :internal-format))
       (eq (target texture) (getf texspec :target))
       (multiple-value-bind (w h) (texspec-real-size texspec width height)
         (and (= w (width texture))
              (= h (height texture))))
       (eq (min-filter texture) (getf texspec :min-filter))
       (eq (mag-filter texture) (getf texspec :mag-filter))))

(defmethod make-port-texture ((port port) width height &optional depth texture)
  (let* ((texspec (normalized-texspec port))
         (texture (if texture
                      (apply #'reinitialize-instance texture texspec)
                      (apply #'make-instance 'texture texspec))))
    (multiple-value-bind (width height depth) (texspec-real-size texspec width height depth)
      (setf (width texture) width)
      (setf (height texture) height)
      (setf (depth texture) depth))
    texture))

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

(defmethod pack-pipeline ((pass shader-pass) width height)
  ;; Allocate port textures
  (dolist (port (flow:ports pass))
    (when (typep port '(and (or static-input flow:out-port) texture-port))
      (setf (texture port) (make-port-texture port width height))))
  (setf (framebuffer pass) (make-pass-framebuffer pass))
  pass)

(defmethod pack-pipeline ((pipeline pipeline) width height)
  (check-consistent pipeline)
  (v:info :trial.pipeline "~a packing for ~ax~a" pipeline width height)
  (let* ((passes (flow:topological-sort (nodes pipeline)))
         (existing-textures (textures pipeline))
         (textures (make-array 0 :initial-element NIL :fill-pointer 0 :adjustable T))
         (texspecs (make-array 0 :initial-element NIL :fill-pointer 0 :adjustable T)))
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
        (let* ((texspec (normalized-texspec port))
               (texture (loop for texture across existing-textures
                              do (when (and (not (find texture textures))
                                            (texture-texspec-matches-p texture texspec width height))
                                   (return texture))
                              finally (return (make-port-texture port width height)))))
          ;; If we're dynamically updating then setting the texture now
          ;; will require it to be allocated to be bound...
          (dolist (connection (flow:connections port))
            (setf (slot-value (flow:right connection) 'texture) texture))
          (when (and (framebuffer (flow:node port))
                     (allocated-p (framebuffer (flow:node port)))
                     (not (allocated-p texture)))
            (allocate texture))
          (setf (slot-value port 'texture) texture)
          (vector-push-extend texture textures)
          (vector-push-extend texspec texspecs))))
    ;; Compute frame buffers
    (dolist (pass passes)
      (when (typep pipeline 'event-loop)
        (add-listener pass pipeline))
      (let ((fbo (make-pass-framebuffer pass)))
        (cond ((not (framebuffer pass))
               (setf (framebuffer pass) fbo))
              ((not (allocated-p (framebuffer pass)))
               (setf (attachments (framebuffer pass)) (attachments fbo)))
              (T
               (dolist (attachment (attachments fbo))
                 (unless (allocated-p (second attachment))
                   (allocate (second attachment))))
               (setf (width (framebuffer pass)) NIL)
               (setf (height (framebuffer pass)) NIL)
               (setf (attachments (framebuffer pass)) (attachments fbo))))))
    ;; Now re-set the activation to short-modify the pipeline as necessary.
    (dolist (pass passes)
      (setf (active-p pass) (active-p pass)))
    ;; All done.
    (v:debug :trial.pipeline "~a pass order: ~a" pipeline passes)
    (v:debug :trial.pipeline "~a texture count: ~a" pipeline (length textures))
    (v:info :trial.pipeline "~a texture allocation: ~:{~%~a~:{~%    ~a: ~a~}~}" pipeline
             (loop for pass in passes
                   collect (list pass (loop for port in (flow:ports pass)
                                            collect (list (flow:name port) (texture port))))))
    (loop for pass across (passes pipeline)
          do (unless (find pass passes)
               (leave pass pipeline)
               (flow:sever pass)
               (finalize pass)))
    (loop for texture across (textures pipeline)
          do (unless (find texture textures)
               (finalize texture)))
    (setf (nodes pipeline) NIL)
    (setf (passes pipeline) (coerce passes 'vector))
    (setf (textures pipeline) textures)
    (setf (texspecs pipeline) texspecs)))

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
