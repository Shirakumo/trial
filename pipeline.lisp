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
(defun allocate-textures (passes textures kind width height)
  (flow:allocate-ports passes :sort NIL :test kind)
  (flet ((texpsec (port)
           (list NIL width height
                 (case (attachment port)
                   (:depth-attachment :depth-component)
                   (:depth-stencil-attachment :depth-stencil)
                   (T :rgba)))))
    (let* ((texture-count (loop for pass in passes
                                when (flow:ports pass)
                                maximize (loop for port in (flow:ports pass)
                                               when (and (flow:attribute port :color)
                                                         (funcall kind port))
                                               maximize (1+ (flow:attribute port :color)))))
           (offset (length textures)))
      (adjust-array textures (+ offset texture-count) :initial-element NIL)
      (dolist (pass passes textures)
        (loop for port in (flow:ports pass)
              do (when (funcall kind port)
                   (let ((color (+ offset (flow:attribute port :color))))
                     (unless (aref textures color)
                       (setf (aref textures color)
                             (load (apply #'make-instance 'texture (texpsec port)))))
                     (setf (texture port) (aref textures color))
                     (dolist (connection (flow:connections port))
                       (setf (texture (flow:right connection)) (aref textures color))))))))))

(defun %color-port-p (port)
  (and (typep port 'output)
       (not (eql :depth-attachment (attachment port)))
       (not (eql :depth-stencil-attachment (attachment port)))))

(defun %depth-port-p (port)
  (and (typep port 'output)
       (eql :depth-attachment (attachment port))))

(defun %depth-stencil-port-p (port)
  (and (typep port 'output)
       (eql :depth-stencil-attachment (attachment port))))

(defmethod resize ((pipeline pipeline) width height)
  (gl:scissor 0 0 width height)
  (loop for texture across (textures pipeline)
        do (resize texture width height)))

(defmethod pack-pipeline ((pipeline pipeline) target)
  (check-consistent pipeline)
  (v:info :trial.pipeline "~a packing for ~a (~ax~a)" pipeline target (width target) (height target))
  ;; FIXME: How to ensure algorithm distinguishes depth and colour buffers?
  (let* ((passes (flow:topological-sort (nodes pipeline)))
         (textures (make-array 0 :initial-element NIL :adjustable T)))
    (clear-pipeline pipeline)
    (allocate-textures passes textures #'%color-port-p (width target) (height target))
    (allocate-textures passes textures #'%depth-port-p (width target) (height target))
    (allocate-textures passes textures #'%depth-stencil-port-p (width target) (height target))
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
