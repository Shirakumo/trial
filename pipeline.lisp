#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(defclass pipeline (entity event-loop)
  ((nodes :initform NIL :accessor nodes)
   (passes :initform #() :accessor passes)
   (textures :initform #() :accessor textures)))

(defmethod handle :after ((tick tick) (pipeline pipeline))
  (process pipeline))

(defmethod load progn ((pipeline pipeline))
  (map NIL #'load (textures pipeline))
  (map NIL #'load (passes pipeline)))

(defmethod finalize ((pipeline pipeline))
  (clear pipeline))

(defmethod register ((pass shader-pass) (pipeline pipeline))
  (pushnew pass (nodes pipeline))
  (add-handler pass pipeline))

(defmethod deregister ((pass shader-pass) (pipeline pipeline))
  (setf (nodes pipeline) (delete pass (nodes pipeline)))
  (remove-handler pass pipeline))

(defmethod clear ((pipeline pipeline))
  (loop for tex across (textures pipeline)
        do (finalize tex))
  (loop for pass across (passes pipeline)
        do (when (framebuffer pass)
             (finalize (framebuffer pass))
             (setf (framebuffer pass) NIL)))
  (setf (nodes pipeline) ())
  (setf (passes pipeline) #())
  (setf (textures pipeline) #()))

(defmethod connect ((source flow:port) (target flow:port) (pipeline pipeline))
  (unless (find (flow:node source) (nodes pipeline))
    (register (flow:node source) pipeline))
  (unless (find (flow:node target) (nodes pipeline))
    (register (flow:node target) pipeline))
  (flow:connect source target 'flow:directed-connection)
  pipeline)

(defmethod check-consistent ((pipeline pipeline))
  (dolist (node (nodes pipeline))
    (dolist (port (flow:ports node))
      (when (and (typep port 'input)
                 (null (flow:connections port))) 
        (error "Pipeline is not consistent.~%~
                Pass ~s is missing a connection to its input ~s."
               node port)))))

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
                             (load (make-asset 'texture-asset (list (texpsec port))))))
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
  (loop for texture across (textures pipeline)
        do (resize texture width height)))

(defmethod pack-pipeline ((pipeline pipeline) target)
  (check-consistent pipeline)
  (v:info :trial.pipeline "~a packing for ~a (~ax~a)" pipeline target (width target) (height target))
  ;; FIXME: How to ensure algorithm distinguishes depth and colour buffers?
  (let* ((passes (flow:topological-sort (nodes pipeline)))
         (textures (make-array 0 :initial-element NIL :adjustable T)))
    (clear pipeline)
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
      (setf (framebuffer pass)
            (load (make-asset 'framebuffer-asset
                              (loop for port in (flow:ports pass)
                                    when (typep port 'output)
                                    collect (list (texture port) :attachment (attachment port)))))))
    (setf (passes pipeline) (coerce passes 'vector))
    (setf (textures pipeline) textures)))

(defmethod paint ((pipeline pipeline) target)
  (loop for pass across (passes pipeline)
        for fbo = (framebuffer pass)
        do (gl:bind-framebuffer :framebuffer (resource fbo))
           (gl:clear :color-buffer :depth-buffer)
           (paint pass target)
        finally (gl:bind-framebuffer :draw-framebuffer 0)
                (%gl:blit-framebuffer 0 0 (width target) (height target) 0 0 (width target) (height target)
                                      (cffi:foreign-bitfield-value '%gl::ClearBufferMask :color-buffer)
                                      (cffi:foreign-enum-value '%gl:enum :nearest))))
