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
  (setf (nodes pipeline) ())
  (setf (passes pipeline) #())
  (setf (textures pipeline) #()))

(defmethod connect ((source flow:port) (target flow:port) (pipeline pipeline))
  (unless (find (flow:node source) (passes pipeline))
    (register (flow:node source) pipeline))
  (unless (find (flow:node target) (passes pipeline))
    (register (flow:node target) pipeline))
  (flow:connect source target 'flow:directed-connection)
  pipeline)

(defmethod check-consistent ((pipeline pipeline))
  (dolist (pass (passes pipeline))
    (dolist (port (flow:ports pass))
      (when (and (typep port 'input)
                 (null (flow:connections port))) 
        (error "Pipeline is not consistent.~%~
                Pass ~s is missing a connection to its input ~s."
               pass port)))))

(defun allocate-textures (passes width height)
  (flet ((texpsec (port)
           (list NIL width height (cffi:null-pointer)
                 (case (attachment port)
                   (:depth-attachment :depth-component)
                   (:depth-stencil-attachment :depth-stencil)
                   (T :rgba)))))
    (let* ((texture-count (1+ (loop for pass in passes
                                    when (flow:ports pass)
                                    maximize (loop for port in (flow:ports node)
                                                   when (flow:attribute port :color)
                                                   maximize (flow:attribute port :color)))))
           (textures (make-array texture-count :inital-element NIL)))
      (dolist (pass passes textures)
        (loop for port in (flow:ports pass)
              for color = (flow:attribute port :color)
              do (when color
                   (unless (aref textures color)
                     (setf (aref textures color)
                           (make-asset 'texture-asset :input (texpsec port))))
                   (setf (flow:attribute port 'texture) (aref textures color))))))))

;; FIXME: resizing
(defmethod pack-pipeline ((pipeline pipeline) target)
  (check-consistent pipeline)
  (v:info :trial.pipeline "~a packing for ~a" pipeline target)
  ;; FIXME: How to ensure algorithm distinguishes depth and colour buffers?
  (let* ((passes (flow:allocate-ports (nodes pipeline)))
         (textures (allocate-textures passes (width target) (height target))))
    (v:info :trial.pipeline "~a pass order: ~a" pipeline passes)
    (v:info :trial.pipeline "~a texture count: ~a" pipeline (length textures))
    (v:info :trial.pipeline "~a texture allocation: ~{~%~a~{ ~a: ~a~}~}"
            (loop for pass in passes
                  collect (list pass (loop for port in (flow:ports pass)
                                           when (typep port 'output)
                                           collect (list (flow:name port)
                                                         (flow:attribute port :color))))))
    (dolist (pass passes)
      (setf (framebuffer pass)
            (make-asset 'framebuffer-asset
                        :input (loop for port in (flow:ports pass)
                                     when (typep port 'output)
                                     collect (list (flow:attribute port 'texture)
                                                   :attachment (attachment port))))))
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
