#|
 This file is a part of trial
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

(define-shader-entity image-renderer (standalone-shader-entity)
  ())

(defmethod stage :after ((entity image-renderer) (area staging-area))
  (stage (// 'trial 'fullscreen-square) area))

(defmethod render ((entity image-renderer) (program shader-program))
  (declare (optimize speed))
  (let* ((vao (vertex-array entity))
         (size (size vao)))
    (declare (type (unsigned-byte 32) size))
    (gl:bind-vertex-array (gl-name vao))
    (if (indexed-p vao)
        (%gl:draw-elements (vertex-form vao) size :unsigned-int 0)
        (%gl:draw-arrays (vertex-form vao) 0 size))
    (gl:bind-vertex-array 0)))

(defmethod render ((renderer image-renderer) (texture texture))
  (let ((fbo (gl:gen-framebuffer)))
    (gl:bind-framebuffer :framebuffer fbo)
    (unwind-protect
         (progn
           (gl:viewport 0 0 (width texture) (height texture))
           (%gl:framebuffer-texture :framebuffer :color-attachment0 (gl-name texture) 0)
           (render renderer NIL))
      (gl:bind-framebuffer :framebuffer 0)
      (gl:delete-framebuffers (list fbo)))))

(define-shader-entity dynamic-image-renderer (image-renderer dynamic-shader-entity)
  ())

(defclass shader-image-generator (resource-generator)
  ())

(defmethod generate-resources ((generator shader-image-generator) input &rest texture-args &key (resource (resource generator T)) (texture-class 'texture) &allow-other-keys)
  (let* ((loader (make-instance 'loader))
         (renderer (make-instance 'dynamic-image-renderer :shaders (resolve-shader-include input))))
    (apply #'ensure-instance resource texture-class (remf* texture-args :resource :texture-class))
    (with-cleanup-on-failure (finalize loader)
      (commit (list renderer resource) loader)
      (render renderer resource)
      (commit (list resource) loader :unload T))))

(defclass shader-image (single-resource-asset
                        shader-image-generator)
  ())

(defmethod generate-resources :around ((image shader-image) input &key)
  (let* ((*default-pathname-defaults* (pool-path (pool image) NIL)))
    (call-next-method)))
