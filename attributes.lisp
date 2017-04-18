#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(defvar *gl-attributes* #(:blend
                          :clip-distance0
                          :clip-distance1
                          :color-logic-op
                          :cull-face
                          :debug-output
                          :debug-output-synchronous
                          :depth-clamp
                          :depth-test
                          :dither
                          :framebuffer-srgb
                          :line-smooth
                          :multisample
                          :polygon-offset-fill
                          :polygon-offset-line
                          :polygon-offset-point
                          :polygon-smooth
                          :primitive-restart
                          :primitive-restart-fixed-index
                          :rasterizer-discard
                          :sample-alpha-to-coverage
                          :sample-alpha-to-one
                          :sample-coverage
                          :sample-shading
                          :sample-mask
                          :scissor-test
                          :stencil-test
                          :texture-cube-map-seamless
                          :program-point-size))

(defvar *default-enabled-gl-attributes* #(:dither :multisample))

(defun make-attribute-table (&optional parent)
  (let ((table (make-hash-table :test 'eq :size (length *gl-attributes*))))
    (if parent
        (loop for k being the hash-keys of parent
              for v being the hash-values of parent
              do (setf (gethash k table) v))
        (reset-attributes table))
    table))

(defun reset-attributes (&optional (table (attribute-table)))
  (loop for k across *gl-attributes*
        do (if (find k *default-enabled-gl-attributes*)
               (setf (gethash k table) T)
               (setf (gethash k table) NIL))))

(defvar *attribute-stack* (list (make-attribute-table)))

(defun attribute-table ()
  (first *attribute-stack*))

(defun enable (&rest attributes)
  (let ((table (attribute-table)))
    (dolist (attrib attributes)
      (unless (gethash attrib table)
        (gl:enable attrib)
        (setf (gethash attrib table) T)))))

(defun disable (&rest attributes)
  (let ((table (attribute-table)))
    (dolist (attrib attributes)
      (when (gethash attrib table)
        (gl:enable attrib)
        (setf (gethash attrib table) NIL)))))

(defun push-attribs (&optional (table (make-attribute-table (attribute-table))))
  (push table *attribute-stack*))

(defun pop-attribs ()
  (let ((prev (pop *attribute-stack*))
        (cur (attribute-table)))
    (loop for k being the hash-keys of prev
          for v being the hash-values of prev
          do (cond ((and v (not (gethash k cur)))
                    (gl:disable k))
                   ((and (not v) (gethash k cur))
                    (gl:enable k))))))

(defmacro with-pushed-attribs (&body body)
  `(progn (push-attribs)
          (unwind-protect
               (progn ,@body)
            (pop-attribs))))
