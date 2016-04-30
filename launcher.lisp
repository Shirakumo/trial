#|
 This file is a part of trial
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(define-widget launcher (QDialog)
  ())

(define-subwidget (launcher resolution) (q+:make-qcombobox launcher)
  (let ((primary (find-if #'cl-monitors:primary-p (cl-monitors:detect))))
    (dolist (mode (cl-monitors:modes primary))
      (q+:add-item resolution
                   (format NIL "~ax~a @ ~a Hz"
                           (cl-monitors:width mode)
                           (cl-monitors:height mode)
                           (cl-monitors:refresh mode))))))

(define-subwidget (launcher vsync) (q+:make-qcombobox launcher)
  (q+:add-items vsync '("Off" "Adaptive" "Strict")))

(define-subwidget (launcher samples) (q+:make-qcombobox launcher)
  (let ((max (with-context (*main*)
               (gl:get* :max-samples))))
    (q+:add-item samples "Off")
    (loop for s = 2 then (* 2 s)
          while (<= s max)
          do (q+:add-item samples (format NIL "~ax" s)))))

(define-subwidget (launcher filtering) (q+:make-qcombobox launcher)
  (q+:add-items filtering '("Nearest" "Linear" "Bilinear" "Trilinear"))
  (let ((max (with-context (*main*)
               (when (gl:extension-present-p "GL_EXT_texture_filter_anisotropic")
                 (gl:get-float :max-texture-max-anisotropy-ext 1)))))
    (loop for s = 2 then (* 2 s)
          while (<= s max)
          do (q+:add-item filtering (format NIL "~ax Anisotropic" s)))))

(define-subwidget (launcher fov) (make-instance 'qui:slider :minimum 10 :maximum 200 :stepping 1
                                                            :default 75))

(define-subwidget (launcher ok) (q+:make-qpushbutton "Ok" launcher)
  (connect! ok (clicked) launcher (accept)))

(define-subwidget (launcher cancel) (q+:make-qpushbutton "Cancel" launcher)
  (connect! cancel (clicked) launcher (reject)))

(define-subwidget (launcher layout) (q+:make-qformlayout launcher)
  (q+:add-row layout "Resolution" resolution)
  (q+:add-row layout "VSync" vsync)
  (q+:add-row layout "Antialiasing" samples)
  (q+:add-row layout "Filtering" filtering)
  (q+:add-row layout "FOV" fov)
  (q+:add-row layout cancel ok))
