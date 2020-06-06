#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

;; LATIN-1
(defparameter *default-charset*
  " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~
¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ")

(defclass font-atlas (texture)
  ((file :initarg :file :initform NIL :accessor file)
   (handle :initform NIL :accessor cl-fond:handle)
   (charset :initarg :charset :reader charset)
   (index :initarg :index :reader index)
   (size :initarg :size :reader size)
   (oversample :initarg :oversample :reader oversample)
   (fit-size :initarg :fit-size :reader fit-size))
  (:default-initargs
   :charset *default-charset*
   :index 0
   :size 24
   :oversample NIL
   :width 64
   :height 64
   :fit-size T
   :internal-format :r8
   :min-filter :linear-mipmap-linear
   :mag-filter :linear
   :wrapping :clamp-to-border))

(defmethod allocate ((font font-atlas))
  (let ((handle (cl-fond::calloc '(:struct cl-fond-cffi:font)))
        (file (uiop:native-namestring (file font))))
    (with-cleanup-on-failure (deallocate font)
      (setf (cl-fond:handle font) handle)
      (setf (cl-fond-cffi:font-file handle) (cffi:foreign-string-alloc file :encoding :utf-8))
      (setf (cl-fond-cffi:font-codepoints handle) (cffi:foreign-string-alloc (charset font) :encoding :utf-32))
      (setf (cl-fond-cffi:font-size handle) (coerce (size font) 'single-float))
      (setf (cl-fond-cffi:font-oversample handle) (or (oversample font) 0))
      (with-accessors ((width width) (height height)) font
        (let ((max (if (fit-size font)
                       (cl-opengl:get* :max-texture-size)
                       (min width height))))
          (call-next-method)
          (gl:bind-texture :texture-2d (gl-name font))
          (gl:tex-parameter :texture-2d :texture-lod-bias -0.65)
          (loop (setf (cl-fond-cffi:font-width handle) width)
                (setf (cl-fond-cffi:font-height handle) height)
                (setf (cl-fond-cffi:font-atlas handle) (gl-name font))
                (if (cl-fond-cffi:load-font handle)
                    (return)
                    (let ((code (cl-fond-cffi:fond-error)))
                      (cond ((and (eql code :font-pack-failed)
                                  (or (< width max) (< height max)))
                             (if (< width height)
                                 (setf width (* 2 width))
                                 (setf height (* 2 height)))
                             (allocate-texture-storage font))
                            (T
                             (cl-fond::show-error)))))))))))

(defmethod deallocate ((font font-atlas))
  (call-next-method)
  (let ((handle (cl-fond:handle font)))
    (unless (cffi:null-pointer-p (cl-fond-cffi:font-file handle))
      (cffi:foreign-string-free (cl-fond-cffi:font-file handle)))
    (unless (cffi:null-pointer-p (cl-fond-cffi:font-codepoints handle))
      (cffi:foreign-string-free (cl-fond-cffi:font-codepoints handle)))
    (cl-fond-cffi:free-font handle)
    (cffi:foreign-free handle)))

(defmethod text-extent ((font font-atlas) text)
  (if (allocated-p font)
      (cl-fond:compute-extent font text)
      '(:l 0 :r 0 :t 0 :b 0 :gap 0)))
