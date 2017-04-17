#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)
(in-readtable :qtools)

(define-pool effects
  :base 'trial)

(define-shader-pass negative-pass (post-effect-pass)
  ("previousPass"))

(define-class-shader negative-pass :fragment-shader
  '(effects #p"negative.frag"))

(define-shader-pass grayscale-pass (post-effect-pass)
  ("previousPass"))

(define-class-shader grayscale-pass :fragment-shader
  '(effects #p"gray-filter.frag"))

(define-shader-pass box-blur-pass (post-effect-pass)
  ("previousPass"))

(define-class-shader box-blur-pass :fragment-shader
  '(effects #p"box-blur.frag"))

(define-shader-pass sobel-pass (post-effect-pass)
  ("previousPass"))

(define-class-shader sobel-pass :fragment-shader
  '(effects #p"sobel.frag"))

(define-shader-pass gaussian-blur-pass (post-effect-pass)
  ("previousPass"))

(define-class-shader gaussian-blur-pass :fragment-shader
  '(effects #p"gaussian.frag"))

(define-shader-pass fxaa-pass (post-effect-pass)
  ("previousPass"))

(define-class-shader fxaa-pass :fragment-shader
  '(effects #p"fxaa.frag"))
