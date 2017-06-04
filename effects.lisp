#|
 This file is a part of trial
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial)

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

(define-shader-pass blend-pass (post-effect-pass)
  ("aPass" "bPass"))

(define-class-shader blend-pass :fragment-shader
  '(effects #p"blend.frag"))

(define-shader-pass high-pass-filter (post-effect-pass)
  ("previousPass"))

(define-class-shader high-pass-filter :fragment-shader
  '(effects #p"high-pass-filter.frag"))

(define-shader-pass low-pass-filter (post-effect-pass)
  ("previousPass"))

(define-class-shader low-pass-filter :fragment-shader
  '(effects #p"low-pass-filter.frag"))
