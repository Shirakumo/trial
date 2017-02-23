
(define-asset shader primitive-vert (trial)
  :file "primitive.vert")
(define-asset shader primitive-frag (trial)
  :file "primitive.frag")
(define-asset shader-program primitive (trial)
  :shaders '((trial primitive-vert)
             (trial primitive-frag)))

(define-asset vertex-buffer triangle (trial)
  :buffer-type :array-buffer
  :buffer-data '(-0.5 -0.5 0.0
                 +0.5 -0.5 0.0
                 +0.0 +0.5 0.0))
(define-asset vertex-array triangle (trial)
  :buffers '((trial triangle)))

(define-asset vertex-buffer square (trial)
  :buffer-type :array-buffer
  :buffer-data '(-0.5 -0.5 0.0
                 +0.5 -0.5 0.0
                 +0.5 +0.5 0.0
                 -0.5 +0.5 0.0))
(define-asset vertex-buffer square-tex (trial)
  :buffer-type :array-buffer
  :buffer-data '(1.0 1.0
                 0.0 1.0
                 0.0 0.0
                 1.0 0.0))
(define-asset vertex-array square (trial)
  :buffers '((trial square)
             (trial square-tex :size 2)))

(defmethod setup-scene ((main main))
  (let ((scene (scene main)))
    ;;(enter (make-instance 'skybox) scene)
    ;;(enter (make-instance 'space-axes) scene)
    ;;(enter (make-instance 'player) scene)
    ;;(enter (make-instance 'following-camera :name :camera :target (unit :player scene)) scene)
    ;;(enter (make-instance 'selection-buffer :name :selection-buffer) scene)
    ))

(defmethod paint ((source main) (target main))
  (issue (scene target) 'tick)
  (process (scene target))
  ;; (paint (scene source) target)
  ;; (paint (hud source) target)
  (let ((shader (get-resource 'shader-program :trial 'primitive))
        (square (get-resource 'vertex-array :trial 'square))
        (texture (get-resource 'texture :trial 'cat)))
    (gl:bind-texture :texture-2d (data texture))
    (gl:use-program (data shader))
    (gl:bind-vertex-array (data square))
    (dotimes (i 100)
      (setf (uniform shader :transform)
            (marr (nmrotate (mtranslation (vec (cos i) (sin i) 0))
                            +vz+ (sin i))))
      (gl:draw-arrays :quads 0 4))
    (gl:use-program 0)))
