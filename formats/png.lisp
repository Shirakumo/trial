(in-package #:org.shirakumo.fraf.trial)

(defmethod report-on-error ((error pngload::png-error))
  (emessage "Failed to access the file ~a. The file does not exist, cannot be accessed, or is corrupted."
            (pngload::path (pngload:png error))))

(defun %png-source (png)
  (make-image-source (pngload:data png) (pngload:width png) (pngload:height png)
                     (infer-pixel-type (max 8 (pngload:bit-depth png)) :unsigned)
                     (ecase (pngload:color-type png)
                       (:greyscale :red)
                       (:greyscale-alpha :rg)
                       (:truecolour :rgb)
                       (:truecolour-alpha :rgba)
                       (:indexed-colour
                        ;; This fucking sucks, man.
                        (ecase (truncate (length (pngload:data png))
                                         (* (pngload:width png) (pngload:height png)))
                          (4 :rgba)
                          (3 :rgb)
                          (2 :rg)
                          (1 :red))))))

(defmethod load-image ((source vector) (type (eql :png)))
  (handler-bind ((pngload::png-warning #'muffle-warning))
    (%png-source (pngload:load-vector source :flatten T :flip-y T :static-vector (static-vector-p source)))))

(defmethod load-image ((source pathname) (type (eql :png))) 
  (handler-bind ((pngload::png-warning #'muffle-warning))
    (%png-source (pngload:load-file source :flatten T :flip-y T))))

(defmethod load-image ((source memory-region) (type (eql :png)))
  (let ((png (pngload::make-png))
        (state (pngload::make-state :decode-data T
                                    :flatten T
                                    :flip-y T
                                    :use-static-vector NIL
                                    :unknown-chunk-warnings NIL)))
    (setf (pngload::state png) state)
    (3bz:with-octet-pointer (pointer-binding (memory-region-pointer source)
                                             (memory-region-size source))
      (let ((source (make-instance 'pngload::octet-pointer-source
                                   :data (memory-region-pointer source)
                                   :end (memory-region-size source))))
        (handler-bind ((pngload::png-warning #'muffle-warning))
          (setf (pngload::state-source state) source
                (pngload::state-mmap-pointer state) pointer-binding
                (pngload::parse-tree png) (pngload::parse-datastream png)))))
    (%png-source png)))

(defmethod save-image ((source texture-source) (path pathname) (type (eql :png)) &key)
  (let ((channels (ecase (pixel-format source) (:rgba 4) (:rgb 3) (:rg 2) (:r 1) ((NIL) 3))))
    (destructuring-bind (x y z w h d) (texture-source-src source)
      (declare (ignore x y z d))
      (zpng:write-png (make-instance 'zpng:png 
                                     :color-type (ecase channels
                                                   (4 :truecolor-alpha)
                                                   (3 :truecolor)
                                                   (2 :grayscale-alpha)
                                                   (1 :grayscale))
                                     :width w :height h
                                     :image-data (flip-image-vertically (pixel-data source) w h channels))
                      path))))

(defmethod transcode (source (source-type symbol) target (target-type (eql :png)) &rest args &key &allow-other-keys)
  (apply #'save-image (load-image source source-type) target target-type args)
  (ignore-errors (run "optipng" "-o" "5" "-clobber" "-out" target target)))
