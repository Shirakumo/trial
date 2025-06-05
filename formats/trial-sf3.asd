(asdf:defsystem trial-sf3/image
  :components ((:file "sf3/image"))
  :depends-on (:trial :cl-sf3/image))

(asdf:defsystem trial-sf3/model
  :components ((:file "sf3/model"))
  :depends-on (:trial :cl-sf3/model))

(asdf:defsystem trial-sf3/physics-model
  :components ((:file "sf3/physics-model"))
  :depends-on (:trial :cl-sf3/physics-model))

(asdf:defsystem trial-sf3
  :depends-on (:trial-sf3/image
               :trial-sf3/model
               :trial-sf3/physics-model))
