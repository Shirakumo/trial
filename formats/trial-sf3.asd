(asdf:defsystem trial-sf3/image
  :components ((:file "sf3/image"))
  :depends-on (:trial :cl-sf3/image))

(asdf:defsystem trial-sf3
  :depends-on (:trial-sf3/image))
