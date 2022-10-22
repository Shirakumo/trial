(asdf:defsystem :trial-animation
  :depends-on (:trial-glfw
               :trial-png
               :trial-gltf)
  :components ((:file "workbench")))
