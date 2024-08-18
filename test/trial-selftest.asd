(asdf:defsystem trial-selftest
  :serial T
  :components ((:file "selftest"))
  :depends-on (:trial
               :trial-harmony))
