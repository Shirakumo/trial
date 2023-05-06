(asdf:defsystem "gjk-test"
  :author "Selwyn Simsek"
  :license "zlib"
  :depends-on ("trial"
               "parachute")
  :components ((:file "gjk-test"))
  :description "Tests for the implementation of the GJK algorithm"
  :perform (test-op (op c) (uiop:symbol-call :parachute :test :trial/tests/gjk)))
