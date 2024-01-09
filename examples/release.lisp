(org.shirakumo.fraf.trial.release:configure
 :build (:features (:trial-release)
         :prune ("pool/**/*.*~"
                 "pool/**/*.kra"
                 "pool/**/*.ttf"
                 "pool/**/#*#"
                 "pool/**/*.blend*"
                 "pool/pool/*/"))
 :depots (:linux ("*.so" "trial-examples.run")
          :windows ("*.dll" "trial-examples.exe")
          :macos ("*.dylib" "trial-examples.o")
          :content ("pool/"))
 :bundles (:linux (:depots (:linux :content))
           :windows (:depots (:windows :content))
           :macos (:depots (:macos :content))
           :all (:depots (:linux :windows :macos :content)))
 :upload (:targets #())
 :bundle (:targets #(:windows :linux))
 :system "trial-examples")
