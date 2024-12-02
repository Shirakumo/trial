(ql:quickload '(trial-alloy trial-steam trial-feedback trial-harmony trial-notify trial-release))

(defmethod staple:find-project ((system (eql (asdf:find-system :trial))) &key)
  (let ((project (make-instance 'staple:simple-project :output (asdf:system-relative-pathname system "docs/"))))
    (push (make-instance 'staple:simple-page
                         :project project
                         :input staple:*default-template*
                         :output (merge-pathnames "symbol-index.html" (staple:output project))
                         :system system
                         :packages (list (find-package '#:org.shirakumo.fraf.trial)
                                         (find-package '#:org.shirakumo.fraf.trial.alloy)
                                         (find-package '#:org.shirakumo.fraf.trial.steam)
                                         (find-package '#:org.shirakumo.fraf.trial.feedback)
                                         (find-package '#:org.shirakumo.fraf.trial.harmony)
                                         (find-package '#:org.shirakumo.fraf.trial.notify)
                                         (find-package '#:org.shirakumo.fraf.trial.release)))
          (staple:pages project))
    project))
