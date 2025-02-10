(in-package #:org.shirakumo.fraf.trial.release)

;;(setf dexador:*use-connection-pool* NIL)

(defmethod upload ((service (eql :ftp)) &key (release (release)) (bundles (config :ftp :bundles)) (user (config :ftp :user)) (port (config :ftp :port)) (hostname (config :ftp :hostname)) (password (config :ftp :password)) (path (config :ftp :path)))
  (org.mapcar.ftp.client:with-ftp-connection (connection :hostname hostname
                                                         :port (or port 21)
                                                         :username user
                                                         :password (or password (password user))
                                                         :passive-ftp-p T)
    (when path
      (org.mapcar.ftp.client:send-cwd-command connection (pathname-utils:native-namestring path)))
    (dolist (bundle bundles)
      (let ((bundle (bundle-path bundle :version (release-version release))))
        (org.mapcar.ftp.client:store-file connection bundle (file-namestring bundle) :type :binary)
        (deploy:status 2 "Uploaded to ~a" hostname)))))

(defmethod upload ((service (eql :ssh)) &key (release (release)) (bundles (config :ssh :bundles)) (user (config :ssh :user)) (port (config :ssh :port)) (hostname (config :ssh :hostname)) (password (config :ssh :password)) (path (config :ssh :path)))
  (trivial-ssh:with-connection (connection hostname (etypecase password
                                                      (pathname (trivial-ssh:key user password))
                                                      (string (trivial-ssh:pass user password))
                                                      ((eql :pass) (trivial-ssh:pass user (password user)))
                                                      ((or null (eql :agent)) (trivial-ssh:agent user)))
                                           trivial-ssh::+default-hosts-db+ (or port 22))
    (dolist (bundle bundles)
      (let* ((bundle (bundle-path bundle :version (release-version release)))
             (target (make-pathname :name (pathname-name bundle) :type (pathname-type bundle) :defaults path)))
        (trivial-ssh:upload-file connection bundle target)
        (deploy:status 2 "Uploaded to ~a" target)))))

(defmethod upload ((service (eql :rsync)) &key (release (release)) (bundles (config :rsync :bundles)) (user (config :rsync :user)) (port (config :rsync :port)) (hostname (config :rsync :hostname)) (path (config :rsync :path)))
  (dolist (bundle bundles)
    (let ((bundle (bundle-path bundle :version (release-version release))))
      (uiop:run-program (list "rsync" "-avz" (format NIL "--rsh=ssh -p~a" (or port 22)) bundle
                              (format NIL "~@[~a@~]~a:~@[~a~]" user hostname path))
                        :output *standard-output* :error-output *error-output*)
      (deploy:status 2 "Uploaded to ~a~@[~a~]" hostname path))))

(defmethod upload ((service (eql :http)) &key (release (release)) (bundles (config :http :bundles)) (url (config :http :url)) (method (config :http :method)) (file-parameter (config :http :file-parameter)) (parameters (config :http :parameters)))
  (dolist (bundle bundles)
    (dexador:request url
                     :method (or method :post)
                     :content (list* (cons (or file-parameter "file") (bundle-path bundle :version (release-version release)))
                                     parameters)))
  (deploy:status 2 "Uploaded to ~a" url))

(defun upload-keygen-file (file file-id version &key (key (config :keygen :key)) (secret (config :keygen :secret)) (token (config :keygen :token)) (token-secret (config :keygen :token-secret)) (api-base (config :keygen :api-base)) (chunk-size (* 1024 1024 5)))
  (let* ((secret-source (or (config :keygen :secrets) api-base))
         (client (make-instance 'north:client :key (or key (password secret-source "key"))
                                              :secret (or secret (password secret-source "secret"))
                                              :token (or token (password secret-source "token"))
                                              :token-secret (or token-secret (password secret-source "token-secret"))
                                              :request-token-uri NIL
                                              :authorize-uri NIL
                                              :access-token-uri NIL))
         (buffer (make-array chunk-size :element-type '(unsigned-byte 8)))
         (endpoint (format NIL "~a/keygen/file/upload" (string-right-trim "/" api-base))))
    (with-open-file (in file :element-type '(unsigned-byte 8))
      (loop with octets-total = (file-length in)
            for read = (read-sequence buffer in)
            for part = (if (= read (length buffer))
                           buffer
                           (make-array read :element-type '(unsigned-byte 8) :displaced-to buffer))
            for chunk from 0
            for octets-read = read then (+ octets-read read)
            while (< 0 read)
            do (when (and (< 0 chunk) (= 0 (mod chunk 80)))
                 (terpri deploy:*status-output*))
               (format deploy:*status-output* "~4,1f" (/ (* 100 octets-read) octets-total))
               (north:make-signed-data-request client endpoint
                                               `(("payload" . (,part :content-type "application/octet-stream" :filename "payload")))
                                               :params `(("file" . ,(princ-to-string file-id))
                                                         ("version" . ,version)
                                                         ("chunk" . ,(princ-to-string chunk))))))
    (north:make-signed-request client endpoint :post
                               :params `(("file" . ,(princ-to-string file-id))
                                         ("payload" . "end")
                                         ("chunk" . "end")))))

(defmethod upload ((service (eql :keygen)) &rest args &key (release (release)) (bundles (config :keygen :bundles)) &allow-other-keys)
  (let* ((version (release-version release)))
    (loop for (bundle file) on bundles by #'cddr
          do (deploy:status 3 "Uploading ~a" bundle)
             (apply #'upload-keygen-file (bundle-path bundle :version version) (princ-to-string file) version args))))

(defun release-systems (release)
  (let ((systems ()))
    (when (directory (make-pathname :name :wild :type "run" :defaults release))
      (push :gnu-linux systems))
    (when (directory (make-pathname :name :wild :type "o" :defaults release))
      (push :mac systems))
    (when (directory (make-pathname :name :wild :type "exe" :defaults release))
      (push :windows systems))
    systems))

(defun release-executable (release system)
  (flet ((try (files)
           (when files
             (file-namestring (first files)))))
    (case system
      ((:linux :gnu-linux)
       (try (directory (make-pathname :name :wild :type "run" :defaults release))))
      (:mac 
       (try (directory (make-pathname :name :wild :type "o" :defaults release))))
      (:windows 
       (try (directory (make-pathname :name :wild :type "exe" :defaults release)))))))

(defun template (template output release &key branch preview platform secret)
  (file-replace template output `(("\\$CONTENT" ,(pathname-utils:native-namestring release))
                                  ("\\$BRANCH" ,(or branch ""))
                                  ("\\$PREVIEW" ,(if preview "1" "0"))
                                  ("\\$VERSION" ,(release-version release))
                                  ("\\$PLATFORM" ,(if platform (string-downcase platform) ""))
                                  ("\\$EXECUTABLE" ,(if platform (release-executable release platform) ""))
                                  ("\\$SECRET" ,(or secret "")))))

(defmethod upload ((service (eql :itch)) &key (release (release)) (bundles (config :itch :bundles)) (user (config :itch :user)) (password (config :itch :password)) (project (config :itch :project)) &allow-other-keys)
  (let ((version (release-version release))
        (config (merge-pathnames "itch/butler_creds" (uiop:xdg-config-pathname))))
    (when (and password (not (probe-file config)))
      (alexandria:write-string-into-file (password password) config))
    (loop for (bundle file) on bundles by #'cddr
          for filename = (or file (format NIL "~a:~{~a~^-~}" (or project (string (config :system))) bundle))
          do (run "butler" "-i" config
                  "push" (bundle-path bundle :version version) (format NIL "~a/~a" user filename)
                  "--userversion" version))))

(defmethod upload ((service (eql :steam)) &key (release (release)) (branch (config :steam :branch)) (preview (config :steam :preview)) (user (config :steam :user)) (password (config :steam :password)) &allow-other-keys)
  (let ((template (make-pathname :name "app-build" :type "vdf" :defaults (output)))
        (build (make-pathname :name "app-build" :type "vdf" :defaults release)))
    (template template build release :branch branch :preview preview)
    (run "steamcmd"
         "+login" user (password (or password user))
         "+run_app_build" build
         "+quit")))

(defmethod upload ((service (eql :gog)) &key (release (release)) (branch (config :gog :branch)) (user (config :gog :user)) (password (config :gog :password)) &allow-other-keys)
  (destructuring-bind (branch &optional branch-password) (if (listp branch) branch (list branch))
    (dolist (system (release-systems release))
      (let ((template (make-pathname :name "gog" :type "json" :defaults (output)))
            (build (make-pathname :name "gog-tmp" :type "json" :defaults (output))))
        (template template build release :platform system :secret (password password "client_secret"))
        (run "GOGGalaxyPipelineBuilder"
             "build-game" build
             "--username" user
             "--password" (password (or password user))
             "--version" release
             "--branch" branch
             "--branch_password" branch-password)
        (delete-file build)))))

(defmethod upload ((service (eql :all)) &rest args &key &allow-other-keys)
  (apply #'upload (remove-if-not #'config '(:itch :steam :gog :http :ssh :ftp :rsync :keygen)) args))

(defmethod upload ((service (eql T)) &rest args &key &allow-other-keys)
  (dolist (service (coerce (config :upload :targets) 'list))
    (apply #'upload service args)))

(defmethod upload ((services cons) &rest args &key &allow-other-keys)
  (dolist (service services)
    (apply #'upload service args)))

(defmethod upload :around (target &rest args &key &allow-other-keys)
  (restart-case
      (call-next-method)
    (continue ()
      :report "Treat the upload as successful")
    (retry ()
      :report "Retry the upload"
      (apply #'upload target args))))
