(asdf:defsystem "compilation-environment"
  :vesion "0.0.1"
  :author "Alex Wood"
  :depends-on (:sicl-global-environment :cleavir-env)
  :components ((:file "packages")
               (:file "retarget-entry" :depends-on ("packages"))
               (:file "compilation-environment" :depends-on ("retarget-entry" "packages"))
               (:file "types" :depends-on ("compilation-environment" "packages"))
               (:file "policy" :depends-on ("compilation-environment" "packages"))))
