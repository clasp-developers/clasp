(asdf:defsystem "clasp-sandbox"
  :description "First class global environments for Clasp"
  :version "0.0.1"
  :author "Alex Wood"
  :depends-on (:sicl-simple-environment :sicl-global-environment
               #-clasp :cleavir-compilation-policy
               #+clasp :clasp-cleavir)
  :components ((:file "packages")
               (:file "setf" :depends-on ("packages"))
               (:file "sandbox" :depends-on ("packages"))
               (:file "cleavir" :depends-on ("sandbox" "packages"))
               (:file "fill" :depends-on ("packages"))
               (:file "clone" :depends-on ("packages"))
               (:file "retarget-entry" :depends-on ("packages"))
               (:file "compilation-environment" :depends-on ("retarget-entry" "packages"))
               (:file "evaluation-environment" :depends-on ("compilation-environment" "packages"))
               (:file "interpreter" :depends-on ("sandbox" "packages"))
               (:file "read-interpreter-info" :depends-on ("packages"))
               (:file "compiler" :depends-on ("interpreter" "clone" "packages"))
               (:file "build-convenience"
                :depends-on ("compiler" "compilation-environment" "evaluation-environment" "packages"))))
