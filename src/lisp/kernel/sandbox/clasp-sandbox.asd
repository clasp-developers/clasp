(asdf:defsystem "clasp-sandbox"
  :description "First class global environments for Clasp"
  :version "0.0.1"
  :author "Alex Wood"
  :depends-on (:sicl-simple-environment :sicl-global-environment :clasp-cleavir)
  :components ((:file "packages")
               (:file "setf" :depends-on ("packages"))
               (:file "sandbox" :depends-on ("packages"))
               (:file "cleavir" :depends-on ("sandbox" "packages"))
               (:file "fill" :depends-on ("packages"))))
