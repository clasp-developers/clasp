(in-package :asdf-user)

(defsystem "clasp-cleavir"
    :description "The Clasp/Cleavir compiler front end"
    :version "0.0.1"
    :author "Christian Schafmeister <chris.schaf@verizon.net>"
    :licence "LGPL-3.0"
    :depends-on (:cleavir-generate-ast
		 :cleavir-ast-to-hir
		 :cleavir-hir-transformations
		 :cleavir-hir-to-mir
		 :cleavir-basic-blocks)
    :components ((:file "packages")
		 (:file "setup" :depends-on ("packages"))
		 (:file "mir" :depends-on ("setup"))
		 (:file "translate" :depends-on ("packages"))))
