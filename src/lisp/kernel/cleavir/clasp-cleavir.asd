(in-package :asdf-user)

(defsystem "clasp-cleavir"
    :description "The Clasp/Cleavir compiler front end"
    :version "0.0.1"
    :author "Christian Schafmeister <chris.schaf@verizon.net>"
    :licence "LGPL-3.0"
    :depends-on (:cleavir-generate-ast
		 :cleavir-ast-to-hir
		 :cleavir-ast-transformations
		 :cleavir-hir-transformations
		 :cleavir-hir-to-mir
		 :cleavir-basic-blocks)
    :components ((:file "packages")
		 (:file "ast" :depends-on ("packages"))
		 (:file "hir" :depends-on ("packages"))
		 (:file "ast-to-hir" :depends-on ("packages"))
		 (:file "setup" :depends-on ("packages" "ast"))
		 (:file "mir" :depends-on ("setup"))
		 (:file "translate" :depends-on ("packages" "ast"))))
