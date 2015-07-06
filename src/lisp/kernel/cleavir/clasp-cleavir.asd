(in-package :asdf-user)

(defsystem "clasp-cleavir"
    :description "The Clasp/Cleavir compiler front end"
    :version "0.0.1"
    :author "Christian Schafmeister <chris.schaf@verizon.net>"
    :licence "LGPL-3.0"
    :depends-on (:cleavir-generate-ast
		 :cleavir-ir
		 :cleavir-ast-to-hir
		 :cleavir-ast-transformations
		 :cleavir-hir-transformations
		 :cleavir-hir-to-mir
		 :cleavir-basic-blocks
                 :sicl-additional-conditions)
    :serial t
    :components ((:file "packages")
                 (:file "compile-cclasp")
		 (:file "system")
		 (:file "ast")
		 (:file "convert-form")
		 (:file "convert-special")
		 (:file "hir")
		 (:file "introduce-invoke")
                 (:file "toplevel")
		 (:file "setup")
		 (:file "ast-to-hir")
		 (:file "mir")
		 (:file "hir-to-mir")
		 (:file "ir")
		 (:file "gml-drawing")
		 (:file "landing-pad")
		 (:file "arguments")
		 (:file "translate")
		 ))
