(in-package :asdf-user)

(defsystem "clasp-cleavir"
  :description "The Clasp/Cleavir compiler front end"
  :version "0.0.1"
  :author "Christian Schafmeister <chris.schaf@verizon.net>"
  :licence "LGPL-3.0"
  :depends-on (:concrete-syntax-tree
               :eclector-concrete-syntax-tree
               :eclector
               :cleavir-generate-ast
               :cleavir-cst-to-ast
               :cleavir-ir
               :cleavir-compilation-policy
               :cleavir-ast-to-hir
               :cleavir-ast-transformations
               :cleavir-kildall-type-inference
               :cleavir-escape
               :cleavir-hir-transformations
               :cleavir-partial-inlining
               :cleavir-remove-useless-instructions
               :cleavir-hir-to-mir
               :cleavir-basic-blocks
               :sicl-additional-conditions)
  :serial t
  :components ((:file "packages")
                 ;;                 (:file "compile-cclasp")
               (:file "cleavir-fixups-and-hacks")
               (:file "system")
               (:file "policy")
               (:file "reader")
               (:file "ast")
               (:file "convert-form")
               (:file "convert-special")
               (:file "eliminate-ltvs")
               (:file "hir")
               (:file "introduce-invoke")
               (:file "ast-interpreter")
               (:file "toplevel")
               (:file "setup")
               (:file "ast-to-hir")
               (:file "mir")
               (:file "hir-to-mir")
               (:file "ir")
               (:file "gml-drawing")
               (:file "landing-pad")
               ;;		 (:file "arguments")
               (:file "closure-optimize")
               (:file "translate")
               (:file "translate-instruction")
               (:file "satiation")
               (:file "fixup-eclector-readtables")
               (:file "activate-clasp-readtables-for-eclector")
               (:file "inline-prep")
               ;;                 (:file "auto-compile")
               ;;                 (:file "inline")
               ))
