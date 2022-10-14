(in-package :asdf-user)

(defsystem "clasp-cleavir"
  :description "The Clasp/Cleavir compiler front end"
  :version "0.0.1"
  :author "Christian Schafmeister <chris.schaf@verizon.net>"
  :licence "LGPL-3.0"
  :depends-on (:concrete-syntax-tree
               :eclector-concrete-syntax-tree
               :eclector
               :cleavir-bir
               :cleavir-ast-to-bir
               :cleavir-bir-transformations
               :cleavir-cst-to-ast
               :cleavir-compilation-policy
               :cleavir-conditions
               :cleavir-attributes
               :cleavir-ast-transformations
               :cleavir-stealth-mixins)
  :serial t
  :components ((:file "packages")
               (:file "system")
               (:file "policy")
               (:file "reader")
               (:file "ast")
               (:file "convert-form")
               (:file "convert-special")
               (:file "toplevel")
               (:file "setup")
               (:file "fold")
               (:file "ir")
               (:file "compile-file-client")
               (:file "translation-environment")
               (:file "bir")
               (:file "bmir")
               (:file "blir")
               (:file "vaslist")
               (:file "bir-to-bmir")
               (:file "representation-selection")
               (:file "bmir-to-blir")
               (:file "landing-pad")
               (:file "primop")
               (:file "interval")
               (:file "type")
               (:file "transform")
               (:file "translate")
               ;;                (:file "satiation")
               (:file "fixup-eclector-readtables")
               (:file "activate-clasp-readtables-for-eclector")
               (:file "define-unicode-tables")
               (:file "inline-prep")
               (:file "proclamations")
               ;;                 (:file "auto-compile")
               ;;                 (:file "inline")
               ))
