(asdf:defsystem #:clasp-cleavir
  :description "The Clasp/Cleavir compiler front end"
  :version "0.0.1"
  :author "Christian Schafmeister <chris.schaf@verizon.net>"
  :licence "LGPL-3.0"
  :depends-on (#:cleavir-ast-to-bir
               #:cleavir-attributes
               #:cleavir-bir
               #:cleavir-bir-transformations
               #:cleavir-compilation-policy
               #:cleavir-conditions
               #:cleavir-cst-to-ast
               #:cleavir-stealth-mixins
               #:concrete-syntax-tree
               #:eclector
               #:eclector-concrete-syntax-tree)
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
               (:file "fixup-eclector-readtables")
               (:file "activate-clasp-readtables-for-eclector")
               (:file "define-unicode-tables")
               (:file "inline-prep")
               (:file "proclamations")
               (:file "auto-compile")
               (:file "bytecode-adaptor")
               (:file "inline")))
