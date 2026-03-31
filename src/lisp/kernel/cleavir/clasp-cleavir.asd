(asdf:defsystem #:clasp-cleavir
  :description "The Clasp/Cleavir compiler front end"
  :version "0.0.1"
  :author "Christian Schafmeister <chris.schaf@verizon.net>"
  :licence "LGPL-3.0"
  :depends-on (#:cleavir-attributes
               #:cleavir-bir
               #:cleavir-bir-transformations
               #:cleavir-compilation-policy
               #:cleavir-conditions
               #:cleavir-bir-builder ; for compile-bytecode
               #:cleavir-stealth-mixins
               #:eclector)
  :serial t
  :components ((:file "packages")
               (:file "system")
               (:file "policy")
               (:file "convert-special")
               (:file "setup")
               (:file "fold")
               (:file "ir")
               (:file "jit")
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
               (:file "literal")
               (:file "translate")
               (:file "compile-bytecode")
               (:file "inline-prep")
               (:file "proclamations")
               (:file "hooks")
               (:file "bytecode-adaptor")
               (:file "inline")))
