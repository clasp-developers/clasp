(require 'asdf)

(mapc #'asdf:register-preloaded-system
      '(:cleavir-environment :cleavir-primop :cleavir-ast
        :cleavir-generate-ast :cleavir-io :cleavir-meter
        :cleavir-ast-transformations :cleavir-code-utilities
        :cleavir-compilation-policy :cleavir-ir :cleavir-ast-to-hir
        :cleavir-kildall :cleavir-liveness :cleavir-kildall-type-inference
        :cleavir-escape :cleavir-hir-transformations :cleavir-utilities
        :cleavir-basic-blocks :cleavir-remove-useless-instructions
        :cleavir-hir-to-mir
        :acclimation :sicl-additional-conditions))

(asdf:register-immutable-system :clasp-cleavir) ; with -preloaded- it still reloads the files? bizarre

(asdf:load-asd #p"sys:kernel;sandbox;clasp-sandbox.asd")

(asdf:load-asd #p"sys:kernel;contrib;sicl;Code;Environment;sicl-global-environment.asd")

(asdf:load-asd #p"sys:kernel;contrib;sicl;Code;Environment;Simple;sicl-simple-environment.asd")

(asdf:load-system :clasp-sandbox :verbose t)
