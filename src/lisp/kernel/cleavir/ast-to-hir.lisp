(in-package :clasp-cleavir-ast-to-hir)



(defmethod cleavir-ast-to-hir:compile-ast ((ast clasp-cleavir-ast:precalc-symbol-reference-ast) context)
  (cleavir-ast-to-hir:check-context-for-one-value-ast context)
  (let ((temp (cleavir-ast-to-hir:make-temp)))
    (cleavir-ast-to-hir:compile-ast
     (clasp-cleavir-ast:precalc-symbol-reference-vector-ast ast)
     (cleavir-ast-to-hir:context (list temp)
				 (list (clasp-cleavir-hir:make-precalc-symbol-instruction
					temp
					(cleavir-ir:make-immediate-input (clasp-cleavir-ast:precalc-symbol-reference-index ast))
					(first (cleavir-ast-to-hir:results context))
					:successor (first (cleavir-ast-to-hir:successors context))
					:original-object (clasp-cleavir-ast:precalc-symbol-reference-ast-original-object ast)
					))
				 (cleavir-ast-to-hir:invocation context)))))



(defmethod cleavir-ast-to-hir:compile-ast ((ast clasp-cleavir-ast:precalc-value-reference-ast) context)
  (cleavir-ast-to-hir:check-context-for-one-value-ast context)
  (let ((temp (cleavir-ast-to-hir:make-temp)))
    (cleavir-ast-to-hir:compile-ast
     (clasp-cleavir-ast:precalc-value-reference-vector-ast ast)
     (cleavir-ast-to-hir:context (list temp)
				 (list (clasp-cleavir-hir:make-precalc-value-instruction
					temp
					(cleavir-ir:make-immediate-input (clasp-cleavir-ast:precalc-value-reference-index ast))
					(first (cleavir-ast-to-hir:results context))
					:successor (first (cleavir-ast-to-hir:successors context))
					:original-object (clasp-cleavir-ast:precalc-value-reference-ast-original-object ast)
					))
				 (cleavir-ast-to-hir:invocation context)))))
