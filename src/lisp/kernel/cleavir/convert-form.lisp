(in-package :clasp-cleavir-generate-ast)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a compound form that calls a local function.
;;; A local function can not have a compiler macro associated with it.


#+(or)(defmethod cleavir-generate-ast:convert-form :around (form (info cleavir-env:local-function-info) env)
  (let* ((call-ast (call-next-method))
	 (up-env (clasp-cleavir-env:unwind-protect-info env)))
    (if up-env
	(let ((up-ast (clasp-cleavir-ast:cleanup-ast (clasp-cleavir-env:unwind-protect-ast up-env))))
	  (change-class call-ast 'clasp-cleavir-ast:invoke-ast :unwind-ast up-ast))
	call-ast)))


#+(or)(defmethod cleavir-generate-ast:convert-form :around (form (info cleavir-env:global-function-info) env)
  (let ((call-ast (call-next-method))
	(up-env (clasp-cleavir-env:unwind-protect-info env)))
    (if up-env
	(let ((up-ast (clasp-cleavir-ast:cleanup-ast (clasp-cleavir-env:unwind-protect-ast up-env))))
	  (change-class call-ast 'clasp-cleavir-ast:invoke-ast :unwind-ast up-ast))
	call-ast)))


#+(or)(defmethod cleavir-generate-ast:convert-lambda-call :around (form env)
  (let ((call-ast (call-next-method))
	(up-env (clasp-cleavir-env:unwind-protect-info env)))
    (if up-env
	(let ((up-ast (clasp-cleavir-ast:cleanup-ast (clasp-cleavir-env:unwind-protect-ast up-env))))
	  (change-class call-ast 'clasp-cleavir-ast:invoke-ast :unwind-ast up-ast))
	call-ast)))
