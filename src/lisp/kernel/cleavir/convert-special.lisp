(in-package :clasp-cleavir-generate-ast)

#+(or)(defmethod cleavir-generate-ast:convert-special
	  ((symbol (eql 'unwind-protect)) form env)
	(let* ((ast (cc-ast:make-unwind-protect-ast nil nil))
	       (new-env (cc-env:add-unwind-protect env ast))
	       (cleanup-forms (cleavir-generate-ast:convert-sequence (cddr form) env)))
	  (setf (cc-ast:cleanup-ast ast)
		(cleavir-ast:make-progn-ast cleanup-forms))
	  (let ((protected-form (cleavir-generate-ast:convert (cadr form) new-env)))
	    (setf (cc-ast:protected-ast ast) protected-form)
	    ast)))
