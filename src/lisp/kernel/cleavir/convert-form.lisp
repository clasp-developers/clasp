(in-package :cc-generate-ast)


(defmethod cleavir-generate-ast:convert-code (lambda-list body env (system clasp-cleavir:clasp) &optional block-name )
  (let ((function-ast (call-next-method)))
    (multiple-value-bind (declarations documentation forms)
	(cleavir-code-utilities:separate-function-body body)
      (let* ((dspecs (reduce #'append (mapcar #'cdr declarations)))
	     (lambda-name (cadr (find 'core:lambda-name dspecs :key #'car))))
	(unless lambda-name (setq lambda-name 'cl:lambda))
    ;; Make the change here to a named-function-ast with lambda-name
	(change-class function-ast 'clasp-cleavir-ast:named-function-ast
		      :lambda-name lambda-name)))))


 
