(in-package :clasp-cleavir-generate-ast)


(defmethod cleavir-generate-ast:convert-code (lambda-list body env (system clasp-cleavir:clasp))
  (let ((function-ast (call-next-method)))
    (multiple-value-bind (declarations documentation forms)
	(cleavir-code-utilities:separate-function-body body)
      (let* ((dspecs (reduce #'append (mapcar #'cdr declarations)))
	     (lambda-name (cadr (find 'core:lambda-name dspecs :key #'car))))
	(unless lambda-name (setq lambda-name "unnamed-lambda"))
    ;; Make the change here to a named-function-ast with lambda-name
	(change-class function-ast 'clasp-cleavir-ast:named-function-ast
		      :lambda-name lambda-name)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MULTIPLE-VALUE-CALL.
;;;
;;; In the case where there is one multiple-value form this gets converted
;;; into a multiple-value-call-ast.  In the general case with multiple forms
;;; it gets converted into a function call to CORE:MULTIPLE-VALUE-FUNCALL.
;;;
(defmethod cleavir-generate-ast:convert-special
    ((symbol (eql 'core:multiple-value-call)) form environment (system clasp-cleavir:clasp))
  (destructuring-bind (function-form . forms) 
      (rest form)
    (if (eql (length forms) 1)
	(cleavir-ast:make-multiple-value-call-ast
	 (cleavir-generate-ast:convert function-form environment system)
	 (cleavir-generate-ast:convert-sequence forms environment system))
	(cleavir-generate-ast:convert `(core:multiple-value-funcall ,function-form ,@(mapcar (lambda (x) #'x) ,forms))
				      environment system))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MULTIPLE-VALUE-PROG1
;;;
;;; This is converted into a call to core:multiple-value-prog1-function func1 func2
;;; Func1 is evaluated and the multiple values are saved and then func2 is evaluated
;;; and the multiple values returned from func1 are restored
;;;
(defmethod cleavir-generate-ast:convert-special
    ((symbol (eql 'core:multiple-value-prog1)) form environment (system clasp-cleavir:clasp))
  (destructuring-bind (first-form . forms) 
      (rest form)
    (cleavir-generate-ast:convert `(core:multiple-value-prog1-function #'(lambda () ,first-form) (lambda () ,@forms)) environment system)))


 
