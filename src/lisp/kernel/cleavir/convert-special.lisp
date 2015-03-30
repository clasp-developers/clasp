(in-package :clasp-cleavir-generate-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Expand local-macro-info definitions
;;; use the macro WITH-EARLY-ACCESSORS - it creates
;;; macrolet macros that substitute for functions - this confuses cleavir
;;; Treat LOCAL-MACRO-INFO function names as globals
(defmethod cleavir-generate-ast:convert-function
    ((info cleavir-env:local-macro-info) env (system clasp-cleavir:clasp))
  (format t "In kernel/cleavir/convert-special.lisp -- cleavir-generate-ast:convert-function for: ~a~%" info)
  (cleavir-generate-ast:convert-global-function info (cleavir-env:global-environment env) system))



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
    ((symbol (eql 'cl:multiple-value-prog1)) form environment (system clasp-cleavir:clasp))
  (destructuring-bind (first-form . forms) 
      (rest form)
    (cleavir-generate-ast:convert `(core:multiple-value-prog1-function #'(lambda () ,first-form) (lambda () ,@forms)) environment system)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CORE:DEBUG-MESSAGE
;;;
;;; This is converted into a call to print a message
;;;
(defmethod cleavir-generate-ast:convert-special
    ((symbol (eql 'core:debug-message)) form environment (system clasp-cleavir:clasp))
  (make-instance 'clasp-cleavir-ast:debug-message-ast :debug-message (cadr form)))

(defmethod cleavir-generate-ast:check-special-form-syntax ((head (eql 'core:debug-message)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 1))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CL:PROGV
;;;
;;; Convert this into a function call
(defmethod cleavir-generate-ast:convert-special
    ((symbol (eql 'cl:progv)) form environment (system clasp-cleavir:clasp))
  (destructuring-bind (symbols values . forms) 
      (rest form)
    (cleavir-generate-ast:convert `(core:progv-function symbols values #'(lambda () ,@forms)) environment system)))





(defmethod cleavir-generate-ast:convert-global-function (info global-env (system clasp-cleavir:clasp))
  (declare (ignore global-env))
  (let ((name (cleavir-env:name info)))
    (cond 
      ((and (consp name) (eq (car name) 'cl:setf))
       (clasp-cleavir-ast:make-setf-fdefinition-ast
	(cleavir-ast:make-load-time-value-ast `',(cadr name) t)
	info))
      ((consp name)
       (error "Illegal name for function - must be (setf xxx)"))
      (t
       (cleavir-ast:make-fdefinition-ast
	(cleavir-ast:make-load-time-value-ast `',name t)
	info)))))
