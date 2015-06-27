(defun compiler-macro-function (name &optional env)
;;  (declare (ignorable env))
  (get-sysprop name 'sys::compiler-macro))


(defun maybe-compiler-macro-expand (form &optional env)
  (or (and (eq (car form) 'cl:funcall)
	   (eq (car (cadr form)) 'cl:function)
	   (let ((expander (compiler-macro-function (cadr (cadr form)) env)))
	     (if expander
		 (funcall *macroexpand-hook* expander (cons (cadr (cadr form)) (cddr form)) env)
		 form)))
      (let ((expander (compiler-macro-function (car form) env)))
	(if expander
	    (funcall *macroexpand-hook* expander form env)
	    form))))


(define-compiler-macro foo (x) `(print ,x))

(compiler-macro-function 'foo)
(untrace)
(trace compiler-macro-function)
(maybe-compiler-macro-expand '(bar 1))


