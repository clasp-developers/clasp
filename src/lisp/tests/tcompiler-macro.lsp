(defun check-compiler-macro-function (f env)
  (format t "ccmf = ~s~%" f)
  t)

(defun mexpansion (e f env)
  (format t "expand = ~s~%" f)
  #'(lambda (x) 'expand-me))

(let ((form '(funcall #'foo 1))
      (env nil))
  (or (and (eq (car form) 'cl:funcall)
	   (eq (car (cadr form)) 'cl:function)
	   (let ((expansion (check-compiler-macro-function (cadr (cadr form)) env)))
	     (if expansion
		 (mexpansion expansion (cons (cadr (cadr form)) (cddr form)) env)
		 form)))
      (let ((expansion (check-compiler-macro-function (car form) env)))
	(if expansion
	    (mexpansion expansion form env)
	    form))))

