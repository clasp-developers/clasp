(defun ffun (f &rest args)
  (apply f args))

(define-compiler-macro funcall (function &rest arguments)
  (let ((fsym (gensym "FUNCTION")))
    `(let ((,fsym ,function))
       (cleavir-primop:funcall
	(if (cleavir-primop:typeq ,fsym function)
	    ,fsym
	    ;; defined below with type check
	    (symbol-function ,fsym))
	,@arguments))))

(ffun #'print 1)


