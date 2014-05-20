(setf sb-ext:*evaluator-mode* :interpret)
(declaim (optimize (speed 0) (space 1) (compilation-speed 0) (debug 3)))
(describe-compiler-policy)

(defun make-env (env) env)

(defun env-extend (alist name expr)
  (acons name expr alist))

(defun env-lookup (alist name)
  (cdr (assoc name alist)))



(defun symbol-to-c (sym)
  (with-output-to-string (sout)
    (with-input-from-string (sin (symbol-name sym))
      (do ((c (read-char sin nil)
	      (read-char sin nil)))
	  ((null c))
 	(cond
	  ((eql c #\-) (format sout "_"))
	  ((eql c #\*) (format sout "Star"))
	  ((upper-case-p c) (format sout "~a" (char-downcase c)))
 	  (t (format sout "~a" c))))))
  )



(defun cemit (fmt args env)
  (apply #'format `( nil ,fmt ,@args)))

(defun cemit-if (exp env)
  (if (eql (length exp) 4)
      (cemit "if (~a) { ~a ; } else { ~a ; }"
	     (list
	      (ceval (cadr exp) env)
	      (ceval (caddr exp) env)
	      (ceval (cadddr exp) env))
	     env )
      (cemit  "if (~a) { ~a ; }"
	     (list 
	      (ceval (cadr exp) env)
	      (ceval (caddr exp) env))
	     env)))


(defun cemit-let (exp env)
  (cemit "eval::special_let(_lisp->nil<Executable_sp>(),~a,_lisp->globalEnvironment(),_lisp);" (ceval-evaluate-args exp env )env))
  
(defun cemit-do (exp env)
  (cemit "eval::special_do(_lisp->nil<Executable_sp>(),~a,_lisp->globalEnvironment(),_lisp);" (ceval-evaluate-args exp env) env ))



#|
(defun cemit-list (exp env)
  (with-output-to-string (sout)
    (format sout "( ql::list(_lisp) ")
    (dolist (one (cdr exp))
      (format sout ", ~a" (ceval one env)))
    (format sout " ).cons() ")
    ))
|#

(defun ceval-evaluate-args (args env)
  (with-output-to-string (sout)
    (format sout "( ql::list(_lisp) ")
    (dolist (one args)
      (format sout ", ~a" (ceval one env)))
    (format sout " ).cons() ")
    ))

(defun cemit-defun (exp env)
  (let* ((name (cadr exp))
	 (args (caddr exp))
	 (body (cdddr exp)))
    (with-output-to-string (sout)
      (format sout "#define ARGS_~a \"~a\"~%" (symbol-to-c name) (args)))))

(defun cemit-apply (exp env)
  (let ((name (car exp))
	(rest (cdr exp)))
    (with-output-to-string (sout)
      (format sout "eval::apply(SID_~a,~a,_lisp)~%"
	      (symbol-to-c name)
	      (cemit-list rest env)))))


(defun cemit-var (exp env)
  (format nil "env->value(SID_~a) " (symbol-to-c exp)))


(defun cemit-function-value (args env)
  (let ((function-name (car args)))
    (format nil "env->function(SD_~a)" (symbol-to-c function-name))))

(defun ceval (exp env)
  (cond
    ((atom exp)
     (cond
       ((numberp exp) (cemit "~a" (list exp) env))
       ((eq exp 't) (cemit "true" () env ) )
       ((symbolp exp) (cemit-var exp env))
       (t (error "Unknown atom type[~a]" exp))))
    (t
     (let ((head (car exp))
	   (args (cdr exp)))
       (cond
	 ;; Do special forms here
	 ((eq head 'if) (cemit-if exp env))
	 ((eq head 'do) (cemit-do exp env))
	 ((eq head 'let) (cemit-let exp env))
	 ((eq head 'when) (cemit-when exp env))
	 ;; Do inlined functions here
	 ((eq head 'function)
	  (cemit-function-value args env))
	 ;; Do functions here
	 (t
	  (let ((evaluatedArgs (ceval-evaluate-args args env)))
	    (cemit "eval::apply(SID_~a, ~a,_lisp)~%" (list (symbol-to-c head) evaluatedArgs) env))))))
    )
  )

(symbol-to-c 'hel-lo*)



(ceval-evaluate-args '(other 2 3) ())  


(ceval '(hi 1 2) ())

(ceval '(mapappend #'canonicalize-defclass-option options) () )

(cemit-let '(let ((a 1)
	      (b 2))
	      (jjj a b)) ())

