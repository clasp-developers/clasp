
(in-package :core)

(export '(T cons))

(defparameter *dump-defun-definitions* nil)
(defparameter *dump-defmacro-definitions* *dump-defun-definitions*)

(defconstant lambda-list-keywords '( &ALLOW-OTHER-KEYS
				    &AUX &BODY &ENVIRONMENT &KEY
				    &OPTIONAL &REST
				    &WHOLE) )
(export '(lambda-list-keywords))



;; Temporary check-type - everything is true
(fset 'check-type 
      #'(lambda (whole env) t)
      t)
(export 'check-type)


(fset '1- #'(lambda (num) (- num 1)))
(fset '1+ #'(lambda (num) (+ num 1)))



#+(or)(si::fset 'defun
		#'(lambda (def env)
		    (let* ((name (second def))
			   (func `(function (ext::lambda-block ,@(cdr def)))))
		      (ext:register-with-pde def `(si::fset ',name ,func))))
		t)

;; Convert (defun XXX (args) body) 
(si::fset 'defun
	  #'(lambda (def env)
	      (let* ((name (second def))
		     (func `(function (lambda ,(caddr def) (block ,(cadr def) ,@(cdddr def))))))
		(ext:register-with-pde def `(si::fset ',name ,func))))
	  t)
(export '(defun))



(si::fset 'and
	  #'(lambda (whole env)
	      (let ((forms (cdr whole)))
		(if (null forms)
		    t
		    (if (null (cdr forms))
			(car forms)
			`(if ,(car forms)
			     (and ,@(cdr forms)))))))
	  t)


(si::fset 'or
	  #'(lambda (whole env)
	      (let ((forms (cdr whole)))
		(if (null forms)
		    nil
		    (if ( null (cdr forms))
			(car forms)
			(let ((tmp (gensym)))
			  `(let ((,tmp ,(car forms)))
			     (if ,tmp
				 ,tmp
				 (or ,@(cdr forms)))))))))
	  t )
(export '(and or))

(defun constantly (object)
  #'(lambda (&rest arguments) object))

(export 'constantly)

(defun simple-program-error (message &rest datum)
  (signal-simple-error 'program-error nil message datum))


;; We do not use this macroexpanso, and thus we do not care whether
;; it is efficiently compiled by ECL or not.
(defmacro multiple-value-bind (vars form &rest body)
  "Syntax: (multiple-value-bind ({var}*) init {decl}* {form}*)
Evaluates INIT and binds the N-th VAR to the N-th value of INIT or, if INIT
returns less than N values, to NIL.  Then evaluates FORMs, and returns all
values of the last FORM.  If no FORM is given, returns NIL."
  (declare (notinline mapcar))
  `(multiple-value-call #'(lambda (&optional ,@(mapcar #'list vars) &rest ,(gensym)) ,@body) ,form))
