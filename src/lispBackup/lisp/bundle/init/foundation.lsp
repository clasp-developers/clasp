
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



(si::fset 'defun
	  #'(lambda (def env)
	      (let* ((name (second def))
		     (func `(function (ext::lambda-block ,@(cdr def)))))
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

(defun simple-program-error (e1 &rest args)
  (eval `(error ,e1 ,@args)))
