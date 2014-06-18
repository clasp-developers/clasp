;-*- Mode:     Lisp -*-
;;;; Author:   Juan Jose Garcia-Ripoll
;;;; Created:  Fri Apr 14 11:13:17 CEST 2006
;;;; Contains: Tools for doing tests, intercepting functions, etc.

(defmacro with-dflet (functions &body body)
  "Syntax:
	(with-dflet ((fname form*)*) body)
Evaluate BODY in an environment in which the function FNAME has been redefined
to evaluate the given forms _before_ executing the orginal code."
  (let ((vars '()) (in-forms '()) (out-forms '()))
    (loop for (name . forms) in functions
	  do (let ((var (gensym)))
	       (push `(,var #',name) vars)
	       (push `(setf (fdefinition ',name)
		       #'(lambda (&rest args) ,@forms (apply ,var args)))
		     in-forms)
	       (push `(setf (fdefinition ',name) ,var) out-forms)))
    `(let ,vars
      (unwind-protect
	   (progn ,@in-forms ,@body)
	(progn ,@out-forms)))))

(defmacro with-compiler ((filename &rest compiler-args) &body forms)
  "Create a lisp file with the given forms and compile it. The forms are
evaluated. The output is stored in a string and output as a second value."
  `(progn
     (with-open-file (s ,filename :direction :output :if-exists :supersede
			:if-does-not-exist :create)
       ,@(loop for f in forms collect `(print ,f s)))
     (let* ((ok t)
	    (output
	     (with-output-to-string (*standard-output*)
	       (let ((*error-output* *standard-output*)
		     (*compile-verbose* t)
		     (*compile-print* t)
		     #-ecl-bytecmp
		     (c::*suppress-compiler-warnings* nil)
		     #-ecl-bytecmp
		     (c::*suppress-compiler-notes* nil))
		 (setf ok (compile-file ,filename ,@compiler-args))))))
       (values ok output))))



