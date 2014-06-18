;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "SYSTEM")

(defun read-evaluated-form ()
  (format *query-io* "~&Type a form to be evaluated:~%")
  (list (eval (read *query-io*))))

(defun wrong-type-argument (object type &optional place function)
  #-ecl-min
  (declare (c::policy-debug-ihs-frame))
  (tagbody again
     (restart-case
	 (error 'simple-type-error
		:format-control
                "In ~:[an anonymous function~;~:*function ~A~], ~
                 ~:[found object~;~:*the value of ~A is~]~%~8t~S~%~
                 which is not of expected type ~A"
		:format-arguments (list function place object type)
		:datum object
		:expected-type type
		)
       (use-value (value)
	 :report (lambda (stream)
		   (format stream "Supply a new value of type ~A." type))
	 :interactive read-evaluated-form
	 (setf object value)
	 (unless (typep object type)
	   (go again)))))
  object)

(defmacro check-type (place type &optional type-string)
  "Args: (check-type place typespec [string-form])
Signals a continuable error, if the value of PLACE is not of the specified
type.  Before continuing, receives a new value of PLACE from the user and
checks the type again.  Repeats this process until the value of PLACE becomes
of the specified type.  STRING-FORM, if given, is evaluated only once and the
value is used to indicate the expected type in the error message."
  (let ((aux (gensym)))
    `(let ((,aux ,place))
       (declare (:read-only ,aux))
       (unless (typep ,aux ',type)
	 (setf ,place (do-check-type ,aux ',type ',type-string ',place)))
       nil)))

(defun do-check-type (value type type-string place)
  (tagbody again
     (unless (typep value type)
       (restart-case
	   (error 'simple-type-error
		  :datum value
		  :expected-type type
		  :format-control "The value of ~S is ~S, which is not ~:[of type ~S~;~:*~A~]."
		  :format-arguments (list place value type-string type))
	 (store-value (new-value)
	   :report (lambda (stream)
		     (format stream "Supply a new value of ~S" place))
	   :interactive read-evaluated-form
	   (setf value new-value)
	   (go again)))))
  value)

(defmacro assert (test-form &optional places &rest arguments)
  "Args: (assert form [({place}*) [string {arg}*]])
Evaluates FORM and signals a continuable error if the value is NIL.  Before
continuing, receives new values of PLACEs from user.  Repeats this process
until FORM returns a non-NIL value.  Returns NIL.  STRING is the format string
for the error message and ARGs are arguments to the format string."
  (let ((repl
         (if places
             `(setf (values ,@places)
                    (assert-failure ',test-form ',places (list ,@places)
                                    ,@arguments))
             `(assert-failure ',test-form
                              ,@(and arguments
                                     (list* nil nil arguments))))))
  `(while (not ,test-form)
     ,repl)))

(defun accumulate-cases (cases list-is-atom-p)
  (declare (si::c-local))
  (do ((c cases (cdr c))
       (l '()))
      ((null c) (nreverse l))
    (let ((keys (caar c)))
      (cond ((atom keys) (unless (null keys) (push keys l)))
	    (list-is-atom-p (push keys l))
	    (t (setq l (append keys l)))))))

(defun ecase-error (value values)
  (error 'CASE-FAILURE :name 'ECASE
	 :datum value
	 :expected-type (cons 'MEMBER values)
	 :possibilities values))

(defmacro ecase (keyform &rest clauses)
  "Syntax: (ecase keyform {({key | ({key}*)} {form}*)}*)
Evaluates KEYFORM and tries to find the KEY that is EQL to the value of
KEYFORM.  If found, then evaluates FORMs that follow the KEY (or the key list
that contains the KEY) and returns all values of the last FORM.  If not,
signals an error."
  (setq clauses (remove-otherwise-from-clauses clauses))
  (let ((key (gensym)))
    `(let ((,key ,keyform))
       (case ,key ,@clauses
	 (t (si::ecase-error ,key ',(accumulate-cases clauses nil)))))))

(defun ccase-error (keyform key values)
  (restart-case (error 'CASE-FAILURE
		       :name 'CCASE
		       :datum key
		       :expected-type (cons 'MEMBER values)
		       :possibilities values)
    (store-value (value)
      :REPORT (lambda (stream)
		(format stream "Supply a new value of ~S" keyform))
      :INTERACTIVE read-evaluated-form
      (return-from ccase-error value))))

(defun remove-otherwise-from-clauses (clauses)
  (declare (si::c-local))
  (mapcar #'(lambda (clause)
	      (let ((options (first clause)))
		(if (member options '(t otherwise))
		    (cons (list options) (rest clause))
		    clause)))
	  clauses))

(defmacro ccase (keyplace &rest clauses)
  "Syntax: (ccase place {({key | ({key}*)} {form}*)}*)
Searches a KEY that is EQL to the value of PLACE.  If found, then evaluates
FORMs in order that follow the KEY (or the key list that contains the KEY) and
returns all values of the last FORM.  If no such KEY is found, signals a
continuable error.  Before continuing, receives a new value of PLACE from
user and searches a KEY again.  Repeats this process until the value of PLACE
becomes EQL to one of the KEYs."
  (let* ((key (gensym))
	 (repeat (gensym))
	 (block (gensym)))
    (setq clauses (remove-otherwise-from-clauses clauses))
    `(block ,block
       (tagbody ,repeat
	 (let ((,key ,keyplace))
	   (return-from ,block
	     (case ,key ,@clauses
	       (t (setf ,keyplace
			(si::ccase-error ',keyplace ,key
					 ',(accumulate-cases clauses nil)))
		  (go ,repeat)))))))))

(defmacro typecase (keyform &rest clauses)
  "Syntax: (typecase keyform {(type {form}*)}*)
Evaluates KEYFORM and searches a TYPE to which the value of KEYFORM belongs.
If found, then evaluates FORMs that follow the TYPE and returns all values of
the last FORM.  If not, simply returns NIL.  The symbols T and OTHERWISE may
be used as a TYPE to specify the default case."
  (do ((l (reverse clauses) (cdr l))
       (form nil) (key (gensym)))
      ((endp l) `(let ((,key ,keyform)) ,form))
      (if (or (eq (caar l) 't) (eq (caar l) 'otherwise))
          (setq form `(progn ,@(cdar l)))
          (setq form
                `(if (typep ,key (quote ,(caar l)))
                     (progn ,@(cdar l))
                     ,form))))
  )

(defun etypecase-error (value types)
  (error 'CASE-FAILURE :name 'ETYPECASE
	 :datum value
	 :expected-type (cons 'OR types)
	 :possibilities types))

(defmacro etypecase (keyform &rest clauses &aux (key (gensym)))
  "Syntax: (etypecase keyform {(type {form}*)}*)
Evaluates KEYFORM and searches a TYPE to which the value of KEYFORM belongs.
If found, then evaluates FORMs that follow the TYPE and returns all values of
the last FORM.  If not, signals an error."
   (setq clauses (remove-otherwise-from-clauses clauses))
   (do ((l (reverse clauses) (cdr l))	; Beppe
        (form `(etypecase-error ,key ',(accumulate-cases clauses t))))
       ((endp l) `(let ((,key ,keyform)) ,form))
       (setq form `(if (typep ,key ',(caar l))
                       (progn ,@(cdar l))
                       ,form))
       )
   )

(defun ctypecase-error (keyplace value types)
  (restart-case (error 'CASE-FAILURE
		       :name 'CTYPECASE
		       :datum value
		       :expected-type (cons 'OR types)
		       :possibilities types)
    (store-value (value)
      :REPORT (lambda (stream)
		(format stream "Supply a new value of ~S." keyplace))
      :INTERACTIVE read-evaluated-form
      (return-from ctypecase-error value))))

(defmacro ctypecase (keyplace &rest clauses &aux (key (gensym)))
  "Syntax: (ctypecase place {(type {form}*)}*)
Searches a TYPE to which the value of PLACE belongs.  If found, then evaluates
FORMs that follow the TYPE and returns all values of the last FORM.  If no
such TYPE is found, signals a continuable error.  Before continuing, receives
a new value of PLACE from the user and searches an appropriate TYPE again.
Repeats this process until the value of PLACE becomes of one of the TYPEs."
  (setq clauses (remove-otherwise-from-clauses clauses))
  `(loop
    (let ((,key ,keyplace))
      ,@(mapcar #'(lambda (l)
		    `(when (typep ,key ',(car l))
		      (return (progn ,@(cdr l)))))
		clauses)
      (setf ,keyplace (ctypecase-error ',keyplace ,key
				       ',(accumulate-cases clauses t))))))
