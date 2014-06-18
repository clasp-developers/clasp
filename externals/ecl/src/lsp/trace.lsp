;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;        Tracer package for Common Lisp

(in-package "SYSTEM")

(defparameter *trace-level* 0)
;; (defparameter *trace-list* nil) ; In all_symbols.d !
(defparameter *trace-max-indent* 20)

(defmacro trace (&rest r)
"Syntax: (trace ({function-name | ({function-name}+)} {keyword [form]\}*)
Begins tracing the specified functions.  With no FUNCTION-NAMEs, returns a
list of functions currently being traced. The printed information consists of
the name of function followed at entry by its arguments and on exit by its
return values.
The keywords allow to control when and how tracing is performed.
The possible keywords are:

 :BREAK		a breakpoint is entered after printing the entry trace
		information, but before applying the traced function to its
		arguments, if form evaluates to non-nil
 :BREAK-AFTER 	like :BREAK but the breakpoint is entered after the function
		has been executed and the exit trace information has been
		printed and before control returns
 :COND-BEFORE	information is printed upon entry if form evaluates to non-nil
 :COND-AFTER	information is printed upon exit if form evaluates to non-nil
 :COND		specifies a single condition for both entry and exit
 :PRINT		prints the values of the forms in the list upon entry.
		They are preceeded by a backslash (\\)
 :PRINT-AFTER	prints the values of the forms in the list upon exit from the
		function. They are preceeded by a backslash (\\)
 :STEP		turns on the stepping facility

Forms can refer to the list of arguments of the function through the variable
SI::ARGS."
  `(trace* ',r))

(defun trace* (r)
  (if (null r)
      (mapcar #'first *trace-list*)
      (mapc #'trace-one r)))

(defmacro untrace (&rest r)
  "Syntax: (untrace {function-name}*)

Ends tracing the specified functions.  With no FUNCTION-NAMEs, ends tracing
all functions."
  `(untrace* ',r))

(defun untrace* (r)
  (mapc #'untrace-one (or r (trace* nil))))

(defparameter *inside-trace* nil)

(defun trace-one (spec)
  (let* (break exitbreak (entrycond t) (exitcond t) entry exit
	       step (barfp t) fname oldf)
    (cond ((si::valid-function-name-p spec)
	   (setq fname spec))
          ((not (si::proper-list-p spec))
           (error "Not a valid argument to TRACE: ~S" spec))
	  ((si::valid-function-name-p (first spec))
	   (setq fname (first spec))
	   (do ((specs (cdr spec) (cdr specs)))
	       ((null specs))
	     (case (car specs)
	       (:break (setq barfp specs specs (cdr specs) break (car specs)))
	       (:break-after (setq barfp specs specs (cdr specs) exitbreak (car specs)))
	       (:step (setq step t))
	       (:cond (setq barfp specs specs (cdr specs))
		      (setq exitcond (setq entrycond (car specs))))
	       (:cond-before (setq barfp specs specs (cdr specs) entrycond (car specs)))
	       (:cond-after (setq barfp specs specs (cdr specs) exitcond (car specs)))
	       (:print (setq barfp specs specs (cdr specs) entry (car specs)))
	       (:print-after (setq barfp specs specs (cdr specs) exit (car specs)))
	       (t (error "Meaningless TRACE keyword: ~S" (car specs))))
	     (unless barfp (error "Parameter missing"))))
	  ((si::proper-list-p (first spec))
	   (let (results)
             (dolist (fname (first spec))
               (push (trace-one (list* fname (rest spec))) results))
             (return-from trace-one (nreverse results))))
          (t
           (error "Not a valid argument to TRACE: ~S" spec)))
    (when (null (fboundp fname))
      (warn "The function ~S is not defined." fname)
      (return-from trace-one nil))
    (when (symbolp fname)
      (when (special-operator-p fname)
	(warn "Unable to trace special form ~S." fname)
	(return-from trace-one nil))
      (when (macro-function fname)
	(warn "Unable to trace macro ~S." fname)
	(return-from trace-one nil)))
    (let ((record (trace-record fname)))
      (when record
        (cond ((traced-and-redefined-p record)
               (delete-from-trace-list fname))
              (t
               (warn "The function ~S is already traced." fname)
               (return-from trace-one nil)))))
    (setq oldf (fdefinition fname))
    (eval
     `(defun ,fname (&rest args)
	  (let* (values (*trace-level* (1+ *trace-level*)))
	    (if *inside-trace*
		(setq values (multiple-value-list (apply ',oldf args)))
		(let ((*inside-trace* t))
		  ,@(when entrycond
		       (if (eq t entrycond)
			 `((trace-print 'ENTER ',fname args ,@entry))
			 `((when ,entrycond
			     (trace-print 'ENTER ',fname args ,@entry)))))
		  ,@(when break
		      `((when ,break (let (*inside-trace*)
				       (break "tracing ~S" ',fname)))))
		  (setq values
			(let (*inside-trace*)
			  (multiple-value-list
			      (apply ',oldf args)
			      #+nil
			      ,(if step
				   `(let (*step-quit*)
				     (applyhook ',oldf args #'stepper nil))
				   `(apply ',oldf args)))))
		  ,@(when exitcond
		      (if (eq t exitcond)
			  `((trace-print 'EXIT ',fname values ,@exit))
			  `((when ,exitcond
			      (trace-print 'EXIT ',fname values ,@exit)))))
		  ,@(when exitbreak
		      `((when ,exitbreak
			  (let (*inside-trace*)
			    (break "after tracing ~S" ',fname)))))))
	    (values-list values))))
  (add-to-trace-list fname oldf)
  (list fname)))

(defun trace-print (direction fname vals &rest extras)
  (let ((indent (min (* (1- *trace-level*) 2) *trace-max-indent*))
        (*print-circle* t))
    (princ
      (with-output-to-string (*trace-output*)
        (fresh-line *trace-output*)
        (case direction
          (ENTER
            (multiple-value-bind (bars rem)
              (floor indent 4)
              (dotimes (i bars) (princ (if (< i 10) "|   " "|    ") *trace-output*))
              (when (plusp rem) (format *trace-output* "~V,,,' A" rem "|")))
            (format *trace-output*
                    "~D> (~S~{ ~S~})~%"
                    *trace-level* fname vals))
          (EXIT
            (multiple-value-bind (bars rem)
              (floor indent 4)
              (dotimes (i bars) (princ "|   " *trace-output*))
              (when (plusp rem) (format *trace-output* "~V,,,' A" rem "|")))
            (format *trace-output*
                    "<~D (~S~{ ~S~})~%"
                    *trace-level*
                    fname vals)
            ))
        (when extras
          (multiple-value-bind (bars rem)
            (floor indent 4)
            (dotimes (i bars) (princ "|   " *trace-output*))
            (when (plusp rem) (format *trace-output* "~V,,,' A" rem "|")))
          (format *trace-output*
                  "~0,4@T\\\\ ~{ ~S~}~%"
                  extras))
        *trace-output*))))

(defun trace-record (fname)
  (declare (si::c-local))
  (find fname *trace-list* :key #'first :test #'equal))

(defun trace-record-name (record)
  (declare (si::c-local))
  (first record))

(defun trace-record-definition (record)
  (declare (si::c-local))
  (second record))

(defun trace-record-old-definition (record)
  (declare (si::c-local))
  (third record))

(defun traced-old-definition (fname)
  (let ((record (trace-record fname)))
    (when record
      (unless (traced-and-redefined-p record)
        (trace-record-old-definition record)))))

(defun delete-from-trace-list (fname)
  (setq *trace-list* (delete fname *trace-list* :key #'first :test #'equal)))

(defun add-to-trace-list (fname old-definition)
  (push (list fname (fdefinition fname) old-definition)
        *trace-list*))

(defun traced-and-redefined-p (record)
  (declare (si::c-local))
  (and record (not (eq (trace-record-definition record)
                       (fdefinition (trace-record-name record))))))

(defun untrace-one (fname)
  (let ((record (trace-record fname)))
    (cond ((null record)
           (warn "The function ~S was not traced." fname))
          ((traced-and-redefined-p record)
           (warn "The function ~S was traced, but redefined." fname))
          (t
           (sys:fset fname (trace-record-old-definition record))))
    (delete-from-trace-list fname)
    (values)))

(defparameter *step-level* 0)
(defparameter *step-action* nil)
(defparameter *step-form* nil)
(defparameter *step-tag* (cons nil nil))
(defparameter *step-functions* nil)
(defconstant step-commands
  `("Stepper commands"
     ((:newline) (step-next) :constant
      "newline		Advance to the next form"
      "newline						[Stepper command]~@
	~@
	Step to next form.~%")
     ((:s :skip) step-skip nil
      ":s(kip)		Skip current form or until function"
      ":skip &optional arg				[Stepper command]~@
	:s &optional arg				[Abbreviation]~@
	~@
	Continue evaluation without stepping.  Without argument, resume
	stepping after the current form.  With numeric argument (n),
	resume stepping at the n-th level above.  With function name, resume
	when given function is called.~%")
     ((:pr :print) (step-print) :constant
      ":pr(int)	Pretty print current form"
      ":print						[Stepper command]~@
	:p						[Abbreviation]~@
	~@
	Pretty print current form.~%")
     ((:form) *step-form* :constant
      ":form		Current form"
      ":form						[Stepper command]~@
	~@
	Return the current form.  Nothing is done, but the current form~@
	is returned as the value of this command.  As a consequence,~@
	it is printed by the top level in the usual way and saved in~@
	the variable *.  The main purpose of this command is to allow~@
	the current form to be examined further by accessing *.~%")
     ((:x :exit) (step-quit) :constant
      ":x or :exit	Finish evaluation and exit stepper"
      ":exit						[Stepper command]~@
       :x						[Abbreviation]~@
       ~@
       Finish evaluation without stepping.~%")
     ))

(defmacro step (form)
"Syntax: (step form)
Evaluates FORM in the Stepper mode and returns all its values.  See ECL Report
for Stepper mode commands."
  `(step* ',form))

(defun step* (form)
  (let* ((*step-action* t)
	 (*step-level* 0)
	 (*step-functions* (make-hash-table :size 128 :test 'eq)))
    (catch *step-tag*
      (si:eval-with-env form nil t))))

(defun steppable-function (form)
  (let ((*step-action* nil))
    (or (gethash form *step-functions*)
	(multiple-value-bind (f env name)
	    (function-lambda-expression form)
	  (if (and (not (trace-record name)) f)
	      (setf (gethash form *step-functions*)
		    (eval-with-env `(function ,f) env t))
	      form)))))

(defun stepper (form)
  (when (typep form '(or symbol function))
    (return-from stepper (steppable-function (coerce form 'function))))
  (let* ((*step-form* form)
	 (*step-action* nil)
	 (indent (min (* *tpl-level* 2) 20))
	 prompt)
    (setq prompt
	  #'(lambda ()
	      (format *debug-io* "~VT" indent)
	      (write form :stream *debug-io* :pretty nil
		     :level 2 :length 2)
	      (princ #\space *debug-io*)
	      (princ #\- *debug-io*)))
    (when (catch *step-tag*
	    (tpl :quiet t
		 :commands (adjoin step-commands
				   (adjoin break-commands *tpl-commands*))
		 :broken-at 'stepper
		 :prompt-hook prompt))
      (throw *step-tag* t))))

(defun step-next ()
  (throw *step-tag* nil))

(defun step-skip (&optional (when 0))
  (setf *step-action* 0)
  (throw *step-tag* nil))

(defun step-print ()
  (write *step-form* :stream *debug-io* :pretty t :level nil :length nil)
  (terpri)
  (values))

(defun step-quit ()
  (throw *step-tag* t))
