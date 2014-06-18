#|----------------------------------------------------------------------------|
 | Copyright 1990 by the Massachusetts Institute of Technology, Cambridge MA. |
 |                                                                            |
 | Permission  to  use,  copy, modify, and distribute this software  and  its |
 | documentation for any purpose  and without fee is hereby granted, provided |
 | that this copyright  and  permission  notice  appear  in  all  copies  and |
 | supporting  documentation,  and  that  the  name  of M.I.T. not be used in |
 | advertising or  publicity  pertaining  to  distribution  of  the  software |
 | without   specific,   written   prior   permission.      M.I.T.  makes  no |
 | representations  about  the  suitability of this software for any purpose. |
 | It is provided "as is" without express or implied warranty.                |
 |                                                                            |
 |  M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,  INCLUDING  |
 |  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL  |
 |  M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAMAGES  OR  |
 |  ANY  DAMAGES  WHATSOEVER  RESULTING  FROM  LOSS OF USE, DATA OR PROFITS,  |
 |  WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER  TORTIOUS  ACTION,  |
 |  ARISING  OUT  OF  OR  IN  CONNECTION WITH THE USE OR PERFORMANCE OF THIS  |
 |  SOFTWARE.                                                                 |
 |----------------------------------------------------------------------------|#

(defpackage :sb-rt
  (:nicknames :rt :regression-test :rtest)
  (:use #:cl)
  (:export #:*do-tests-when-defined* #:*test* #:continue-testing
	   #:deftest #:do-test #:do-tests #:get-test #:pending-tests
	   #:rem-all-tests #:rem-test)
  (:documentation "The MIT regression tester"))

(in-package :sb-rt)

(defvar *test* nil "Current test name")
(defvar *do-tests-when-defined* nil)
(defvar *entries* '(nil) "Test database")
(defvar *in-test* nil "Used by TEST")
(defvar *debug* nil "For debugging")
(defvar *catch-errors* t
  "When true, causes errors in a test to be caught.")
(defvar *print-circle-on-failure* nil
  "Failure reports are printed with *PRINT-CIRCLE* bound to this value.")
(defvar *compile-tests* nil
  "When true, compile the tests before running them.")
(defvar *optimization-settings* '((safety 3)))
(defvar *expected-failures* nil
  "A list of test names that are expected to fail.")

(defstruct (entry (:conc-name nil)
		  (:type list))
  pend name form)

(defmacro vals (entry) `(cdddr ,entry))

(defmacro defn (entry) `(cdr ,entry))

(defun pending-tests ()
  (do ((l (cdr *entries*) (cdr l))
       (r nil))
      ((null l) (nreverse r))
    (when (pend (car l))
      (push (name (car l)) r))))

(defun rem-all-tests ()
  (setq *entries* (list nil))
  nil)

(defun rem-test (&optional (name *test*))
  (do ((l *entries* (cdr l)))
      ((null (cdr l)) nil)
    (when (equal (name (cadr l)) name)
      (setf (cdr l) (cddr l))
      (return name))))

(defun get-test (&optional (name *test*))
  (defn (get-entry name)))

(defun get-entry (name)
  (let ((entry (find name (cdr *entries*)
		     :key #'name
		     :test #'equal)))
    (when (null entry)
      (report-error t
        "~%No test with name ~:@(~S~)."
	name))
    entry))

(defmacro deftest (name form &rest values)
  `(add-entry '(t ,name ,form .,values)))

(defun add-entry (entry)
  (setq entry (copy-list entry))
  (do ((l *entries* (cdr l))) (nil)
    (when (null (cdr l))
      (setf (cdr l) (list entry))
      (return nil))
    (when (equal (name (cadr l)) 
		 (name entry))
      (setf (cadr l) entry)
      (report-error nil
        "Redefining test ~:@(~S~)"
        (name entry))
      (return nil)))
  (when *do-tests-when-defined*
    (do-entry entry))
  (setq *test* (name entry)))

(defun report-error (error? &rest args)
  (cond (*debug* 
	 (apply #'format t args)
	 (if error? (throw '*debug* nil)))
	(error? (apply #'error args))
	(t (apply #'warn args))))

(defun do-test (&optional (name *test*))
  (do-entry (get-entry name)))

(defun equalp-with-case (x y)
  "Like EQUALP, but doesn't do case conversion of characters."
  (cond
   ((eq x y) t)
   ((consp x)
    (and (consp y)
	 (equalp-with-case (car x) (car y))
	 (equalp-with-case (cdr x) (cdr y))))
   ((and (typep x 'array)
	 (= (array-rank x) 0))
    (equalp-with-case (aref x) (aref y)))
   ((typep x 'vector)
    (and (typep y 'vector)
	 (let ((x-len (length x))
	       (y-len (length y)))
	   (and (eql x-len y-len)
		(loop
		 for e1 across x
		 for e2 across y
		 always (equalp-with-case e1 e2))))))
   ((and (typep x 'array)
	 (typep y 'array)
	 (not (equal (array-dimensions x)
		     (array-dimensions y))))
    nil)
   ((typep x 'array)
    (and (typep y 'array)
	 (let ((size (array-total-size x)))
	   (loop for i from 0 below size
		 always (equalp-with-case (row-major-aref x i)
					  (row-major-aref y i))))))
   (t (eql x y))))

(defun do-entry (entry &optional
		       (s *standard-output*))
  (catch '*in-test*
    (setq *test* (name entry))
    (setf (pend entry) t)
    (let* ((*in-test* t)
	   ;; (*break-on-warnings* t)
	   (aborted nil)
	   r)
      ;; (declare (special *break-on-warnings*))

      (block aborted
	(setf r
	      (flet ((%do
		      ()
		      (if *compile-tests*
			  (multiple-value-list
			   (funcall (compile
				     nil
				     `(lambda ()
				       (declare
					(optimize ,@*optimization-settings*))
				       ,(form entry)))))
			  (multiple-value-list
			   (eval (form entry))))))
		(if *catch-errors*
		    (handler-bind
			((style-warning #'muffle-warning)
			 (error #'(lambda (c)
				    (setf aborted t)
				    (setf r (list c))
				    (return-from aborted nil))))
		      (%do))
		  (%do)))))

      (setf (pend entry)
	    (or aborted
		(not (equalp-with-case r (vals entry)))))
      
      (when (pend entry)
	(let ((*print-circle* *print-circle-on-failure*))
	  (format s "~&Test ~:@(~S~) failed~
                   ~%Form: ~S~
                   ~%Expected value~P: ~
                      ~{~S~^~%~17t~}~%"
		  *test* (form entry)
		  (length (vals entry))
		  (vals entry))
	  (format s "Actual value~P: ~
                      ~{~S~^~%~15t~}.~%"
		  (length r) r)
	  (let ((x (first r)))
	    (when (typep x 'condition)
	      (format s "~&Condition: ~A" x)))
	  ))))
  (when (not (pend entry)) *test*))

(defun continue-testing ()
  (if *in-test*
      (throw '*in-test* nil)
      (do-entries *standard-output*)))

(defun do-tests (&optional
		 (out *standard-output*))
  (dolist (entry (cdr *entries*))
    (setf (pend entry) t))
  (if (streamp out)
      (do-entries out)
      (with-open-file 
	  (stream out :direction :output)
	(do-entries stream))))

(defun do-entries (s)
  (format s "~&Doing ~A pending test~:P ~
             of ~A tests total.~%"
          (count t (cdr *entries*)
		 :key #'pend)
	  (length (cdr *entries*)))
  (dolist (entry (cdr *entries*))
    (when (pend entry)
      (format s "~@[~<~%~:; ~:@(~S~)~>~]"
	      (do-entry entry s))))
  (let ((pending (pending-tests))
	(expected-table (make-hash-table :test #'equal)))
    (dolist (ex *expected-failures*)
      (setf (gethash ex expected-table) t))
    (let ((new-failures
	   (loop for pend in pending
		 unless (gethash pend expected-table)
		 collect pend)))
      (if (null pending)
	  (format s "~&No tests failed.")
	(progn
	  (format s "~&~A out of ~A ~
                   total tests failed: ~
                   ~:@(~{~<~%   ~1:;~S~>~
                         ~^, ~}~)."
		  (length pending)
		  (length (cdr *entries*))
		  pending)
	  (if (null new-failures)
	      (format s "~&No unexpected failures.")
	    (when *expected-failures*
	      (format s "~&~A unexpected failures: ~
                   ~:@(~{~<~%   ~1:;~S~>~
                         ~^, ~}~)."
		    (length new-failures)
		    new-failures)))
	  ))
      (finish-output s)
      (null pending))))

(provide '#:rt)
