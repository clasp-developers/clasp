#|#########################################################################
# Copyright (C) 1997-2011 Ufasoft                                         #
# http://ufasoft.com   mailto:support@ufasoft.com                         #
#                                                                         #
# This program is free software; you can redistribute it and/or modify it #
# under the terms of the GNU General Public License as published by the   #
# Free Software Foundation; either version 3, or (at your option) any     #
# later version.                                                          #
#                                                                         #
# This program is distributed in the hope that it will be useful,         #
# but WITHOUT ANY WARRANTY; without even the implied warranty of          #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           #
# GNU General Public License for more details.                            #
#                                                                         #
# You should have received a copy of the GNU General Public License       #
# along with this program; If not, see <http://www.gnu.org/licenses/>     #
########################################################################=|#

(in-package "SYS")

;;; CASE-BODY returns code for all the standard "case" macros.  Name is the
;;; macro name, and keyform is the thing to case on.  Multi-p indicates whether
;;; a branch may fire off a list of keys; otherwise, a key that is a list is
;;; interpreted in some way as a single key.  When multi-p, test is applied to
;;; the value of keyform and each key for a given branch; otherwise, test is
;;; applied to the value of keyform and the entire first element, instead of
;;; each part, of the case branch.  When errorp, no t or otherwise branch is
;;; permitted, and an ERROR form is generated.  When proceedp, it is an error
;;; to omit errorp, and the ERROR form generated is executed within a
;;; RESTART-CASE allowing keyform to be set and retested.
;;;
(defun case-body (name keyform cases multi-p test errorp proceedp)
  (let ((keyform-value (gensym))
	(clauses ())
	(keys ()))
    (dolist (case cases)
      (cond ((atom case)
	     (error "~S -- Bad clause in ~S." case name))
	    ((memq (car case) '(t otherwise))
	     (if errorp
		 (error "No default clause allowed in ~S: ~S" name case)
		 (push `(t nil ,@(rest case)) clauses)))
	    ((and multi-p (listp (first case)))
	     (setf keys (append (first case) keys))
	     (push `((or ,@(mapcar #'(lambda (key)
				       `(,test ,keyform-value ',key))
				   (first case)))
		     nil ,@(rest case))
		   clauses))
	    (t
	     (push (first case) keys)
	     (push `((,test ,keyform-value
			    ',(first case)) nil ,@(rest case)) clauses))))
    (case-body-aux name keyform keyform-value clauses keys errorp proceedp
		   `(,(if multi-p 'member 'or) ,@keys))))

;;; CASE-BODY-AUX provides the expansion once CASE-BODY has groveled all the
;;; cases.  Note: it is not necessary that the resulting code signal
;;; case-failure conditions, but that's what KMP's prototype code did.  We call
;;; CASE-BODY-ERROR, because of how closures are compiled.  RESTART-CASE has
;;; forms with closures that the compiler causes to be generated at the top of
;;; any function using the case macros, regardless of whether they are needed.
;;;
(defun case-body-aux (name keyform keyform-value clauses keys
		      errorp proceedp expected-type)
  (if proceedp
      (let ((block (gensym))
	    (again (gensym)))
	`(let ((,keyform-value ,keyform))
	   (block ,block
	     (tagbody
	      ,again
	      (return-from
	       ,block
	       (cond ,@(nreverse clauses)
		     (t
		      (setf ,keyform-value
			    (setf ,keyform
				  (case-body-error
				   ',name ',keyform ,keyform-value
				   ',expected-type ',keys)))
		      (go ,again))))))))
      `(let ((,keyform-value ,keyform))
	 ,keyform-value ; prevent warnings when key not used eg (case key (t))
	 (cond
	  ,@(nreverse clauses)
	  ,@(if errorp
		`((t (error 'case-failure
			    :name ',name
			    :datum ,keyform-value
			    :expected-type ',expected-type
			    :possibilities ',keys))))))))


(defun case-body-error (name keyform keyform-value expected-type keys)
  (restart-case
      (error 'case-failure
	     :name name
	     :datum keyform-value
	     :expected-type expected-type
	     :possibilities keys)
    (store-value (value)
      :report (lambda (stream)
		(format stream "Supply a new value for ~S." keyform))
      :interactive read-evaluated-form
      value)))

#|!!!
(defmacro case (keyform &body cases)
  "CASE Keyform {({(Key*) | Key} Form*)}*
  Evaluates the Forms in the first clause with a Key EQL to the value of
  Keyform.  If a singleton key is T then the clause is a default clause."
  (case-body 'case keyform cases t 'eql nil nil))
  |#

(defmacro ccase (keyform &body cases)
  "CCASE Keyform {({(Key*) | Key} Form*)}*
  Evaluates the Forms in the first clause with a Key EQL to the value of
  Keyform.  If none of the keys matches then a correctable error is
  signalled."
  (case-body 'ccase keyform cases t 'eql t t))

(defmacro ecase (keyform &body cases)
  "ECASE Keyform {({(Key*) | Key} Form*)}*
  Evaluates the Forms in the first clause with a Key EQL to the value of
  Keyform.  If none of the keys matches then an error is signalled."
  (case-body 'ecase keyform cases t 'eql t nil))

(defmacro typecase (keyform &body cases)
  "TYPECASE Keyform {(Type Form*)}*
  Evaluates the Forms in the first clause for which TYPEP of Keyform and Type
  is true."
  (case-body 'typecase keyform cases nil 'typep nil nil))

(defmacro ctypecase (keyform &body cases)
  "CTYPECASE Keyform {(Type Form*)}*
  Evaluates the Forms in the first clause for which TYPEP of Keyform and Type
  is true.  If no form is satisfied then a correctable error is signalled."
  (case-body 'ctypecase keyform cases nil 'typep t t))

(defmacro etypecase (keyform &body cases)
  "ETYPECASE Keyform {(Type Form*)}*
  Evaluates the Forms in the first clause for which TYPEP of Keyform and Type
  is true.  If no form is satisfied then an error is signalled."
  (case-body 'etypecase keyform cases nil 'typep t nil))

