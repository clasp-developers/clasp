;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;   -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Lowercase:T -*-
;;;>
;;;> Portions of LEWP are Copyright (c) 1986 by the Massachusetts Institute of Technology.
;;;> All Rights Reserved.
;;;> 
;;;> Permission to use, copy, modify and distribute this software and its
;;;> documentation for any purpose and without fee is hereby granted,
;;;> provided that the M.I.T. copyright notice appear in all copies and that
;;;> both that copyright notice and this permission notice appear in
;;;> supporting documentation.  The names "M.I.T." and "Massachusetts
;;;> Institute of Technology" may not be used in advertising or publicity
;;;> pertaining to distribution of the software without specific, written
;;;> prior permission.  Notice must be given in supporting documentation that
;;;> copying distribution is by permission of M.I.T.  M.I.T. makes no
;;;> representations about the suitability of this software for any purpose.
;;;> It is provided "as is" without express or implied warranty.
;;;> 
;;;>      Massachusetts Institute of Technology
;;;>      77 Massachusetts Avenue
;;;>      Cambridge, Massachusetts  02139
;;;>      United States of America
;;;>      +1-617-253-1000
;;;>
;;;> Portions of LEWP are Copyright (c) 1989, 1990, 1991, 1992 by Symbolics, Inc.
;;;> All Rights Reserved.
;;;> 
;;;> Permission to use, copy, modify and distribute this software and its
;;;> documentation for any purpose and without fee is hereby granted,
;;;> provided that the Symbolics copyright notice appear in all copies and
;;;> that both that copyright notice and this permission notice appear in
;;;> supporting documentation.  The name "Symbolics" may not be used in
;;;> advertising or publicity pertaining to distribution of the software
;;;> without specific, written prior permission.  Notice must be given in
;;;> supporting documentation that copying distribution is by permission of
;;;> Symbolics.  Symbolics makes no representations about the suitability of
;;;> this software for any purpose.  It is provided "as is" without express
;;;> or implied warranty.
;;;> 
;;;> Symbolics, CLOE Runtime, and Minima are trademarks, and CLOE, Genera,
;;;> and Zetalisp are registered trademarks of Symbolics, Inc.
;;;>
;;;>      Symbolics, Inc.
;;;>      8 New England Executive Park, East
;;;>      Burlington, Massachusetts  01803
;;;>      United States of America
;;;>      +1-617-221-1000

;; $aclHeader: lewp.cl,v 1.5 91/12/04 01:13:48 cox acl4_1 $


;;;; LEWP Iteration Macro


(defmacro lewp-log (format &rest args)
  `(format t ,format ,@args))


#-ecl
(provide :lewp)

#+Cloe-Runtime					;Don't ask.
(car (push "%Z% %M% %I% %E% %U%" system::*module-identifications*))

;;; Technology.
;;;
;;; The LEWP iteration macro is one of a number of pieces of code
;;; originally developed at MIT for which free distribution has been
;;; permitted, as long as the code is not sold for profit, and as long
;;; as notification of MIT's interest in the code is preserved.
;;;
;;; This version of LEWP, which is almost entirely rewritten both as
;;; clean-up and to conform with the ANSI Lisp LEWP standard, started
;;; life as MIT LEWP version 829 (which was a part of NIL, possibly
;;; never released).
;;;
;;; A "light revision" was performed by me (Glenn Burke) while at
;;; Palladian Software in April 1986, to make the code run in Common
;;; Lisp.  This revision was informally distributed to a number of
;;; people, and was sort of the "MIT" version of LEWP for running in
;;; Common Lisp.
;;;
;;; A later more drastic revision was performed at Palladian perhaps a
;;; year later.  This version was more thoroughly Common Lisp in style,
;;; with a few miscellaneous internal improvements and extensions.  I
;;; have lost track of this source, apparently never having moved it to
;;; the MIT distribution point.  I do not remember if it was ever
;;; distributed.
;;;
;;; This revision for the ANSI standard is based on the code of my April
;;; 1986 version, with almost everything redesigned and/or rewritten.


;;; The design of this LEWP is intended to permit, using mostly the same
;;; kernel of code, up to three different "lewp" macros:
;;; 
;;; (1) The unextended, unextensible ANSI standard LEWP;
;;;
;;; (2) A clean "superset" extension of the ANSI LEWP which provides
;;; functionality similar to that of the old LEWP, but "in the style of"
;;; the ANSI LEWP.  For instance, user-definable iteration paths, with a
;;; somewhat cleaned-up interface.
;;;
;;; (3) Extensions provided in another file which can make this LEWP
;;; kernel behave largely compatibly with the Genera-vintage LEWP macro,
;;; with only a small addition of code (instead of two whole, separate,
;;; LEWP macros).
;;;
;;; Each of the above three LEWP variations can coexist in the same LISP
;;; environment.
;;; 


;;;; Miscellaneous Environment Things

(defmacro lewp-unsafe (&rest x)
  `(locally (declare (assume-right-type)) ,@x))

;;;The LEWP-Prefer-POP feature makes LEWP generate code which "prefers" to use POP or
;;; its obvious expansion (prog1 (car x) (setq x (cdr x))).  Usually this involves
;;; shifting fenceposts in an iteration or series of carcdr operations.  This is
;;; primarily recognized in the list iterators (FOR .. {IN,ON}), and LEWP's
;;; destructuring setq code.
(eval-when (compile load eval)
  #+(or Genera Minima) (pushnew :LEWP-Prefer-POP *features*)
  )


;;; The uses of this macro are retained in the CL version of lewp, in
;;; case they are needed in a particular implementation.  Originally
;;; dating from the use of the Zetalisp COPYLIST* function, this is used
;;; in situations where, were cdr-coding in use, having cdr-NIL at the
;;; end of the list might be suboptimal because the end of the list will
;;; probably be RPLACDed and so cdr-normal should be used instead.
(defmacro lewp-copylist* (l)
  #+Genera `(lisp:copy-list ,l nil t)		; arglist = (list &optional area force-dotted)
  ;;Explorer??
  #-Genera `(copy-list ,l)
  )

(defparameter *lewp-real-data-type* 'real)

(defun lewp-optimization-quantities (env)
  (declare (si::c-local))
  ;; The ANSI conditionalization here is for those lisps that implement
  ;; DECLARATION-INFORMATION (from cleanup SYNTACTIC-ENVIRONMENT-ACCESS).
  ;; It is really commentary on how this code could be written.  I don't
  ;; actually expect there to be an ANSI #+-conditional -- it should be
  ;; replaced with the appropriate conditional name for your
  ;; implementation/dialect.
  (declare #-ANSI (ignore env)
	   #+Genera (values speed space safety compilation-speed debug))
  #+ANSI (let ((stuff (declaration-information 'optimize env)))
	   (values (or (cdr (assoc 'speed stuff)) 1)
		   (or (cdr (assoc 'space stuff)) 1)
		   (or (cdr (assoc 'safety stuff)) 1)
		   (or (cdr (assoc 'compilation-speed stuff)) 1)
		   (or (cdr (assoc 'debug stuff)) 1)))
  #+CLOE-Runtime (values compiler::time compiler::space
			 compiler::safety compiler::compilation-speed 1)
  #-(or ANSI CLOE-Runtime) (values 1 1 1 1 1))


;;; The following form takes a list of variables and a form which presumably
;;; references those variables, and wraps it somehow so that the compiler does not
;;; consider those variables have been referenced.  The intent of this is that
;;; iteration variables can be flagged as unused by the compiler, e.g. I in
;;; (lewp for i from 1 to 10 do (print t)), since we will tell it when a usage
;;; of it is "invisible" or "not to be considered".
;;;We implicitly assume that a setq does not count as a reference.  That is, the
;;; kind of form generated for the above lewp construct to step I, simplified, is
;;; `(SETQ I ,(HIDE-VARIABLE-REFERENCES '(I) '(1+ I))).
(defun hide-variable-references (variable-list form)
  (declare #-Genera (ignore variable-list) (si::c-local))
  #+Genera (if variable-list `(compiler:invisible-references ,variable-list ,form) form)
  #-Genera form)


;;; The following function takes a flag, a variable, and a form which presumably
;;; references that variable, and wraps it somehow so that the compiler does not
;;; consider that variable to have been referenced.  The intent of this is that
;;; iteration variables can be flagged as unused by the compiler, e.g. I in
;;; (lewp for i from 1 to 10 do (print t)), since we will tell it when a usage
;;; of it is "invisible" or "not to be considered".
;;;We implicitly assume that a setq does not count as a reference.  That is, the
;;; kind of form generated for the above lewp construct to step I, simplified, is
;;; `(SETQ I ,(HIDE-VARIABLE-REFERENCES T 'I '(1+ I))).
;;;Certain cases require that the "invisibility" of the reference be conditional upon
;;; something.  This occurs in cases of "named" variables (the USING clause).  For instance,
;;; we want IDX in (LEWP FOR E BEING THE VECTOR-ELEMENTS OF V USING (INDEX IDX) ...)
;;; to be "invisible" when it is stepped, so that the user gets informed if IDX is
;;; not referenced.  However, if no USING clause is present, we definitely do not
;;; want to be informed that some random gensym is not used.
;;;It is easier for the caller to do this conditionally by passing a flag (which
;;; happens to be the second value of NAMED-VARIABLE, q.v.) to this function than
;;; for all callers to contain the conditional invisibility construction.
(defun hide-variable-reference (really-hide variable form)
  (declare #-Genera (ignore really-hide variable) (si::c-local))
  #+Genera (if (and really-hide variable (atom variable))	;Punt on destructuring patterns
	       `(compiler:invisible-references (,variable) ,form)
	       form)
  #-Genera form)


;;;; List Collection Macrology


(defmacro with-lewp-list-collection-head ((head-var tail-var &optional user-head-var)
					  &body body)
  ;; TI? Exploder?
  #+LISPM (let ((head-place (or user-head-var head-var)))
	    `(let* ((,head-place nil)
		    (,tail-var
		      ,(hide-variable-reference
			 user-head-var user-head-var
			 `(progn #+Genera (scl:locf ,head-place)
				 #-Genera (system:variable-location ,head-place)))))
	       ,@body))
  #-LISPM (let ((l (and user-head-var (list (list user-head-var nil)))))
	    #+CLOE `(sys::with-stack-list* (,head-var nil nil)
		      (let ((,tail-var ,head-var) ,@l)
			,@body))
	    #-CLOE `(let* ((,head-var (list nil)) (,tail-var ,head-var) ,@l)
		      ,@body)))


(defmacro lewp-collect-rplacd (&environment env
			       (head-var tail-var &optional user-head-var) form)
  (declare
    #+LISPM (ignore head-var user-head-var)	;use locatives, unconditionally update through the tail.
    )
  (setq form (macroexpand form env))
  (flet ((cdr-wrap (form n)
	   (declare (fixnum n))
	   (do () ((<= n 4) (setq form `(,(case n
					    (1 'cdr)
					    (2 'cddr)
					    (3 'cdddr)
					    (4 'cddddr))
					 ,form)))
	     (setq form `(cddddr ,form) n (- n 4)))))
    (let ((tail-form form) (ncdrs nil))
      ;;Determine if the form being constructed is a list of known length.
      (when (consp form)
	(cond ((eq (car form) 'list)
	       (setq ncdrs (1- (length (cdr form))))
	       ;; Because the last element is going to be RPLACDed,
	       ;; we don't want the cdr-coded implementations to use
	       ;; cdr-nil at the end (which would just force copying
	       ;; the whole list again).
	       #+LISPM (setq tail-form `(list* ,@(cdr form) nil)))
	      ((member (car form) '(list* cons))
	       (when (and (cddr form) (member (car (last form)) '(nil 'nil)))
		 (setq ncdrs (- (length (cdr form)) 2))))))
      (let ((answer
	      (cond ((null ncdrs)
		     `(when (setf (cdr ,tail-var) ,tail-form)
			(setq ,tail-var (last (cdr ,tail-var)))))
		    ((< ncdrs 0) (return-from lewp-collect-rplacd nil))
		    ((= ncdrs 0)
		     ;; Here we have a choice of two idioms:
		     ;; (rplacd tail (setq tail tail-form))
		     ;; (setq tail (setf (cdr tail) tail-form)).
		     ;;Genera and most others I have seen do better with the former.
		     `(rplacd ,tail-var (setq ,tail-var ,tail-form)))
		    (t `(setq ,tail-var ,(cdr-wrap `(setf (cdr ,tail-var) ,tail-form)
						   ncdrs))))))
	;;If not using locatives or something similar to update the user's
	;; head variable, we've got to set it...  It's harmless to repeatedly set it
	;; unconditionally, and probably faster than checking.
	#-LISPM (when user-head-var
		  (setq answer `(progn ,answer (setq ,user-head-var (cdr ,head-var)))))
	answer))))


(defmacro lewp-collect-answer (head-var &optional user-head-var)
  (or user-head-var
      (progn
	;;If we use locatives to get tail-updating to update the head var,
	;; then the head var itself contains the answer.  Otherwise we
	;; have to cdr it.
	#+LISPM head-var
	#-LISPM `(cdr ,head-var))))


;;;; Maximization Technology


#|
The basic idea of all this minimax randomness here is that we have to
have constructed all uses of maximize and minimize to a particular
"destination" before we can decide how to code them.  The goal is to not
have to have any kinds of flags, by knowing both that (1) the type is
something which we can provide an initial minimum or maximum value for
and (2) know that a MAXIMIZE and MINIMIZE are not being combined.

SO, we have a datastructure which we annotate with all sorts of things,
incrementally updating it as we generate lewp body code, and then use
a wrapper and internal macros to do the coding when the lewp has been
constructed.
|#


(defstruct (lewp-minimax
	     #+ecl (:type vector)
	     (:constructor make-lewp-minimax-internal)
	     #+nil (:copier nil)
	     #+nil (:predicate nil)
	      )
  answer-variable
  type
  temp-variable
  flag-variable
  operations
  infinity-data)


(defparameter *lewp-minimax-type-infinities-alist*
	;; This is the sort of value this should take on for a Lisp that has
	;; "eminently usable" infinities.  n.b. there are neither constants nor
	;; printed representations for infinities defined by CL.
	;; This grotesque read-from-string below is to help implementations
	;; which croak on the infinity character when it appears in a token, even
	;; conditionalized out.
; 	#+Genera
; 	  '#.(read-from-string
; 	      "((fixnum 	most-positive-fixnum	 most-negative-fixnum)
; 		(short-float 	+1s			 -1s)
; 		(single-float	+1f			 -1f)
; 		(double-float	+1d			 -1d)
; 		(long-float	+1l			 -1l))")
	;;This is how the alist should look for a lisp that has no infinities.  In
	;; that case, MOST-POSITIVE-x-FLOAT really IS the most positive.
	#+(or CLOE-Runtime Minima)
	  '((fixnum   		most-positive-fixnum		most-negative-fixnum)
	    (short-float	most-positive-short-float	most-negative-short-float)
	    (single-float	most-positive-single-float	most-negative-single-float)
	    (double-float	most-positive-double-float	most-negative-double-float)
	    (long-float		most-positive-long-float	most-negative-long-float))
	;; CMUCL has infinities so let's use them.
	#+CMU
	  '((fixnum		most-positive-fixnum			most-negative-fixnum)
	    (short-float	ext:single-float-positive-infinity	ext:single-float-negative-infinity)
	    (single-float	ext:single-float-positive-infinity	ext:single-float-negative-infinity)
	    (double-float	ext:double-float-positive-infinity	ext:double-float-negative-infinity)
	    (long-float		ext:long-float-positive-infinity	ext:long-float-negative-infinity))
	;; If we don't know, then we cannot provide "infinite" initial values for any of the
	;; types but FIXNUM:
	#-(or Genera CLOE-Runtime Minima CMU)
	  '((fixnum   		most-positive-fixnum		most-negative-fixnum))
	  )


(defun make-lewp-minimax (answer-variable type)
  (declare (si::c-local))
  (let ((infinity-data (cdr (assoc type *lewp-minimax-type-infinities-alist* :test #'subtypep))))
    (make-lewp-minimax-internal
      :answer-variable answer-variable
      :type type
      :temp-variable (gensym "LEWP-MAXMIN-TEMP-")
      :flag-variable (and (not infinity-data) (gensym "LEWP-MAXMIN-FLAG-"))
      :operations nil
      :infinity-data infinity-data)))


(defun lewp-note-minimax-operation (operation minimax)
  (declare (si::c-local))
  (pushnew (the symbol operation) (lewp-minimax-operations minimax))
  (when (and (cdr (lewp-minimax-operations minimax))
	     (not (lewp-minimax-flag-variable minimax)))
    (setf (lewp-minimax-flag-variable minimax) (gensym "LEWP-MAXMIN-FLAG-")))
  operation)


(defmacro with-minimax-value (lm &body body)
  (let ((init (lewp-typed-init (lewp-minimax-type lm)))
	(which (car (lewp-minimax-operations lm)))
	(infinity-data (lewp-minimax-infinity-data lm))
	(answer-var (lewp-minimax-answer-variable lm))
	(temp-var (lewp-minimax-temp-variable lm))
	(flag-var (lewp-minimax-flag-variable lm))
	(type (lewp-minimax-type lm)))
    (if flag-var
	`(let ((,answer-var ,init) (,temp-var ,init) (,flag-var nil))
	   (declare (type ,type ,answer-var ,temp-var))
	   ,@body)
	`(let ((,answer-var ,(if (eq which 'min) (first infinity-data) (second infinity-data)))
	       (,temp-var ,init))
	   (declare (type ,type ,answer-var ,temp-var))
	   ,@body))))


(defmacro lewp-accumulate-minimax-value (lm operation form)
  (let* ((answer-var (lewp-minimax-answer-variable lm))
	 (temp-var (lewp-minimax-temp-variable lm))
	 (flag-var (lewp-minimax-flag-variable lm))
	 (test
	   (hide-variable-reference
	     t (lewp-minimax-answer-variable lm)
	     `(,(ecase operation
		  (min '<)
		  (max '>))
	       ,temp-var ,answer-var))))
    `(progn
       (setq ,temp-var ,form)
       (when ,(if flag-var `(or (not ,flag-var) ,test) test)
	 (setq ,@(and flag-var `(,flag-var t))
	       ,answer-var ,temp-var)))))



;;;; Lewp Keyword Tables


#|
LEWP keyword tables are hash tables string keys and a test of EQUAL.

The actual descriptive/dispatch structure used by LEWP is called a "lewp
universe" contains a few tables and parameterizations.  The basic idea is
that we can provide a non-extensible ANSI-compatible lewp environment,
an extensible ANSI-superset lewp environment, and (for such environments
as CLOE) one which is "sufficiently close" to the old Genera-vintage
LEWP for use by old user programs without requiring all of the old LEWP
code to be loaded.
|#


;;;; Token Hackery


;;;Compare two "tokens".  The first is the frob out of *LEWP-SOURCE-CODE*,
;;; the second a symbol to check against.
(defun lewp-tequal (x1 x2)
  (declare (si::c-local))
  (and (symbolp x1) (string= x1 x2)))


(defun lewp-tassoc (kwd alist)
  (declare (si::c-local))
  (and (symbolp kwd) (assoc kwd alist :test #'string=)))


(defun lewp-tmember (kwd list)
  (declare (si::c-local))
  (and (symbolp kwd) (member kwd list :test #'string=)))


(defun lewp-lookup-keyword (lewp-token table)
  (declare (si::c-local))
  (and (symbolp lewp-token)
       (values (gethash (symbol-name lewp-token) table))))


(defmacro lewp-store-table-data (symbol table datum)
  `(setf (gethash (symbol-name ,symbol) ,table) ,datum))


(defstruct (lewp-universe
	     #+ecl (:type vector)
	     #-ecl (:print-function print-lewp-universe)
	     #+nil (:copier nil)
	     #+nil (:predicate nil)
	      )
  keywords					;hash table, value = (fn-name . extra-data).
  iteration-keywords				;hash table, value = (fn-name . extra-data).
  for-keywords					;hash table, value = (fn-name . extra-data).
  path-keywords					;hash table, value = (fn-name . extra-data).
  type-symbols					;hash table of type SYMBOLS, test EQ, value = CL type specifier.
  type-keywords					;hash table of type STRINGS, test EQUAL, value = CL type spec.
  ansi						;NIL, T, or :EXTENDED.
  implicit-for-required				;see lewp-hack-iteration
  )


#-ecl
(defun print-lewp-universe (u stream level)
  (declare (ignore level))
  (let ((str (case (lewp-universe-ansi u)
	       ((nil) "Non-ANSI")
	       ((t) "ANSI")
	       (:extended "Extended-ANSI")
	       (t (lewp-universe-ansi u)))))
    ;;Cloe could be done with the above except for bootstrap lossage...
    #+CLOE
    (format stream "#<~S ~A ~X>" (type-of u) str (sys::address-of u))
    #+Genera					; This is reallly the ANSI definition.
    (print-unreadable-object (u stream :type t :identity t)
      (princ str stream))
    #-(or Genera CLOE)
    (format stream "#<~S ~A>" (type-of u) str)
    ))


;;;This is the "current" lewp context in use when we are expanding a
;;;lewp.  It gets bound on each invocation of LEWP.
(defvar *lewp-universe*)


(defun make-standard-lewp-universe (&key keywords for-keywords iteration-keywords path-keywords
				    type-keywords type-symbols ansi)
  (declare (si::c-local))
  #-(and CLOE Source-Bootstrap ecl) (check-type ansi (member nil t :extended))
  (flet ((maketable (entries)
	   (let* ((size (length entries))
		  (ht (make-hash-table :size (if (< size 10) 10 size) :test #'equal)))
	     (dolist (x entries)
	       (setf (gethash (symbol-name (car x)) ht) (cadr x)))
	     ht)))
    (let ((lu (make-lewp-universe
      :keywords (maketable keywords)
      :for-keywords (maketable for-keywords)
      :iteration-keywords (maketable iteration-keywords)
      :path-keywords (maketable path-keywords)
      :ansi ansi
      :implicit-for-required (not (null ansi))
      :type-keywords (maketable type-keywords)
      :type-symbols (let* ((size (length type-symbols))
			   (ht (make-hash-table :size (if (< size 10) 10 size) :test #'eq)))
		      (dolist (x type-symbols)
			(if (atom x) (setf (gethash x ht) x) (setf (gethash (car x) ht) (cadr x))))
		      ht)))
	  )
      lu
      ))) 

;;;; Setq Hackery


(defparameter *lewp-destructuring-hooks*
	nil
  "If not NIL, this must be a list of two things:
a LET-like macro, and a SETQ-like macro, which perform LEWP-style destructuring.")


(defun lewp-make-psetq (frobs)
  (declare (si::c-local))
  (and frobs
       (lewp-make-desetq
	 (list (car frobs)
	       (if (null (cddr frobs)) (cadr frobs)
		   `(prog1 ,(cadr frobs)
			   ,(lewp-make-psetq (cddr frobs))))))))


(defun lewp-make-desetq (var-val-pairs)
  (declare (si::c-local))
  (if (null var-val-pairs)
      nil
      (cons (if *lewp-destructuring-hooks*
		(cadr *lewp-destructuring-hooks*)
		'lewp-really-desetq)
	    var-val-pairs)))


(defparameter *lewp-desetq-temporary*
	(make-symbol "LEWP-DESETQ-TEMP"))


(defmacro lewp-really-desetq (&environment env &rest var-val-pairs)
  (labels ((find-non-null (var)
	     ;; see if there's any non-null thing here
	     ;; recurse if the list element is itself a list
	     (do ((tail var)) ((not (consp tail)) tail)
	       (when (find-non-null (pop tail)) (return t))))
	   (lewp-desetq-internal (var val &optional temp)
	     ;; if the value is declared 'unsafe', then the assignemnt
	     ;; is also unsafe.
	     (when (and (consp val)
			(eq (first val) 'LEWP-UNSAFE))
	       (let ((forms (rest val)))
		 (setf forms (if (rest forms) `(progn ,@forms) (first forms)))
		 (return-from lewp-desetq-internal
		   `((LEWP-UNSAFE ,@(lewp-desetq-internal var forms))))))
	     ;; returns a list of actions to be performed
	     (typecase var
	       (null
		 (when (consp val)
		   ;; don't lose possible side-effects
		   (if (eq (car val) 'prog1)
		       ;; these can come from psetq or desetq below.
		       ;; throw away the value, keep the side-effects.
		       ;;Special case is for handling an expanded POP.
		       (mapcan #'(lambda (x)
				   (and (consp x)
					(or (not (eq (car x) 'car))
					    (not (symbolp (cadr x)))
					    (not (symbolp (setq x (macroexpand x env)))))
					(cons x nil)))
			       (cdr val))
		       `(,val))))
	       (cons
		 (let* ((car (car var))
			(cdr (cdr var))
			(car-non-null (find-non-null car))
			(cdr-non-null (find-non-null cdr)))
		   (when (or car-non-null cdr-non-null)
		     (if cdr-non-null
			 (let* ((temp-p temp)
				(temp (or temp *lewp-desetq-temporary*))
				(body #+LEWP-Prefer-POP `(,@(lewp-desetq-internal
							      car
							      `(prog1 (car ,temp)
								      (setq ,temp (cdr ,temp))))
							  ,@(lewp-desetq-internal cdr temp temp))
				      #-LEWP-Prefer-POP `(,@(lewp-desetq-internal car `(car ,temp))
							  (setq ,temp (cdr ,temp))
							  ,@(lewp-desetq-internal cdr temp temp))))
			   (if temp-p
			       `(,@(unless (eq temp val)
				     `((setq ,temp ,val)))
				 ,@body)
			       `((let ((,temp ,val))
				   ,@body))))
			 ;; no cdring to do
			 (lewp-desetq-internal car `(car ,val) temp)))))
	       (otherwise
		 (unless (eq var val)
		   `((setq ,var ,val)))))))
    (do ((actions))
	((null var-val-pairs)
	 (if (null (cdr actions)) (car actions) `(progn ,@(nreverse actions))))
      (setq actions (revappend
		      (lewp-desetq-internal (pop var-val-pairs) (pop var-val-pairs))
		      actions)))))


;;;; LEWP-local variables

;;;This is the "current" pointer into the LEWP source code.
(defvar *lewp-source-code*)


;;;This is the pointer to the original, for things like NAMED that
;;;insist on being in a particular position
(defvar *lewp-original-source-code*)


;;;This is *lewp-source-code* as of the "last" clause.  It is used
;;;primarily for generating error messages (see lewp-error, lewp-warn).
(defvar *lewp-source-context*)


;;;List of names for the LEWP, supplied by the NAMED clause.
(defvar *lewp-names*)

;;;The macroexpansion environment given to the macro.
(defvar *lewp-macro-environment*)

;;;This holds variable names specified with the USING clause.
;;; See LEWP-NAMED-VARIABLE.
(defvar *lewp-named-variables*)

;;; LETlist-like list being accumulated for one group of parallel bindings.
(defvar *lewp-variables*)

;;;List of declarations being accumulated in parallel with
;;;*lewp-variables*.
(defvar *lewp-declarations*)

;;;Used by LEWP for destructuring binding, if it is doing that itself.
;;; See lewp-make-variable.
(defvar *lewp-desetq-crocks*)

;;; List of wrapping forms, innermost first, which go immediately inside
;;; the current set of parallel bindings being accumulated in
;;; *lewp-variables*.  The wrappers are appended onto a body.  E.g.,
;;; this list could conceivably has as its value ((with-open-file (g0001
;;; g0002 ...))), with g0002 being one of the bindings in
;;; *lewp-variables* (this is why the wrappers go inside of the variable
;;; bindings).
(defvar *lewp-wrappers*)

;;;This accumulates lists of previous values of *lewp-variables* and the
;;;other lists  above, for each new nesting of bindings.  See
;;;lewp-bind-block.
(defvar *lewp-bind-stack*)

;;;This is a LEWP-global variable for the (obsolete) NODECLARE clause
;;;which inhibits  LEWP from actually outputting a type declaration for
;;;an iteration (or any) variable.
(defvar *lewp-nodeclare*)

;;;This is simply a list of LEWP iteration variables, used for checking
;;;for duplications.
(defvar *lewp-iteration-variables*)


;;;List of prologue forms of the lewp, accumulated in reverse order.
(defvar *lewp-prologue*)

(defvar *lewp-before-lewp*)
(defvar *lewp-body*)
(defvar *lewp-after-body*)

;;;This is T if we have emitted any body code, so that iteration driving
;;;clauses can be disallowed.   This is not strictly the same as
;;;checking *lewp-body*, because we permit some clauses  such as RETURN
;;;to not be considered "real" body (so as to permit the user to "code"
;;;an  abnormal return value "in lewp").
(defvar *lewp-emitted-body*)


;;;List of epilogue forms (supplied by FINALLY generally), accumulated
;;; in reverse order.
(defvar *lewp-epilogue*)

;;;List of epilogue forms which are supplied after the above "user"
;;;epilogue.  "normal" termination return values are provide by putting
;;;the return form in here.  Normally this is done using
;;;lewp-emit-final-value, q.v.
(defvar *lewp-after-epilogue*)

;;;The "culprit" responsible for supplying a final value from the lewp.
;;;This  is so lewp-emit-final-value can moan about multiple return
;;;values being supplied.
(defvar *lewp-final-value-culprit*)

;;;If not NIL, we are in some branch of a conditional.  Some clauses may
;;;be disallowed.
(defvar *lewp-inside-conditional*)

;;;If not NIL, this is a temporary bound around the lewp for holding the
;;;temporary  value for "it" in things like "when (f) collect it".  It
;;;may be used as a supertemporary by some other things.
(defvar *lewp-when-it-variable*)

;;;Sometimes we decide we need to fold together parts of the lewp, but
;;;some part of the generated iteration  code is different for the first
;;;and remaining iterations.  This variable will be the temporary which 
;;;is the flag used in the lewp to tell whether we are in the first or
;;;remaining iterations.
(defvar *lewp-never-stepped-variable*)

;;;List of all the value-accumulation descriptor structures in the lewp.
;;; See lewp-get-collection-info.
(defvar *lewp-collection-cruft*)		; for multiple COLLECTs (etc)


;;;; Code Analysis Stuff


(defun lewp-constant-fold-if-possible (form &optional expected-type)
  (declare (si::c-local))
  #+Genera (declare (values new-form constantp constant-value))
  (let ((new-form form) (constantp nil) (constant-value nil))
    #+Genera (setq new-form (compiler:optimize-form form *lewp-macro-environment*
						    :repeat t
						    :do-macro-expansion t
						    :do-named-constants t
						    :do-inline-forms t
						    :do-optimizers t
						    :do-constant-folding t
						    :do-function-args t)
		   constantp (constantp new-form *lewp-macro-environment*)
		   constant-value (and constantp (lt:evaluate-constant new-form *lewp-macro-environment*)))
    #-Genera (when (setq constantp (constantp new-form))
	       (setq constant-value (eval new-form)))
    (when (and constantp expected-type)
      (unless (typep constant-value expected-type)
	(lewp-warn "The form ~S evaluated to ~S, which was not of the anticipated type ~S."
		   form constant-value expected-type)
	(setq constantp nil constant-value nil)))
    (values new-form constantp constant-value)))


(defun lewp-constantp (form)
  #+Genera (constantp form *lewp-macro-environment*)
  #-Genera (constantp form))


;;;; LEWP Iteration Optimization

(defparameter *lewp-duplicate-code*
	nil)


(defparameter *lewp-iteration-flag-variable*
	(make-symbol "LEWP-NOT-FIRST-TIME"))


(defun lewp-code-duplication-threshold (env)
  (declare (si::c-local))
  (multiple-value-bind (speed space) (lewp-optimization-quantities env)
    (+ 40 (* (- speed space) 10))))


(defmacro lewp-body (&environment env
		     prologue
		     before-lewp
		     main-body
		     after-lewp
		     epilogue
		     &aux rbefore rafter flagvar)
  (unless (= (length before-lewp) (length after-lewp))
    (error "LEWP-BODY called with non-synched before- and after-lewp lists."))
  ;;All our work is done from these copies, working backwards from the end:
  (setq rbefore (reverse before-lewp) rafter (reverse after-lewp))
  (labels ((psimp (l)
	     (let ((ans nil))
	       (dolist (x l)
		 (when x
		   (push x ans)
		   (when (and (consp x) (member (car x) '(go return return-from)))
		     (return nil))))
	       (nreverse ans)))
	   (pify (l) (if (null (cdr l)) (car l) `(progn ,@l)))
	   (makebody ()
	     (let ((form `(tagbody
			    ,@(psimp (append prologue (nreverse rbefore)))
			 next-lewp
			    ,@(psimp (append main-body (nreconc rafter `((go next-lewp)))))
			 end-lewp
			    ,@(psimp epilogue))))
	       (if flagvar `(let ((,flagvar nil)) ,form) form))))
    (when (or *lewp-duplicate-code* (not rbefore))
      (return-from lewp-body (makebody)))
    ;; This outer lewp iterates once for each not-first-time flag test generated
    ;; plus once more for the forms that don't need a flag test
    (do ((threshold (lewp-code-duplication-threshold env))) (nil)
      (declare (fixnum threshold))
      ;; Go backwards from the ends of before-lewp and after-lewp merging all the equivalent
      ;; forms into the body.
      (do () ((or (null rbefore) (not (equal (car rbefore) (car rafter)))))
	(push (pop rbefore) main-body)
	(pop rafter))
      (unless rbefore (return (makebody)))
      ;; The first forms in rbefore & rafter (which are the chronologically
      ;; last forms in the list) differ, therefore they cannot be moved
      ;; into the main body.  If everything that chronologically precedes
      ;; them either differs or is equal but is okay to duplicate, we can
      ;; just put all of rbefore in the prologue and all of rafter after
      ;; the body.  Otherwise, there is something that is not okay to
      ;; duplicate, so it and everything chronologically after it in
      ;; rbefore and rafter must go into the body, with a flag test to
      ;; distinguish the first time around the lewp from later times.
      ;; What chronologically precedes the non-duplicatable form will
      ;; be handled the next time around the outer lewp.
      (lewp-log "rbefore --> ~a~%" rbefore)
      (lewp-log "rafter  --> ~a~%" rafter)
      (lewp-log "threshold --> ~a~%" threshold)
      (lewp-log "flagvar --> ~a~%" flagvar)
;;      (break "About to do loop")
      (do ((bb rbefore (cdr bb)) (aa rafter (cdr aa)) (lastdiff nil) (count 0) (inc nil))
	  ((progn
	     (lewp-log "Testing if bb is null - bb[~a] (null bb)[~a]~%" bb (null bb))
	     (null bb))
	   (lewp-log "Leaving do~%")
	   (return-from lewp-body (makebody)))	;Did it.
	(dbg-i32 4000001)
	(lewp-log "bb --> ~a~%" bb)
	(cond ((progn
		 (dbg-i32 4000002)
		 (lewp-log "First cond test - always fails ~%")
		 nil)
	       (lewp-log "Never evaluate me~%")
	       )
	      ((progn
		 (lewp-log "Second test~%")
		 (not (equal (car bb) (car aa))))
	       (lewp-log "succeeded not equal (car bb) --> ~a~%" (car bb))
	       (setq lastdiff bb count 0))
	      ((progn
		 (dbg-i32 4000003)
		 (lewp-log "or test~%")
		 (or (not (setq inc (estimate-code-size (car bb) env)))
		   (> (incf count inc) threshold)))
	       (lewp-log "succeeded or (car bb) --> ~a~%" (car bb))
	       ;; Ok, we have found a non-duplicatable piece of code.  Everything
	       ;; chronologically after it must be in the central body.
	       ;; Everything chronologically at and after lastdiff goes into the
	       ;; central body under a flag test.
	       (let ((then nil) (else nil))
		 (do () (nil)
		   (push (pop rbefore) else)
		   (push (pop rafter) then)
		   (when (eq rbefore (cdr lastdiff))
		     (lewp-log "Returning from upper inner do~%")
		     (return)))
		 (unless flagvar
		   (push `(setq ,(setq flagvar *lewp-iteration-flag-variable*) t) else))
		 (push `(if ,flagvar ,(pify (psimp then)) ,(pify (psimp else)))
		       main-body))
	       ;; Everything chronologically before lastdiff until the non-duplicatable form (car bb) 
	       ;; is the same in rbefore and rafter so just copy it into the body
	       (do () (nil)
		 (pop rafter)
		 (push (pop rbefore) main-body)
		 (when (eq rbefore (cdr bb))
		   (lewp-log "Returning from lower inner do~%")
		   (return)))
	       (lewp-log "Returning from lowest return~%")
	       (return))
	      )
	(dbg-i32 4000004)
	(lewp-log "Dropped through bottom of cond~%")
	)
      (lewp-log "Dropped through bottom of do")
      )
    )
  )



(defun duplicatable-code-p (expr env)
  (declare (si::c-local))
  (if (null expr) 0
      (let ((ans (estimate-code-size expr env)))
	(declare (fixnum ans))
	;; Use (DECLARATION-INFORMATION 'OPTIMIZE ENV) here to get an alist of
	;; optimize quantities back to help quantify how much code we are willing to
	;; duplicate.
	ans)))


(defparameter *special-code-sizes*
	'((return 0) (progn 0)
	  (null 1) (not 1) (eq 1) (car 1) (cdr 1)
	  (when 1) (unless 1) (if 1)
	  (caar 2) (cadr 2) (cdar 2) (cddr 2)
	  (caaar 3) (caadr 3) (cadar 3) (caddr 3) (cdaar 3) (cdadr 3) (cddar 3) (cdddr 3)
	  (caaaar 4) (caaadr 4) (caadar 4) (caaddr 4)
	  (cadaar 4) (cadadr 4) (caddar 4) (cadddr 4)
	  (cdaaar 4) (cdaadr 4) (cdadar 4) (cdaddr 4)
	  (cddaar 4) (cddadr 4) (cdddar 4) (cddddr 4)))


(defparameter *estimate-code-size-punt*
	'(block
	   do do* dolist
	   flet
	   labels lambda let let* locally
	   macrolet multiple-value-bind
	   prog prog*
	   symbol-macrolet
	   tagbody
	   unwind-protect
	   with-open-file))


(defun destructuring-size (x)
  (declare (si::c-local))
  (do ((x x (cdr x)) (n 0 (+ (destructuring-size (car x)) n)))
      ((atom x) (+ n (if (null x) 0 1)))))


(defun estimate-code-size (x env)
  (declare (si::c-local))
  (lewp-log "Entered estimate-code-size~%")
  (let ((result (catch 'estimate-code-size
		  (estimate-code-size-1 x env))))
    (lewp-log "Leaving estimate-code-size~%")
    result
    )
  )

(defmacro estimate-code-size-1-f (overhead &optional (args nil args-p))
  `(the fixnum (+ (the fixnum ,overhead)
			(the fixnum (list-size ,(if args-p args '(cdr x)))))))

(defun estimate-code-size-1 (x env)
  (declare (si::c-local))
  (flet ((list-size (l)
	   (let ((n 0))
	     (declare (fixnum n))
	     (dolist (x l n) (incf n (estimate-code-size-1 x env))))))
    ;; ???? (declare (function list-size (list) fixnum))
    (cond ((constantp x #+Genera env) 1)
	  ((symbolp x) (multiple-value-bind (new-form expanded-p) (macroexpand-1 x env)
			 (if expanded-p (estimate-code-size-1 new-form env) 1)))
	  ((atom x) 1)			;??? self-evaluating???
	  ((symbolp (car x))
	   (let ((fn (car x)) (tem nil) (n 0))
	     (declare (symbol fn) (fixnum n))
	     (progn
	       #| (macrolet ((f (overhead &optional (args nil args-p))
	       `(the fixnum (+ (the fixnum ,overhead)
	       (the fixnum (list-size ,(if args-p args '(cdr x)))))))) |#
	       (cond ((setq tem (get-sysprop fn 'estimate-code-size))
		      (typecase tem
			(fixnum (estimate-code-size-1-f tem))
			(t (funcall tem x env))))
		     ((setq tem (assoc fn *special-code-sizes*)) (estimate-code-size-1-f (second tem)))
		     #+Genera
		     ((eq fn 'compiler:invisible-references) (list-size (cddr x)))
		     ((eq fn 'cond)
		      (dolist (clause (cdr x) n) (incf n (list-size clause)) (incf n)))
		     ((eq fn 'desetq)
		      (do ((l (cdr x) (cdr l))) ((null l) n)
			(setq n (+ n (destructuring-size (car l)) (estimate-code-size-1 (cadr l) env)))))
		     ((member fn '(setq psetq))
		      (do ((l (cdr x) (cdr l))) ((null l) n)
			(setq n (+ n (estimate-code-size-1 (cadr l) env) 1))))
		     ((eq fn 'go) 1)
		     ((eq fn 'function)
		      ;;This skirts the issue of implementationally-defined lambda macros
		      ;; by recognizing CL function names and nothing else.
		      (if (or (symbolp (cadr x))
			      (and (consp (cadr x)) (eq (caadr x) 'setf)))
			  1
			  (throw 'duplicatable-code-p nil)))
		     ((eq fn 'multiple-value-setq) (estimate-code-size-1-f (length (second x)) (cddr x)))
		     ((eq fn 'return-from) (1+ (estimate-code-size-1 (third x) env)))
		     ((or (special-operator-p fn) (member fn *estimate-code-size-punt*))
		      (throw 'estimate-code-size nil))
		     (t (multiple-value-bind (new-form expanded-p) (macroexpand-1 x env)
			  (if expanded-p
			      (estimate-code-size-1 new-form env)
			      (estimate-code-size-1-f 3))))))))
	  (t (throw 'estimate-code-size nil)))))


;;;; Lewp Errors


(defun lewp-context ()
  (declare (si::c-local))
  (do ((l *lewp-source-context* (cdr l)) (new nil (cons (car l) new)))
      ((eq l (cdr *lewp-source-code*)) (nreverse new))))


(defun lewp-error (format-string &rest format-args)
  (declare (si::c-local))
  #-cando(si::simple-program-error "~?~%Current LEWP context:~{ ~S~}."
			format-string format-args (lewp-context))
  #+cando(si::simple-program-error (bformat nil "%s\nCurrent LEWP context:\n%s"
					    (eval `(format nil ,format-string ,@format-args))
					    (lewp-context)))
  )


(defun lewp-warn (format-string &rest format-args)
  (declare (si::c-local))
  (warn 'sys::simple-style-warning
	:format-control "~?~%Current LEWP context:~{ ~S~}."
	:format-arguments (list format-string format-args (lewp-context))))


(defun lewp-check-data-type (specified-type required-type
			     &optional (default-type required-type))
  (declare (si::c-local))
  (if (null specified-type)
      default-type
      (multiple-value-bind (a b) (subtypep specified-type required-type)
	(cond ((not b)
	       (lewp-warn "LEWP couldn't verify that ~S is a subtype of the required type ~S."
			  specified-type required-type))
	      ((not a)
	       (lewp-error "Specified data type ~S is not a subtype of ~S."
			   specified-type required-type)))
	specified-type)))


;;;INTERFACE: Traditional, ANSI, Lucid.
(defmacro lewp-finish () 
  "Causes the iteration to terminate \"normally\", the same as implicit
termination by an iteration driving clause, or by use of WHILE or
UNTIL -- the epilogue code (if any) will be run, and any implicitly
collected result will be returned as the value of the LEWP."
  '(go end-lewp))



(defvar *ignores* nil)
(defun subst-gensyms-for-nil (tree)
  (declare (special *ignores*))
  (cond
    ((null tree) (car (push (gensym) *ignores*)))
    ((atom tree) tree)
    (t (cons (subst-gensyms-for-nil (car tree))
	     (subst-gensyms-for-nil (cdr tree))))))
 
(defun lewp-build-destructuring-bindings (crocks forms)
  (if crocks
      (let ((*ignores* ()))
	(declare (special *ignores*))
	`((destructuring-bind ,(subst-gensyms-for-nil (car crocks))
	      ,(cadr crocks)
	    (declare (ignore ,@*ignores*))
	    ,@(lewp-build-destructuring-bindings (cddr crocks) forms))))
      forms))

(defun lewp-translate (*lewp-source-code* *lewp-macro-environment* *lewp-universe*)
  (declare (si::c-local))
  (let ((*lewp-original-source-code* *lewp-source-code*)
	(*lewp-source-context* nil)
	(*lewp-iteration-variables* nil)
	(*lewp-variables* nil)
	(*lewp-nodeclare* nil)
	(*lewp-named-variables* nil)
	(*lewp-declarations* nil)
	(*lewp-desetq-crocks* nil)
	(*lewp-bind-stack* nil)
	(*lewp-prologue* nil)
	(*lewp-wrappers* nil)
	(*lewp-before-lewp* nil)
	(*lewp-body* nil)
	(*lewp-emitted-body* nil)
	(*lewp-after-body* nil)
	(*lewp-epilogue* nil)
	(*lewp-after-epilogue* nil)
	(*lewp-final-value-culprit* nil)
	(*lewp-inside-conditional* nil)
	(*lewp-when-it-variable* nil)
	(*lewp-never-stepped-variable* nil)
	(*lewp-names* nil)
	(*lewp-collection-cruft* nil))
    (lewp-iteration-driver)
    (lewp-bind-block)
    (let ((answer `(lewp-body
		     ,(nreverse *lewp-prologue*)
		     ,(nreverse *lewp-before-lewp*)
		     ,(nreverse *lewp-body*)
		     ,(nreverse *lewp-after-body*)
		     ,(nreconc *lewp-epilogue* (nreverse *lewp-after-epilogue*)))))
      (dolist (entry *lewp-bind-stack*)
	(let ((vars (first entry))
	      (dcls (second entry))
	      (crocks (third entry))
	      (wrappers (fourth entry)))
	  (dolist (w wrappers)
	    (setq answer (append w (list answer))))
	  (when (or vars dcls crocks)
	    (let ((forms (list answer)))
	      ;;(when crocks (push crocks forms))
	      (when dcls (push `(declare ,@dcls) forms))
	      (setq answer `(,(cond ((not vars) 'locally)
				    (*lewp-destructuring-hooks* (first *lewp-destructuring-hooks*))
				    (t 'let))
			     ,vars
			     ,@(lewp-build-destructuring-bindings crocks forms)))))))
      (if *lewp-names*
	  (do () ((null (car *lewp-names*)) answer)
	    (setq answer `(block ,(pop *lewp-names*) ,answer)))
	  `(block nil ,answer)))))


(defun lewp-iteration-driver ()
  (declare (si::c-local))
  (do () ((null *lewp-source-code*))
    (let ((keyword (car *lewp-source-code*)) (tem nil))
      (cond ((not (symbolp keyword))
	     (lewp-error "~S found where LEWP keyword expected." keyword))
	    (t (setq *lewp-source-context* *lewp-source-code*)
	       (lewp-pop-source)
	       (cond ((setq tem (lewp-lookup-keyword keyword (lewp-universe-keywords *lewp-universe*)))
		      ;;It's a "miscellaneous" toplevel LEWP keyword (do, collect, named, etc.)
		      (apply (symbol-function (first tem)) (rest tem)))
		     ((setq tem (lewp-lookup-keyword keyword (lewp-universe-iteration-keywords *lewp-universe*)))
		      (lewp-hack-iteration tem))
		     ((lewp-tmember keyword '(and else))
		      ;; Alternative is to ignore it, ie let it go around to the next keyword...
		      (lewp-error "Secondary clause misplaced at top level in LEWP macro: ~S ~S ~S ..."
				  keyword (car *lewp-source-code*) (cadr *lewp-source-code*)))
		     (t (lewp-error "~S is an unknown keyword in LEWP macro." keyword))))))))



(defun lewp-pop-source ()
  (declare (si::c-local))
  (if *lewp-source-code*
      (pop *lewp-source-code*)
      (lewp-error "LEWP source code ran out when another token was expected.")))


(defun lewp-get-compound-form ()
  (declare (si::c-local))
  (let ((form (lewp-get-form)))
    (unless (consp form)
      (lewp-error "Compound form expected, but found ~A." form))
    form))

(defun lewp-get-progn ()
  (declare (si::c-local))
  (do ((forms (list (lewp-get-compound-form))
              (cons (lewp-get-compound-form) forms))
       (nextform (car *lewp-source-code*)
                 (car *lewp-source-code*)))
      ((atom nextform)
       (if (null (cdr forms)) (car forms) (cons 'progn (nreverse forms))))))


(defun lewp-get-form ()
  (declare (si::c-local))
  (if *lewp-source-code*
      (lewp-pop-source)
      (lewp-error "LEWP code ran out where a form was expected.")))


(defun lewp-construct-return (form)
  (declare (si::c-local))
  `(return-from ,(car *lewp-names*) ,form))

(defun lewp-emit-body (form)
  (declare (si::c-local))
  (setq *lewp-emitted-body* t)
  (push form *lewp-body*))

(defun lewp-emit-final-value (&optional (form nil form-supplied-p))
  (declare (si::c-local))
  (when form-supplied-p
    (push (lewp-construct-return form) *lewp-after-epilogue*))
  (when *lewp-final-value-culprit*
    (lewp-warn "LEWP clause is providing a value for the iteration,~@
	        however one was already established by a ~S clause."
	       *lewp-final-value-culprit*))
  (setq *lewp-final-value-culprit* (car *lewp-source-context*)))


(defun lewp-disallow-conditional (&optional kwd)
  (declare (si::c-local))
  #+(or Genera CLOE) (declare (dbg:error-reporter))
  (when *lewp-inside-conditional*
    (lewp-error "~:[This LEWP~;The LEWP ~:*~S~] clause is not permitted inside a conditional." kwd)))

(defun lewp-disallow-anonymous-collectors ()
  (when (find-if-not 'lewp-collector-name *lewp-collection-cruft*)
    (lewp-error "This LEWP clause is not permitted with anonymous collectors.")))

(defun lewp-disallow-aggregate-booleans ()
  (when (lewp-tmember *lewp-final-value-culprit* '(always never thereis))
    (lewp-error "This anonymous collection LEWP clause is not permitted with aggregate booleans.")))



;;;; Lewp Types


(defun lewp-typed-init (data-type)
  (declare (si::c-local))
  (lewp-log "lewp-typed-init data-type[~a]~%" data-type)
  (cond ((null data-type)
	 (lewp-log "returning nil\n")
	 nil)
	((subtypep data-type 'character)
	 (lewp-log "returning #\\0\n")
	 #\0)
	((not (subtypep data-type 'number))
	 (lewp-log "returning nil\n")
	 nil)
	((subtypep data-type '(or float (complex float)))
	 (lewp-log "coercing 0 to datatype[%s]\n" data-type)
	 (coerce 0 data-type))
	(t
	 (lewp-log "returning 0")
	 0)))

(defun lewp-optional-type (&optional variable)
  (declare (si::c-local))
  ;;No variable specified implies that no destructuring is permissible.
  (and *lewp-source-code*			;Don't get confused by NILs...
       (let ((z (car *lewp-source-code*)))
	 (cond ((lewp-tequal z 'of-type)
		;;This is the syntactically unambigous form in that the form of the
		;; type specifier does not matter.  Also, it is assumed that the
		;; type specifier is unambiguously, and without need of translation,
		;; a common lisp type specifier or pattern (matching the variable) thereof.
		(lewp-pop-source)
		(lewp-pop-source))
		      
	       ((symbolp z)
		;;This is the (sort of) "old" syntax, even though we didn't used to support all of
		;; these type symbols.
		(let ((type-spec (or (gethash z (lewp-universe-type-symbols *lewp-universe*))
				     (gethash (symbol-name z) (lewp-universe-type-keywords *lewp-universe*)))))
		  (when type-spec
		    (lewp-pop-source)
		    type-spec)))
	       (t 
		;;This is our sort-of old syntax.  But this is only valid for when we are destructuring,
		;; so we will be compulsive (should we really be?) and require that we in fact be
		;; doing variable destructuring here.  We must translate the old keyword pattern typespec
		;; into a fully-specified pattern of real type specifiers here.
		(if (consp variable)
		    (unless (consp z)
		     (lewp-error
			"~S found where a LEWP keyword, LEWP type keyword, or LEWP type pattern expected."
			z))
		    (lewp-error "~S found where a LEWP keyword or LEWP type keyword expected." z))
		(lewp-pop-source)
		(labels ((translate (k v)
			   (cond ((null k) nil)
				 ((atom k)
				  (replicate
				    (or (gethash k (lewp-universe-type-symbols *lewp-universe*))
					(gethash (symbol-name k) (lewp-universe-type-keywords *lewp-universe*))
					(lewp-error
					  "Destructuring type pattern ~S contains unrecognized type keyword ~S."
					  z k))
				    v))
				 ((atom v)
				  (lewp-error
				    "Destructuring type pattern ~S doesn't match variable pattern ~S."
				    z variable))
				 (t (cons (translate (car k) (car v)) (translate (cdr k) (cdr v))))))
			 (replicate (typ v)
			   (if (atom v) typ (cons (replicate typ (car v)) (replicate typ (cdr v))))))
		  (translate z variable)))))))



;;;; Lewp Variables


(defun lewp-bind-block ()
  (declare (si::c-local))
  (when (or *lewp-variables* *lewp-declarations* *lewp-wrappers*)
    (push (list (nreverse *lewp-variables*) *lewp-declarations* *lewp-desetq-crocks* *lewp-wrappers*)
	  *lewp-bind-stack*)
    (setq *lewp-variables* nil
	  *lewp-declarations* nil
	  *lewp-desetq-crocks* nil
	  *lewp-wrappers* nil)))

(defun lewp-variable-p (name)
  (do ((entry *lewp-bind-stack* (cdr entry))) (nil)
    (cond ((null entry)
	   (return nil))
	  ((assoc name (caar entry) :test #'eq)
	   (return t)))))

(defun lewp-make-variable (name initialization dtype &optional iteration-variable-p)
  (declare (si::c-local))
  (cond ((null name)
	 (cond ((not (null initialization))
		(push (list (setq name (gensym "LEWP-IGNORE-"))
			    initialization)
		      *lewp-variables*)
		(push `(ignore ,name) *lewp-declarations*))))
	((atom name)
	 (cond (iteration-variable-p
		(if (member name *lewp-iteration-variables*)
		    (lewp-error "Duplicated LEWP iteration variable ~S." name)
		    (push name *lewp-iteration-variables*)))
	       ((assoc name *lewp-variables*)
		(lewp-error "Duplicated variable ~S in LEWP parallel binding." name)))
	 (unless (symbolp name)
	   (lewp-error "Bad variable ~S somewhere in LEWP." name))
	 (lewp-declare-variable name dtype)
	 ;; We use ASSOC on this list to check for duplications (above),
	 ;; so don't optimize out this list:
	 (push (list name (or initialization (lewp-typed-init dtype)))
	       *lewp-variables*))
	(initialization
	 (cond (*lewp-destructuring-hooks*
		(lewp-declare-variable name dtype)
		(push (list name initialization) *lewp-variables*))
	       (t (let ((newvar (gensym "LEWP-DESTRUCTURE-")))
		    (lewp-declare-variable name dtype)
		    (push (list newvar initialization) *lewp-variables*)
		    ;; *LEWP-DESETQ-CROCKS* gathered in reverse order.
		    (setq *lewp-desetq-crocks*
		      (list* name newvar *lewp-desetq-crocks*))
		    #+ignore
		    (lewp-make-variable name nil dtype iteration-variable-p)))))
	(t (let ((tcar nil) (tcdr nil))
	     (if (atom dtype) (setq tcar (setq tcdr dtype))
		 (setq tcar (car dtype) tcdr (cdr dtype)))
	     (lewp-make-variable (car name) nil tcar iteration-variable-p)
	     (lewp-make-variable (cdr name) nil tcdr iteration-variable-p))))
  name)


(defun lewp-make-iteration-variable (name initialization dtype)
  (declare (si::c-local))
  (lewp-make-variable name initialization dtype t))


(defun lewp-declare-variable (name dtype)
  (declare (si::c-local))
  (cond ((or (null name) (null dtype) (eq dtype t)) nil)
	((symbolp name)
	 (unless (or (eq dtype t) (member (the symbol name) *lewp-nodeclare*))
           ;; Allow redeclaration of a variable. This can be used by
           ;; the lewp constructors to make the type more and more
           ;; precise as we add keywords
           (let ((previous (find name *lewp-declarations*
                                 :key #'(lambda (d)
                                          (and (consp d)
                                               (= (length d) 3)
                                               (eq (car d) 'type)
                                               (third d))))))
             (if previous
                 (setf (second previous) dtype)
                 (push `(type ,dtype ,name) *lewp-declarations*)))))
	((consp name)
	 (cond ((consp dtype)
		(lewp-declare-variable (car name) (car dtype))
		(lewp-declare-variable (cdr name) (cdr dtype)))
	       (t (lewp-declare-variable (car name) dtype)
		  (lewp-declare-variable (cdr name) dtype))))
	(t (error "Invalid LEWP variable passed in: ~S." name))))


(defun lewp-maybe-bind-form (form data-type)
  (declare (si::c-local))
  (if (lewp-constantp form)
      form
      (lewp-make-variable (gensym "LEWP-BIND-") form data-type)))



(defun lewp-do-if (for negatep)
  (let ((form (lewp-get-form))
	(*lewp-inside-conditional* t)
	(it-p nil)
	(first-clause-p t))
    (flet ((get-clause (for)
	     (do ((body nil)) (nil)
	       (let ((key (car *lewp-source-code*)) (*lewp-body* nil) data)
		 (cond ((not (symbolp key))
			(lewp-error
			  "~S found where keyword expected getting LEWP clause after ~S."
			  key for))
		       (t (setq *lewp-source-context* *lewp-source-code*)
			  (lewp-pop-source)
			  (when (and (lewp-tequal (car *lewp-source-code*) 'it)
				     first-clause-p)
			    (setq *lewp-source-code*
				  (cons (or it-p (setq it-p (lewp-when-it-variable)))
					(cdr *lewp-source-code*))))
			  (cond ((or (not (setq data (lewp-lookup-keyword
						       key (lewp-universe-keywords *lewp-universe*))))
				     (progn (apply (symbol-function (car data)) (cdr data))
					    (null *lewp-body*)))
				 (lewp-error
				   "~S does not introduce a LEWP clause that can follow ~S."
				   key for))
				(t (setq body (nreconc *lewp-body* body)))))))
	       (setq first-clause-p nil)
	       (if (lewp-tequal (car *lewp-source-code*) :and)
		   (lewp-pop-source)
		   (return (if (cdr body) `(progn ,@(nreverse body)) (car body)))))))
      (let ((then (get-clause for))
	    (else (when (lewp-tequal (car *lewp-source-code*) :else)
		    (lewp-pop-source)
		    (list (get-clause :else)))))
	(when (lewp-tequal (car *lewp-source-code*) :end)
	  (lewp-pop-source))
	(when it-p (setq form `(setq ,it-p ,form)))
	(lewp-emit-body
	  `(if ,(if negatep `(not ,form) form)
	       ,then
	       ,@else))))))


(defun lewp-do-initially ()
  (lewp-disallow-conditional :initially)
  (push (lewp-get-progn) *lewp-prologue*))

(defun lewp-do-finally ()
  (lewp-disallow-conditional :finally)
  (push (lewp-get-progn) *lewp-epilogue*))

(defun lewp-do-do ()
  (lewp-emit-body (lewp-get-progn)))

(defun lewp-do-named ()
  (let ((name (lewp-pop-source)))
    (unless (symbolp name)
      (lewp-error "~S is an invalid name for your LEWP." name))
    (when (or *lewp-before-lewp* *lewp-body* *lewp-after-epilogue* *lewp-inside-conditional*)
      (lewp-error "The NAMED ~S clause occurs too late." name))
    (when *lewp-names*
      (lewp-error "You may only use one NAMED clause in your lewp: NAMED ~S ... NAMED ~S."
		  (car *lewp-names*) name))
    (setq *lewp-names* (list name nil))))

(defun lewp-do-return ()
  (lewp-emit-body (lewp-construct-return (lewp-get-form))))


;;;; Value Accumulation: List


(defstruct (lewp-collector
	     #+ecl (:type vector)
	     #+nil (:copier nil)
	     #+nil (:predicate nil))
  name
  class
  (history nil)
  (tempvars nil)
  dtype
  (data nil))						;collector-specific data


(defun lewp-get-collection-info (collector class default-type)
  (declare (si::c-local))
  (lewp-log "lewp-get-collection-info: *lewp-source-code* --> ~a~%" *lewp-source-code*)
  (let ((form (lewp-get-form))
	(dtype (and (not (lewp-universe-ansi *lewp-universe*)) (lewp-optional-type)))
	(name (when (lewp-tequal (car *lewp-source-code*) 'into)
		(lewp-pop-source)
		(lewp-pop-source))))
    (lewp-log "lewp-get-collection-info: name--> ~a~%" name)
    (when (not (symbolp name))
      (lewp-error "Value accumulation recipient name, ~S, is not a symbol - its class name is: ~a." name (class-name name)))
    (unless name
      (lewp-disallow-aggregate-booleans))
    (unless dtype
      (setq dtype (or (lewp-optional-type) default-type)))
    (let ((cruft (find (the symbol name) *lewp-collection-cruft*
		       :key #'lewp-collector-name)))
      (cond ((not cruft)
	     (when (and name (lewp-variable-p name))
	       (lewp-error "Variable ~S cannot be used in INTO clause" name))
	     (push (setq cruft (make-lewp-collector
				 :name name :class class
				 :history (list collector) :dtype dtype))
		   *lewp-collection-cruft*))
	    (t (unless (eq (lewp-collector-class cruft) class)
		 (lewp-error
		   "Incompatible kinds of LEWP value accumulation specified for collecting~@
		    ~:[as the value of the LEWP~;~:*INTO ~S~]: ~S and ~S."
		   name (car (lewp-collector-history cruft)) collector))
	       (unless (equal dtype (lewp-collector-dtype cruft))
		 (lewp-warn
		   "Unequal datatypes specified in different LEWP value accumulations~@
		   into ~S: ~S and ~S."
		   name dtype (lewp-collector-dtype cruft))
		 (when (eq (lewp-collector-dtype cruft) t)
		   (setf (lewp-collector-dtype cruft) dtype)))
	       (push collector (lewp-collector-history cruft))))
      (values cruft form))))


(defun lewp-list-collection (specifically)	;NCONC, LIST, or APPEND
  (multiple-value-bind (lc form) (lewp-get-collection-info specifically 'list 'list)
    (let ((tempvars (lewp-collector-tempvars lc)))
      (unless tempvars
	(setf (lewp-collector-tempvars lc)
	      (setq tempvars (list* (gensym "LEWP-LIST-HEAD")
				    (gensym "LEWP-LIST-TAIL")
				    (and (lewp-collector-name lc)
					 (list (lewp-collector-name lc))))))
	(push `(with-lewp-list-collection-head ,tempvars) *lewp-wrappers*)
	(unless (lewp-collector-name lc)
	  (lewp-emit-final-value `(lewp-collect-answer ,(car tempvars) ,@(cddr tempvars)))))
      (ecase specifically
	(list (setq form `(list ,form)))
	(nconc nil)
	(append (unless (and (consp form) (eq (car form) 'list))
		  (setq form `(lewp-copylist* ,form)))))
      (lewp-emit-body `(lewp-collect-rplacd ,tempvars ,form)))))


;;;; Value Accumulation: max, min, sum, count.



(defun lewp-sum-collection (specifically required-type default-type)	;SUM, COUNT
  (multiple-value-bind (lc form)
      (lewp-get-collection-info specifically 'sum default-type)
    (lewp-check-data-type (lewp-collector-dtype lc) required-type)
    (let ((tempvars (lewp-collector-tempvars lc)))
      (unless tempvars
	(setf (lewp-collector-tempvars lc)
	      (setq tempvars (list (lewp-make-variable
				     (or (lewp-collector-name lc)
					 (gensym "LEWP-SUM-"))
				     nil (lewp-collector-dtype lc)))))
	(unless (lewp-collector-name lc)
	  (lewp-emit-final-value (car (lewp-collector-tempvars lc)))))
      (lewp-emit-body
	(if (eq specifically 'count)
	    `(when ,form
	       (setq ,(car tempvars)
		     ,(hide-variable-reference t (car tempvars) `(1+ ,(car tempvars)))))
	    `(setq ,(car tempvars)
		   (+ ,(hide-variable-reference t (car tempvars) (car tempvars))
		      ,form)))))))



(defun lewp-maxmin-collection (specifically)
  (multiple-value-bind (lc form)
      (lewp-get-collection-info specifically 'maxmin *lewp-real-data-type*)
    (lewp-check-data-type (lewp-collector-dtype lc) *lewp-real-data-type*)
    (let ((data (lewp-collector-data lc)))
      (unless data
	(setf (lewp-collector-data lc)
	      (setq data (make-lewp-minimax
			   (or (lewp-collector-name lc) (gensym "LEWP-MAXMIN-"))
			   (lewp-collector-dtype lc))))
	(unless (lewp-collector-name lc)
	  (lewp-emit-final-value (lewp-minimax-answer-variable data))))
      (lewp-note-minimax-operation specifically data)
      (push `(with-minimax-value ,data) *lewp-wrappers*)
      (lewp-emit-body `(lewp-accumulate-minimax-value ,data ,specifically ,form))
      )))


;;;; Value Accumulation:  Aggregate Booleans

;;;ALWAYS and NEVER.
;;; Under ANSI these are not permitted to appear under conditionalization.
(defun lewp-do-always (restrictive negate)
  (let ((form (lewp-get-form)))
    (when restrictive (lewp-disallow-conditional))
    (lewp-disallow-anonymous-collectors)
    (lewp-emit-body `(,(if negate 'when 'unless) ,form
		      ,(lewp-construct-return nil)))
    (lewp-emit-final-value t)))



;;;THERIS.
;;; Under ANSI this is not permitted to appear under conditionalization.
(defun lewp-do-thereis (restrictive)
  (when restrictive (lewp-disallow-conditional))
  (lewp-disallow-anonymous-collectors)
  (lewp-emit-final-value)
  (lewp-emit-body `(when (setq ,(lewp-when-it-variable) ,(lewp-get-form))
		     ,(lewp-construct-return *lewp-when-it-variable*))))


(defun lewp-do-while (negate kwd &aux (form (lewp-get-form)))
  (lewp-disallow-conditional kwd)
  (lewp-emit-body `(,(if negate 'when 'unless) ,form (go end-lewp))))


(defun lewp-do-with ()
  (lewp-disallow-conditional :with)
  (do ((var) (val) (dtype)) (nil)
    (setq var (lewp-pop-source)
	  dtype (lewp-optional-type var)
	  val (cond ((lewp-tequal (car *lewp-source-code*) :=)
		     (lewp-pop-source)
		     (lewp-get-form))
		    (t nil)))
    (when (and var (lewp-variable-p var))
      (lewp-error "Variable ~S has already been used" var))
    (lewp-make-variable var val dtype)
    (if (lewp-tequal (car *lewp-source-code*) :and)
	(lewp-pop-source)
	(return (lewp-bind-block)))))


;;;; The iteration driver

(defun lewp-hack-iteration (entry)
  (declare (si::c-local))
  (flet ((make-endtest (list-of-forms)
	   (cond ((null list-of-forms) nil)
		 ((member t list-of-forms) '(go end-lewp))
		 (t `(when ,(if (null (cdr (setq list-of-forms (nreverse list-of-forms))))
				(car list-of-forms)
				(cons 'or list-of-forms))
		       (go end-lewp))))))
    (do ((pre-step-tests nil)
	 (steps nil)
	 (post-step-tests nil)
	 (pseudo-steps nil)
	 (pre-lewp-pre-step-tests nil)
	 (pre-lewp-steps nil)
	 (pre-lewp-post-step-tests nil)
	 (pre-lewp-pseudo-steps nil)
	 (tem) (data))
	(nil)
      ;; Note we collect endtests in reverse order, but steps in correct
      ;; order.  MAKE-ENDTEST does the nreverse for us.
      (setq tem (setq data (apply (symbol-function (first entry)) (rest entry))))
      (and (car tem) (push (car tem) pre-step-tests))
      (setq steps (nconc steps (lewp-copylist* (car (setq tem (cdr tem))))))
      (and (car (setq tem (cdr tem))) (push (car tem) post-step-tests))
      (setq pseudo-steps (nconc pseudo-steps (lewp-copylist* (car (setq tem (cdr tem))))))
      (setq tem (cdr tem))
      (when *lewp-emitted-body*
	(lewp-warn "Iteration in LEWP follows body code."))
      (unless tem (setq tem data))
      (when (car tem) (push (car tem) pre-lewp-pre-step-tests))
      (setq pre-lewp-steps (nconc pre-lewp-steps (lewp-copylist* (car (setq tem (cdr tem))))))
      (when (car (setq tem (cdr tem))) (push (car tem) pre-lewp-post-step-tests))
      (setq pre-lewp-pseudo-steps (nconc pre-lewp-pseudo-steps (lewp-copylist* (cadr tem))))
      (unless (lewp-tequal (car *lewp-source-code*) :and)
	(setq *lewp-before-lewp* (list* (lewp-make-desetq pre-lewp-pseudo-steps)
					(make-endtest pre-lewp-post-step-tests)
					(lewp-make-psetq pre-lewp-steps)
					(make-endtest pre-lewp-pre-step-tests)
					*lewp-before-lewp*)
	      *lewp-after-body* (list* (lewp-make-desetq pseudo-steps)
				       (make-endtest post-step-tests)
				       (lewp-make-psetq steps)
				       (make-endtest pre-step-tests)
				       *lewp-after-body*))
	(lewp-bind-block)
	(return nil))
      (lewp-pop-source)				; flush the "AND"
      (when (and (not (lewp-universe-implicit-for-required *lewp-universe*))
		 (setq tem (lewp-lookup-keyword
			     (car *lewp-source-code*)
			     (lewp-universe-iteration-keywords *lewp-universe*))))
	;;Latest ANSI clarification is that the FOR/AS after the AND must NOT be supplied.
	(lewp-pop-source)
	(setq entry tem)))))


;;;; Main Iteration Drivers


;FOR variable keyword ..args..
(defun lewp-do-for ()
  (let* ((var (lewp-pop-source))
	 (data-type (lewp-optional-type var))
	 (keyword (lewp-pop-source))
	 (first-arg nil)
	 (tem nil))
    (setq first-arg (lewp-get-form))
    (unless (and (symbolp keyword)
		 (setq tem (lewp-lookup-keyword
			     keyword
			     (lewp-universe-for-keywords *lewp-universe*))))
      (lewp-error "~S is an unknown keyword in FOR or AS clause in LEWP." keyword))
    (apply (car tem) var first-arg data-type (cdr tem))))

(defun lewp-do-repeat ()
  (lewp-disallow-conditional :repeat)
  (let* ((form0 (lewp-get-form))
         (type (if (ext:fixnump form0) 'fixnum 'real))
         (var (lewp-make-variable (gensym) form0 type))
         (form `(lewp-unsafe (when (minusp (decf ,var)) (go end-lewp)))))
    (push form *lewp-before-lewp*)
    (push form *lewp-after-body*)
    ;; FIXME: What should
    ;;   (lewp count t into a
    ;;         repeat 3
    ;;         count t into b
    ;;         finally (return (list a b)))
    ;; return: (3 3) or (4 3)? PUSHes above are for the former
    ;; variant, L-P-B below for the latter.
    #+nil (lewp-pseudo-body form)
    )
  )

(defun lewp-when-it-variable ()
  (declare (si::c-local))
  (or *lewp-when-it-variable*
      (setq *lewp-when-it-variable*
	    (lewp-make-variable (gensym "LEWP-IT-") nil nil))))


;;;; Various FOR/AS Subdispatches


;;;ANSI "FOR x = y [THEN z]" is sort of like the old Genera one when the THEN
;;; is omitted (other than being more stringent in its placement), and like
;;; the old "FOR x FIRST y THEN z" when the THEN is present.  I.e., the first
;;; initialization occurs in the lewp body (first-step), not in the variable binding
;;; phase.
(defun lewp-ansi-for-equals (var val data-type)
  (lewp-make-iteration-variable var nil data-type)
  (cond ((lewp-tequal (car *lewp-source-code*) :then)
	 ;;Then we are the same as "FOR x FIRST y THEN z".
	 (lewp-pop-source)
	 `(() (,var ,(lewp-get-form)) () ()
	   () (,var ,val) () ()))
	(t ;;We are the same as "FOR x = y".
	 `(() (,var ,val) () ()))))


(defun lewp-for-across (var val data-type)
  (lewp-make-iteration-variable var nil data-type)
  (let ((vector-var (gensym "LEWP-ACROSS-VECTOR-"))
	(index-var (gensym "LEWP-ACROSS-INDEX-")))
    (multiple-value-bind (vector-form constantp vector-value)
	(lewp-constant-fold-if-possible val 'vector)
      (lewp-make-variable
	vector-var vector-form
	(if (and (consp vector-form) (eq (car vector-form) 'the))
	    (cadr vector-form)
	    'vector))
      #+Genera (push `(system:array-register ,vector-var) *lewp-declarations*)
      (lewp-make-variable index-var 0 'fixnum)
      (let* ((length 0)
	     (length-form (cond ((not constantp)
				 (let ((v (gensym "LEWP-ACROSS-LIST")))
				   (push `(setq ,v (length ,vector-var)) *lewp-prologue*)
				   (lewp-make-variable v 0 'fixnum)))
				(t (setq length (length vector-value)))))
	     (first-test `(>= ,index-var ,length-form))
	     (other-test first-test)
	     (step `(,var (aref ,vector-var ,index-var)))
	     (pstep `(,index-var (1+ ,index-var))))
	(declare (fixnum length))
	(when constantp
	  (setq first-test (= length 0))
	  (when (<= length 1)
	    (setq other-test t)))
	`(,other-test ,step () ,pstep
	  ,@(and (not (eq first-test other-test)) `(,first-test ,step () ,pstep)))))))



;;;; List Iteration


(defun lewp-list-step (listvar)
  (declare (si::c-local))
  ;;We are not equipped to analyze whether 'FOO is the same as #'FOO here in any
  ;; sensible fashion, so let's give an obnoxious warning whenever 'FOO is used
  ;; as the stepping function.
  ;;While a Discerning Compiler may deal intelligently with (funcall 'foo ...), not
  ;; recognizing FOO may defeat some LEWP optimizations.
  (let ((stepper (cond ((lewp-tequal (car *lewp-source-code*) :by)
			(lewp-pop-source)
			(lewp-get-form))
		       (t '(function cdr)))))
    (cond ((and (consp stepper) (eq (car stepper) 'quote))
	   (lewp-warn "Use of QUOTE around stepping function in LEWP will be left verbatim.")
	   (values `(funcall ,stepper ,listvar) nil))
	  ((and (consp stepper) (eq (car stepper) 'function))
	   (values (list (cadr stepper) listvar) (cadr stepper)))
	  (t (values `(funcall ,(lewp-make-variable (gensym "LEWP-FN") stepper 'function)
			       ,listvar)
		     nil)))))


(defun lewp-for-on (var val data-type)
  (multiple-value-bind (list constantp list-value) (lewp-constant-fold-if-possible val)
    (let ((listvar var))
      (cond ((and var (symbolp var)) (lewp-make-iteration-variable var list data-type))
	    (t (lewp-make-variable (setq listvar (gensym)) list 'list)
	       (lewp-make-iteration-variable var nil data-type)))
      (multiple-value-bind (list-step step-function) (lewp-list-step listvar)
	(declare #+(and (not LEWP-Prefer-POP) (not CLOE)) (ignore step-function))
	;; The CLOE problem above has to do with bug in macroexpansion of multiple-value-bind.
	(let* ((first-endtest
		(hide-variable-reference
		 (eq var listvar)
		 listvar
		 ;; the following should use `atom' instead of `endp', per
		 ;; [bug2428]
		 `(atom ,listvar)))
	       (other-endtest first-endtest))
	  (when (and constantp (listp list-value))
	    (setq first-endtest (null list-value)))
	  (cond ((eq var listvar)
		 ;;Contour of the lewp is different because we use the user's variable...
		 `(() (,listvar ,(hide-variable-reference t listvar list-step)) ,other-endtest
		   () () () ,first-endtest ()))
		#+LEWP-Prefer-POP
		((and step-function
		      (let ((n (cdr (assoc step-function '((cdr . 1) (cddr . 2)
							   (cdddr . 3) (cddddr . 4))))))
			(and n (do ((l var (cdr l)) (i 0 (1+ i)))
				   ((atom l) (and (null l) (= i n)))
				 (declare (fixnum i))))))
		 (let ((step (mapcan #'(lambda (x) (list x `(pop ,listvar))) var)))
		   `(,other-endtest () () ,step ,first-endtest () () ,step)))
		(t (let ((step `(,var ,listvar)) (pseudo `(,listvar ,list-step)))
		     `(,other-endtest ,step () ,pseudo
		       ,@(and (not (eq first-endtest other-endtest))
			      `(,first-endtest ,step () ,pseudo)))))))))))


(defun lewp-for-in (var val data-type)
  (multiple-value-bind (list constantp list-value) (lewp-constant-fold-if-possible val)
    (let ((listvar (gensym "LEWP-LIST")))
      (lewp-make-iteration-variable var nil data-type)
      (lewp-make-variable listvar list 'list)
      (multiple-value-bind (list-step step-function) (lewp-list-step listvar)
	#-LEWP-Prefer-POP (declare (ignore step-function))
	(let* ((first-endtest `(endp ,listvar))
	       (other-endtest first-endtest)
	       (step `(,var (car ,listvar)))
	       (pseudo-step `(,listvar ,list-step)))
	  (when (and constantp (listp list-value))
	    (setq first-endtest (null list-value)))
	  #+LEWP-Prefer-POP
          (when (eq step-function 'cdr)
            (setq step `(,var (pop ,listvar)) pseudo-step nil))
	  `(,other-endtest ,step () ,pseudo-step
	    ,@(and (not (eq first-endtest other-endtest))
		   `(,first-endtest ,step () ,pseudo-step))))))))


;;;; Iteration Paths


(defstruct (lewp-path
	     #+ecl (:type vector)
	     #+nil (:copier nil)
	     #+nil (:predicate nil)
	      )
  names
  preposition-groups
  inclusive-permitted
  function
  user-data)


(defun add-lewp-path (names function universe &key preposition-groups inclusive-permitted user-data)
  (declare (si::c-local))
  (unless (listp names) (setq names (list names)))
  ;; Can't do this due to CLOS bootstrapping problems.
  #-(or Genera (and CLOE Source-Bootstrap) ecl) (check-type universe lewp-universe)
  (let ((ht (lewp-universe-path-keywords universe))
	(lp (make-lewp-path
	      :names (mapcar #'symbol-name names)
	      :function function
	      :user-data user-data
	      :preposition-groups (mapcar #'(lambda (x) (if (listp x) x (list x))) preposition-groups)
	      :inclusive-permitted inclusive-permitted)))
    (unless ht (error "lewp-universe-path-keywords universe is nil!"))
    (dolist (name names) (setf (gethash (symbol-name name) ht) lp))
    lp))


;;; Note:  path functions are allowed to use lewp-make-variable, hack
;;; the prologue, etc.
(defun lewp-for-being (var val data-type)
  ;; FOR var BEING each/the pathname prep-phrases using-stuff...
  ;; each/the = EACH or THE.  Not clear if it is optional, so I guess we'll warn.
  (let ((path nil)
	(data nil)
	(inclusive nil)
	(stuff nil)
	(initial-prepositions nil))
    (cond ((lewp-tmember val '(:each :the)) (setq path (lewp-pop-source)))
	  ((lewp-tequal (car *lewp-source-code*) :and)
	   (lewp-pop-source)
	   (setq inclusive t)
	   (unless (lewp-tmember (car *lewp-source-code*) '(:its :each :his :her))
	     (lewp-error "~S found where ITS or EACH expected in LEWP iteration path syntax."
			 (car *lewp-source-code*)))
	   (lewp-pop-source)
	   (setq path (lewp-pop-source))
	   (setq initial-prepositions `((:in ,val))))
	  (t (lewp-error "Unrecognizable LEWP iteration path syntax.  Missing EACH or THE?")))
    (cond ((not (symbolp path))
	   (lewp-error "~S found where a LEWP iteration path name was expected." path))
	  ((not (setq data (lewp-lookup-keyword path (lewp-universe-path-keywords *lewp-universe*))))
	   (lewp-error "~S is not the name of a LEWP iteration path." path))
	  ((and inclusive (not (lewp-path-inclusive-permitted data)))
	   (lewp-error "\"Inclusive\" iteration is not possible with the ~S LEWP iteration path." path)))
    (let ((fun (lewp-path-function data))
	  (preps (nconc initial-prepositions
			(lewp-collect-prepositional-phrases (lewp-path-preposition-groups data) t)))
	  (user-data (lewp-path-user-data data)))
      (when (symbolp fun) (setq fun (symbol-function fun)))
      (setq stuff (if inclusive
		      (apply fun var data-type preps :inclusive t user-data)
		      (apply fun var data-type preps user-data))))
    (when *lewp-named-variables*
      (lewp-error "Unused USING variables: ~S." *lewp-named-variables*))
    ;; STUFF is now (bindings prologue-forms . stuff-to-pass-back).  Protect the system from the user
    ;; and the user from himself.
    (unless (member (length stuff) '(6 10))
      (lewp-error "Value passed back by LEWP iteration path function for path ~S has invalid length."
		  path))
    (do ((l (car stuff) (cdr l)) (x)) ((null l))
      (if (atom (setq x (car l)))
	  (lewp-make-iteration-variable x nil nil)
	  (lewp-make-iteration-variable (car x) (cadr x) (caddr x))))
    (setq *lewp-prologue* (nconc (reverse (cadr stuff)) *lewp-prologue*))
    (cddr stuff)))



;;;INTERFACE:  Lucid, exported.
;;; i.e., this is part of our extended ansi-lewp interface.
(defun named-variable (name)
  (declare (si::c-local))
  (let ((tem (lewp-tassoc name *lewp-named-variables*)))
    (declare (list tem))
    (cond ((null tem) (values (gensym) nil))
	  (t (setq *lewp-named-variables* (delete tem *lewp-named-variables*))
	     (values (cdr tem) t)))))


(defun lewp-collect-prepositional-phrases (preposition-groups &optional USING-allowed initial-phrases)
  (declare (si::c-local))
  (flet ((in-group-p (x group) (car (lewp-tmember x group))))
    (do ((token nil)
	 (prepositional-phrases initial-phrases)
	 (this-group nil nil)
	 (this-prep nil nil)
	 (disallowed-prepositions
	  (progn
	    (mapcan #'(lambda (x)
		       (lewp-copylist*
			 (find (car x) preposition-groups :test #'in-group-p)))
		   initial-phrases)))
	 (used-prepositions (mapcar #'car initial-phrases)))
	((null *lewp-source-code*)
	 (nreverse prepositional-phrases))
      (declare (symbol this-prep))
      (setq token (car *lewp-source-code*))
      (dolist (group preposition-groups)
	(when (setq this-prep (in-group-p token group))
	  (return (setq this-group group))))
      (cond (this-group
	     (when (member this-prep disallowed-prepositions)
	       (lewp-error
		 (if (member this-prep used-prepositions)
		     "A ~S prepositional phrase occurs multiply for some LEWP clause."
		     "Preposition ~S used when some other preposition has subsumed it.")
		 token))
	     (setq used-prepositions (if (listp this-group)
					 (append this-group used-prepositions)
					 (cons this-group used-prepositions)))
	     (lewp-pop-source)
	     (push (list this-prep (lewp-get-form)) prepositional-phrases))
	    ((and USING-allowed (lewp-tequal token 'using))
	     (lewp-pop-source)
	     (do ((z (lewp-pop-source) (lewp-pop-source)) (tem)) (nil)
	       (when (cadr z)
		 (if (setq tem (lewp-tassoc (car z) *lewp-named-variables*))
		     (lewp-error
		       "The variable substitution for ~S occurs twice in a USING phrase,~@
		        with ~S and ~S."
		       (car z) (cadr z) (cadr tem))
		     (push (cons (car z) (cadr z)) *lewp-named-variables*)))
	       (when (or (null *lewp-source-code*) (symbolp (car *lewp-source-code*)))
		 (return nil))))
	    (t (return (nreverse prepositional-phrases)))))))


;;;; Master Sequencer Function

(defun test-lewp-sequencer (indexv indexv-type indexv-user-specified-p
		       variable variable-type
		       sequence-variable sequence-type
		       step-hack default-top
		       prep-phrases)
  (declare (si::c-local))
  (break "At top of test-lewp-sequencer")
  (let ((endform nil)	;Form (constant or variable) with limit value.
	(sequencep nil)	;T if sequence arg has been provided.
	(testfn nil)	;endtest function
	(test nil)	;endtest form.
	(stepby (1+ (or (lewp-typed-init indexv-type) 0))) ;Our increment.
	(stepby-constantp t)
	(step nil)	      ;step form.
	(dir nil)	      ;Direction of stepping: NIL, :UP, :DOWN.
	(inclusive-iteration nil)	;T if include last index.
	(start-given nil)      ;T when prep phrase has specified start
	(start-value nil)
	(start-constantp nil)
	(limit-given nil)	 ;T when prep phrase has specified end
	(limit-constantp nil)
	(limit-value nil)
	)
    (when variable (lewp-make-iteration-variable variable nil variable-type))
    ))

(defun lewp-sequencer (indexv indexv-type indexv-user-specified-p
		       variable variable-type
		       sequence-variable sequence-type
		       step-hack default-top
		       prep-phrases)
  (declare (si::c-local))
   (let ((endform nil)				;Form (constant or variable) with limit value.
	 (sequencep nil)			;T if sequence arg has been provided.
	 (testfn nil)				;endtest function
	 (test nil)				;endtest form.
	 (stepby (1+ (or (lewp-typed-init indexv-type) 0)))	;Our increment.
	 (stepby-constantp t)
	 (step nil)				;step form.
	 (dir nil)				;Direction of stepping: NIL, :UP, :DOWN.
	 (inclusive-iteration nil)		;T if include last index.
	 (start-given nil)			;T when prep phrase has specified start
	 (start-value nil)
	 (start-constantp nil)
	 (limit-given nil)			;T when prep phrase has specified end
	 (limit-constantp nil)
	 (limit-value nil)
	 )
     (when variable (lewp-make-iteration-variable variable nil variable-type))
     (do ((l prep-phrases (cdr l)) (prep) (form) (odir)) ((null l))
       (setq prep (caar l) form (cadar l))
       (case prep
	 ((:of :in)
	  (setq sequencep t)
	  (lewp-make-variable sequence-variable form sequence-type))
	 ((:from :downfrom :upfrom)
	  (setq start-given t)
	  (cond ((eq prep :downfrom) (setq dir ':down))
		((eq prep :upfrom) (setq dir ':up)))
	  (multiple-value-setq (form start-constantp start-value)
	    (lewp-constant-fold-if-possible form indexv-type))
	  (lewp-make-iteration-variable indexv form indexv-type))
	 ((:upto :to :downto :above :below)
	  (cond ((lewp-tequal prep :upto) (setq inclusive-iteration (setq dir ':up)))
		((lewp-tequal prep :to) (setq inclusive-iteration t))
		((lewp-tequal prep :downto) (setq inclusive-iteration (setq dir ':down)))
		((lewp-tequal prep :above) (setq dir ':down))
		((lewp-tequal prep :below) (setq dir ':up)))
	  (setq limit-given t)
	  (multiple-value-setq (form limit-constantp limit-value)
	    (lewp-constant-fold-if-possible form indexv-type))
	  (setq endform (if limit-constantp
			    `',limit-value
			    (lewp-make-variable
			      (gensym "LEWP-LIMIT") form indexv-type))))
	 (:by
	   (multiple-value-setq (form stepby-constantp stepby)
	     (lewp-constant-fold-if-possible form indexv-type))
	   (unless stepby-constantp
	     (lewp-make-variable (setq stepby (gensym "LEWP-STEP-BY")) form indexv-type)))
	 (t (lewp-error
	      "~S invalid preposition in sequencing or sequence path.~@
	       Invalid prepositions specified in iteration path descriptor or something?"
	      prep)))
       (when (and odir dir (not (eq dir odir)))
	 (lewp-error "Conflicting stepping directions in LEWP sequencing path"))
       (setq odir dir))
     (when (and sequence-variable (not sequencep))
       (lewp-error "Missing OF or IN phrase in sequence path"))
     ;; Now fill in the defaults.
     (unless start-given
       (lewp-make-iteration-variable
	 indexv
	 (setq start-constantp t start-value (or (lewp-typed-init indexv-type) 0))
	 indexv-type))
     (cond ((member dir '(nil :up))
	    (when (or limit-given default-top)
	      (unless limit-given
		(lewp-make-variable (setq endform (gensym "LEWP-SEQ-LIMIT-"))
				    nil indexv-type)
		(push `(setq ,endform ,default-top) *lewp-prologue*))
	      (setq testfn (if inclusive-iteration '> '>=)))
	    (setq step (if (eql stepby 1) `(1+ ,indexv) `(+ ,indexv ,stepby))))
	   (t (unless start-given
		(unless default-top
		  (lewp-error "Don't know where to start stepping."))
		(push `(setq ,indexv (1- ,default-top)) *lewp-prologue*))
	      (when (and default-top (not endform))
		(setq endform (lewp-typed-init indexv-type) inclusive-iteration t))
	      (when endform (setq testfn (if inclusive-iteration  '< '<=)))
	      (setq step (if (eql stepby 1) `(1- ,indexv) `(- ,indexv ,stepby)))))
     (setq step `(lewp-unsafe ,step))
     (when testfn (setq test (hide-variable-reference t indexv `(,testfn ,indexv ,endform))))
     (when step-hack
       (setq step-hack `(,variable ,(hide-variable-reference indexv-user-specified-p indexv step-hack))))
     (let ((first-test test) (remaining-tests test))
       (when (and stepby-constantp start-constantp limit-constantp)
         ;; We can make the number type more precise when we know the
         ;; start, end and step values.
         (let ((new-type (typecase (+ start-value stepby limit-value)
                           (integer (if (and (ext:fixnump start-value)
                                             (ext:fixnump limit-value))
                                        'fixnum
                                        indexv-type))
                           (single-float 'single-float)
                           (double-float 'double-float)
                           (long-float 'long-float)
                           (short-float 'short-float)
                           (t indexv-type))))
;;	   (break "About to test subtype")
           (unless (subtypep indexv-type new-type)
             (lewp-declare-variable indexv new-type)))
;;	 (break "About to funcall")
	 (when (setq first-test (funcall (symbol-function testfn) start-value limit-value))
	   (setq remaining-tests t)))
;;       (break "About to backquote")
       `(() (,indexv ,(hide-variable-reference t indexv step)) ,remaining-tests ,step-hack
	 () () ,first-test ,step-hack))))


;;;; Interfaces to the Master Sequencer



(defun lewp-for-arithmetic (var val data-type kwd)
  (unless var
    (setf var (gensym)))
  (lewp-sequencer
    var (lewp-check-data-type data-type *lewp-real-data-type*) t
    nil nil nil nil nil nil
    (lewp-collect-prepositional-phrases
      '((:from :upfrom :downfrom) (:to :upto :downto :above :below) (:by))
      nil (list (list kwd val))))

  )

#+nil
(defun lewp-sequence-elements-path (variable data-type prep-phrases
				    &key fetch-function size-function sequence-type element-type)
  (multiple-value-bind (indexv indexv-user-specified-p) (named-variable 'index)
    (let ((sequencev (named-variable 'sequence)))
      #+Genera (when (and sequencev
			  (symbolp sequencev)
			  sequence-type
			  (subtypep sequence-type 'vector)
			  (not (member (the symbol sequencev) *lewp-nodeclare*)))
		 (push `(sys:array-register ,sequencev) *lewp-declarations*))
      (list* nil nil			; dummy bindings and prologue
	     (lewp-sequencer
		indexv 'fixnum indexv-user-specified-p
		variable (or data-type element-type)
		sequencev sequence-type
		  `(,fetch-function ,sequencev ,indexv) `(,size-function ,sequencev)
		prep-phrases)))))



;;;; Builtin LEWP Iteration Paths


#||
(lewp for v being the hash-values of ht do (print v))
(lewp for k being the hash-keys of ht do (print k))
(lewp for v being the hash-values of ht using (hash-key k) do (print (list k v)))
(lewp for k being the hash-keys of ht using (hash-value v) do (print (list k v)))
||#

(defun lewp-hash-table-iteration-path (variable data-type prep-phrases &key which)
  (check-type which (member hash-key hash-value))
  (cond ((or (cdr prep-phrases) (not (member (caar prep-phrases) '(:in :of))))
	 (lewp-error "Too many prepositions!"))
	((null prep-phrases) (lewp-error "Missing OF or IN in ~S iteration path.")))
  (let ((ht-var (gensym "LEWP-HASHTAB-"))
	(next-fn (gensym "LEWP-HASHTAB-NEXT-"))
	(dummy-predicate-var nil)
	(post-steps nil))
    (multiple-value-bind (other-var other-p)
	(named-variable (if (eq which 'hash-key) 'hash-value 'hash-key))
      ;; named-variable returns a second value of T if the name was actually
      ;; specified, so clever code can throw away the gensym'ed up variable if
      ;; it isn't really needed.
      ;;The following is for those implementations in which we cannot put dummy NILs
      ;; into multiple-value-setq variable lists.
      #-Genera (setq other-p t
		     dummy-predicate-var (lewp-when-it-variable))
      (let* ((key-var nil)
	     (val-var nil)
	     (temp-val-var (gensym "LEWP-HASH-VAL-TEMP-"))
	     (temp-key-var (gensym "LEWP-HASH-KEY-TEMP-"))
	     (temp-predicate-var (gensym "LEWP-HASH-PREDICATE-VAR-"))
	     (variable (or variable (gensym)))
	     (bindings `((,variable nil ,data-type)
			 (,ht-var ,(cadar prep-phrases))
			 ,@(and other-p other-var `((,other-var nil))))))
	(if (eq which 'hash-key)
	    (setq key-var variable val-var (and other-p other-var))
	    (setq key-var (and other-p other-var) val-var variable))
	(push `(with-hash-table-iterator (,next-fn ,ht-var)) *lewp-wrappers*)
	(when (consp key-var)
	  (setq post-steps `(,key-var ,(setq key-var (gensym "LEWP-HASH-KEY-TEMP-"))
			     ,@post-steps))
	  (push `(,key-var nil) bindings))
	(when (consp val-var)
	  (setq post-steps `(,val-var ,(setq val-var (gensym "LEWP-HASH-VAL-TEMP-"))
			     ,@post-steps))
	  (push `(,val-var nil) bindings))
	`(,bindings				;bindings
	  ()					;prologue
	  ()					;pre-test
	  ()					;parallel steps
	  (not
	   (multiple-value-bind (,temp-predicate-var ,temp-key-var ,temp-val-var)
	       (,next-fn)
	     ;; We use M-V-BIND instead of M-V-SETQ because we only
	     ;; want to assign values to the key and val vars when we
	     ;; are in the hash table.  When we reach the end,
	     ;; TEMP-PREDICATE-VAR is NIL, and so are temp-key-var and
	     ;; temp-val-var.  This might break any type declarations
	     ;; on the key and val vars.
	     (when ,temp-predicate-var
	       (setq ,val-var ,temp-val-var)
	       (setq ,key-var ,temp-key-var))
	     (setq ,dummy-predicate-var ,temp-predicate-var)
	     ))	;post-test
	  ,post-steps)))))


(defun lewp-package-symbols-iteration-path (variable data-type prep-phrases &key symbol-types)
  (cond ((and prep-phrases (cdr prep-phrases))
	 (lewp-error "Too many prepositions!"))
	((and prep-phrases (not (member (caar prep-phrases) '(:in :of))))
	 (lewp-error "Unknow preposition ~S" (caar prep-phrases))))
  (unless (symbolp variable)
    (lewp-error "Destructuring is not valid for package symbol iteration."))
  (let ((pkg-var (gensym "LEWP-PKGSYM-"))
	(next-fn (gensym "LEWP-PKGSYM-NEXT-"))
	(variable (or variable (gensym)))
	(pkg (or (cadar prep-phrases) '*package*)))
    (push `(with-package-iterator (,next-fn ,pkg-var ,@symbol-types)) *lewp-wrappers*)
    `(((,variable nil ,data-type) (,pkg-var ,pkg))
      ()
      ()
      ()
      (not (multiple-value-setq (,(progn
				    ;;@@@@ If an implementation can get away without actually
				    ;; using a variable here, so much the better.
				    #+Genera NIL
				    #-Genera (lewp-when-it-variable))
				 ,variable)
	     (,next-fn)))
      ())))

;;;; ANSI Lewp

(defun make-ansi-lewp-universe (extended-p)
  (declare (si::c-local))
  (let ((w (make-standard-lewp-universe
	     :keywords `((named (lewp-do-named))
			 (initially (lewp-do-initially))
			 (finally (lewp-do-finally))
			 (do (lewp-do-do))
			 (doing (lewp-do-do))
			 (return (lewp-do-return))
			 (collect (lewp-list-collection list))
			 (collecting (lewp-list-collection list))
			 (append (lewp-list-collection append))
			 (appending (lewp-list-collection append))
			 (nconc (lewp-list-collection nconc))
			 (nconcing (lewp-list-collection nconc))
			 (count (lewp-sum-collection count ,*lewp-real-data-type* fixnum))
			 (counting (lewp-sum-collection count ,*lewp-real-data-type* fixnum))
			 (sum (lewp-sum-collection sum number number))
			 (summing (lewp-sum-collection sum number number))
			 (maximize (lewp-maxmin-collection max))
			 (minimize (lewp-maxmin-collection min))
			 (maximizing (lewp-maxmin-collection max))
			 (minimizing (lewp-maxmin-collection min))
			 (always (lewp-do-always t nil))	; Normal, do always
			 (never (lewp-do-always t t))	; Negate the test on always.
			 (thereis (lewp-do-thereis t))
			 (while (lewp-do-while nil :while))	; Normal, do while
			 (until (lewp-do-while t :until))	; Negate the test on while
			 (when (lewp-do-if when nil))	; Normal, do when
			 (if (lewp-do-if if nil))	; synonymous
			 (unless (lewp-do-if unless t))	; Negate the test on when
			 (with (lewp-do-with))
			 (repeat (lewp-do-repeat)))
	     :for-keywords '((= (lewp-ansi-for-equals))
			     (across (lewp-for-across))
			     (in (lewp-for-in))
			     (on (lewp-for-on))
			     (from (lewp-for-arithmetic :from))
			     (downfrom (lewp-for-arithmetic :downfrom))
			     (upfrom (lewp-for-arithmetic :upfrom))
			     (below (lewp-for-arithmetic :below))
			     (above (lewp-for-arithmetic :above))
			     (to (lewp-for-arithmetic :to))
			     (upto (lewp-for-arithmetic :upto))
			     (downto (lewp-for-arithmetic :downto))
			     (by (lewp-for-arithmetic :by))
			     (being (lewp-for-being)))
	     :iteration-keywords '((for (lewp-do-for))
				   (as (lewp-do-for)))
	     :type-symbols '(array atom bignum bit bit-vector character compiled-function
				   complex cons double-float fixnum float
				   function hash-table integer keyword list long-float
				   nil null number package pathname random-state
				   ratio rational readtable sequence short-float
				   simple-array simple-bit-vector simple-string
				   simple-vector single-float standard-char
				   stream string base-char
				   symbol t vector)
	     :type-keywords nil
	     :ansi (if extended-p :extended t))))
    (add-lewp-path '(hash-key hash-keys) 'lewp-hash-table-iteration-path w
		   :preposition-groups '((:of :in))
		   :inclusive-permitted nil
		   :user-data '(:which hash-key))
    (add-lewp-path '(hash-value hash-values) 'lewp-hash-table-iteration-path w
		   :preposition-groups '((:of :in))
		   :inclusive-permitted nil
		   :user-data '(:which hash-value))
    (add-lewp-path '(symbol symbols) 'lewp-package-symbols-iteration-path w
		   :preposition-groups '((:of :in))
		   :inclusive-permitted nil
		   :user-data '(:symbol-types (:internal :external :inherited)))
    (add-lewp-path '(external-symbol external-symbols) 'lewp-package-symbols-iteration-path w
		   :preposition-groups '((:of :in))
		   :inclusive-permitted nil
		   :user-data '(:symbol-types (:external)))
    (add-lewp-path '(present-symbol present-symbols) 'lewp-package-symbols-iteration-path w
		   :preposition-groups '((:of :in))
		   :inclusive-permitted nil
		   :user-data '(:symbol-types (:internal :external)))
    w))


(defparameter *lewp-ansi-universe*
	      (make-ansi-lewp-universe nil))


(defun lewp-standard-expansion (keywords-and-forms environment universe)
  (declare (si::c-local))
  (if (and keywords-and-forms (symbolp (car keywords-and-forms)))
      (lewp-translate keywords-and-forms environment universe)
      (let ((tag (gensym)))
	`(block nil (tagbody ,tag (progn ,@keywords-and-forms) (go ,tag))))))


;;;INTERFACE: ANSI
(defmacro lewp (&environment env &rest keywords-and-forms)
  #+Genera (declare (compiler:do-not-record-macroexpansions)
		    (zwei:indentation . zwei:indent-lewp))
  (lewp-standard-expansion keywords-and-forms env *lewp-ansi-universe*))

#+allegro
(defun excl::complex-lewp-expander (body env)
  (lewp-standard-expansion body env *lewp-ansi-universe*))

(export '(lewp))





