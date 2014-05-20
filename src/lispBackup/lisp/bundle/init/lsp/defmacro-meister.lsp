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
;;;;         defines SYS:DEFMACRO*, the defmacro preprocessor

(in-package "SYSTEM")

#+ecl-min
(si::fset 'push
	  #'(lambda-block push (args env)
	      (let* ((what (second args))
		     (where (caddr args)))
		`(setq ,where (cons ,what ,where))))
	  t)

#+ecl-min
(si::fset 'pop
	  #'(lambda-block pop (args env)
	      (let ((where (cadr args)))
		`(let* ((l ,where)
			(v (car l)))
		   (setq ,where (cdr l))
		   v)))
	  t)

#+ecl-min
(si::fset 'incf
	  #'(lambda-block incf (args env)
			  (let* ((where (second args))
				 (what (caddr args)))
			    (if what
				`(setq ,where (+ ,where ,what))
				`(setq ,where (1+ ,where)))))
	  t)

#+ecl-min
(si::fset 'decf
	  #'(lambda-block decf (args env)
			  (let* ((where (second args))
				 (what (caddr args)))
			    (if what
				`(setq ,where (- ,where ,what))
				`(setq ,where (1- ,where)))))
	  t)
(export '( push pop incf decf))



      









(defun sys::search-keyword (list key)
  (cond ((atom list) 'missing-keyword)
	((atom (cdr list)) 'missing-keyword)
	((eq (car list) key) (cadr list))
	(t (search-keyword (cddr list) key))))

(defun check-keyword (tail keywords &optional (allow-other-keys nil aok-flag))
  (do (head
       arg
       (err nil))
      ((null tail)
       (if (and err (not allow-other-keys))
	   (error "The key ~s is not allowed" err)))
    (if (atom tail)
	(error "keyword list is not a proper list")
	(setq head (car tail) tail (cdr tail)))
    (if (atom tail)
	(error "keyword list is not a proper list")
	(setq arg (car tail) tail (cdr tail)))
    (cond ((eq head :allow-other-keys)
	   (if (not aok-flag)
	       (setq allow-other-keys tail aok-flag t)))
	  ((not (member head keywords))
	   (setq err head)))))


(defun dm-too-many-arguments (*current-form*)
  (error "Too many arguments supplied to a macro or a destructuring-bind form:~%~s"
	 *current-form*))

 (defun dm-too-few-arguments (form-or-nil)
  (if form-or-nil
      (let ((*current-form* form-or-nil))
	(error "Too few arguments supplied to a macro or a destructuring-bind form:~%~S"
	       *current-form*))
      (error "Too few arguments supplied to a inlined lambda form.")))

(defvar  *dl*)
(defvar *arg-check*)
(defvar *key-check*)

#|
(si::fset 'dbg-value #'(lambda (whole env) nil) t)
(si::fset 'dbg-msg #'(lambda (whole env) nil) t)
|#



(defparameter *debug-destructure* t)

(si::fset 'dbg-value #'(lambda (whole env)
			 (if *debug-destructure*
			     (let ((var (cadr whole)))
			       `(bformat t "%s:%d DBG-VALUE %s --> %s\n" ,(source-file-info-file-name (source-file-info whole)) ,(source-file-info-lineno whole) ,(repr var) ,var))))
	  t)

(si::fset 'dbg-msg #'(lambda (whole env)
		       (if *debug-destructure*
			   (let ((msg (cadr whole)))
			     `(bformat t "%s:%d DBG-MSG %s\n" ,(source-file-info-file-name (source-file-info whole)) ,(source-file-info-lineno whole) ,msg))))
	  t)



(si::fset 'prog1 #'(lambda (whole env)
		     (let ((sym (gensym))
			   (first (cadr whole))
			   (body (cddr whole)))
		       (if body
			   `(let ((,sym ,first))
			      ,@body
			      ,sym)
			   first)))
	  t)
(export 'prog1)



(defun sys::destructure (vl macro &aux (basis-form (gensym)) )
  (declare (si::c-local)
	   (special *dl* *arg-check*))
  (dbg-value vl)
  (dbg-value macro)
  (labels ((destructure-dm-vl (vl whole macro)
	     (dbg-value vl)
	     (dbg-value whole)
	     (dbg-value macro)
	     (multiple-value-bind (reqs opts rest key-flag keys allow-other-keys auxs)
		 (si::process-lambda-list-for-destructure vl (if macro 'macro 'destructuring-bind))
	       (let* ((pointer (gensym))
		      (cons-pointer `(the cons ,pointer))
		      (unsafe-car `(car ,cons-pointer))
		      (unsafe-cdr `(cdr ,cons-pointer))
		      (unsafe-pop `(setq ,pointer ,unsafe-cdr))
		      (no-check nil)
		      (ppn (+ (length reqs) (first opts)))
		      all-keywords)
		 ;; In macros, eliminate the name of the macro from the list
		 (destructure-dm-v pointer (if macro `(cdr (the cons ,whole)) whole))
		 #|		 (dolist (v (cdr reqs))
		 (destructure-dm-v v `(progn
		 (if (null ,pointer)
		 (dm-too-few-arguments ,basis-form))
		 (prog1 ,unsafe-car ,unsafe-pop))))
		 |#
		 (BLOCK NIL
		   (dbg-msg "Top of block")
		   (LET ((cur (CDR REQS)))
		     (dbg-msg "top of let")
		     (TAGBODY
			(dbg-msg "top of tagbody")
		      start
			(dbg-msg "about to do if")
			(if (ENDP cur)
			    nil
			    (progn
			      (dbg-msg "progn AAA")
			      (LET* ((V (CAR cur)))
				(dbg-msg "inner let")
				(SETQ cur (CDR cur))
				(TAGBODY
				   (dbg-msg "inner tagbody")
				   (destructure-DM-V V
						     `(PROGN
							(IF (NULL ,POINTER)
							    (DM-TOO-FEW-ARGUMENTS ,BASIS-FORM))
							(PROG1 ,UNSAFE-CAR ,UNSAFE-POP)))))
			      (dbg-msg "about to go start")
			      (GO start)))))
		   NIL)

		 #|		 (dotimes (i (pop opts))
		 (let* ((x (first opts))
		 (init (second opts))
		 (sv (third opts)))
		 (setq opts (cdddr opts))
		 (if sv
		 (progn
		 (destructure-dm-v x `(if ,pointer ,unsafe-car ,init))
		 (destructure-dm-v sv `(and ,pointer (progn ,unsafe-pop t))))
		 (progn
		 (destructure-dm-v x `(if ,pointer
		 (prog1 ,unsafe-car ,unsafe-pop)
		 ,init))))))
		 |#
		 (BLOCK NIL
		   (dbg-msg "Starting opts")
		   (LET ((I 0) (icount (POP OPTS)))
		     (DECLARE (TYPE UNSIGNED-BYTE I)
			      (TYPE INTEGER icount))
		     (dbg-value icount)
		     (dbg-value opts)
		     (TAGBODY
			(dbg-msg "About to go bottom")
			(GO bottom)
		      top
			(progn
			   (LET* ((X (FIRST OPTS)) (INIT (SECOND OPTS)) (SV (THIRD OPTS)))
			     (SETQ OPTS (CDDDR OPTS))
			     (dbg-value sv)
			     (dbg-value x)
			     (IF SV
				 (PROGN
				   (destructure-DM-V X
						     `(IF ,POINTER
							  ,UNSAFE-CAR
							  ,INIT))
				   (destructure-DM-V SV `(AND ,POINTER (PROGN ,UNSAFE-POP T))))
				 (PROGN
				   (destructure-DM-V X
						     `(IF ,POINTER
							  (PROG1 ,UNSAFE-CAR ,UNSAFE-POP)
							  ,INIT))))))
			(dbg-value I)
			(PSETQ I (1+ I))
		      bottom
			(dbg-msg "At bottom of opts")
			(dbg-value I)
			(dbg-value icount)
			(if (>= I icount)
			    nil
			    (progn
			      (dbg-msg "About to GO TOP")
			      (GO top)))
			(dbg-msg "About to RETURN-FROM NIL")
			(RETURN-FROM NIL (PROGN
					   (dbg-msg "return-from nil")
					   NIL))
			) ;; TAGBODY
		     ) ;; LET
		   ) ;; BLOCK NIL
		 (dbg-msg "came out of block nil")
		 (dbg-value rest)
		 (if rest
		     (progn
		       (dbg-value rest)
		       (dbg-value pointer)
		       (destructure-dm-v rest pointer)
		       (setq no-check t)))
#| (dotimes (i (pop keys)) ;
		 (let* ((temp (gensym))
		 (k (first keys))
		 (v (second keys))
		 (init (third keys))
		 (sv (fourth keys)))
		 (setq no-check t)
		 (setq keys (cddddr keys))
		 (destructure-dm-v temp `(search-keyword ,pointer ',k))
		 (destructure-dm-v v `(if (eq ,temp 'missing-keyword) ,init ,temp))
		 (if sv (destructure-dm-v sv `(not (eq ,temp 'missing-keyword))))
		 (push k all-keywords)))
		 |#
		 (BLOCK NIL
		   (LET ((I 0) (count779 (POP KEYS)))
		     (DECLARE (TYPE UNSIGNED-BYTE I)
			      (TYPE INTEGER count779))
		     (TAGBODY
			(GO bottom)
		      top
			(progn
			   (LET* ((TEMP (GENSYM))
				  (K (FIRST KEYS))
				  (V (SECOND KEYS))
				  (INIT (THIRD KEYS))
				  (SV (FOURTH KEYS)))
			     (SETQ NO-CHECK T)
			     (SETQ KEYS (CDDDDR KEYS))
			     (destructure-DM-V TEMP `(SEARCH-KEYWORD ,POINTER ',K))
			     (destructure-DM-V V
					       `(IF (EQ ,TEMP 'MISSING-KEYWORD)
						    ,INIT
						    ,TEMP))
			     (IF SV
				 (destructure-DM-V SV `(NOT (EQ ,TEMP 'MISSING-KEYWORD))))
			     (PUSH K ALL-KEYWORDS)))
			(dbg-value I)
			(PSETQ I (1+ I))
		      bottom
			(dbg-value I)
			(if (>= I count779) nil (GO top))
			(RETURN-FROM NIL (PROGN NIL)))))

		 #|		 (do ((l auxs (cddr l))) ((endp l))
		 (let* ((v (first l))
		 (init (second l)))
		 (destructure-dm-v v init)))
		 |#
		 (BLOCK NIL
		   (LET ((L AUXS))
		     (TAGBODY
			(GO next)
		      top
			(TAGBODY
			   (LET* ((V (FIRST L)) (INIT (SECOND L)))
			     (destructure-DM-V V INIT)))
			(PSETQ L (CDDR L))
		      next
			(if (ENDP L) nil (GO top))
			(RETURN-FROM NIL (PROGN)))))

		 (if key-flag
		     (push `(check-keyword ,pointer ',all-keywords
					   ,@(if allow-other-keys '(t) '()))
			   *arg-check*)
		     (if (not no-check)
			 (push `(if ,pointer (dm-too-many-arguments ,basis-form))
			       *arg-check*)
			 nil))
		 (dbg-msg "leaving destructure-dm-vl")
		 ppn)))
	   (destructure-dm-v (v init)
	     (dbg-value v)
	     (dbg-value init)
	     (dbg-value (and v (symbolp v)))
	     (if (and v (symbolp v))
		 (progn
		   (push (if init (list v init) v) *dl*)
		   (dbg-value *dl*)
		   *dl*)
		 (progn
		   (dbg-value (and v (atom v)))
		   (if (and v (atom v))
		       (error "destructure: ~A is not a list nor a symbol" v)
		       (if (eq (first v) '&whole)
			   (let ((whole-var (second v)))
			     (if (listp whole-var)
				 (let ((new-whole (gensym)))
				   (dbg-value new-whole)
				   (dbg-value init)
				   (destructure-dm-v new-whole init)
				   (destructure-dm-vl whole-var new-whole nil)
				   (setq whole-var new-whole))
				 (progn
				   (dbg-value whole-var)
				   (dbg-value init)
				   (destructure-dm-v whole-var init)
				   ))
			     (destructure-dm-vl (cddr v) whole-var nil))
			   (let ((temp (gensym)))
			     (progn
			       (push (if init (list temp init) temp) *dl*)
			       (dbg-value *dl*)
			       *dl*)
			     (destructure-dm-vl v temp nil)))
		       )))
	     )
	   )
    (dbg-msg "Next stage")
    (let* ((whole basis-form)
	   (*dl* nil)
	   (*arg-check* nil))
      (declare (special *dl* *arg-check*))
      (dbg-value *dl*)
      (if (listp vl)
	  (progn
	    (if (eq (first vl) '&whole)
		(let ((named-whole (second vl)))
		  (setq vl (cddr vl))
		  (if (listp named-whole)
		      (destructure-dm-vl named-whole whole nil)
		      (progn
			(dbg-value (list (list named-whole whole)))
			(setq *dl* (list (list named-whole whole))))
		      ))))
	  (if (symbolp vl)
	      (setq vl (list '&rest vl))
	      (error "The destructuring-lambda-list ~s is not a list." vl)))
      (dbg-value *dl*)
      (values (destructure-dm-vl vl whole macro) whole
	      (nreverse *dl*)
	      *arg-check*))))

;;; valid lambda-list to DEFMACRO is:
;;;
;;;	( [ &whole sym ]
;;;	  [ &environment sym ]
;;;	  { v }*
;;;	  [ &optional { sym | ( v [ init [ v ] ] ) }* ]
;;;	  {  [ { &rest | &body } v ]
;;;	     [ &key { sym | ( { sym | ( key v ) } [ init [ v ]] ) }*
;;;		    [ &allow-other-keys ]]
;;;	     [ &aux { sym | ( v [ init ] ) }* ]
;;;	  |  . sym }
;;;	 )
;;;
;;; where v is short for { defmacro-lambda-list | sym }.
;;; A symbol may be accepted as a DEFMACRO lambda-list, in which case
;;; (DEFMACRO <name> <symbol> ... ) is equivalent to
;;; (DEFMACRO <name> (&REST <symbol>) ...).
;;; Defmacro-lambda-list is defined as:
;;;
;;;	( { v }*
;;;	  [ &optional { sym | ( v [ init [ v ] ] ) }* ]
;;;	  {  [ { &rest | &body } v ]
;;;	     [ &key { sym | ( { sym | ( key v ) } [ init [ v ]] ) }*
;;;		    [ &allow-other-keys ]]
;;;	     [ &aux { sym | ( v [ init ] ) }* ]
;;;	  |  . sym }
;;;	 )

(defun find-documentation (body)
  (nth-value 3 (process-declarations body t)))

(defun remove-documentation (body)
  (multiple-value-bind (decls body doc)
      (process-declarations body t)
    (if decls (push `(declare ,@decls) body))
    (values body doc)))

(defun find-declarations (body &optional (doc t))
  (multiple-value-bind (decls body doc)
      (process-declarations body doc)
    (values (if decls `((declare ,@decls)) nil)
	    body doc)))

(defun expand-defmacro (name vl body)
  (multiple-value-bind (decls body doc)
      (find-declarations body)
    ;; We turn (a . b) into (a &rest b)
    ;; This is required because MEMBER (used below) does not like improper lists
    (let ((cell (last vl)))
      (if (rest cell)
	  (setq vl (nconc (butlast vl 0) (list '&rest (rest cell))))))
    ;; If we find an &environment variable in the lambda list, we take not of the
    ;; name and remove it from the list so that DESTRUCTURE does not get confused
    (let ((env (member '&environment vl :test #'eq)))
      (if env
	  (setq vl (nconc (ldiff vl env) (cddr env))
		env (second env))
	  (setq env (gensym)
		decls (list* `(declare (ignore ,env)) decls)))
      (multiple-value-bind (ppn whole dl arg-check)
	  (destructure vl t)
	(values `(lambda-block ,name (,whole ,env &aux ,@dl)
			       ,@decls 
			       ,@arg-check
			       ,@body)
		ppn
		doc)))))


(defun parse-macro (name vl body &optional env)
  (multiple-value-bind (lambda-block ppn doc)
      (expand-defmacro name vl body)
    lambda-block))

;; ecl defmacro
(fset 'defmacro
      #'(lambda-block defmacro (def env)
		      (declare (ignore env))
		      (let* ((name (second def))
			     (vl (third def))
			     (body (cdddr def))
			     (function))
			(multiple-value-bind (function pprint doc)
			    (expand-defmacro name vl body)
			  (declare (ignore doc))
			  (setq function `(function ,function))
			  `(si::fset ',name ,function t ))))
      t)

;; meister defmacro
#|
(fset 'meister-defmacro
      #'(lambda (def env)
	  (let* ((name (cadr def))
		 (vl (caddr def))
		 (body (cdddr def))
		 (func))
	    (multiple-value-bind (declares code doc)
		(process-declarations body t)
	      (let* ((local-whole (gensym "local-whole"))
		     (local-env (gensym "local-env"))
		     (inner-ll (process-macro-lambda-list vl))
		     (inner-lambda `(lambda ,inner-ll ,@declares
					    #|(break "about to invoke inner code") |#
					    ,@code)))
		;;			    (break "About to set the func")
		(setq func
		      `#'(lambda ,(list local-whole local-env)
			   (block ,name
			     (apply (function ,inner-lambda)
				    ,local-whole
				    ,local-env
				    ,local-whole
				    nil))))))
	    #| ;; Old way used my goofy lambda-block special
	    (setq func
	    `(lambda-block ,name
	    ,(list local-whole local-env)
	    (apply (function ,inner-lambda) ,local-whole ,local-env ,local-whole nil)))))
	    |#
	    ;;			(bformat t "macro set as:\n%s\n" func )
	    `(si::fset ',name ,func t )))
      t)
|#

(fset 'multiple-value-bind 
      #'(lambda (def env)
	  (let ((vars (cadr def))
		(multiple-value-form (caddr def))
		(body (cdddr def))
		(restname (gensym)))
	    `(multiple-value-call #'(lambda (&optional ,@(mapcar #'list vars) &rest ,restname ) ,@body) ,multiple-value-form)))
      t)
(export '(multiple-value-bind))



(export 'defmacro)



;;; valid lambda-list to DESTRUCTURING-BIND is:
;;;
;;;	( [ &whole sym ]
;;;	  { v }*
;;;	  [ &optional { sym | ( v [ init [ v ] ] ) }* ]
;;;	  {  [ { &rest | &body } v ]
;;;	     [ &key { sym | ( { sym | ( key v ) } [ init [ v ]] ) }*
;;;		    [ &allow-other-keys ]]
;;;	     [ &aux { sym | ( v [ init ] ) }* ]
;;;	  |  . sym }
;;;	 )
;;;
;;; where v is short for { destructuring-bind-lambda-list | sym }.
;;; A symbol may be accepted as a DESTRUCTURING-BIND lambda-list, in which case
;;; (DESTRUCTURING-BIND <name> <symbol> ... ) is equivalent to
;;; (DESTRUCTURING-BIND <name> (&REST <symbol>) ...).
;;; Destructuring-bind-lambda-list is defined as:
;;;
;;;	( [ &whole sym ]
;;;	  { v }*
;;;	  [ &optional { sym | ( v [ init [ v ] ] ) }* ]
;;;	  {  [ { &rest | &body } v ]
;;;	     [ &key { sym | ( { sym | ( key v ) } [ init [ v ]] ) }*
;;;		    [ &allow-other-keys ]]
;;;	     [ &aux { sym | ( v [ init ] ) }* ]
;;;	  |  . sym }
;;;	 )

(defmacro destructuring-bind (vl list &body body)
  (multiple-value-bind (decls body)
      (find-declarations body)
    (multiple-value-bind (ppn whole dl arg-check)
	(destructure vl nil)
      (declare (ignore ppn))
      `(let* ((,whole ,list) ,@dl)
	 ,@decls
	 ,@arg-check
	 ,@body))))

(export 'destructuring-bind)

#|

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ; ;
;;;					; ;
;;; MACROLET HELPER			; ;
;;;					; ;

(defun cmp-env-for-bytecodes (old-env)
  "Produce an environment which is safe to pass to the bytecodes
compiler. We remove all blocks and tags and ensure that
references to local variables will cause an error. This
environment can be used to bytecompile the functions in MACROLET
or SYMBOL-MACRO forms, and also to evaluate other forms."
  (declare (si::c-local))
  (flet ((local-var-error-function (name)
	   #'(lambda (whole env)
	       (declare (ignore whole env))
	       (error
		"In a MACROLET function you tried to access a local variable, ~A,
from the function in which it appears." name)))
	 (local-fun-error-function (name)
	   #'(lambda (whole env)
	       (declare (ignore whole env))
	       (error
		"In a MACROLET function you tried to access a local function, ~A,
from the function in which it appears." name))))
    (cons (do ((env (car old-env) (cdr env))
	       (variables '()))
	      ((endp env) (nreverse variables))
	    (let ((i (car env)))
	      (if (consp i)
		  (let ((name (first i)))
		    (if (not (keywordp name))
			(push (if (second i)
				  i
				  (list name 'si::symbol-macro (local-var-error-function name)))
			      variables))))))
	  (do ((env (cdr old-env) (cdr env))
	       (macros '()))
	      ((endp env) (nreverse macros))
	    (let ((i (car env)))
	      (if (consp i)
		  (push (if (eq (second i) 'SI::MACRO)
			    i
			    (list (first i) 'SI:MACRO (local-fun-error-function (first i))))
			macros)))))))

(defun macrolet-functions (definitions old-env)
  (declare (si::c-local))
  (let ((env (cmp-env-for-bytecodes old-env)))
    (si::eval-with-env
     (cons 'list
	   (mapcar #'(lambda (x)
		       (let* ((name (first x))
			      (llist (second x))
			      (def (cddr x)))
			 `(list ',name ,(si::expand-defmacro name llist def))))
		   definitions))
     env nil t)))

(defun cmp-env-register-macrolet (definitions old-env)
  (let ((macros (cdr old-env)))
    (dolist (record (macrolet-functions definitions old-env))
      (push (list (first record) 'si::macro (second record))
	    macros))
    (rplacd (truly-the cons old-env) macros)))
|#

