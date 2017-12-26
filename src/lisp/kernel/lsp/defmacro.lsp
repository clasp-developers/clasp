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

#+clasp-min
(si::fset 'push
	   #'(lambda (args env)
               (declare (core:lambda-name push))
               (let* ((what (second args))
                      (where (caddr args)))
                 `(setq ,where (cons ,what ,where))))
	  t)

#+clasp-min
(si::fset 'pop
	   #'(lambda (args env)
               (declare (core:lambda-name pop))
               (let ((where (cadr args)))
                 `(let* ((l ,where)
                         (v (car l)))
                    (setq ,where (cdr l))
                    v)))
	  t)

#+clasp-min
(si::fset 'incf
	   #'(lambda (args env)
               (declare (core:lambda-name incf))
               (let* ((where (second args))
                      (what (caddr args)))
                 (if what
                     `(setq ,where (+ ,where ,what))
                     `(setq ,where (1+ ,where)))))
	  t)

#+clasp-min
(si::fset 'decf
	   #'(lambda (args env)
               (declare (core:lambda-name decf))
               (let* ((where (second args))
                      (what (caddr args)))
                 (if what
                     `(setq ,where (- ,where ,what))
                     `(setq ,where (1- ,where)))))
	  t)

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
       (when (and err (not allow-other-keys))
	 (error "The key ~s is not allowed" err)))
    (if (atom tail)
      (error "keyword list is not a proper list")
      (setq head (car tail) tail (cdr tail)))
    (if (atom tail)
      (error "keyword list is not a proper list")
      (setq arg (car tail) tail (cdr tail)))
    (cond ((eq head :allow-other-keys)
           (when (not aok-flag)
             (setq allow-other-keys arg aok-flag t)))
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

(defun sys::destructure (vl macro &optional macro-name
                         &aux (basis-form (gensym))
                           (destructure-symbols (list basis-form)))
  (declare (si::c-local)
	   (special *dl* *arg-check*))
  (labels ((tempsym ()
	     (let ((x (gensym)))
	       (push x destructure-symbols)
	       x))
	   (dm-vl (vl whole macro)
	     (multiple-value-bind (reqs opts rest key-flag keys allow-other-keys auxs)
		 (si::process-lambda-list vl (if macro 'macro 'destructuring-bind))
	       (let* ((pointer (tempsym))
		      (cons-pointer `(ext:truly-the cons ,pointer))
		      (unsafe-car `(car ,cons-pointer))
		      (unsafe-cdr `(cdr ,cons-pointer))
		      (unsafe-pop `(setq ,pointer ,unsafe-cdr))
		      (no-check nil)
		      (ppn (+ (length reqs) (first opts)))
		      all-keywords)
		 ;; In macros, eliminate the name of the macro from the list
		 (dm-v pointer (if macro
                                   ;; Special handling if define-compiler-macro called this
                                   (if (member macro '(define-compiler-macro bclasp-define-compiler-macro))
                                       `(if (and (eq (car ,whole) 'cl:funcall)
                                                 (consp (cadr ,whole))
                                                 (eq (caadr ,whole) 'cl:function)
                                                 (consp (cdadr ,whole))
                                                 (eq (second (second ,whole)) ',macro-name))
                                            (cddr (ext:truly-the cons ,whole))
                                            (cdr (ext:truly-the cons ,whole)))
                                       `(cdr (ext:truly-the cons ,whole)))
                                   whole))
		 (dolist (v (cdr reqs))
		   (dm-v v `(progn
			      (if (null ,pointer)
				  (dm-too-few-arguments ,basis-form))
			      (prog1 ,unsafe-car ,unsafe-pop))))
		 (dotimes (i (pop opts))
		   (let* ((x (first opts))
			  (init (second opts))
			  (sv (third opts)))
		     (setq opts (cdddr opts))
		     (cond (sv
			    (dm-v x `(if ,pointer ,unsafe-car ,init))
			    (dm-v sv `(and ,pointer (progn ,unsafe-pop t))))
			   (t
			    (dm-v x `(if ,pointer
					 (prog1 ,unsafe-car ,unsafe-pop)
					 ,init))))))
		 (when rest
		   (dm-v rest pointer)
		   (setq no-check t))
		 (dotimes (i (pop keys))
		   (let* ((temp (tempsym))
			  (k (first keys))
			  (v (second keys))
			  (init (third keys))
			  (sv (fourth keys)))
		     (setq no-check t)
		     (setq keys (cddddr keys))
		     (dm-v temp `(search-keyword ,pointer ',k))
		     (dm-v v `(if (eq ,temp 'missing-keyword) ,init ,temp))
		     (when sv (dm-v sv `(not (eq ,temp 'missing-keyword))))
		     (push k all-keywords)))
		 (do ((l auxs (cddr l))) ((endp l))
		   (let* ((v (first l))
			  (init (second l)))
		     (dm-v v init)))
		 (cond (key-flag
			(push `(check-keyword ,pointer ',all-keywords
                                              ,@(if allow-other-keys '(t) '()))
			      *arg-check*))
		       ((not no-check)
			(push `(if ,pointer (dm-too-many-arguments ,basis-form))
			      *arg-check*)))
                 ppn)))

	   (dm-v (v init)
	     (cond ((and v (symbolp v))
                    (let ((push-val (if init (list v init) v)))
                      (push push-val *dl*)))
                   ((and v (atom v))
                    (error "destructure: ~A is not a list nor a symbol" v))
                   ((eq (first v) '&whole)
                    (let ((whole-var (second v)))
                      (if (listp whole-var)
                          (let ((new-whole (tempsym)))
                            (dm-v new-whole init)
                            (dm-vl whole-var new-whole nil)
                            (setq whole-var new-whole))
                          (dm-v whole-var init))
                      (dm-vl (cddr v) whole-var nil)))
                   (t
                    (let* ((temp (tempsym))
                           (push-val (if init (list temp init) temp)))
		      (push push-val *dl*)
		      (dm-vl v temp nil))))))
    (let* ((whole basis-form)
	   (*dl* nil)
	   (*arg-check* nil))
      (declare (special *dl* *arg-check*))
      (cond ((listp vl)
	     (when (eq (first vl) '&whole)
               (let ((named-whole (second vl)))
                 (setq vl (cddr vl))
                 (if (listp named-whole)
                     (dm-vl named-whole whole nil)
                     (setq *dl* (list (list named-whole whole)))))))
	    ((symbolp vl)
	     (setq vl (list '&rest vl)))
	    (t (error "The destructuring-lambda-list ~s is not a list." vl)))
      (values (dm-vl vl whole macro) whole
	      (nreverse *dl*)
              *arg-check*
	      destructure-symbols))))

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
    (when decls (push `(declare ,@decls) body))
    (values body doc)))
#+clasp(export 'remove-documentation)

(defun find-declarations (body &optional (docp t))
  (multiple-value-bind (decls body doc)
      (process-declarations body docp)
    (values (if decls `((declare ,@decls)) nil)
	    body doc)))

;; Optional argument context can be 'cl:define-compiler-macro, 'core:bclasp-define-compiler-macro or 'cl:defmacro (default)
(defun sys::expand-defmacro (name vl body &optional (context 'cl:defmacro))
  (multiple-value-bind (decls body doc)
      (find-declarations body)
    ;; We turn (a . b) into (a &rest b)
    ;; This is required because MEMBER (used below) does not like improper lists
    (let ((cell (last vl)))
      (when (rest cell)
        (setq vl (nconc (butlast vl 0) (list '&rest (rest cell))))))
    ;; If we find an &environment variable in the lambda list, we take not of the
    ;; name and remove it from the list so that DESTRUCTURE does not get confused
    (let ((env-part (member '&environment vl :test #'eq)))
      (if env-part
          (setq vl (nconc (ldiff vl env-part) (cddr env-part))
                env-part (second env-part))
          (setq env-part (gensym)
                decls (list* `(declare (ignore ,env-part)) decls)))
                                        ;(bformat t "About to call multiple-value-call\n")
      (multiple-value-bind (ppn whole dl arg-check ignorables)
          (destructure vl context name)
        (values 
         `(lambda (,whole ,env-part &aux ,@dl)
            (declare (ignorable ,@ignorables) (core:lambda-name ,name))
            ,@decls
            (block ,(si::function-block-name name)
              ,@arg-check
              ,@body))
         ppn
         doc)))))

#+clasp-min
(si::fset 'defmacro
          #'(lambda (def env)
              (declare (ignore env) (core:lambda-name defmacro))
	      (let* ((name (second def))
		     (vl (third def))
		     (body (cdddr def))
		     (function))
		(multiple-value-bind (function pprint doc)
		    (sys::expand-defmacro name vl body)
		  (declare (ignore doc))
		  (setq function `(function ,function))
		  (when *dump-defmacro-definitions*
		    (bformat t "EARLY defmacro.lsp defmacro %s -> %s\n" name function)
		    #++(setq function `(si::bc-disassemble ,function)))
		  (ext:register-with-pde def `(si::fset ',name ,function
                                                        t ; macro
                                                        ,pprint ; ecl pprint
                                                        ',vl ; lambda-list
                                                        )))))
	  t)

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
    (multiple-value-bind (ppn whole dl arg-check ignorables)
        (destructure vl nil)
      (declare (ignore ppn))
      `(let* ((,whole ,list) ,@dl)
	 (declare (ignorable ,@ignorables))
         ,@decls
         ,@arg-check
         ,@body))))
