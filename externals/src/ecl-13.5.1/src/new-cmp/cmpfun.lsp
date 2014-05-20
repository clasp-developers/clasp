;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPFUN  Library functions.

;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi and William F. Schelter.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.


(in-package "COMPILER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FUNCTION AND FORM PROPERTIES
;;;

(defun form-causes-side-effect (form)
  (if (listp form)
      (some #'form-causes-side-effect form)
      (case (c1form-name form)
        ((LOCATION VAR SYS:STRUCTURE-REF #+clos SYS:INSTANCE-REF)
         nil)
        (CALL-GLOBAL
         (let ((fname (c1form-arg 0 form))
               (args (c1form-arg 1 form)))
           (or (function-may-have-side-effects fname)
               (args-cause-side-effect args))))
        (t t))))

(defun args-cause-side-effect (forms)
  (some #'form-causes-side-effect forms))

(defun function-may-have-side-effects (fname)
  (declare (si::c-local))
  (not (get-sysprop fname 'no-side-effects)))

(defun function-may-change-sp (fname)
  (not (or (get-sysprop fname 'no-side-effects)
	   (get-sysprop fname 'no-sp-change))))

(defun function-can-be-evaluated-at-compile-time (fname)
  (get-sysprop fname 'pure))

(defun function-closure-variables (fun)
  (remove-if #'(lambda (x)
                 (or
                  ;; non closure variable
                  (not (ref-ref-ccb x))
                  ;; special variable
                  (eq (var-kind x) 'special)
                  ;; not actually referenced
                  (and (not (var-referenced-in-form x (fun-lambda fun)))
                       (not (var-changed-in-form x (fun-lambda fun))))
                  ;; parameter of this closure
                  ;; (not yet bound, therefore var-loc is OBJECT)
                  (eq (var-loc x) 'OBJECT)))
             (fun-referred-vars fun)))

(defun fun-narg-p (fun)
  (not (fun-fixed-narg fun)))

(defun fun-volatile-p (fun)
  (loop for f in (fun-lambda fun)
     thereis (and (not (tag-p f)) (eq (c1form-name f) 'frame-set))))

(defun fun-fixed-narg (fun)
  "Returns true if the function has a fixed number of arguments and it is not a closure.
The function thus belongs to the type of functions that ecl_make_cfun accepts."
  (let (narg)
    (and (not (eq (fun-closure fun) 'CLOSURE))
	 (= (fun-minarg fun) (setf narg (fun-maxarg fun)))
	 (<= narg si::c-arguments-limit)
	 narg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CERTAIN OPTIMIZERS
;;;

(defvar *princ-string-limit* 80)

(defun c1apply (destination args)
  (check-args-number 'APPLY args 2)
  (let* ((fun (first args))
	 (arguments (rest args)))
    (cond ((and (consp fun)
		(eq (first fun) 'LAMBDA))
	   (c1translate destination
                        (optimize-funcall/apply-lambda (cdr fun) arguments t)))
	  ((and (consp fun)
		(eq (first fun) 'EXT::LAMBDA-BLOCK))
	   (setf fun (macroexpand-1 fun))
	   (c1translate destination
                        (optimize-funcall/apply-lambda (cdr fun) arguments t)))
	  ((and (consp fun)
		(eq (first fun) 'FUNCTION)
		(consp (second fun))
		(member (caadr fun) '(LAMBDA EXT::LAMBDA-BLOCK)))
	   (c1apply destination (list* (second fun) arguments)))
	  (t
	   (c1funcall destination (list* '#'APPLY args))))))

(defun expand-rplaca/d (car-p cons value env)
  (flet ((main-form (car-p cons value)
           `(ffi:c-inline (,cons ,value) (:object :object)
                          :object
                          ,(if car-p "ECL_CONS_CAR(#0)=#1" "ECL_CONS_CDR(#0)=#1")
                          :one-liner t)))
    (if (policy-assume-right-type env)
        (main-form car-p cons value)
        (let ((aux (gensym)))
          `(let ((,aux ,cons))
             (declare (:read-only ,aux))
             (when (atom ,aux)
               (error-not-a 'rplaca 1 ,aux 'cons))
             (locally (declare (optimize (safety 0)))
               ,(main-form car-p aux value)))))))

(defmacro error-not-a (name ndx object type)
  `(c-inline (name ,ndx ,object 'cons)
             (:object :index :object :object) :void
             "FEwrong_type_nth_arg(#0,#1,#2,#3);" :one-liner nil))

(define-compiler-macro rplaca (&whole form cons value &environment env)
  (if (policy-inline-accessors env)
      (expand-rplaca/d (eq (first form) 'rplaca) cons value env)
      form))

(defconstant +assoc-expansions+
  '(('EQ . #1="ecl_assq(#0,#1)")
    (#'EQ . #1#)
    ('#'EQ . #1#)
    ('EQL . #2="ecl_assql(#0,#1)")
    (#'EQL . #2#)
    ('#'EQL . #2#)
    ('EQUAL . #3="ecl_assoc(#0,#1)")
    (#'EQUAL . #3#)
    ('#'EQUAL . #3#)
    ('EQUALP . #4="ecl_assql(#0,#1)")
    (#'EQUALP . #4#)
    ('#'EQUALP . #4#)))

(define-compiler-macro assoc (&whole form value list &rest extra &environment env)
  (unless extra
    (setf extra '(:test 'EQL)))
  (when (and (= (length extra) 2)
             (eq (first extra) :test))
    (let ((test (assoc (second extra)
                       +assoc-expansions+ :test #'equal)))
      (when test
        (setf form `(C-INLINE (,value ,list) (:object :object) :object
                              ,(cdr test) :one-liner t :side-effects nil)))))
  form)

(define-compiler-macro nth (&whole form which cons &environment env)
  (if (and (policy-inline-accessors env) (numberp which) (<= 0 which 7))
      (case which
        (0 (list 'CAR cons))
        (1 (list 'CADR cons))
        (2 (list 'CADDR cons))
        (3 (list 'CADDDR cons))
        (4 (list 'CAR (list 'CDDDDR cons)))
        (5 (list 'CADR (list 'CDDDDR cons)))
        (6 (list 'CADDR (list 'CDDDDR cons)))
        (7 (list 'CADDDR (list 'CDDDDR cons))))
      form))

(define-compiler-macro nthcdr (&whole form which cons &environment env)
  (if (and (policy-inline-accessors env) (numberp which) (<= 0 which 7))
      (case which
        (0 cons)
        (1 (list 'CDR cons))
        (2 (list 'CDDR cons))
        (3 (list 'CDDDR cons))
        (4 (list 'CDDDDR cons))
        (5 (list 'CDR (list 'CDDDDR cons)))
        (6 (list 'CDDR (list 'CDDDDR cons)))
        (7 (list 'CDDDR (list 'CDDDDR cons))))
      form))

;;----------------------------------------------------------------------
;; We transform BOOLE into the individual operations, which have
;; inliners
;;

(define-compiler-macro boole (&whole form op-code op1 op2)
  (or (and (constantp op-code)
	   (case (eval op-code)
	     (#. boole-clr `(progn ,op1 ,op2 0))
	     (#. boole-set `(progn ,op1 ,op2 -1))
	     (#. boole-1 `(prog1 ,op1 ,op2))
	     (#. boole-2 `(progn ,op1 ,op2))
	     (#. boole-c1 `(prog1 (lognot ,op1) ,op2))
	     (#. boole-c2 `(progn ,op1 (lognot ,op2)))
	     (#. boole-and `(logand ,op1 ,op2))
	     (#. boole-ior `(logior ,op1 ,op2))
	     (#. boole-xor `(logxor ,op1 ,op2))
	     (#. boole-eqv `(logeqv ,op1 ,op2))
	     (#. boole-nand `(lognand ,op1 ,op2))
	     (#. boole-nor `(lognor ,op1 ,op2))
	     (#. boole-andc1 `(logandc1 ,op1 ,op2))
	     (#. boole-andc2 `(logandc2 ,op1 ,op2))
	     (#. boole-orc1 `(logorc1 ,op1 ,op2))
	     (#. boole-orc2 `(logorc2 ,op1 ,op2))))
      form))
