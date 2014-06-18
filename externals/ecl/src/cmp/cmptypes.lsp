;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 2010, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;
;;;;  CMPTYPES -- Data types for the Lisp core structures
;;;;

(in-package #-new-cmp "COMPILER" #+new-cmp "C-DATA")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMPILER STRUCTURES
;;;

;;;
;;; REF OBJECT
;;;
;;; Base object for functions, variables and statements. We use it to
;;; keep track of references to objects, how many times the object is
;;; referenced, by whom, and whether the references cross some closure
;;; boundaries.
;;;

(defstruct (ref (:print-object print-ref))
  name			;;; Identifier of reference.
  (ref 0 :type fixnum)	;;; Number of references.
  ref-ccb		;;; Cross closure reference.
			;;; During Pass1, T or NIL.
			;;; During Pass2, the index into the closure env
  ref-clb		;;; Cross local function reference.
			;;; During Pass1, T or NIL.
			;;; During Pass2, the lex-address for the
			;;; block id, or NIL.
  read-nodes		;;; Nodes (c1forms) in which the reference occurs
)

(deftype OBJECT () `(not (or fixnum character float)))

(defstruct (var (:include ref) (:constructor %make-var) (:print-object print-var))
;  name		;;; Variable name.
;  (ref 0 :type fixnum)
		;;; Number of references to the variable (-1 means IGNORE).
;  ref-ccb	;;; Cross closure reference: T or NIL.
;  ref-clb	;;; Cross local function reference: T or NIL.
;  read-nodes	;;; Nodes (c1forms) in which the reference occurs
  set-nodes	;;; Nodes in which the variable is modified
  kind		;;; One of LEXICAL, CLOSURE, SPECIAL, GLOBAL, :OBJECT,
                ;;; or some C representation type (:FIXNUM, :CHAR, etc)
  (function *current-function*)
		;;; For local variables, in which function it was created.
		;;; For global variables, it doesn't have a meaning.
  (functions-setting nil)
  (functions-reading nil)
		;;; Functions in which the variable has been modified or read.
  (loc 'OBJECT)	;;; During Pass 1: indicates whether the variable can
		;;; be allocated on the c-stack: OBJECT means
		;;; the variable is declared as OBJECT, and CLB means
		;;; the variable is referenced across Level Boundary and thus
		;;; cannot be allocated on the C stack.  Note that OBJECT is
		;;; set during variable binding and CLB is set when the
		;;; variable is used later, and therefore CLB may supersede
		;;; OBJECT.
		;;; During Pass 2:
  		;;; For :FIXNUM, :CHAR, :FLOAT, :DOUBLE, :OBJECT:
  		;;;   the cvar for the C variable that holds the value.
  		;;; For LEXICAL or CLOSURE: the frame-relative address for
		;;; the variable in the form of a cons '(lex-levl . lex-ndx)
		;;;	lex-levl is the level of lexical environment
		;;;	lex-ndx is the index within the array for this env.
		;;; For SPECIAL and GLOBAL: the vv-index for variable name.
  (type t)	;;; Type of the variable.
  #-new-cmp
  (index -1)    ;;; position in *vars*. Used by similar.
  #-new-cmp
  (ignorable nil) ;;; Whether there was an IGNORABLE/IGNORE declaration
  #+new-cmp
  read-only-p   ;;; T for variables that are assigned only once.
  )

;;; A function may be compiled into a CFUN, CCLOSURE or CCLOSURE+LISP_CLOSURE
;;; Here are examples of function FOO for the 3 cases:
;;; 1.  (flet ((foo () (bar))) (foo))		CFUN
;;; 2.  (flet ((foo () (bar))) #'foo)		CFUN+LISP_CFUN
;;; 3.  (flet ((foo () x)) #'(lambda () (foo))) CCLOSURE
;;; 4.  (flet ((foo () x)) #'foo)		CCLOSURE+LISP_CLOSURE

;;; A function can be referenced across a ccb without being a closure, e.g:
;;;   (flet ((foo () (bar))) #'(lambda () (foo)))
;;;   [the lambda also need not be a closure]
;;; and it can be a closure without being referenced across ccb, e.g.:
;;;   (flet ((foo () x)) #'foo)  [ is this a mistake in local-function-ref?]
;;; Here instead the lambda must be a closure, but no closure is needed for foo
;;;   (flet ((foo () x)) #'(lambda () (foo)))
;;; So we use two separate fields: ref-ccb and closure.
;;; A CCLOSURE must be created for a function when:
;;; 1. it appears within a FUNCTION construct and
;;; 2. it uses some ccb references (directly or indirectly).
;;; ref-ccb corresponds to the first condition, i.e. function is referenced
;;;   across CCB. It is computed during Pass 1. A value of 'RETURNED means
;;;   that it is immediately within FUNCTION.
;;; closure corresponds to second condition and is computed in Pass 2 by
;;;   looking at the info-referenced-vars and info-local-referenced of its body.

;;; A LISP_CFUN or LISP_CLOSURE must be created when the function is returned.
;;; The LISP funob may then be referenced locally or across LB or CB:
;;;     (flet ((foo (z) (bar z))) (list #'foo)))
;;;     (flet ((foo (z) z)) (flet ((bar () #'foo)) (bar)))
;;;     (flet ((foo (z) (bar z))) #'(lambda () #'foo)))
;;; therefore we need field funob.

(defstruct (fun (:include ref))
;  name			;;; Function name.
;  (ref 0 :type fixnum)	;;; Number of references.
			;;; During Pass1, T or NIL.
			;;; During Pass2, the vs-address for the
			;;; function closure, or NIL.
;  ref-ccb		;;; Cross closure reference.
 			;;; During Pass1, T or NIL, depending on whether a
			;;; function object will be built.
			;;; During Pass2, the vs-address for the function
			;;; closure, or NIL.
;  ref-clb		;;; Unused.
;  read-nodes		;;; Nodes (c1forms) in which the reference occurs
  cfun			;;; The cfun for the function.
  #+new-cmp
  (last-lcl 0)		;;; Number of local variables (just to bookkeep names)
  #+new-cmp
  (last-label 0)	;;; Number of generated labels (same as last-lcl)
  (level 0)		;;; Level of lexical nesting for a function.
  (env 0)     		;;; Size of env of closure.
  (global nil)		;;; Global lisp function.
  (exported nil)	;;; Its C name can be seen outside the module.
  (no-entry nil)	;;; NIL if declared as C-LOCAL. Then we create no
			;;; function object and the C function is called
			;;; directly
  (shares-with nil)	;;; T if this function shares the C code with another one.
			;;; In that case we need not emit this one.
  closure		;;; During Pass2, T if env is used inside the function
  var			;;; the variable holding the funob
  description		;;; Text for the object, in case NAME == NIL.
  #+new-cmp
  lambda-list		;;; List of (requireds optionals rest-var keywords-p
		        ;;;          keywords allow-other-keys-p)
  lambda		;;; Lambda c1-form for this function.
  lambda-expression     ;;; LAMBDA or LAMBDA-BLOCK expression
  (minarg 0)		;;; Min. number arguments that the function receives.
  (maxarg call-arguments-limit)
			;;; Max. number arguments that the function receives.
  (return-type '(VALUES &REST T))
  #+new-cmp
  doc			;;; Documentation
  (parent *current-function*)
			;;; Parent function, NIL if global.
  (local-vars nil)	;;; List of local variables created here.
  (referenced-vars nil)	;;; List of external variables referenced here.
  (referenced-funs nil)	;;; List of external functions called in this one.
			;;; We only register direct calls, not calls via object.
  (referencing-funs nil);;; Functions that reference this one
  (child-funs nil)	;;; List of local functions defined here.
  #+new-cmp
  (debug 0)		;;; Debug quality
  (file (car ext:*source-location*))
			;;; Source file or NIL
  (file-position (or (cdr ext:*source-location*) *compile-file-position*))
			;;; Top-level form number in source file
  #+new-cmp
  (toplevel-form *current-toplevel-form*)
  #+new-cmp
  code-gen-props	;;; Extra properties for code generation
  (cmp-env (cmp-env-copy)) ;;; Environment
  required-lcls         ;;; Names of the function arguments
  )

(defstruct (blk (:include ref))
;  name			;;; Block name.
;  (ref 0 :type fixnum)	;;; Number of references.
;  ref-ccb		;;; Cross closure reference.
			;;; During Pass1, T or NIL.
			;;; During Pass2, the ccb-lex for the
			;;; block id, or NIL.
;  ref-clb		;;; Cross local function reference.
			;;; During Pass1, T or NIL.
			;;; During Pass2, the lex-address for the
			;;; block id, or NIL.
;  read-nodes		;;; Nodes (c1forms) in which the reference occurs
  exit			;;; Where to return.  A label.
  destination		;;; Where the value of the block to go.
  var			;;; Variable containing the block ID.
  #-new-cmp
  (type '(VALUES &REST T)) ;;; Estimated type.
  #+new-cmp
  env                   ;;; Block environment.
  )

(defstruct (tag (:include ref))
;  name			;;; Tag name.
;  (ref 0 :type fixnum)	;;; Number of references.
;  ref-ccb		;;; Cross closure reference.
			;;; During Pass1, T or NIL.
;  ref-clb		;;; Cross local function reference.
			;;; During Pass1, T or NIL.
;  read-nodes		;;; Nodes (c1forms) in which the reference occurs
  label			;;; Where to jump: a label.
  unwind-exit		;;; Where to unwind-no-exit.
  var			;;; Variable containing frame ID.
  index			;;; An integer denoting the label.
  #+new-cmp
  env                   ;;; Tag environment.
  )

(defstruct (info)
  (local-vars nil)	;;; List of var-objects created directly in the form.
  #-new-cmp
  (type '(VALUES &REST T)) ;;; Type of the form.
  (sp-change nil)	;;; Whether execution of the form may change
			;;; the value of a special variable.
  (volatile nil)	;;; whether there is a possible setjmp. Beppe
  )

(defstruct (inline-info)
  name			;;; Function name
  arg-rep-types		;;; List of representation types for the arguments
  return-rep-type	;;; Representation type for the output
  arg-types		;;; List of lisp types for the arguments
  return-type		;;; Lisp type for the output
  exact-return-type	;;; Only use this expansion when the output is
			;;; declared to have a subtype of RETURN-TYPE
  multiple-values       ;;; Works with all destinations, including VALUES / RETURN
  expansion		;;; C template containing the expansion
  one-liner		;;; Whether the expansion spans more than one line
)

(defstruct (c1form (:include info)
		   (:print-object print-c1form)
		   (:constructor do-make-c1form))
  (name nil)
  (parents nil)
  #+new-cmp
  (env (c-env:cmp-env-copy)) ;; Environment in which this form was compiled
  #-new-cmp
  (env (cmp-env-copy)) ;; Environment in which this form was compiled
  (args '())
  (side-effects nil) ;;; Does it have side effects
  (form nil)
  (toplevel-form nil)
  (file nil)
  (file-position 0))

(defstruct vv
  (location nil)
  (used-p nil)
  (permanent-p t)
  (value nil))

(defstruct machine
  (c-types '())
  rep-type-hash
  sorted-types
  inline-information)

(defstruct (rep-type (:constructor %make-rep-type))
  (index 0) ; Precedence order in the type list
  (name t)
  (lisp-type t)
  (bits nil)
  (numberp nil)
  (integerp nil)
  (c-name nil)
  (to-lisp nil)
  (from-lisp nil)
  (from-lisp-unsafe nil))
