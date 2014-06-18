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
;;;;  CMPGLOBALS -- Global variables and flag definitions
;;;;

(in-package #-new-cmp "COMPILER" #+new-cmp "C-DATA")

;;;
;;; VARIABLES
;;;

;;; --cmpinline.lsp--
;;;
;;; Empty info struct
;;;
#-new-cmp
(defvar *info* (make-info))
(defvar *inline-blocks* 0)
(defvar *opened-c-braces* 0)
;;; *inline-blocks* holds the number of C blocks opened for declaring
;;; temporaries for intermediate results of the evaluation of inlined
;;; function calls.

(defvar *inline-max-depth* 3
  "Depth at which inlining of functions stops.")
(defvar *inline-information* nil)

;;; --cmputil.lsp--
;;;
;;; Variables and constants for error handling
;;;
(defvar *current-form* '|compiler preprocess|)
(defvar *current-toplevel-form* '|compiler preprocess|)
(defvar *compile-file-position* -1)
(defvar *first-error* t)
(defvar *active-protection* nil)
(defvar *pending-actions* nil)

(defvar *compiler-conditions* '()
  "This variable determines whether conditions are printed or just accumulated.")

(defvar *compile-print* nil
  "This variable controls whether the compiler displays messages about
each form it processes. The default value is NIL.")

(defvar *compile-verbose* nil
  "This variable controls whether the compiler should display messages about its
progress. The default value is T.")

(defvar *compiler-features* #+ecl-min nil #-ecl-min '#.*compiler-features*
  "This alternative list of features contains keywords that were gathered from
running the compiler. It may be updated by running ")

(defvar *suppress-compiler-messages*
  #+ecl-min 'compiler-debug-note #-ecl-min 'compiler-note
  "A type denoting which compiler messages and conditions are _not_ displayed.")

(defvar *suppress-compiler-notes* nil) ; Deprecated
(defvar *suppress-compiler-warnings* nil) ; Deprecated

(defvar *compiler-break-enable* nil)

(defvar *compiler-in-use* nil)
(defvar *compiler-input*)
(defvar *compiler-output1*)
(defvar *compiler-output2*)

;;; --cmpcbk.lsp--
;;;
;;; List of callbacks to be generated
;;;
(defvar *callbacks* nil)

;;; --cmpc-machine.lsp, cmpffi.lsp ---
(defvar *machine* nil)

;;; --cmpcall.lsp--
;;;
;;; Whether to use linking calls.
;;;
(defvar *compile-to-linking-call* t)
(defvar *compiler-declared-globals*)

;;; --cmpenv.lsp--
;;;
;;; These default settings are equivalent to (optimize (speed 3) (space 0) (safety 2))
;;;
(defvar *safety* 2)
(defvar *speed* 3)
(defvar *space* 0)
(defvar *debug* 0)

;;; Emit automatic CHECK-TYPE forms for function arguments in lambda forms.
(defvar *automatic-check-type-in-lambda* t)

;;;
;;; Compiled code uses the following kinds of variables:
;;; 1. Vi, declared explicitely, either unboxed or not (*lcl*, next-lcl)
;;; 2. Ti, declared collectively, of type object, may be reused (*temp*, next-temp)
;;; 4. lexi[j], for lexical variables in local functions
;;; 5. CLVi, for lexical variables in closures

(defvar *lcl* 0)		; number of local variables

#-new-cmp
(defvar *temp* 0)		; number of temporary variables
#-new-cmp
(defvar *max-temp* 0)		; maximum *temp* reached

(defvar *level* 0)		; nesting level for local functions

(defvar *lex* 0)		; number of lexical variables in local functions
(defvar *max-lex* 0)		; maximum *lex* reached

(defvar *env* 0)		; number of variables in current form
(defvar *max-env* 0)		; maximum *env* in whole function
(defvar *env-lvl* 0)		; number of levels of environments
#-new-cmp
(defvar *aux-closure* nil)	; stack allocated closure needed for indirect calls
#-new-cmp
(defvar *ihs-used-p* nil)       ; function must be registered in IHS?

#-new-cmp
(defvar *next-cmacro* 0)	; holds the last cmacro number used.
(defvar *next-cfun* 0)		; holds the last cfun used.

;;;
;;; *tail-recursion-info* holds NIL, if tail recursion is impossible.
;;; If possible, *tail-recursion-info* holds
;;	( c1-lambda-form  required-arg .... required-arg ),
;;; where each required-arg is a var-object.
;;;
(defvar *tail-recursion-info* nil)

(defvar *allow-c-local-declaration* t)

;;; --cmpexit.lsp--
;;;
;;; *last-label* holds the label# of the last used label.
;;; *exit* holds an 'exit', which is
;;	( label# . ref-flag ) or one of RETURNs (i.e. RETURN, RETURN-FIXNUM,
;;	RETURN-CHARACTER, RETURN-DOUBLE-FLOAT, RETURN-SINGLE-FLOAT, or
;;	RETURN-OBJECT).
;;; *unwind-exit* holds a list consisting of:
;;	( label# . ref-flag ), one of RETURNs, TAIL-RECURSION-MARK, FRAME,
;;	JUMP, BDS-BIND (each pushed for a single special binding), or a
;;	LCL (which holds the bind stack pointer used to unbind).
;;;
(defvar *last-label* 0)
(defvar *exit*)
(defvar *unwind-exit*)

(defvar *current-function* nil)

(defvar *cmp-env* nil
"The compiler environment consists of a pair or cons of two
lists, one containing variable records, the other one macro and
function recors:

variable-record = (:block block-name) |
                  (:tag ({tag-name}*)) |
                  (:function function-name) |
                  (var-name {:special | nil} bound-p) |
                  (symbol si::symbol-macro macro-function) |
                  CB | LB | UNWIND-PROTECT
macro-record =	(function-name function) |
                (macro-name si::macro macro-function)
                CB | LB | UNWIND-PROTECT

A *-NAME is a symbol. A TAG-ID is either a symbol or a number. A
MACRO-FUNCTION is a function that provides us with the expansion
for that local macro or symbol macro. BOUND-P is true when the
variable has been bound by an enclosing form, while it is NIL if
the variable-record corresponds just to a special declaration.
CB, LB and UNWIND-PROTECT are only used by the C compiler and
they denote closure, lexical environment and unwind-protect
boundaries. Note that compared with the bytecodes compiler, these
records contain an additional variable, block, tag or function
object at the end.")

(defvar *cmp-env-root*
  (cons nil (list (list '#:no-macro 'si::macro (constantly nil))))
"This is the common environment shared by all toplevel forms. It can
only be altered by DECLAIM forms and it is used to initialize the
value of *CMP-ENV*.")

;;; --cmplog.lsp--
;;;
;;; Destination of output of different forms. See cmploc.lsp for types
;;; of destinations.
;;;
(defvar *destination*)

;;; --cmpmain.lsp--
;;;
;;; Do we debug the compiler? Then we need files not to be deleted.

(defvar *debug-compiler* nil)
(defvar *delete-files* t)
(defvar *files-to-be-deleted* '())

(defvar *user-ld-flags* '()
"Flags and options to be passed to the linker when building FASL, shared libraries
and standalone programs. It is not required to surround values with quotes or use
slashes before special characters.")

(defvar *user-cc-flags* '()
"Flags and options to be passed to the C compiler when building FASL, shared libraries
and standalone programs. It is not required to surround values with quotes or use
slashes before special characters.")

;;;
;;; Compiler program and flags.
;;;

;;; --cmptop.lsp--
;;;
(defvar *do-type-propagation* t
  "Flag for switching on the type propagation phase. Use with care, experimental.")

(defvar *compiler-phase* nil)

(defvar *volatile*)
#-new-cmp
(defvar *setjmps* 0)

(defvar *compile-toplevel* T
  "Holds NIL or T depending on whether we are compiling a toplevel form.")

(defvar *clines-string-list* '()
  "List of strings containing C/C++ statements which are directly inserted
in the translated C/C++ file. Notice that it is unspecified where these
lines are inserted, but the order is preserved")

(defvar *compile-time-too* nil)
#-new-cmp
(defvar *not-compile-time* nil)

(defvar *permanent-data* nil)		; detemines whether we use *permanent-objects*
					; or *temporary-objects*
(defvar *permanent-objects* nil)	; holds { ( object (VV vv-index) ) }*
(defvar *temporary-objects* nil)	; holds { ( object (VV vv-index) ) }*
(defvar *load-objects* nil)		; hash with association object -> vv-location
(defvar *load-time-values* nil)		; holds { ( vv-index form ) }*,
;;;  where each vv-index should be given an object before
;;;  defining the current function during loading process.
(defvar *setf-definitions* nil)         ; C forms to find out (SETF fname) locations

(defvar *optimizable-constants* nil)	; (value . c1form) pairs for inlining constants
(defvar *use-static-constants-p*        ; T/NIL flag to determine whether one may
  #+ecl-min t #-ecl-min nil)            ; generate lisp constant values as C structs
(defvar *static-constants* nil)		; constants that can be built as C values
                                        ; holds { ( object c-variable constant ) }*

(defvar *compiler-constants* nil)	; a vector with all constants
					; only used in COMPILE

(defvar *proclaim-fixed-args* nil)	; proclaim automatically functions
					; with fixed number of arguments.
					; watch out for multiple values.

(defvar *global-vars* nil)		; variables declared special
(defvar *global-funs* nil)		; holds	{ fun }*
(defvar *use-c-global* nil)		; honor si::c-global declaration
(defvar *global-cfuns-array* nil)	; holds	{ fun }*
(defvar *linking-calls* nil)		; holds { ( global-fun-name fun symbol c-fun-name var-name ) }*
(defvar *local-funs* nil)		; holds { fun }*
(defvar *top-level-forms* nil)		; holds { top-level-form }*
(defvar *make-forms* nil)		; holds { top-level-form }*

;;;
;;;     top-level-form:
;;;	  ( 'DEFUN'     fun-name cfun lambda-expr doc-vv sp )
;;;	| ( 'DEFMACRO'  macro-name cfun lambda-expr doc-vv sp )
;;;	| ( 'ORDINARY'  expr )
;;;	| ( 'DECLARE'   var-name-vv )
;;;	| ( 'DEFVAR'	var-name-vv expr doc-vv )
;;;	| ( 'CLINES'	string* )
;;;	| ( 'LOAD-TIME-VALUE' vv )

;;; *global-entries* holds (... ( fname cfun return-types arg-type ) ...).
(defvar *global-entries* nil)

(defvar *global-macros* nil)

(defvar *self-destructing-fasl* '()
"A value T means that, when a FASL module is being unloaded (for
instance during garbage collection), the associated file will be
deleted. We need this for #'COMPILE because windows DLLs cannot
be deleted if they have been opened with LoadLibrary.")

(defvar *undefined-vars* nil)

;;; Only these flags are set by the user.
;;; If (safe-compile) is ON, some kind of run-time checks are not
;;; included in the compiled code.  The default value is OFF.

(defconstant +init-env-form+
  '((*gensym-counter* 0)
    (*compiler-in-use* t)
    (*compiler-phase* 't1)
    (*callbacks* nil)
    (*cmp-env-root* (copy-tree *cmp-env-root*))
    (*cmp-env* nil)
    #-new-cmp
    (*max-temp* 0)
    #-new-cmp
    (*temp* 0)
    #-new-cmp
    (*next-cmacro* 0)
    (*next-cfun* 0)
    (*last-label* 0)
    (*load-objects* (make-hash-table :size 128 :test #'equal))
    (*setf-definitions* nil)
    (*make-forms* nil)
    (*static-constants* nil)
    (*permanent-objects* nil)
    (*temporary-objects* nil)
    (*local-funs* nil)
    (*global-vars* nil)
    (*global-funs* nil)
    (*global-cfuns-array* nil)
    (*linking-calls* nil)
    (*global-entries* nil)
    (*undefined-vars* nil)
    (*top-level-forms* nil)
    (*compile-time-too* nil)
    (*clines-string-list* '())
    (*inline-blocks* 0)
    (*open-c-braces* 0)
    #+new-cmp
    (*type-and-cache* (type-and-empty-cache))
    #+new-cmp
    (*type-or-cache* (type-or-empty-cache))
    #+new-cmp
    (*values-type-or-cache* (values-type-or-empty-cache))
    #+new-cmp
    (*values-type-and-cache* (values-type-and-empty-cache))
    #+new-cmp
    (*values-type-primary-type-cache* (values-type-primary-type-empty-cache))
    #+new-cmp
    (*values-type-to-n-types-cache* (values-type-to-n-types-empty-cache))
    (si::*defun-inline-hook* 'maybe-install-inline-function)
    (*machine* (or *machine* +default-machine+))
    (*optimizable-constants* (make-optimizable-constants *machine*))
    (*inline-information*
     (let ((r (machine-inline-information *machine*)))
       (if r (si::copy-hash-table r) (make-inline-information *machine*))))
    ))

