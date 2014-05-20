;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or modify
;;;;    it under the terms of the GNU General Public License as published by
;;;;    the Free Software Foundation; either version 2 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;;   setdoc.lsp
;;;;
;;;;                    Sets doc-strings for built-in symbols.

(in-package "COMPILER")			; in case it does not exist
(in-package "SYSTEM")

(defmacro docfun (symbol kind args doc)
  (do-docfun symbol kind args doc))

(defun do-docfun (symbol kind args doc)
  ;(print symbol)
  (assert (listp args))
  (ext:annotate symbol ':lambda-list nil args)
  (cond ((and doc (search "Syntax:" doc))
	 (setf args nil))
	((and doc (search "Args:" doc))
	 (setf args nil))
	((member kind '(macro special))
	 (setf args (format nil "Syntax: ~A" args)))
	(t
	 (setf args (format nil "Args: ~A" args))))
  (si::set-documentation
   symbol 'function
   (format nil "~A in ~A package:~@[~%~A~]~@[~%~A~]~%"
	   (ecase kind
	     (special "Special Form")
	     (macro "Macro")
	     (function "Function")
	     (method "Generic function"))
	   (package-name (symbol-package (si::function-block-name symbol)))
	   args doc)))

(defmacro docvar (symbol kind doc)
  (do-docvar symbol kind doc))

(defun do-docvar (symbol kind doc)
  ;(print symbol)
  (si::set-documentation
   symbol 'variable
   (format nil "~@(~A~) in ~A package:~A~%"
	   kind (package-name (symbol-package symbol)) doc)))

(defmacro doctype (symbol doc)
  (do-doctype symbol doc))

(defun do-doctype (symbol doc)
  ;(print symbol)
  (si::set-documentation symbol 'type doc))

(defun tree-search (tree x)
  (cond ((eq tree x)
	 t)
	((atom tree)
	 nil)
	((tree-search (car tree) x)
	 t)
	(t
	 (tree-search (cdr tree) x))))

(defun our-pde-hook (location definition output-form)
  (when (consp definition)
    (handler-case
	(let* ((documentation nil)
	       (name (second definition)))
	  (loop for i in (cddr definition)
	     do (cond ((stringp i)
		       (setf documentation i)
		       (return))
		      ((and (consp i) (eq (first i) 'DECLARE))
		       ;; Produce no documentation for si::c-local functions
		       (when (tree-search i 'si::c-local)
			 (return-from our-pde-hook output-form)))
		      (t (return))))
	  (case (first definition)
	    (defun (do-docfun name 'function (third definition) documentation))
	    (defmacro (do-docfun name 'macro (third definition) documentation))
	    ((defvar defparameter) (when documentation (do-docvar name 'variable documentation)))
	    (defconstant (when documentation (do-docvar name 'constant documentation)))
	    (deftype (when documentation (do-doctype name documentation)))))
      (error (c) (princ c) (quit))))
  output-form)

(setf ext:*register-with-pde-hook* #'our-pde-hook)

#||
(defmacro docfun (symbol kind args string)
  `(progn (si::putprop ',symbol ,string 'si::function-documentation)
	  (si::putprop ',symbol ',args 'arglist)
          ',symbol))

(defmacro docvar (symbol kind string)
          (declare (ignore kind))
  `(progn (si::putprop ',symbol ,string 'si::variable-documentation)
          ',symbol))

(defmacro doctype (symbol string)
  `(progn (si::putprop ',symbol ,string 'si::type-documentation)
          ',symbol))
||#

;;;----------------------------------------------------------------------
;;;	Ordered alphabetically for binary search
;;;----------------------------------------------------------------------

(docvar + variable "
The last top-level form.")

(docvar ++ variable "
The last-but-one top-level form.")

(docvar +++ variable "
The last-but-two top-level form.")

(docvar / variable "
The list of all values of the last top-level form.")

(docvar // variable "
The list of all values of the last-but-one top-level form.")

(docvar /// variable "
The list of all values of the last-but-two top-level form.")

(docvar - variable "
The top-level form ECL is currently evaluating.")

(docvar * variable "
The value of the last top-level form.")

(docfun * function (&rest numbers) "
Returns the product of the args.  With no args, returns 1.")

(docvar *debugger-hook* variable "
This is either NIL or a function of two arguments, a condition and the value
of *DEBUGGER-HOOK*. This function can either handle the condition or return
which causes the standard debugger to execute. The system passes the value
of this variable to the function because it binds *DEBUGGER-HOOK* to NIL
around the invocation.")

(docvar *debug-io* variable "
The stream used by the ECL debugger.  The initial value is a synonym stream to
*TERMINAL-IO*.")

(docvar *default-pathname-defaults* variable "
The default pathname used by some pathname-handling functions such as ENOUGH-
NAMESTRING.")

(docvar *error-output* variable "
The output stream to which error messages are output.  The initial value is an
synonym stream to *TERMINAL-IO*.")

(docvar *features* variable "
List of symbols that name features of the current version of ECL.  These
features are used in connection with the read macros #+ and #-.  When the
reader encounters
	#+ feature-spec form
it reads FORM in the usual manner if FEATURE-SPEC is satisfied.  Otherwise,
the reader just skips FORM.
	#- feature-spec form
is equivalent to
	#- (not feature-spec) form
A feature-spec may be a symbol, in which case the spec is satisfied iff the
symbol is an element of *FEATURES*.  Or else, a feature-spec must be one of
the following forms.
	(and {feature-spec}*)
		Satisfied iff all FEATURE-SPECs are satisfied
	(or {feature-spec}*)
		Satisfied iff at least one of FEATURE-SPECs is satisfied
	(not feature-spec)
		Satisfied iff FEATURE-SPEC is not satisfied")

#-boehm-gc
(docvar si::*gc-message* variable "
ECL specific.
If the value of SI::*GC-MESSAGE* is non-NIL, the garbage collector prints some
debugging information on the terminal.  Usually SI::*GC-MESSAGE* is set NIL.")

#-boehm-gc
(docvar si::*gc-verbose* variable "
ECL specific.
If the value of this variable is non-NIL, then the garbage collector notifies
that it begins to run whenever it is invoked.  Otherwise, garbage collection
begins silently.")

(docvar si::*ignore-eof-on-terminal-io* variable "
ECL specific.
If the value of this variable is non-NIL, ECL ignores the EOF-character
(usually ^D) on the terminal.  The initial value is NIL.")

#-boehm-gc
(docfun si::ignore-maximum-pages function (&optional (boolean t)) "
ECL specific.
Tells the ECL memory manager whether (non-NIL) or not (NIL) it should expand
memory when the maximum allocatable pages have been used up.  The initial
value is T. If no arguments are passed, returns the current value of the
flag.")

(docvar si::*indent-formatted-output* variable "
ECL specific.
The FORMAT directive ~~% indents the next line, if the value of this variable
is non-NIL.  If NIL, ~~% simply does Newline.")

(docvar si::*interrupt-enable* variable "
ECL specific.
If the value of SI::*INTERRUPT-ENABLE* is non-NIL, ECL signals an error on the
terminal interrupt (this is the default case).  If it is NIL, ECL ignores the
interrupt and assigns T to SI::*INTERRUPT-ENABLE*.")

(docvar ext::*invoke-debugger-hook* variable "
ECL specific.
This is either NIL or a designator for a function of two arguments,
to be run when the debugger is about to be entered.  The function is
run with *INVOKE-DEBUGGER-HOOK* bound to NIL to minimize recursive
errors, and receives as arguments the condition that triggered
debugger entry and the previous value of *INVOKE-DEBUGGER-HOOK*

This mechanism is an extension similar to the standard *DEBUGGER-HOOK*.
In contrast to *DEBUGGER-HOOK*, it is observed by INVOKE-DEBUGGER even when
called by BREAK.")

#-boehm-gc
(docvar si::*lisp-maxpages* variable "
ECL specific.
The current maximum number of pages (1 page = 2048 bytes) for the ECL process.
The result of changing the value of SI::*LISP-MAXPAGES* is unpredictable.")

(docvar *load-verbose* variable "
The default value for the :VERBOSE parameter of LOAD.
It initial value is T.")

(docvar *macroexpand-hook* variable "
The value of this variable must be a three-argument function object.
Each time a macro form is expanded, ECL calls that function with
	1. the macro expansion function (see MACRO-FUNCTION)
	2. the macro form to expand
	3. an environment (NIL in most case)
as three arguments, and uses the returned value as the expanded form.
The initial value of this variable is the function FUNCALL.")

(docfun si::*make-constant function (symbol value) "
ECL specific.
Declares that the global variable named by SYMBOL is a constant with VALUE as
its constant value.")

(docfun si::*make-special function (symbol) "
ECL specific.
Declares the variable named by NAME as a special variable.")

(docvar *package* variable "
The current package.  The initial value is the USER package.")

(docvar *print-array* variable "
Specifies whether ECL should print elements when it prints arrays other than
strings.  ECL uses the following abbreviation notations.
	#<bit-vector n>		for bit-vectors
	#<vector n>		for vectors other than strings and bit-vectors
	#<array n>		for arrays other than vectors
where N is a number that identifies the array.")

(docvar *print-base* variable "
The radix used to print integers and ratios.  The value must be an integer
from 2 to 36, inclusive.  The initial value is 10.")

(docvar *print-case* variable "
Specifies how to print ordinary symbols.  Possible values are:
	:UPCASE		in upper case
	:DOWNCASE	in lower case
	:CAPITALIZE	the first character in upper case, the rest in lower
The initial value is :UPCASE.")

(docvar *print-circle* variable "
Specifies whether the ECL printer should take care of circular lists.")

(docvar *print-escape* variable "
Specifies whether the ECL printer should output objects in the way that they
can be reread later if possible.")

(docvar *print-gensym* variable "
Specifies whether the ECL printer should prefix uninterned symbols with \"#:\".")

(docvar *print-length* variable "
Specifies how many elements the ECL printer should print when it prints a
list.  ECL printer prints all elements if the value of this variable is NIL.")

(docvar *print-level* variable "
Specifies how many levels of depth the ECL printer should print when it prints
a list.  ECL printer prints all levels if the value of this variable is NIL.")

(docvar *print-pretty* variable "
Specifies whether the ECL printer should pretty-print.  See PPRINT for more
information about pretty-printing.")

(docvar *print-radix* variable "
Specifies whether the ECL printer should print the radix when it prints
integers and ratios.")

#+profile
(docvar si::*profile-array* variable "
ECL specific.
Contains the profile histogram: two short integer counters are packed in each
value of this array of fixnums.")

(docvar *query-io* variable "
The query I/O stream. The initial value is a synonym stream to *TERMINAL-IO*.")

(docvar *random-state* variable "
The default random-state object used by RANDOM.")

(docvar *read-base* variable "
The radix used to read numbers.  The initial value is 10.")

(docvar *read-default-float-format* variable "
The default float format the ECL reader uses when reading floats.  Must be one
of the symbols SHORT-FLOAT, SINGLE-FLOAT, DOUBLE-FLOAT, and LONG-FLOAT.")

(docvar *read-suppress* variable "
When the value of this variable is non-NIL, the ECL reader parses input
characters without most of the ordinary processings such as interning.  Used
to skip over forms.")

(docvar *readtable* variable "
The current readtable.")

(docvar *standard-input* variable "
The default input stream used by the ECL reader.  The initial value is a
synonym stream to *TERMINAL-IO*.")

(docvar *standard-output* variable "
The default output stream used by the ECL printer.  The initial value is a
synonym stream to *TERMINAL-IO*.")

(docvar *terminal-io* variable "
The terminal I/O stream.")

(docvar *trace-output* variable "
The stream used for trace output.  The initial value is a synonym stream to
*TERMINAL-IO*.")

(docfun + function (&rest numbers) "
Returns the sum of the args.  With no args, returns 0.")

(docfun - function (number &rest more-numbers) "
Returns the first arg subtracted by the rest of args.  With one arg, returns
- NUMBER.")

(docfun / function (number &rest more-numbers) "
Returns the first arg divided by the rest of args.  With one arg, returns
1/NUMBER.")

(docfun /= function (number &rest more-numbers) "
Returns T if no two of the args are numerically equal; NIL otherwise.")

(docfun 1+ function (number) "
Returns NUMBER plus one.")

(docfun 1- function (number) "
Returns NUMBER minus one.")

(docfun < function (number &rest more-numbers) "
Returns T if the args are in increasing order; NIL otherwise.")

(docfun <= function (number &rest more-numbers) "
Returns T if the args are in non-decreasing order; NIL otherwise.")

(docfun = function (number &rest more-numbers) "
Returns T if all args are numerically equal; NIL otherwise.")

(docfun > function (number &rest more-numbers) "
Returns T if the args are in decreasing order; NIL otherwise.")

(docfun >= function (number &rest more-numbers) "
Returns T if the args are in non-increasing order; NIL otherwise.")

(docfun abs function (number) "
Returns the absolute value of NUMBER.")

(docfun acons function (key datum alist) "
Equivalent to (CONS (CONS KEY DATUM) ALIST).")

(docfun adjoin function (item list &key (key '#'identity) (test '#'eql) test-not) "
Returns cons of ITEM and LIST unless ITEM is already an element of LIST.
Otherwise, returns LIST.")

(docfun adjustable-array-p function (array) "
Returns T if ARRAY is adjustable; NIL otherwise.")

#-boehm-gc
(docfun allocate function (type number &optional (really-allocate nil)) "
ECL specific.
Sets the maximum number of pages for the type class of the ECL implementation
type TYPE to NUMBER.  If REALLY-ALLOCATE is non-NIL, then the specified number
of pages will be allocated immediately.")

#-boehm-gc
(docfun si::allocate-contiguous-pages function (number &optional (really-allocate nil)) "
ECL specific.
Sets the maximum number of pages for contiguous blocks to NUMBER.  If REALLY-
ALLOCATE is non-NIL, then the specified number of pages will be allocated
immediately.")

#+clos
(docfun si::allocate-gfun function (name arity hash-table) "
ECL/CLOS specific.
Allocates a gfun object in which NAME is the generic function name, ARITY
is the number of arguments and HASH-TABLE is the hashtable for cashing
methods.")

#+clos
(docfun si::allocate-instance function (class length) "
ECL/CLOS specific.
Allocates an istance of CLASS with LENGTH slots.")

#-boehm-gc
(docfun si::allocated-contiguous-pages function () "
ECL specific.
Returns the number of pages currently allocated for contiguous blocks.")

#-boehm-gc
(docfun si::allocated-pages function (type) "
ECL specific.
Returns the number of pages currently allocated for the type class of the ECL
implementation type TYPE.")

(docfun alpha-char-p function (char) "
Returns T if CHAR is alphabetic; NIL otherwise.")

(docfun alphanumericp function (char) "
Returns T if CHAR is either numeric or alphabetic; NIL otherwise.")

(docfun and macro (&rest forms) "
Evaluates FORMs in order.  If any FORM evaluates to NIL, returns
immediately with the value NIL.  Otherwise, returns all values of the
last FORM.")

(docfun append function (&rest lists) "
Constructs and returns a new list by concatenating the args.")

(docfun apply function (function arg &rest more-args) "
Calls FUNCTION with all ARGs except the last and all elements of the last ARG
as the arguments to FUNCTION.  Returns all values that FUNCTION returns.")

(docfun applyhook function (function list evalhookfn applyhookfn &optional (env nil)) "
Calls FUNCTION with all elements of LIST as the arguments and with *EVALHOOK*
and *APPLYHOOK* bound to EVALHOOKFN and APPLYHOOKFN respectively.  Returns all
values that FUNCTION returns.")

(docfun aref function (array &rest indexes) "
Returns the element of ARRAY specified by INDEXES.")

(docfun si::argc function () "
ECL specific.
Returns the number of arguments given in the command line that invoked ECL.")

(docfun si::argv function (n) "
ECL specific.
Returns the N-th argument given in the command line that invoked ECL.")

(doctype array "
An array is a compound object whose elements are referenced by indexing.  One-
dimensional arrays are called vectors.  Other arrays are notated as
	#?a( ... )	or	#?A( ... )
where '?' is actually the rank of the array.
Arrays may be displaced to another array, may have a fill-pointer, or may be
adjustable.  Other arrays are called simple-arrays.  Only simple-arrays can be
input in the above format.")

(docfun array-dimension function (array n) "
Returns the length of the N-th dimension of ARRAY.")

(docvar array-dimension-limit constant "
The upper bound of the length of an array dimension.")

(docfun array-element-type function (array) "
Returns the element type ARRAY.")

(docfun array-has-fill-pointer-p function (array) "
Returns T if ARRAY has a fill-pointer; NIL otherwise.")

(docfun array-rank function (array) "
Returns the rank of ARRAY.")

(docvar array-rank-limit constant "
The upper bound of the rank of an array.")

(docfun array-total-size function (array) "
Returns the total number of elements of ARRAY.")

(docvar array-total-size-limit constant "
The upper bound of the total number of elements of an array.")

(docfun arrayp function (x) "
Returns T if X is an array; NIL otherwise.")

(docfun ash function (integer count) "
Returns the integer obtained by shifting the bits that represent INTEGER as
specified by COUNT.  Shifts left in COUNT bits if COUNT is positive.  Shifts
right in -COUNT bits if COUNT is negative.")

(docfun assoc function (item alist &key (test '#'eql) test-not (key '#'identity)) "
Returns the first pair in ALIST whose car is equal (in the sense of TEST) to
ITEM.  Returns NIL if no such pair exists.
The function KEY is applied to extract the key for comparison.")

(docfun atan function (x &optional (y 1)) "
Returns the arc tangent of X/Y.")

(docfun atom function (x) "
Returns T if X is not a cons; NIL otherwise.")

(docfun si::bds-val function (n) "
ECL specific.
Returns the value of the N-th entity in the bind stack.")

(docfun si::bds-var function (n) "
ECL specific.
Returns the symbol of the N-th entity in the bind stack.")

(doctype bignum "
A bignum is an integer that is not a fixnum.")

(docfun bit-vector-p function (x) "
Returns T if X is a bit-vector; NIL otherwise.")

(docfun block special (name &body forms) "
Establishes a block named by NAME, evaluates FORMs in order, and returns all
values of the last FORM.  Returns NIL if no FORMs are given.
The scope of the established block is the body (i.e. the FORMs) of the BLOCK
form.  If (return-from name value-form) is evaluated within the scope, the
execution of the BLOCK form terminates immediately and all values of
VALUE-FORM will be returned as the values of the terminated BLOCK form.")

(docfun boole function (op integer1 integer2) "
Returns the integer produced by the logical operation specified by OP on the
two integers.  OP must be the value of one of the following constants.
	BOOLE-CLR	BOOLE-C1	BOOLE-XOR	BOOLE-ANDC1
	BOOLE-SET	BOOLE-C2	BOOLE-EQV	BOOLE-ANDC2
	BOOLE-1		BOOLE-AND	BOOLE-NAND	BOOLE-ORC1
	BOOLE-2		BOOLE-IOR	BOOLE-NOR	BOOLE-ORC2
Each logical operation on integers produces an integer represented by the bit
sequence obtained by a bit-wise logical operation on the bit sequences that
represent the integers.  Two's-complement representation is assumed to obtain
the bit sequence that represents an integer.  For example,
	 2:  ...010
	 1:  ...001
	 0:  ...000
	-1:  ...111
	-2:  ...110
where each '...' represents either an infinite sequence of 0's (for non-
negative integers) or an infinite sequence of 1's (for negative integers).")

(docvar boole-1 constant "
Makes BOOLE return INTEGER1.")

(docvar boole-2 constant "
Makes BOOLE return INTEGER2.")

(docvar boole-and constant "
Makes BOOLE return the AND of INTEGER1 and INTEGER2.")

(docvar boole-andc1 constant "
Makes BOOLE return the AND of {the NOT of INTEGER1} and INTEGER2.")

(docvar boole-andc2 constant "
Makes BOOLE return the AND of INTEGER1 and {the NOT of INTEGER2}.")

(docvar boole-c1 constant "
Makes BOOLE return the NOT of INTEGER1.")

(docvar boole-c2 constant "
Makes BOOLE return the NOT of INTEGER2.")

(docvar boole-clr constant "
Makes BOOLE return 0.")

(docvar boole-eqv constant "
Makes BOOLE return the EQUIVALENCE of INTEGER1 and INTEGER2.")

(docvar boole-ior constant "
Makes BOOLE return the INCLUSIVE OR of INTEGER1 and INTEGER2.")

(docvar boole-nand constant "
Makes BOOLE return the NOT of {the AND of INTEGER1 and INTEGER2}.")

(docvar boole-nor constant "
Makes BOOLE return the NOT of {the INCLUSIVE OR of INTEGER1 and INTEGER2}.")

(docvar boole-orc1 constant "
Makes BOOLE return the INCLUSIVE OR of {the NOT of INTEGER1} and INTEGER2.")

(docvar boole-orc2 constant "
Makes BOOLE return the INCLUSIVE OR of INTEGER1 and {the NOT of INTEGER2}.")

(docvar boole-set constant "
Makes BOOLE return -1.")

(docvar boole-xor constant "
Makes BOOLE return the EXCLUSIVE OR of INTEGER1 and INTEGER2.")

(docfun both-case-p function (char) "
Returns T if CHAR is an alphabetic character; NIL otherwise.  Equivalent to
ALPHA-CHAR-P.")

(docfun boundp function (symbol) "
Returns T if the global variable named SYMBOL has a value; NIL otherwise.")

#|
(docfun compiler:build-ecl function (program-name &rest components) "

Builds a standalone executable using the object files, libraries and flags
which follow PROGRAM-NAME, which is the name of the final executable.

Each argument in COMPONENTS can be either a symbol or a string. If it is a
string, it is passed as such to the C compiler when building the program. This
way you can specify FASL code (i.e. compiled lisp code) and additional
libraries or custom C code.

However, if the argument is a symbol, it is interpreted as the name of a lisp
library of FASL code. You should use symbols to call in optional parts of the
interpreter, such as the compiler 'CMP or the 'CLX library (not yet available)

For example:
	(compile-file \"my-code.lsp\" :system-p)
	(build-ecl \"my-ecl\" \"my-code.o\" \"-Bdynamic -lX11\" 'cmp)
builds an new interpreter with some custom lisp code given in \"my-code.o\" and
with the ECL compiler (You must explicitely mention the compiler if you want
it). Finally, the X-Windows dynamically linked libraries are also included
because \"my-code.lsp\" uses the foreign function interface to do some
graphics.")
|#

(docfun butlast function (list &optional (n 1)) "
Returns a copy of LIST with the last N elements removed.")

(docfun by function () "
ECL specific.
Exits from ECL.  Equivalent to BYE.")

(docfun bye function () "
ECL specific.
Exits from ECL.  Equivalent to BY.")

(docfun caaaar function (x) "
Equivalent to (CAR (CAR (CAR (CAR X)))).")

(docfun caaadr function (x) "
Equivalent to (CAR (CAR (CAR (CDR X)))).")

(docfun caaar function (x) "
Equivalent to (CAR (CAR (CAR X))).")

(docfun caadar function (x) "
Equivalent to (CAR (CAR (CDR (CAR X)))).")

(docfun caaddr function (x) "
Equivalent to (CAR (CAR (CDR (CDR X)))).")

(docfun caadr function (x) "
Equivalent to (CAR (CAR (CDR X))).")

(docfun caar function (x) "
Equivalent to (CAR (CAR X)).")

(docfun cadaar function (x) "
Equivalent to (CAR (CDR (CAR (CAR X)))).")

(docfun cadadr function (x) "
Equivalent to (CAR (CDR (CAR (CDR X)))).")

(docfun cadar function (x) "
Equivalent to (CAR (CDR (CAR X))).")

(docfun caddar function (x) "
Equivalent to (CAR (CDR (CDR (CAR X)))).")

(docfun cadddr function (x) "
Equivalent to (CAR (CDR (CDR (CDR X)))).")

(docfun caddr function (x) "
Equivalent to (CAR (CDR (CDR X))).")

(docfun cadr function (x) "
Equivalent to (CAR (CDR X)).")

(docvar call-arguments-limit constant "
The upper bound of the number of arguments to a function.  Ignore this value
since there is no such logical upper bound in ECL.")

(docfun car function (x) "
Returns the car of X if X is a cons.  Returns NIL if X is NIL.")

(docfun case macro (keyform &rest alternatives) "
Syntax: (case keyform {({key | ({key}*)} {form}*)}*)

Evaluates KEYFORM and searches a KEY that is EQL to the value of KEYFORM.  If
found, then evaluates FORMs in order that follow the KEY (or the key list that
contains the KEY) and returns all values of the last FORM.  Returns NIL if no
such key is found.  The symbols T and OTHERWISE may be used at the place of a
key list to specify the default case.")

(docfun catch special (tag-form &body forms) "
Syntax: (catch tag-form {form}*)

Sets up a catcher whose catch tag is the value of TAG-FORM.  Then evaluates
FORMs in order and returns all values of the last FORM.  During the evaluation
of FORMs, if a THROW form is evaluated that specifies a catch tag EQ to the
value of the TAG-FORM, then the execution of the CATCH form terminates
immediately and the values specified by the THROW form are returned as the
value of the CATCH form.")

(docfun cdaaar function (x) "
Equivalent to (CDR (CAR (CAR (CAR X)))).")

(docfun cdaadr function (x) "
Equivalent to (CDR (CAR (CAR (CDR X)))).")

(docfun cdaar function (x) "
Equivalent to (CDR (CAR (CAR X))).")

(docfun cdadar function (x) "
Equivalent to (CDR (CAR (CDR (CAR X)))).")

(docfun cdaddr function (x) "
Equivalent to (CDR (CAR (CDR (CDR X)))).")

(docfun cdadr function (x) "
Equivalent to (CDR (CAR (CDR X))).")

(docfun cdar function (x) "
Equivalent to (CDR (CAR X)).")

(docfun cddaar function (x) "
Equivalent to (CDR (CDR (CAR (CAR X)))).")

(docfun cddadr function (x) "
Equivalent to (CDR (CDR (CAR (CDR X)))).")

(docfun cddar function (x) "
Equivalent to (CDR (CDR (CAR X))).")

(docfun cdddar function (x) "
Equivalent to (CDR (CDR (CDR (CAR X)))).")

(docfun cddddr function (x) "
Equivalent to (CDR (CDR (CDR (CDR X)))).")

(docfun cdddr function (x) "
Equivalent to (CDR (CDR (CDR X))).")

(docfun cddr function (x) "
Equivalent to (CDR (CDR X)).")

(docfun cdr function (x) "
Returns the cdr of X if X is a cons.  Returns NIL if X is NIL.")

(docfun ceiling function (number &optional (divisor 1)) "
Returns the smallest integer not less than NUMBER/DIVISOR.  Returns the value
of (- NUMBER (* first-value DIVISOR)) as the second value.")

(docfun cerror function (continue-format-string error-format-string &rest args) "
Signals a continuable error.")

(docfun char function (string index) "
Returns the INDEX-th character in STRING.")

(docfun char-bit function (char name) "
Returns T if the specified bit attribute of CHAR is 'on'; NIL otherwise.
In ECL the bit-attributes handled are :control :meta :super and :hyper")

(docfun char-bits function (char) "
Returns the bit attributes of CHAR as an integer. In ECL it returns a value
between 0 and 16, since ECL handle 4 bit attributes.")

(docvar char-bits-limit constant "
The upper bound of values returned by CHAR-BITS.  16 in ECL.")

(docfun char-code function (char) "
Returns the character code of CHAR as a fixnum.")

(docvar char-code-limit constant "
The upper bound of values returned by CHAR-CODE.")

(docvar char-control-bit constant "
The bit position indicating a control character.  1 in ECL.")

(docfun char-downcase function (char) "
Returns the lower-case character corresponding to CHAR, if CHAR is upper-case.
Otherwise, returns CHAR.")

(docfun char-equal function (char &rest more-chars) "
Returns T if all CHARs are the same; NIL otherwise.  Lower-case characters are
regarded the same as the corresponding upper-case characters.")

(docfun char-font function (char) "
Returns the font attribute of CHAR.  Returns always 0 in ECL, since ECL
characters have no font attributes.")

(docvar char-font-limit constant "
The upper bound of values returned by CHAR-FONT.  1 in ECL.")

(docfun char-greaterp function (char &rest more-chars) "
Returns T if the character codes of CHARs are in decreasing order; NIL
otherwise.  For lower-case characters, codes of corresponding upper-case
characters are used.")

(docvar char-hyper-bit constant "
The bit position indicating a hyper character.  8 in ECL.")

(docfun char-int function (char) "
Returns the font, bits, and code attributes as an integer.  Equivalent to
CHAR-CODE in ECL.")

(docfun char-lessp function (char &rest more-chars) "
Returns T if the character codes of CHARs are in increasing order; NIL
otherwise.  For lower-case characters, codes of corresponding upper-case
characters are used.")

(docvar char-meta-bit constant "
The bit position indicating a meta character.  2 in ECL.")

(docfun char-name function (char) "
Returns the 'character name' of CHAR as a string; NIL if CHAR has no character
name.  Only #\\Backspace, #\\Tab, #\\Newline (or #\\Linefeed), #\\Page,
#\\Return, and #\\Rubout have character names in ECL.")

(docfun char-not-equal function (char &rest more-chars) "
Returns T if no two of CHARs are the same; NIL otherwise.  Lower-case
characters are regarded the same as the corresponding upper-case characters.")

(docfun char-not-greaterp function (char &rest more-chars) "
Returns T if the character codes of CHARs are in non-decreasing order; NIL
otherwise.  For lower-case characters, codes of corresponding upper-case
characters are used.")

(docfun char-not-lessp function (char &rest more-chars) "
Returns T if the character codes of CHARs are in non-increasing order; NIL
otherwise.  For lower-case characters, codes of corresponding upper-case
characters are used.")

(docvar char-super-bit constant "
The bit position indicating a super character.  4 in ECL.")

(docfun char-upcase function (char) "
Returns the upper-case character of CHAR, if CHAR is lower-case.  Otherwise,
returns CHAR.")

(docfun char/= function (char &rest more-chars) "
Returns T if no two of CHARs are the same; NIL otherwise.")

(docfun char< function (char &rest more-chars) "
Returns T if the character codes of CHARs are in increasing order; NIL
otherwise.")

(docfun char<= function (char &rest more-chars) "
Returns T if the character codes of CHARs are in non-decreasing order; NIL
otherwise.")

(docfun char= function (char &rest more-chars) "
Returns T if all CHARs are the same; NIL otherwise.")

(docfun char> function (char &rest more-chars) "
Returns T if the character codes of CHARs are in decreasing order; NIL
otherwise.")

(docfun char>= function (char &rest more-chars) "
Returns T if the character codes of CHARs are in non-increasing order; NIL
otherwise.")

(doctype character "
A character represents a character that can be handled by the computer.
Characters have font, bits, and code attributes.  Font and bits attributes
are always 0 in ECL.  Most versions of ECL uses ASCII code:
  000 - 037	#\\^@  #\\^A  #^B ... #\\Z  #\\^[  #\\^\\  #\\^]  #\\^^  #\\^_
		except #\\Tab(011)     #\\Newline(012)     #\\Page(014)
		       #\\Return(015)  #\\Backspace(031)
  040 - 057	#\\Space  #\\!  #\\\"  #\\#  #\\$  #\\%  #\\&  #\\'  #\\(  #\\)  #\\*
		#\\+  #\\,  #\\-  #\\.  #\\/
  060 - 071	#\\0  #\\1  #\\2  #\\3  #\\4  #\\5  #\\6  #\\7  #\\8  #\\9
  072 - 100	#\\:  #\\;  #\\<  #\\=  #\\>  #\\?  #\\@
  101 - 132	#\\A ... #\\Z
  133 - 140	#\\[  #\\\\  #\\]  #\\^  #\\_  #\\`
  141 - 172	#\\a ... #\\z
  173 - 177	#\\{  #\\|  #\\}  #\\~~  #\\Rubout
Some versions of ECL support additional characters to represent Japanese
character set.")

(docfun character function (x) "
Coerces X into a character if possible.  Signals an error if not possible.")

(docfun characterp function (x) "
Returns T if X is a character; NIL otherwise.")

(docfun ext:getcwd function (&optional (update-lisp t)) "
Returns the current working directory of the C library. When UPDATE-LISP is
true, *DEFAULT-PATHNAME-DEFAULTS* is set to this value.")

(docfun ext:chdir function (filespec &optional (update-lisp t)) "
Changes the current working directory of the C library to the one specified by
FILESPEC.  FILESPEC may be a symbol, a string, or a pathname. UPDATE-LISP
determines whether the value of *DEFAULT-PATHNAME-DEFAULTS* is also to be
changed.")

(docfun clear-input function (&optional (stream *standard-input*)) "
Clears the input buffer of STREAM and returns NIL.  Contents of the buffer are
discarded.")

(docfun clear-output function (&optional (stream *standard-output*)) "
Clears the output buffer of STREAM and returns NIL.  Contents of the buffer
are discarded.")

(docfun ffi:clines special (&body c-strings) "
Syntax: (clines {string}*)
The ECL compiler embeds STRINGs into the intermediate C language code.  The
interpreter ignores this form.")

#|| eliminated. Beppe
(docfun cdeclaration macro "(cdeclaration {string}*)" "
ECL specific.
The ECL compiler embeds STRINGs into the intermediate H language code.
The interpreter ignores this form.")

(docfun cinitialization macro "(cinitialization {string}*)" "
ECL specific.
The ECL compiler embeds STRINGs into the intermediate C language init_code
function. This allows to perform initialization operations when the binary
file is loaded.
The interpreter ignores this form.")
||#

(docfun close function (stream &key (abort nil)) "
Closes STREAM.  Returns NIL if STREAM is closed successfully; non-NIL
otherwise.  A non-NIL value of ABORT indicates an abnormal termination but ECL
ignores it.")

(docfun clrhash function (hash-table) "
Removes all entries of HASH-TABLE and returns HASH-TABLE.")

(docfun code-char function (code &optional (bits 0) (font 0)) "
Returns a character with the specified character code, if any.  Returns NIL
if no such character exists.  BITS and FONT specify the bits and font
attributes of the returned character but are both ignored in ECL.")

(doctype common "
COMMON is the type of all Common Lisp data objects.")

(docfun commonp function (x) "
Returns T if X is a Common Lisp object; NIL otherwise.")

(doctype compiled-function "
A compiled function is an object that is created by compiling a function.  A
compiled function is notated in either of the following formats:
	#<compiled-function s>
	#<compiled-closure nil>
where S is actually the symbol that names the function.")

(docfun si::compiled-function-name function (compiled-function) "
ECL specific.
Returns the function name associated with COMPILED-FUNCTION.")

(docfun compiled-function-p function (x) "
Returns T if X is a compiled function object; NIL otherwise.")

(docfun compiler-let special (bindings &body forms)
	"Syntax: (compiler-let ({var | (var [value])}*) {form}*)

When interpreted, this form works just like a LET form with all VARs declared
special.  When compiled, FORMs are processed with the VARs bound at compile
time, but no bindings occur when the compiled code is executed.")

(doctype complex "
A complex number represents a complex number in mathematical sense, consisting
of a real part and an imaginary part.  A complex number is notated as
	#c( realpart  imagpart )  or  #C( realpart  imagpart )
where REALPART and IMAGPART are non-complex numbers.")

(docfun complex function (realpart &optional (imagpart 0)) "
Returns a complex number with the given realpart and imagpart.  Returns
REALPART if it is a rational and IMAGPART is 0.")

(docfun complexp function (x) "
Returns T if X is a complex number; NIL otherwise.")

(docfun conjugate function (number) "
Returns the complex conjugate of NUMBER.  Returns NUMBER if it is not a
complex number.")

(doctype cons "
A cons is a compound object consisting of two components car and cdr.")

(docfun cons function (x y) "
Returns a new cons whose car and cdr are X and Y respectively.")

(docfun consp function (x) "
Returns T if X is a cons; NIL otherwise.")

(docfun constantp function (x) "
Returns T if ECL is sure that X, when given as a form, always evaluates to a
same value.  Returns NIL otherwise.  Typically used to check whether a symbol
names a constant variable.")

(docfun copy-alist function (alist) "
Returns a new list consisting of copies of all pairs in ALIST.")

(docfun copy-list function (list) "
Returns a new list consisting of all elements in LIST.")

(docfun copy-readtable function (&optional (readtable *readtable*) (to-readtable nil)) "
Returns a new copy of READTABLE.  If TO-READTABLE is non-NIL, then copies the
contents of READTABLE into TO-READTABLE and returns TO-READTABLE.")

(docfun copy-seq function (sequence) "
Returns a new copy of SEQUENCE.")

(docfun copy-symbol function (symbol &optional (flag nil)) "
Returns a new uninterned symbol with the same print name as SYMBOL.  If FLAG
is NIL, the symbol property of the new symbol is empty.  Otherwise, the new
symbol gets a copy of the property list of SYMBOL.")

(docfun copy-tree function (tree) "
Returns a copy of TREE.  Defined as:
	(defun copy-tree (tree)
	  (if (atom tree)
	      tree
	      (cons (copy-tree (car tree)) (copy-tree (cdr tree)))))")

(docfun cos function (radians) "
Returns the cosine of RADIANS.")

(docfun cosh function (number) "
Returns the hyperbolic cosine of NUMBER.")

(docfun count function (item sequence
       &key (key '#'identity) (test '#'eql) test-not
            (start 0) (end (length sequence)) (from-end nil)) "
Returns the number of elements in SEQUENCE satisfying TEST with ITEM as the
first argument.")

(docfun count-if function (test sequence
       &key (key '#'identity)
            (start 0) (end (length sequence)) (from-end nil)) "
Returns the number of elements in SEQUENCE satisfying TEST.")

(docfun count-if-not function (test sequence
            (start 0) (end (length sequence)) (from-end nil)) "
Returns the number of elements in SEQUENCE not satisfying TEST.")

(docfun declare special (&rest declaration-specifiers)
"Syntax: (declare {decl-spec}*)

Gives declarations.  Possible DECL-SPECs are:
  (SPECIAL {var}*)
  (TYPE type {var}*)
  (type {var}*) where 'type' is one of the following symbols
	array		fixnum		package		simple-string
	atom		float		pathname	simple-vector
	bignum		function	random-state	single-float
	bit		hash-table	ratio		standard-char
	bit-vector	integer		rational	stream
	character	keyword		readtable	string
	common		list		sequence	string-char
	compiled-function  long-float	short-float	symbol
	complex		nil		signed-byte	t
	cons		null		simple-array	unsigned-byte
	double-float	number		simple-bit-vector  vector
  (OBJECT {var}*)
  (FTYPE type {function-name}*)
  (FUNCTION function-name ({arg-type}*) {return-type}*)
  (INLINE {function-name}*)
  (NOTINLINE {function-name}*)
  (IGNORE {var}*)
  (OPTIMIZE {({SPEED | SPACE | SAFETY | COMPILATION-SPEED} {0 | 1 | 2 | 3})}*)
  (DECLARATION {non-standard-decl-name}*)
  (:READ-ONLY {variable-name}*).")

(docfun decode-float function (float) "
Returns the significand F, the exponent E, and the sign S of FLOAT.  These
values satisfy
	1/B <= F < 1
and			 E
	FLOAT = S * F * B
where B is the radix used to represent FLOAT.  S and F are floats of the same
float format as FLOAT, and E is an integer.")

(docfun defun macro (name lambda-list &body)
        "Syntax: (defun name lambda-list {decl | doc}* {form}*)
Defines a global function named by NAME.
The complete syntax of a lambda-list is:
	({var}*
	 [&optional {var | (var [init [svar]])}*]
	 [&rest var]
	 [&key {var | ({var | (keyword var)} [init [svar]])}*
	       [&allow-other-keys]]
	 [&aux {var | (var [init])}*])
The doc-string DOC, if supplied, is saved as a FUNCTION doc and can be
retrieved by (documentation 'NAME 'function).")

(docfun defmacro macro (name lambda-list &body body)
"Syntax: (defmacro name defmacro-lambda-list {decl | doc}* {form}*)
Defines a global macro named by NAME.  The complete syntax of DEFMACRO-LAMBDA-
LIST is:
	( [&whole var] [&environment var] . pvar )
where PVAR may be a symbol,
	( {pvar}* [&optional {var | (pvar [init [pvar]])}*] . var )
or
	( {pvar}*
	  [&optional {var | (pvar [init [pvar]])}*]
	  [{&rest | &body} pvar]
	  [&key {var | ({var | (keyword pvar)} [init [pvar]])}*
	        [&allow-other-keys]]
	  [&aux {var | (pvar [init])}*] )
The doc-string DOC, if supplied, is saved as a FUNCTION doc and can be
retrieved by (documentation 'NAME 'function).  See LIST for the backquote
macro useful for defining macros.")

(docfun delete function (item sequence
       &key (key '#'identity) (test '#'eql) test-not
            (start 0) (end (length sequence))
            (count most-positive-fixnum) (from-end nil)) "
Destructive REMOVE.  SEQUENCE may be destroyed.")

(docfun delete-file function (filespec) "
Deletes the specified file.  FILESPEC may be a symbol, a string, a pathname,
or a file stream.")

(docfun delete-if function (test sequence
       &key (key '#'identity) (start 0) (end (length sequence))
            (count most-positive-fixnum) (from-end nil)) "
Destructive REMOVE-IF.  SEQUENCE may be destroyed")

(docfun delete-if-not function (test sequence
       &key (key '#'identity) (start 0) (end (length sequence))
            (count most-positive-fixnum) (from-end nil)) "
Destructive REMOVE-IF-NOT.  SEQUENCE may be destroyed")

(docfun denominator function (rational) "
Returns the denominator of RATIONAL as a positive integer, if RATIONAL is a
ratio.  Returns RATIONAL if it is an integer.")

(docfun digit-char function (digit &optional (n 10) (font 0)) "
Returns a character that represents the DIGIT in radix N.  Returns NIL if no
such character exists.")

(docfun digit-char-p function (char &optional (n 10)) "
If CHAR represents a digit in radix N, then returns an integer represented by
that digit.  Otherwise, returns NIL.")

(docfun directory function (filespec) "
Returns a list of full pathnames of all those files that match FILESPEC.
FILESPEC may be a symbol, a string, a pathname, or a file stream.")

(docfun directory-namestring function (filespec) "
Returns as a string the directory part of the pathname specified by FILESPEC.
FILESPEC may be a symbol, a string, a pathname, or a file stream.")

(docfun si::displaced-array-p function (array) "
ECL specific.
Returns T if the ARRAY is displaced to another array; NIL otherwise.")

(docfun do macro (bindings (test &optional result) &body forms)
	"Syntax: (do ({(var [init [step]])}*) (test {result}*)
          {decl}* {tag | statement}*)

Establishes a NIL block, binds each VAR to the value of the corresponding INIT
(which defaults to NIL), and then executes STATEMENTs repeatedly until TEST is
satisfied.  After each iteration, evaluates STEP and assigns the value to the
corresponding VAR.  No assignment occurs for those VARs to which STEP is not
given.  When TEST is satisfied, evaluates RESULTs as a PROGN and returns all
values of the last RESULT.  Performs variable bindings and assignments in
parallel, just as LET and PSETQ do.")

(docfun do* macro (bindings (test &optional result) &body forms)
	"Syntax: (do* ({(var [init [step]])}*) (test {result}*)
          {decl}* {tag | statement}*)

Similar to DO, but performs variable bindings and assignments in serial, just
as LET* and SETQ do.")

(docfun dolist macro ((var form &optional result) &body forms)
	"Establishes a NIL block and executes STATEMENTs once for each member of the
list value of FORM, with VAR bound to the member.  Then evaluates RESULT
(which defaults to NIL) and returns all values.")

(doctype double-float "
A double-float is a double-precision floating point number.
DOUBLE-FLOAT as a type specifier is equivalent to LONG-FLOAT in ECL.")

(docfun dotimes macro ((var form &optional result) &body forms)
	"Establishes a NIL block and executes STATEMENTs once for each integer between
0 (inclusive) and the value of FORM (exclusive), with VAR bound to the
integer.  Then evaluates RESULT (which defaults to NIL) and returns all
values.")

(docfun eighth function (x) "
Equivalent to (CADDDR (CDDDDR X)).")

(docfun elt function (sequence n) "
Returns the N-th element of SEQUENCE.")

(docfun endp function (x) "
Returns T if X is NIL.  Returns NIL if X is a cons.  Otherwise, signals an
error.")

(docfun enough-namestring function (filespec &optional (defaults *default-pathname-defaults*)) "
Returns a string which uniquely identifies the file specified by FILESPEC,
with respect to DEFAULTS.  FILESPEC and DEFAULTS may be a symbol, a string, a
pathname, or a file stream.")

(docfun eq function (x y) "
Returns T if the args are identical; NIL otherwise.")

(docfun eql function (x y) "
Returns T if the args satisfy one of the following conditions.
	1. identical
	2. are numbers of the same type with the same value
	3. are characters that represent the same character
Returns NIL otherwise.")

(docfun equal function (x y) "
Returns T if the args satisfy one of the following conditions.
	1. EQL
	2. are conses with EQUAL cars and EQUAL cdrs
	3. are strings of the same length and element-wise EQL
	4. are bit-vectors of the same length and element-wise EQL
	5. are pathnames with EQUAL slots
Returns NIL otherwise.")

(docfun equalp function (x y) "
Returns T if the args satisfy one of the following conditions.
	1. EQUAL
	2. are characters that satisfy CHARACTER-EQUAL
	3. are numbers that satisfy =
	4. are conses with EQUALP cars and EQUALP cdrs
	5. are arrays of the same dimensions and element-wise EQUALP
Returns NIL otherwise.")

(docfun error function (format-string &rest args) "
Signals an error.  The args are FORMATed to *error-output*.")

(docfun eval function (form) "
Evaluates FORM and returns all values.")

(docfun eval-when special ((&rest situation) &body forms) "
Specifies when to evaluate FORMs.  Each SITUATION must be one of the following
symbols.
	COMPILE	(compile-time)
	LOAD	(load-time of the fasl file)
	EVAL	(load-time of the source file)")

(docfun evalhook function (form fun1 fun2 &optional (env nil)) "
Evaluates FORM with *EVALHOOK* bound to FUN1 and *APPLYHOOK* bound to FUN2,
and returns all the values.")

(docfun evenp function (integer) "
Returns T if INTEGER is an even number; NIL otherwise.")

(docfun exp function (number) "
Returns E raised to the power NUMBER, where E is the base of natural
logarithms.")

(docfun export function (symbol &optional (package *package*)) "
Register SYMBOL as an external symbol of PACKAGE.  SYMBOL may be a list of
symbols.")

(docfun expt function (number1 number2) "
Returns NUMBER1 raised to the power NUMBER2.")

(docfun fboundp function (symbol) "
Returns T if SYMBOL names a special form, a global macro, or a global
function.  Returns NIL otherwise.")

(docfun fifth function (x) "
Equivalent to (CAR (CDDDDR X)).")

(docfun file-author function (filespec) "
Returns the author of the specified file, as a string.  Returns NIL if the
author is unknown.  FILESPEC may be a symbol, a string, a pathname, or a file
stream.")

(docfun file-length function (file-stream) "
Returns the length of the specified FILE-STREAM.  Returns NIL if the length is
unknown.")

(docfun file-namestring function (filespec) "
Returns as a string the name, type, and version parts of the specified
pathname.  FILESPEC may be a symbol, a string, a pathname, or a file stream.")

(docfun file-position function (file-stream &optional file-position) "
With one arg, returns the current position of FILE-STREAM's file pointer as a
non-negative integer.  Returns NIL if the position is unknown.  With two args,
resets the file pointer and returns T.  Returns NIL if the file pointer cannot
be reset.  FILE-POSITION may be a non-negative integer, :START, or :END.")

(docfun file-write-date function (filespec) "
Returns an integer that represents the last write day-and-time of the
specified file (See GET-DECODED-TIME).   Returns NIL if the last write day-
and-time is unknown.  FILESPEC may be a symbol, a string, a pathname, or a
file stream.")

(docfun fill function (sequence item &key (start 0) (end (length sequence))) "
Replaces the specified elements of SEQUENCE with ITEM.")

(docfun fill-pointer function (vector) "
Returns the fill-pointer of VECTOR as an integer.  VECTOR must have a fill-
pointer.")

(docfun find function (item sequence
       &key (key '#'identity) (test '#'eql) test-not
            (start 0) (end (length sequence)) (from-end nil)) "
Returns the first element in SEQUENCE satisfying TEST with ITEM.  Returns NIL
if no such element exists.")

(docfun find-if function (test sequence
       &key (key '#'identity) (start 0) (end (length sequence)) (from-end nil)) "
Returns the index of the first element in SEQUENCE that satisfies TEST.
Returns NIL if no such element exists.")

(docfun find-if-not function (test sequence
       &key (key '#'identity) (start 0) (end (length sequence)) (from-end nil)) "
Returns the index of the first element in SEQUENCE that does not satisfy TEST.
Returns NIL if no such element exists.")

(docfun find-package function (name) "
Returns the package whose package name or nickname is NAME.  Returns NIL if no
such package exists.  NAME may be a string or a symbol.")

(docfun find-symbol function (string &optional (package *package*)) "
Searches PACKAGE for a symbol whose print name is NAME.  If such a symbol is
found, then returns the symbol as the first value and returns one of the
following symbols as the second value.
	:INTERNAL (internal symbol in PACKAGE)
	:EXTERNAL (external symbol in PACKAGE)
	:INHERITED (external symbol of a package that PACKAGE is using)
If no such symbol is found, returns NIL as the first and second values.")

(docfun finish-output function (&optional (stream *standard-output*)) "
Sends the contents of the output buffer for STREAM to the destination.  Waits
until the buffer becomes empty and then returns NIL.")

(docfun first function (x) "
Equivalent to CAR.")

(docfun si::fixnump function (x) "
ECL specific.
Returns T if the X is a fixnum; NIL otherwise.")

(docfun flet special ((&rest functions) &body forms) "
Introduces local functions and evaluates BODY as a PROGN.  BODY is the scope
of each local function but the local function definitions are not.  Thus each
local function can reference externally defined functions of the same name as
local functions.  Doc-strings for local functions are simply ignored.")

(doctype float "
A float (floating-point number) represents a real number or its approximation.
ECL supports two formats for floats.  One format is called SHORT-FLOAT and the
other format is called SINGLE-FLOAT, DOUBLE-FLOAT, or LONG-FLOAT.  Precisions
and exponent sizes of floats depends on the version of ECL.  See the ECL
Report at your hand for details.
The following syntax is used to notate a float.
	[+ | -] {digit}* . {digit}+ [exp]
	[+ | -] {digit}+ [. {digit}*}] exp
where DIGIT is a decimal digit (0,..,9) and EXP is
	marker [+ | -] {digit}+
with one of the following marker.
	e or E	the default float format
	s or S	short-float
	f or F	single-float
	d or D	double-float
	l or L	long-float
The default float format is single-float normally, but may be any other float
format.  See *READ-DEFAULT-FLOAT-FORMAT*.")

(docfun float function (number &optional float) "
With one arg, converts NUMBER to a single-float.  With two args, converts
NUMBER to a float of the same float format as FLOAT.")

(docfun float-digits function (float) "
Returns the number of radix-B digits used to represent the significand of
FLOAT, where B is the base number used in the representation of FLOAT.")

(docfun float-precision function (float) "
Returns the number of effective radix-B digits in the representation of the
significand of FLOAT, where B is the base number used in the representation
of FLOAT.")

(docfun float-radix function (float) "
Returns the base number used in the representation of FLOAT.")

(docfun float-sign function (float1 &optional (float2 (float 1 float1))) "
Returns a float with the same sign as FLOAT1 and with the same absolute value
as FLOAT2.")

(docfun floatp function (x) "
Returns T if X is a float; NIL otherwise.")

(docfun floor function (number &optional (divisor 1)) "
Returns the largest integer not larger than the NUMBER divided by DIVISOR.
Returns the value of (- NUMBER (* first-value DIVISOR)) as the second value.")

(docfun fmakunbound function (symbol) "
Removes the global function definition associated with SYMBOL.  Returns SYMBOL.")

(docfun force-output function (&optional (stream *standard-output*)) "
Sends the contents of the output buffer for STREAM to the destination.
Returns NIL without waiting until the buffer becomes empty.")

(docfun format function (destination format-string &rest args) "
Outputs ARGs to DESTINATION in the format specified by FORMAT-STRING.  FORMAT-
STRING is a string consisting of characters to output and format directives
which begin with '~~'.  Outputs to DESTINATION if it is a stream and to the
standard output if DESTINATION is T.  If DESTINATION is NIL, does not output
actually but returns the output as a string.  Here are some format directives:
	~~A	PRINCs one arg
	~~S	PRIN1s one arg
	~~D	Prints one integer in decimal
	~~B	Prints one integer in binary
	~~O	Prints one integer in octal
	~~X	Prints one integer in hexa
	~~%	Does TERPRI
	~~&	Does FRESH-LINE
	~~|	Outputs #\\Page
	~~~~	Outputs '~~'")

(docfun fourth function (x) "
Equivalent to CADDDR.")

(docfun fresh-line function (&optional (stream *standard-output*)) "
Outputs a newline character only if the current position of STREAM is not at
the beginning of a line.  Returns T if it outputs a newline; NIL otherwise.")

(docfun si::frs-bds function (n) "
ECL specific.
Returns the bind stack index of the N-th entity in the frame stack.")

(docfun si::frs-ihs function (n) "
ECL specific.
Returns the invocation history stack index of the N-th entity in the frame
stack.")

(docfun funcall function (function &rest args) "
Calls FUNCTION with the ARGs as the arguments and returns all values that the
call returns.")

(doctype function "
A function object specifies a function to be invoked by function-calling
functions such as FUNCALL or APPLY.  A function is either:
	1. a compiled function
	2. a list of one of the following form
		(lambda lambda-list . body)
		(lambda-block block-name lambda-list . body)
		(lambda-closure env1 env2 env3 lambda-list . body)
		(lambda-block-closure env1 env2 env3 block-name lambda-list
		                      . body)
	   where ENV1, ENV2, and ENV3 respectively represent the variable
	   environment, the function/macro environment, and the block/tagbody
	   environment at the time of the function creation.
	3. a symbol that names a global function.")

(docfun function special (function-name) "
If X is a lambda expression, (function x) creates and returns a lexical closure
of X in the current lexical environment.  If X is a symbol that names a function,
returns that function definition.")

(docfun functionp function (x) "
Returns T if X is an object that can be used to specify a function to be
invoked by function-calling functions such as FUNCALL or APPLY.  Returns NIL
otherwise.")

(docfun gc function (x) "
ECL specific.
Starts garbage collection with the specified collection level.  If X is NIL,
collects only cells.  If X is T, collects everything.")

#-boehm-gc
(docfun si::gc-time function () "
ECL specific.
Returns the amount of time (in 1/100 seconds) spent during garbage collection.")

(docfun gcd function (&rest integers) "
Returns the greatest common divisor of the args.")

(docfun gensym function (&optional (x nil)) "
Creates and returns a new uninterned symbol whose print name begins with some
prefix (initially \"G\"), followed by a generation number.  The generation
number is incremented by one at each call to GENSYM.  If X is an integer, it
becomes the new generation number.  If X is a string, it becomes the new
prefix.")

(docfun gentemp function (&optional (string \"T\") (package *package*)) "
Creates a new symbol interned in PACKAGE with PREFIX and returns the symbol.
The symbol is given a print name beginning with PREFIX followed by some
generation number.")

(docfun get function (symbol property &optional (default nil)) "
Searches the symbol property of SYMBOL for a property that is EQ to PROPERTY.
If found, returns the value of the property.  Otherwise, returns DEFAULT.")

(docfun get-dispatch-macro-character function (char subchar &optional (readtable *readtable*)) "
Returns the read macro for SUBCHAR associated with the dispatch macro
character CHAR in READTABLE.")

#-boehm-gc
(docfun si::get-hole-size function () "
ECL specific.
Returns as a fixnum the size of the memory hole (in pages).")

(docfun get-internal-real-time function () "
Returns the time (in 1/100 seconds) since the invocation of ECL.")

(docfun get-internal-run-time function () "
Returns the CPU time (in 1/100 seconds) since the invocation of ECL.")

(docfun get-macro-character function (char &optional (readtable *readtable*)) "
Returns the read macro associated with the macro character CHAR in READTABLE.
Returns the non-terminating-p flag (see READTABLE) as the second value.
Returns NIL if CHAR is not a macro character.")

(docfun get-output-stream-string function (string-output-stream) "
Returns as a string all outputs to STRING-OUTPUT-STREAM since the last call of
GET-OUTPUT-STREAM-STRING for the same stream.")

(docfun get-properties function (plist list) "
Searches PLIST for a property that is EQ to one of the members of LIST.
Returns three values.  If such a property if found, returns the property, the
value of the property, and the rest of LIST.  If not, returns three NILs.")

(docfun get-universal-time function () "
Returns the current day-and-time as an integer.  See DECODE-UNIVERSAL-TIME.")

(docfun ext:getenv function (string) "
ECL/UNIX specific.
Returns the environment with the name STRING as a string.  Returns NIL, if the
specified environment is not found.")

(docfun getf function (plist property &optional (default nil)) "
Searches PLIST for a property that is EQ to PROPERTY.  If one is found,
returns the value of the property.  If not, returns DEFAULT.
The SETF form
	(setf (getf place property-form) value-form)
replaces the property value of the plist stored in PLACE, or adds a new
property if the plist does not have the property yet.")

(docfun gethash function (key hash-table &optional (default nil)) "
Searches HASH-TABLE for the entry of KEY.  If found, returns the value of the
entry and T, as two values.  If not, returns DEFAULT and NIL.")

#+clos
(docfun si::gfun-instance function (gfun) "
ECL/CLOS specific.
Returns the generic function instance associated with the GFUN
generic function object.")

#+clos
(docfun si::gfun-instance-set function (gfun instance) "
ECL/CLOS specific.
Sets to INSTANCE the generic function instance associated with the
FUN generic function object.")

#+clos
(docfun si::gfun-name function (gfun) "
ECL/CLOS specific.
Returns the name of the GFUN generic function object.")

#+clos
(docfun si::gfun-name-set function (gfun name) "
ECL/CLOS specific.
Sets to NAME the name of the GFUN generic function object.")

#+clos
(docfun si::gfun-method-ht function (gfun) "
ECL/CLOS specific.
Returns the hashtable for caching methods associated with the GFUN
generic function object.")

#+clos
(docfun si::gfun-method-ht-set function (gfun hash-table) "
ECL/CLOS specific.
Sets to HASH-TABLE the hashtable for caching methods associated with the
GFUN generic function object.")

#+clos
(docfun si::gfun-spec-how-ref function (gfun index) "
ECL/CLOS specific.
Returns the INDEX-th element of specialization list associated  with the
GFUN generic function object. The first element has INDEX equal to zero.")

#+clos
(docfun si::gfun-spec-how-set function (gfun index specializer) "
ECL/CLOS specific.
Sets to SPECIALIZER the INDEX-th element of specialization list associated
with the GFUN generic function object. The first element has INDEX
equal to zero.")

#+clos
(docfun si::gfunp function (object) "
ECL/CLOS specific.
Returns T if OBJECT is of gfun type.")

(docfun go special (tag) "
Jumps to TAG.  See TAGBODY.")

(docfun graphic-char-p function (char) "
Returns T if CHAR is a printing character, i.e., a standard character other
than #\\Newline.  Returns NIL otherwise.")

(doctype hash-table "
A hash-table is a table used to map from objects to objects efficiently by the
hashing technique.  A hash-table is notated as
	#<hash-table n>
where N is actually a number that identifies the hash-table.")

(docfun hash-table-count function (hash-table) "
Returns the number of entries in HASH-TABLE.")

(docfun hash-table-p function (x) "
Returns T if X is a hash-table object; NIL otherwise.")

(docfun host-namestring function (filespec) "
Returns as a string the host part of the pathname specified by FILESPEC.
FILESPEC may be a symbol, a string, a pathname, or a file stream.")

(docfun identity function (x) "
Returns X.")

(docfun if special (test true-form &optional false-form) "
If TEST evaluates to non-NIL, then evaluates FORM1 and returns all values.
Otherwise, evaluates FORM2 (which defaults to NIL) and returns all values.")

(docfun si::ihs-fun function (n) "
ECL specific.
Returns the function value of the N-th entity in the invocation history stack.")

(docfun imagpart function (number) "
Returns the imagpart of NUMBER if it is a complex.  Otherwise, returns zero of
the same type as NUMBER.")

(docfun import function (symbol &optional (package *package*)) "
Registers SYMBOL to PACKAGE as an internal symbol.  Does nothing if SYMBOL is
already registered in PACKAGE.  SYMBOL may be a list of symbols.")

(docfun in-package function (package-name &key (nicknames nil) (use '(lisp))) "
Makes the package named PACKAGE-NAME as the current package.  If such a
package does not exist, then creates one by passing all args to MAKE-PACKAGE.
Otherwise, adds the specified nicknames and packages to the nickname list and
use list of the package.  NICKNAMES must be a list consisting of strings and
symbols.  USE must be a list consisting of package objects and package names
(either string or symbol).")

(docfun input-stream-p function (stream) "
Returns T if STREAM can handle input operations; NIL otherwise.")

#+clos
(docfun si::instancep function (object) "
ECL/CLOS specific.
Returns T if OBJECT is of instance type.")

#+clos
(docfun si::instance-ref function (instance index) "
ECL/CLOS specific.
Returns the value of the INDEX-th slot of INSTANCE. The first slot has
INDEX equal to zero.")

#+clos
(docfun si::instance-set function (instance index value) "
ECL/CLOS specific.
Sets to VALUE the value of INDEX-th slot of INSTANCE. The first slot has
INDEX equal to zero.")

#+clos
(docfun si::instance-class function (instance) "
ECL/CLOS specific.
Returns the class of which the given INSTANCE is an instance.")

#+clos
(docfun si::instance-class-set function (instance class) "
ECL/CLOS specific.
Makes INSTANCE an instance of CLASS class.")

(docfun int-char function (integer) "
Equivalent to CODE-CHAR.")

(doctype integer "
An integer object represents an integer in mathematical sense.  An integer may
be a fixnum, or else it is a bignum.  Normally, an integer is notated in radix
10 (see *PRINT-BASE* and *READ-BASE*) as
	[sign] {digit}+
where DIGIT is a decimal digit ('0', ..., '9') and SIGN is either '+' or '-'.
Also, the following syntax is used to notate the radix explicitly.
	# radix {r | R} [sign] {digit}+
where RADIX is one of '2', '3', ..., '36' and DIGIT is a digit in radix RADIX:
	Digits in radix 2 are '0' and '1'
	Digits in radix 8 are '0', ..., '7'
	Digits in radix 16 are '0', ..., '9', 'a', ..., 'f', and 'A', ..., 'F'
The following syntax is also available for radix 2, 8, 10, and 16.
	# {b | B} [sign] {digit}+
	# {o | O} [sign] {digit}+
		  [sign] {digit}+ .
	# {x | X} [sign] {digit}+")

(docfun integer-decode-float function (float) "
Returns, as three values, the integer interpretation of significand F, the
exponent E, and the sign S of FLOAT, such that
	FLOAT = S * F * B^E
where B = (float-radix FLOAT).  F is a non-negative integer, E is an integer,
and S is either 1 or -1.")

(docfun integer-length function (integer) "
Returns the number of \"significant bits\" in the representation of INTEGER.
With positive arg, returns one plus the position of the most significant bit
that is 'on'.  With negative arg other than -1, returns one plus the position
of the most significant bit that is 'off'.  For 0 and -1, returns 0.")

(docfun integerp function (x) "
Returns T if X is an integer; NIL otherwise.")

(docfun intern function (string &optional (package *package*)) "
Searches PACKAGE for a symbol whose print name is STRING.  If such a symbol is
found, returns the symbol and, as the second value, one of the keywords
:INTERNAL, :EXTERNAL, and :INHERITED.  Otherwise, creates and returns a new
symbol and, as the second value, returns NIL.")

(docvar internal-time-units-per-second constant "
Gives the time unit used by GET-INTERNAL-REAL-TIME and GET-INTERNAL-RUN-TIME.
1000 in ECL.")

(doctype keyword "
A keyword is a symbol in the keyword package.")

(docfun keywordp function (x) "
Returns T if X is a symbol that belongs to the KEYWORD package; NIL otherwise.")

(docfun labels special ((&rest functions) &body forms) "
Introduces local functions and evaluates BODY as a PROGN.  The scope of each
local function include the local function definitions.  Thus self- and mutual-
recursive local functions can be defined.  Doc-strings for local functions are
simply ignored.")

(docvar lambda-list-keywords constant "
List of all lambda-list keywords, including
	&optional	&rest		&key
	&allow-other-keys		&aux
	&whole		&environment	&body")

(docvar lambda-parameters-limit constant "
The upper bound of the number of parameters specified by a lambda list.
Ignore this number; there is no such upper bound in ECL.")

(docfun last function (list) "
Returns the last cons that constitute LIST.  Returns NIL if LIST is NIL.")

(docfun lcm function (integer &rest more-integers) "
Returns the least common multiple of the args.  Returns 0 if at least one of
the args is 0.")

(docfun ldiff function (list x) "
If X is a cons that constitutes LIST, then returns a new list consisting of
those elements of LIST that appear before X.  Otherwise, returns a copy of
LIST.")

(docvar least-negative-double-float constant "
Same as LEAST-NEGATIVE-LONG-FLOAT.")

(docvar least-negative-long-float constant "
The negative long-float with the smallest absolute value.")

(docvar least-negative-short-float constant "
The negative short-float with the smallest absolute value.")

(docvar least-negative-single-float constant "
Same as LEAST-NEGATIVE-LONG-FLOAT.")

(docvar least-positive-double-float constant "
Same as LEAST-POSITIVE-LONG-FLOAT.")

(docvar least-positive-long-float constant "
The smallest positive long-float.")

(docvar least-positive-short-float constant "
The smallest positive short-float.")

(docvar least-positive-single-float constant "
Same as LEAST-POSITIVE-LONG-FLOAT.")

(docfun length function (sequence) "
Returns the length of SEQUENCE.")

(docfun let special ((&rest bindings) &body body) "
Evaluates all INITs (which defaults to NIL), binds the value of each INIT to
the corresponding VAR, evaluates FORMs, and returns all values of the last
FORM.  Returns NIL if no FORM is given.")

(docfun let* special ((&rest bindings) &body body) "
Evaluates INIT (which defaults to NIL) and binds the value to the
corresponding VAR, one by one for each pair of VAR and INIT.  Then evaluates
FORMs and returns all values of the last FORM.  Returns NIL if no FORM is
given.")

(docfun list function (&rest args) "
Returns a list of the args.")

(docfun list* function (arg &rest more-args) "
With one arg, simply returns it.  With n args (n > 1), conses the first arg to
the LIST* of the rest of args.")

(docfun list-all-packages function () "
Returns a list of all packages.")

(docfun list-length function (list) "
Returns the length of LIST.  Returns NIL if LIST is circular.")

(docfun listen function (&optional (stream *standard-input*)) "
Returns T if STREAM is ready to input a character from; NIL otherwise.  In
some versions of ECL, this function does not work correctly because the
underlying OS does not support such a mechanism.")

(docfun listp function (x) "
Returns T if X is either a cons or NIL.  Otherwise, returns NIL.")

(docfun load function (filespec
       &key (verbose *load-verbose*) (print nil) (if-does-not-exist :error)) "
Loads the contents of the specified file into ECL.
If the filetype is not specified, ECL first tries to load the fasl file with
filetype \".fasl\", then tries to load the source file with filetype \".lsp\",
and then tries to load the source file with no filetype.
FILESPEC may be a symbol, a string, a pathname, or a file stream.  VERBOSE
specifies whether or not the loader prints a loading message.  PRINT specifies
whether or not the loader prints the values of the top-level forms.
IF-DOES-NOT-EXIST specifies the behavior of the loader when the specified file
is not found.  It may be :ERROR or NIL.
If the file was loaded successfully, returns the pathname of the file actually
loaded")

(docfun locally macro (&body forms) "
Gives DECLs locally while evaluating FORMs, and returns all values of the last
FORM.  Returns NIL if no FORM is given.")

(docfun log function (number1 &optional number2) "
With two args, returns the logarithm of NUMBER1 in base NUMBER2.  With one
arg, returns the natural logarithm of the arg.")

(docfun logand function (&rest integers) "
Returns the bit-wise AND of the args.")

(docfun logandc1 function (integer1 integer2) "
Equivalent to (LOGAND (LOGNOT INTEGER1) INTEGER2).")

(docfun logandc2 function (integer1 integer2) "
Equivalent to (LOGAND INTEGER1 (LOGNOT INTEGER2)).")

(docfun logbitp function (bit-position integer) "
Returns T if the specified bit of INTEGER is 1; NIL otherwise.  BIT-POSITION
must be a non-negative integer, with 0 representing the least significant bit.")

(docfun logcount function (integer) "
If INTEGER is negative, returns the number of 0 bits.  Otherwise, returns the
number of 1 bits.")

(docfun logeqv function (&rest integers) "
Returns the bit-wise EQUIVALENCE of the args.")

(docfun logior function (&rest integers) "
Returns the bit-wise INCLUSIVE OR of the args.")

(docfun lognand function (integer1 integer2) "
Equivalent to (LOGNOT (LOGAND INTEGER1 INTEGER2)).")

(docfun lognor function (integer1 integer2) "
Equivalent to (LOGNOT (LOGIOR INTEGER1 INTEGER2)).")

(docfun lognot function (integer) "
Returns the bit-wise logical NOT of the arg.")

(docfun logorc1 function (integer1 integer2) "
Equivalent to (LOGIOR (LOGNOT INTEGER1) INTEGER2).")

(docfun logorc2 function (integer1 integer2) "
Equivalent to (LOGIOR INTEGER1 (LOGNOT INTEGER2)).")

(docfun logxor function (&rest integers) "
Returns the bit-wise EXCLUSIVE OR of the args.")

(doctype long-float "
A long-float is a long-precision floating point number.")

(docfun lower-case-p function (char) "
Returns T if CHAR is a lower-case character; NIL otherwise.")

(docfun macro-function function (symbol) "
Returns the expansion function of the global macro named SYMBOL.  Returns NIL
if no such macro exists.  The expansion function receives a macro form and an
environment, and returns the expanded form.")

(docfun macroexpand function (form &optional (env nil)) "
If FORM is a macro form, then expands it repeatedly until the result is not a
macro any more, and returns the result as the first value and T as the second
value.  Otherwise, returns FORM and NIL as two values.")

(docfun macroexpand-1 function (form &optional (env nil)) "
If FORM is a macro form, then expands it once and returns the result as the
first value and T as the second value.  Otherwise, returns FORM and NIL as two
values.")

(docfun macrolet special ((&rest macros) &body forms)
"Syntax: (macrolet ({(name defmacro-lambda-list {decl | doc}* {form}*)}*)
          . body)
Introduces local macros and evaluates BODY as a PROGN.  See DEFMACRO for the
complete syntax of defmacro-lambda-list.  Doc-strings for local macros are
simply ignored.")

(docfun make-broadcast-stream function (&rest streams) "
Creates and returns a broadcast stream.  Outputs to this stream are output to
all STREAMs.  A broadcast stream is notated as
	#<broadcast stream n>
where N is a number that identify the stream.")

(docfun make-char function (char &optional (bits 0) (font 0)) "
Returns a character object with the same code as CHAR and with the specified
BITS and FONT attributes.  Returns NIL if no such character exists.")

(docfun make-concatenated-stream function (&rest streams) "
Creates and returns a concatenated stream.  Inputs from this stream are first
obtained from the first STREAM.  When the end of the first STREAM is reached,
then inputs are obtained from the second STREAM.  And so forth.
A concatenated stream is notated as
	#<concatenated stream n>
where N is a number that identifies the stream.")

(docfun make-dispatch-macro-character function (char &optional (non-terminating-p nil) (readtable *readtable*)) "
Register CHAR as a dispatch macro character in READTABLE.  NON-TERMINATING-P
specifies whether CHAR is non-terminating (see READTABLE).")

(docfun make-echo-stream function (stream1 stream2) "
Creates and returns an echo stream.  Inputs from this stream are obtained from
STREAM1 and outputs to this stream are output to STREAM2.  In addition, all
inputs from STREAM1 are output to STREAM2.
An echo stream is notated as
	#<echo stream n>
where N is a number that identifies the stream.")

(docfun make-hash-table function (&key (test 'eql) (size 1024) (rehash-size 1.5) (rehash-threshold 0.7)) "
Creates and returns a hash-table.
TEST specifies which predicate should be used to access hash-table entries.
It must be EQ, EQL, or EQUAL.  SIZE specifies the number of entries in the
hash-table.  REHASH-SIZE, if an integer, specifies how many entries should be
added when the hash-table becomes 'almost full'.  REHASH-SIZE, if a float,
specifies the ratio of the new size and the old size.  REHASH-THRESHOLD
specifies when to expand the hash-table.  If an integer, the hash-table is
expanded when REHASH-THRESHOLD / REHASH-SIZE entries have been used.  If a
float, the hash-table is expanded when REHASH-THRESHOLD times the whole
entries have been used.")

(docfun make-list function (length &key (initial-element nil)) "
Creates and returns a list of the specified LENGTH, whose elements are all the
value of INITIAL-ELEMENT.")

(docfun make-package function (package-name &key (nicknames nil) (use '(lisp))) "
Creates and returns a new package named PACKAGE-NAME.  PACKAGE-NAME must be a
string or a symbol.  The print name is used if PACKAGE-NAME is a symbol.
NICKNAMES gives the nicknames of the package.  It must be a list of strings
and symbols.  USE specifies the packages used by the created package.  It must
be a list of package objects, strings, and symbols.")

(docfun make-pathname function (&key (defaults (parse-namestring \"\"
                        (pathname-host *default-pathname-defaults*)))
            (host (pathname-host defaults))
            (device (pathname-device defaults))
            (directory (pathname-directory defaults))
            (name (pathname-name defaults))
            (type (pathname-type defaults))
            (version (pathname-version defaults))) "
Creates a pathname object with the slot values specified by HOST, DEVICE,
DIRECTORY, NAME, TYPE, and VERSION.")

(docfun make-random-state function (&optional (random-state nil)) "
Creates and returns a random-state object.  If RANDOM-STATE is NIL, copies the
value of *RANDOM-STATE*.  If RANDOM-STATE is a random-state, copies it.  If
RANDOM-STATE is T, creates a random-state randomly.")

(docfun make-string function (length &key (initial-element #\Space)) "
Creates and returns a new string of the given LENGTH, whose elements are all
INITIAL-ELEMENT.")

(docfun make-string-input-stream function (string &optional (start 0) (end (length string))) "
Creates and returns a string-input stream.  Inputs from this stream are
obtained form STRING.  A string-input stream is notated as
	#<string-input stream from s>
where S is a string.")

(docfun make-string-output-stream function () "
Creates and returns a string-output stream.  Outputs to this stream are
obtained as a string by GET-OUTPUT-STREAM-STRING.  A string-output stream
is notated as
	#<string-output stream n>
where N is a number that identifies the stream.")

(docfun si::make-string-output-stream-from-string function (string) "
ECL specific.
Creates and returns a string-output-stream to STRING.  STRING must have a
fill-pointer.")

(docfun make-symbol function (string) "
Creates and returns a new uninterned symbol whose print name is STRING.")

(docfun make-synonym-stream function (symbol) "
Creates and returns a synonym stream to SYMBOL.  Inputs from this stream are
obtained from, and outputs to this stream are sent to the stream that is the
value of the global variable named SYMBOL.  A synonym stream is notated as
	#<synonym stream to s>
where S is a symbol.")

(docfun make-two-way-stream function (stream1 stream2) "
Creates and returns a two-way stream.  Inputs from this stream are obtained
from STREAM1 and outputs to this stream are sent to STREAM2.  A two-way stream
is notated as
	#<two-way stream n>
where N is a number that identifies the stream.")

(docfun makunbound function (symbol) "
Makes the global variable named SYMBOL have no value.  Returns SYMBOL.")

(docfun mapc function (function list &rest more-lists) "
For each N (0 <= N < K), applies FUNCTION to the N-th elements of the given
LISTs, where K is the minimum length of the given LISTs.  Returns the first
LIST.")

(docfun mapcan function (function list &rest more-lists) "
For each N (0 <= N < K), applies FUNCTION to the N-th elements of the given
LISTs, where K is the minimum length of the given LISTs.  Nconcs the values,
one for each call to FUNCTION, and returns the result.")

(docfun mapcar function (function list &rest more-lists) "
Creates and returns a list of K elements, with the N-th element being the
value of applying FUNCTION to the N-th elements of the given LISTs, where K
is the minimum length of the given LISTs.")

(docfun mapcon function (function list &rest more-lists) "
For each N (0 <= N < K), applies FUNCTION to the N-th cdrs of the given LISTs,
where K is the minimum length of the given LISTs.  Nconcs the values, one for
each call to FUNCTION, and returns the result.")

(docfun maphash function (function hash-table) "
For each entry of HASH-TABLE, applies FUNCTION to the key and the value of the
entry.  Returns NIL.")

(docfun mapl function (function list &rest more-lists) "
For each N (0 <= N < K), applies FUNCTION to the N-th cdrs of the given LISTs,
where K is the minimum length of the given LISTs.  Returns the first LIST.")

(docfun maplist function (function list &rest more-lists) "
Creates and returns a list of K elements, with the N-th element being the
value of applying FUNCTION to the N-th cdrs of the given LISTs, where K is the
minimum length of the given LISTs.")

(docfun max function (number &rest more-numbers) "
Returns the largest arg.  The args must be non-complex numbers.")

(docfun maximum-allocatable-pages function (type) "
ECL specific.
Returns the current maximum number of pages for the type class of the ECL
implementation type TYPE.")

#-boehm-gc
(docfun si::maximum-contiguous-pages function () "
ECL specific.
Returns the current maximum number of pages for contiguous blocks.")

(docfun member function (item list &key (key '#'identity) (test '#'eql) test-not) "
Searches LIST for an element that is equal to ITEM in the sense of the TEST.
If found, returns the sublist of LIST that begins with the element.
Otherwise, returns NIL.")

(docfun merge-pathnames function (filespec
       &optional (defaults *default-pathname-defaults*) default-version) "
Fills in unspecified slots of the pathname specified by FILESPEC from the
pathname specified by DEFAULTS, and returns the result pathname.  DEFAULT-
VERSION is simply ignored in ECL.  FILESPEC and DEFAULTS may be a symbol, a
string, a pathname, or a file stream.")

(docfun min function (number &rest more-numbers) "
Returns the smallest arg.  The args must be non-complex numbers.")

(docfun minusp function (number) "
Returns T if NUMBER is negative; NIL otherwise.")

(docfun mod function (number divisor) "
Returns the second result of (FLOOR NUMBER DIVISOR), i.e. the value of
	(- NUMBER (* (FLOOR NUMBER DIVISOR) DIVISOR))")

(docvar most-negative-double-float constant "
Same as MOST-NEGATIVE-LONG-FLOAT.")

(docvar most-negative-fixnum constant "
The negative fixnum with the largest absolute value.  - 2^29 in ECL.")

(docvar most-negative-long-float constant "
The long-float with the largest absolute value.")

(docvar most-negative-short-float constant "
The short-float with the largest absolute value.")

(docvar most-negative-single-float constant "
Same as MOST-NEGATIVE-LONG-FLOAT.")

(docvar most-positive-double-float constant "
Same as MOST-POSITIVE-LONG-FLOAT.")

(docvar most-positive-fixnum constant "
The largest positive fixnum.  2^29 - 1 in ECL.")

(docvar most-positive-long-float constant "
The largest positive long-float.")

(docvar most-positive-short-float constant "
The largest positive short-float.")

(docvar most-positive-single-float constant "
Same as MOST-POSITIVE-LONG-FLOAT.")

(docfun multiple-value-call special (function-form &rest forms) "
Evaluates FUNCTION-FORM, whose value must be a function.  Then evaluates FORMs
and applies the function to all values of FORMs.  Unlike FUNCALL, all values
of each FORM are used as arguments.  Returns all values of the function.")

(docfun multiple-value-prog1 special (first-form &rest forms) "
Evaluates FIRST-FORM, saves all values it returns, and then evaluates FORMs.
Returns all the saved values of FIRST-FORM.")

(docvar multiple-values-limit constant "
The upper bound on the number of values that a function can return.  Actually,
however, there is no such upper bound in ECL.")

(docfun name-char function (name) "
Given an argument acceptable to string,
Returns a character object with the specified character name (see CHARACTER).
Returns NIL if no such character object exists.  NAME is typically a string
but may be any object that can be coerced to string.")

(docfun namestring function (filespec) "
Returns as a string all slots of the pathname specified by FILESPEC.  FILESPEC
may be a symbol, a string, a pathname, or a file stream.")

(docfun nbutlast function (list &optional (n 1)) "
Destructive BUTLAST.  LIST may be destroyed.")

(docfun nconc function (&rest lists) "
Destructive APPEND.  The args except for the last may be destroyed.")

(doctype nil "
The type NIL is a subtype of every type.  No object belongs to this type.")

(docvar nil constant "
The value of NIL is NIL.")

(docfun ninth function (x) "
Equivalent to (CAR (CDDDDR (CDDDDR X))).")

(docfun not function (x) "
Returns T if X is NIL; NIL otherwise.")

(docfun nreconc function (x y) "
Equivalent to (NCONC (NREVERSE X) Y).")

(docfun nreverse function (sequence) "
Destructive REVERSE.  The arg may be destroyed.")

(docfun nstring-capitalize function (string &key (start 0) (end (length string))) "
Destructive STRING-CAPITALIZE.  STRING may be destroyed.")

(docfun nstring-downcase function (string &key (start 0) (end (length string))) "
Destructive STRING-DOWNCASE.  STRING may be destroyed.")

(docfun nstring-upcase function (string &key (start 0) (end (length string))) "
Destructive STRING-UPCASE.  STRING may be destroyed.")

(docfun nsublis function (alist tree &key (key '#'identity) (test '#'eql) test-not) "
Destructive SUBLIS.  TREE may be destroyed.")

(docfun nsubst function (new old tree &key (key '#'identity) (test '#'eql) test-not) "
Destructive SUBST.  TREE may be destroyed.")

(docfun nsubstitute function (new old sequence
       &key (key '#'identity) (test '#'eql) test-not
            (start 0) (end (length sequence))
            (count most-positive-fixnum) (from-end nil)) "
Destructive SUBSTITUTE.  SEQUENCE may be destroyed.")

(docfun nsubstitute-if function (new test sequence
       &key (key '#'identity) (start 0) (end (length sequence))
            (count most-positive-fixnum) (from-end nil)) "
Destructive SUBSTITUTE-IF.  SEQUENCE may be destroyed.")

(docfun nsubstitute-if-not function (new test sequence
       &key (key '#'identity) (start 0) (end (length sequence))
            (count most-positive-fixnum) (from-end nil)) "
Destructive SUBSTITUTE-IF-NOT.  SEQUENCE may be destroyed.")

(docfun nth function (n list) "
Returns the N-th element of LIST, the first element of LIST being the zeroth.
Returns NIL if the length of LIST is less than N.  N must be a non-negative
integer.")

(docfun nthcdr function (n list) "
Returns the N-th cdr of LIST.  N must be a non-negative integer.")

(docfun null function (x) "
Returns T if X is NIL; NIL otherwise.")

(docfun si:null-pointer-p function (ptr) "
Return true if PTR is a null pointer.")

(doctype number "
A number is an integer, a ratio, a float, or a complex number.  Integers and
ratios are collectively called rationals.")

(docfun numberp function (x) "
Returns T if X is a number; NIL otherwise.")

(docfun numerator function (rational) "
Returns the numerator of RATIONAL as an integer, if RATIONAL is a ratio.
Returns RATIONAL if it is an integer.")

(docfun oddp function (integer) "
Returns T if INTEGER is an odd number; NIL otherwise.")

(docfun open function (filespec &key (direction :input) element-type
                     if-exists if-does-not-exist) "
Opens the specified file and returns a file stream to/from the file.  FILESPEC
may be a symbol, a string, a pathname, or a file stream.  DIRECTION may be
:INPUT, :OUTPUT, :IO, or :PROBE.  ELEMENT-TYPE is simply ignored in ECL.  IF-
EXISTS specifies what to do when DIRECTION is either :OUTPUT or :IO and the
specified file exists already.  It may be :ERROR (the default), :NEW-VERSION,
:RENAME, :RENAME-AND-DELETE, :OVERWRITE, :APPEND, :SUPERSEDE, or NIL.  IF-
DOES-NOT-EXIST specifies what to do when the specified file does not exists.
It may be :ERROR (the default when DIRECTION is :INPUT), :CREATE (the default
when DIRECTION is either :OUTPUT or :IO), or NIL.
File streams are notated in one of the following ways:
	#<input stream f>
	#<output stream f>
	#<io stream f>
	#<probe stream f>
where F is the file name.")

(docfun ext:make-pipe function ()
"Creates a pipe in the form of a two-way stream that can be used for
interprocess and interthread communication.")

(docfun or macro (&rest forms) "
Evaluates FORMs in order from left to right.  If any FORM evaluates to non-
NIL, quits and returns that (single) value.  If the last FORM is reached,
returns whatever values it returns.")

(docfun output-stream-p function (stream) "
Returns T if STREAM can handle output operations; NIL otherwise.")

(doctype package "
A package object serves as a name space of symbols.  A package is notated as
#<s package> where S is actually the name of the package.  ECL provides five
built-in packages:
	lisp	 standard symbols of Common Lisp.
	user	 the package that the user uses by default.
	keyword	 keyword symbols.
	system	 system internal symbols.  Has nicknames SYS and SI.
	compiler system internal symbols for the ECL compiler.")

(docfun package-name function (package) "
Returns the name of PACKAGE as a string.")

(docfun package-nicknames function (package) "
Returns the nicknames of PACKAGE as a list of strings.")

(docfun package-shadowing-symbols function (package) "
Returns, as a list, those symbols in PACKAGE that are shadowing symbols in
other packages.")

(docfun package-use-list function (package) "
Returns, as a list, those packages that PACKAGE uses.")

(docfun package-used-by-list function (package) "
Returns, as a list, those packages that use PACKAGE.")

(docfun packagep function (x) "
Returns T if X is a package object; NIL otherwise.")

(docfun pairlis function (keys items &optional (alist nil)) "
Conses each KEY and the corresponding ITEM, adds them to ALIST, and returns
the result.  KEYS and ITEMS must be of the same length.")

(docfun parse-integer function (string
       &key (start 0) (end (length string)) (radix 10) (junk-allowed nil)) "
Parses STRING for an integer and returns it.  As the second value, returns the
index to the character next to the last character that is parsed.  If JUNK-
ALLOWED is non-NIL, ignores white spaces before and after the number
representation in STRING and returns NIL even if STRING is not parsed
successfully.")

(docfun parse-namestring function (string &optional host defaults &key (start 0) end (junk-allowed nil)) "
Parses STRING and returns a pathname.  As the second value, returns the index
to the character next to the last character that has been parsed.  STRING is
usually a string object but it may be a symbol, a pathname, or a file stream.
START and END are meaningful only when STRING is a string or a symbol.  They
default to 0 and (length (string FILESPEC)) respectively.  When the parsing is
failed, signals an error (if JUNK-ALLOWED is NIL) or simply returns NIL.  HOST
and DEFAULTS are simply ignored in ECL.")

(doctype pathname "
A pathname object identifies an external file or a collection of external
files.  A pathname object consists of six slots, HOST, DEVICE, DIRECTORY,
NAME, and TYPE.  HOST, DEVICE, and VERSION slots are meaningless in ECL,
though they are harmless at all.
A pathname is notated as #\\\"...\", where '...' is actually some information
on the pathname.  This depends on the version of ECL.  Refer to the ECL Report
for details.")

(docfun pathname function (filespec) "
Returns a pathname specified by FILESPEC.  FILESPEC may be a symbol, a string,
a pathname, or a file stream.")

(docfun pathname-device function (filespec) "
Returns the device slot of the pathname specified by FILESPEC.  FILESPEC may
be a symbol, a string, a pathname, or a file stream.")

(docfun pathname-directory function (filespec) "
Returns the directory slot of the pathname specified by FILESPEC.  FILESPEC
may be a symbol, a string, a pathname, or a file stream.")

(docfun pathname-host function (filespec) "
Returns the host slot of the pathname specified by FILESPEC.  FILESPEC may be
a symbol, a string, a pathname, or a file stream.")

(docfun pathname-name function (filespec) "
Returns the name slot of the pathname specified by FILESPEC.  FILESPEC may be
a symbol, a string, a pathname, or a file stream.")

(docfun pathname-type function (filespec) "
Returns the type slot of the pathname specified by FILESPEC.  FILESPEC may be
a symbol, a string, a pathname, or a file stream.")

(docfun pathname-version function (filespec) "
Returns the version slot of the pathname specified by FILESPEC.  FILESPEC may
be a symbol, a string, a pathname, or a file stream.")

(docfun pathnamep function (x) "
Returns T if X is a pathname object; NIL otherwise.")

(docfun peek-char function (&optional (char-spec nil) (stream *standard-input*)
                 (eof-error-p t) (eof-value nil) (recursive-p nil)) "
Reads characters from STREAM until the specified character is read.  Returns
the last character but leaves it in STREAM.  CHAR-SPEC may be a character
object, T (specifies non-whitespace characters), or NIL (specifies all
characters).")

(docvar pi constant "
The float that is approximately equal to the ratio of the circumference of the
circle to the diameter.")

(docfun si::pointer function (object) "
ECL specific.
Returns the address of the OBJECT as a fixnum.")

(docfun plusp function (number) "
Returns T if NUMBER is positive; NIL otherwise.")

(docfun position function (item sequence
       &key (key '#'identity) (test '#'eql) test-not
            (start 0) (end (length sequence)) (from-end nil)) "
Returns the index to the first element in SEQUENCE that is equal to ITEM in
the sense of TEST.  Returns NIL if no such element exists.")

(docfun position-if function (test sequence
       &key (key '#'identity) (start 0) (end (length sequence)) (from-end nil)) "
Returns the index to the first element in SEQUENCE that satisfies TEST.
Returns NIL if no such element exists.")

(docfun position-if-not function (test sequence
       &key (key '#'identity) (start 0) (end (length sequence)) (from-end nil)) "
Returns the index to the first element in SEQUENCE that does not satisfy TEST.
Returns NIL if no such element exists.")

(docfun pprint function (object &optional (stream *standard-output*)) "
Pretty-prints OBJECT.  Returns no values.  Equivalent to
	(PROGN (WRITE OBJECT :STREAM STREAM :PRETTY T :ESCAPE T)
	       (VALUES))
The SI::PRETTY-PRINT-FORMAT property N (which must be a non-negative integer)
of a symbol SYMBOL controls the pretty-printing of form
	(SYMBOL f1 ... fN fN+1 ... fM)
in such a way that the subforms fN+1, ..., fM are regarded as the 'body' of
the entire form.  For instance, the property value of 2 is initially given to
the symbol DO.")

(docfun prin1 function (object &optional (stream *standard-output*)) "
Prints OBJECT in the way that the output can be reread later if possible.
Returns OBJECT.  Equivalent to (WRITE OBJECT :STREAM STREAM :ESCAPE T).")

(docfun princ function (object &optional (stream *standard-output*)) "
Prints OBJECT without escape characters.  Returns OBJECT.  Equivalent to
(WRITE OBJECT :STREAM STREAM :ESCAPE NIL).")

(docfun print function (object &optional (stream *standard-output*)) "
Outputs a newline character, and then PRIN1s OBJECT.  Returns OBJECT.
Equivalent to
	(PROGN (TERPRI STREAM)
	       (WRITE OBJECT :STREAM STREAM :ESCAPE T))")

(docfun probe-file function (filespec) "
Returns the full pathname of the specified file if it exists.  Returns NIL
otherwise.  FILESPEC may be a symbol, a string, a pathname, or a file stream.")

(docfun progn special (&body forms) "
Evaluates FORMs in order, and returns all values of the last FORM.  Returns
NIL if no FORM is given.")

(docfun progv special (symbols-form values-form &body forms) "
Evaluates SYMBOLS-FORM and VALUES-FORM.  The value of SYMBOLS-FORM must be a
list of symbols (S1 ... Sn) and the value of VALUES-FORM must be a list
(V1 ... Vm).  Binds each Si to Vi or to NIL if i > m.  Then evaluates FORMs
and returns all values of the last FORM.  Returns NIL if no FORM is given.")

(docfun quote special (x) "
Simply returns X without evaluating it.")

(docfun random function (number &optional (random-state *random-state*)) "
Creates and returns a random number by using RANDOM-STATE.  NUMBER must be
either a positive integer or a positive float.  If NUMBER is a positive
integer, returns a positive integer less than NUMBER.  If NUMBER is a positive
float, returns a positive float less than NUMBER in the same float format as
NUMBER.")

(doctype random-state "
A random-state object stores information used to generate random numbers.  A
random-state is notated as '#$' followed by a certain number.")

(docfun random-state-p function (x) "
Returns T if X is a random-state object; NIL otherwise.")

(docfun rassoc function (item alist &key (test '#'eql) test-not (key '#'identity)) "
Returns the first pair in ALIST whose cdr is equal (in the sense of TEST) to
ITEM.  Returns NIL if no such pair exists.
The function KEY is applied to extract the key for comparison.")

(doctype ratio "
A ratio is notated by its numerator and denominator, separated by a slash '/'.
Normally, a ratio is notated in radix 10 (see *PRINT-BASE* and *READ-BASE*) as
	[sign] {digit}+ / {digit}+
where DIGIT is a decimal digit ('0', ..., '9') and SIGN is either '+' or '-'.
Also, the following syntax is used to notate the radix explicitly.
	# radix {r | R} [sign] {digit}+ / {digit}+
where RADIX is one of '2', '3', ..., '36' and DIGIT is a digit in radix RADIX:
	Digits in radix 2 are '0' and '1'
	Digits in radix 8 are '0', ..., '7'
	Digits in radix 16 are '0', ..., '9', 'a', ..., 'f', and 'A', ..., 'F'
The following syntax is also available for radix 2, 8, 10, and 16.
	# {b | B} [sign] {digit}+ / {digit}+
	# {o | O} [sign] {digit}+ / {digit}+
	# {x | X} [sign] {digit}+ / {digit}+")

(docfun rational function (real) "
Converts REAL into rational accurately and returns the result.")

(docfun rationalize function (real) "
Converts REAL into rational approximately and returns the result.")

(doctype rational "
A ratio is either an integer or a ratio.")

(docfun rationalp function (x) "
Returns T if X is an integer or a ratio; NIL otherwise.")

(docfun read function (&optional (stream *standard-input*)
                 (eof-error-p t) (eof-value nil) (recursivep nil)) "
Reads an object from STREAM and returns the object.")

(docfun read-byte function (stream &optional (eof-error-p t) (eof-value nil)) "
Reads one byte from STREAM and returns it as an integer.")

(docfun read-char function (&optional (stream *standard-input*)
                 (eof-error-p t) (eof-value nil) (recursive-p nil)) "
Reads a character from STREAM and returns it.")

(docfun read-char-no-hang function (&optional (stream *standard-input*)
                 (eof-error-p t) (eof-value nil) (recursive-p nil)) "
Returns the next character from STREAM if one is available; NIL otherwise.")

(docfun read-delimited-list function (char &optional (stream *standard-input*) (recursive-p nil)) "
Reads objects from STREAM until the next character after an object's
representation is CHAR.  Returns all objects read, as a list.")

(docfun read-line function (&optional (stream *standard-input*)
                 (eof-error-p t) (eof-value nil) (recursive-p nil)) "
Reads a line of characters from STREAM and returns them as a string.  The
newline character at the end of the line will be discarded.")

(docfun read-preserving-whitespace function (&optional (stream *standard-input*)
                 (eof-error-p t) (eof-value nil) (recursive-p nil)) "
Reads an object from STREAM and returns the object.  Unlike READ, always
leaves the character next to the object's representation.")

(doctype readtable "
A readtable defines the syntax used to read objects.
Each readtable object remembers the syntactic class of each character.  The
following syntactic classes are supported.  The characters in parenthesis
below are those standard characters that belong to each syntactic class as
defined in the standard readtable.
	white-space (space and newline)
	single-escape ( \\ )
	multiple-escape ( | )
	macro-character ( \"  #  '  (  )  ,  ;  ` )
	constituent (the others)
For each macro-character, the readtable remembers the definition of the
associated read macro and the non-terminating-p flag.  In the standard
readtable, only single-quote is non-terminating.  Dispatch macro characters
are classified to macro-characters.  A readtable is notated as
	#<readtable n>
where N is actually a number that identifies the readtable.")

(docfun readtablep function (x) "
Returns T if X is a readtable object; NIL otherwise.")

(docfun realpart function (number) "
Returns the realpart of NUMBER if it is a complex.  Otherwise, returns NUMBER.")

(docfun reduce function (function sequence
       &key (from-end nil) (start 0) (end (length sequence)) initial-value) "
Combines all the elements of SEQUENCE using the binary operation FUNCTION.")

(docfun rem function (number divisor) "
Returns the second value of (TRUNCATE NUMBER DIVISOR), i.e. the value of
	(- NUMBER (* (TRUNCATE NUMBER DIVISOR) DIVISOR))")

(docfun remhash function (key hash-table) "
Removes the entry for KEY in HASH-TABLE.  Returns T if such an entry existed;
NIL otherwise.")

(docfun remove function (item sequence
       &key (key '#'identity) (test '#'eql) test-not
            (start 0) (end (length sequence))
            (count most-positive-fixnum) (from-end nil)) "
Returns a copy of SEQUENCE with those elements equal to ITEM (in the sense of
TEST) removed.")

(docfun remove-if function (test sequence
       &key (key '#'identity) (start 0) (end (length sequence))
            (count most-positive-fixnum) (from-end nil)) "
Returns a copy of SEQUENCE with elements satisfying TEST removed.")

(docfun remove-if-not function (test sequence
       &key (key '#'identity) (start 0) (end (length sequence))
            (count most-positive-fixnum) (from-end nil)) "
Returns a copy of SEQUENCE with elements not satisfying TEST removed.")

(docfun remprop function (symbol indicator) "
Removes the specified property from the property list associated with SYMBOL.
Returns T if the property list had the specified property; NIL otherwise.")

(docfun rename-file function (filespec new-filespec &key (if-exists :error)) "
Renames the file specified by FILESPEC as specified by NEW-FILESPEC.  Returns
as three values the new pathname, the old full pathname, and the new full
pathname.  FILESPEC and NEW-FILESPEC may be a symbol, a string, a pathname, or
a file stream.

:IF-EXISTS is an ECL-specific extension that modifies the behavior of rename-file
if new-filespec already exists. It may be :ERROR (the default), NIL, :SUPERSEDE,
or T.")

(docfun rename-package function (package new-name &optional (new-nicknames nil)) "
Renames PACKAGE to NEW-NAME and replaces the nicknames with NEW-NICKNAMES.
See MAKE-PACKAGE.")

(docfun replace function (sequence1 sequence2
       &key (start1 0) (end1 (length sequence1))
            (start2 0) (end2 (length sequence2))) "
Replaces elements of SEQUENCE1 with the corresponding elements of SEQUENCE2.
SEQUENCE1 may be destroyed and is returned.")

#-boehm-gc
(docfun si::reset-gc-count function () "
ECL specific.
Resets the counter of the garbage collector that records how many times the
garbage collector has been called for each implementation type.")

(docfun rest function (x) "
Equivalent to CDR.")

(docfun return macro (&optional result) "
Terminates execution of the lexically surrounding NIL block and returns all
values of RESULT (which defaults to NIL) as the values of the terminated
block.")

(docfun return-from special (symbol &optional result) "
Terminates execution of the lexically surrounding block named SYMBOL and
returns all values of RESULT (which defaults to NIL) as the values of the
terminated block.")

(docfun revappend function (x y) "
Equivalent to (APPEND (REVERSE X) Y)")

(docfun reverse function (sequence) "
Returns a new sequence containing the same elements as SEQUENCE but in the
reverse order.")

(docfun round function (number &optional (divisor 1)) "
Returns the integer nearest to NUMBER/DIVISOR.  Returns the value of (- NUMBER
(* first-value DIVISOR)) as the second value.")

(docfun rplaca function (cons x) "
Replaces the car of CONS with X, and returns the modified CONS.")

(docfun rplacd function (cons x) "
Replaces the cdr of CONS with X, and returns the modified CONS.")

(docfun save function (filespec) "
ECL specific.
Saves the current ECL core image into a program file specified by PATHNAME.
FILESPEC may be a symbol, a string, a pathname, or a file stream.  This
function depends on the version of ECL.  See ECL Report for details.")

(docfun system function (command) "
ECL specific.
Executes a Shell command as if the string COMMAND is an input to the Shell.  
On return from the Shell command, it returns the exit code
of the command as an integer.")

(docfun scale-float function (float integer) "
Returns the value of (* FLOAT (expt (float-radix FLOAT) INTEGER)).")

(docfun schar function (simple-string n) "
Returns the character object representing the N-th character in SIMPLE-STRING.
This is faster than CHAR.")

(docfun second function (x) "
Equivalent to CADR.")

(docfun set function (symbol object) "
Assigns OBJECT to the global variable named SYMBOL.  Returns OBJECT.")

(docfun set-char-bit function (char bit-name flag) "
Returns a character with the same code and attributes as CHAR except the
bit specified by BIT-NAME is on (if FLAG is non-NIL) or off. In ECL, the
bit-attributes handled are :control :meta :super and :hyper")


(docfun set-dispatch-macro-character function (char subchar function &optional (readtable *readtable*)) "
Replaces FUNCTION for the read macro of SUBCHAR associated with the dispatch
macro character CHAR in READTABLE.  When the ECL reader reads an object that
begins with CHAR followed by SUBCHAR, it calls FUNCTION with the input stream,
SUBCHAR, and NIL as arguments.  When the ECL reader reads an object that
begins with CHAR, followed by a decimal representation of a number N, followed
by SUB-CHAR, it calls FUNCTION with N as the third argument.  In both cases,
if FUNCTION returns a single value, then that value is returned as the value
of the reader.  If FUNCTION returns no value, then the reader tries to read an
object again.  See MAKE-DISPATCH-MACRO-CHARACTER and GET-DISPATCH-MACRO-
CHARACTER.")

(docfun si::set-hole-size function (fixnum) "
ECL specific.
Sets the size of the memory hole (in pages).")

(docfun set-macro-character function (char function
       &optional (non-terminating-p nil) (readtable *readtable*)) "
Registers CHAR as a macro character in READTABLE and makes FUNCTION the read
macro associated with CHAR.  When the ECL reader reads an object that begins
with CHAR, it calls FUNCTION with the input stream and CHAR as arguments.  If
FUNCTION returns a single value, it is returned as the value of the reader.
If FUNCTION returns no value, then the reader tries to read an object again.
NON-TERMINATING-P specifies whether CHAR is non-terminating or not (see
READTABLE).
Use GET-MACRO-CHARACTER to get the read macro associated with a character.")

(docfun set-syntax-from-char function (to-char from-char
       &optional (to-readtable *readtable*) (from-readtable nil)) "
Replaces the information for TO-CHAR in TO-READTABLE with the information for
FROM-CHAR in FROM-READTABLE.  If FROM-READTABLE is NIL, then the standard
readtable is used.  TO-CHAR belongs to the same syntactic class as FROM-CHAR,
and if FROM-CHAR is a macro character, TO-CHAR inherits the read macro and
non-terminating-p flag of FROM-CHAR.  See READTABLE.")

(docfun setq special (&rest var-form-pairs) "
Syntax: (setq {var form}*)

Evaluates each FORM and assigns the value to VAR in order.  Returns the value
of the last FORM.")

(docfun seventh function (x) "
Equivalent to (CADDR (CDDDDR X)).")

(docfun shadow function (symbol &optional (package *package*)) "
If no symbol is registered in PACKAGE with the same name as SYMBOL, then
creates an internal symbol with the same name and registers it into PACKAGE.
The created symbol shadows external symbols of the same name in those packages
that PACKAGE uses.  SYMBOL may be a list of symbols.")

(docfun shadowing-import function (symbol &optional (package *package*)) "
Registers SYMBOL as an internal symbol of PACKAGE.  Does nothing if SYMBOL is
already registered in PACKAGE.  If there exists already a symbol in PACKAGE
with the same name, then uninterns the symbol first.  SYMBOL shadows external
symbols of the same name in those packages that PACKAGE uses.  SYMBOL may be a
list of symbols.")

(doctype short-float "
A short-float is a short-precision floating point number.")

(doctype simple-array "
A simple-array is an array that is not displaced to another array, has no
fill-pointer, and is not adjustable.")

(docfun simple-bit-vector-p function (x) "
Returns T if X is a simple-bit-vector; NIL otherwise.")

(docfun simple-string-p function (x) "
Returns T if X is a simple-string; NIL otherwise.")

(docfun simple-vector-p function (x) "
Returns T if X is a simple-vector; NIL otherwise.")

(docfun sin function (radians) "
Returns the sine of RADIANS.")

(doctype single-float "
A single-float is a single-precision floating point number.
SINGLE-FLOAT as a type specifier is equivalent to LONG-FLOAT in ECL.")

(docfun sinh function (number) "
Returns the hyperbolic sine of NUMBER.")

(docfun sixth function (x) "
Equivalent to (CADR (CDDDDR X)).")

(docfun sleep function (n) "
Suspends execution for N seconds.  N may be any non-negative, non-complex
number.")

#+clos
(docfun si::sl-boundp function (object) "
ECL/CLOS specific.
Returns nil if the OBJECT is not null.")

#+clos
(docfun si::sl-makunbound function (instance index) "
ECL/CLOS specific.
Removes the value associated with the INDEX-th slot of INSTANCE.")

(docfun special-operator-p function (symbol) "
Returns T if SYMBOL names a special form; NIL otherwise.
The special forms defined in Common Lisp are:
	block		if			progv
	catch		labels			quote
	compiler-let	let			return-from
	declare		let*			setq
	eval-when	macrolet		tagbody
	flet		multiple-value-call	the
	function	multiple-value-prog1	throw
	go		progn			unwind-protect
In addition, ECL implements the following macros as special forms, though of
course macro-expanding functions such as MACROEXPAND work correctly for these
macros.
	and		incf			prog1
	case		locally			prog2
	cond		loop			psetq
	decf		multiple-value-bind	push
	defmacro	multiple-value-list	return
	defun		multiple-value-set	setf
	do		or			unless
	do*		pop			when
	dolist		prog
	dotimes		prog*")

(docfun si::specialp function (symbol) "
ECL specific.
Returns T if the SYMBOL names a globally special variable; NIL otherwise.")

(docfun sqrt function (number) "
Returns the square root of the arg.")

(doctype standard-char "
A standard-char is a space character (#\\Space), a newline character
(#\\Newline,) or a character that represents one of the following letters.
	!  \"  #  $  %  &  '  (  )  *  +  ,  -  .  /  0  1  2  3  4
	5  6  7  8  9  :  ;  <  =  >  ?  @  A  B  C  D  E  F  G  H
	I  J  K  L  M  N  O  P  Q  R  S  T  U  V  W  X  Y  Z  [  \\
	]  ^  _  `  a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p
	q  r  s  t  u  v  w  x  y  z  {  |  }  ~~")

(docfun standard-char-p function (char) "
Returns T if CHAR is a standard-char; NIL otherwise.")

(doctype stream "
A stream is a source of input or a destination of output.  The following kinds
of streams are supported.
	file streams
	string-input streams
	string-output streams
	two-way streams
	echo streams
	synonym streams
	concatenated streams
	broadcast streams
Basically, file streams are created by OPEN and other kinds of streams are
created by MAKE-...-STREAM.  See these functions.")

(docfun stream-element-type function (stream) "
Returns the type specifier for the io unit of STREAM.")

(docfun streamp function (x) "
Returns T if X is a stream object; NIL otherwise.")

;;; CLOS Streams ------------------------------------------------------------

#+CLOS
(docfun stream-read-char method ((obj stream-class)) "
Reads the next character object from the CLOS stream OBJ.")

#+CLOS
(docfun stream-read-line method ((obj stream-class) &rest make-array-options) "
Reads character objects from the CLOS stream OBJ, up to and including the
next newline character, and returns them as a string (without the newline).
If given, the MAKE-ARRAY-OPTIONS arguments are passed to make-array
when the returned string is created.")

#+CLOS
(docfun stream-unread-char method ((obj stream-class) character) "
Unreads the character object.
CHARACTER will be the next character read by STREAM-READ-CHAR .")

#+CLOS
(docfun stream-peek-char method ((obj stream-class) peek-type) "
Returns the character object which would be returned by STREAM-READ-CHAR
but does not remove it from the input buffer.
If PEEK-TYPE is T, stream-peek-char skips over any whitespace characters,
removing them from the input buffer, and returns the next character.")

#+CLOS
(docfun stream-listen method ((obj stream-class)) "
Returns NIL if no character is immediately available from the CLOS stream.
Otherwise, the next character is returned, as if stream-peek-char
had been called.")

#+CLOS
(docfun stream-clear-input method ((obj stream-class)) "
Clears any buffered characters received from the CLOS stream OBJ.
Returns NIL.")

#+CLOS
(docfun stream-write-char method ((obj stream-class) character) "
Outputs the CHARACTER to the CLOS stream OBJ and returns the CHARACTER.")

#+CLOS
(docfun stream-write-string method ((obj stream-class) string &optional start end) "
Outputs characters in the STRING to the CLOS stream OBJ and returns the
STRING. The START and END arguments, if given, indicate a substring that
is to be output.")

#+CLOS
(docfun stream-fresh-line method ((obj stream-class)) "
Outputs a newline to the CLOS stream if and only if the CLOS stream OBJ
is not already at the beginning of a new line. Returns non-NIL if a
newline was output and NIL otherwise.")

#+CLOS
(docfun stream-clear-output method ((obj stream-class)) "
Aborts any outstanding output operation on the CLOS stream OBJ
and returns NIL .")

#+CLOS
(docfun stream-force-output method ((obj stream-class)) "
Initiates the emptying of internal buffers on the CLOS stream OBJ
and returns NIL.")

;;; end of CLOS streams ---------------------------------------------------

(docfun string function (x) "
Coerces X into a string.  If X is a string, then returns X itself.  If X is a
symbol, then returns its print name.  If X is a character, then returns a one
element string containing that character.  Signals an error if X cannot be
coerced into a string.")

(docfun string-capitalize function (string &key (start 0) (end (length string))) "
Returns a copy of STRING with the first character of each word converted to
upper case, and remaining characters converted to lower case.  Its destructive
version is NSTRING-CAPITALIZE.")

(doctype string-char "
A string-char is a character that can be stored in strings.  In ECL, every
character is a string-character.")

(docfun string-char-p function (char) "
Returns T if CHAR is a string-char, i.e. can be stored in strings; NIL
otherwise.  In ECL, this function always returns T.")

(docfun si::string-concatenate function (&rest strings) "
ECL specific.
Concatenates STRINGs and returns the result.")

(docfun string-downcase function (string &key (start 0) (end (length string))) "
Returns a copy of STRING with all upper case characters converted to lower
case.  Its destructive version is NSTRING-DOWNCASE.")

(docfun string-equal function (string1 string2
       &key (start1 0) (end1 (length string1))
            (start2 0) (end2 (length string2))) "
Returns T if STRING1 and STRING2 are character-wise CHAR-EQUAL; NIL otherwise.")

(docfun string-greaterp function (string1 string2
       &key (start1 0) (end1 (length string1))
            (start2 0) (end2 (length string2))) "
Similar to STRING>, but ignores cases.")

(docfun string-left-trim function (char-bag string) "
Returns a copy of STRING with the specified characters removed from the left
end.  CHAR-SPEC must be a sequence of characters.")

(docfun string-lessp function (string1 string2
       &key (start1 0) (end1 (length string1))
            (start2 0) (end2 (length string2))) "
Similar to STRING<, but ignores cases.")

(docfun string-not-equal function (string1 string2
       &key (start1 0) (end1 (length string1))
            (start2 0) (end2 (length string2))) "
Returns NIL if the strings are character-wise CHAR-EQUAL.  Otherwise, returns
the number of characters in the longest common prefix of the strings.")

(docfun string-not-greaterp function (string1 string2
       &key (start1 0) (end1 (length string1))
            (start2 0) (end2 (length string2))) "
Similar to STRING<=, but ignores cases.")

(docfun string-not-lessp function (string1 string2
       &key (start1 0) (end1 (length string1))
            (start2 0) (end2 (length string2))) "
Similar to STRING>=, but ignores cases.")

(docfun string-right-trim function (char-bag string) "
Returns a copy of STRING with the specified characters removed from the right
end.  CHAR-SPEC must be a sequence of characters.")

(docfun si::string-to-object function (string) "
ECL specific.
Equivalent to (READ-FROM-STRING STRING), but is much faster.")

(docfun string-trim function (char-spec string) "
Returns a copy of STRING with the specified characters removed from both ends.
CHAR-SPEC must be a sequence of characters.")

(docfun string-upcase function (string &key (start 0) (end (length string))) "
Returns a copy of STRING with all lower case characters converted to upper
cases.  Its destructive version is NSTRING-UPCASE.")

(docfun string/= function (string1 string2
       &key (start1 0) (end1 (length string1))
            (start2 0) (end2 (length string2))) "
Returns NIL if the strings are character-wise CHAR=.  Otherwise, returns the
number of characters in the longest common prefix of the strings.")

(docfun string< function (string1 string2
       &key (start1 0) (end1 (length string1))
            (start2 0) (end2 (length string2))) "
If STRING1 comes before STRING2 in lexicographic order, then returns the
number of characters in the longest common prefix of the strings.  Otherwise,
returns NIL.")

(docfun string<= function (string1 string2
       &key (start1 0) (end1 (length string1))
            (start2 0) (end2 (length string2))) "
If STRING1 comes before STRING2 in lexicographic order or if the strings are
character-wise CHAR=, then returns the number of characters in the longest
common prefix of the strings.  Otherwise, returns NIL.")

(docfun string= function (string1 string2
       &key (start1 0) (end1 (length string1))
            (start2 0) (end2 (length string2))) "
Returns T if STRING1 and STRING2 are character-wise CHAR=; NIL otherwise.")

(docfun string> function (string1 string2
       &key (start1 0) (end1 (length string1))
            (start2 0) (end2 (length string2))) "
If STRING1 comes after STRING2 in lexicographic order or if the strings are
character-wise CHAR=, then returns the number of characters in the longest
common prefix of the strings.  Otherwise, returns NIL.")

(docfun string>= function (string1 string2
       &key (start1 0) (end1 (length string1))
            (start2 0) (end2 (length string2))) "
If STRING1 comes after STRING2 in lexicographic order or if the strings are
character-wise CHAR=, then returns the number of characters in the longest
common prefix of the strings.  Otherwise, returns NIL.")

(docfun stringp function (x) "
Returns T if X is a string object; NIL otherwise.")

(docfun si::structurep function (x) "
ECL specific.
Returns T if X is a structure object defined by DEFSTRUCT; NIL otherwise.")

(docfun sublis function (alist tree &key (key '#'identity) (test '#'eql) test-not) "
Substitutes subtrees of TREE by using ALIST and returns the result.  The
original TREE is not destroyed.")

(docfun subseq function (sequence start &optional (end (length sequence))) "
Returns a copy of the subsequence of SEQUENCE between START (inclusive) and
END (exclusive).")

(docfun subst function (new old tree &key (key '#'identity) (test '#'eql) test-not) "
Substitutes NEW for subtrees of TREE that match OLD and returns the result.
The original TREE is not destroyed.")

(docfun substitute function (new old sequence
       &key (key '#'identity) (test '#'eql) test-not
            (start 0) (end (length sequence))
            (count most-positive-fixnum) (from-end nil)) "
Returns a copy of SEQUENCE with all elements that match OLD replaced by NEW.
The original SEQUENCE is not destroyed.")

(docfun substitute-if function (new test sequence
       &key (key '#'identity) (start 0) (end (length sequence))
            (count most-positive-fixnum) (from-end nil)) "
Returns a copy of SEQUENCE with all elements that satisfy TEST replaced by
NEW.  The original SEQUENCE is not destroyed.")

(docfun substitute-if-not function (new test sequence
       &key (key '#'identity) (start 0) (end (length sequence))
            (count most-positive-fixnum) (from-end nil)) "
Returns a copy of SEQUENCE with all elements that do not satisfy TEST replaced
by NEW.  The original SEQUENCE is not destroyed.")

(docfun svref function (simple-vector n) "
Returns the N-th element of SIMPLE-VECTOR.")

(docfun sxhash function (object) "
Returns the hash code for OBJECT as an integer.")

(doctype symbol "
Symbol objects.")

(docfun symbol-function function (symbol) "
Returns the global function definition named SYMBOL.")

(docfun symbol-name function (symbol) "
Returns the print name of SYMBOL.")

(docfun symbol-package function (symbol) "
Returns the home package of SYMBOL.  Returns NIL if SYMBOL is not interned.")

(docfun symbol-plist function (symbol) "
Returns the property list of SYMBOL.")

(docfun symbol-value function (symbol) "
Returns the value of the global variable named SYMBOL.")

(docfun symbolp function (x) "
Returns T if X is a symbol; NIL otherwise.")

(docfun ext:system function (string) "
Executes a Shell command as if STRING is an input to the Shell.")

(doctype t "
The type T is a supertype of every type.  Every object belongs to this type.")

(docvar t constant "
The value of T is T.")

(docfun tagbody special (&body forms)
"Syntax: (tagbody {tag | statement}*)

Executes STATEMENTs in order and returns NIL after the execution of the last
STATEMENT.  But, if a GO form causes a jump to one of the TAGs, then execution
continues at the point right after the TAG.  Lists are regarded as STATEMENTs
and other objects are regarded as TAGs.")

(docfun tailp function (x list) "
Returns T if X is identical to one of the conses that constitute LIST.
Returns NIL otherwise.")

(docfun tan function (radians) "
Returns the tangent of RADIANS.")

(docfun tanh function (number) "
Returns the hyperbolic tangent of NUMBER.")

(docfun tenth function (x) "
Equivalent to (CADR (CDDDDR (CDDDDR X))).")

(docfun terpri function (&optional (stream *standard-output*)) "
Outputs a newline character.")

(docfun the special (type form) "
Declares that FORM evaluates to a value of TYPE.  Evaluates FORM and checks if
the value belongs to TYPE.  If it does, returns the value.  Otherwise, signals
an error.")

(docfun third function (x) "
Equivalent to CADDR.")

(docfun throw special (tag form) "
Evaluates TAG and aborts the execution of the most recent CATCH form that
establishes a catcher with the same catch tag.  Returns all values of FORM as
the values of the CATCH form.")

(docfun tree-equal function (x y &key (test '#'eql) test-not) "
Returns T if X and Y have the same tree structures and corresponding leaves
are all the same in the sense of TEST.  Returns NIL otherwise.")

(docfun truename function (filespec) "
Returns the full pathname of the file specified by FILESPEC.  FILESPEC may be
a symbol, a string, a pathname, or a file stream.")

(docfun truncate function (number &optional (divisor 1)) "
Returns the integer obtained by truncating NUMBER/DIVISOR.  Returns the value
of (- NUMBER (* first-value DIVISOR)) as the second value.")

(docfun type-of function (x) "
Returns a type specifier of the type to which X belongs.")

(docfun unexport function (symbol &optional (package *package*)) "
Undoes the registration of SYMBOL as an external symbol of PACKAGE and makes
SYMBOL internal to PACKAGE.  SYMBOL may be a list of symbols.")

(docfun unintern function (symbol &optional (package *package*)) "
Removes SYMBOL from PACKAGE.  If PACKAGE is the home package of SYMBOL, then
makes SYMBOL uninterned.  Returns T if SYMBOL is actually registered in
PACKAGE; NIL otherwise.")

(docfun unread-char function (char &optional (stream *standard-input*)) "
Puts CHAR back on the front of the input stream STREAM.")

(docfun unuse-package function (package-spec &optional (package *package*)) "
Causes PACKAGE not to use packages specified by PACKAGE-SPEC.  PACKAGE-SPEC
may be a package object, a string, a symbol, or a list consisting of package
objects, strings, and, symbols.")

(docfun unwind-protect special (form &body cleanup-forms) "
Evaluates FORM and returns all its values.  Before returning, evaluates
CLEANUP-FORMs in order, whether FORM returns normally or abnormally by a non-
local exit.")

(docfun upper-case-p function (char) "
Returns T if CHAR is an upper-case character; NIL otherwise.")

(docfun use-package function (package-spec &optional (package *package*)) "
Causes PACKAGE to use packages specified by PACKAGE-SPEC, in addition to those
packages that PACKAGE already uses.  PACKAGE-SPEC may be a package object, a
string, a symbol, or a list consisting of package objects, strings, and
symbols.")

(docfun user-homedir-pathname function (&optional host) "
Returns a pathname the represents the user's home directory.  HOST is simply
ignored in ECL.")

(docfun values function (&rest args) "
Returns ARGs as multiple values, the N-th ARG being the N-th value.")

(docfun values-list function (list) "
Returns all elements of LIST as multiple values, the N-th element of LIST
being the N-th value.")

(docfun vectorp function (x) "
Returns T if X is a vector; NIL otherwise.")

(docfun vector-push function (new-element vector) "
Replaces ITEM for the element of VECTOR that is pointed to by the fill-pointer
of VECTOR and then increments the fill-pointer by one.  Returns NIL if the new
value of the fill-pointer becomes too large.  Otherwise, returns the new fill-
pointer as the value.")

(docfun vector-push-extend function (new-element vector &optional (extension 1)) "
Replaces ITEM for the element of VECTOR that is pointed to by the fill-pointer
of VECTOR and then increments the fill-pointer by one.  If the new value of
the fill-pointer becomes too large, extends VECTOR for N more elements.
Returns the new value of the fill-pointer.")

(docfun when macro (test &body forms) "
If TEST evaluates to non-NIL, then evaluates FORMs and returns all values of
the last FORM.  If not, simply returns NIL.")

(docfun write function (object &key (stream *standard-output*) (escape *print-escape*)
                   (radix *print-radix*) (base *print-base*)
                   (circle *print-circle*) (pretty *print-pretty*)
                   (level *print-level*) (length *print-length*)
                   (case *print-case*) (array *print-array*)
                   (gensym *print-gensym*)) "
Prints OBJECT in the specified mode.  See the variable docs of *PRINT-...* for
the mode.")

(docfun write-byte function (integer stream) "
Outputs INTEGER to the binary stream STREAM.  Returns INTEGER.")

(docfun write-char function (char &optional (stream *standard-output*)) "
Outputs CHAR to STREAM.  Returns CHAR.")

(docfun write-line function (string &optional (stream *standard-output*)
              &key (start 0) (end (length string))) "
Outputs STRING and a newline character to STREAM.  Returns STRING.")

(docfun write-string function (string &optional (stream *standard-output*)
              &key (start 0) (end (length string))) "
Outputs STRING to STREAM.  Returns STRING.")

(docfun zerop function (number) "
Returns T if the arg is zero; NIL otherwise.")

#||
;;; ----------------------------------------------------------------------
;;; System Builder Tools

(unless (find-package 'sbt) (make-package 'sbt))

(docfun sbt::build-system macro "(system &optional op mode)" "
It allows to perform operations on a system defined with SBT:DEFSYSTEM.
The possible operations are: :LOAD, :COMPILE and :PRINT.
For the load operation, in alternative to the default of loading all the
binaries in the appropriate order, there are two modes of operation
specifiable via the optional parameter MODE, which can be
:QUERY and :SOURCE.
The latter option will load the sources of the system, while with :QUERY
the user will be prompted on each file to be loaded.

The default mode for compilation is to compile just the files which need
to be recompiled according to their dependencies.
With the :FORCE option, all the files are recompiled, while with the
:QUERY option, the user will be prompted.

By supplying \fCT\fP for the :PRINT option, the sequence of operations
to be performed to build the system will be printed.
")

(docfun sbt::defsystem macro
	"(name &key :modules :directory :pathname-types)" "
NAME should be a symbol which will be used to refer to the system.
The value of :MODULES should be a list of module dependencies of
the form:

	(file load-deps compile-deps recompilation-deps)

where load-deps compile-deps recompilation-deps are lists of module names.
If the value specified for :directory is a cons, then the CAR is used as
the source file directory and the CDR is used as the binary file directory.
The values specified for :PATHNAME-TYPES specifies the extensions for
LISP souce files and binaries.")

;;; ----------------------------------------------------------------------
;;; THREADS

(docfun %delay function (nsec) "
Stops the thread execution for a time interval of NSEC real seconds. The
thread status is set to suspended.")

(docfun %disable-scheduler function () "
Disables the scheduler, so that the execution of the current thread will not be
interrupted by the scheduler until the %enable-scheduler function is called")

(docfun %enable-scheduler function () "
Enables the scheduler, so that it will time-slice execution among all
running threads.")

(docfun %suspend function () "
Sets the current thread status to suspended and suspends its execution.")

(docfun %thread-wait function (predicate &rest args) "
Applies the PREDICATE to the ARGS, in the environment of the calling thread.
If the result is not nil the thread will continue its execution. Otherwise the
thread is suspended and its status set to waiting. The thread will be resumed
again when the condition will become true.")

(docfun %thread-wait-with-timeout function (nsec predicate &rest args) "
Applies the PREDICATE to the ARGS, in the environment of the calling thread.
If the result is not nil the thread will continue its execution. Otherwise the
thread is suspended and its status set to waiting.  The thread will be resumed
again when either the condition will become true or the timeout of NSEC 
seconds has expired.")

(docfun current-thread function (thread) "
Returns the THREAD within which this function was called.")

(docfun deactivate function (thread) "
Stops a running THREAD, setting its status to stopped. A stopped thread can
be resumed with reactivate function.")

(docfun kill-thread function (thread) "
Stops the THREAD execution and set its status to dead.")

(docfun make-continuation function (thread &key :cont) "
Creates a unique new continuation for resuming the THREAD. :CONT is an optional
continuation to be supplied to the thread.")

(docfun make-thread function (function) "
Creates a new thread ready to execute FUNCTION. The thread is in a suspended
status and can run only making a continuation for it and issuing a resume
to such continuation with a list of arguments.")

(docfun reactivate function (thread) "
Sets the THREAD status to running.")

(docfun resume function (continuation &rest args) "
Resumes execution of CONTINUATION and passes it the ARGS.")

(docfun spawn macro (function &rest args) "
Creates a new thread where FUNCTION is applied to the ARGS. Returns immediately
the new thread without waiting for the function to return.")

(docfun thread-list function (thread) "
Returns the full list of the not DEAD threads")

(docfun thread-status function (thread) "
Returns the THREAD status (this can be: running, suspended, stopped or dead)")

(docfun without-scheduling macro "({form}*)" "
Executes the FORMs in sequence within a critical region, ensuring that the
scheduler is disabled.")

;;; ----------------------------------------------------------------------
;;; Unify instructions

(docfun si::dereference function (locative) "
ECL specific.
Given LOCATIVE, it returns the object to which it points. If the
location is unbound, the value returned is OBJNULL.")

(docfun si::locativep function (object) "
ECL specific.
Returns true if OBJECT is bound to a locative.")

(docfun si::make-variable function (name) "
ECL specific. 
Creates a new logical variable with name name implemented as a cons. 
Name is used just for printing purposes.")

(docfun si::get-constant function (constant object) "
ECL specific.
The value of OBJECT is unified with the constant CONSTANT.
Returns T if successful, otherwise NIL.")

(docfun si::get-cons function (object) "
ECL specific.
The value of OBJECT is unified with a CONS cell.
Returns T if successful, otherwise NIL.")

(docfun si::get-instance function (object class arity) "
ECL specific.
The value of OBJECT is unified with an instance of class CLASS
with ARITY number of slots.
Returns T if successful, otherwise NIL.")

(docfun si::get-value function (variable object) "
ECL specific.
The value of VARIABLE and OBJECT are unified.
Returns T if successful, otherwise NIL.")

(docfun get-variable macro (variable object) "
ECL specific.
Identical to SETQ: assigns to the variable VARIABLE the value OBJECT.")

(docfun si::get-nil function (object) "
ECL specific.
The value of OBJECT is unified with the constant NIL.
Returns T if successful, otherwise NIL.")

(docfun si::trail-mark function () "
ECL specific.
Sets up a choice point by putting a mark on the trail stack for
backtracking.")

(docfun si::trail-restore function () "
ECL specific.
Unwinds the trail stack up to the latest choice point.")

(docfun si::trail-unmark function () "
ECL specific.
Does a TRAIL-RESTORE and also removes the latest choice point.")

(docfun si::unboundp function (locative) "
ECL specific.
Returns true if LOCATIVE is bound to OBJNULL.")

(docfun si::unify-constant function (constant) "
ECL specific.
Read mode: the next subterm is unified with the constant CONSTANT.
Write mode: the constant constant is stored as the next subterm.")

(docfun si::unify-nil function () "
ECL specific.
Read mode: the next subterm is unified with the constant NIL.
Write mode: the constant NIL is stored as the next subterm.")

(docfun si::unify-value function (variable) "
ECL specific.
Read mode: the value of VARIABLE is unified with the next subterm.
Write mode: the value of VARIABLE is stored as the next subterm.")

(docfun si::unify-variable macro (variable) "
ECL specific.
Read mode: VARIABLE is assigned the next subterm.
Write mode: a new variable is stored in VARIABLE as the next subterm.")

||#
;;;----------------------------------------------------------------------
