#||
(core:select-package :cl)

(core::fset 'multiple-value-bind 
      #'(lambda (def env)
	  (let ((vars (cadr def))
		(multiple-value-form (caddr def))
		(body (cdddr def))
		(restname (gensym)))
	    `(multiple-value-call #'(lambda (&optional ,@(mapcar #'list vars) &rest ,restname ) ,@body) ,multiple-value-form)))
      t)

(export '(compute-restarts multiple-value-bind length sublis nsublis adjust-array))

(export '(WITH-COMPILATION-UNIT UNREAD-CHAR TYPE TIME SPEED SPACE SIN SAFETY
	  READ-CHAR PEEK-CHAR PACKAGE-USED-BY-LIST PACKAGE-NICKNAMES OPTIMIZE
	  NUMERATOR LISTEN LISP-IMPLEMENTATION-VERSION INTEGER-DECODE-FLOAT
	  INSPECT INLINE FINISH-OUTPUT FILL-POINTER DENOMINATOR DEBUG
	  COUNT CLEAR-OUTPUT CHAR ALPHANUMERICP /// // +++ ++
	  *READ-DEFAULT-FLOAT-FORMAT* *** **
	  *error-output*))

(export '(print copy-pprint-dispatch pprint-dispatch set-pprint-dispatch *print-pprint-dispatch*
	  pprint-exit-if-list-exhausted pprint-fill pprint-indent pprint-linear pprint-logical-block
	  pprint-newline pprint-pop pprint-tab pprint-tabular))




(export '(load compile eval))

(export '(
	  CELL-ERROR-NAME SHIFTF VARIABLE READ-PRESERVING-WHITESPACE
	  PRIN1-TO-STRING DEFPACKAGE TYPE-ERROR-DATUM GET-DECODED-TIME
	  *PRINT-RADIX* WITH-STANDARD-IO-SYNTAX NTH-VALUE NEXT-METHOD-P
	  WARN ADJUSTABLE-ARRAY-P SUBST PPRINT LOOP-FINISH
	  IGNORE CERROR SIMPLE-VECTOR-P ABORT DEBUG CHARACTER
))

(export '(
	  TYPE DPB HASH-TABLE PROG WITH-PACKAGE-ITERATOR DEFTYPE PSETF
	  DEPOSIT-FIELD ARITHMETIC-ERROR-OPERATION BROADCAST-STREAM SUBTYPEP RASSOC
	  PRINT-UNREADABLE-OBJECT MAKE-BROADCAST-STREAM NOTINLINE REMF SYNONYM-STREAM DRIBBLE
	  COMPILER-MACRO-FUNCTION WITH-OPEN-FILE FILE-POSITION RENAME-PACKAGE WITH-HASH-TABLE-ITERATOR
	  *PRINT-BASE* WITH-OPEN-STREAM *PRINT-PRETTY* SBIT SIN READTABLE-CASE
))

(export '(
	  STRING-STREAM SATISFIES ARITHMETIC-ERROR-OPERANDS CEILING DEFGENERIC
	  ARRAY-DISPLACEMENT *READ-DEFAULT-FLOAT-FORMAT* DO-ALL-SYMBOLS LDB *PRINT-LENGTH*
	  APROPOS-LIST CONCATENATED-STREAM COMPILER-MACRO STREAM-EXTERNAL-FORMAT SYMBOL-PLIST
	  SIMPLE-BASE-STRING TYPEP STRUCTURE FILE-STREAM COERCE FILL-POINTER
	  DO-EXTERNAL-SYMBOLS ECHO-STREAM CHAR MAP WITH-OUTPUT-TO-STRING
	  DOCUMENTATION UPGRADED-COMPLEX-PART-TYPE RANDOM-STATE DEFINE-MODIFY-MACRO COPY-SEQ SIMPLE-STRING
	  SPEED DEFINE-METHOD-COMBINATION DEFINE-SYMBOL-MACRO MASK-FIELD
	  FIND-ALL-SYMBOLS MAP-INTO DEFMETHOD YES-OR-NO-P UNUSE-PACKAGE WRITE-TO-STRING
))

(export '(
	  SPACE DEFCLASS HANDLER-BIND PRINT-OBJECT PROG2 IMAGPART REALPART
	  PRINC-TO-STRING MAKE-STRING-INPUT-STREAM TWO-WAY-STREAM UPGRADED-ARRAY-ELEMENT-TYPE
	  IGNORABLE MAKE-ECHO-STREAM PROG* *PRINT-LEVEL* PRINT-NOT-READABLE-OBJECT
	  OPTIMIZE BASE-STRING DO-SYMBOLS SAFETY LOGICAL-PATHNAME
	  TYPE-ERROR-EXPECTED-TYPE DEFINE-SETF-EXPANDER MULTIPLE-VALUE-LIST RANDOM-STATE-P
	  FORMATTER UNSIGNED-BYTE SIGNED-BYTE Y-OR-N-P CALL-NEXT-METHOD
	  NSUBST DECLARATION WITH-INPUT-FROM-STRING ROTATEF COMPILE-FILE
	  disassemble
	  ))


(export '(WITH-SIMPLE-RESTART WITH-CONDITION-RESTARTS WARNING USE-VALUE
	  UNTRACE UNDEFINED-FUNCTION UNBOUND-VARIABLE UNBOUND-SLOT-INSTANCE
	  UNBOUND-SLOT TRUNCATE TRACE STYLE-WARNING STRING-LEFT-TRIM
	  STREAM-ERROR-STREAM STORE-VALUE STORAGE-CONDITION SIMPLE-WARNING))

(export '(SIMPLE-TYPE-ERROR SIMPLE-CONDITION-FORMAT-CONTROL
	  SIMPLE-CONDITION-FORMAT-ARGUMENTS SIMPLE-CONDITION SIGNAL
	  SERIOUS-CONDITION ROUND RESTART-NAME RESTART-CASE
	  REM PROGRAM-ERROR PRINT-NOT-READABLE PPRINT-TAB
	  PPRINT-POP PPRINT-NEWLINE PPRINT-LOGICAL-BLOCK
	  PPRINT-INDENT PACKAGE-ERROR-PACKAGE NSTRING-UPCASE
	  NSTRING-DOWNCASE NSTRING-CAPITALIZE MUFFLE-WARNING
	  INVOKE-RESTART-INTERACTIVELY INVOKE-DEBUGGER IGNORE-ERRORS))

(export '(HANDLER-CASE GRAPHIC-CHAR-P FUNCTION-LAMBDA-EXPRESSION
	  FLOATING-POINT-UNDERFLOW FLOATING-POINT-OVERFLOW
	  FLOATING-POINT-INVALID-OPERATION FLOATING-POINT-INEXACT
	  FILE-ERROR-PATHNAME END-OF-FILE DIVISION-BY-ZERO
	  DEFINE-CONDITION DECODE-FLOAT CONTINUE CONDITION
	  BREAK ARITHMETIC-ERROR *PRINT-CIRCLE* *ERROR-OUTPUT*  ))

(export '(*print-readably* COMPILATION-SPEED PPRINT-DISPATCH))
(export '(ansi-stream))
(export '(T cons))
(export '(lambda-list-keywords))
(export 'check-type)
(export '(1- 1+))
(export '(defun))
(export '(and or))
(export 'constantly)
(export 'return)
(export 'psetq)
(export '(do do*))

(export 'when)
(export 'unless)
(export '(until while))

(export 'prog1)
(export 'nconc)
(export 'tailp)
(export 'ldiff)
(export '(in-package defmacro push pop incf decf))
(export 'adjoin)
(export 'pushnew)
(export '(concatenate 1+ 1-))
(export 'proclaim)
(export '(
	  read-evaluated-form 
	  wrong-type-argument 
	  check-type 
	  do-check-type 
	  assert 
	  accumulate-cases 
	  ecase-error 
	  ecase 
	  ccase-error 
	  remove-otherwise-from-clauses 
	  ccase 
	  typecase 
	  etypecase-error 
	  etypecase 
	  ctypecase-error 
	  ctypecase 
	  ))
(export 'defstruct)
(export 'structure-subtype-p)
(export '(declaim))
(export '( union nunion 
	  intersection nintersection 
	  set-difference nset-difference 
	  swap-args 
	  set-exclusive-or nset-exclusive-or 
	  subsetp 
	  rassoc-if rassoc-if-not 
	  assoc-if assoc-if-not 
	  member-if member-if-not 
	  subst-if subst-if-not 
	  nsubst-if nsubst-if-not ))
(export '(loop))
(export 'make-array)
(export '(provide require))
(export '(
	  seqtype 
	  sequence-count 
	  unsafe-funcall1 
	  reduce 
	  fill 
	  replace 
	  filter-vector 
	  remove-list 
	  remove 
	  remove-if 
	  remove-if-not 
	  delete-list 
	  delete 
	  delete-if 
	  delete-if-not 
	  count 
	  count-if 
	  count-if-not 
	  substitute 
	  substitute-if 
	  substitute-if-not 
	  nsubstitute 
	  nsubstitute-if 
	  nsubstitute-if-not 
	  find 
	  find-if 
	  find-if-not 
	  position 
	  position-if 
	  position-if-not 
	  remove-duplicates-list 
	  remove-duplicates 
	  delete-duplicates-list 
	  filter-duplicates-vector 
	  delete-duplicates 
	  mismatch 
	  search 
	  search-vector 
	  search-generic 
	  sort 
	  list-merge-sort 
	  quick-sort 
	  stable-sort 
	  merge 
	  complement 
	  ))
(export '(defsetf
	  setf
	  get-setf-expansion
	  push
	  pushnew
	  pop
	  define-compiler-macro
	  std-compute-applicable-methods
	  defconstant
	  defparameter
	  defvar
	  ))

(defvar +++ nil)
(defvar ++ nil)
(defvar + nil)
(defvar - nil)
(defvar * nil)
(defvar ** nil)
(defvar *** nil)
(defvar / nil)
(defvar // nil)
(defvar /// nil)

||#

(core::select-package :core)


(defparameter *all-cl-names* '("&ALLOW-OTHER-KEYS"
			   "&AUX"
			   "&BODY"
			   "&ENVIRONMENT"
			   "&KEY"
			   "&OPTIONAL"
			   "&REST"
			   "&WHOLE"
			   "*"
			   "**"
			   "***"
			   "*BREAK-ON-SIGNALS*"
			   "*COMPILE-FILE-PATHNAME*"
			   "*COMPILE-FILE-TRUENAME*"
			   "*COMPILE-PRINT*"
			   "*COMPILE-VERBOSE*"
			   "*DEBUG-IO*"
			   "*DEBUGGER-HOOK*"
			   "*DEFAULT-PATHNAME-DEFAULTS*"
			   "*ERROR-OUTPUT*"
			   "*FEATURES*"
			   "*GENSYM-COUNTER*"
			   "*LOAD-PATHNAME*"
			   "*LOAD-PRINT*"
			   "*LOAD-TRUENAME*"
			   "*LOAD-VERBOSE*"
			   "*MACROEXPAND-HOOK*"
			   "*MODULES*"
			   "*PACKAGE*"
			   "*PRINT-ARRAY*"
			   "*PRINT-BASE*"
			   "*PRINT-CASE*"
			   "*PRINT-CIRCLE*"
			   "*PRINT-ESCAPE*"
			   "*PRINT-GENSYM*"
			   "*PRINT-LENGTH*"
			   "*PRINT-LEVEL*"
			   "*PRINT-LINES*"
			   "*PRINT-MISER-WIDTH*"
			   "*PRINT-PPRINT-DISPATCH*"
			   "*PRINT-PRETTY*"
			   "*PRINT-RADIX*"
			   "*PRINT-READABLY*"
			   "*PRINT-RIGHT-MARGIN*"
			   "*QUERY-IO*"
			   "*RANDOM-STATE*"
			   "*READ-BASE*"
			   "*READ-DEFAULT-FLOAT-FORMAT*"
			   "*READ-EVAL*"
			   "*READ-SUPPRESS*"
			   "*READTABLE*"
			   "*STANDARD-INPUT*"
			   "*STANDARD-OUTPUT*"
			   "*TERMINAL-IO*"
			   "*TRACE-OUTPUT*"
			   "+"
			   "++"
			   "+++"
			   "-"
			   "/"
			   "//"
			   "///"
			   "/="
			   "1+"
			   "1-"
			   "<"
			   "<="
			   "="
			   ">"
			   ">="
			   "ABORT"
			   "ABS"
			   "ACONS"
			   "ACOS"
			   "ACOSH"
			   "ADD-METHOD"
			   "ADJOIN"
			   "ADJUST-ARRAY"
			   "ADJUSTABLE-ARRAY-P"
			   "ALLOCATE-INSTANCE"
			   "ALPHA-CHAR-P"
			   "ALPHANUMERICP"
			   "AND"
			   "APPEND"
			   "APPLY"
			   "APROPOS"
			   "APROPOS-LIST"
			   "AREF"
			   "ARITHMETIC-ERROR"
			   "ARITHMETIC-ERROR-OPERANDS"
			   "ARITHMETIC-ERROR-OPERATION"
			   "ARRAY"
			   "ARRAY-DIMENSION"
			   "ARRAY-DIMENSION-LIMIT"
			   "ARRAY-DIMENSIONS"
			   "ARRAY-DISPLACEMENT"
			   "ARRAY-ELEMENT-TYPE"
			   "ARRAY-HAS-FILL-POINTER-P"
			   "ARRAY-IN-BOUNDS-P"
			   "ARRAY-RANK"
			   "ARRAY-RANK-LIMIT"
			   "ARRAY-ROW-MAJOR-INDEX"
			   "ARRAY-TOTAL-SIZE"
			   "ARRAY-TOTAL-SIZE-LIMIT"
			   "ARRAYP"
			   "ASH"
			   "ASIN"
			   "ASINH"
			   "ASSERT"
			   "ASSOC"
			   "ASSOC-IF"
			   "ASSOC-IF-NOT"
			   "ATAN"
			   "ATANH"
			   "ATOM"
			   "BASE-CHAR"
			   "BASE-STRING"
			   "BIGNUM"
			   "BIT"
			   "BIT-AND"
			   "BIT-ANDC1"
			   "BIT-ANDC2"
			   "BIT-EQV"
			   "BIT-IOR"
			   "BIT-NAND"
			   "BIT-NOR"
			   "BIT-NOT"
			   "BIT-ORC1"
			   "BIT-ORC2"
			   "BIT-VECTOR"
			   "BIT-VECTOR-P"
			   "BIT-XOR"
			   "BLOCK"
			   "BOOLE"
			   "BOOLE-1"
			   "BOOLE-2"
			   "BOOLE-AND"
			   "BOOLE-ANDC1"
			   "BOOLE-ANDC2"
			   "BOOLE-C1"
			   "BOOLE-C2"
			   "BOOLE-CLR"
			   "BOOLE-EQV"
			   "BOOLE-IOR"
			   "BOOLE-NAND"
			   "BOOLE-NOR"
			   "BOOLE-ORC1"
			   "BOOLE-ORC2"
			   "BOOLE-SET"
			   "BOOLE-XOR"
			   "BOOLEAN"
			   "BOTH-CASE-P"
			   "BOUNDP"
			   "BREAK"
			   "BROADCAST-STREAM"
			   "BROADCAST-STREAM-STREAMS"
			   "BUILT-IN-CLASS"
			   "BUTLAST"
			   "BYTE"
			   "BYTE-POSITION"
			   "BYTE-SIZE"
			   "CAAAAR"
			   "CAAADR"
			   "CAAAR"
			   "CAADAR"
			   "CAADDR"
			   "CAADR"
			   "CAAR"
			   "CADAAR"
			   "CADADR"
			   "CADAR"
			   "CADDAR"
			   "CADDDR"
			   "CADDR"
			   "CADR"
			   "CALL-ARGUMENTS-LIMIT"
			   "CALL-METHOD"
			   "CALL-NEXT-METHOD"
			   "CAR"
			   "CASE"
			   "CATCH"
			   "CCASE"
			   "CDAAAR"
			   "CDAADR"
			   "CDAAR"
			   "CDADAR"
			   "CDADDR"
			   "CDADR"
			   "CDAR"
			   "CDDAAR"
			   "CDDADR"
			   "CDDAR"
			   "CDDDAR"
			   "CDDDDR"
			   "CDDDR"
			   "CDDR"
			   "CDR"
			   "CEILING"
			   "CELL-ERROR"
			   "CELL-ERROR-NAME"
			   "CERROR"
			   "CHANGE-CLASS"
			   "CHAR"
			   "CHAR-CODE"
			   "CHAR-CODE-LIMIT"
			   "CHAR-DOWNCASE"
			   "CHAR-EQUAL"
			   "CHAR-GREATERP"
			   "CHAR-INT"
			   "CHAR-LESSP"
			   "CHAR-NAME"
			   "CHAR-NOT-EQUAL"
			   "CHAR-NOT-GREATERP"
			   "CHAR-NOT-LESSP"
			   "CHAR-UPCASE"
			   "CHAR/="
			   "CHAR<"
			   "CHAR<="
			   "CHAR="
			   "CHAR>"
			   "CHAR>="
			   "CHARACTER"
			   "CHARACTERP"
			   "CHECK-TYPE"
			   "CIS"
			   "CLASS"
			   "CLASS-NAME"
			   "CLASS-OF"
			   "CLEAR-INPUT"
			   "CLEAR-OUTPUT"
			   "CLOSE"
			   "CLRHASH"
			   "CODE-CHAR"
			   "COERCE"
			   "COMPILATION-SPEED"
			   "COMPILE"
			   "COMPILE-FILE"
			   "COMPILE-FILE-PATHNAME"
			   "COMPILED-FUNCTION"
			   "COMPILED-FUNCTION-P"
			   "COMPILER-MACRO"
			   "COMPILER-MACRO-FUNCTION"
			   "COMPLEMENT"
			   "COMPLEX"
			   "COMPLEXP"
			   "COMPUTE-APPLICABLE-METHODS"
			   "COMPUTE-RESTARTS"
			   "CONCATENATE"
			   "CONCATENATED-STREAM"
			   "CONCATENATED-STREAM-STREAMS"
			   "COND"
			   "CONDITION"
			   "CONJUGATE"
			   "CONS"
			   "CONSP"
			   "CONSTANTLY"
			   "CONSTANTP"
			   "CONTINUE"
			   "CONTROL-ERROR"
			   "COPY-ALIST"
			   "COPY-LIST"
			   "COPY-PPRINT-DISPATCH"
			   "COPY-READTABLE"
			   "COPY-SEQ"
			   "COPY-STRUCTURE"
			   "COPY-SYMBOL"
			   "COPY-TREE"
			   "COS"
			   "COSH"
			   "COUNT"
			   "COUNT-IF"
			   "COUNT-IF-NOT"
			   "CTYPECASE"
			   "DEBUG"
			   "DECF"
			   "DECLAIM"
			   "DECLARATION"
			   "DECLARE"
			   "DECODE-FLOAT"
			   "DECODE-UNIVERSAL-TIME"
			   "DEFCLASS"
			   "DEFCONSTANT"
			   "DEFGENERIC"
			   "DEFINE-COMPILER-MACRO"
			   "DEFINE-CONDITION"
			   "DEFINE-METHOD-COMBINATION"
			   "DEFINE-MODIFY-MACRO"
			   "DEFINE-SETF-EXPANDER"
			   "DEFINE-SYMBOL-MACRO"
			   "DEFMACRO"
			   "DEFMETHOD"
			   "DEFPACKAGE"
			   "DEFPARAMETER"
			   "DEFSETF"
			   "DEFSTRUCT"
			   "DEFTYPE"
			   "DEFUN"
			   "DEFVAR"
			   "DELETE"
			   "DELETE-DUPLICATES"
			   "DELETE-FILE"
			   "DELETE-IF"
			   "DELETE-IF-NOT"
			   "DELETE-PACKAGE"
			   "DENOMINATOR"
			   "DEPOSIT-FIELD"
			   "DESCRIBE"
			   "DESCRIBE-OBJECT"
			   "DESTRUCTURING-BIND"
			   "DIGIT-CHAR"
			   "DIGIT-CHAR-P"
			   "DIRECTORY"
			   "DIRECTORY-NAMESTRING"
			   "DISASSEMBLE"
			   "DIVISION-BY-ZERO"
			   "DO"
			   "DO*"
			   "DO-ALL-SYMBOLS"
			   "DO-EXTERNAL-SYMBOLS"
			   "DO-SYMBOLS"
			   "DOCUMENTATION"
			   "DOLIST"
			   "DOTIMES"
			   "DOUBLE-FLOAT"
			   "DOUBLE-FLOAT-EPSILON"
			   "DOUBLE-FLOAT-NEGATIVE-EPSILON"
			   "DPB"
			   "DRIBBLE"
			   "DYNAMIC-EXTENT"
			   "ECASE"
			   "ECHO-STREAM"
			   "ECHO-STREAM-INPUT-STREAM"
			   "ECHO-STREAM-OUTPUT-STREAM"
			   "ED"
			   "EIGHTH"
			   "ELT"
			   "ENCODE-UNIVERSAL-TIME"
			   "END-OF-FILE"
			   "ENDP"
			   "ENOUGH-NAMESTRING"
			   "ENSURE-DIRECTORIES-EXIST"
			   "ENSURE-GENERIC-FUNCTION"
			   "EQ"
			   "EQL"
			   "EQUAL"
			   "EQUALP"
			   "ERROR"
			   "ETYPECASE"
			   "EVAL"
			   "EVAL-WHEN"
			   "EVENP"
			   "EVERY"
			   "EXP"
			   "EXPORT"
			   "EXPT"
			   "EXTENDED-CHAR"
			   "FBOUNDP"
			   "FCEILING"
			   "FDEFINITION"
			   "FFLOOR"
			   "FIFTH"
			   "FILE-AUTHOR"
			   "FILE-ERROR"
			   "FILE-ERROR-PATHNAME"
			   "FILE-LENGTH"
			   "FILE-NAMESTRING"
			   "FILE-POSITION"
			   "FILE-STREAM"
			   "FILE-STRING-LENGTH"
			   "FILE-WRITE-DATE"
			   "FILL"
			   "FILL-POINTER"
			   "FIND"
			   "FIND-ALL-SYMBOLS"
			   "FIND-CLASS"
			   "FIND-IF"
			   "FIND-IF-NOT"
			   "FIND-METHOD"
			   "FIND-PACKAGE"
			   "FIND-RESTART"
			   "FIND-SYMBOL"
			   "FINISH-OUTPUT"
			   "FIRST"
			   "FIXNUM"
			   "FLET"
			   "FLOAT"
			   "FLOAT-DIGITS"
			   "FLOAT-PRECISION"
			   "FLOAT-RADIX"
			   "FLOAT-SIGN"
			   "FLOATING-POINT-INEXACT"
			   "FLOATING-POINT-INVALID-OPERATION"
			   "FLOATING-POINT-OVERFLOW"
			   "FLOATING-POINT-UNDERFLOW"
			   "FLOATP"
			   "FLOOR"
			   "FMAKUNBOUND"
			   "FORCE-OUTPUT"
			   "FORMAT"
			   "FORMATTER"
			   "FOURTH"
			   "FRESH-LINE"
			   "FROUND"
			   "FTRUNCATE"
			   "FTYPE"
			   "FUNCALL"
			   "FUNCTION"
			   "FUNCTION-KEYWORDS"
			   "FUNCTION-LAMBDA-EXPRESSION"
			   "FUNCTIONP"
			   "GCD"
			   "GENERIC-FUNCTION"
			   "GENSYM"
			   "GENTEMP"
			   "GET"
			   "GET-DECODED-TIME"
			   "GET-DISPATCH-MACRO-CHARACTER"
			   "GET-INTERNAL-REAL-TIME"
			   "GET-INTERNAL-RUN-TIME"
			   "GET-MACRO-CHARACTER"
			   "GET-OUTPUT-STREAM-STRING"
			   "GET-PROPERTIES"
			   "GET-SETF-EXPANSION"
			   "GET-UNIVERSAL-TIME"
			   "GETF"
			   "GETHASH"
			   "GO"
			   "GRAPHIC-CHAR-P"
			   "HANDLER-BIND"
			   "HANDLER-CASE"
			   "HASH-TABLE"
			   "HASH-TABLE-COUNT"
			   "HASH-TABLE-P"
			   "HASH-TABLE-REHASH-SIZE"
			   "HASH-TABLE-REHASH-THRESHOLD"
			   "HASH-TABLE-SIZE"
			   "HASH-TABLE-TEST"
			   "HOST-NAMESTRING"
			   "IDENTITY"
			   "IF"
			   "IGNORABLE"
			   "IGNORE"
			   "IGNORE-ERRORS"
			   "IMAGPART"
			   "IMPORT"
			   "IN-PACKAGE"
			   "INCF"
			   "INITIALIZE-INSTANCE"
			   "INLINE"
			   "INPUT-STREAM-P"
			   "INSPECT"
			   "INTEGER"
			   "INTEGER-DECODE-FLOAT"
			   "INTEGER-LENGTH"
			   "INTEGERP"
			   "INTERACTIVE-STREAM-P"
			   "INTERN"
			   "INTERNAL-TIME-UNITS-PER-SECOND"
			   "INTERSECTION"
			   "INVALID-METHOD-ERROR"
			   "INVOKE-DEBUGGER"
			   "INVOKE-RESTART"
			   "INVOKE-RESTART-INTERACTIVELY"
			   "ISQRT"
			   "KEYWORD"
			   "KEYWORDP"
			   "LABELS"
			   "LAMBDA"
			   "LAMBDA-LIST-KEYWORDS"
			   "LAMBDA-PARAMETERS-LIMIT"
			   "LAST"
			   "LCM"
			   "LDB"
			   "LDB-TEST"
			   "LDIFF"
			   "LEAST-NEGATIVE-DOUBLE-FLOAT"
			   "LEAST-NEGATIVE-LONG-FLOAT"
			   "LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT"
			   "LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT"
			   "LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT"
			   "LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT"
			   "LEAST-NEGATIVE-SHORT-FLOAT"
			   "LEAST-NEGATIVE-SINGLE-FLOAT"
			   "LEAST-POSITIVE-DOUBLE-FLOAT"
			   "LEAST-POSITIVE-LONG-FLOAT"
			   "LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT"
			   "LEAST-POSITIVE-NORMALIZED-LONG-FLOAT"
			   "LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT"
			   "LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT"
			   "LEAST-POSITIVE-SHORT-FLOAT"
			   "LEAST-POSITIVE-SINGLE-FLOAT"
			   "LENGTH"
			   "LET"
			   "LET*"
			   "LISP-IMPLEMENTATION-TYPE"
			   "LISP-IMPLEMENTATION-VERSION"
			   "LIST"
			   "LIST*"
			   "LIST-ALL-PACKAGES"
			   "LIST-LENGTH"
			   "LISTEN"
			   "LISTP"
			   "LOAD"
			   "LOAD-LOGICAL-PATHNAME-TRANSLATIONS"
			   "LOAD-TIME-VALUE"
			   "LOCALLY"
			   "LOG"
			   "LOGAND"
			   "LOGANDC1"
			   "LOGANDC2"
			   "LOGBITP"
			   "LOGCOUNT"
			   "LOGEQV"
			   "LOGICAL-PATHNAME"
			   "LOGICAL-PATHNAME-TRANSLATIONS"
			   "LOGIOR"
			   "LOGNAND"
			   "LOGNOR"
			   "LOGNOT"
			   "LOGORC1"
			   "LOGORC2"
			   "LOGTEST"
			   "LOGXOR"
			   "LONG-FLOAT"
			   "LONG-FLOAT-EPSILON"
			   "LONG-FLOAT-NEGATIVE-EPSILON"
			   "LONG-SITE-NAME"
			   "LOOP"
			   "LOOP-FINISH"
			   "LOWER-CASE-P"
			   "MACHINE-INSTANCE"
			   "MACHINE-TYPE"
			   "MACHINE-VERSION"
			   "MACRO-FUNCTION"
			   "MACROEXPAND"
			   "MACROEXPAND-1"
			   "MACROLET"
			   "MAKE-ARRAY"
			   "MAKE-BROADCAST-STREAM"
			   "MAKE-CONCATENATED-STREAM"
			   "MAKE-CONDITION"
			   "MAKE-DISPATCH-MACRO-CHARACTER"
			   "MAKE-ECHO-STREAM"
			   "MAKE-HASH-TABLE"
			   "MAKE-INSTANCE"
			   "MAKE-INSTANCES-OBSOLETE"
			   "MAKE-LIST"
			   "MAKE-LOAD-FORM"
			   "MAKE-LOAD-FORM-SAVING-SLOTS"
			   "MAKE-METHOD"
			   "MAKE-PACKAGE"
			   "MAKE-PATHNAME"
			   "MAKE-RANDOM-STATE"
			   "MAKE-SEQUENCE"
			   "MAKE-STRING"
			   "MAKE-STRING-INPUT-STREAM"
			   "MAKE-STRING-OUTPUT-STREAM"
			   "MAKE-SYMBOL"
			   "MAKE-SYNONYM-STREAM"
			   "MAKE-TWO-WAY-STREAM"
			   "MAKUNBOUND"
			   "MAP"
			   "MAP-INTO"
			   "MAPC"
			   "MAPCAN"
			   "MAPCAR"
			   "MAPCON"
			   "MAPHASH"
			   "MAPL"
			   "MAPLIST"
			   "MASK-FIELD"
			   "MAX"
			   "MEMBER"
			   "MEMBER-IF"
			   "MEMBER-IF-NOT"
			   "MERGE"
			   "MERGE-PATHNAMES"
			   "METHOD"
			   "METHOD-COMBINATION"
			   "METHOD-COMBINATION-ERROR"
			   "METHOD-QUALIFIERS"
			   "MIN"
			   "MINUSP"
			   "MISMATCH"
			   "MOD"
			   "MOST-NEGATIVE-DOUBLE-FLOAT"
			   "MOST-NEGATIVE-FIXNUM"
			   "MOST-NEGATIVE-LONG-FLOAT"
			   "MOST-NEGATIVE-SHORT-FLOAT"
			   "MOST-NEGATIVE-SINGLE-FLOAT"
			   "MOST-POSITIVE-DOUBLE-FLOAT"
			   "MOST-POSITIVE-FIXNUM"
			   "MOST-POSITIVE-LONG-FLOAT"
			   "MOST-POSITIVE-SHORT-FLOAT"
			   "MOST-POSITIVE-SINGLE-FLOAT"
			   "MUFFLE-WARNING"
			   "MULTIPLE-VALUE-BIND"
			   "MULTIPLE-VALUE-CALL"
			   "MULTIPLE-VALUE-LIST"
			   "MULTIPLE-VALUE-PROG1"
			   "MULTIPLE-VALUE-SETQ"
			   "MULTIPLE-VALUES-LIMIT"
			   "NAME-CHAR"
			   "NAMESTRING"
			   "NBUTLAST"
			   "NCONC"
			   "NEXT-METHOD-P"
			   "NIL"
			   "NINTERSECTION"
			   "NINTH"
			   "NO-APPLICABLE-METHOD"
			   "NO-NEXT-METHOD"
			   "NOT"
			   "NOTANY"
			   "NOTEVERY"
			   "NOTINLINE"
			   "NRECONC"
			   "NREVERSE"
			   "NSET-DIFFERENCE"
			   "NSET-EXCLUSIVE-OR"
			   "NSTRING-CAPITALIZE"
			   "NSTRING-DOWNCASE"
			   "NSTRING-UPCASE"
			   "NSUBLIS"
			   "NSUBST"
			   "NSUBST-IF"
			   "NSUBST-IF-NOT"
			   "NSUBSTITUTE"
			   "NSUBSTITUTE-IF"
			   "NSUBSTITUTE-IF-NOT"
			   "NTH"
			   "NTH-VALUE"
			   "NTHCDR"
			   "NULL"
			   "NUMBER"
			   "NUMBERP"
			   "NUMERATOR"
			   "NUNION"
			   "ODDP"
			   "OPEN"
			   "OPEN-STREAM-P"
			   "OPTIMIZE"
			   "OR"
			   "OTHERWISE"
			   "OUTPUT-STREAM-P"
			   "PACKAGE"
			   "PACKAGE-ERROR"
			   "PACKAGE-ERROR-PACKAGE"
			   "PACKAGE-NAME"
			   "PACKAGE-NICKNAMES"
			   "PACKAGE-SHADOWING-SYMBOLS"
			   "PACKAGE-USE-LIST"
			   "PACKAGE-USED-BY-LIST"
			   "PACKAGEP"
			   "PAIRLIS"
			   "PARSE-ERROR"
			   "PARSE-INTEGER"
			   "PARSE-NAMESTRING"
			   "PATHNAME"
			   "PATHNAME-DEVICE"
			   "PATHNAME-DIRECTORY"
			   "PATHNAME-HOST"
			   "PATHNAME-MATCH-P"
			   "PATHNAME-NAME"
			   "PATHNAME-TYPE"
			   "PATHNAME-VERSION"
			   "PATHNAMEP"
			   "PEEK-CHAR"
			   "PHASE"
			   "PI"
			   "PLUSP"
			   "POP"
			   "POSITION"
			   "POSITION-IF"
			   "POSITION-IF-NOT"
			   "PPRINT"
			   "PPRINT-DISPATCH"
			   "PPRINT-EXIT-IF-LIST-EXHAUSTED"
			   "PPRINT-FILL"
			   "PPRINT-INDENT"
			   "PPRINT-LINEAR"
			   "PPRINT-LOGICAL-BLOCK"
			   "PPRINT-NEWLINE"
			   "PPRINT-POP"
			   "PPRINT-TAB"
			   "PPRINT-TABULAR"
			   "PRIN1"
			   "PRIN1-TO-STRING"
			   "PRINC"
			   "PRINC-TO-STRING"
			   "PRINT"
			   "PRINT-NOT-READABLE"
			   "PRINT-NOT-READABLE-OBJECT"
			   "PRINT-OBJECT"
			   "PRINT-UNREADABLE-OBJECT"
			   "PROBE-FILE"
			   "PROCLAIM"
			   "PROG"
			   "PROG*"
			   "PROG1"
			   "PROG2"
			   "PROGN"
			   "PROGRAM-ERROR"
			   "PROGV"
			   "PROVIDE"
			   "PSETF"
			   "PSETQ"
			   "PUSH"
			   "PUSHNEW"
			   "QUOTE"
			   "RANDOM"
			   "RANDOM-STATE"
			   "RANDOM-STATE-P"
			   "RASSOC"
			   "RASSOC-IF"
			   "RASSOC-IF-NOT"
			   "RATIO"
			   "RATIONAL"
			   "RATIONALIZE"
			   "RATIONALP"
			   "READ"
			   "READ-BYTE"
			   "READ-CHAR"
			   "READ-CHAR-NO-HANG"
			   "READ-DELIMITED-LIST"
			   "READ-FROM-STRING"
			   "READ-LINE"
			   "READ-PRESERVING-WHITESPACE"
			   "READ-SEQUENCE"
			   "READER-ERROR"
			   "READTABLE"
			   "READTABLE-CASE"
			   "READTABLEP"
			   "REAL"
			   "REALP"
			   "REALPART"
			   "REDUCE"
			   "REINITIALIZE-INSTANCE"
			   "REM"
			   "REMF"
			   "REMHASH"
			   "REMOVE"
			   "REMOVE-DUPLICATES"
			   "REMOVE-IF"
			   "REMOVE-IF-NOT"
			   "REMOVE-METHOD"
			   "REMPROP"
			   "RENAME-FILE"
			   "RENAME-PACKAGE"
			   "REPLACE"
			   "REQUIRE"
			   "REST"
			   "RESTART"
			   "RESTART-BIND"
			   "RESTART-CASE"
			   "RESTART-NAME"
			   "RETURN"
			   "RETURN-FROM"
			   "REVAPPEND"
			   "REVERSE"
			   "ROOM"
			   "ROTATEF"
			   "ROUND"
			   "ROW-MAJOR-AREF"
			   "RPLACA"
			   "RPLACD"
			   "SAFETY"
			   "SATISFIES"
			   "SBIT"
			   "SCALE-FLOAT"
			   "SCHAR"
			   "SEARCH"
			   "SECOND"
			   "SEQUENCE"
			   "SERIOUS-CONDITION"
			   "SET"
			   "SET-DIFFERENCE"
			   "SET-DISPATCH-MACRO-CHARACTER"
			   "SET-EXCLUSIVE-OR"
			   "SET-MACRO-CHARACTER"
			   "SET-PPRINT-DISPATCH"
			   "SET-SYNTAX-FROM-CHAR"
			   "SETF"
			   "SETQ"
			   "SEVENTH"
			   "SHADOW"
			   "SHADOWING-IMPORT"
			   "SHARED-INITIALIZE"
			   "SHIFTF"
			   "SHORT-FLOAT"
			   "SHORT-FLOAT-EPSILON"
			   "SHORT-FLOAT-NEGATIVE-EPSILON"
			   "SHORT-SITE-NAME"
			   "SIGNAL"
			   "SIGNED-BYTE"
			   "SIGNUM"
			   "SIMPLE-ARRAY"
			   "SIMPLE-BASE-STRING"
			   "SIMPLE-BIT-VECTOR"
			   "SIMPLE-BIT-VECTOR-P"
			   "SIMPLE-CONDITION"
			   "SIMPLE-CONDITION-FORMAT-ARGUMENTS"
			   "SIMPLE-CONDITION-FORMAT-CONTROL"
			   "SIMPLE-ERROR"
			   "SIMPLE-STRING"
			   "SIMPLE-STRING-P"
			   "SIMPLE-TYPE-ERROR"
			   "SIMPLE-VECTOR"
			   "SIMPLE-VECTOR-P"
			   "SIMPLE-WARNING"
			   "SIN"
			   "SINGLE-FLOAT"
			   "SINGLE-FLOAT-EPSILON"
			   "SINGLE-FLOAT-NEGATIVE-EPSILON"
			   "SINH"
			   "SIXTH"
			   "SLEEP"
			   "SLOT-BOUNDP"
			   "SLOT-EXISTS-P"
			   "SLOT-MAKUNBOUND"
			   "SLOT-MISSING"
			   "SLOT-UNBOUND"
			   "SLOT-VALUE"
			   "SOFTWARE-TYPE"
			   "SOFTWARE-VERSION"
			   "SOME"
			   "SORT"
			   "SPACE"
			   "SPECIAL"
			   "SPECIAL-OPERATOR-P"
			   "SPEED"
			   "SQRT"
			   "STABLE-SORT"
			   "STANDARD"
			   "STANDARD-CHAR"
			   "STANDARD-CHAR-P"
			   "STANDARD-CLASS"
			   "STANDARD-GENERIC-FUNCTION"
			   "STANDARD-METHOD"
			   "STANDARD-OBJECT"
			   "STEP"
			   "STORAGE-CONDITION"
			   "STORE-VALUE"
			   "STREAM"
			   "STREAM-ELEMENT-TYPE"
			   "STREAM-ERROR"
			   "STREAM-ERROR-STREAM"
			   "STREAM-EXTERNAL-FORMAT"
			   "STREAMP"
			   "STRING"
			   "STRING-CAPITALIZE"
			   "STRING-DOWNCASE"
			   "STRING-EQUAL"
			   "STRING-GREATERP"
			   "STRING-LEFT-TRIM"
			   "STRING-LESSP"
			   "STRING-NOT-EQUAL"
			   "STRING-NOT-GREATERP"
			   "STRING-NOT-LESSP"
			   "STRING-RIGHT-TRIM"
			   "STRING-STREAM"
			   "STRING-TRIM"
			   "STRING-UPCASE"
			   "STRING/="
			   "STRING<"
			   "STRING<="
			   "STRING="
			   "STRING>"
			   "STRING>="
			   "STRINGP"
			   "STRUCTURE"
			   "STRUCTURE-CLASS"
			   "STRUCTURE-OBJECT"
			   "STYLE-WARNING"
			   "SUBLIS"
			   "SUBSEQ"
			   "SUBSETP"
			   "SUBST"
			   "SUBST-IF"
			   "SUBST-IF-NOT"
			   "SUBSTITUTE"
			   "SUBSTITUTE-IF"
			   "SUBSTITUTE-IF-NOT"
			   "SUBTYPEP"
			   "SVREF"
			   "SXHASH"
			   "SYMBOL"
			   "SYMBOL-FUNCTION"
			   "SYMBOL-MACROLET"
			   "SYMBOL-NAME"
			   "SYMBOL-PACKAGE"
			   "SYMBOL-PLIST"
			   "SYMBOL-VALUE"
			   "SYMBOLP"
			   "SYNONYM-STREAM"
			   "SYNONYM-STREAM-SYMBOL"
			   "T"
			   "TAGBODY"
			   "TAILP"
			   "TAN"
			   "TANH"
			   "TENTH"
			   "TERPRI"
			   "THE"
			   "THIRD"
			   "THROW"
			   "TIME"
			   "TRACE"
			   "TRANSLATE-LOGICAL-PATHNAME"
			   "TRANSLATE-PATHNAME"
			   "TREE-EQUAL"
			   "TRUENAME"
			   "TRUNCATE"
			   "TWO-WAY-STREAM"
			   "TWO-WAY-STREAM-INPUT-STREAM"
			   "TWO-WAY-STREAM-OUTPUT-STREAM"
			   "TYPE"
			   "TYPE-ERROR"
			   "TYPE-ERROR-DATUM"
			   "TYPE-ERROR-EXPECTED-TYPE"
			   "TYPE-OF"
			   "TYPECASE"
			   "TYPEP"
			   "UNBOUND-SLOT"
			   "UNBOUND-SLOT-INSTANCE"
			   "UNBOUND-VARIABLE"
			   "UNDEFINED-FUNCTION"
			   "UNEXPORT"
			   "UNINTERN"
			   "UNION"
			   "UNLESS"
			   "UNREAD-CHAR"
			   "UNSIGNED-BYTE"
			   "UNTRACE"
			   "UNUSE-PACKAGE"
			   "UNWIND-PROTECT"
			   "UPDATE-INSTANCE-FOR-DIFFERENT-CLASS"
			   "UPDATE-INSTANCE-FOR-REDEFINED-CLASS"
			   "UPGRADED-ARRAY-ELEMENT-TYPE"
			   "UPGRADED-COMPLEX-PART-TYPE"
			   "UPPER-CASE-P"
			   "USE-PACKAGE"
			   "USE-VALUE"
			   "USER-HOMEDIR-PATHNAME"
			   "VALUES"
			   "VALUES-LIST"
			   "VARIABLE"
			   "VECTOR"
			   "VECTOR-POP"
			   "VECTOR-PUSH"
			   "VECTOR-PUSH-EXTEND"
			   "VECTORP"
			   "WARN"
			   "WARNING"
			   "WHEN"
			   "WILD-PATHNAME-P"
			   "WITH-ACCESSORS"
			   "WITH-COMPILATION-UNIT"
			   "WITH-CONDITION-RESTARTS"
			   "WITH-HASH-TABLE-ITERATOR"
			   "WITH-INPUT-FROM-STRING"
			   "WITH-OPEN-FILE"
			   "WITH-OPEN-STREAM"
			   "WITH-OUTPUT-TO-STRING"
			   "WITH-PACKAGE-ITERATOR"
			   "WITH-SIMPLE-RESTART"
			   "WITH-SLOTS"
			   "WITH-STANDARD-IO-SYNTAX"
			   "WRITE"
			   "WRITE-BYTE"
			   "WRITE-CHAR"
			   "WRITE-LINE"
			   "WRITE-SEQUENCE"
			   "WRITE-STRING"
			   "WRITE-TO-STRING"
			   "Y-OR-N-P"
			   "YES-OR-NO-P"
			   "ZEROP"
			   ))

(defun tally-cl-symbols ()
  (let ((missing ())
	(found 0))
    (dolist (symbol-name *all-cl-names*)
      (multiple-value-bind (sym kind)
	  (find-symbol symbol-name :cl)
	  (if sym
	      (setq found (1+ found))
	      (push symbol-name missing))))
    (values found missing)))



(defun identify-cl-symbols-for-export-from-package (pkg-name)
  (let* ((pkg (find-package pkg-name))
	 symbols
	 ;; If pkg == :COMMON-LISP then ignore external symbols
	 (ignore-externals (eq pkg (find-package :cl))))
    (dolist (symbol-name *all-cl-names*)
      (multiple-value-bind (symbol status)
	  (find-symbol symbol-name pkg)
	(when (or (eq status :internal)
		  (and (not ignore-externals) (eq status :external)))
	  (push symbol symbols))))
    symbols))




(defun identify-cl-symbols-for-import (pkg-names)
  (let ((pkgs (mapcar #'find-package pkg-names))
	symbols missing)
    (dolist (symbol-name *all-cl-names*)
      (let (found-pkg found-symbol)
	(dolist (pkg pkgs)
	  (multiple-value-bind (symbol status)
	      (find-symbol symbol-name pkg)
	    (when (eq status :external)
	      (if (not found-pkg)
		  (progn
		    (setq found-pkg pkg)
		    (setq found-symbol symbol))
		  (error "The symbol ~a is in more than one package: ~a and ~a"
			 symbol-name found-pkg pkg)))))
	(if found-pkg
	    (push (format nil "~A:~A" (package-name found-pkg) (symbol-name found-symbol)) symbols)
	    (push symbol-name missing))))
    (values symbols missing)))


(defun split-list (list sublist-size)
  (let (result)
    (dotimes (i (/ (length list) sublist-size))
      (let* ((start (* i sublist-size))
	     (stop (* (+ 1 i) sublist-size))
		 (end (if (> stop (length list)) nil stop))
		 (seq (subseq list start end)))
	(push seq result)))
    result))


(defun generate-cl-import (file-name pkg-names)
  (multiple-value-bind (symbols missing)
      (identify-cl-symbols-for-import pkg-names)
    (with-open-file (fout file-name :direction :output :if-exists :supersede)
      (format fout ";; Number of defined COMMON-LISP symbols: ~A~%" (length symbols))
      (dolist (sublist (split-list symbols 5))
	(format fout "(import '~A :cl)~%" sublist))
      (format fout ";; Number of missing COMMON-LISP symbols: ~A~%" (length missing))
      (dolist (sublist (split-list missing 5))
	(format fout ";; Missing: ~A~%" sublist)))))



(defun summarize-cl-coverage (&optional verbose)
  (multiple-value-bind (found missing)
      (identify-cl-symbols-for-import '(:core :clos))
    (bformat t "COMMON-LISP symbols defined: %d    missing: %d%N" (length found) (length missing))
    (if verbose
	(progn
	  (bformat t "Symbols defined: %s%N" found)
	  (bformat t "--------------------------%N")
	  (bformat t "Symbols missing: %s%N" missing))
	(bformat t "Use: (summarize-cl-coverage t) for all symbols%N")))
  (bformat t "--------------------------%N")
  (let ((symbols (identify-cl-symbols-for-export-from-package :core)))
    (bformat t "Symbols missing from :core  --> %s%N" symbols))
  (bformat t "--------------------------%N")
  (let ((symbols (identify-cl-symbols-for-export-from-package :clos)))
    (bformat t "Symbols missing from :clos  --> %s%N" symbols))
  )


(defun present-symbols (pkg)
  (let ((package (find-package pkg))
	syms)
    (do-symbols (sym package)
      (when (eq (symbol-package sym) package)
	(push sym syms))
      )
    syms))

(defun common-symbols (pkg1 pkg2)
  (let ((symbols-pkg1 (present-symbols (find-package pkg1)))
	(symbols-pkg2 (present-symbols (find-package pkg2))))
    (intersection symbols-pkg1 symbols-pkg2)))


(export '(identify-cl-symbols-for-export-from-package))
