(in-package #:clasp-tests)

#|
Functions, Macros, and Special Forms:

documentation (x function) (doc-type (eql 't))
documentation (x function) (doc-type (eql 'function))
documentation (x list) (doc-type (eql 'function))
documentation (x list) (doc-type (eql 'compiler-macro))
documentation (x symbol) (doc-type (eql 'function))
documentation (x symbol) (doc-type (eql 'compiler-macro))
documentation (x symbol) (doc-type (eql 'setf))
(setf documentation) new value (x function) (doc-type (eql 't))
(setf documentation) new value (x function) (doc-type (eql 'function))
(setf documentation) new value (x list) (doc-type (eql 'function))
(setf documentation) new value (x list) (doc-type (eql 'compiler-macro))
(setf documentation) new value (x symbol) (doc-type (eql 'function))
(setf documentation) new value (x symbol) (doc-type (eql 'compiler-macro))
(setf documentation) new value (x symbol) (doc-type (eql 'setf))
Method Combinations:

documentation (x method-combination) (doc-type (eql 't))
documentation (x method-combination) (doc-type (eql 'method-combination))
documentation (x symbol) (doc-type (eql 'method-combination))
(setf documentation) new value (x method-combination) (doc-type (eql 't))
(setf documentation) new value (x method-combination) (doc-type (eql 'method-combination))
(setf documentation) new value (x symbol) (doc-type (eql 'method-combination))
Methods:

documentation (x standard-method) (doc-type (eql 't))
(setf documentation) new value (x standard-method) (doc-type (eql 't))
tes:

documentation (x package) (doc-type (eql 't))
(setf documentation) new value (x package) (doc-type (eql 't))
Types, Classes, and Structure Names:

documentation (x standard-class) (doc-type (eql 't))
documentation (x standard-class) (doc-type (eql 'type))
documentation (x standard-class) (doc-type (eql 't))
documentation (x standard-class) (doc-type (eql 'type))
documentation (x symbol) (doc-type (eql 'type))
documentation (x symbol) (doc-type (eql 'structure))
(setf documentation) new value (x standard-class) (doc-type (eql 't))
(setf documentation) new value (x standard-class) (doc-type (eql 'type))
(setf documentation) new value (x standard-class) (doc-type (eql 't))
(setf documentation) new value (x standard-class) (doc-type (eql 'type))
(setf documentation) new value (x symbol) (doc-type (eql 'type))
(setf documentation) new value (x symbol) (doc-type (eql 'structure))
Variables:

documentation (x symbol) (doc-type (eql 'variable))
(setf documentation) new value (x symbol) (doc-type (eql 'variable))
|#

(defun foo-test-describe (a b c)
  "to test describe"
  (list a b c))

(defun test-documentation-with-args (object &optional (doc-category 'function) requiredp)
  (let ((doc (documentation object doc-category)))
    (cond (doc
           (let ((describe-string
                  (with-output-to-string (*standard-output*)
                    (describe object)))
                 (args (ext:function-lambda-list object)))
             (and (search doc describe-string)
                  (search (write-to-string args :escape t :readably t) describe-string))))
          (requiredp
           nil)
          (t
           (with-output-to-string (*standard-output*)
             (describe object))))))

(defun test-documentation-without-args (object &optional (doc-category 'function))
  (let ((doc (documentation object doc-category)))
    (if doc
         (let ((describe-string 
                (with-output-to-string (*standard-output*)
                  (describe object))))
           (search doc describe-string))
         (with-output-to-string (*standard-output*)
                  (describe object)))))

(test-true describe-function-documentation
           (test-documentation-with-args 'foo-test-describe))

(test-true describe-function-documentation-a
           (test-documentation-with-args 'ext:function-lambda-list))

(test-true describe-function-documentation-macro
           (test-documentation-with-args 'cl:defconstant))

(test-true describe-function-documentation-function-docstring
           (test-documentation-with-args 'core:function-docstring 'function t))

(test-true describe-function-documentation-setf-function-docstring
           (test-documentation-with-args 'core:function-docstring 'setf t))

(test-true describe-function-documentation-car
           (test-documentation-with-args 'cl:car 'function t))

(test-true describe-function-documentation-setf-car
           (test-documentation-with-args 'cl:car 'setf t))

(defun stupid-function (a) a)

(define-compiler-macro stupid-function (&whole form arg)
  "This is an empty compiler macro"
  (declare (ignore form))
  `(list ,arg))

(test-true describe-compiler-macro
           (test-documentation-without-args 'stupid-function 'compiler-macro))

(test-true describe-special-form (test-documentation-with-args 'progn))

(defconstant a-silly-constant 23 "and the constant doc")

(test-true describe-constant
           (test-documentation-without-args 'a-silly-constant 'variable))

(defparameter *a-silly-var* 23 "and the special doc")

(test-true describe-special
           (test-documentation-without-args '*a-silly-var* 'variable))

(deftype a-silly-type ()
  "Documentation of a type"
  'fixnum)

(test-true describe-type (test-documentation-without-args 'a-silly-type 'type))

(defstruct silly-struct-normal "Documentation for a normal struct" a (b 23) (c "nada" :type string)(d 25 :read-only t))
(defstruct (silly-struct-vector (:type vector)) "Documentation for a vector struct" a b c)
(defstruct (silly-struct-list (:type vector)) "Documentation for a list struct" a b c)

(test-true describe-struct-a (test-documentation-without-args 'silly-struct-normal 'structure))

(test-true describe-struct-b (test-documentation-without-args 'silly-struct-vector 'structure))

(test-true describe-struct-c (test-documentation-without-args  'silly-struct-list 'structure))

(defun (setf nada) (new old)
  "asdjhaskdhj"
  (list new old))

(test-true simple-setf (documentation '(setf nada) 'function))

(defun setf-access-fn (value) "a fn doc" value)
(defun setf-update-fn (old new)(list old new))
(defsetf setf-access-fn setf-update-fn "A short detsetf documentation")

(test-true describe-setf-short (test-documentation-without-args 'setf-access-fn 'setf))

(defun setf-access-fn-for-long-setf (value) "a fn doc" value)

(defsetf setf-access-fn-for-long-setf (sequence start &optional end) (new-sequence)
  "A long form of setf"
   `(progn (replace ,sequence ,new-sequence
                    :start1 ,start :end1 ,end)
           ,new-sequence))

(test-true describe-setf-long (test-documentation-without-args 'setf-access-fn-for-long-setf 'setf))

(defclass describe-class ()
  ((foo :initform 23 :initarg :foo :accessor describe-class))
  (:documentation "A doc for a class"))

(test-true describe-class (test-documentation-without-args (find-class 'describe-class) t))

(test-true describe-instance
      (let* ((object (make-instance 'describe-class)))
        (with-output-to-string (*standard-output*)
          (describe object))))

(defpackage nonsense-for-describe
  (:use :cl)
  (:documentation "A package doc")
  (:nicknames :a-package-nickname)
  (:shadow #:car)
  (:local-nicknames (:mine :cl-user)))

(defpackage nonsense-for-describe-uses
  (:use :cl :nonsense-for-describe))

(test-true describe-package (test-documentation-without-args (find-package :nonsense-for-describe) t))

(defgeneric blah-blah (a b c)
  (:documentation "A generic function documentation")
  (:method ((a describe-class) (b string) c)
    "A method documentation"
    (list a b c)))

(test-true describe-generic-function (test-documentation-without-args #'blah-blah  'function))

(test-true describe-method-via-generic-function
      (test-documentation-without-args
       (find-method #'blah-blah nil (list (find-class 'describe-class)(find-class 'string)(find-class t)))
       'function))

#|
(define-method-combination test1 :identity-with-one-argument t) 

(describe (CLOS::SEARCH-METHOD-COMBINATION 'test1))
|#

(test-true describe-alltogether
      (with-output-to-string (*standard-output*)
        (let ((objects-to-describe
               (list
                'foo-test-describe
                'ext:function-lambda-list
                'cl:defconstant
                'stupid-function
                'progn
                'a-silly-constant
                '*a-silly-var*
                'a-silly-type
                'silly-struct-normal 
                'silly-struct-vector 
                'silly-struct-list
                '(setf nada)
                'setf-access-fn
                'setf-access-fn-for-long-setf
                (find-class 'describe-class)
                (make-instance 'describe-class)
                (find-package :nonsense-for-describe)
                #'blah-blah
                (find-method #'blah-blah nil (list (find-class 'describe-class)(find-class 'string)(find-class t)))
                #\newline
                (code-char 256)
                "234987234iuziuz"
                most-positive-fixnum
                (1+ most-positive-fixnum)
                pi
                (/ 3 23)
                (complex .4 .5)
                (vector 1 2 3)
                (make-array (list 2 2) :initial-contents '((1 2)(3 4)))
                (make-hash-table))))
          (dolist (object objects-to-describe)
            (describe object)))))

(test issue-1099
      (let ((cl-function-list '(* +
                          -
                          /
                          /=
                          1+
                          1-
                          <
                          <=
                          =
                          >
                          >=
                          ABORT
                          ABS
                          ACONS
                          ACOS
                          ACOSH
                          ADJOIN
                          ADJUST-ARRAY
                          ADJUSTABLE-ARRAY-P
                          ALPHA-CHAR-P
                          ALPHANUMERICP
                          APPEND
                          APPLY
                          APROPOS
                          APROPOS-LIST
                          ARITHMETIC-ERROR-OPERANDS
                          ARITHMETIC-ERROR-OPERATION
                          ARRAY-DIMENSION
                          ARRAY-DIMENSIONS
                          ARRAY-DISPLACEMENT
                          ARRAY-ELEMENT-TYPE
                          ARRAY-HAS-FILL-POINTER-P
                          ARRAY-IN-BOUNDS-P
                          ARRAY-RANK
                          ARRAY-ROW-MAJOR-INDEX
                          ARRAY-TOTAL-SIZE
                          ARRAYP
                          ASH
                          ASIN
                          ASINH
                          ASSOC-IF-NOT
                          ASSOC
                          ASSOC-IF
                          ATAN
                          ATANH
                          ATOM
                          BIT-AND
                          BIT-ANDC1
                          BIT-ANDC2
                          BIT-EQV
                          BIT-IOR
                          BIT-NAND
                          BIT-NOR
                          BIT-NOT
                          BIT-ORC1
                          BIT-ORC2
                          BIT-VECTOR-P
                          BIT-XOR
                          BOOLE
                          BOTH-CASE-P
                          BOUNDP
                          BREAK
                          BROADCAST-STREAM-STREAMS
                          BUTLAST
                          BYTE
                          BYTE-POSITION
                          BYTE-SIZE
                          CEILING
                          CELL-ERROR-NAME
                          CERROR
                          CHAR-CODE
                          CHAR-DOWNCASE
                          CHAR-EQUAL
                          CHAR-GREATERP
                          CHAR-INT
                          CHAR-LESSP
                          CHAR-NAME
                          CHAR-NOT-EQUAL
                          CHAR-NOT-GREATERP
                          CHAR-NOT-LESSP
                          CHAR-UPCASE
                          CHAR/=
                          CHAR<
                          CHAR<=
                          CHAR=
                          CHAR>
                          CHAR>=
                          CHARACTER
                          CHARACTERP
                          CIS
                          CLASS-OF
                          CLEAR-INPUT
                          CLEAR-OUTPUT
                          CLOSE
                          CLRHASH
                          CODE-CHAR
                          COERCE
                          COMPILE
                          COMPILE-FILE
                          COMPILE-FILE-PATHNAME
                          COMPILED-FUNCTION-P
                          COMPLEMENT
                          COMPLEX
                          COMPLEXP
                          COMPUTE-RESTARTS
                          CONCATENATE
                          CONCATENATED-STREAM-STREAMS
                          CONJUGATE
                          CONS
                          CONSP
                          CONSTANTLY
                          CONSTANTP
                          CONTINUE
                          COPY-ALIST
                          COPY-LIST
                          COPY-PPRINT-DISPATCH
                          COPY-READTABLE
                          COPY-SEQ
                          COPY-STRUCTURE
                          COPY-SYMBOL
                          COPY-TREE
                          COS
                          COSH
                          COUNT
                          COUNT-IF
                          COUNT-IF-NOT
                          DECODE-FLOAT
                          DECODE-UNIVERSAL-TIME
                          DELETE
                          DELETE-DUPLICATES
                          DELETE-FILE
                          DELETE-IF
                          DELETE-IF-NOT
                          DELETE-PACKAGE
                          DENOMINATOR
                          DEPOSIT-FIELD
                          DESCRIBE
                          DIGIT-CHAR
                          DIGIT-CHAR-P
                          DIRECTORY
                          DIRECTORY-NAMESTRING
                          DISASSEMBLE
                          DPB
                          DRIBBLE
                          ECHO-STREAM-INPUT-STREAM
                          ECHO-STREAM-OUTPUT-STREAM
                          ENCODE-UNIVERSAL-TIME
                          ENDP
                          ENOUGH-NAMESTRING
                          ENSURE-DIRECTORIES-EXIST
                          ENSURE-GENERIC-FUNCTION
                          EQ
                          EQL
                          EQUAL
                          EQUALP
                          ERROR
                          EVAL
                          EVENP
                          EVERY
                          EXP
                          EXPORT
                          EXPT
                          FBOUNDP
                          FCEILING
                          FFLOOR
                          FILE-AUTHOR
                          FILE-ERROR-PATHNAME
                          FILE-LENGTH
                          FILE-NAMESTRING
                          FILE-POSITION
                          FILE-STRING-LENGTH
                          FILE-WRITE-DATE
                          FILL
                          FIND
                          FIND-ALL-SYMBOLS
                          FIND-IF
                          FIND-IF-NOT
                          FIND-PACKAGE
                          FIND-RESTART
                          FIND-SYMBOL
                          FINISH-OUTPUT
                          FLOAT
                          FLOAT-DIGITS
                          FLOAT-PRECISION
                          FLOAT-RADIX
                          FLOAT-SIGN
                          FLOATP
                          FLOOR
                          FMAKUNBOUND
                          FORCE-OUTPUT
                          FORMAT
                          FRESH-LINE
                          FROUND
                          FUNCALL
                          FUNCTION-LAMBDA-EXPRESSION
                          FUNCTIONP
                          GCD
                          GENSYM
                          GENTEMP
                          GET-DECODED-TIME
                          GET-DISPATCH-MACRO-CHARACTER
                          GET-INTERNAL-REAL-TIME
                          GET-INTERNAL-RUN-TIME
                          GET-MACRO-CHARACTER
                          GET-OUTPUT-STREAM-STRING
                          GET-PROPERTIES
                          GET-SETF-EXPANSION
                          GET-UNIVERSAL-TIME
                          GRAPHIC-CHAR-P
                          HASH-TABLE-COUNT
                          HASH-TABLE-P
                          HASH-TABLE-REHASH-SIZE
                          HASH-TABLE-REHASH-THRESHOLD
                          HASH-TABLE-SIZE
                          HASH-TABLE-TEST
                          HOST-NAMESTRING
                          IDENTITY
                          IMAGPART
                          IMPORT
                          INPUT-STREAM-P
                          INSPECT
                          INTEGER-DECODE-FLOAT
                          INTEGER-LENGTH
                          INTEGERP
                          INTERACTIVE-STREAM-P
                          INTERN
                          INTERSECTION
                          INVALID-METHOD-ERROR
                          INVOKE-DEBUGGER
                          INVOKE-RESTART
                          INVOKE-RESTART-INTERACTIVELY
                          ISQRT
                          KEYWORDP
                          LAST
                          LCM
                          LDB-TEST
                          LDIFF
                          LENGTH
                          LISP-IMPLEMENTATION-TYPE
                          LISP-IMPLEMENTATION-VERSION
                          LIST
                          LIST*
                          LIST-ALL-PACKAGES
                          LIST-LENGTH
                          LISTEN
                          LISTP
                          LOAD
                          LOAD-LOGICAL-PATHNAME-TRANSLATIONS
                          LOG
                          LOGAND
                          LOGANDC1
                          LOGANDC2
                          LOGBITP
                          LOGCOUNT
                          LOGEQV
                          LOGICAL-PATHNAME
                          LOGIOR
                          LOGNAND
                          LOGNOR
                          LOGNOT
                          LOGORC1
                          LOGORC2
                          LOGTEST
                          LOGXOR
                          LONG-SITE-NAME
                          LOWER-CASE-P
                          MACHINE-INSTANCE
                          MACHINE-TYPE
                          MACHINE-VERSION
                          MACROEXPAND
                          MACROEXPAND-1
                          MAKE-ARRAY
                          MAKE-BROADCAST-STREAM
                          MAKE-CONCATENATED-STREAM
                          MAKE-CONDITION
                          MAKE-DISPATCH-MACRO-CHARACTER
                          MAKE-ECHO-STREAM
                          MAKE-HASH-TABLE
                          MAKE-LIST
                          MAKE-LOAD-FORM-SAVING-SLOTS
                          MAKE-PACKAGE
                          MAKE-PATHNAME
                          MAKE-RANDOM-STATE
                          MAKE-SEQUENCE
                          MAKE-STRING
                          MAKE-STRING-INPUT-STREAM
                          MAKE-STRING-OUTPUT-STREAM
                          MAKE-SYMBOL
                          MAKE-SYNONYM-STREAM
                          MAKE-TWO-WAY-STREAM
                          MAKUNBOUND
                          MAP
                          MAP-INTO
                          MAPC
                          MAPCAN
                          MAPCAR
                          MAPCON
                          MAPHASH
                          MAPL
                          MAPLIST
                          MAX
                          MEMBER
                          MEMBER-IF
                          MEMBER-IF-NOT
                          MERGE
                          MERGE-PATHNAMES
                          METHOD-COMBINATION-ERROR
                          MIN
                          MINUSP
                          MISMATCH
                          MOD
                          MUFFLE-WARNING
                          NAME-CHAR
                          NAMESTRING
                          NBUTLAST
                          NCONC
                          NINTERSECTION
                          NOT
                          NOTANY
                          NOTEVERY
                          NRECONC
                          NREVERSE
                          NSET-DIFFERENCE
                          NSET-EXCLUSIVE-OR
                          NSTRING-CAPITALIZE
                          NSTRING-DOWNCASE
                          NSTRING-UPCASE
                          NSUBLIS
                          NSUBST
                          NSUBST-IF
                          NSUBST-IF-NOT
                          NSUBSTITUTE
                          NSUBSTITUTE-IF
                          NSUBSTITUTE-IF-NOT
                          NTHCDR
                          NULL
                          NUMBERP
                          NUMERATOR
                          NUNION
                          ODDP
                          OPEN
                          OPEN-STREAM-P
                          OUTPUT-STREAM-P
                          PACKAGE-ERROR-PACKAGE
                          PACKAGE-NAME
                          PACKAGE-NICKNAMES
                          PACKAGE-SHADOWING-SYMBOLS
                          PACKAGE-USE-LIST
                          PACKAGE-USED-BY-LIST
                          PACKAGEP
                          PAIRLIS
                          PARSE-INTEGER
                          PARSE-NAMESTRING
                          PATHNAME
                          PATHNAME-DEVICE
                          PATHNAME-DIRECTORY
                          PATHNAME-HOST
                          PATHNAME-MATCH-P
                          PATHNAME-NAME
                          PATHNAME-TYPE
                          PATHNAME-VERSION
                          PATHNAMEP
                          PEEK-CHAR
                          PHASE
                          PLUSP
                          POSITION
                          POSITION-IF
                          POSITION-IF-NOT
                          PPRINT
                          PPRINT-DISPATCH
                          PPRINT-FILL
                          PPRINT-INDENT
                          PPRINT-LINEAR
                          PPRINT-NEWLINE
                          PPRINT-TAB
                          PPRINT-TABULAR
                          PRIN1
                          PRIN1-TO-STRING
                          PRINC
                          PRINC-TO-STRING
                          PRINT
                          PRINT-NOT-READABLE-OBJECT
                          PROBE-FILE
                          PROCLAIM
                          PROVIDE
                          RANDOM
                          RANDOM-STATE-P
                          RASSOC
                          RASSOC-IF
                          RASSOC-IF-NOT
                          RATIONAL
                          RATIONALIZE
                          RATIONALP
                          READ
                          READ-BYTE
                          READ-CHAR
                          READ-CHAR-NO-HANG
                          READ-DELIMITED-LIST
                          READ-FROM-STRING
                          READ-LINE
                          READ-PRESERVING-WHITESPACE
                          READ-SEQUENCE
                          READTABLEP
                          REALP
                          REALPART
                          REDUCE
                          REM
                          REMHASH
                          REMOVE
                          REMOVE-DUPLICATES
                          REMOVE-IF
                          REMOVE-IF-NOT
                          REMPROP
                          RENAME-FILE
                          RENAME-PACKAGE
                          REPLACE
                          REQUIRE
                          RESTART-NAME
                          REVAPPEND
                          REVERSE
                          ROOM
                          ROUND
                          RPLACA
                          RPLACD
                          SCALE-FLOAT
                          SEARCH
                          SET
                          SET-DIFFERENCE
                          SET-DISPATCH-MACRO-CHARACTER
                          SET-EXCLUSIVE-OR
                          SET-MACRO-CHARACTER
                          SET-PPRINT-DISPATCH
                          SET-SYNTAX-FROM-CHAR
                          SHADOW
                          SHADOWING-IMPORT
                          SHORT-SITE-NAME
                          SIGNAL
                          SIGNUM
                          SIMPLE-BIT-VECTOR-P
                          SIMPLE-CONDITION-FORMAT-ARGUMENTS
                          SIMPLE-CONDITION-FORMAT-CONTROL
                          SIMPLE-STRING-P
                          SIMPLE-VECTOR-P
                          SIN
                          SINH
                          SLOT-EXISTS-P
                          SLEEP
                          SLOT-BOUNDP
                          SLOT-MAKUNBOUND
                          SLOT-VALUE
                          SOFTWARE-TYPE
                          SOFTWARE-VERSION
                          SOME
                          SORT
                          SPECIAL-OPERATOR-P
                          SQRT
                          STABLE-SORT
                          STANDARD-CHAR-P
                          STORE-VALUE
                          STREAM-ELEMENT-TYPE
                          STREAM-ERROR-STREAM
                          STREAM-EXTERNAL-FORMAT
                          STREAMP
                          STRING
                          STRING-CAPITALIZE
                          STRING-DOWNCASE
                          STRING-EQUAL
                          STRING-GREATERP
                          STRING-LEFT-TRIM
                          STRING-LESSP
                          STRING-NOT-EQUAL
                          STRING-NOT-GREATERP
                          STRING-NOT-LESSP
                          STRING-RIGHT-TRIM
                          STRING-TRIM
                          STRING-UPCASE
                          STRING/=
                          STRING<
                          STRING<=
                          STRING=
                          STRING>
                          STRING>=
                          STRINGP
                          SUBLIS
                          SUBSETP
                          SUBST
                          SUBST-IF
                          SUBST-IF-NOT
                          SUBSTITUTE
                          SUBSTITUTE-IF
                          SUBSTITUTE-IF-NOT
                          SUBTYPEP
                          SXHASH
                          SYMBOL-NAME
                          SYMBOL-PACKAGE
                          SYMBOLP
                          SYNONYM-STREAM-SYMBOL
                          TAILP
                          TAN
                          TANH
                          TERPRI
                          TRANSLATE-LOGICAL-PATHNAME
                          TRANSLATE-PATHNAME
                          TREE-EQUAL
                          TRUENAME
                          TRUNCATE
                          FTRUNCATE
                          TWO-WAY-STREAM-INPUT-STREAM
                          TWO-WAY-STREAM-OUTPUT-STREAM
                          TYPE-ERROR-DATUM
                          TYPE-ERROR-EXPECTED-TYPE
                          TYPE-OF
                          TYPEP
                          UNBOUND-SLOT-INSTANCE
                          UNEXPORT
                          UNINTERN
                          UNION
                          UNREAD-CHAR
                          UNUSE-PACKAGE
                          UPGRADED-ARRAY-ELEMENT-TYPE
                          UPGRADED-COMPLEX-PART-TYPE
                          UPPER-CASE-P
                          USE-PACKAGE
                          USE-VALUE
                          USER-HOMEDIR-PATHNAME
                          VALUES-LIST
                          VECTOR
                          VECTOR-POP
                          VECTOR-PUSH
                          VECTOR-PUSH-EXTEND
                          VECTORP
                          WARN
                          WILD-PATHNAME-P
                          WRITE
                          WRITE-BYTE
                          WRITE-CHAR
                          WRITE-LINE
                          WRITE-SEQUENCE
                          WRITE-STRING
                          WRITE-TO-STRING
                          Y-OR-N-P
                          YES-OR-NO-P
                          ZEROP)))
        (LOOP FOR S IN cl-function-list
              FOR FN = (SYMBOL-FUNCTION S)
              FOR DOC = (DOCUMENTATION FN T)
              UNLESS (OR (NULL DOC) (STRING DOC))
                COLLECT (LIST S DOC)))
      (nil))
