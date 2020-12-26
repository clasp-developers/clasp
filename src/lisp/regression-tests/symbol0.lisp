(in-package #:clasp-tests)

(TEST-EXPECT-ERROR TEST-MAKE-SYMBOL-ERROR-0 (MAKE-SYMBOL 23) :TYPE TYPE-ERROR)
(TEST-EXPECT-ERROR TEST-MAKE-SYMBOL-ERROR-1 (MAKE-SYMBOL 'DEFUN) :TYPE
 TYPE-ERROR)
(TEST-EXPECT-ERROR TEST-MAKE-SYMBOL-ERROR-2 (MAKE-SYMBOL #\A) :TYPE TYPE-ERROR)
(TEST TEST-MAKE-SYMBOL-0
 (SYMBOLP (HANDLER-CASE (MAKE-SYMBOL "ABCCC") (ERROR (E) E))))
(TEST TEST-MAKE-SYMBOL-1
 (SYMBOLP
  (HANDLER-CASE
   (MAKE-SYMBOL
    (MAKE-ARRAY '(6)
                :INITIAL-CONTENTS
                '(#\A #\B #\C #\D #\E #\F)
                :ELEMENT-TYPE
                'CHARACTER
                :FILL-POINTER
                4))
    (ERROR (E) E))))

(test-expect-error makunbound-1
                   (let ((foo 23))
                     (makunbound foo))
                   :type type-error)
                   
(test-expect-error gensym-2
                   (gensym -1)
                   :type type-error)

(test-expect-error gensym-3
                   (gensym (1- most-negative-fixnum))
                   :type type-error)

(test-expect-error gensym-4
                   (gensym 'defun)
                   :type type-error)

(test gensym-5 (gensym (+ most-positive-fixnum  most-positive-fixnum)))

(test gensym-6
      (let ((bignum 12345678901234567890123456789012345678901234567890))
        (= (1+ bignum)
           (LET ((*GENSYM-COUNTER* bignum))
             (GENSYM)
             *GENSYM-COUNTER*))))

(test gensym-7
      (= (1+ most-positive-fixnum)
         (let ((*gensym-counter* most-positive-fixnum))
           (gensym)
           *gensym-counter*)))

(test-expect-error gensym-8
                   (let ((*gensym-counter* -1))
                     (gensym))
                    :type type-error)

(test-expect-error gensym-9
                   (let ((*gensym-counter* (1- most-negative-fixnum)))
                     (gensym))
                   :type type-error)

(test-expect-error gensym-10
                   (let ((*gensym-counter* 'defun))
                     (gensym))
                    :type type-error)
      

(test-expect-error gentemp-1
                   (gentemp nil)
                   :type type-error)

(test build-sbcl-1 (setf (symbol-plist nil)(list 1 2)))
(test build-sbcl-2 (setf (get nil 1) 2))

(test 978-symbols-common-lisp-exported
      (let ((sum 0))
        (do-external-symbols (sym (find-package :cl))
          (declare (ignore sym))
          (incf sum))
        (= 978 sum)))

(test
 fboundp.8
 (let ((cl-non-function-macro-special-operator-symbols
         '(&ALLOW-OTHER-KEYS &AUX &BODY &ENVIRONMENT &KEY &OPTIONAL &REST &WHOLE ** ***
           *BREAK-ON-SIGNALS* *COMPILE-FILE-PATHNAME* *COMPILE-FILE-TRUENAME*
           *COMPILE-PRINT* *COMPILE-VERBOSE* *DEBUG-IO* *DEBUGGER-HOOK*
           *DEFAULT-PATHNAME-DEFAULTS* *ERROR-OUTPUT* *FEATURES* *GENSYM-COUNTER*
           *LOAD-PATHNAME* *LOAD-PRINT* *LOAD-TRUENAME* *LOAD-VERBOSE* *MACROEXPAND-HOOK*
           *MODULES* *PACKAGE* *PRINT-ARRAY* *PRINT-BASE* *PRINT-CASE* *PRINT-CIRCLE*
           *PRINT-ESCAPE* *PRINT-GENSYM* *PRINT-LENGTH* *PRINT-LEVEL* *PRINT-LINES*
           *PRINT-MISER-WIDTH* *PRINT-PPRINT-DISPATCH* *PRINT-PRETTY* *PRINT-RADIX*
           *PRINT-READABLY* *PRINT-RIGHT-MARGIN* *QUERY-IO* *RANDOM-STATE* *READ-BASE*
           *READ-DEFAULT-FLOAT-FORMAT* *READ-EVAL* *READ-SUPPRESS* *READTABLE*
           *STANDARD-INPUT* *STANDARD-OUTPUT* *TERMINAL-IO* *TRACE-OUTPUT* ++ +++ // ///
           ARITHMETIC-ERROR ARRAY ARRAY-DIMENSION-LIMIT ARRAY-RANK-LIMIT
           ARRAY-TOTAL-SIZE-LIMIT BASE-CHAR BASE-STRING BIGNUM BIT-VECTOR BOOLE-1 BOOLE-2
           BOOLE-AND BOOLE-ANDC1 BOOLE-ANDC2 BOOLE-C1 BOOLE-C2 BOOLE-CLR BOOLE-EQV
           BOOLE-IOR BOOLE-NAND BOOLE-NOR BOOLE-ORC1 BOOLE-ORC2 BOOLE-SET BOOLE-XOR
           BOOLEAN BROADCAST-STREAM BUILT-IN-CLASS CALL-ARGUMENTS-LIMIT CELL-ERROR
           CHAR-CODE-LIMIT CLASS COMPILATION-SPEED COMPILED-FUNCTION COMPILER-MACRO
           CONCATENATED-STREAM CONDITION CONTROL-ERROR DEBUG DECLARATION DIVISION-BY-ZERO
           DOUBLE-FLOAT DOUBLE-FLOAT-EPSILON DOUBLE-FLOAT-NEGATIVE-EPSILON DYNAMIC-EXTENT
           ECHO-STREAM END-OF-FILE EXTENDED-CHAR FILE-ERROR FILE-STREAM FIXNUM
           FLOATING-POINT-INEXACT FLOATING-POINT-INVALID-OPERATION
           FLOATING-POINT-OVERFLOW FLOATING-POINT-UNDERFLOW FTYPE GENERIC-FUNCTION
           HASH-TABLE IGNORABLE IGNORE INLINE INTEGER INTERNAL-TIME-UNITS-PER-SECOND
           KEYWORD LAMBDA-LIST-KEYWORDS LAMBDA-PARAMETERS-LIMIT
           LEAST-NEGATIVE-DOUBLE-FLOAT LEAST-NEGATIVE-LONG-FLOAT
           LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT
           LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT
           LEAST-NEGATIVE-SHORT-FLOAT LEAST-NEGATIVE-SINGLE-FLOAT
           LEAST-POSITIVE-DOUBLE-FLOAT LEAST-POSITIVE-LONG-FLOAT
           LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT LEAST-POSITIVE-NORMALIZED-LONG-FLOAT
           LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT
           LEAST-POSITIVE-SHORT-FLOAT LEAST-POSITIVE-SINGLE-FLOAT LONG-FLOAT
           LONG-FLOAT-EPSILON LONG-FLOAT-NEGATIVE-EPSILON METHOD METHOD-COMBINATION
           MOST-NEGATIVE-DOUBLE-FLOAT MOST-NEGATIVE-FIXNUM MOST-NEGATIVE-LONG-FLOAT
           MOST-NEGATIVE-SHORT-FLOAT MOST-NEGATIVE-SINGLE-FLOAT
           MOST-POSITIVE-DOUBLE-FLOAT MOST-POSITIVE-FIXNUM MOST-POSITIVE-LONG-FLOAT
           MOST-POSITIVE-SHORT-FLOAT MOST-POSITIVE-SINGLE-FLOAT MULTIPLE-VALUES-LIMIT NIL
           NOTINLINE NUMBER OPTIMIZE OTHERWISE PACKAGE PACKAGE-ERROR PARSE-ERROR PI
           PRINT-NOT-READABLE PROGRAM-ERROR RANDOM-STATE RATIO READER-ERROR READTABLE
           REAL RESTART SAFETY SATISFIES SEQUENCE SERIOUS-CONDITION SHORT-FLOAT
           SHORT-FLOAT-EPSILON SHORT-FLOAT-NEGATIVE-EPSILON SIGNED-BYTE SIMPLE-ARRAY
           SIMPLE-BASE-STRING SIMPLE-BIT-VECTOR SIMPLE-CONDITION SIMPLE-ERROR
           SIMPLE-STRING SIMPLE-TYPE-ERROR SIMPLE-VECTOR SIMPLE-WARNING SINGLE-FLOAT
           SINGLE-FLOAT-EPSILON SINGLE-FLOAT-NEGATIVE-EPSILON SPACE SPECIAL SPEED
           STANDARD STANDARD-CHAR STANDARD-CLASS STANDARD-GENERIC-FUNCTION
           STANDARD-METHOD STANDARD-OBJECT STORAGE-CONDITION STREAM STREAM-ERROR
           STRING-STREAM STRUCTURE STRUCTURE-CLASS STRUCTURE-OBJECT STYLE-WARNING SYMBOL
           SYNONYM-STREAM T TWO-WAY-STREAM TYPE TYPE-ERROR UNBOUND-SLOT UNBOUND-VARIABLE
           UNDEFINED-FUNCTION UNSIGNED-BYTE VARIABLE WARNING)))     
      (null
       (loop for x in cl-non-function-macro-special-operator-symbols
        when (fboundp x)
        collect x))))

