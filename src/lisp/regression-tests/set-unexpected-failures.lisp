(in-package #:clasp-tests)

(setq *expected-failures*
      '(ANSI-PUSHNEW.14A 
        ;;; EQUALP-2
        ;;; EQUALP-CLHS-2
        ;;; EQUALP-3 EQUALP-4
        ;;; EQUALP-6 EQUALP-7
        ;;; NREVERSE-1
        ;;; REVERSE-1
        ;;; ACCESSOR-TOO-MANY-ARGS-1
        ;;; HELL-MDBIT-5A
        ;;; LOAD-STREAM.1
        ;;; LISTEN-2
        ;;; BROADCAST-STREAM
        ;;; NEQ-1 NEQ-4 NEQ-5 NEQ-6
        ;;; NEQ-8 EQ-7
        ;;; NEGATE-MOST-NEGATIVE-FIXNUM-1
        ;;; ABS-1
        ;;; ASH-1
        ;;; LOGBITP-1
        ;;; CL-SYMBOLS-1
        ;;; loop this are all fixed using sicl loop
        LOOP-COMPLEX-1
        ;;; LOOP-FLOAT-1 LOOP-FLOAT-2
        LOOP-FIXNUM-1
        LOOP-FINALLY-1 LOOP-FINALLY-2 LOOP-FINALLY-3 LOOP-FINALLY-4
        LOOP-COLLECT-1 LOOP-COLLECT-2 LOOP-COLLECT-3
        LOOP-COLLECT-4 LOOP-COLLECT-5 LOOP-COLLECT-6
        ;;; print-5 
        PRINT-READ-1
        ;;; GENTEMP-1 SPECIAL-OPERATOR-P-1

        ;;; EQUALP-8 EQUALP-9
        ;;; TYPES-CLASSES-9
        TYPES-CLASSES-10
        HASH-TABLE-SIZE-WEAK-KEY
        #-cst BABEL-SIMPLE-STRINGS-1
        ;;; printer
        PRINT.ARRAY.RANDOM.1.TAKE.1
        PRINT.ARRAY.RANDOM.1.TAKE.2
        ;;; PRINT.RATIOS.RANDOM.SIMPLIFIED.FLOATS
        ;;; PRINT.SHORT-FLOAT.RANDOM.SIMPLYFIED
        ;;; PRINT.SINGLE-FLOAT.RANDOM.SIMPLYFIED.1
        ;;; PRINT.SINGLE-FLOAT.RANDOM.SIMPLYFIED.3
        PRINT.VECTOR.RANDOM.1.TAKE.1
        PRINT.VECTOR.RANDOM.1.TAKE.2

        ;;; reader
        ;;; READ-14
        ;;; READ-FROM-STRING-1A
        ;;; READ-FROM-STRING-1C
        ;;; READ-SYMBOL.11
        ;;; READ-SYMBOL.12
        READ-SYMBOL.19
        ;;; READ-SYMBOL.23
        ;;; READ-SYMBOL.23A
        ;;; READ-SYMBOL.23B
        ;;; READ-SYMBOL.LETTERS.LETTERS
        ;;; PRINT.ARRAY.0.11.SIMPLIFIED
        ;;; SYNTAX.SHARP-B.5
        ;;; SYNTAX.SHARP-O.7
        ;;; SYNTAX.SHARP-X.15
        ;;; SYNTAX.SHARP-X.15A
        ;;; READ-RATIO-PRINT-BASE-1
        READ-SUPPRESS.17
        ;;; READ-SUPPRESS.SHARP-ASTERISK.1
        CLHS-23.2.16
        ;;; READ-SUPPRESS.SHARP-ASTERISK.2
        ;;; READ-SUPPRESS.SHARP-ASTERISK.10
        ;;; READ-SUPPRESS.SHARP-ASTERISK.11
        ;;; READ-SUPPRESS.SHARP-ASTERISK.12
        ;;; READ-SUPPRESS.SHARP-ASTERISK.14
        ;;; SYNTAX.SHARP-COLON.ERROR.1
        ;;; SYNTAX.DOT-TOKEN.1
        ;;; SYNTAX.DOT-TOKEN.2
        ;;; SYNTAX.DOT-TOKEN.3
        ;;; SYNTAX.DOT-TOKEN.4
        ;;; SYNTAX.DOT-TOKEN.5
        ;;; SYNTAX.DOT-TOKEN.6
        ;;; SYNTAX.DOT-TOKEN.7
        ;;; SYNTAX.ESCAPED.3.SIMPLYFIED.1

        ;;; unary operands and error checks
        MAX-2A
        MIN-2A
        NUMBER-COMPARISON-REAL-1A
        NUMBER-COMPARISON-REAL-2A
        NUMBER-COMPARISON-REAL-3A
        NUMBER-COMPARISON-REAL-4A
        NUMBER-COMPARISON-REAL-7A
        NUMBER-COMPARISON-REAL-8A
        NUMBER-COMPARISON-REAL-9A
        NUMBER-COMPARISON-REAL-10A
        NUMBER-COMPARISON-NUMBER-11A
        NUMBER-COMPARISON-NUMBER-12A
        NUMBER-COMPARISON-NUMBER-13A
        NUMBER-COMPARISON-NUMBER-14A
        ;;; NUMBER-UNARY-OPERATIONS-1-INLINE
        TEST-CHAR-0A
        TEST-CHAR-1A
        TEST-CHAR-2A TEST-CHAR-3A TEST-CHAR-4A TEST-CHAR-5A
        TEST-CHAR-6A TEST-CHAR-7A TEST-CHAR-8A TEST-CHAR-9A
        TEST-CHAR-10A TEST-CHAR-11A

        ;;; READ-FROM-STRING0
        READ-PRINT-CONSISTENCY-ARRAYS
        ;;; EQUALP-HASH-TABLE-2

        ;;; glsl-toolkit-grammar.lisp-1 errors while compiling in ast
        ;;; make-instance.error.5 the same

        ;;; encoding-latin-2-plain
        ;;; encoding-latin-2-lambda
        ;;; encoding-iso-8859-2-lambda
        SBCL-CROSS-COMPILE-4 ;;;not important
        INCLUDE-LEVEL-2B INCLUDE-LEVEL-3 ;;; a problem for sbcl x-compiling
        )
      )
