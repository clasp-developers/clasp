(in-package #:clasp-tests)

(setq *expected-failures*
      '(ANSI-PUSHNEW.14A 
        LOOP-FIXNUM-1 LOOP-FINALLY-1 LOOP-FINALLY-2 LOOP-FINALLY-3 LOOP-FINALLY-4
        PRINT-READ-1
        TYPES-CLASSES-10
        HASH-TABLE-SIZE-WEAK-KEY
        #-cst BABEL-SIMPLE-STRINGS-1
        ;;; printer
        PRINT.ARRAY.RANDOM.1.TAKE.1
        PRINT.ARRAY.RANDOM.1.TAKE.2
        PRINT.VECTOR.RANDOM.1.TAKE.1
        PRINT.VECTOR.RANDOM.1.TAKE.2

        READ-SYMBOL.19
        READ-SUPPRESS.17
        CLHS-23.2.16
        
        READ-PRINT-CONSISTENCY-ARRAYS
        SBCL-CROSS-COMPILE-4 ;;;not important
        INCLUDE-LEVEL-2A INCLUDE-LEVEL-2B INCLUDE-LEVEL-3 ;;; a problem for sbcl x-compiling
        TEST-ISSUE-946 TEST-ISSUE-950 
        )
      )
