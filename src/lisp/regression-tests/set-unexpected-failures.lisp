(in-package #:clasp-tests)

(setq *expected-failures*
      '(
        LOOP-FIXNUM-1 LOOP-FINALLY-1 LOOP-FINALLY-2 LOOP-FINALLY-3 LOOP-FINALLY-4
        random-short random-double random-long
        find-all-symbols.1 read-symbol.19
        test-issue-950
        compile-file-no-unwind
        TYPES-CLASSES-10
        SBCL-CROSS-COMPILE-4 ;;;not important
        INCLUDE-LEVEL-2A INCLUDE-LEVEL-2B INCLUDE-LEVEL-3 ;;; a problem for sbcl x-compiling
        frame-function
  ))
