(in-package #:clasp-tests)

(setq *expected-failures*
      '(
        LOOP-FIXNUM-1 LOOP-FINALLY-1 LOOP-FINALLY-2 LOOP-FINALLY-3 LOOP-FINALLY-4
        TYPES-CLASSES-10
        READ-SUPPRESS.17
        CLHS-23.2.16
        SBCL-CROSS-COMPILE-4 ;;;not important
        INCLUDE-LEVEL-2A INCLUDE-LEVEL-2B INCLUDE-LEVEL-3 ;;; a problem for sbcl x-compiling
  ))
