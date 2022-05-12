(in-package #:clasp-tests)

(setq *expected-failures*
      '(loop-fixnum-1 loop-finally-1 loop-finally-2 loop-finally-3 loop-finally-4
        random-short random-double random-long
        test-issue-950
        compile-file-no-unwind
        types-classes-10
        sbcl-cross-compile-4 ;;;not important
        include-level-2a include-level-2b include-level-3 ;;; a problem for sbcl x-compiling
        frame-function))
