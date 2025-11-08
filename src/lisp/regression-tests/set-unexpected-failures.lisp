(in-package #:clasp-tests)

(setq *expected-failures*
      '(random-short random-double random-long
        ;; compile-file-no-unwind
        types-classes-10
        sbcl-cross-compile-4 ;;;not important
        ;; include-level-2a
        include-level-2b include-level-3 ;;; a problem for sbcl x-compiling
        frame-function frame-locals))
