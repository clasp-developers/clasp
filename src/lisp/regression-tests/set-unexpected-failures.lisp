(in-package #:clasp-tests)

(setq *expected-failures*
      '(random-short random-double random-long
        ;; compile-file-no-unwind
        types-classes-10
        sbcl-cross-compile-4 ;;;not important
        ;; include-level-2a
        include-level-2b include-level-3 ;;; a problem for sbcl x-compiling
        frame-function frame-locals
        ;; these don't work well on boehm. In particular
        ;; key-or-value tables are effectively strong.
        #+use-boehm weak-key-and-value-weakness
        #+use-boehm weak-key-or-value-weakness))
