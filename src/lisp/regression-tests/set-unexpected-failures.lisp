(in-package #:clasp-tests)

(setq *expected-failures*
      '(ANSI-PUSHNEW.14 ;;; this is debatable, will ask jackdaniel to verify, I consider this undefined behaviour
        EQUALP-2 EQUALP-CLHS-2 EQUALP-3 EQUALP-4 EQUALP-6 EQUALP-7
        NREVERSE-1 REVERSE-1
        ACCESSOR-TOO-MANY-ARGS-1
        HELL-MDBIT-5A
        LOAD-STREAM.1
        ))
