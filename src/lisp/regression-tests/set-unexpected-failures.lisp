(in-package #:clasp-tests)

(setq *expected-failures*
      '(ANSI-PUSHNEW.14A 
        EQUALP-2
        ;;; EQUALP-CLHS-2
        EQUALP-3 EQUALP-4
        ;;; EQUALP-6 EQUALP-7
        ;;; NREVERSE-1
        REVERSE-1
        ;;; ACCESSOR-TOO-MANY-ARGS-1
        HELL-MDBIT-5A
        ;;; LOAD-STREAM.1
        ;;; LISTEN-2
        ;;; BROADCAST-STREAM
        NEQ-1 NEQ-4 NEQ-5 NEQ-6
        ;;; NEQ-8 EQ-7
        ;;; NEGATE-MOST-NEGATIVE-FIXNUM-1
        ;;; ABS-1
        ;;; ASH-1
        ;;; LOGBITP-1
        ;;; CL-SYMBOLS-1
        ;;; loop this are all fixed using sicl loop
        LOOP-COMPLEX-1 LOOP-FLOAT-1 LOOP-FLOAT-2 LOOP-FIXNUM-1
        LOOP-FINALLY-1 LOOP-FINALLY-2 LOOP-FINALLY-3 LOOP-FINALLY-4
        LOOP-COLLECT-1 LOOP-COLLECT-2 LOOP-COLLECT-3
        LOOP-COLLECT-4 LOOP-COLLECT-5 LOOP-COLLECT-6
        ;;; print-5 
        PRINT-READ-1
        )
      )
