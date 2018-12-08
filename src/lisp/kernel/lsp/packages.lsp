;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Export symbols in CORE package
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (core:select-package "CORE"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '( single-dispatch-missing-dispatch-argument-error
            too-few-arguments-error
            too-many-arguments-error
            unrecognized-keyword-argument-error
            argc
            argv
            rmdir
            external-process-pid
            select-source-files
            compile-kernel-file
            *target-backend*
            target-backend-pathname
            default-target-backend
            load-system
            compile-system
            maybe-load-clasprc
            process-command-line-load-eval-sequence
            top-level
            run-repl
            *defun-inline-hook*
            *proclaim-hook*
            proper-list-p
            expand-associative
            expand-compare
            expand-uncompare
            with-memory-ramp
            with-dtrace-trigger
            ))

  (export '( ;;;;MISSING SYMBOLS!!!!!
            fill-pointer-set ;;; MISSING!!!! from C++ source code used in vector-pop arraylib.lsp
            set-symbol-plist
            structure-name
            )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Export symbols in GRAY package
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (core:select-package "GRAY"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '( fundamental-character-output-stream
          )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Export symbols in EXT package
;;;
(eval-when (:execute :compile-toplevel :load-toplevel)
  (core:select-package :ext))

(eval-when (:execute :compile-toplevel :load-toplevel)
  (export '(check-arguments-type
            array-index
            byte8
            integer8
            byte16
            integer16
            byte32
            integer32
            byte64
            integer64
            assume-no-errors
            sequence-stream
            all-encodings
            load-encoding
            make-encoding
            assume-right-type
            segmentation-violation
            stack-overflow
            stack-overflow-size
            stack-overflow-type
            storage-exhausted
            illegal-instruction
            unix-signal-received
            unix-signal-received-code
            interactive-interrupt
            hash-table-content
            hash-table-fill
            compiled-function-file
            lisp-implementation-vcs-id
            getcwd
            chdir
            +process-standard-input+
            external-process-wait
            external-process-status
            compiled-function-name
            system
            float-nan-string
            float-infinity-string
            package-local-nicknames
            add-package-local-nickname
            remove-package-local-nickname
            package-locally-nicknamed-by-list
            keep-old change-nick ; restarts for add-package-local-nicknames
            )))


