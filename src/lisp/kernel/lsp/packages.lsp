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
;;; Export symbols in SEQUENCE package
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (core:select-package "SEQUENCE"))

;;; SEQUENCE does not :use CL, so qualify CL symbols.
(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:export '(;; core protocol
               adjust-sequence
               elt
               length
               make-sequence-like
               ;; iterator protocol
               iterator-step
               iterator-endp
               iterator-element
               iterator-index
               iterator-copy
               make-simple-sequence-iterator
               make-sequence-iterator
               ;; may be customized or derived
               emptyp
               ;; ditto, but are CL symbols too
               count count-if count-if-not
               copy-seq
               delete delete-if delete-if-not
               delete-duplicates
               fill
               find find-if find-if-not
               mismatch
               nsubstitute nsubstitute-if nsubstitute-if-not
               nreverse
               position position-if position-if-not
               reduce
               remove remove-if remove-if-not
               remove-duplicates
               replace
               reverse
               search
               sort stable-sort
               subseq
               substitute substitute-if substitute-if-not
               ;; helper macros
               dosequence
               with-sequence-iterator
               with-simple-sequence-iterator
               ;; clasp extensions
               protocol-unimplemented
               protocol-unimplemented-operation
               make-sequence
               define-random-access-sequence
               make-random-access-iterator
               define-iterative-sequence
               ))
  (core:select-package "CORE"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Export symbols in MP package
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (core:select-package "MP"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(;; compare and swap
            cas
            get-cas-expansion define-cas-expander
            ;; atomic operations
            atomic-update
            atomic-incf atomic-decf
            ))
  (core:select-package "CORE"))

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
            unix-signal-received-handler
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


