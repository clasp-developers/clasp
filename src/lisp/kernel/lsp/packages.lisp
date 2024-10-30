;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Export symbols in CORE package
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (core:select-package "CORE"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(single-dispatch-missing-dispatch-argument-error
            unrecognized-keyword-argument-error
            argc
            argv
            rmdir
            select-source-files
            compile-kernel-file
            load-system
            compile-system
            maybe-load-clasprc
            process-command-line-load-eval-sequence
            top-level
            *defun-inline-hook*
            *proclaim-hook*
            proper-list-p
            expand-associative
            expand-compare
            expand-uncompare
            with-memory-ramp
            ;; bytecode
            do-instructions do-module-instructions
            ;;;;MISSING SYMBOLS!!!!!
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
  (export '(fundamental-stream
            fundamental-input-stream
            fundamental-output-stream
            fundamental-character-stream
            fundamental-binary-stream
            fundamental-character-input-stream
            fundamental-character-output-stream
            fundamental-binary-input-stream
            fundamental-binary-output-stream)))

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
  (export '(;; atomic operations
            atomic fence cas get-atomic-expansion define-atomic-expander
            not-atomic not-atomic-place
            atomic-update atomic-update-explicit
            atomic-incf atomic-decf atomic-incf-explicit atomic-decf-explicit
            atomic-push atomic-push-explicit atomic-pop atomic-pop-explicit
            atomic-pushnew atomic-pushnew-explicit
            ;; interrupts
            interrupt service-interrupt interrupt-process
            process-kill process-suspend
            simple-interrupt simple-interactive-interrupt
            cancellation-interrupt suspension-interrupt
            call-interrupt call-interrupt-function
            ))
  (core:select-package "CORE"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Export symbols in EXT package
;;;
(eval-when (:execute :compile-toplevel :load-toplevel)
  (core:select-package :ext))

(import '(cmp::muffle-note
          core:argc
          core:argv
          core:getpid
          core:hash-table-weakness
          core:list-all-logical-hosts
          core:logical-host-p
          core:make-weak-pointer
          core:temporary-directory
          core:mkstemp
          core:printing-char-p
          core:quit
          core:rmdir
          core:weak-pointer-valid
          core:weak-pointer-value
          core:num-logical-processors
          core:quasiquote
          core:unquote
          core:unquote-splice
          core:unquote-nsplice
          gctools:finalize
          gctools:garbage-collect
          gctools:save-lisp-and-die)
        :ext)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (export '(*module-provider-functions*
            *source-location-kinds*
            current-source-location
            source-location
            source-location-pathname
            source-location-offset
            source-location-definer
            source-location-description
            compiled-function-name
            compiled-function-file
            who-calls
            who-binds
            who-sets
            who-references
            who-macroexpands
            who-specializes-directly
            list-callers
            list-callees
            list-all-logical-hosts
            logical-host-p
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
            all-encodings
            make-encoding
            assert-error
            float-nan-p
            float-infinity-p
            character-coding-error
            character-encoding-error
            character-decoding-error
            stream-encoding-error
            stream-decoding-error
            generate-encoding-hashtable
            quit
            with-float-traps-masked
            enable-interrupt default-interrupt ignore-interrupt
            get-signal-handler set-signal-handler
            *ed-functions*
            ;;; for asdf and slime and trivial-garbage to use ext:
            getpid argc argv rmdir temporary-directory mkstemp weak-pointer-value
            make-weak-pointer weak-pointer-valid hash-table-weakness
            num-logical-processors
            quasiquote
            unquote
            unquote-splice
            unquote-nsplice
            compiler-note
            muffle-note
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
            getcwd
            chdir
            +process-standard-input+
            system
            float-nan-string
            float-infinity-string
            package-local-nicknames
            add-package-local-nickname
            remove-package-local-nickname
            package-locally-nicknamed-by-list
            package-implements-list
            with-unlocked-packages
            keep-old change-nick ; restarts for add-package-local-nicknames
            ;; symbol name conflicts
            name-conflict name-conflict-candidates resolve-conflict
            ;; Readers of RESTART objects
            restart-function restart-report-function
            restart-interactive-function restart-test-function
            restart-associated-conditions
            ;; Debugger
            tpl-frame tpl-argument tpl-arguments
            ;; GC
            garbage-collect finalize save-lisp-and-die
            ;; Compiler
            describe-compiler-policy
            with-current-source-form
            start-autocompilation
            stop-autocompilation
            ;; Misc
            printing-char-p)))
