(in-package #:ext)

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
          gctools:save-lisp-and-die))

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
          macroexpand-all
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
          printing-char-p))
