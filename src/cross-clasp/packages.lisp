(defpackage #:cross-clasp.clasp.core
  (:use #:cl)
  (:export #:+type-header-value-map+ #:header-stamp
           #:stamps-adjacent-p
           #:next-startup-position)
  (:export #:vaslist #:vaslistp #:vaslist-length #:vaslist-pop
           #:list-from-vaslist)
  (:export #:operator-shadowed-p #:process-declarations)
  (:export #:simple-program-error
           #:out-of-extent-unwind #:no-catch-tag
           #:simple-stream-error #:closed-stream
           #:simple-file-error #:file-does-not-exist #:file-exists
           #:simple-package-error #:import-name-conflict #:export-name-conflict
           #:use-package-name-conflict #:unintern-name-conflict
           #:package-lock-violation
           #:do-not-funcall-special-operator #:wrong-number-of-arguments
           #:odd-keywords #:unrecognized-keyword-argument-error
           #:simple-parse-error #:simple-reader-error)
  (:export #:check-pending-interrupts #:terminal-interrupt
           #:signal-code-alist)
  ;; Clasp usually only defines these if the underlying OS has the given signal.
  ;; Defining them unconditionally shouldn't be a problem, though. They'll just
  ;; never actually be signaled.
  (:export #:sigabrt #:sigalrm #:sigbus #:sigchld #:sigcont #:sigemt #:sigfpe
           #:sighup #:sigill #:sigint #:sigio #:sigkill #:sigpipe #:sigpoll
           #:sigprof #:sigpwr #:sigquit #:sigsegv #:sigstop #:sigtstp #:sigsys
           #:sigterm #:sigtrap #:sigttin #:sigttou #:sigurg #:sigusr1 #:sigusr2
           #:sigvtalrm #:sigxcpu #:sigxfsz #:sigwinch)
  (:export #:lambda-name #:lambda-list)
  (:export #:general #:generalp)
  (:export #:parse-bytespec)
  (:export #:put-f #:rem-f)
  (:export #:hash-table-pairs #:hash-equal #:hash-table-weakness
           #:hash-table-custom #:hash-table-equal #:hash-table-eql #:hash-table-eq
           #:hash-table-equalp)
  (:export #:fmt)
  (:export #:name-of-class #:class-source-location #:cxx-method-source-location
           #:instancep #:instance-ref)
  (:export #:proper-list-p)
  (:export #:ratiop
           #:short-float-p #:single-float-p #:double-float-p #:long-float-p)
  (:export #:printing-char-p)
  (:export #:data-vector-p #:replace-array #:vref
           #:make-simple-vector-t
           #:complex-vector-t #:simple-mdarray-t #:mdarray-t
           #:abstract-simple-vector #:complex-vector #:mdarray #:simple-mdarray
           #:str-ns #:str8ns #:simple-character-string #:str-wns #:bit-vector-ns
           #:simple-mdarray-base-char #:mdarray-base-char
           #:simple-mdarray-character #:mdarray-character
           #:simple-mdarray-bit #:mdarray-bit
           #:sbv-bit-and #:sbv-bit-ior #:sbv-bit-xor #:sbv-bit-eqv
           #:sbv-bit-nand #:sbv-bit-nor #:sbv-bit-andc1 #:sbv-bit-andc2
           #:sbv-bit-orc1 #:sbv-bit-orc2 #:sbv-bit-not)
  (:export #:num-op-asin #:num-op-acos #:num-op-atan
           #:num-op-asinh #:num-op-acosh #:num-op-atanh)
  (:export #:car-atomic #:rplaca-atomic #:cas-car
           #:cdr-atomic #:rplacd-atomic #:cas-cdr
           #:atomic-symbol-value #:atomic-set-symbol-value #:cas-symbol-value
           #:atomic-symbol-plist #:atomic-set-symbol-plist #:cas-symbol-plist)
  (:export #:function-name #:setf-function-name #:setf-lambda-list
           #:function-docstring #:function-source-pos #:set-source-pos-info)
  (:export #:single-dispatch-generic-function-p)
  (:export #:creator #:instance-creator #:standard-class-creator
           #:funcallable-instance-creator #:derivable-cxx-class-creator
           #:class-rep-creator
           #:compute-instance-creator
           #:class-holder
           #:allocate-standard-instance #:allocate-raw-instance
           #:allocate-raw-funcallable-instance
           #:class-stamp-for-instances #:class-new-stamp
           #:instance #:funcallable-instance #:derivable-cxx-object
           #:instance-sig #:instance-sig-set #:instance-stamp
           #:rack #:make-rack #:rack-sig #:rack-ref
           #:instance-rack #:instance-class)
  (:export #:core-fun-generator #:simple-core-fun-generator
           #:simple-fun #:core-fun #:simple-core-fun #:closure
           #:single-dispatch-generic-function #:single-dispatch-method
           #:gfbytecode-simple-fun #:gfbytecode-simple-fun/make
           #:gfbytecode-simple-fun/bytecode #:gfbytecode-simple-fun/literals
           #:bytecode-simple-fun #:bytecode-simple-fun/code
           #:bytecode-simple-fun/entry-pc-n #:bytecode-simple-fun/bytecode-size
           #:bytecode-module #:bytecode-module/bytecode #:bytecode-module/literals
           #:bytecode-module/debug-info
           #:function/entry-point
           #:make-closure #:closure-length #:closure-ref)
  (:export #:setf-find-class)
  (:export #:cxx-object #:make-cxx-object #:cxx-object-p #:cxx-class
           #:clbind-cxx-class #:derivable-cxx-class
           #:encode #:decode)
  (:export #:argc #:argv #:getpid #:temporary-directory #:mkstemp #:rmdir)
  (:export #:unix-get-local-time-zone #:unix-daylight-saving-time #:waitpid)
  (:export #:thread-local-write-to-string-output-stream
           #:get-thread-local-write-to-string-output-stream-string
           #:write-addr)
  (:export #:list-all-logical-hosts #:logical-host-p)
  (:export #:weak-pointer #:ephemeron
           #:make-weak-pointer #:weak-pointer-valid #:weak-pointer-value
           #:ephemeron)
  (:export #:external-object #:pointer #:wrapped-pointer #:immobile-object
           #:native-vector<int>)
  (:export #:symbol-to-enum-converter)
  (:export #:*echo-repl-tpl-read*)
  (:export #:signal-servicing)
  (:export #:num-logical-processors)
  (:export #:noprint-p #:noinform-p)
  (:export #:quasiquote #:*quasiquote*
           #:unquote #:unquote-nsplice #:unquote-splice)
  (:export #:scope #:file-scope #:file-scope-pathname #:mkstemp)
  (:export #:interpret)
  (:export #:wrong-number-of-arguments #:sequence-out-of-bounds)
  (:export #:set-breakstep #:unset-breakstep #:breakstepping-p
           #:invoke-internal-debugger #:debugger-disabled-p)
  (:export #:variable-cell #:function-cell)
  (:export #:dyn-env #:unknown-dyn-env #:dest-dyn-env #:lex-dyn-env
           #:block-dyn-env #:tagbody-dyn-env #:catch-dyn-env
           #:unwind-protect-dyn-env #:binding-dyn-env
           #:vmframe-dyn-env)
  (:export #:call-with-frame #:primitive-print-backtrace
           #:debugger-frame #:debugger-frame-up #:debugger-frame-down
           #:debugger-frame-fname #:debugger-frame-source-position
           #:debugger-frame-function-description #:debugger-frame-lang
           #:debugger-frame-closure #:debugger-frame-xep-p
           #:debugger-frame-args-available-p #:debugger-frame-args
           #:debugger-local #:debugger-frame-locals)
  (:export #:function-description #:function-description-lambda-list
           #:function-description-source-pathname
           #:function-description-lineno #:function-description-column
           #:function-description-docstring)
  (:export #:package-documentation
           #:package-local-nicknames-internal
           #:call-with-package-read-lock #:call-with-package-read-write-lock)
  (:export #:*functions-to-inline* #:*functions-to-notinline*)
  (:export #:*variable-source-infos*)
  (:export #:make-source-pos-info #:source-pos-info
           #:source-pos-info-lineno #:source-pos-info-column
           #:source-pos-info-file-handle #:source-pos-info-filepos
           #:source-pos-info-inlined-at #:source-pos-info-function-scope
           #:setf-source-pos-info-extra)
  (:export #:input-stream-source-pos-info)
  (:export #:bytecode-debug-info #:bytecode-debug-info/start #:bytecode-debug-info/end
           #:bytecode-debug-vars #:bytecode-debug-vars/bindings
           #:bytecode-debug-var
           #:bytecode-debug-var/decls #:bytecode-debug-var/name
           #:bytecode-debug-var/frame-index #:bytecode-debug-var/cellp
           #:bytecode-debug-location #:bytecode-debug-location/location
           #:bytecode-ast-decls #:bytecode-ast-decls/decls
           #:bytecode-ast-the #:bytecode-ast-the/type #:bytecode-ast-the/receiving
           #:bytecode-ast-if #:bytecode-ast-if/receiving
           #:bytecode-ast-tagbody #:bytecode-ast-tagbody/tags
           #:bytecode-ast-block
           #:bytecode-ast-block/receiving #:bytecode-ast-block/name
           #:bytecode-debug-macroexpansion #:bytecode-debug-macroexpansion/macro-name)
  (:export #:record #:user-data #:light-user-data #:directory-entry
           #:file-status #:string-input-stream #:string-output-stream
           #:posix-file-stream #:c-file-stream #:path
           #:weak-value-mapping #:strong-mapping #:weak-key-and-value-mapping
           #:weak-key-or-value-mapping #:weak-key-mapping #:mapping
           #:small-map #:small-multimap #:exposer #:core-exposer
           #:sharp-equal-wrapper #:load-time-values
           #:iterator #:directory-iterator #:recursive-directory-iterator)
  (:export #:simple-vector-byte2-t #:simple-vector-byte4-t
           #:simple-vector-byte8-t #:simple-vector-byte16-t
           #:simple-vector-byte32-t #:simple-vector-byte64-t
           #:simple-vector-size-t #:simple-vector-fixnum
           #:simple-vector-int2-t #:simple-vector-int4-t
           #:simple-vector-int8-t #:simple-vector-int16-t
           #:simple-vector-int32-t #:simple-vector-int64-t
           #:complex-vector-byte2-t #:complex-vector-byte4-t
           #:complex-vector-byte8-t #:complex-vector-byte16-t
           #:complex-vector-byte32-t #:complex-vector-byte64-t
           #:complex-vector-size-t #:complex-vector-fixnum
           #:complex-vector-int2-t #:complex-vector-int4-t
           #:complex-vector-int8-t #:complex-vector-int16-t
           #:complex-vector-int32-t #:complex-vector-int64-t
           #:simple-mdarray-byte2-t #:simple-mdarray-byte4-t
           #:simple-mdarray-byte8-t #:simple-mdarray-byte16-t
           #:simple-mdarray-byte32-t #:simple-mdarray-byte64-t
           #:simple-mdarray-size-t #:simple-mdarray-fixnum
           #:simple-mdarray-int2-t #:simple-mdarray-int4-t
           #:simple-mdarray-int8-t #:simple-mdarray-int16-t
           #:simple-mdarray-int32-t #:simple-mdarray-int64-t
           #:mdarray-byte2-t #:mdarray-byte4-t
           #:mdarray-byte8-t #:mdarray-byte16-t
           #:mdarray-byte32-t #:mdarray-byte64-t
           #:mdarray-size-t #:mdarray-fixnum
           #:mdarray-int2-t #:mdarray-int4-t
           #:mdarray-int8-t #:mdarray-int16-t
           #:mdarray-int32-t #:mdarray-int64-t)
  (:export #:simple-vector-short-float #:simple-vector-float
           #:simple-vector-double #:simple-vector-long-float
           #:complex-vector-short-float #:complex-vector-float
           #:complex-vector-double #:complex-vector-long-float
           #:simple-mdarray-short-float #:simple-mdarray-float
           #:simple-mdarray-double #:simple-mdarray-long-float
           #:mdarray-short-float #:mdarray-float
           #:mdarray-double #:mdarray-long-float)
  (:export #:syntax-type #:+standard-readtable+
           #:*read-hook* #:*read-preserving-whitespace-hook*)
  (:export #:load-source #:link-fasl-files)
  (:export #:command-line-load-eval-sequence
           #:rc-file-name #:no-rc-p #:noinform-p
           #:is-interactive-lisp #:load-extensions #:startup-type
           #:*extension-systems* #:*initialize-hooks* #:*terminate-hooks*)
  (:export #:*use-interpreter-for-eval*)
  (:export #:sl-boundp #:unbound #:unused)
  (:export #:quit))

(defpackage #:cross-clasp.clasp.gctools
  (:use #:cl)
  (:export #:garbage-collect #:finalize)
  (:export #:save-lisp-and-die)
  (:export #:thread-local-unwind-counter #:bytes-allocated))

(defpackage #:cross-clasp.clasp.clos
  (:use #:cl)
  (:local-nicknames (#:core #:cross-clasp.clasp.core)
                    (#:mop #:closer-mop))
  (:shadow #:define-method-combination)
  (:export #:slot-value-using-class)
  (:export #:standard-instance-access
           #:funcallable-standard-instance-access)
  (:export #:set-funcallable-instance-function #:get-funcallable-instance-function))

(defpackage #:cross-clasp.clasp.mp
  (:use #:cl)
  (:local-nicknames (#:core #:cross-clasp.clasp.core)
                    (#:clos #:cross-clasp.clasp.clos))
  (:export #:mutex #:make-lock #:get-lock #:giveup-lock #:holding-lock-p)
  (:export #:shared-mutex #:recursive-mutex #:make-shared-mutex
           #:shared-lock #:write-lock
           #:shared-unlock #:write-unlock)
  (:export #:condition-variable #:make-condition-variable
           #:condition-variable-wait #:condition-variable-signal)
  (:export #:with-lock #:with-rwlock
           #:without-interrupts #:with-interrupts)
  (:export #:*current-process* #:all-processes
           #:process #:process-name #:process-active-p
           #:interrupt-process #:process-suspend #:process-resume #:process-join
           #:suspend-loop #:abort-process #:process-kill #:process-cancel)
  (:export #:process-error #:process-error-process
           #:process-join-error #:process-join-error-original-condition
           #:process-join-error-aborted
           #:push-default-special-binding)
  (:export #:interrupt #:service-interrupt #:enqueue-interrupt
           #:signal-pending-interrupts #:signal-interrupt #:raise
           #:interactive-interrupt #:simple-interrupt #:simple-interactive-interrupt
           #:cancellation-interrupt #:call-interrupt #:call-interrupt-function
           #:suspension-interrupt #:posix-interrupt)
  (:export #:get-atomic-expansion #:define-atomic-expander
           #:not-atomic #:not-atomic-place))

(defpackage #:cross-clasp.clasp.llvm-sys
  (:use #:cl)
  (:export #:tag-tests)
  (:export
   #:insert-point #:linker #:dwarfunit #:target #:module #:llvm-context
   #:memory-buffer #:target-options #:dwarfcontext #:triple #:named-mdnode
   #:thread-safe-context #:target-machine #:constant-array #:undef-value
   #:constant-pointer-null #:constant-expr #:constant-fp #:constant-int
   #:constant-struct #:constant-data-array #:constant-data-sequential
   #:block-address #:global-variable #:function #:global-value #:constant
   #:branch-inst #:indirect-br-inst #:phinode #:invoke-inst #:call-inst
   #:call-base #:fence-inst #:atomic-cmp-xchg-inst #:resume-inst #:return-inst
   #:unreachable-inst #:store-inst #:landing-pad-inst #:atomic-rmwinst
   #:switch-inst #:alloca-inst #:load-inst #:vaarg-inst #:unary-instruction
   #:instruction #:user #:metadata-as-value #:basic-block #:argument #:value
   #:mdstring #:value-as-metadata #:dilocation #:diexpression #:dicompile-unit
   #:dilexical-block #:dilexical-block-base #:disubprogram #:dilocal-scope
   #:disubroutine-type #:dibasic-type #:diderived-type #:dicomposite-type
   #:ditype #:difile #:discope #:dilocal-variable #:divariable #:dinode #:mdnode
   #:metadata #:line-table #:enginebuilder #:sectioned-address #:dibuilder
   #:apint #:apfloat #:irbuilder #:irbuilder-base #:dicontext #:jitdylib
   #:vector-type #:struct-type #:integer-type #:function-type #:array-type
   #:pointer-type #:type #:target-subtarget-info #:mcsubtarget-info
   #:execution-engine #:struct-layout #:clasp-jit #:data-layout
   #:ditype-ref-array #:code-block #:library-file #:object-file #:library-base
   #:dinode-array #:library #:code-base #:function-callee #:mdbuilder
   #:debug-loc #:attribute-set #:attribute))
(defpackage #:cross-clasp.clasp.llvm
  (:use #:cl)
  (:export #:apsint #:apint))
(defpackage #:cross-clasp.clasp.clbind
  (:use #:cl)
  (:export #:class-registry #:class-rep #:dummy-creator #:constructor-creator))

(defpackage #:cross-clasp.clasp.cmp
  (:use #:cl)
  (:export #:code-walk)
  (:export #:module
           #:module/make #:module/link #:module/create-bytecode #:module/literals
           #:module/create-debug-info
           #:cfunction
           #:cfunction #:cfunction/nlocals #:cfunction/closed #:cfunction/entry-point
           #:cfunction/final-size #:cfunction/name #:cfunction/doc
           #:cfunction/lambda-list #:cfunction/module
           #:annotation #:annotation/module-position #:label #:fixup
           #:label-fixup #:control-label-fixup #:jump-if-supplied-fixup
           #:lex-fixup #:lex-ref-fixup #:encage-fixup #:lex-set-fixup
           #:entry-fixup #:restore-spfixup #:exit-fixup #:entry-close-fixup
           #:lexical-info #:var-info #:lexical-var-info #:special-var-info
           #:symbol-macro-var-info #:constant-var-info #:fun-info
           #:global-fun-info #:local-fun-info #:global-macro-info
           #:local-macro-info #:block-info #:tag-info
           #:constant-info #:constant-info/value
           #:load-time-value-info
           #:load-time-value-info/form #:load-time-value-info/read-only-p
           #:function-cell-info #:function-cell-info/fname
           #:variable-cell-info #:variable-cell-info/vname
           #:env-info
           #:make-null-lexical-environment
           #:lexenv
           #:lexenv/make #:lexenv/add-specials #:lexenv/macroexpansion-environment
           #:lexenv/vars #:lexenv/tags #:lexenv/blocks #:lexenv/funs
           #:lexenv/decls #:lexenv/frame-end
           #:symbol-macro-var-info/make
           #:local-macro-info/make
           #:bytecompile #:bytecompile-into)
  (:export #:with-atomic-file-rename)
  (:export #:*source-locations*)
  (:export #:*btb-compile-hook* #:*cleavir-compile-hook*))

(defpackage #:cross-clasp.clasp.sequence
  (:use)
  (:export #:make-sequence-iterator #:with-sequence-iterator #:dosequence)
  (:export #:elt #:length #:make-sequence-like #:adjust-sequence)
  (:export #:make-simple-sequence-iterator
           #:iterator-step #:iterator-endp #:iterator-element
           #:iterator-index #:iterator-copy)
  (:export #:protocol-unimplemented #:protocol-unimplemented-operation)
  (:export #:emptyp #:count #:count-if #:count-if-not #:find #:find-if #:find-if-not
           #:position #:position-if #:position-if-not #:subseq #:copy-seq #:fill
           #:nsubstitute #:nsubstitute-if #:nsubstitute-if-not
           #:substitute #:substitute-if #:substitute-if-not
           #:replace #:nreverse #:reverse #:reduce #:mismatch #:search
           #:delete #:delete-if #:delete-if-not #:remove #:remove-if #:remove-if-not
           #:delete-duplicates #:remove-duplicates #:sort #:stable-sort)
  (:export #:make-sequence #:define-iterative-sequence
           #:define-random-access-sequence #:make-random-access-iterator))

(defpackage #:cross-clasp.clasp.debug
  (:use #:cl)
  (:export #:with-truncated-stack)
  (:export #:step-form #:step-into #:step-over)
  (:export #:frame-arguments))

(defpackage #:cross-clasp.clasp.ext
  (:use #:cl)
  (:export #:byte2 #:byte4 #:byte8 #:byte16 #:byte32 #:byte64
           #:integer2 #:integer4 #:integer8 #:integer16
           #:integer32 #:integer64)
  (:export #:short-float-to-bits #:single-float-to-bits
           #:double-float-to-bits #:long-float-to-bits)
  (:export #:specialp #:symbol-macro)
  (:export #:check-arguments-type)
  (:export #:ansi-stream)
  (:export #:+process-standard-input+ #:+process-standard-output+
           #:+process-error-output+ #:+process-terminal-io+)
  (:export #:constant-form-value)
  (:export #:with-current-source-form #:current-source-location
           #:source-location #:source-location-pathname)
  (:export #:function-lambda-list)
  (:export #:type-expander)
  (:export #:parse-define-setf-expander #:setf-expander)
  (:export #:parse-deftype)
  (:export #:parse-macro #:parse-compiler-macro)
  (:export #:array-index)
  (:export #:interactive-interrupt)
  (:export #:add-package-local-nickname #:add-implementation-package
           #:package-implemented-by-list
           #:lock-package #:unlock-package #:package-locked-p)
  (:export #:*ed-functions*)
  (:export #:*invoke-debugger-hook* #:*inspector-hook*
           #:restart-associated-conditions
           #:restart-function #:restart-report-function
           #:restart-interactive-function #:restart-test-function)
  (:export #:segmentation-violation
           #:interactive-interrupt
           #:stack-overflow #:stack-overflow-size #:stack-overflow-type
           #:storage-exhausted #:bus-error
           #:name-conflict #:name-conflict-candidates #:resolve-conflict
           #:undefined-class #:assert-error
           #:character-coding-error #:encoding-error #:decoding-error
           #:character-encoding-error #:character-decoding-error
           #:stream-encoding-error #:stream-decoding-error)
  (:export #:tpl-frame #:tpl-argument #:tpl-arguments)
  (:export #:ansi-stream)
  (:export #:annotate #:*module-provider-functions*)
  (:export #:getenv)
  (:export #:*toplevel-hook*)
  (:export #:compiler-note #:start-autocompilation)
  (:import-from #:cross-clasp.clasp.core #:quit)
  (:export #:quit))

(defpackage #:cross-clasp.clasp.gray
  (:use #:cl)
  (:shadow #:streamp #:open-stream-p #:input-stream-p #:output-stream-p)
  (:shadow #:pathname #:truename)
  (:shadow #:stream-external-format #:stream-element-type)
  (:shadow #:close)
  (:import-from #:cross-clasp.clasp.ext #:ansi-stream)
  (:export #:fundamental-stream
           #:fundamental-input-stream #:fundamental-output-stream
           #:fundamental-character-stream #:fundamental-binary-stream
           #:fundamental-character-input-stream #:fundamental-character-output-stream
           #:fundamental-binary-input-stream #:fundamental-binary-output-stream)
  (:export #:streamp #:input-stream-p #:output-stream-p
           #:open-stream-p #:stream-interactive-p)
  (:export #:stream-write-sequence #:stream-read-sequence)
  (:export #:stream-write-char #:stream-unread-char
           #:stream-peek-char #:stream-read-char
           #:stream-write-string #:stream-read-line
           #:stream-read-char-no-hang #:stream-terpri #:stream-fresh-line)
  (:export #:stream-write-byte #:stream-read-byte)
  (:export #:stream-clear-input #:stream-clear-output #:stream-listen
           #:stream-finish-output #:stream-force-output)
  (:export #:stream-element-type #:stream-external-format
           #:stream-file-length #:stream-file-string-length)
  (:export #:pathname #:truename #:stream-file-descriptor)
  (:export #:close)
  (:export #:stream-input-line #:stream-input-column
           #:stream-line-number #:stream-start-line-p
           #:stream-line-length #:stream-line-column
           #:stream-file-position #:stream-advance-to-column))

(defpackage #:cross-clasp.clasp.clasp-ffi
  (:use #:cl)
  (:export #:foreign-type-spec #:foreign-data))
(defpackage #:cross-clasp.clasp.mpi
  (:use #:cl)
  (:export #:mpi))
(defpackage #:cross-clasp.clasp.clang-comments
  (:use #:cl)
  (:export #:full-comment #:comment))
(defpackage #:cross-clasp.clasp.clang-ast
  (:use #:cl)
  (:export
   #:identifier-info #:template-argument-list #:type-source-info #:template-name
   #:template-argument #:cxxbase-specifier #:type-loc #:ext-vector-type
   #:vector-type #:using-type #:unresolved-using-type #:unary-transform-type
   #:typedef-type #:type-of-type #:type-of-expr-type #:template-type-parm-type
   #:template-specialization-type #:record-type #:enum-type #:tag-type
   #:subst-template-type-parm-type #:subst-template-type-parm-pack-type
   #:rvalue-reference-type #:lvalue-reference-type #:reference-type
   #:pointer-type #:pipe-type #:paren-type #:pack-expansion-type
   #:obj-ctype-param-type #:obj-cinterface-type #:obj-cobject-type
   #:obj-cobject-pointer-type #:member-pointer-type
   #:dependent-sized-matrix-type #:constant-matrix-type #:matrix-type
   #:macro-qualified-type #:injected-class-name-type #:function-proto-type
   #:function-no-proto-type #:function-type #:elaborated-type
   #:dependent-vector-type #:dependent-template-specialization-type
   #:dependent-sized-ext-vector-type #:dependent-name-type
   #:dependent-bit-int-type #:dependent-address-space-type
   #:deduced-template-specialization-type #:auto-type #:deduced-type
   #:decltype-type #:complex-type #:builtin-type #:block-pointer-type
   #:bit-int-type #:btftag-attributed-type #:attributed-type #:atomic-type
   #:variable-array-type #:incomplete-array-type #:dependent-sized-array-type
   #:constant-array-type #:array-type #:decayed-type #:adjusted-type #:type
   #:while-stmt #:label-stmt #:vaarg-expr #:unary-operator
   #:unary-expr-or-type-trait-expr #:typo-expr #:type-trait-expr
   #:subst-non-type-template-parm-pack-expr #:subst-non-type-template-parm-expr
   #:string-literal #:stmt-expr #:source-loc-expr #:size-of-pack-expr
   #:shuffle-vector-expr #:syclunique-stable-name-expr #:requires-expr
   #:recovery-expr #:pseudo-object-expr #:predefined-expr #:paren-list-expr
   #:paren-expr #:pack-expansion-expr #:unresolved-member-expr
   #:unresolved-lookup-expr #:overload-expr #:opaque-value-expr #:offset-of-expr
   #:obj-csubscript-ref-expr #:obj-cstring-literal #:obj-cselector-expr
   #:obj-cprotocol-expr #:obj-cproperty-ref-expr #:obj-cmessage-expr
   #:obj-civar-ref-expr #:obj-cisa-expr #:obj-cindirect-copy-restore-expr
   #:obj-cencode-expr #:obj-cdictionary-literal #:obj-cboxed-expr
   #:obj-cbool-literal-expr #:obj-cavailability-check-expr #:obj-carray-literal
   #:ompiterator-expr #:omparray-shaping-expr #:omparray-section-expr
   #:no-init-expr #:member-expr #:matrix-subscript-expr
   #:materialize-temporary-expr #:msproperty-subscript-expr
   #:msproperty-ref-expr #:lambda-expr #:integer-literal #:init-list-expr
   #:implicit-value-init-expr #:imaginary-literal #:generic-selection-expr
   #:gnunull-expr #:function-parm-pack-expr #:expr-with-cleanups #:constant-expr
   #:full-expr #:floating-literal #:fixed-point-literal
   #:ext-vector-element-expr #:expression-trait-expr
   #:designated-init-update-expr #:designated-init-expr
   #:dependent-scope-decl-ref-expr #:dependent-coawait-expr #:decl-ref-expr
   #:coyield-expr #:coawait-expr #:coroutine-suspend-expr #:convert-vector-expr
   #:concept-specialization-expr #:compound-literal-expr #:choose-expr
   #:character-literal #:implicit-cast-expr #:obj-cbridged-cast-expr
   #:cxxstatic-cast-expr #:cxxreinterpret-cast-expr #:cxxdynamic-cast-expr
   #:cxxconst-cast-expr #:cxxaddrspace-cast-expr #:cxxnamed-cast-expr
   #:cxxfunctional-cast-expr #:cstyle-cast-expr #:builtin-bit-cast-expr
   #:explicit-cast-expr #:cast-expr #:user-defined-literal
   #:cxxoperator-call-expr #:cxxmember-call-expr #:cudakernel-call-expr
   #:call-expr #:cxxuuidof-expr #:cxxunresolved-construct-expr #:cxxtypeid-expr
   #:cxxthrow-expr #:cxxthis-expr #:cxxstd-initializer-list-expr
   #:cxxscalar-value-init-expr #:cxxrewritten-binary-operator
   #:cxxpseudo-destructor-expr #:cxxparen-list-init-expr
   #:cxxnull-ptr-literal-expr #:cxxnoexcept-expr #:cxxnew-expr
   #:cxxinherited-ctor-init-expr #:cxxfold-expr #:cxxdependent-scope-member-expr
   #:cxxdelete-expr #:cxxdefault-init-expr #:cxxdefault-arg-expr
   #:cxxtemporary-object-expr #:cxxconstruct-expr #:cxxbool-literal-expr
   #:cxxbind-temporary-expr #:block-expr #:compound-assign-operator
   #:binary-operator #:atomic-expr #:as-type-expr #:array-type-trait-expr
   #:array-subscript-expr #:array-init-loop-expr #:array-init-index-expr
   #:addr-label-expr #:conditional-operator #:binary-conditional-operator
   #:abstract-conditional-operator #:expr #:attributed-stmt #:value-stmt
   #:switch-stmt #:default-stmt #:case-stmt #:switch-case #:sehtry-stmt
   #:sehleave-stmt #:sehfinally-stmt #:sehexcept-stmt #:return-stmt
   #:obj-cfor-collection-stmt #:obj-cautorelease-pool-stmt #:obj-cat-try-stmt
   #:obj-cat-throw-stmt #:obj-cat-synchronized-stmt #:obj-cat-finally-stmt
   #:obj-cat-catch-stmt #:ompteams-directive #:omptaskyield-directive
   #:omptaskwait-directive #:omptaskgroup-directive #:omptask-directive
   #:omptarget-update-directive #:omptarget-teams-directive
   #:omptarget-parallel-for-directive #:omptarget-parallel-directive
   #:omptarget-exit-data-directive #:omptarget-enter-data-directive
   #:omptarget-directive #:omptarget-data-directive #:ompsingle-directive
   #:ompsections-directive #:ompsection-directive #:ompscan-directive
   #:ompparallel-sections-directive #:ompparallel-master-directive
   #:ompparallel-masked-directive #:ompparallel-directive #:ompordered-directive
   #:ompmeta-directive #:ompmaster-directive #:ompmasked-directive
   #:ompunroll-directive #:omptile-directive #:omploop-transformation-directive
   #:ompteams-generic-loop-directive #:ompteams-distribute-simd-directive
   #:ompteams-distribute-parallel-for-simd-directive
   #:ompteams-distribute-parallel-for-directive #:ompteams-distribute-directive
   #:omptask-loop-simd-directive #:omptask-loop-directive
   #:omptarget-teams-generic-loop-directive
   #:omptarget-teams-distribute-simd-directive
   #:omptarget-teams-distribute-parallel-for-simd-directive
   #:omptarget-teams-distribute-parallel-for-directive
   #:omptarget-teams-distribute-directive #:omptarget-simd-directive
   #:omptarget-parallel-generic-loop-directive
   #:omptarget-parallel-for-simd-directive #:ompsimd-directive
   #:ompparallel-master-task-loop-simd-directive
   #:ompparallel-master-task-loop-directive
   #:ompparallel-masked-task-loop-simd-directive
   #:ompparallel-masked-task-loop-directive #:ompparallel-generic-loop-directive
   #:ompparallel-for-simd-directive #:ompparallel-for-directive
   #:ompmaster-task-loop-simd-directive #:ompmaster-task-loop-directive
   #:ompmasked-task-loop-simd-directive #:ompmasked-task-loop-directive
   #:ompgeneric-loop-directive #:ompfor-simd-directive #:ompfor-directive
   #:ompdistribute-simd-directive #:ompdistribute-parallel-for-simd-directive
   #:ompdistribute-parallel-for-directive #:ompdistribute-directive
   #:omploop-directive #:omploop-based-directive #:ompinterop-directive
   #:ompflush-directive #:omperror-directive #:ompdispatch-directive
   #:ompdepobj-directive #:ompcritical-directive
   #:ompcancellation-point-directive #:ompcancel-directive
   #:ompbarrier-directive #:ompatomic-directive #:ompexecutable-directive
   #:ompcanonical-loop #:null-stmt #:msdependent-exists-stmt
   #:indirect-goto-stmt #:if-stmt #:goto-stmt #:for-stmt #:do-stmt #:decl-stmt
   #:coroutine-body-stmt #:coreturn-stmt #:continue-stmt #:compound-stmt
   #:captured-stmt #:cxxtry-stmt #:cxxfor-range-stmt #:cxxcatch-stmt
   #:break-stmt #:msasm-stmt #:gccasm-stmt #:asm-stmt #:stmt
   #:translation-unit-decl #:top-level-stmt-decl #:static-assert-decl
   #:requires-expr-body-decl #:pragma-detect-mismatch-decl #:pragma-comment-decl
   #:obj-cproperty-impl-decl #:ompthread-private-decl #:omprequires-decl
   #:ompallocate-decl #:unresolved-using-value-decl
   #:unnamed-global-constant-decl #:template-param-object-decl
   #:ompdeclare-reduction-decl #:ompdeclare-mapper-decl #:msguid-decl
   #:indirect-field-decl #:enum-constant-decl
   #:var-template-partial-specialization-decl #:var-template-specialization-decl
   #:parm-var-decl #:ompcaptured-expr-decl #:implicit-param-decl
   #:decomposition-decl #:var-decl #:non-type-template-parm-decl
   #:msproperty-decl #:cxxdestructor-decl #:cxxconversion-decl
   #:cxxconstructor-decl #:cxxmethod-decl #:cxxdeduction-guide-decl
   #:function-decl #:obj-civar-decl #:obj-cat-defs-field-decl #:field-decl
   #:declarator-decl #:binding-decl #:value-decl #:constructor-using-shadow-decl
   #:using-shadow-decl #:using-pack-decl #:using-directive-decl
   #:unresolved-using-if-exists-decl #:unresolved-using-typename-decl
   #:typedef-decl #:type-alias-decl #:obj-ctype-param-decl #:typedef-name-decl
   #:template-type-parm-decl #:class-template-partial-specialization-decl
   #:class-template-specialization-decl #:cxxrecord-decl #:record-decl
   #:enum-decl #:tag-decl #:type-decl #:template-template-parm-decl
   #:var-template-decl #:type-alias-template-decl #:function-template-decl
   #:class-template-decl #:redeclarable-template-decl #:concept-decl
   #:builtin-template-decl #:template-decl #:obj-cproperty-decl
   #:obj-cmethod-decl #:obj-cprotocol-decl #:obj-cinterface-decl
   #:obj-cimplementation-decl #:obj-ccategory-impl-decl #:obj-cimpl-decl
   #:obj-ccategory-decl #:obj-ccontainer-decl #:obj-ccompatible-alias-decl
   #:namespace-alias-decl #:namespace-decl #:label-decl #:hlslbuffer-decl
   #:using-enum-decl #:using-decl #:base-using-decl #:named-decl
   #:linkage-spec-decl #:lifetime-extended-temporary-decl #:import-decl
   #:implicit-concept-specialization-decl #:friend-template-decl #:friend-decl
   #:file-scope-asm-decl #:extern-ccontext-decl #:export-decl #:empty-decl
   #:class-scope-function-specialization-decl #:captured-decl #:block-decl
   #:access-spec-decl #:decl #:qual-type #:presumed-loc #:source-location))
(defpackage #:cross-clasp.clasp.ast-tooling
  (:use #:cl)
  (:export
   #:compile-command #:bound-nodes #:match-result #:match-callback
   #:match-callback-abstract #:match-finder #:variant-matcher #:variant-value
   #:dyn-typed-matcher #:arguments-adjuster #:frontend-action-factory
   #:clang-frontend-action-factory #:tool-action #:syntax-only-action #:astunit
   #:rewriter #:refactoring-tool #:replacements #:range #:replacement
   #:clang-tool #:astfrontend-action #:clang-syntax-only-action
   #:clang-astfrontend-action #:frontend-action #:compiler-instance
   #:char-source-range #:source-range #:source-manager #:astcontext
   #:preprocessor #:lexer #:lang-options #:clang-astconsumer
   #:jsoncompilation-database #:compilation-database #:asttooling-exposer))

;; Used by some external packages and such, for some reason
(defpackage #:cross-clasp.clasp.cl-user
  (:use #:cl))

(defpackage #:cross-clasp.clasp.alexandria
  (:use #:cl))

(defpackage #:cross-clasp
  (:use #:cl)
  (:local-nicknames (#:m #:maclina.machine)
                    (#:core #:cross-clasp.clasp.core)
                    (#:clos #:cross-clasp.clasp.clos)
                    (#:cmp #:cross-clasp.clasp.cmp)
                    (#:gray #:cross-clasp.clasp.gray)
                    (#:gc #:cross-clasp.clasp.gctools)
                    (#:mp #:cross-clasp.clasp.mp)
                    (#:llvm-sys #:cross-clasp.clasp.llvm-sys)
                    (#:ext #:cross-clasp.clasp.ext))
  (:shadow #:proclaim #:constantp)
  (:export #:client)
  (:export #:fill-environment)
  (:export #:find-compiler-class #:gf-info)
  (:import-from #:cross-clasp.clasp.ext #:constant-form-value)
  (:export #:build-macroexpand #:build-macroexpand-1
           #:describe-variable
           #:constantp #:constant-form-value)
  (:export #:initialize #:cross-compile-file #:install-delayed-macros #:build))
