(defpackage #:clasp-cleavir-2
  (:use #:cl)
  (:local-nicknames (#:cc #:clasp-cleavir)
                    (#:bir #:cleavir-bir)
                    (#:bir-transformations #:cleavir-bir-transformations)
                    (#:ast #:cleavir-ast)
                    (#:ctype #:cleavir-ctype)
                    (#:cst-to-ast #:cleavir-cst-to-ast)
                    (#:build #:cleavir-bir-builder)
                    (#:env #:cleavir-env)
                    (#:policy #:cleavir-compilation-policy)))

(in-package #:clasp-cleavir-2)

;;; Backend information associated with a BIR function.
(defclass llvm-function-info ()
  (;; In BIR, function environments are sets but we'd like to have it
   ;; be a list to ensure ordering.
   (%environment :initarg :environment :type list :reader cc::environment)
   ;; The argument variables of the function lambda list.
   (%arguments :initarg :arguments :type list :reader core:arguments)
   (%main-function :initarg :main-function :reader cc::main-function)
   (%general-xep :initarg :general-xep :reader general-xep)
   (%fixed-xeps :initarg :fixed-xeps :reader fixed-xeps
                ;; obviously null <: sequence but this is more explicit.
                :type (or null sequence))
   ;; The global variable holding an array of the XEP pointers.
   (%xep :initarg :xep :reader xep)
   ;; These should be obtainable from llvm-sys:get-name, but that seems to
   ;; return "" for functions after some point. Weird. FIXME
   (%main-function-name :initarg :main-function-name
                        :reader main-function-name)
   (%xep-name :initarg :xep-name :reader xep-name)))

;;; Result of TRANSLATE (below) containing everything needed for
;;; a compiled Lisp module:
;;; An LLVM-IR module, a map from BIR functions to function infos,
;;; and a constants map (from BIR constants etc. to either indices,
;;; or LLVM values for immediates).
(defclass translation ()
  ((%module :initarg :module :reader module)
   (%constant-values :initarg :constant-values :reader constant-values)
   (%function-info :initarg :function-info :reader function-info)))

(defun allocate-llvm-function-info (function &key toplevels)
  (let* ((lambda-name (cc::get-or-create-lambda-name function))
         (jit-function-name (cc::jit-function-name lambda-name))
          (arguments (cc::compute-arglist (bir:lambda-list function)))
         (mtype (cc::compute-llvm-function-type function arguments))
         (xep-p (or (member function toplevels) (xep-needed-p function)))
         (analysis (cmp:calculate-cleavir-lambda-list-analysis
                    (bir:lambda-list function)))
         (main-function
           (cmp:irc-function-create
            mtype 'llvm-sys:external-linkage
            (concatenate 'string jit-function-name ".main")
            cmp:*the-module*))
         (general-xep
           (when xep-p
             (cmp:irc-function-create
              (cmp:fn-prototype :general-entry) 'llvm-sys:external-linkage
              (concatenate 'string jit-function-name ".xep-general")
              cmp:*the-module*)))
         (fixed-xeps
           (when xep-p
             (loop for i from cmp:+entry-point-arity-begin+
                     below cmp:+entry-point-arity-end+
                   for name = (format nil "~a.xep-~d" jit-function-name i)
                   collect (if (cmp::generate-function-for-arity-p i analysis)
                               (cmp:irc-function-create
                                (cmp:fn-prototype i)
                                'llvm-sys:external-linkage name
                                cmp:*the-module*)
                               :placeholder))))
         (xep
           (when xep-p
             (let* ((name (concatenate 'string jit-function-name ".xep"))
                    ;; We rely on opaque pointer types here. Without them,
                    ;; we'd have to do some bitcasting.
                    (ptype (llvm-sys:type-get-pointer-to cmp:%void%))
                    (xtype (llvm-sys:array-type-get
                            ptype
                            (1+ cmp:+entry-point-arity-end+)))
                    (null (llvm-sys:constant-pointer-null-get ptype))
                    (init (llvm-sys:constant-array-get
                           xtype
                           (list* general-xep
                                  (loop for f in fixed-xeps
                                        collect (if (eq f :placeholder)
                                                    null
                                                    f))))))
               (llvm-sys:make-global-variable cmp:*the-module* xtype nil
                                              'llvm-sys:external-linkage
                                              init name))))
         ;; Check for a forced closure layout first.
         ;; if there isn't one, make one up.
         (env (or (cc::fixed-closure function)
                  (cleavir-set:set-to-list
                   (bir:environment function)))))
    (make-instance 'llvm-function-info
      :environment env
      :arguments arguments
      :main-function main-function
      :general-xep general-xep
      :fixed-xeps fixed-xeps
      :xep xep
      :main-function-name (llvm-sys:get-name main-function)
      :xep-name (when xep-p (llvm-sys:get-name xep)))))

;;; See if a function needs a XEP based on the IR.
;;; Note that the toplevel function (i.e. the one returned by CL:COMPILE)
;;; also gets a XEP, but it's handled differently.
(defun xep-needed-p (function)
  (or (bir:enclose function)
      ;; We need a XEP for more involved lambda lists.
      (cc::lambda-list-too-hairy-p (bir:lambda-list function))
      ;; or for mv-calls that might need to signal an error.
      (and (cleavir-set:some #'cc::nontrivial-mv-local-call-p
                             (bir:local-calls function))
           (multiple-value-bind (req opt rest)
               (cmp:process-bir-lambda-list (bir:lambda-list function))
             (declare (ignore opt))
             (or (plusp (car req)) (not rest))))))

(defun allocate-module-constants (module)
  (let ((i 0))
    ;; Functions: If a XEP is needed, put in space for one
    ;; except for toplevel functions, which don't need space in the
    ;; literals vector as nothing inside the code references them.
    (bir:do-functions (function module)
      (when (xep-needed-p function)
        ;; Keys in the *constant-values* table are usually BIR:CONSTANTs and
        ;; stuff. So there is no possibility of overlap between a BIR:FUNCTION
        ;; and a BIR:FUNCTION that literally appears in the code somehow.
        (setf (gethash function cc::*constant-values*) i)
        (incf i)))
    ;; Actual constants
    (cleavir-set:doset (value (bir:constants module))
      (let ((immediate (core:create-tagged-immediate-value-or-nil value)))
        (setf (gethash value cc::*constant-values*)
              (if immediate
                  (cmp:irc-int-to-ptr (cc::%i64 immediate) cmp:%t*%)
                  (prog1 i (incf i))))))
    i))

(defun nconstants ()
  (loop for value being the hash-values of cc::*constant-values*
        maximizing (if (integerp value) (1+ value) 0)))

;; A constant introduced at translate time. We use this instead of
;; bir:constant just to avoid any unneeded silliness with BIR modules
;; and backpointers.
(defclass last-minute-constant ()
  ((%value :initarg :value :reader bir:constant-value)))

(defun reference-literal (value &optional read-only-p)
  (declare (ignore read-only-p))
  (let ((next-index
          ;; Check for an existing constant.
          ;; While we're doing that, also check what a new constant
          ;; index would be.
          (loop for key being the hash-keys of cc::*constant-values*
                  using (hash-value indexoid)
                when (and (typep key '(or bir:constant last-minute-constant))
                          (eql (bir:constant-value key) value))
                  do (return-from reference-literal
                       (values indexoid (integerp indexoid)))
                maximizing (if (integerp indexoid) (1+ indexoid) 0)))
        (immediate (core:create-tagged-immediate-value-or-nil value))
        (constant (make-instance 'last-minute-constant :value value)))
      (if immediate
          (values (setf (gethash constant cc::*constant-values*)
                        (cmp:irc-int-to-ptr (cc::%i64 immediate) cmp:%t*%))
                  nil)
          (values (setf (gethash constant cc::*constant-values*) next-index) t))))

(defun layout-xep-function* (xep arity ir lambda-list-analysis calling-convention)
  (cmp:with-irbuilder (cmp:*irbuilder-function-alloca*)
    ;; Parse lambda list.
    (cmp:with-landing-pad nil
      (let ((ret (cmp:compile-lambda-list-code lambda-list-analysis
                                               calling-convention
                                               arity)))
        (unless ret
          (error "cmp:compile-lambda-list-code returned NIL which means this is not a function that should be generated")))
      ;; Import cells.
      (let* ((closure-vec (first (llvm-sys:get-argument-list xep)))
             (llvm-function-info (cc::find-llvm-function-info ir))
             (environment-values
               (loop for import in (cc::environment llvm-function-info)
                     for i from 0
                     for offset = (cmp:%closure%.offset-of[n]/t* i)
                     when import ; skip unused fixed closure entries
                       collect (cmp:irc-t*-load-atomic
                                (cmp::gen-memref-address closure-vec offset))))
             #+(or)
             (source-pos-info (cc::function-source-pos-info ir)))
        ;; Tail call the real function.
        (let* ((main-function (cc::main-function llvm-function-info))
               (function-type (llvm-sys:get-function-type main-function))
               (arguments
                 (mapcar (lambda (arg)
                           (cc::translate-cast (cc::in arg)
                                               '(:object) (cc-bmir:rtype arg)))
                         (core:arguments llvm-function-info)))
               (c
                 (cmp:irc-create-call-wft
                  function-type main-function
                  ;; Augment the environment lexicals as a local call would.
                  (nconc environment-values arguments)))
               (returni (bir:returni ir))
               (rrtype (and returni (cc-bmir:rtype (bir:input returni)))))
          #+(or)(llvm-sys:set-calling-conv c 'llvm-sys:fastcc)
          ;; Box/etc. results of the local call.
          (if returni
              (cmp:irc-ret (cc::translate-cast
                            (cc::local-call-rv->inputs c rrtype)
                            rrtype :multiple-values))
              (cmp:irc-unreachable))))))
  xep)

(defun layout-xep-function (xep arity ir lambda-list-analysis lambda-name)
  ;; XEP is the LLVM function we are creating.
  (let* ((cc::*datum-values* (make-hash-table :test #'eq))
         (jit-function-name (cc::jit-function-name lambda-name))
         (cmp:*current-function-name* jit-function-name)
         (cmp:*gv-current-function-name*
           (cmp:module-make-global-string jit-function-name "fn-name"))
         (llvm-function-type (cmp:fn-prototype arity))
         (cmp:*current-function* xep)
         (entry-block (cmp:irc-basic-block-create "entry" xep))
         (cc::*function-current-multiple-value-array-address* nil)
         (cmp:*irbuilder-function-alloca*
           (llvm-sys:make-irbuilder (cmp:thread-local-llvm-context)))
         (source-pos-info (cc::function-source-pos-info ir))
         (lineno (core:source-pos-info-lineno source-pos-info)))
    (llvm-sys:set-personality-fn xep (cmp:irc-personality-function))
    (llvm-sys:add-fn-attr2string xep "uwtable" "async")
    (when (null (bir:returni ir))
      (llvm-sys:add-fn-attr xep 'llvm-sys:attribute-no-return))
    (unless (policy:policy-value (bir:policy ir)
                                 'perform-optimization)
      (llvm-sys:add-fn-attr xep 'llvm-sys:attribute-no-inline)
      (llvm-sys:add-fn-attr xep 'llvm-sys:attribute-optimize-none))
    (cmp:irc-set-insert-point-basic-block entry-block
                                          cmp:*irbuilder-function-alloca*)
    (cmp:with-irbuilder (cmp:*irbuilder-function-alloca*)
      (when sys:*drag-native-calls*
        (cmp::irc-intrinsic "drag_native_calls"))
      (let ((calling-convention
              (cmp:setup-calling-convention xep
                                            arity
                                            :debug-on
                                            (policy:policy-value
                                             (bir:policy ir)
                                             'save-register-args)
                                            :cleavir-lambda-list-analysis lambda-list-analysis
                                            :rest-alloc (cc::compute-rest-alloc lambda-list-analysis))))
        (layout-xep-function* xep arity ir lambda-list-analysis calling-convention)))))

(defun layout-xep-group (function lambda-name abi)
  (declare (ignore abi))
  ;; This goes way up here because we want it only noted once, not
  ;; once for each arity we happen to emit.
  (cc::maybe-note-return-cast function)
  (let ((info (cc::find-llvm-function-info function))
        (analysis
          (cmp:calculate-cleavir-lambda-list-analysis
           (bir:lambda-list function))))
    (layout-xep-function (general-xep info) :general-entry
                         function analysis lambda-name)
    (loop for xep in (fixed-xeps info)
          for arity from 0
          unless (eq xep :placeholder)
            do (layout-xep-function xep arity function analysis lambda-name))))

(defmacro with-literals (&body body)
  `(let ((cmp:*load-time-value-holder-global-var-type* cmp:%t*[DUMMY]%)
         (cmp:*load-time-value-holder-global-var*
           (llvm-sys:make-global-variable cmp:*the-module*
                                          cmp:%t*[DUMMY]% ; type
                                          nil ; isConstant
                                          'llvm-sys:external-linkage
                                          (llvm-sys:undef-value-get cmp:%t*[DUMMY]%)
                                          "literals-DUMMY")))
     ,@body
     ;; Now that the body has been run, we know exactly how many literals
     ;; there are. As such, we can replace the array.
     (let* ((arrayt
              (llvm-sys:array-type-get cmp:%t*% (nconstants)))
            (new-holder
              (llvm-sys:make-global-variable cmp:*the-module*
                                             arrayt
                                             nil ; isConstant
                                             'llvm-sys:external-linkage
                                             (llvm-sys:undef-value-get arrayt)
                                             "__clasp_literals_"))
            (bcast
              (cmp:irc-bit-cast new-holder cmp:%t*[DUMMY]*% "bitcast-literals")))
       (llvm-sys:replace-all-uses-with cmp:*load-time-value-holder-global-var*
                                       bcast)
       (llvm-sys:erase-from-parent cmp:*load-time-value-holder-global-var*))
     (values)))

(defun layout-procedure (function lambda-name abi &key toplevels)
  (when (or (member function toplevels) (xep-needed-p function))
    (layout-xep-group function lambda-name abi))
  (cc::layout-main-function function lambda-name abi))

(defun layout-module (module abi &key toplevels)
  ;; Create llvm IR functions for each BIR function.
  (bir:do-functions (function module)
    ;; Assign IDs to unwind destinations. We start from 1 to allow
    ;; things to work with setjmp, which cannot return 0 from longjmp.
    (let ((i 1))
      (cleavir-set:doset (entrance (bir:entrances function))
        (setf (gethash entrance cc::*unwind-ids*) i)
        (incf i)))
    (setf (gethash function cc::*function-info*)
          (allocate-llvm-function-info function :toplevels toplevels)))
  (with-literals
      (allocate-module-constants module)
    (bir:do-functions (function module)
      (layout-procedure function (cc::get-or-create-lambda-name function)
                        abi :toplevels toplevels))))

(defun compute-debug-namestring (bir)
  (if bir
      (let ((origin (bir:origin bir)))
        (if origin
            (namestring
             (core:file-scope-pathname
              (core:file-scope
               (core:source-pos-info-file-handle origin))))
            "repl-code"))
      "repl-code"))

(defun translate (bir-module
                  &key abi (name "compile") toplevels debug-namestring)
  (unless debug-namestring
    (setf debug-namestring (compute-debug-namestring (first toplevels))))
  (let ((module (cmp::llvm-create-module name))
        (cc::*unwind-ids* (make-hash-table :test #'eq))
        (cc::*constant-values* (make-hash-table :test #'eq))
        (cc::*function-info* (make-hash-table :test #'eq))
        (cc::*literal-fn* #'reference-literal))
    (cmp::with-module (:module module)
      (layout-module bir-module abi :toplevels toplevels)
      (cmp:irc-verify-module-safe module)
      (cmp::potentially-save-module))
    (make-instance 'translation
      :module module :constant-values cc::*constant-values*
      :function-info cc::*function-info*)))

(defmethod cc::reference-xep (function (info llvm-function-info))
  (let ((cindex (or (gethash function cc::*constant-values*)
                    (error "BUG: Tried to ENCLOSE a function with no XEP"))))
    (literal:constants-table-value cindex)))

(defun bir->function (bir &key (abi cc::*abi-x86-64*))
  (let* ((translation
           (translate (bir:module bir) :abi abi :toplevels (list bir)))
         (module (module translation))
         (constants (constant-values translation))
         (cc::*function-info* (function-info translation))
         (jit (llvm-sys:clasp-jit))
         (dylib (llvm-sys:create-and-register-jitdylib jit "repl"))
         (object (llvm-sys:add-irmodule jit dylib module
                                        cmp:*thread-safe-context* 0)))
    (declare (ignore object))
    (fill-constants jit dylib constants)
    (let ((existing (gethash bir constants)))
      (if existing
          ;; There will already be a compiled fun in the literals
          ;; if it's e.g. enclosed.
          (core:literals-vref (llvm-sys:lookup jit dylib "__clasp_literals_")
                              existing)
          ;; Otherwise, make a new function.
          (make-compiled-fun jit dylib (make-function-description bir)
                             (cc::find-llvm-function-info bir))))))

;;; Return code for an IRMODULE as bytes.
;;; Used in COMPILE-FILE (specifically in compile-bytecode.lisp).
(defun emit-module (module)
  (let* ((triple-string (llvm-sys:get-target-triple module))
         (normalized-triple-string
           (llvm-sys:triple-normalize triple-string))
         (triple (llvm-sys:make-triple normalized-triple-string))
         (target-options (llvm-sys:make-target-options)))
    (multiple-value-bind (target msg)
        (llvm-sys:target-registry-lookup-target "" triple)
      (unless target
        (error msg))
      (llvm-sys:emit-module (llvm-sys:create-target-machine target
                                                            (llvm-sys:get-triple triple)
                                                            ""
                                                            ""
                                                            target-options
                                                            cmp::*default-reloc-model*
                                                            (cmp::code-model :jit nil)
                                                            'llvm-sys:code-gen-opt-default
                                                            nil)
                            :simple-vector-byte8
                            nil ; dwo-stream for dwarf objects
                            'llvm-sys:code-gen-file-type-object-file
                            module))))

(defgeneric resolve-constant (ir))

(defmethod resolve-constant ((ir bir:constant))
  (bir:constant-value ir))
(defmethod resolve-constant ((ir last-minute-constant))
  (bir:constant-value ir))

(defmethod resolve-constant ((ir bir:load-time-value))
  (eval (bir:form ir)))

(defmethod resolve-constant ((ir bir:function-cell))
  (core:ensure-function-cell (bir:function-name ir)))

(defmethod resolve-constant ((ir bir:variable-cell))
  (core:ensure-variable-cell (bir:variable-name ir)))

(defun fill-constants (jit dylib constants)
  (let ((literals (llvm-sys:lookup jit dylib "__clasp_literals_")))
    (loop for value being the hash-keys of constants
            using (hash-value cinf)
          ;; cinf is either a fixnum, meaning a literal,
          ;; or an llvm Value, meaning an immediate.
          ;; We don't need to do anything for immediates.
          ;; We also need to handle functions a bit differently- building
          ;; a simple fun for them.
          if (typep value 'bir:function)
            do (setf (cmp:literals-vref literals cinf)
                     (make-compiled-fun jit dylib
                                        (make-function-description value)
                                        (cc::find-llvm-function-info value)))
          else if (integerp cinf)
                 do (setf (core:literals-vref literals cinf)
                          (resolve-constant value))))
  (values))

(defun make-function-description (irfun)
  (let ((spi (cc::origin-spi (cc::origin-source (bir:origin irfun)))))
    (if spi
        (sys:function-description/make
         :function-name (cc::get-or-create-lambda-name irfun)
         :lambda-list (bir:original-lambda-list irfun)
         :docstring (bir:docstring irfun)
         :source-pathname (core:file-scope-pathname
                           (core:file-scope
                            (core:source-pos-info-file-handle spi)))
         :lineno (core:source-pos-info-lineno spi)
         ;; Why 1+?
         :column (1+ (core:source-pos-info-column spi))
         :filepos (core:source-pos-info-filepos spi))
        (sys:function-description/make
         :function-name (cc::get-or-create-lambda-name irfun)
         :lambda-list (bir:original-lambda-list irfun)
         :docstring (bir:docstring irfun)
         :source-pathname "-unknown-file-"))))

(defun make-compiled-fun (jit dylib fdesc info)
  (let ((main (llvm-sys:lookup jit dylib (main-function-name info)))
        (xep (llvm-sys:lookup jit dylib (xep-name info))))
    (core:make-simple-core-fun fdesc main xep)))

(in-package #:clasp-bytecode-to-bir)

(defmethod bir-constant->cmp ((constant clasp-cleavir-2::last-minute-constant))
  (cmp:constant-info/make (bir:constant-value constant)))

(defun compute-native-fmap (funmap function-info)
  (loop for (bcfun irfun) in (fmap funmap)
        for info = (gethash irfun function-info)
        for main = (clasp-cleavir-2::main-function-name info)
        for xep = (clasp-cleavir-2::xep-name info)
        collect (list bcfun main xep)))

(defun compile-cmodule (bytecode annotations literals
                        &key debug-namestring
                          (system clasp-cleavir:*clasp-system*)
                          (abi clasp-cleavir:*abi-x86-64*))
  (let* ((irmod (make-instance 'bir:module))
         (cliterals (compute-compile-literals literals))
         (funmap
           (compile-bytecode-into bytecode annotations cliterals irmod))
         (_ (clasp-cleavir::bir-transformations irmod system))
         (translation
           (clasp-cleavir-2::translate
            irmod :debug-namestring debug-namestring :abi abi
            ;; All IR functions with a corresponding bytecode function
            ;; need a XEP, so they can be put in the bytecode function.
            :toplevels (mapcar #'second (fmap funmap))))
         (nliterals (compute-nliterals cliterals (clasp-cleavir-2::constant-values translation)))
         (lmod (clasp-cleavir-2::module translation))
         (code (clasp-cleavir-2::emit-module lmod))
         (fmap (compute-native-fmap funmap (clasp-cleavir-2::function-info translation))))
    (declare (ignore _))
    (make-instance 'nmodule :code code :fmap fmap :literals nliterals)))

(defun compile-function (function
                         &key (abi clasp-cleavir:*abi-x86-64*)
                           (system clasp-cleavir:*clasp-system*)
                           (disassemble nil))
  (multiple-value-bind (module funmap)
      (compile-bcmodule (core:simple-fun-code function))
    (bir:verify module)
    (when disassemble
      (cleavir-bir-disassembler:display module))
    (clasp-cleavir::bir-transformations module system)
    (let ((cleavir-cst-to-ast:*compiler* 'cl:compile)
          ;; Ensure any closures have the same layout as original
          ;; bytecode closures, so the simple fun can be swapped
          ;; out transparently.
          (clasp-cleavir::*fixed-closures*
            (fixed-closures-map (fmap funmap)))
          (bir (finfo-irfun (find-bcfun function funmap))))
      (clasp-cleavir-2::bir->function bir :abi abi))))
