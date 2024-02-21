(in-package #:clasp-cleavir)

;;; Backend information associated with a BIR function.
(defclass llvm-function-info ()
  (;; In BIR, function environments are sets but we'd like to have it
   ;; be a list to ensure ordering.
   (%environment :initarg :environment :type list :reader environment)
   ;; The argument variables of the function lambda list.
   (%arguments :initarg :arguments :type list :reader arguments)
   (%main-function :initarg :main-function :reader main-function :type string)
   (%general-xep :initarg :general-xep :reader general-xep
                 :type (or null string))
   (%fixed-xeps :initarg :fixed-xeps :reader fixed-xeps
                ;; obviously null <: sequence but this is more explicit.
                :type (or null sequence))))

(defun allocate-llvm-function-info (function &key linkage)
  (let* ((lambda-name (get-or-create-lambda-name function))
         (jit-function-name (jit-function-name lambda-name))
         ;(function-info (calculate-function-info function lambda-name))
         (arguments (compute-arglist (bir:lambda-list function)))
         ;(function-description (cmp:irc-make-function-description function-info jit-function-name))
         (mtype (compute-llvm-function-type function arguments))
         (xep-p (xep-needed-p function))
         (analysis (cmp:calculate-cleavir-lambda-list-analysis
                    (bir:lambda-list function)))
         (main-function
           (cmp:irc-function-create
            mtype linkage
            (concatenate 'string jit-function-name ".main")
            cmp:*the-module*))
         (general-xep
           (when xep-p
             (cmp:irc-function-create
              (cmp:fn-prototype :general-entry) linkage
              (concatenate 'string jit-function-name ".xep-general")
              cmp:*the-module*)))
         (fixed-xeps
           (when xep-p
             (loop for i from cmp:+entry-point-arity-begin+
                     below cmp:+entry-point-arity-end+
                   for name = (format nil "~a.xep-~d" jit-function-name i)
                   collect (if (cmp::generate-function-for-arity-p i analysis)
                               (cmp:irc-function-create
                                (cmp:fn-prototype i) linkage name
                                cmp:*the-module*)
                               :placeholder))))
         ;; Check for a forced closure layout first.
         ;; if there isn't one, make one up.
         (env (or (fixed-closure function)
                  (cleavir-set:set-to-list
                   (bir:environment function)))))
    (make-instance 'llvm-function-info
      :environment env
      :main-function main-function
      :general-xep general-xep
      :fixed-xeps fixed-xeps
      :arguments arguments)))

(defun allocate-module-constants (module)
  (let ((constants (bir:constants module))
        (i 0))
    ;; Functions: If a XEP is needed, put in space for one
    (bir:do-functions (function module)
      (when (xep-needed-p function)
        ;; Note that there is no possibility of this BIR function appearing
        ;; as a literal constant, which would be ambiguous. We made the BIR
        ;; as part of this compilation process, so it's not in the code.
        (setf (gethash function *constant-values*)
              (cons i (find-llvm-function-info function)))
        (incf i)))
    ;; Actual constants
    (cleavir-set:doset (value constants)
      (let ((immediate (core:create-tagged-immediate-value-or-nil value)))
        (setf (gethash value *constant-values*)
              (if immediate
                  (cmp:irc-int-to-ptr (%i64 immediate) cmp:%t*%)
                  (prog1 i (incf i))))))
    i))

(defun nconstants ()
  (1+ (loop for value being the hash-values of *constant-values*
            maximizing (if (integerp value) value 0))))

(defun literal:reference-literal (value &optional read-only-p)
  (declare (ignore read-only-p))
  (let ((immediate (core:create-tagged-immediate-value-or-nil value)))
    (if immediate
        (values (setf (gethash value *constant-values*)
                      (cmp:irc-int-to-ptr (%i64 immediate) cmp:%t*%))
                nil)
        ;; FIXME: inefficient
        (values (setf (gethash value *constant-values*) (nconstants)) t))))

(defun layout-xep-function* (xep arity ir lambda-list-analysis calling-convention)
  (cmp:with-irbuilder (cmp:*irbuilder-function-alloca*)
    ;; Parse lambda list.
    (cmp:with-landing-pad nil
      (let ((ret (cmp:compile-lambda-list-code lambda-list-analysis
                                               calling-convention
                                               arity
                                               :argument-out #'out)))
        (unless ret
          (error "cmp:compile-lambda-list-code returned NIL which means this is not a function that should be generated")))
      ;; Import cells.
      (let* ((closure-vec (first (llvm-sys:get-argument-list xep)))
             (llvm-function-info (find-llvm-function-info ir))
             (environment-values
               (loop for import in (environment llvm-function-info)
                     for i from 0
                     for offset = (cmp:%closure%.offset-of[n]/t* i)
                     when import ; skip unused fixed closure entries
                       collect (cmp:irc-t*-load-atomic
                                (cmp::gen-memref-address closure-vec offset))))
             (source-pos-info (function-source-pos-info ir)))
        ;; Tail call the real function.
        (cmp:with-debug-info-source-position (source-pos-info)
          (let* ((main-function (main-function llvm-function-info))
                 (function-type (llvm-sys:get-function-type main-function))
                 (arguments
                   (mapcar (lambda (arg)
                             (translate-cast (in arg)
                                             '(:object) (cc-bmir:rtype arg)))
                           (arguments llvm-function-info)))
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
                (cmp:irc-ret (translate-cast
                              (local-call-rv->inputs c rrtype)
                              rrtype :multiple-values))
                (cmp:irc-unreachable)))))))
  xep)

(defun layout-xep-function (xep arity ir lambda-list-analysis lambda-name)
  ;; XEP is the LLVM function we are creating.
  (let* ((*datum-values* (make-hash-table :test #'eq))
         (jit-function-name (jit-function-name lambda-name))
         (cmp:*current-function-name* jit-function-name)
         (cmp:*gv-current-function-name*
           (cmp:module-make-global-string jit-function-name "fn-name"))
         (llvm-function-type (cmp:fn-prototype arity))
         (cmp:*current-function* xep)
         (entry-block (cmp:irc-basic-block-create "entry" xep))
         (*function-current-multiple-value-array-address* nil)
         (cmp:*irbuilder-function-alloca*
           (llvm-sys:make-irbuilder (cmp:thread-local-llvm-context)))
         (source-pos-info (function-source-pos-info ir))
         (lineno (core:source-pos-info-lineno source-pos-info)))
    (cmp:with-guaranteed-*current-source-pos-info* ()
      (cmp:with-dbg-function (:lineno lineno
                              :function-type llvm-function-type
                              :function xep)
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
          (cmp:with-debug-info-source-position (source-pos-info)
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
                                                  :rest-alloc (compute-rest-alloc lambda-list-analysis))))
              (layout-xep-function* xep arity ir lambda-list-analysis calling-convention))))))))

(defun layout-xep-group (function lambda-name abi)
  (declare (ignore abi))
  ;; This goes way up here because we want it only noted once, not
  ;; once for each arity we happen to emit.
  (maybe-note-return-cast function)
  (let ((info (find-llvm-function-info function))
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
                                             "literals"))
            (bcast
              (cmp:irc-bit-cast new-holder cmp:%t*[DUMMY]*% "bitcast-literals")))
       (llvm-sys:replace-all-uses-with cmp:*load-time-value-holder-global-var*
                                       bcast)
       (llvm-sys:erase-from-parent cmp:*load-time-value-holder-global-var*))
     (values)))

(defun layout-module (module abi &key (linkage 'llvm-sys:external-linkage))
  ;; Create llvm IR functions for each BIR function.
  (bir:do-functions (function module)
    ;; Assign IDs to unwind destinations. We start from 1 to allow
    ;; things to work with setjmp, which cannot return 0 from longjmp.
    (let ((i 1))
      (cleavir-set:doset (entrance (bir:entrances function))
        (setf (gethash entrance *unwind-ids*) i)
        (incf i)))
    (setf (gethash function *function-info*)
          (allocate-llvm-function-info function :linkage linkage)))
  (with-literals
      (allocate-module-constants module)
    (bir:do-functions (function module)
      (layout-procedure function (get-or-create-lambda-name function)
                        abi :linkage linkage))))

(defun translate (bir &key abi linkage)
  (let* ((*unwind-ids* (make-hash-table :test #'eq))
         (*function-info* (make-hash-table :test #'eq))
         (*constant-values* (make-hash-table :test #'eq)))
    (layout-module (bir:module bir) abi :linkage linkage)
    (cmp::potentially-save-module)
    (find-llvm-function-info bir)))

(defun enclose (function extent &optional (delay t))
  (let* ((code-info (find-llvm-function-info function))
         (centry (gethash function *constant-values*))
         (cindex (if (consp centry)
                     (car centry)
                     (error "BUG: Tried to ENCLOSE a function with no XEP")))
         (environment (environment code-info))
         (simple (literal:constants-table-value cindex)))
    (if environment
        (let* ((ninputs (length environment))
               (sninputs (%size_t ninputs))
               (enclose
                 (ecase extent
                   (:dynamic
                    (%intrinsic-call
                     "cc_stack_enclose"
                     (list (cmp:alloca-i8 (core:closure-size ninputs)
                                           :alignment cmp:+alignment+
                                           :label "stack-allocated-closure")
                           simple sninputs)))
                   (:indefinite
                    (%intrinsic-invoke-if-landing-pad-or-call
                     "cc_enclose"
                     (list simple sninputs))))))
          ;; We may not initialize the closure immediately in case it partakes
          ;; in mutual reference.
          ;; (If DELAY NIL is passed this delay is not necessary.)
          (if delay
              (delay-initializer
               (lambda ()
                 (%intrinsic-invoke-if-landing-pad-or-call
                  "cc_initialize_closure"
                  (list* enclose sninputs
                         (mapcar #'variable-as-argument environment)))))
              (%intrinsic-invoke-if-landing-pad-or-call
               "cc_initialize_closure"
               (list* enclose sninputs
                      (mapcar #'variable-as-argument environment))))
          enclose)
        ;; When the function has no environment, it can be compiled and
        ;; referenced as literal.
        simple)))

(defmethod translate-simple-instruction ((instruction bir:enclose) abi)
  (declare (ignore abi))
  (out (enclose (bir:code instruction) (bir:extent instruction))
       (bir:output instruction)))

(defun bir->function (bir &key (abi *abi-x86-64*)
                              (linkage 'llvm-sys:external-linkage))
  (cmp::with-compiler-env ()
    (let* ((module (cmp::create-run-time-module-for-compile)))
      ;; Link the C++ intrinsics into the module
      (cmp::with-module (:module module)
        (let ((pathname (if *load-pathname*
                            (namestring *load-pathname*)
                            "repl-code")))
          (cmp:with-debug-info-generator (:module cmp:*the-module* :pathname pathname)
            (translate bir :linkage linkage :abi abi)))
        (llvm-sys:dump-module module)))))

(in-package #:clasp-bytecode-to-bir)

(defun compile-function (function
                         &key (abi clasp-cleavir:*abi-x86-64*)
                           (linkage 'llvm-sys:external-linkage)
                           (system clasp-cleavir:*clasp-system*)
                           (disassemble nil))
  (multiple-value-bind (module funmap)
      (compile-bcmodule (core:simple-fun-code function))
    (bir:verify module)
    (when disassemble
      (cleavir-bir-disassembler:display module))
    (clasp-cleavir::bir-transformations module system)
    (let ((cleavir-cst-to-ast:*compiler* 'cl:compile)
          ;; necessary for bir->function debug info to work. KLUDGE
          (*load-pathname* (core:function-source-pos function))
          ;; Ensure any closures have the same layout as original
          ;; bytecode closures, so the simple fun can be swapped
          ;; out transparently.
          (clasp-cleavir::*fixed-closures*
            (fixed-closures-map (fmap funmap)))
          (bir (finfo-irfun (find-bcfun function funmap))))
      (clasp-cleavir::bir->function bir :abi abi :linkage linkage))))
