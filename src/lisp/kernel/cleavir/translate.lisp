(in-package #:clasp-cleavir)

;;; Backend information associated with a BIR function.
(defclass llvm-function-info ()
  (;; In BIR, function environments are sets but we'd like to have it
   ;; be a list to ensure ordering.
   (%environment :initarg :environment :type list :reader environment)
   ;; The argument variables of the function lambda list.
   (%arguments :initarg :arguments :type list :reader arguments)
   ;; The eXternal Entry Point is in charge of loading values and
   ;; cells from the closure vector and parsing the number of arguments.
   (%xep-function :initarg :xep-function :reader xep-function)
   (%xep-function-description :initarg :xep-function-description :reader xep-function-description)
   (%main-function :initarg :main-function :reader main-function)
   (%main-function-description :initarg :main-function-description :reader main-function-description)))

(defun lambda-list-too-hairy-p (lambda-list)
  (multiple-value-bind (reqargs optargs rest-var
                        key-flag keyargs aok aux varest-p)
      (cmp::process-cleavir-lambda-list lambda-list)
    (declare (ignore reqargs optargs rest-var keyargs aok aux))
    (or key-flag varest-p)))

;; Assume that functions with no encloses and no local calls are
;; toplevel and need a XEP.
(defun xep-needed-p (function)
  (or (not (cleavir-set:empty-set-p (cleavir-bir:encloses function)))
      ;; We need a XEP for more involved lambda lists.
      (lambda-list-too-hairy-p (cleavir-bir:lambda-list function))
      ;; or for mv-calls that might need to signal an error.
      (and (cleavir-set:some (lambda (c) (typep c 'cleavir-bir:mv-local-call))
                             (cleavir-bir:local-calls function))
           (multiple-value-bind (req opt rest)
               (cmp::process-cleavir-lambda-list
                (cleavir-bir:lambda-list function))
             (declare (ignore opt))
             (or (plusp (car req)) (not rest))))
      ;; Else it would have been removed or deleted as it is
      ;; unreferenced otherwise.
      (cleavir-set:empty-set-p (cleavir-bir:local-calls function))))

(defun allocate-llvm-function-info (function &key (linkage 'llvm-sys:internal-linkage))
  (let* ((lambda-name (get-or-create-lambda-name function))
         (llvm-function-name (cmp:jit-function-name lambda-name))
         (function-info (calculate-function-info function lambda-name))
         (arguments
           (let ((arglist '()))
             (dolist (item (cleavir-bir:lambda-list function))
               (unless (symbolp item)
                 (if (consp item)
                     (ecase (length item)
                       (2
                        (push (first item) arglist)
                        (push (second item) arglist))
                       (3
                        (push (second item) arglist)
                        (push (third item) arglist)))
                     (push item arglist))))
             (nreverse arglist))))
    (multiple-value-bind (the-function function-description)
        (cmp:irc-cclasp-function-create
         (llvm-sys:function-type-get
          cmp::%tmv%
          (make-list (+ (cleavir-set:size (cleavir-bir:environment function))
                        (length arguments))
                     :initial-element cmp::%t*%))
         'llvm-sys:private-linkage
         llvm-function-name
         cmp:*the-module*
         function-info)
      (multiple-value-bind (xep-function xep-function-description)
          (if (xep-needed-p function)
              (cmp:irc-cclasp-function-create
               cmp:%fn-prototype%
               linkage
               llvm-function-name
               cmp:*the-module*
               function-info)
              (values :xep-unallocated :xep-unallocated))
        (make-instance 'llvm-function-info
          :environment (cleavir-set:set-to-list (cleavir-bir:environment function))
          :main-function the-function
          :main-function-description function-description
          :xep-function xep-function
          :xep-function-description xep-function-description
          :arguments arguments)))))

;;; For a computation, return its llvm value (for the :around method).
;;; For other instructions, return value is unspecified/irrelevant.
(defgeneric translate-simple-instruction (instruction return-value abi))

;;; Ditto
(defgeneric translate-terminator (instruction return-value abi next))

;;; Ditto
(defgeneric translate-primop (opname instruction))

;;; In CSTs and stuff the origin is (spi . spi). Use the head.
(defun origin-spi (origin)
  (if (consp origin) (car origin) origin))

(defun ensure-origin (origin &optional (num 999905))
  (or origin
      (core:make-source-pos-info "no-source-info-available" num num num)))

;;; Put in source info.
(defmethod translate-simple-instruction :around
    ((instruction cleavir-bir:instruction) return-value abi)
  (declare (ignore return-value abi))
  (cmp:with-debug-info-source-position ((ensure-origin
                                         (cleavir-bir:origin instruction)
                                         999902))
    (call-next-method)))
(defmethod translate-terminator :around
    ((instruction cleavir-bir:instruction) return-value abi next)
  (declare (ignore return-value abi next))
  (cmp:with-debug-info-source-position ((ensure-origin
                                         (cleavir-bir:origin instruction)
                                         999903))
    (call-next-method)))

;;; Output the computation's value.
(defmethod translate-simple-instruction :around
    ((instruction cleavir-bir:computation) return-value abi)
  (declare (ignore return-value abi))
  (out (call-next-method) instruction))
(defmethod translate-terminator :around
    ((instruction cleavir-bir:computation) return-value abi next)
  (declare (ignore return-value abi next))
  (out (call-next-method) instruction))

(defmethod translate-terminator ((instruction cleavir-bir:unreachable)
                                 return-value abi next)
  (declare (ignore return-value abi next))
  (cmp:irc-unreachable))

(defmethod translate-terminator ((instruction cleavir-bir:returni)
                                 return-value abi next)
  (declare (ignore abi next))
  (cmp:irc-ret (load-return-value return-value)))

(defmethod translate-terminator ((instruction cleavir-bir:alloca)
                                 return-value abi next)
  (declare (ignore abi))
  ;; For now, we only handle m-v-prog1.
  (assert (eq (cleavir-bir:rtype instruction) :multiple-values))
  (with-return-values (return-value abi nvalsl return-regs)
    (declare (ignore return-regs))
    (let* ((nvals (cmp:irc-load nvalsl))
           ;; NOTE: Must be done BEFORE the alloca.
           (save (%intrinsic-call "llvm.stacksave" nil))
           (mv-temp (cmp:alloca-temp-values nvals)))
      (setf (dynenv-storage instruction) (list save nvals mv-temp))))
  ;; Continue
  (cmp:irc-br (first next)))

(defgeneric undo-dynenv (dynamic-environment return-value))

(defmethod undo-dynenv ((dynamic-environment cleavir-bir:dynamic-leti) return-value)
  ;; Could undo stack allocated cells here
  (declare (ignore return-value)))
(defmethod undo-dynenv ((dynenv cleavir-bir:catch) return-value)
  ;; ditto, and mark the continuation out of extent
  (declare (ignore return-value)))
(defmethod undo-dynenv ((dynenv cleavir-bir:alloca) return-value)
  (declare (ignore return-value))
  (destructuring-bind (stackpos storage1 storage2)
      (dynenv-storage dynenv)
    (declare (ignore storage1 storage2))
    (%intrinsic-call "llvm.stackrestore" (list stackpos))))

(defun translate-local-unwind (jump return-value)
  (loop with target = (cleavir-bir:dynamic-environment
                       (first (cleavir-bir:next jump)))
        for de = (cleavir-bir:dynamic-environment jump)
          then (cleavir-bir:parent de)
        when (eq target de)
          do (loop-finish)
        when (typep de 'cleavir-bir:function)
          do (error "BUG: Dynamic environment chain screwed up somehow")
        do (undo-dynenv de return-value)))

(defmethod translate-terminator ((instruction cleavir-bir:jump)
                                 return-value abi next)
  (declare (ignore abi))
  (assert (= (length next) 1))
  (when (cleavir-bir:unwindp instruction)
    (translate-local-unwind instruction return-value))
  (loop for in in (cleavir-bir:inputs instruction)
        for out in (cleavir-bir:outputs instruction)
        unless (eq (cleavir-bir:rtype out) :multiple-values)
          do (phi-out (in in) out (llvm-sys:get-insert-block cmp:*irbuilder*)))
  (cmp:irc-br (first next)))

(defgeneric translate-conditional-test (instruction next))

(defmethod translate-conditional-test (instruction next)
  ;; When the test is not a conditional test, just grab the value and
  ;; compare to NIL.
  (cmp:irc-cond-br (cmp:irc-icmp-eq (in instruction) (%nil))
                   (second next) (first next)))

(defmethod translate-conditional-test ((instruction cleavir-bir:conditional-test) next)
  (error "Don't know how to translate this conditional test ~a." instruction))

(defmethod translate-conditional-test ((instruction cleavir-bir:eq-test) next)
  (let ((inputs (cleavir-bir:inputs instruction)))
    (cmp:irc-cond-br
     (cmp:irc-icmp-eq (in (first inputs)) (in (second inputs)))
     (first next) (second next))))

(defmethod translate-terminator ((instruction cleavir-bir:ifi)
                                 return-value abi next)
  (declare (ignore return-value abi))
  (translate-conditional-test (first (cleavir-bir:inputs instruction)) next))

(defmethod translate-simple-instruction ((instruction cleavir-bir:conditional-test)
                                         return-value abi)
  (declare (ignore instruction return-value abi))
  ;; Don't do anything besides assert that it is used by an IF.
  (assert (typep (cleavir-bir:use instruction) 'cleavir-bir:ifi)))

(defmethod translate-terminator ((instruction cleavir-bir:case)
                                 return-value abi next)
  (declare (ignore return-value abi))
  (assert (= (length next) (length (cleavir-bir:next instruction))
             (1+ (length (cleavir-bir:comparees instruction)))))
  (let* ((input (in (first (cleavir-bir:inputs instruction))))
         (default (first (last next)))
         (dests (butlast next))
         (comparees (cleavir-bir:comparees instruction))
         (ncases (loop for list in comparees summing (length list)))
         (only-fixnums-p (loop for list in comparees
                               always (every #'core:fixnump list)))
         ;; LLVM does better with contiguous ranges. It's not smart enough to
         ;; recognize that it could get a contiguous range by shifting.
         ;; (Or maybe it doesn't care. How often does that happen?)
         (rinput (if only-fixnums-p
                     (let ((fixnum-block (cmp:irc-basic-block-create "is-fixnum")))
                       ;; same as fixnump instruction
                       (cmp:compile-tag-check input
                                              cmp:+fixnum-mask+
                                              cmp:+fixnum00-tag+
                                              fixnum-block default)
                       (cmp:irc-begin-block fixnum-block)
                       (cmp:irc-untag-fixnum input cmp:%i64% "switch-input"))
                     (cmp:irc-ptr-to-int input cmp:%i64%)))
         (switch (cmp:irc-switch rinput default ncases)))
    (loop for list in comparees
          for dest in dests
          do (loop for object in list
                   for immediate = (core:create-tagged-immediate-value-or-nil object)
                   do (assert (not (null immediate)))
                      (cmp:irc-add-case switch
                                        (%i64 (if only-fixnums-p
                                                  (ash immediate -2)
                                                  immediate))
                                        dest)))))

(defun translate-sjlj-catch (catch return-value successors)
  ;; Call setjmp, switch on the result.
  (let ((bufp (cmp:alloca cmp::%jmp-buf-tag% 1 "jmp-buf")))
    (out (cmp:irc-bit-cast bufp cmp:%t*%) catch)
    (let* ((sj (%intrinsic-call "_setjmp" (list bufp)))
           (blocks (loop repeat (length (rest successors))
                         collect (cmp:irc-basic-block-create
                                  "catch-restore")))
           (default (cmp:irc-basic-block-create "catch-default"))
           (sw (cmp:irc-switch sj default (length successors))))
      (cmp:irc-begin-block default)
      (cmp:irc-unreachable)
      (cmp:irc-add-case sw (%i32 0) (first successors))
      (loop for succ in (rest successors) for block in blocks
            for i from 1
            do (cmp:irc-add-case sw (%i32 i) block)
               (cmp:irc-begin-block block)
               (restore-multiple-value-0 return-value)
               (cmp:irc-br succ)))))

(defmethod translate-terminator ((instruction cleavir-bir:catch)
                                 return-value abi next)
  (declare (ignore abi))
  (cond
    ((cleavir-set:empty-set-p (cleavir-bir:unwinds instruction))
     (cmp:irc-br (first next)))
    (t
     (cond
       ((cleavir-bir-transformations:simple-unwinding-p instruction)
        (translate-sjlj-catch instruction return-value next))
       (t
        ;; Assign the catch the continuation.
        (out (%intrinsic-call "llvm.frameaddress" (list (%i32 0)) "frame")
             instruction)
        ;; Unconditional branch to the normal successor;
        ;; dynamic environment stuff is handled in layout-iblock.
        (cmp:irc-br (first next)))))))

(defmethod translate-terminator ((instruction cleavir-bir:unwind)
                                 return-value abi next)
  (declare (ignore abi next))
  (let* ((cont (in (cleavir-bir:catch instruction)))
         (inputs (cleavir-bir:inputs instruction))
         (rv (first inputs))
         (destination (cleavir-bir:destination instruction))
         (destination-id (get-destination-id destination)))
    ;; We can only transmit multiple values, so make sure the adapter in
    ;; bir.lisp forced that properly
    (ecase (length inputs)
      ;; GO
      (0)
      ;; RETURN-FROM
      (1 (assert (cleavir-bir:rtype= (cleavir-bir:rtype rv) :multiple-values))))
    ;; Transmit those values
    (when rv
      (save-multiple-value-0 return-value))
    ;; unwind
    (if (cleavir-bir-transformations:simple-unwinding-p
         (cleavir-bir:catch instruction))
        ;; SJLJ
        ;; (Note: No landing pad because in order for SJLJ to occur,
        ;;  the dynamic environment must just be the function.)
        (let ((bufp (cmp:irc-bit-cast cont cmp::%jmp-buf-tag*%)))
          (%intrinsic-invoke-if-landing-pad-or-call
           ;; `+ because we can't pass 0 to longjmp.
           "longjmp" (list bufp (%i32 (1+ destination-id)))))
        ;; C++ exception
        (cmp:with-landing-pad (never-entry-landing-pad
                               (cleavir-bir:dynamic-environment instruction)
                               return-value)
          (%intrinsic-invoke-if-landing-pad-or-call
           "cc_unwind" (list cont (%size_t destination-id))))))
  (cmp:irc-unreachable))

(defmethod translate-terminator ((instruction cc-bir:unwind-protect)
                                 return-value abi next)
  (declare (ignore return-value abi))
  (setf (dynenv-storage instruction) (in (first (cleavir-bir:inputs instruction))))
  (cmp:irc-br (first next)))

(defmethod undo-dynenv ((dynenv cc-bir:unwind-protect) return-value)
  ;; Call the thunk.
  ;; We have to save values around it in case we're in the middle of returning values.
  ;; NOTE: ABI is ignored. literally irrelevant to the macro. FIXME
  (with-return-values (return-value abi nvalsl return-regs)
    (let* ((nvals (cmp:irc-load nvalsl))
           (primary (cmp:irc-load (return-value-elt return-regs 0)))
           (mv-temp (cmp:alloca-temp-values nvals)))
      (%intrinsic-call "cc_save_values" (list nvals primary mv-temp))
      ;; FIXME: Should this be never-entry?
      (cmp:with-landing-pad (maybe-entry-landing-pad
                             (cleavir-bir:parent dynenv) return-value *tags*)
        (closure-call-or-invoke
         (dynenv-storage dynenv) return-value nil))
      (store-tmv (%intrinsic-call "cc_load_values" (list nvals mv-temp))
                 return-value))))

(defmethod translate-terminator ((instruction cc-bir:bind) return-value abi next)
  (declare (ignore return-value abi))
  (let* ((inputs (cleavir-bir:inputs instruction))
         (sym (in (first inputs)))
         (val (in (second inputs))))
    (setf (dynenv-storage instruction)
          (list sym (%intrinsic-call "cc_TLSymbolValue" (list sym))))
    (%intrinsic-call "cc_setTLSymbolValue" (list sym val)))
  (cmp:irc-br (first next)))

(defmethod undo-dynenv ((dynenv cc-bir:bind) return-value)
  (declare (ignore return-value))
  (%intrinsic-call "cc_resetTLSymbolValue" (dynenv-storage dynenv)))

(defmethod translate-terminator ((instruction cleavir-bir:typew)
                                 return-value abi next)
  (declare (ignore return-value abi))
  (cmp:irc-br (third next)))

(defmethod translate-terminator ((instruction cleavir-bir:choke)
                                 return-value abi next)
  (declare (ignore return-value abi))
  (cmp:irc-br (first next)))

(defmacro define-tag-test (inst mask tag)
  `(defmethod translate-terminator ((instruction ,inst) return-value abi next)
     (declare (ignore return-value abi))
     (cmp:compile-tag-check (in (first (cleavir-bir:inputs instruction)))
                            ,mask ,tag
                            (first next) (second next))))
(define-tag-test cc-bmir:fixnump cmp:+fixnum-mask+ cmp:+fixnum00-tag+)
(define-tag-test cc-bmir:consp cmp:+immediate-mask+ cmp:+cons-tag+)
(define-tag-test cc-bmir:characterp cmp:+immediate-mask+ cmp:+character-tag+)
(define-tag-test cc-bmir:single-float-p
  cmp:+immediate-mask+ cmp:+single-float-tag+)
(define-tag-test cc-bmir:generalp cmp:+immediate-mask+ cmp:+general-tag+)

(defmethod translate-terminator ((instruction cc-bmir:headerq)
                                 return-value abi next)
  (declare (ignore return-value abi))
  (cmp:compile-header-check
   (cc-bmir:info instruction)
   (in (first (cleavir-bir:inputs instruction)))
   (first next) (second next)))

(defmethod translate-terminator
    ((instruction cc-bir:header-stamp-case) return-value abi next)
  (declare (ignore return-value abi))
  (let* ((stamp (in (first (cleavir-bir:inputs instruction))))
         (stamp-i64 (cmp:irc-ptr-to-int stamp cmp:%i64%))
         (where (cmp:irc-and stamp-i64 (%i64 cmp:+where-tag-mask+)))
         (defaultb (cmp:irc-basic-block-create "impossible-default"))
         (sw (cmp:irc-switch where defaultb 4)))
    (cmp:irc-add-case sw (%i64 cmp:+derivable-where-tag+) (first next))
    (cmp:irc-add-case sw (%i64 cmp:+rack-where-tag+) (second next))
    (cmp:irc-add-case sw (%i64 cmp:+wrapped-where-tag+) (third next))
    (cmp:irc-add-case sw (%i64 cmp:+header-where-tag+) (fourth next))
    (cmp:irc-begin-block defaultb)
    (cmp:irc-unreachable)))

(defmethod translate-simple-instruction ((instruction cleavir-bir:enclose)
                                         return-value abi)
  (declare (ignore return-value abi))
  (enclose (find-llvm-function-info (cleavir-bir:code instruction))
           (cleavir-bir:extent instruction)))

(defmethod translate-simple-instruction :before
    ((instruction cleavir-bir:abstract-call) return-value abi)
  (declare (ignore instruction return-value abi))
  ;; We must force all closure initializers to run before a call.
  (force-initializers))

(defmethod translate-simple-instruction ((instruction cleavir-bir:leti)
                                         return-value abi)
  (declare (ignore return-value abi))
  (bind-variable (first (cleavir-bir:outputs instruction)))
  (call-next-method))

(defmethod translate-simple-instruction ((instruction cleavir-bir:writevar)
                                         return-value abi)
  (declare (ignore return-value abi))
  (variable-out (in (first (cleavir-bir:inputs instruction)))
                (first (cleavir-bir:outputs instruction))))

(defmethod translate-simple-instruction ((instruction cleavir-bir:readvar)
                                         return-value abi)
  (declare (ignore return-value abi))
  (variable-in (first (cleavir-bir:inputs instruction))))

(defun gen-rest-list (present-arguments)
  ;; Generate a call to cc_list.
  ;; TODO: DX &rest lists.
  (%intrinsic-invoke-if-landing-pad-or-call
   "cc_list" (list* (%size_t (length present-arguments))
                    (mapcar #'in present-arguments))))

;; Create the argument list for a local call by parsing the callee's
;; lambda list and filling in the correct values at compile time. We
;; assume that we have already checked the validity of this call.
(defun parse-local-call-arguments (callee present-arguments)
  (let* ((lambda-list (cleavir-bir:lambda-list callee))
         (callee-info (find-llvm-function-info callee))
         (environment (environment callee-info))
         (state :required)
         (arguments '()))
    (dolist (item lambda-list)
      (if (symbolp item)
          (setq state item)
          (ecase state
            (:required
             (assert present-arguments)
             (push (in (pop present-arguments)) arguments))
            (&optional
             (cond (present-arguments
                    (push (in (pop present-arguments)) arguments)
                    (push (cmp::irc-t) arguments))
                   (t
                    (push (cmp:irc-undef-value-get cmp:%t*%) arguments)
                    (push (%nil) arguments))))
            (&key
             (error "I don't know how to do this."))
            (&rest
             (push (if (cleavir-bir:unused-p item) ; unused &rest
                       (cmp:irc-undef-value-get cmp:%t*%)
                       (gen-rest-list present-arguments))
                   arguments)))))
    ;; Augment the environment values to the arguments of the
    ;; call. Make sure to get the variable location and not
    ;; necessarily the value.
    (nconc (mapcar #'variable-as-argument environment) (nreverse arguments))))

(defun enclose (code-info extent &optional (delay t))
  (let* ((environment (environment code-info))
         (enclosed-function (xep-function code-info))
         (function-description (xep-function-description code-info)))
    (when (eq enclosed-function :xep-unallocated)
      (error "BUG: Tried to ENCLOSE a function with no XEP"))
    (if environment
        (let* ((ninputs (length environment))
               (sninputs (%size_t ninputs))
               (enclose
                 (ecase extent
                   (:dynamic
                    (%intrinsic-call
                     "cc_stack_enclose"
                     (list (cmp:alloca-i8 (core:closure-with-slots-size ninputs)
                                           :alignment cmp:+alignment+
                                           :label "stack-allocated-closure")
                           enclosed-function
                           (cmp:irc-bit-cast function-description cmp:%i8*%)
                           sninputs)))
                   (:indefinite
                    (%intrinsic-invoke-if-landing-pad-or-call
                     "cc_enclose"
                     (list enclosed-function
                           (cmp:irc-bit-cast function-description cmp:%i8*%)
                           sninputs))))))
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
        (%closurette-value enclosed-function function-description))))

(defmethod translate-simple-instruction ((instruction cleavir-bir:local-call)
                                         return-value abi)
  (declare (ignore abi))
  (let* ((callee (cleavir-bir:callee instruction))
         (callee-info (find-llvm-function-info callee))
         (lisp-arguments (rest (cleavir-bir:inputs instruction)))
         (nargs (length lisp-arguments)))
    (cond ((lambda-list-too-hairy-p (cleavir-bir:lambda-list callee))
           ;; Has &key or something, so use the normal call protocol.
           ;; We allocate a fresh closure for every call. Hopefully this
           ;; isn't too expensive. We can always use stack allocation since
           ;; there's no possibility of this closure being stored in a closure
           ;; (If we local-call a self-referencing closure, the closure cell
           ;;  will get its value from some enclose.
           ;;  FIXME we could use that instead?)
           (closure-call-or-invoke
            (enclose callee-info :dynamic nil)
            return-value (mapcar #'in lisp-arguments)))
          (t
           ;; Call directly.
           ;; Note that Cleavir doesn't make local-calls if there's an
           ;; argcount mismatch, so we don't need to sweat that.
           (let* ((arguments
                    (parse-local-call-arguments callee lisp-arguments))
                  (function (main-function callee-info))
                  (result-in-registers
                    (cmp::irc-call-or-invoke function arguments)))
             (llvm-sys:set-calling-conv result-in-registers 'llvm-sys:fastcc)
             (store-tmv result-in-registers return-value))))))

(defmethod translate-simple-instruction ((instruction cleavir-bir:call)
                                         return-value abi)
  (declare (ignore abi))
  (let ((inputs (cleavir-bir:inputs instruction)))
    (closure-call-or-invoke
     (in (first inputs)) return-value (mapcar #'in (rest inputs)))))

(defun general-mv-local-call (callee-info return-value)
  (%intrinsic-invoke-if-landing-pad-or-call
   "cc_call_multipleValueOneFormCallWithRet0"
   (list (enclose callee-info :dynamic nil)
         (load-return-value return-value))))

(defun direct-mv-local-call (return-value abi callee-info nreq nopt rest-var)
  (with-return-values (return-value abi nret return-regs)
    (let* ((rnret (cmp:irc-load nret))
           (nfixed (+ nreq nopt))
           (mismatch
             (unless (and (zerop nreq) rest-var)
               (cmp:irc-basic-block-create "lmvc-arg-mismatch")))
           (mte (if rest-var
                    (cmp:irc-basic-block-create "lmvc-more-than-enough")
                    mismatch))
           (merge (cmp:irc-basic-block-create "lmvc-after"))
           (sw (cmp:irc-switch rnret mte (+ 1 nreq nopt)))
           (environment (environment callee-info)))
      ;; Generate phis for the merge block's call.
      (cmp:irc-begin-block merge)
      (let ((opt-phis
              (loop for i below nopt
                    collect (cmp:irc-phi cmp:%t*% (1+ nopt))
                    collect (cmp:irc-phi cmp:%t*% (1+ nopt))))
            (rest-phi
              (cond ((null rest-var) nil)
                    ((cleavir-bir:unused-p rest-var)
                     (cmp:irc-undef-value-get cmp:%t*%))
                    (t (cmp:irc-phi cmp:%t*% (1+ nopt))))))
        ;; Generate the mismatch block, if it exists.
        (when mismatch
          (cmp:irc-begin-block mismatch)
          (cmp::irc-intrinsic-call-or-invoke
           "cc_wrong_number_of_arguments"
           (list (enclose callee-info :indefinite nil) rnret
                 (%size_t nreq) (%size_t nfixed)))
          (cmp:irc-unreachable))
        ;; Generate not-enough-args cases.
        (loop for i below nreq
              do (cmp:irc-add-case sw (%size_t i) mismatch))
        ;; Generate optional arg cases, including the exactly-enough case.
        (loop for i upto nopt
              for b = (cmp:irc-basic-block-create
                       (format nil "lmvc-optional-~d" i))
              for these-opt-phis = opt-phis
              do (cmp:irc-add-case sw (%size_t (+ nreq i)) b)
                 (cmp:irc-begin-block b)
                 (loop for j below i
                       do (cmp:irc-phi-add-incoming
                           (pop these-opt-phis)
                           (cmp:irc-load
                            (return-value-elt return-regs (+ nreq j)))
                           b)
                          (cmp:irc-phi-add-incoming (pop these-opt-phis)
                                                    (cmp::irc-t) b))
                 (loop repeat (- nopt i)
                       do (cmp:irc-phi-add-incoming
                           (pop these-opt-phis)
                           (cmp:irc-undef-value-get cmp:%t*%)
                           b)
                          (cmp:irc-phi-add-incoming (pop these-opt-phis)
                                                    (%nil) b))
                 (when (and rest-var
                            (not (cleavir-bir:unused-p rest-var)))
                   (cmp:irc-phi-add-incoming rest-phi (%nil) b))
                 (cmp:irc-br merge))
        ;; If there's a &rest, generate the more-than-enough arguments case.
        (when rest-var
          (cmp:irc-begin-block mte)
          (loop with these-opt-phis = opt-phis
                for j below nopt
                do (cmp:irc-phi-add-incoming
                    (pop these-opt-phis)
                    (cmp:irc-load (return-value-elt return-regs (+ nreq j)))
                    mte)
                   (cmp:irc-phi-add-incoming
                    (pop these-opt-phis) (cmp::irc-t) mte))
          (unless (cleavir-bir:unused-p rest-var)
            (cmp:irc-phi-add-incoming
             rest-phi
             (%intrinsic-invoke-if-landing-pad-or-call
              "cc_mvcGatherRest" (list rnret
                                       (cmp:irc-load
                                        (return-value-elt return-regs 0))
                                       (%size_t nfixed)))
             mte))
          (cmp:irc-br merge))
        ;; Generate the call, in the merge block.
        (cmp:irc-begin-block merge)
        (let* ((arguments
                 (nconc
                  (mapcar #'variable-as-argument environment)
                  (loop for j below nreq
                        collect (cmp:irc-load (return-value-elt return-regs j)))
                  opt-phis
                  (when rest-var (list rest-phi))))
               (function (main-function callee-info))
               (call
                 (cmp::irc-call-or-invoke function arguments)))
          (llvm-sys:set-calling-conv call 'llvm-sys:fastcc)
          call)))))

(defmethod translate-simple-instruction ((instruction cleavir-bir:mv-local-call)
                                         return-value abi)
  (let* ((callee (cleavir-bir:callee instruction))
         (callee-info (find-llvm-function-info callee)))
    (multiple-value-bind (req opt rest-var key-flag keyargs aok aux varest-p)
        (cmp::process-cleavir-lambda-list (cleavir-bir:lambda-list callee))
      (declare (ignore keyargs aok aux))
      (let ((call-result
              (if (or key-flag varest-p)
                  (general-mv-local-call callee-info return-value)
                  (direct-mv-local-call return-value abi callee-info
                                        (car req) (car opt) rest-var))))
        (store-tmv call-result return-value)
        call-result))))

(defmethod translate-simple-instruction ((instruction cleavir-bir:mv-call)
                                         return-value abi)
  (declare (ignore abi))
  (let ((call-result (%intrinsic-invoke-if-landing-pad-or-call
                      "cc_call_multipleValueOneFormCallWithRet0"
                      (list (in (first (cleavir-bir:inputs instruction)))
                            (load-return-value return-value)))))
    ;; call-result is a T_mv, and return-value a T_mv*
    (store-tmv call-result return-value)
    call-result))

(defmethod translate-simple-instruction ((instruction cc-bir:mv-foreign-call)
                                         return-value abi)
  (clasp-cleavir:unsafe-multiple-value-foreign-call
   (cc-bir:function-name instruction)
   return-value (mapcar #'in (cleavir-bir:inputs instruction)) abi))

(defmethod translate-simple-instruction
    ((instruction cc-bir:foreign-call-pointer) return-value abi)
  (declare (ignore return-value))
  (let ((inputs (cleavir-bir:inputs instruction)))
    (clasp-cleavir:unsafe-foreign-call-pointer
     :call (cc-bir:foreign-types instruction) (in (first inputs))
     (mapcar #'in (rest inputs)) abi)))

(defmethod translate-simple-instruction
    ((instruction cc-bir:defcallback)
     return-value (abi abi-x86-64))
  (declare (ignore return-value))
  (let* ((args (cc-bir:defcallback-args instruction))
         (closure (in (first (cleavir-bir:inputs instruction)))))
    (cmp::gen-defcallback
     (first args) (second args) (third args) (fourth args)
     (fifth args) (sixth args) (seventh args) (eighth args)
     closure)))

(defmethod translate-simple-instruction
    ((instruction cleavir-bir:fixed-to-multiple)
     return-value (abi abi-x86-64))
  (let* ((inputs (cleavir-bir:inputs instruction))
         (ninputs (length inputs)))
    (with-return-values (return-value abi nret ret-regs)
      (cmp:irc-store (%size_t ninputs) nret)
      (dotimes (i ninputs)
        (cmp:irc-store (in (elt inputs i))
                       (return-value-elt ret-regs i)))
      (loop for i from ninputs below +pointers-returned-in-registers+
            do (cmp:irc-store (%nil) (return-value-elt ret-regs i))))))

(defmethod translate-simple-instruction
    ((instr cleavir-bir:multiple-to-fixed) return-value (abi abi-x86-64))
  ;; Outputs that are returned in registers (see +pointers-returned-in-registers+) can be
  ;; unconditionally assigned, as things that return values ensure that those return registers
  ;; are always valid - e.g., (values) explicitly sets them to NIL.
  ;; Beyond that, we have to branch on nret.
  (with-return-values (return-value abi nret return-regs)
    (let* ((outputs (cleavir-bir:outputs instr))
           (nouts (length outputs)))
      ;; The easy ones.
      (loop for out in outputs
            for i below +pointers-returned-in-registers+
            do (out (cmp:irc-load (return-value-elt return-regs i)) out))
      ;; Now do the branch stuff (if there are enough outputs to require it)
      ;; We end up with a switch on nret. Say we have three outputs and +p-r-i-r+ is 1;
      ;; then we want
      ;; out[0] = values0; // values0 is a register
      ;; switch (nret) {
      ;; case 0: // don't need to bother with out[0] any more, so fallthrough
      ;; case 1: out[1] = nil; out[2] = nil; break;
      ;; case 2: out[1] = values[1]; out[2] = nil; break;
      ;; default: out[1] = values[1]; out[2] = values[1]; break; // any extra values ignored
      ;; }
      ;; We generate SSA directly, so the assignments are just phis.
      (when (> nouts +pointers-returned-in-registers+)
        (let* ((rets (loop for i from +pointers-returned-in-registers+ below nouts
                           collect (return-value-elt return-regs i)))
               (default (cmp:irc-basic-block-create "mtf-enough"))
               (switch (cmp:irc-switch (cmp:irc-load nret) default nouts))
               (final (cmp:irc-basic-block-create "mtf-final"))
               ;; Generate the default block, while keeping values for the phis.
               (default-vars (prog2 (cmp:irc-begin-block default)
                                 (mapcar #'cmp:irc-load rets)
                               (cmp:irc-br final)))
               ;; Generate the switch blocks. Put them in the switch while we're at it.
               ;; The binding here is to a list of (block . vars) so we can phi it.
               (blocks-and-vars
                 (loop for retn from +pointers-returned-in-registers+ below nouts
                       for block = (cmp:irc-basic-block-create (format nil "mtf-~d" retn))
                       do (cmp:irc-add-case switch (%size_t retn) block)
                          (cmp:irc-begin-block block)
                       collect (cons block
                                     (loop for ret in rets
                                           for i from +pointers-returned-in-registers+ below nouts
                                           collect (if (< i retn) (cmp:irc-load ret) (%nil))))
                       do (cmp:irc-br final))))
          ;; Set up all the register-only cases to use the first block.
          ;; (which sets all the outputs to NIL)
          (loop with low = (caar blocks-and-vars)
                for retn from 0 below +pointers-returned-in-registers+
                do (cmp:irc-add-case switch (%size_t retn) low))
          ;; Final generation: generate the phis and then output them.
          ;; NOTE: We can't output as we generate because (out ...) may generate a store,
          ;; and phis must not have any stores (or anything but a phi) preceding them.
          (cmp:irc-begin-block final)
          (let* ((vector-outs (nthcdr +pointers-returned-in-registers+ outputs))
                 (phis (loop for out in vector-outs
                            for i from 0
                            for phi = (cmp:irc-phi cmp:%t*% (1+ nouts))
                            do (loop for (block . vars) in blocks-and-vars
                                     do (cmp:irc-phi-add-incoming phi (elt vars i) block))
                               (cmp:irc-phi-add-incoming phi (elt default-vars i) default)
                            collect phi)))
            (loop for phi in phis
                  for out in vector-outs
                  do (out phi out))))))))

(defmethod translate-simple-instruction ((inst cc-bmir:memref2) return-value abi)
  (declare (ignore return-value abi))
  (cmp::gen-memref-address (in (first (cleavir-bir:inputs inst)))
                           (cc-bmir:offset inst)))

(defmethod translate-simple-instruction ((inst cc-bmir:load) return-value abi)
  (declare (ignore return-value abi))
  (cmp:irc-load-atomic (in (first (cleavir-bir:inputs inst)))))

(defmethod translate-simple-instruction ((inst cc-bmir:store) return-value abi)
  (declare (ignore return-value abi))
  (cmp:irc-store-atomic (in (first (cleavir-bir:inputs inst)))
                        (in (second (cleavir-bir:inputs inst)))))

(defmethod translate-simple-instruction ((inst cleavir-bir:vprimop)
                                         return-value abi)
  (declare (ignore return-value abi))
  (translate-primop (cleavir-primop-info:name (cleavir-bir:info inst)) inst))

(defmethod translate-primop ((name (eql 'symbol-value)) inst)
  (%intrinsic-invoke-if-landing-pad-or-call
   "cc_safe_symbol_value" (list (in (first (cleavir-bir:inputs inst))))))
(defmethod translate-primop ((name (eql 'fdefinition)) inst)
  (let ((symbol (in (first (cleavir-bir:inputs inst)))))
    (cmp:irc-fdefinition symbol)))
(defmethod translate-primop ((name (eql 'cc-bir::setf-fdefinition)) inst)
  (let ((setf-symbol (in (first (cleavir-bir:inputs inst)))))
    (cmp:irc-setf-fdefinition setf-symbol)))
(defmethod translate-primop ((name (eql 'core::vector-length)) inst)
  (cmp::gen-vector-length (in (first (cleavir-bir:inputs inst)))))
(defmethod translate-primop ((name (eql 'core::%displacement)) inst)
  (cmp:irc-real-array-displacement (in (first (cleavir-bir:inputs inst)))))
(defmethod translate-primop ((name (eql 'core::%displaced-index-offset)) inst)
  (cmp:irc-tag-fixnum
   (cmp:irc-real-array-index-offset (in (first (cleavir-bir:inputs inst))))))
(defmethod translate-primop ((name (eql 'core::%array-total-size)) inst)
  (cmp:irc-tag-fixnum
   (cmp:irc-array-total-size (in (first (cleavir-bir:inputs inst))))))
(defmethod translate-primop ((name (eql 'core::%array-rank)) inst)
  (cmp:irc-tag-fixnum
   (cmp:irc-array-rank (in (first (cleavir-bir:inputs inst))))))
(defmethod translate-primop ((name (eql 'core::%array-dimension)) inst)
  (cmp:gen-%array-dimension (in (first (cleavir-bir:inputs inst)))
                            (in (second (cleavir-bir:inputs inst)))))
(defmethod translate-primop ((name (eql 'clos:standard-instance-access)) inst)
  (cmp::gen-instance-ref (in (first (cleavir-bir:inputs inst)))
                         (in (second (cleavir-bir:inputs inst)))))
(defmethod translate-primop ((name (eql 'core::instance-cas)) inst)
  (let ((inputs (cleavir-bir:inputs inst)))
    (cmp::gen-instance-cas (in (third inputs)) (in (fourth inputs))
                           (in (first inputs)) (in (second inputs)))))
(defmethod translate-primop ((name (eql 'core:vaslist-pop)) inst)
  (cmp:gen-vaslist-pop (in (first (cleavir-bir:inputs inst)))))
(defmethod translate-primop ((name (eql 'core:vaslist-length)) inst)
  (cmp:gen-vaslist-length (in (first (cleavir-bir:inputs inst)))))
(defmethod translate-primop ((name (eql 'core::header-stamp)) inst)
  (cmp:irc-header-stamp (in (first (cleavir-bir:inputs inst)))))
(defmethod translate-primop ((name (eql 'core::rack-stamp)) inst)
  (cmp:irc-rack-stamp (in (first (cleavir-bir:inputs inst)))))
(defmethod translate-primop ((name (eql 'core::wrapped-stamp)) inst)
  (cmp:irc-wrapped-stamp (in (first (cleavir-bir:inputs inst)))))
(defmethod translate-primop ((name (eql 'core::derivable-stamp)) inst)
  (cmp:irc-derivable-stamp (in (first (cleavir-bir:inputs inst)))))
(defmethod translate-primop ((name (eql 'core:instance-rack)) inst)
  (cmp:gen-instance-rack (in (first (cleavir-bir:inputs inst)))))
(defmethod translate-primop ((name (eql 'core:instance-rack-set)) inst)
  (cmp:gen-instance-rack-set (in (first (cleavir-bir:inputs inst)))
                             (in (second (cleavir-bir:inputs inst)))))
(defmethod translate-primop ((name (eql 'core:rack-ref)) inst)
  (cmp:gen-rack-ref (in (first (cleavir-bir:inputs inst)))
                    (in (second (cleavir-bir:inputs inst)))))
(defmethod translate-primop ((name (eql 'core:rack-set)) inst)
  (cmp:gen-rack-set (in (first (cleavir-bir:inputs inst)))
                    (in (second (cleavir-bir:inputs inst)))
                    (in (third (cleavir-bir:inputs inst)))))

(defmethod translate-simple-instruction ((inst cleavir-bir:nvprimop)
                                         return-value abi)
  (declare (ignore return-value abi))
  (translate-primop (cleavir-primop-info:name (cleavir-bir:info inst)) inst))

(defmethod translate-primop ((name cons) inst) ; FIXME
  (cond ((equal name '(setf symbol-value))
         (%intrinsic-invoke-if-landing-pad-or-call
          "cc_setSymbolValue" (mapcar #'in (cleavir-bir:inputs inst))))
        ((equal name '(setf clos:standard-instance-access))
         (let ((inputs (cleavir-bir:inputs inst)))
           (cmp::gen-instance-set (in (first inputs)) (in (second inputs))
                                  (in (third inputs)))))
        (t
         (error "BUG: Don't know how to translate primop ~a" name))))

(defun gen-vector-effective-address (array index element-type fixnum-type)
  (let* ((type (llvm-sys:type-get-pointer-to
                (cmp::simple-vector-llvm-type element-type)))
         (cast (cmp:irc-bit-cast array type))
         (untagged (cmp:irc-untag-fixnum index fixnum-type "vector-index")))
    ;; 0 is for LLVM reasons, that pointers are C arrays. or something.
    ;; For layout of the vector, check simple-vector-llvm-type's definition.
    ;; untagged is the actual offset.
    (cmp:irc-gep-variable
     cast
     (list (%i32 0) (%i32 cmp::+simple-vector-data-slot+) untagged) "aref")))

(defmethod translate-simple-instruction ((inst cc-bir:acas) return-value abi)
  (declare (ignore return-value))
  (let ((et (cc-bir:element-type inst))
        (inputs (cleavir-bir:inputs inst)))
    (cmp:irc-cmpxchg
     ;; This will err if et = bit or the like.
     (gen-vector-effective-address
      (in (first inputs)) (in (second inputs)) et
      (%default-int-type abi))
     (in (third inputs)) (in (fourth inputs)))))

(defmethod translate-simple-instruction ((inst cleavir-bir:writetemp)
                                         return-value abi)
  (let ((alloca (cleavir-bir:alloca inst)))
    (check-type alloca cleavir-bir:alloca)
    ;; only handling m-v-prog1 for the moment
    (assert (eq (cleavir-bir:rtype alloca) :multiple-values))
    (destructuring-bind (stackpos storage1 storage2)
        (dynenv-storage alloca)
      (declare (ignore stackpos storage1))
      (with-return-values (return-value abi nvalsl return-regs)
        (%intrinsic-call
         "cc_save_values"
         (list (cmp:irc-load nvalsl)
               (cmp:irc-load (return-value-elt return-regs 0))
               storage2))))))

(defmethod translate-simple-instruction ((inst cleavir-bir:readtemp)
                                         return-value abi)
  (declare (ignore abi))
  (let ((alloca (cleavir-bir:alloca inst)))
    (check-type alloca cleavir-bir:alloca)
    (assert (eq (cleavir-bir:rtype alloca) :multiple-values))
    (destructuring-bind (stackpos storage1 storage2)
        (dynenv-storage alloca)
      (declare (ignore stackpos))
      (store-tmv
       (%intrinsic-call "cc_load_values" (list storage1 storage2))
       return-value))))

(defmethod translate-simple-instruction ((inst cleavir-bir:load-time-value)
                                         return-value abi)
  (declare (ignore return-value abi))
  (let ((index (gethash inst *constant-values*))
        (label ""))
    (cmp:irc-load
     (cmp:irc-gep-variable (literal:ltv-global)
                           (list (%size_t 0)
                                 (%i64 index))
                           label))))

(defmethod translate-simple-instruction ((inst cleavir-bir:constant-reference)
                                         return-value abi)
  (declare (ignore return-value abi))
  (let* ((constant (first (cleavir-bir:inputs inst)))
         (immediate-or-index (gethash constant *constant-values*)))
    (assert immediate-or-index () "Constant not found!")
    (if (integerp immediate-or-index)
        (cmp:irc-load
         (cmp:irc-gep-variable (literal:ltv-global)
                               (list (%size_t 0)
                                     (%i64 immediate-or-index))
                               ;; Maybe we can have a nice label for this.
                               ""))
        immediate-or-index)))

(defun initialize-iblock-translation (iblock)
  (let ((phis (cleavir-bir:inputs iblock)))
    (unless (null phis)
      (cmp:irc-begin-block (iblock-tag iblock))
      (let ((ndefinitions (+ (cleavir-set:size (cleavir-bir:predecessors iblock))
                             (cleavir-set:size (cleavir-bir:entrances iblock)))))
        (loop for phi in phis
              unless (eq (cleavir-bir:rtype phi) :multiple-values)
                do (setf (gethash phi *datum-values*)
                         (cmp:irc-phi cmp:%t*% ndefinitions)))))))

(defun layout-iblock (iblock return-value abi)
  (cmp:irc-begin-block (iblock-tag iblock))
  (cmp:with-landing-pad (maybe-entry-landing-pad
                         (cleavir-bir:dynamic-environment iblock)
                         return-value *tags*)
    (let ((*enclose-initializers* '()))
      (loop with end = (cleavir-bir:end iblock)
            for instruction = (cleavir-bir:start iblock)
              then (cleavir-bir:successor instruction)
            until (eq instruction end)
            do (translate-simple-instruction instruction return-value abi)
            finally (progn
                      (force-initializers)
                      (translate-terminator
                       instruction return-value abi
                       (mapcar #'iblock-tag (cleavir-bir:next end))))))))

(defun function-source-pos-info (irfunction)
  (ensure-origin (origin-spi (cleavir-bir:origin irfunction)) 999909))

(defun calculate-function-info (irfunction lambda-name)
  (let* ((origin (cleavir-bir:origin irfunction))
         (spi (origin-spi origin)))
    (cmp:make-function-info
     :function-name lambda-name
     :lambda-list (cleavir-bir:original-lambda-list irfunction)
     :docstring (cleavir-bir:docstring irfunction)
     :declares nil
     :form nil
     :spi spi)))

(defun iblock-name (iblock)
  (let ((name (cleavir-bir:name iblock)))
    (if name
        (string-downcase (symbol-name name))
        "iblock")))

(defun layout-xep-function* (the-function ir calling-convention
                             abi &key (linkage 'llvm-sys:internal-linkage))
  (cmp:with-irbuilder (cmp:*irbuilder-function-alloca*)
      ;; Parse lambda list.
    (cmp:compile-lambda-list-code (cleavir-bir:lambda-list ir)
                                  calling-convention
                                  :argument-out #'out)
    ;; Import cells.
    (let* ((closure-vec (first (llvm-sys:get-argument-list the-function)))
           (llvm-function-info (find-llvm-function-info ir))
           (environment-values
             (loop for import in (environment llvm-function-info)
                   for i from 0
                   for offset = (cmp:%closure-with-slots%.offset-of[n]/t* i)
                   collect (cmp:irc-load-atomic
                            (cmp::gen-memref-address closure-vec offset)))))
      ;; Tail call the real function.
      (cmp:with-debug-info-source-position
          ((core:make-source-pos-info "no-source-info-available" 999905 999905 999905))
        (cmp:irc-ret
         (let ((c
                 (cmp:irc-create-call
                  (main-function llvm-function-info)
                  ;; Augment the environment lexicals as a local call would.
                  (nconc environment-values
                         (mapcar #'in (arguments llvm-function-info))))))
           (llvm-sys:set-calling-conv c 'llvm-sys:fastcc)
           c)))))
  the-function)

(defun layout-main-function* (the-function ir
                              body-irbuilder body-block
                              abi &key (linkage 'llvm-sys:internal-linkage))
  (cmp:with-irbuilder (cmp:*irbuilder-function-alloca*)
    (let ((return-value (alloca-return)))
      (with-return-values (return-value abi nret ret-regs)
        (declare (ignore ret-regs))
        (cmp:irc-store (%size_t 0) nret))
      (cmp:with-irbuilder (body-irbuilder)
        (with-catch-pad-prep
            (cmp:irc-begin-block body-block)
          (cmp:with-landing-pad (never-entry-landing-pad ir return-value)
            ;; Bind the arguments and the environment values
            ;; appropriately.
            (let ((llvm-function-info (find-llvm-function-info ir)))
              (loop for arg in (llvm-sys:get-argument-list the-function)
                    for lexical in (append (environment llvm-function-info)
                                           (arguments llvm-function-info))
                    do (setf (gethash lexical *datum-values*) arg)))
            ;; Branch to the start block.
            (cmp:irc-br (iblock-tag (cleavir-bir:start ir)))
            ;; Lay out blocks.
            (cleavir-bir::map-reachable-iblocks
             (lambda (ib)
               (layout-iblock ib return-value abi))
             (cleavir-bir:start ir)))))))
  ;; Finish up by jumping from the entry block to the body block
  (cmp:with-irbuilder (cmp:*irbuilder-function-alloca*)
    (cmp:irc-br body-block))
  the-function)

(defun layout-xep-function (function lambda-name abi
                            &key (linkage 'llvm-sys:internal-linkage))
  (let* ((*datum-values* (make-hash-table :test #'eq))
         (llvm-function-name (cmp:jit-function-name lambda-name))
         (cmp:*current-function-name* llvm-function-name)
         (cmp:*gv-current-function-name*
           (cmp:module-make-global-string llvm-function-name "fn-name"))
         (llvm-function-type cmp:%fn-prototype%)
         (function-info (find-llvm-function-info function))
         (xep-function (xep-function function-info))
         (xep-function-description (xep-function-description function-info))
         (cmp:*current-function* xep-function)
         (cmp:*current-function-description* xep-function-description)
         (entry-block (cmp:irc-basic-block-create "entry" xep-function))
         (*function-current-multiple-value-array-address*
           nil)
         (cmp:*irbuilder-function-alloca*
           (llvm-sys:make-irbuilder (cmp:thread-local-llvm-context)))
         (source-pos-info (function-source-pos-info function))
         (fileid (core:source-pos-info-file-handle source-pos-info))
         (lineno (core:source-pos-info-lineno source-pos-info)))
    (cmp:with-dbg-function (:lineno lineno :linkage-name llvm-function-name
                            :function-type llvm-function-type
                            :function xep-function)
      (llvm-sys:set-personality-fn xep-function
                                   (cmp:irc-personality-function))
      (llvm-sys:add-fn-attr xep-function 'llvm-sys:attribute-uwtable)
      (cmp:irc-set-insert-point-basic-block entry-block
                                            cmp:*irbuilder-function-alloca*)
      (cmp:with-irbuilder (cmp:*irbuilder-function-alloca*)
        (cmp:with-debug-info-source-position (source-pos-info)
          (let* ((fn-args (llvm-sys:get-argument-list xep-function))
                 (lambda-list (cleavir-bir:lambda-list function))
                 (calling-convention
                   (cmp:setup-calling-convention
                    fn-args
                    :debug-on
                    (cleavir-policy:policy-value
                     (cleavir-bir:policy function)
                     'save-register-args)
                    :cleavir-lambda-list lambda-list)))
            (layout-xep-function* xep-function function calling-convention
                                  abi :linkage linkage)))))))

(defun layout-main-function (function lambda-name abi
                             &aux (linkage 'llvm-sys:private-linkage))
  (let* ((*tags* (make-hash-table :test #'eq))
         (*datum-values* (make-hash-table :test #'eq))
         (*dynenv-storage* (make-hash-table :test #'eq))
         (llvm-function-name (cmp:jit-function-name lambda-name))
         (cmp:*current-function-name* llvm-function-name)
         (cmp:*gv-current-function-name*
           (cmp:module-make-global-string llvm-function-name "fn-name"))
         (llvm-function-type cmp:%fn-prototype%)
         (function-info (find-llvm-function-info function))
         (the-function (main-function function-info))
         (function-description (main-function-description function-info))
         (cmp:*current-function* the-function)
         (cmp:*current-function-description* function-description)
         (entry-block (cmp:irc-basic-block-create "entry" the-function))
         (*function-current-multiple-value-array-address*
           nil)
         (cmp:*irbuilder-function-alloca*
           (llvm-sys:make-irbuilder (cmp:thread-local-llvm-context)))
         (body-irbuilder (llvm-sys:make-irbuilder
                          (cmp:thread-local-llvm-context)))
         (body-block (cmp:irc-basic-block-create "body"))
         (source-pos-info (function-source-pos-info function))
         (fileid (core:source-pos-info-file-handle source-pos-info))
         (lineno (core:source-pos-info-lineno source-pos-info)))
    (cmp:with-dbg-function (:lineno lineno :linkage-name llvm-function-name
                            :function-type llvm-function-type
                            :function the-function)
      (llvm-sys:set-calling-conv the-function 'llvm-sys:fastcc)
      (llvm-sys:set-personality-fn the-function
                                   (cmp:irc-personality-function))
      (llvm-sys:add-fn-attr the-function 'llvm-sys:attribute-uwtable)
      (cmp:with-irbuilder (body-irbuilder)
        (cleavir-bir:map-iblocks
         (lambda (ib)
           (setf (gethash ib *tags*)
                 (cmp:irc-basic-block-create
                  (iblock-name ib)))
           (initialize-iblock-translation ib))
         function))
      (cmp:irc-set-insert-point-basic-block entry-block
                                            cmp:*irbuilder-function-alloca*)
      (cmp:with-irbuilder (cmp:*irbuilder-function-alloca*)
        (cmp:with-debug-info-source-position (source-pos-info)
          (cmp:with-dbg-lexical-block
              (:lineno (core:source-pos-info-lineno source-pos-info))
            
            (let* ((fn-args (llvm-sys:get-argument-list the-function))
                   (lambda-list (cleavir-bir:lambda-list function)))
              (layout-main-function* the-function function
                                     body-irbuilder body-block
                                     abi :linkage linkage))))))))

(defun layout-procedure (function lambda-name abi
                         &key (linkage 'llvm-sys:internal-linkage))
  (when (xep-needed-p function)
    (layout-xep-function function lambda-name abi :linkage linkage))
  (layout-main-function function lambda-name abi))

(defun get-or-create-lambda-name (bir)
  (or (cleavir-bir:name bir) 'top-level))

;;; Given a BIR module, allocate its constants and load time
;;; values. We translate immediates directly, and use an index into
;;; the literal table for non-immediate constants.
(defun allocate-module-constants (module)
  (cleavir-set:doset (constant (cleavir-bir:constants module))
    (let* ((value (cleavir-bir:constant-value constant))
           (immediate (core:create-tagged-immediate-value-or-nil value)))
      (setf (gethash constant *constant-values*)
            (if immediate
                (cmp:irc-int-to-ptr
                 (%i64 immediate)
                 cmp:%t*%)
                (literal:reference-literal value t)))))
  (assert (or (cleavir-set:empty-set-p (cleavir-bir:load-time-values module))
              (eq cleavir-cst-to-ast:*compiler* 'cl:compile-file))
          ()
          "Found load-time-values to dump but not file compiling!")
  (cleavir-set:doset (load-time-value (cleavir-bir:load-time-values module))
    (let ((form (cleavir-bir:form load-time-value)))
      (setf (gethash load-time-value *constant-values*)
            ;; Allocate an index in the literal table for this load-time-value.
            (literal:load-time-value-from-thunk
             (compile-form form *clasp-env*))))))

(defun layout-module (module abi &key (linkage 'llvm-sys:internal-linkage))
  (let ((functions (cleavir-bir:functions module)))
    ;; Create llvm IR functions for each BIR function.
    (cleavir-set:doset (function functions)
      ;; Assign IDs to unwind destinations.
      (let ((i 0))
        (cleavir-set:doset (entrance (cleavir-bir:entrances function))
          (setf (gethash entrance *unwind-ids*) i)
          (incf i)))
      (setf (gethash function *function-info*)
            (allocate-llvm-function-info function :linkage linkage)))
    (allocate-module-constants module)
    (cleavir-set:doset (function functions)
      (layout-procedure function (get-or-create-lambda-name function)
                        abi :linkage linkage))))

(defun translate (bir &key abi linkage)
  (let* ((*unwind-ids* (make-hash-table :test #'eq))
         (*function-info* (make-hash-table :test #'eq))
         (*constant-values* (make-hash-table :test #'eq)))
    (layout-module (cleavir-bir:module bir) abi :linkage linkage)
    (cmp::potentially-save-module)
    (xep-function (find-llvm-function-info bir))))

(defun conversion-error-handler (condition)
  ;; Resignal the condition to see if anything higher up wants to handle it.
  ;; If not, continue compilation by replacing the errant form with a form
  ;; that will signal an error if it's reached at runtime.
  ;; The nature of this form is a bit tricky because it can't just include
  ;; the original condition, if we're in COMPILE-FILE - conditions aren't
  ;; necessarily dumpable, and nor is the source.
  ;; For now we just assume we're in COMPILE-FILE.
  (signal condition)
  (let* ((cst (cleavir-cst-to-ast:cst condition))
         (form (cst:raw cst))
         (origin (cst:source cst)))
    (invoke-restart 'cleavir-cst-to-ast:substitute-cst
                    (cst:reconstruct
                     `(error 'cmp:compiled-program-error
                             :form ,(with-standard-io-syntax
                                      (write-to-string form
                                                       :escape t :pretty t
                                                       :circle t :array nil))
                             :origin ',(origin-spi origin)
                             :condition ,(princ-to-string condition))
                     cst clasp-cleavir:*clasp-system* :default-source origin))))

(defun cst->ast (cst &optional (env *clasp-env*))
  "Compile a cst into an AST and return it.
Does not hoist.
COMPILE might call this with an environment in ENV.
COMPILE-FILE will use the default *clasp-env*."
  (handler-bind
      ((cleavir-env:no-variable-info
         (lambda (condition)
           (cmp:warn-undefined-global-variable
            (origin-spi (cleavir-env:origin condition))
            (cleavir-environment:name condition))
           (invoke-restart 'cleavir-cst-to-ast:consider-special)))
       (cleavir-env:no-function-info
         (lambda (condition)
           (cmp:register-global-function-ref
            (cleavir-environment:name condition)
            (origin-spi (cleavir-env:origin condition)))
           (invoke-restart 'cleavir-cst-to-ast:consider-global)))
       (cleavir-cst-to-ast:compiler-macro-expansion-error
         (lambda (condition)
           (warn 'cmp:compiler-macro-expansion-error-warning
                 :origin (origin-spi (cst:source (cleavir-cst-to-ast:cst condition)))
                 :condition condition)
           (continue condition)))
       ((and cleavir-cst-to-ast:compilation-program-error
             ;; If something goes wrong evaluating an eval-when,
             ;; we just want a normal error signal-
             ;; we can't recover and keep compiling.
             (not cleavir-cst-to-ast:eval-error))
         #'conversion-error-handler))
    (cleavir-cst-to-ast:cst-to-ast cst env clasp-cleavir:*clasp-system*)))

;;; Given an AST that may not be a function-ast, wrap it
;;; in a function AST. Useful for the pattern of
;;; (eval form) = (funcall (compile nil `(lambda () ,form)))
;;; as this essentially does the lambda wrap.
(defun wrap-ast (ast)
  (cleavir-ast:make-function-ast
   ast nil
   :origin (cleavir-ast:origin ast)
   :policy (cleavir-ast:policy ast)))

(defun ast->bir (ast system)
  (cleavir-ast-to-bir:compile-toplevel ast system))

(defvar *dis* nil)

(defun bir-transformations (module)
  (when *dis*
    (cleavir-bir::print-disasm
     (cleavir-bir:disassemble module)))
  (cleavir-bir-transformations:module-eliminate-catches module)
  (cleavir-bir-transformations:find-module-local-calls module)
  (cleavir-bir-transformations:module-optimize-variables module)
  (cleavir-bir-transformations:meta-evaluate-module module)
  (cc-bir-to-bmir:reduce-module-typeqs module)
  (cc-bir-to-bmir:reduce-module-primops module)
  ;; These should happen last since they are like "post passes".
  ;; NOTE: These must come in this order to maximize analysis.
  (cleavir-bir-transformations:determine-function-environments module)
  (cleavir-bir-transformations:determine-closure-extents module)
  (cleavir-bir-transformations:determine-variable-extents module)
  (values))

(defun translate-ast (ast &key (abi *abi-x86-64*)
                               (linkage 'llvm-sys:internal-linkage)
                               (system *clasp-system*))
  (let* ((bir (ast->bir ast system))
         (module (cleavir-bir:module bir)))
    ;;(cleavir-bir:verify module)
    (bir-transformations module)
    (cleavir-bir:verify module)
    (translate bir :abi abi :linkage linkage)))

(defun bir-compile (form env pathname
                    &key (linkage 'llvm-sys:internal-linkage))
  (bir-compile-cst (cst:cst-from-expression form) env pathname :linkage linkage))

(defun bir-compile-cst (cst env pathname
                        &key (linkage 'llvm-sys:internal-linkage))
  (let* (function
         ordered-raw-constants-list constants-table startup-fn shutdown-fn
         (cleavir-cst-to-ast:*compiler* 'cl:compile)
         (ast (cst->ast cst env)))
    (cmp:with-debug-info-generator (:module cmp:*the-module* :pathname pathname)
      (multiple-value-setq (ordered-raw-constants-list constants-table startup-fn shutdown-fn)
        (literal:with-rtv
            (setq function (translate-ast ast)))))
    (unless function
      (error "There was no function returned by translate-ast"))
    ;;(llvm-sys:dump-module cmp:*the-module* *standard-output*)
    (cmp:jit-add-module-return-function
     cmp:*the-module*
     function startup-fn shutdown-fn ordered-raw-constants-list)))

(defun bir-compile-in-env (form &optional env)
  (bir-compile-cst-in-env (cst:cst-from-expression form) env))

(defun bir-compile-cst-in-env (cst &optional env)
  (let ((cleavir-cst-to-ast:*compiler* 'cl:compile)
        (core:*use-cleavir-compiler* t))
    (cmp:compile-in-env cst env #'bir-compile-cst cmp:*default-compile-linkage*)))

(defun compile-form (form &optional (env *clasp-env*))
  (let* ((cst (cst:cst-from-expression form))
         (pre-ast (cst->ast cst env))
         (ast (wrap-ast pre-ast)))
    (translate-ast ast)))

(defun compile-file-cst (cst &optional (env *clasp-env*))
  (let* ((cmp:*default-condition-origin* (origin-spi (cst:source cst)))
         (pre-ast (cst->ast cst env))
         (ast (wrap-ast pre-ast)))
    (literal:arrange-thunk-as-top-level
     (translate-ast ast :linkage cmp:*default-linkage*))))

(defun bir-loop-read-and-compile-file-forms (source-sin environment)
  (let ((eof-value (gensym))
        (eclector.reader:*client* *cst-client*)
        (eclector.readtable:*readtable* cl:*readtable*)
        (cleavir-cst-to-ast:*compiler* 'cl:compile-file)
        (core:*use-cleavir-compiler* t))
    (loop
      ;; Required to update the source pos info. FIXME!?
      (peek-char t source-sin nil)
      ;; FIXME: if :environment is provided we should probably use a different read somehow
      (let* ((core:*current-source-pos-info* (cmp:compile-file-source-pos-info source-sin))
             (cst (eclector.concrete-syntax-tree:cst-read source-sin nil eof-value)))
        #+debug-monitor(sys:monitor-message "source-pos ~a" core:*current-source-pos-info*)
        (if (eq cst eof-value)
            (return nil)
            (progn
              (when *compile-print* (cmp::describe-form (cst:raw cst)))
              (core:with-memory-ramp (:pattern 'gctools:ramp)
                (compile-file-cst cst environment))))))))
