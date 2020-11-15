(in-package #:clasp-cleavir-translate-bir)

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

;; Assume that functions with no encloses and no local calls are
;; toplevel and need a XEP.
(defun xep-needed-p (function)
  (or (not (cleavir-set:empty-set-p (cleavir-bir:encloses function)))
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
  (cmp:irc-ret (clasp-cleavir::load-return-value return-value)))

(defmethod translate-terminator ((instruction cleavir-bir:alloca)
                                 return-value abi next)
  ;; For now, we only handle m-v-prog1.
  (assert (eq (cleavir-bir:rtype instruction) :multiple-values))
  (clasp-cleavir::with-return-values (return-value abi nvalsl return-regs)
    (let* ((nvals (cmp:irc-load nvalsl))
           ;; NOTE: Must be done BEFORE the alloca.
           (save (clasp-cleavir::%intrinsic-call "llvm.stacksave" nil))
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
    (clasp-cleavir::%intrinsic-call "llvm.stackrestore" (list stackpos))))

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

(defmethod translate-terminator ((instruction cleavir-bir:eqi)
                                 return-value abi next)
  (declare (ignore return-value abi))
  (let ((inputs (cleavir-bir:inputs instruction)))
    (cmp:irc-cond-br
     (cmp:irc-icmp-eq (in (first inputs)) (in (second inputs)))
     (first next) (second next))))

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
                                        (clasp-cleavir::%i64 (if only-fixnums-p
                                                                 (ash immediate -2)
                                                                 immediate))
                                        dest)))))

(defun translate-sjlj-catch (catch return-value successors)
  ;; Call setjmp, switch on the result.
  (let ((bufp (cmp:alloca cmp::%jmp-buf-tag% 1 "jmp-buf")))
    (out (cmp:irc-bit-cast bufp cmp:%t*%) catch)
    (let* ((sj (clasp-cleavir::%intrinsic-call "_setjmp" (list bufp)))
           (blocks (loop repeat (length (rest successors))
                         collect (cmp:irc-basic-block-create
                                  "catch-restore")))
           (default (cmp:irc-basic-block-create "catch-default"))
           (sw (cmp:irc-switch sj default (length successors))))
      (cmp:irc-begin-block default)
      (cmp:irc-unreachable)
      (cmp:irc-add-case sw (clasp-cleavir::%i32 0) (first successors))
      (loop for succ in (rest successors) for block in blocks
            for i from 1
            do (cmp:irc-add-case sw (clasp-cleavir::%i32 i) block)
               (cmp:irc-begin-block block)
               (clasp-cleavir::restore-multiple-value-0 return-value)
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
        (out (clasp-cleavir::%intrinsic-call
              "llvm.frameaddress"
              (list (clasp-cleavir::%i32 0)) "frame")
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
      (clasp-cleavir::save-multiple-value-0 return-value))
    ;; unwind
    (if (cleavir-bir-transformations:simple-unwinding-p
         (cleavir-bir:catch instruction))
        ;; SJLJ
        ;; (Note: No landing pad because in order for SJLJ to occur,
        ;;  the dynamic environment must just be the function.)
        (let ((bufp (cmp:irc-bit-cast cont cmp::%jmp-buf-tag*%)))
          (clasp-cleavir::%intrinsic-invoke-if-landing-pad-or-call
           ;; `+ because we can't pass 0 to longjmp.
           "longjmp" (list bufp (clasp-cleavir::%i32 (1+ destination-id)))))
        ;; C++ exception
        (cmp:with-landing-pad (never-entry-landing-pad
                               (cleavir-bir:dynamic-environment instruction)
                               return-value)
          (clasp-cleavir::%intrinsic-invoke-if-landing-pad-or-call
           "cc_unwind"
           (list cont (clasp-cleavir::%size_t destination-id))))))
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
  (clasp-cleavir::with-return-values (return-value abi nvalsl return-regs)
    (let* ((nvals (cmp:irc-load nvalsl))
           (primary (cmp:irc-load (clasp-cleavir::return-value-elt return-regs 0)))
           (mv-temp (cmp:alloca-temp-values nvals)))
      (clasp-cleavir::%intrinsic-call "cc_save_values" (list nvals primary mv-temp))
      ;; FIXME: Should this be never-entry?
      (cmp:with-landing-pad (maybe-entry-landing-pad
                             (cleavir-bir:parent dynenv) return-value *tags*)
        (clasp-cleavir::closure-call-or-invoke
         (dynenv-storage dynenv) return-value nil))
      (clasp-cleavir::store-tmv
       (clasp-cleavir::%intrinsic-call "cc_load_values" (list nvals mv-temp))
       return-value))))

(defmethod translate-terminator ((instruction cc-bir:bind) return-value abi next)
  (declare (ignore return-value abi))
  (let* ((inputs (cleavir-bir:inputs instruction))
         (sym (in (first inputs)))
         (val (in (second inputs))))
    (setf (dynenv-storage instruction)
          (list sym (clasp-cleavir::%intrinsic-call "cc_TLSymbolValue" (list sym))))
    (clasp-cleavir::%intrinsic-call "cc_setTLSymbolValue" (list sym val)))
  (cmp:irc-br (first next)))

(defmethod undo-dynenv ((dynenv cc-bir:bind) return-value)
  (declare (ignore return-value))
  (clasp-cleavir::%intrinsic-call "cc_resetTLSymbolValue"
                                  (dynenv-storage dynenv)))

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
  (declare (ignore abi))
  (let* ((stamp (in (first (cleavir-bir:inputs instruction))))
         (stamp-i64 (cmp:irc-ptr-to-int stamp cmp:%i64%))
         (where (cmp:irc-and stamp-i64 (clasp-cleavir::%i64 cmp:+where-tag-mask+)))
         (defaultb (cmp:irc-basic-block-create "impossible-default"))
         (sw (cmp:irc-switch where defaultb 4)))
    (cmp:irc-add-case sw (clasp-cleavir::%i64 cmp:+derivable-where-tag+) (first next))
    (cmp:irc-add-case sw (clasp-cleavir::%i64 cmp:+rack-where-tag+) (second next))
    (cmp:irc-add-case sw (clasp-cleavir::%i64 cmp:+wrapped-where-tag+) (third next))
    (cmp:irc-add-case sw (clasp-cleavir::%i64 cmp:+header-where-tag+) (fourth next))
    (cmp:irc-begin-block defaultb)
    (cmp:irc-unreachable)))

(defmethod translate-simple-instruction ((instruction cleavir-bir:enclose)
                                         return-value abi)
  (declare (ignore return-value))
  (let* ((code (cleavir-bir:code instruction))
         (code-info (find-llvm-function-info code))
         (environment (environment code-info))
         (enclosed-function (xep-function (find-llvm-function-info code)))
         (function-description
           (llvm-sys:get-named-global
            cmp:*the-module* (cmp::function-description-name enclosed-function))))
    (if environment
        (let* ((ninputs (length environment))
               (sninputs (clasp-cleavir::%size_t ninputs))
               (enclose
                 (ecase (cleavir-bir:extent instruction)
                   (:dynamic
                    (clasp-cleavir::%intrinsic-call
                     "cc_stack_enclose"
                     (list (cmp:alloca-i8 (core:closure-with-slots-size ninputs)
                                           :alignment cmp:+alignment+
                                           :label "stack-allocated-closure")
                           enclosed-function
                           (cmp:irc-bit-cast function-description cmp:%i8*%)
                           sninputs)))
                   (:indefinite
                    (clasp-cleavir::%intrinsic-invoke-if-landing-pad-or-call
                     "cc_enclose"
                     (list enclosed-function
                           (cmp:irc-bit-cast function-description cmp:%i8*%)
                           sninputs))))))
          ;; We don't initialize the closure immediately in case it partakes
          ;; in mutual reference.
          (delay-initializer
           (lambda ()
             (clasp-cleavir::%intrinsic-invoke-if-landing-pad-or-call
              "cc_initialize_closure"
              (list* enclose sninputs
                     (mapcar (lambda (var)
                               (or (gethash var *datum-values*)
                                   (error "BUG: Cell missing: ~a" var)))
                             environment)))))
          enclose)
        ;; When the function has no environment, it can be compiled and
        ;; referenced as literal.
        (clasp-cleavir::%closurette-value enclosed-function
                                          function-description))))

(defmethod translate-simple-instruction :before
    ((instruction cleavir-bir:abstract-call)
     return-value abi)
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

;; Create the argument list for a local call by parsing the callee's
;; lambda list and filling in the correct values at compile time. We
;; assume that we have already checked the validity of this call.
(defun parse-local-call-arguments (instruction callee)
  (let* ((lambda-list (cleavir-bir:lambda-list callee))
         (callee-info (find-llvm-function-info callee))
         (present-arguments (rest (cleavir-bir:inputs instruction)))
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
                    (push (cmp::irc-nil) arguments)
                    (push (cmp::irc-nil) arguments))))
            (&key
             (error "I don't know how to do this."))
            (&rest
             (error "I don't know how to do this either.")))))
    ;; Augment the environment values to the arguments of the
    ;; call. Make sure to get the variable location and not
    ;; necessarily the value.
    (nconc (mapcar (lambda (variable)
                     (or (gethash variable *datum-values*)
                         (error "Closure value or cell missing: ~a" variable)))
                   environment)
           (nreverse arguments))))

(defmethod translate-simple-instruction ((instruction cleavir-bir:local-call)
                                         return-value abi)
  (let* ((callee (cleavir-bir:callee instruction))
         (callee-info (find-llvm-function-info callee))
         (arguments (parse-local-call-arguments instruction callee))
         (lambda-name (get-or-create-lambda-name callee))
         (function (main-function callee-info))
         (result-in-registers
           (if cmp::*current-unwind-landing-pad-dest*
               (cmp:irc-create-invoke function arguments cmp::*current-unwind-landing-pad-dest*)
               ;; FIXME: USE FASTCC CONVENTION HERE.
               (cmp:irc-create-call function arguments))))
    (clasp-cleavir::store-tmv result-in-registers return-value)))

(defmethod translate-simple-instruction ((instruction cleavir-bir:call)
                                         return-value abi)
  (declare (ignore abi))
  (let ((inputs (cleavir-bir:inputs instruction)))
    (clasp-cleavir::closure-call-or-invoke
     (in (first inputs)) return-value (mapcar #'in (rest inputs)))))

(defmethod translate-simple-instruction ((instruction cleavir-bir:mv-call)
                                         return-value abi)
  (let ((call-result (clasp-cleavir::%intrinsic-invoke-if-landing-pad-or-call
                      "cc_call_multipleValueOneFormCallWithRet0"
                      (list (in (first (cleavir-bir:inputs instruction)))
                            (clasp-cleavir::load-return-value return-value)))))
    ;; call-result is a T_mv, and return-value a T_mv*
    (clasp-cleavir::store-tmv call-result return-value)
    call-result))

(defmethod translate-simple-instruction ((instruction cc-bir:mv-foreign-call)
                                         return-value abi)
  (clasp-cleavir:unsafe-multiple-value-foreign-call
   (cc-bir:function-name instruction)
   return-value (mapcar #'in (cleavir-bir:inputs instruction)) abi))

(defmethod translate-simple-instruction
    ((instruction cc-bir:foreign-call-pointer) return-value abi)
  (let ((inputs (cleavir-bir:inputs instruction)))
    (clasp-cleavir:unsafe-foreign-call-pointer
     :call (cc-bir:foreign-types instruction) (in (first inputs))
     (mapcar #'in (rest inputs)) abi)))

(defmethod translate-simple-instruction
    ((instruction cc-bir:defcallback)
     return-value (abi clasp-cleavir::abi-x86-64))
  (let* ((args (cc-bir:defcallback-args instruction))
         (closure (in (first (cleavir-bir:inputs instruction)))))
    (cmp::gen-defcallback
     (first args) (second args) (third args) (fourth args)
     (fifth args) (sixth args) (seventh args) (eighth args)
     closure)))

(defmethod translate-simple-instruction
    ((instruction cleavir-bir:fixed-to-multiple)
     return-value (abi clasp-cleavir::abi-x86-64))
  (let* ((inputs (cleavir-bir:inputs instruction))
         (ninputs (length inputs)))
    (clasp-cleavir::with-return-values (return-value abi nret ret-regs)
      (cmp:irc-store (clasp-cleavir::%size_t ninputs) nret)
      (dotimes (i ninputs)
        (cmp:irc-store (in (elt inputs i))
                       (clasp-cleavir::return-value-elt ret-regs i)))
      (loop for i from ninputs below clasp-cleavir::+pointers-returned-in-registers+
            do (cmp:irc-store (clasp-cleavir::%nil)
                              (clasp-cleavir::return-value-elt ret-regs i))))))

(defmethod translate-simple-instruction
    ((instr cleavir-bir:multiple-to-fixed) return-value (abi clasp-cleavir::abi-x86-64))
  ;; Outputs that are returned in registers (see +pointers-returned-in-registers+) can be
  ;; unconditionally assigned, as things that return values ensure that those return registers
  ;; are always valid - e.g., (values) explicitly sets them to NIL.
  ;; Beyond that, we have to branch on nret.
  (clasp-cleavir::with-return-values (return-value abi nret return-regs)
    (let* ((outputs (cleavir-bir:outputs instr))
           (nouts (length outputs)))
      ;; The easy ones.
      (loop for out in outputs
            for i below clasp-cleavir::+pointers-returned-in-registers+
            do (out (cmp:irc-load (clasp-cleavir::return-value-elt return-regs i)) out))
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
      (when (> nouts clasp-cleavir::+pointers-returned-in-registers+)
        (let* ((rets (loop for i from clasp-cleavir::+pointers-returned-in-registers+ below nouts
                           collect (clasp-cleavir::return-value-elt return-regs i)))
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
                 (loop for retn from clasp-cleavir::+pointers-returned-in-registers+ below nouts
                       for block = (cmp:irc-basic-block-create (format nil "mtf-~d" retn))
                       do (cmp:irc-add-case switch (clasp-cleavir::%size_t retn) block)
                          (cmp:irc-begin-block block)
                       collect (cons block
                                     (loop for ret in rets
                                           for i from clasp-cleavir::+pointers-returned-in-registers+ below nouts
                                           collect (if (< i retn) (cmp:irc-load ret) (clasp-cleavir::%nil))))
                       do (cmp:irc-br final))))
          ;; Set up all the register-only cases to use the first block.
          ;; (which sets all the outputs to NIL)
          (loop with low = (caar blocks-and-vars)
                for retn from 0 below clasp-cleavir::+pointers-returned-in-registers+
                do (cmp:irc-add-case switch (clasp-cleavir::%size_t retn) low))
          ;; Final generation: generate the phis and then output them.
          ;; NOTE: We can't output as we generate because (out ...) may generate a store,
          ;; and phis must not have any stores (or anything but a phi) preceding them.
          (cmp:irc-begin-block final)
          (let* ((vector-outs (nthcdr clasp-cleavir::+pointers-returned-in-registers+ outputs))
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
  (translate-primop (cleavir-bir:name (cleavir-bir::info inst)) inst))

(defmethod translate-primop ((name (eql 'symbol-value)) inst)
  (clasp-cleavir::%intrinsic-invoke-if-landing-pad-or-call
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
  (translate-primop (cleavir-bir:name (cleavir-bir::info inst)) inst))

(defmethod translate-primop ((name cons) inst) ; FIXME
  (cond ((equal name '(setf symbol-value))
         (clasp-cleavir::%intrinsic-invoke-if-landing-pad-or-call
          "cc_setSymbolValue" (mapcar #'in (cleavir-bir:inputs inst))))
        ((equal name '(setf clos:standard-instance-access))
         (let ((inputs (cleavir-bir:inputs inst)))
           (cmp::gen-instance-set (in (first inputs)) (in (second inputs))
                                  (in (third inputs)))))
        (t
         (error "BUG: Don't know how to translate primop ~a" name))))

(defmethod translate-simple-instruction ((inst cleavir-bir:writetemp)
                                         return-value abi)
  (let ((alloca (cleavir-bir:alloca inst)))
    (check-type alloca cleavir-bir:alloca)
    ;; only handling m-v-prog1 for the moment
    (assert (eq (cleavir-bir:rtype alloca) :multiple-values))
    (destructuring-bind (stackpos storage1 storage2)
        (dynenv-storage alloca)
      (declare (ignore stackpos storage1))
      (clasp-cleavir::with-return-values (return-value abi nvalsl return-regs)
        (clasp-cleavir::%intrinsic-call
         "cc_save_values"
         (list (cmp:irc-load nvalsl)
               (cmp:irc-load (clasp-cleavir::return-value-elt return-regs 0))
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
      (clasp-cleavir::store-tmv
       (clasp-cleavir::%intrinsic-call "cc_load_values"
                                       (list storage1 storage2))
       return-value))))

(defmethod translate-simple-instruction ((inst cleavir-bir:load-time-value)
                                         return-value abi)
  (declare (ignore return-value abi))
  (let ((index (gethash inst *constant-values*))
        (label ""))
    (cmp:irc-load
     (cmp:irc-gep-variable (literal:ltv-global)
                           (list (clasp-cleavir::%size_t 0)
                                 (clasp-cleavir::%i64 index))
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
                               (list (clasp-cleavir::%size_t 0)
                                     (clasp-cleavir::%i64 immediate-or-index))
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
        ;; FIXME: USE FASTCC CONVENTION HERE.
        (cmp:irc-ret
         (cmp:irc-create-call
          (main-function llvm-function-info)
          ;; Augment the environment lexicals as a local call would.
          (nconc environment-values (mapcar #'in (arguments llvm-function-info))))))))
  the-function)

(defun layout-main-function* (the-function ir
                              body-irbuilder body-block
                              abi &key (linkage 'llvm-sys:internal-linkage))
  (cmp:with-irbuilder (cmp:*irbuilder-function-alloca*)
    (let ((return-value (clasp-cleavir::alloca-return)))
      (clasp-cleavir::with-return-values (return-value abi nret ret-regs)
        (declare (ignore ret-regs))
        (cmp:irc-store (clasp-cleavir::%size_t 0) nret))
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
         (clasp-cleavir::*function-current-multiple-value-array-address*
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
                     'clasp-cleavir::save-register-args)
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
         (clasp-cleavir::*function-current-multiple-value-array-address*
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
                 (clasp-cleavir::%i64 immediate)
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
            (literal:with-load-time-value
                (clasp-cleavir::compile-form form clasp-cleavir::*clasp-env*))))))

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
  (cc-bir-to-bmir:reduce-module-typeqs module)
  (cc-bir-to-bmir:reduce-module-primops module)
  ;; These should happen last since they are like "post passes".
  (cleavir-bir-transformations:process-captured-variables module)
  (cleavir-bir-transformations:dynamic-extent-analyze-closures module)
  (values))

(defun translate-ast (ast &key (abi clasp-cleavir::*abi-x86-64*)
                               (linkage 'llvm-sys:internal-linkage)
                               (system clasp-cleavir::*clasp-system*))
  (let* ((bir (ast->bir ast system))
         (module (cleavir-bir:module bir)))
    ;;(cleavir-bir:verify module)
    (bir-transformations module)
    (cleavir-bir:verify module)
    (translate bir :abi abi :linkage linkage)))

(defun bir-compile (form env pathname
                    &key (linkage 'llvm-sys:internal-linkage))
  (let* (function
         ordered-raw-constants-list constants-table startup-fn shutdown-fn
         (cleavir-cst-to-ast:*compiler* 'cl:compile)
         (cst (cst:cst-from-expression form))
         (ast (clasp-cleavir::cst->ast cst env)))
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
  (let ((cleavir-cst-to-ast:*compiler* 'cl:compile)
        (core:*use-cleavir-compiler* t))
    (cmp:compile-in-env form env #'bir-compile cmp:*default-compile-linkage*)))

(defun compile-file-cst (cst &optional (env clasp-cleavir::*clasp-env*))
  (let ((cmp:*default-condition-origin* (origin-spi (cst:source cst))))
    (literal:with-top-level-form
        (let* ((pre-ast (clasp-cleavir::cst->ast cst env))
               (ast (clasp-cleavir::wrap-ast pre-ast)))
          (translate-ast ast :linkage cmp:*default-linkage*)))))

(defun bir-loop-read-and-compile-file-forms (source-sin environment)
  (let ((eof-value (gensym))
        (eclector.reader:*client* clasp-cleavir::*cst-client*)
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
