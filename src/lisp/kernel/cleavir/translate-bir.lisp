(in-package #:clasp-cleavir-translate-bir)

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

(defun bind-if-necessary (var binder)
  (when (eq (cleavir-bir:binder var) binder)
    (if (cleavir-bir:closed-over-p var)
        (setf (gethash var *datum-values*)
              (if (cleavir-bir:immutablep var)
                  ;; This should get initialized eventually.
                  nil
                  ;; make a cell
                  (clasp-cleavir::%intrinsic-invoke-if-landing-pad-or-call
                   "cc_makeCell" nil "")))
        (if (cleavir-bir:immutablep var)
            (setf (gethash var *datum-values*)
                  ;; This should get initialized eventually.
                  nil)
            ;; just an alloca
            (setf (gethash var *variable-allocas*)
                  (cmp:alloca-t*))))))

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

(defun translate-sjlj-catch (variable return-value successors)
  ;; Call setjmp, switch on the result.
  (let ((bufp (cmp:alloca cmp::%jmp-buf-tag% 1 "jmp-buf")))
    (variable-out (cmp:irc-bit-cast bufp cmp:%t*%) variable)
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
     ;; Bind the variable if needed.
     (bind-if-necessary (first (cleavir-bir:outputs instruction)) instruction)
     (cond
       ((cleavir-bir-transformations:simple-unwinding-p instruction)
        (translate-sjlj-catch 
         (first (cleavir-bir:outputs instruction)) return-value next))
       (t
        ;; Fill the variable with the continuation.
        (variable-out (clasp-cleavir::%intrinsic-call
                       "llvm.frameaddress"
                       (list (clasp-cleavir::%i32 0)) "frame")
                      (first (cleavir-bir:outputs instruction)))
        ;; Unconditional branch to the normal successor;
        ;; dynamic environment stuff is handled in layout-iblock.
        (cmp:irc-br (first next)))))))

(defmethod translate-terminator ((instruction cleavir-bir:unwind)
                                 return-value abi next)
  (declare (ignore abi next))
  (let* ((inputs (cleavir-bir:inputs instruction))
         (cont (in (first inputs)))
         (rv (second inputs))
         (destination (cleavir-bir:destination instruction))
         (destination-id (get-destination-id destination)))
    ;; We can only transmit multiple values, so make sure the adapter in
    ;; bir.lisp forced that properly
    (ecase (length inputs)
      ;; GO
      (1)
      ;; RETURN-FROM
      (2 (assert (cleavir-bir:rtype= (cleavir-bir:rtype rv) :multiple-values))))
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
         (enclose-list (function-enclose-list code))
         (lambda-name (get-or-create-lambda-name code))
         (enclosed-function (memoized-layout-procedure code lambda-name abi))
         (function-description
           (llvm-sys:get-named-global
            cmp:*the-module* (cmp::function-description-name enclosed-function)))
         (ninputs (length enclose-list))
         (sninputs (clasp-cleavir::%size_t ninputs))
         (enclose-args
           (list enclosed-function
                 (cmp:irc-bit-cast function-description cmp:%i8*%)
                 sninputs))
         (enclose
           (clasp-cleavir::%intrinsic-invoke-if-landing-pad-or-call
            "cc_enclose" enclose-args)))
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
                       enclose-list)))))
    enclose))

(defmethod translate-simple-instruction :before
    ((instruction cleavir-bir:abstract-call)
     return-value abi)
  (declare (ignore instruction return-value abi))
  ;; We must force all closure initializers to run before a call.
  (force-initializers))

(defmethod translate-simple-instruction ((instruction cleavir-bir:leti)
                                         return-value abi)
  (declare (ignore return-value abi))
  (cleavir-set:mapset nil (lambda (v) (bind-if-necessary v instruction))
                      (cleavir-bir:bindings instruction)))

(defmethod translate-simple-instruction ((instruction cleavir-bir:writevar)
                                         return-value abi)
  (declare (ignore return-value abi))
  (variable-out (in (first (cleavir-bir:inputs instruction)))
                (first (cleavir-bir:outputs instruction))))

(defmethod translate-simple-instruction ((instruction cleavir-bir:readvar)
                                         return-value abi)
  (declare (ignore return-value abi))
  (variable-in (first (cleavir-bir:inputs instruction))))

(defmethod translate-simple-instruction ((instruction cleavir-bir:local-call)
                                         return-value abi)
  (let* ((callee (cleavir-bir:callee instruction))
         (arguments (mapcar #'in (rest (cleavir-bir:inputs instruction))))
         (real-args (cmp:irc-calculate-real-args arguments))
         (args (list* (cmp:irc-undef-value-get cmp:%i8*%)
                      (clasp-cleavir::%size_t (length arguments))
                      real-args))
         (lambda-name (get-or-create-lambda-name callee))
         (function (memoized-layout-procedure callee lambda-name abi))
         (result-in-registers
           (if cmp::*current-unwind-landing-pad-dest*
               (cmp:irc-create-invoke function args cmp::*current-unwind-landing-pad-dest*)
               (cmp:irc-create-call function args))))
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
  (cmp:gen-%array-dimension (in (first (cleavir-bir:inputs inst))
                                (second (cleavir-bir:inputs inst)))))
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
  (let ((alloca (cleavir-bir:dynamic-environment inst)))
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
  (let ((alloca (cleavir-bir:dynamic-environment inst)))
    (check-type alloca cleavir-bir:alloca)
    (assert (eq (cleavir-bir:rtype alloca) :multiple-values))
    (destructuring-bind (stackpos storage1 storage2)
        (dynenv-storage alloca)
      (declare (ignore stackpos))
      (clasp-cleavir::store-tmv
       (clasp-cleavir::%intrinsic-call "cc_load_values"
                                       (list storage1 storage2))
       return-value))))

(defmethod translate-simple-instruction ((inst cc-bir:precalc-value)
                                         return-value abi)
  (declare (ignore return-value abi))
  (let* ((index (cc-bir:precalc-value-index inst))
         (label ""))
    (cmp:irc-load
     (cmp:irc-gep-variable (literal:ltv-global)
                           (list (clasp-cleavir::%size_t 0)
                                 (clasp-cleavir::%i64 index))
                           label))))

(defun initialize-iblock-translation (iblock)
  (let ((phis (cleavir-bir:inputs iblock)))
    (unless (null phis)
      (cmp:irc-begin-block (iblock-tag iblock))
      (loop for phi in phis
            for ndefinitions = (cleavir-set:size (cleavir-bir:definitions phi))
            unless (eq (cleavir-bir:rtype phi) :multiple-values)
              do (setf (gethash phi *datum-values*)
                       (cmp:irc-phi cmp:%t*% ndefinitions))))))

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

(defun layout-procedure* (the-function ir calling-convention
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
            ;; Assign IDs to unwind destinations.
            (let ((i 0))
              (cleavir-set:doset (entrance (cleavir-bir:entrances ir))
                                 (setf (gethash entrance *unwind-ids*) i)
                                 (incf i)))
            ;; Allocate any new cells, and allocas for local variables.
            (cleavir-set:mapset nil (lambda (v) (bind-if-necessary v ir))
                                (cleavir-bir:variables ir))
            ;; Import cells.
            (let ((imports (gethash ir *function-enclose-lists*))
                  (closure-vec
                    (first (llvm-sys:get-argument-list the-function))))
              (loop for import in imports for i from 0
                    for offset = (cmp:%closure-with-slots%.offset-of[n]/t* i)
                    do (setf (gethash import *datum-values*)
                             (cmp:irc-load-atomic
                              (cmp::gen-memref-address closure-vec offset)))))
            ;; Parse lambda list.
            (cmp:compile-lambda-list-code (cleavir-bir:lambda-list ir)
                                          calling-convention
                                          :argument-out #'out)
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

(defun function-source-pos-info (irfunction)
  (declare (ignore irfunction))
  (core:make-source-pos-info "no-source-info-available" 999905 999905 999905))

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

(defun layout-procedure (ir lambda-name abi
                         &key (linkage 'llvm-sys:internal-linkage))
  (let* ((*tags* (make-hash-table :test #'eq))
         (*datum-values* (make-hash-table :test #'eq))
         (*variable-allocas* (make-hash-table :test #'eq))
         (*dynenv-storage* (make-hash-table :test #'eq))
         (llvm-function-name (cmp:jit-function-name lambda-name))
         (cmp:*current-function-name* llvm-function-name)
         (cmp:*gv-current-function-name*
           (cmp:module-make-global-string llvm-function-name "fn-name"))
         (llvm-function-type cmp:%fn-prototype%))
    (multiple-value-bind
          (the-function function-description)
        (cmp:irc-cclasp-function-create
         llvm-function-type
         linkage
         llvm-function-name
         cmp:*the-module*
         (calculate-function-info ir lambda-name))
      (let* ((cmp:*current-function* the-function)
             (cmp:*current-function-description* function-description)
             (entry-block (cmp:irc-basic-block-create "entry" the-function))
             (clasp-cleavir::*function-current-multiple-value-array-address*
               nil)
             (cmp:*irbuilder-function-alloca*
               (llvm-sys:make-irbuilder (cmp:thread-local-llvm-context)))
             (body-irbuilder (llvm-sys:make-irbuilder
                              (cmp:thread-local-llvm-context)))
             (body-block (cmp:irc-basic-block-create "body"))
             (source-pos-info (function-source-pos-info ir))
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
                     (cmp:irc-basic-block-create "iblock"))
               (initialize-iblock-translation ib))
             ir))
          (cmp:irc-set-insert-point-basic-block entry-block
                                                cmp:*irbuilder-function-alloca*)
          (cmp:with-irbuilder (cmp:*irbuilder-function-alloca*)
            (cmp:with-debug-info-source-position (source-pos-info)
              (let* ((fn-args (llvm-sys:get-argument-list the-function))
                     (lambda-list (cleavir-bir:lambda-list ir))
                     (calling-convention
                       (cmp:setup-calling-convention
                        fn-args
                        :cleavir-lambda-list lambda-list)))
                (layout-procedure* the-function ir calling-convention
                                   body-irbuilder body-block
                                   abi :linkage linkage)))))))))

(defun memoized-layout-procedure (bir lambda-name abi
                                  &key (linkage 'llvm-sys:internal-linkage))
  (or (gethash bir *compiled-enters*)
      (setf (gethash bir *compiled-enters*)
            (layout-procedure bir lambda-name abi :linkage linkage))))

(defun get-or-create-lambda-name (bir)
  (or (cleavir-bir:name bir) 'top-level))

(defun translate (bir &key abi linkage)
  (let* ((*function-enclose-lists* (make-hash-table :test #'eq))
         (*unwind-ids* (make-hash-table :test #'eq))
         (*compiled-enters* (make-hash-table :test #'eq))
         (lambda-name (get-or-create-lambda-name bir))
         (result
           (memoized-layout-procedure bir lambda-name abi :linkage linkage)))
    (cmp::potentially-save-module)
    result))

(defun ast->bir (ast system)
  (cleavir-ast-to-bir:compile-toplevel ast system))

(defun bir->bmir (ir env)
  (cleavir-bir:verify ir)
  (cleavir-bir-transformations:inline-functions ir)
  (cleavir-bir-transformations:delete-temporary-variables ir)
  (cc-bir-to-bmir:reduce-typeqs ir)
  (cc-bir-to-bmir:reduce-primops ir)
  (eliminate-load-time-value-inputs ir clasp-cleavir::*clasp-system* env)
  (cleavir-bir-transformations:process-captured-variables ir)
  (cleavir-bir-transformations:local-call-analyze-module (cleavir-bir:module ir))
  ir)

(defun translate-hoisted-ast (ast &key (abi clasp-cleavir::*abi-x86-64*)
                                  (linkage 'llvm-sys:internal-linkage)
                                    (env clasp-cleavir::*clasp-env*)
                                    (system clasp-cleavir::*clasp-system*))
  (let* ((bir (ast->bir ast system))
         (bmir (bir->bmir bir env)))
    (translate bmir :abi abi :linkage linkage)))

(defun translate-ast (ast &key (abi clasp-cleavir::*abi-x86-64*)
                          (linkage 'llvm-sys:internal-linkage)
                            (env clasp-cleavir::*clasp-env*))
  (let ((hoisted-ast (clasp-cleavir::hoist-ast ast env)))
    (translate-hoisted-ast hoisted-ast :abi abi :linkage linkage :env env)))

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
            (setq function (translate-ast ast :env env)))))
    (unless function
      (error "There was no function returned by translate-ast"))
    ;;(llvm-sys:dump-module cmp:*the-module* *standard-output*)
    (cmp:jit-add-module-return-function
     cmp:*the-module*
     function startup-fn shutdown-fn ordered-raw-constants-list)))

(defun bir-compile-in-env (form &optional env)
  (let (#-cst (cleavir-generate-ast:*compiler* 'cl:compile)
        #+cst (cleavir-cst-to-ast:*compiler* 'cl:compile)
        (core:*use-cleavir-compiler* t))
    (cmp:compile-in-env form env #'bir-compile cmp:*default-compile-linkage*)))

(defun compile-file-cst (cst &optional (env clasp-cleavir::*clasp-env*))
  (literal:with-top-level-form
    (let* ((pre-ast (clasp-cleavir::cst->ast cst env))
           (ast (clasp-cleavir::wrap-ast pre-ast)))
      (translate-ast ast :env env :linkage cmp:*default-linkage*))))

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
             #+cst
             (cst (eclector.concrete-syntax-tree:cst-read source-sin nil eof-value))
             #-cst
             (form (read source-sin nil eof-value)))
        #+debug-monitor(sys:monitor-message "source-pos ~a" core:*current-source-pos-info*)
        #+cst
        (if (eq cst eof-value)
            (return nil)
            (progn
              (when *compile-print* (cmp::describe-form (cst:raw cst)))
              (core:with-memory-ramp (:pattern 'gctools:ramp)
                (compile-file-cst cst environment))))
        #-cst
        (if (eq form eof-value)
            (return nil)
            (progn
              (when *compile-print* (cmp::describe-form form))
              (core:with-memory-ramp (:pattern 'gctools:ramp)
                (cleavir-compile-file-form form))))))))
