(in-package #:clasp-cleavir)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on TRANSLATE-SIMPLE-INSTRUCTION.

(defmethod translate-simple-instruction
    ((instr cleavir-ir:enter-instruction) return-value (abi abi-x86-64) function-info)
  (let ((lambda-list (cleavir-ir:lambda-list instr))
        (closed-env-dest (cleavir-ir:static-environment instr))
        (dynenv (cleavir-ir:dynamic-environment-output instr))
        (calling-convention (calling-convention function-info)))
    (out (cmp:calling-convention-closure calling-convention) closed-env-dest)
    ;; see comment in catch-instruction
    (out (cmp:irc-undef-value-get cmp:%t*%) dynenv)
    ;; actual argument parsing
    (cmp:compile-lambda-list-code lambda-list calling-convention
                                  :argument-out #'out)))

(defmethod translate-simple-instruction
    ((instr clasp-cleavir-hir:bind-va-list-instruction) return-value (abi abi-x86-64) function-info)
  (let* ((lambda-list          (cleavir-ir:lambda-list instr))
         (vaslist              (first (cleavir-ir:inputs instr)))
         (vaslist-value        (in vaslist))
         (src-remaining-nargs* (cmp:irc-vaslist-remaining-nargs-address vaslist-value))
         (src-va_list*         (cmp:irc-vaslist-va_list-address vaslist-value))
         (local-va_list*       (cmp:alloca-va_list "local-va_list"))
         (_                    (%intrinsic-call "llvm.va_copy"
                                                (list (cmp:irc-pointer-cast local-va_list* cmp:%i8*%)
                                                      (cmp:irc-pointer-cast src-va_list* cmp:%i8*%))))
         (callconv             (cmp:make-calling-convention-impl
                                :nargs (cmp:irc-load src-remaining-nargs*)
                                :va-list* local-va_list*
                                :rest-alloc (clasp-cleavir-hir:rest-alloc instr))))
    (cmp:compile-lambda-list-code lambda-list callconv
                                  ;; BIND-VA-LIST is used exclusively internally, and furthermore,
                                  ;; in method bodies. In that case the generic function does the
                                  ;; checking anyway, so there's no point in each method repeating.
                                  :safep nil
                                  :argument-out #'out)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:instruction) return-value abi function-info)
  (error "Implement instruction: ~a for abi: ~a~%" instruction abi))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:assignment-instruction) return-value abi function-info)
  (out (in (first (cleavir-ir:inputs instruction))) (first (cleavir-ir:outputs instruction))))

(defun safe-llvm-name (obj)
  "Generate a name for the object that can be used as a variable label in llvm"
  (cond
    ((stringp obj) obj)
    ((symbolp obj) (string obj))
    ((listp obj) "list")
    ((arrayp obj) "array")
    (t "object")))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:precalc-value-instruction) return-value abi function-info)
  (let* ((index (clasp-cleavir-hir:precalc-value-instruction-index instruction))
         (label (safe-llvm-name (clasp-cleavir-hir:precalc-value-instruction-original-object instruction)))
         (value (cmp:irc-load
                 (cmp:irc-gep-variable (literal:ltv-global) (list (%size_t 0) (%i64 index)) label))))
    (out value (first (cleavir-ir:outputs instruction)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:fixed-to-multiple-instruction) return-value (abi abi-x86-64) function-info)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (ninputs (length inputs)))
    (with-return-values (return-value abi nret ret-regs)
      (cmp:irc-store (%size_t ninputs) nret)
      ;; store inputs in the registers
      (dotimes (i ninputs)
        (cmp:irc-store (in (elt inputs i)) (return-value-elt ret-regs i)))
      ;; if there are more pointers returned in registers than inputs,
      ;; fill the remainder with nils.
      (loop for i from ninputs below +pointers-returned-in-registers+
            do (cmp:irc-store (%nil) (return-value-elt ret-regs i))))))

(defmethod translate-simple-instruction
    ((instr cleavir-ir:multiple-to-fixed-instruction) return-value (abi abi-x86-64) function-info)
  ;; Outputs that are returned in registers (see +pointers-returned-in-registers+) can be
  ;; unconditionally assigned, as things that return values ensure that those return registers
  ;; are always valid - e.g., (values) explicitly sets them to NIL.
  ;; Beyond that, we have to branch on nret.
  (with-return-values (return-value abi nret return-regs)
    (let* ((outputs (cleavir-ir:outputs instr))
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
                            for phi = (cmp:irc-phi cmp:%t*% (1+ nouts) (datum-name-as-string out))
                            do (loop for (block . vars) in blocks-and-vars
                                     do (cmp:irc-phi-add-incoming phi (elt vars i) block))
                               (cmp:irc-phi-add-incoming phi (elt default-vars i) default)
                            collect phi)))
            (loop for phi in phis
                  for out in vector-outs
                  do (out phi out))))))))

(defmethod translate-simple-instruction
    ((instr clasp-cleavir-hir:save-values-instruction) return-value abi function-info)
  (declare (ignore function-info))
  (with-return-values (return-value abi nvalsl return-regs)
    (let* ((outputs (cleavir-ir:outputs instr))
           (nvals (cmp:irc-load nvalsl))
           (temp-nvals-loc (first outputs))
           (temp-vals-loc (second outputs))
           ;; Get the part we need.
           (primary (cmp:irc-load (return-value-elt return-regs 0)))
           ;; Allocate storage. Note this is in-line, not in the alloca-irbuilder,
           ;; of course because it's of variable size.
           (mv-temp (cmp:alloca-temp-values nvals)))
      ;; Do the actual storing into mv-temp
      (%intrinsic-call "cc_save_values" (list nvals primary mv-temp))
      ;; Put the stuff in the outputs
      (out nvals temp-nvals-loc)
      (out mv-temp temp-vals-loc))))

(defmethod translate-simple-instruction
    ((instr clasp-cleavir-hir:load-values-instruction) return-value abi function-info)
  (declare (ignore abi function-info))
  (let* ((inputs (cleavir-ir:inputs instr))
         (nvals-loc (first inputs))
         (vals-loc (second inputs))
         (loaded
           (%intrinsic-call "cc_load_values" (list (in nvals-loc) (in vals-loc)))))
    (store-tmv loaded return-value)))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:multiple-value-foreign-call-instruction) return-value (abi abi-x86-64) function-info)
  (check-type (clasp-cleavir-hir:function-name instruction) string)
  ;; NOTE: We use a maybe entry pad instead of a never entry one because a few foreign
  ;; calls may return to this function by calling a Lisp closure - for example,
  ;; cc_error_type_error, which calls ERROR and therefore handler closures.
  ;; See bug #935.
  ;; In the future, it may be worthwhile to statically list or determine which functions
  ;; can or can't return in this way, and thereby save a bit of processing.
  (cmp:with-landing-pad (maybe-entry-landing-pad
                         (cleavir-ir:dynamic-environment instruction)
                         return-value *tags* function-info)
    (clasp-cleavir:unsafe-multiple-value-foreign-call
     (clasp-cleavir-hir:function-name instruction)
     return-value (mapcar #'in (cleavir-ir:inputs instruction)) abi)))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:foreign-call-instruction) return-value (abi abi-x86-64) function-info)
  ;; NOTE: See landing pad note in multiple-value-foreign-call-instruction
  (cmp:with-landing-pad (maybe-entry-landing-pad
                         (cleavir-ir:dynamic-environment instruction)
                         return-value *tags* function-info)
    (let ((output (first (cleavir-ir:outputs instruction))))
      (out
       (clasp-cleavir:unsafe-foreign-call :call (clasp-cleavir-hir:foreign-types instruction)
                                          (clasp-cleavir-hir:function-name instruction)
                                          (mapcar #'in (cleavir-ir:inputs instruction)) abi
                                          :label (datum-name-as-string output))
       output))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:foreign-call-pointer-instruction) return-value (abi abi-x86-64) function-info)
  ;; NOTE: See landing pad note in multiple-value-foreign-call-instruction
  (cmp:with-landing-pad (maybe-entry-landing-pad
                         (cleavir-ir:dynamic-environment instruction)
                         return-value *tags* function-info)
    (let ((inputs (cleavir-ir:inputs instruction))
          (output (first (cleavir-ir:outputs instruction))))
      (out
       (clasp-cleavir:unsafe-foreign-call-pointer
        :call (clasp-cleavir-hir:foreign-types instruction) (in (first inputs))
        (mapcar #'in (rest inputs)) abi
        :label (datum-name-as-string output))
       output))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:defcallback-instruction) return-value (abi abi-x86-64) function-info)
  (let* ((args (clasp-cleavir-hir:defcallback-args instruction))
         (closure (in (first (cleavir-ir:inputs instruction)))))
    (cmp::gen-defcallback
     (first args) (second args) (third args) (fourth args)
     (fifth args) (sixth args) (seventh args) (eighth args)
     closure)))

;;; shared between funcall and funcall-no-return and some unwind-protect stuff
(defun gen-call (function arguments dynamic-environment return-value function-info)
  (cmp:with-landing-pad (maybe-entry-landing-pad
                         dynamic-environment return-value *tags* function-info)
    (closure-call-or-invoke function return-value arguments)))

(defun translate-funcall (instruction return-value abi function-info)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (function (first inputs))
         (dynamic-environment (cleavir-ir:dynamic-environment instruction))
         (arguments (cdr inputs)))
    (gen-call (in function) (mapcar #'in arguments)
              dynamic-environment return-value function-info)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:funcall-instruction) return-value (abi abi-x86-64) function-info)
  (translate-funcall instruction return-value abi function-info))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:nop-instruction) return-value abi function-info)
  (declare (ignore return-value inputs outputs abi function-info)))

;;; FIXME: Hey this is confusing with generate-unbind existing and all.
(defun gen-unbind (symbol old-value)
  ;; This function cannot throw, so no landing pad needed
  (%intrinsic-call "cc_resetTLSymbolValue" (list symbol old-value)))

(defun gen-protect (thunk dynamic-environment return-value function-info)
  ;; We basically do save-values, funcall, load-values, except we generate these
  ;; from landing pads so there's no HIR... FIXME??
  ;; NOTE that this is kind of really dumb. We save the values, i.e. alloca
  ;; a VLA, for every unwind protect executed. We could at least merge unwind
  ;; protects in the same frame - but what would be really smart would be
  ;; just having the exception object carry the values, so we can fuck with the
  ;; global (thread-local) values with impunity while unwinding.
  ;; Probably challenging to arrange in C++, though.
  (with-return-values (return-value abi nvalsl return-regs)
    (let* ((nvals (cmp:irc-load nvalsl))
           (primary (cmp:irc-load (return-value-elt return-regs 0)))
           (mv-temp (cmp:alloca-temp-values nvals)))
      (%intrinsic-call "cc_save_values" (list nvals primary mv-temp))
      ;; Values saved, now do the call
      (gen-call thunk nil dynamic-environment return-value function-info)
      ;; Now load the values
      (store-tmv
       (%intrinsic-call "cc_load_values" (list nvals mv-temp))
       return-value))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:local-unwind-instruction) return-value abi function-info)
  (declare (ignore inputs outputs abi))
  ;; FIXME: Move this into... somewhere. Separate pass maybe.
  (let* ((succ (first (cleavir-ir:successors instruction)))
         (outer-dynenv (cleavir-ir:dynamic-environment succ)))
    (loop for dynenv = (cleavir-ir:dynamic-environment instruction)
          ;; Note that this is the definer from the previous loop iteration.
            then (cleavir-ir:dynamic-environment definer)
          for definer = (dynenv-definer dynenv)
          until (eq dynenv outer-dynenv)
          do (etypecase definer
               ((or cleavir-ir:catch-instruction cleavir-ir:assignment-instruction))
               (clasp-cleavir-hir:bind-instruction
                (let ((symbol (in (first (cleavir-ir:inputs definer))))
                      (old-value (in (first (cleavir-ir:outputs definer)))))
                  (gen-unbind symbol old-value)))
               (clasp-cleavir-hir:unwind-protect-instruction
                (let ((thunk (in (first (cleavir-ir:inputs definer))))
                      ;; NOTE: Using this means we're doing EXIT-EXTENT:MEDIUM.
                      ;; See more extensive note in generate-protect (landing-pad.lisp).
                      (protection-dynenv (cleavir-ir:dynamic-environment definer)))
                  (gen-protect thunk protection-dynenv return-value function-info)))
               (cleavir-ir:enter-instruction
                (unless (eq dynenv outer-dynenv)
                  (error "BUG: Fucked up the dynenvs yet again. succ = ~a" succ)))))))

;;; Again, note that the frame-value is in the function-info rather than an actual location.
(defmethod translate-simple-instruction
    ((instruction cc-mir:save-frame-instruction) return-value abi function-info)
  (setf (frame-value function-info)
        ;; NOTE: Considered as a T_O*, the frame address could, hypothetically, have nonzero
        ;; low bits (though that's pretty unlikely given how machines work), and therefore be
        ;; some invalid object instead of a fixnum. But that's okay, since we only use this
        ;; for pointer equality anyway.
        ;; FIXME: The frame address is not adequate as a frame identifier. If a BLOCK extent
        ;; ends but a closure returning to it survives, a new frame could be established that
        ;; happens to have the same stack frame address as the disestablished one. If the
        ;; closure was then called, we'd "return" to that new frame, but the IP would be
        ;; deranged and bad things would happen. There should be an additional value to
        ;; distinguish frames - some arbitrary integer, like a counter or the time.
        (cmp:irc-bit-cast
         (%intrinsic-call "llvm.frameaddress" (list (%i32 0)) "frame")
         cmp:%t*%)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:create-cell-instruction) return-value abi function-info)
  (let* ((output (first (cleavir-ir:outputs instruction)))
         (result (%intrinsic-invoke-if-landing-pad-or-call
                  "cc_makeCell" nil (datum-name-as-string output))))
    (out result output)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:fdefinition-instruction) return-value abi function-info)
  ;; How do we figure out if we should use safe or unsafe version
  (let* ((symbol (in (first (cleavir-ir:inputs instruction)) "func-name"))
         (output (first (cleavir-ir:outputs instruction)))
         (symbol-function (cmp:irc-fdefinition symbol)))
      (out symbol-function output)))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:debug-message-instruction) return-value abi function-info)
  (let ((msg (cmp:jit-constant-unique-string-ptr (clasp-cleavir-hir:debug-message instruction))))
    (%intrinsic-call "debugMessage" (list msg))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:debug-break-instruction) return-value abi function-info)
  (%intrinsic-call "debugBreak"))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:setf-fdefinition-instruction) return-value abi function-info)
  (let* ((setf-symbol (in (first (cleavir-ir:inputs instruction)) "setf-func-name"))
         (output (first (cleavir-ir:outputs instruction)))
         (setf-symbol-function (cmp:irc-setf-fdefinition setf-symbol)))
    (out setf-symbol-function output)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:symbol-value-instruction) return-value abi function-info)
  (let* ((sym (in (first (cleavir-ir:inputs instruction)) "sym-name"))
         (output (first (cleavir-ir:outputs instruction)))
         (result (%intrinsic-invoke-if-landing-pad-or-call
                  "cc_safe_symbol_value" (list sym) (datum-name-as-string output))))
      (out result output)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:set-symbol-value-instruction) return-value abi function-info)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (sym (in (first inputs) "sym-name"))
         (val (in (second inputs) "value")))
    (%intrinsic-invoke-if-landing-pad-or-call "cc_setSymbolValue" (list sym val))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:bind-instruction) return-value abi function-info)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (outputs (cleavir-ir:outputs instruction))
         (sym (in (first inputs) "sym-name"))
         (val (in (second inputs) "value"))
         (old (first outputs)) (dynenv-out (second outputs)))
    ;; Neither of these intrinsics can signal.
    (out (%intrinsic-call "cc_TLSymbolValue" (list sym)
                                                   (datum-name-as-string old))
         old)
    (%intrinsic-call "cc_setTLSymbolValue" (list sym val))
    (out (cleavir-ir:dynamic-environment instruction)
         dynenv-out
         "sham-dynamic-environment")))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:unwind-protect-instruction) return-value abi function-info)
  (declare (ignore return-value abi function-info))
  ;; This is basically a NOP except we do need to keep up the dynamic environment.
  (out (cleavir-ir:dynamic-environment instruction)
       (first (cleavir-ir:outputs instruction))
       "sham-dynamic-environment"))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:enclose-instruction) return-value abi function-info)
  (let* ((enter-instruction (cleavir-ir:code instruction))
         (lambda-name (get-or-create-lambda-name enter-instruction))
         (enclosed-function (memoized-layout-procedure enter-instruction lambda-name abi))
         (function-description (llvm-sys:get-named-global cmp:*the-module* (cmp::function-description-name enclosed-function)))
         (ninputs (cleavir-ir:closure-size enter-instruction))
         (dx-p (cleavir-ir:dynamic-extent-p instruction))
         (enclose-args
           (list enclosed-function
                 (cmp:irc-bit-cast function-description cmp:%i8*%)
                 (%size_t ninputs)))
         (result
           (progn
             (cond
               (dx-p
                ;; Closure is dynamic extent, so we can use stack storage.
                (%intrinsic-call
                 "cc_stack_enclose"
                 (list* (cmp:alloca-i8 (core:closure-with-slots-size ninputs) "stack-allocated-closure")
                        enclose-args)
                 (format nil "closure->~a" lambda-name)))
               (t
                ;; General case.
                (%intrinsic-invoke-if-landing-pad-or-call "cc_enclose" enclose-args))))))
    (out result (first (cleavir-ir:outputs instruction)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:initialize-closure-instruction) return-from abi function-info)
  (let* ((closure (in (first (cleavir-ir:inputs instruction))))
         (loaded-inputs (mapcar (lambda (x) (in x "closure_var"))
                                (rest (cleavir-ir:inputs instruction))))
         (ninputs (length loaded-inputs))
         (closure-vars
           (list* closure
                  (%size_t ninputs)
                  loaded-inputs)))
    (%intrinsic-invoke-if-landing-pad-or-call "cc_initialize_closure"
                                              closure-vars)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:multiple-value-call-instruction) return-value abi function-info)
  ;; second input is the dynamic-environment argument.
  (cmp:with-landing-pad (maybe-entry-landing-pad (cleavir-ir:dynamic-environment instruction)
                                                 return-value *tags* function-info)
    (let ((call-result (%intrinsic-invoke-if-landing-pad-or-call
                        "cc_call_multipleValueOneFormCallWithRet0" 
                        (list (in (first (cleavir-ir:inputs instruction)))
                              (load-return-value return-value)))))
      ;; call-result is a T_mv, and return-valuea  T_mv*
      (store-tmv call-result return-value)
      (cc-dbg-when *debug-log*
                   (format *debug-log*
                           "    translate-simple-instruction multiple-value-call-instruction: ~a~%" 
                           (cc-mir:describe-mir instruction))
                   (format *debug-log* "     instruction --> ~a~%" call-result)))))

(defun gen-vector-effective-address (array index element-type fixnum-type)
  (let* ((type (llvm-sys:type-get-pointer-to (cmp::simple-vector-llvm-type element-type)))
         (cast (cmp:irc-bit-cast array type))
         (untagged (cmp:irc-untag-fixnum index fixnum-type "vector-index")))
    ;; 0 is for LLVM reasons, that pointers are C arrays. or something.
    ;; For layout of the vector, check simple-vector-llvm-type's definition.
    ;; untagged is the actual offset.
    (cmp:irc-gep-variable cast (list (%i32 0) (%i32 cmp::+simple-vector-data-slot+) untagged) "aref")))

(defun translate-bit-aref (array index &optional (label ""))
  (let* ((untagged (cmp:irc-untag-fixnum index cmp:%size_t%))
         (bit (%intrinsic-call "cc_simpleBitVectorAref" (list array untagged) "bit-aref")))
    (cmp:irc-tag-fixnum (cmp:irc-sext bit) "bit")))

(defun translate-bit-aset (value array index)
  (let ((offset (cmp:irc-untag-fixnum index cmp:%size_t% "untagged-offset"))
        (untagged-value (cmp:irc-untag-fixnum value cmp:%i8% "untagged-value")))
    ;; Note: We cannot label void calls, because then they'll get a variable
    (%intrinsic-call "cc_simpleBitVectorAset" (list array offset untagged-value))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:aref-instruction) return-value abi function-info)
  (let* ((et (cleavir-ir:element-type instruction))
         (inputs (cleavir-ir:inputs instruction))
         (output (first (cleavir-ir:outputs instruction)))
         (label (datum-name-as-string output)))
    (out
     (cond ((eq et 'bit) ; have to special case due to the layout.
            (translate-bit-aref (in (first inputs)) (in (second inputs)) label))
           ((member et '(ext:byte2 ext:integer2 ext:byte4 ext:integer4))
            (error "BUG: Inline array access for 2/4-bit arrays has not been implemented"))
           (t
            (let ((addr
                    (gen-vector-effective-address (in (first inputs)) (in (second inputs))
                                                  et (%default-int-type abi))))
              ;; FIXME: To do atomic loads with other types, we need to know alignment.
              (if (eq et 't)
                  (cmp:irc-load-atomic addr label)
                  (cmp:irc-load addr label)))))
     output)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:aset-instruction) return-value abi function-info)
  (let ((et (cleavir-ir:element-type instruction))
        (inputs (cleavir-ir:inputs instruction)))
    (cond ((eq et 'bit) ; have to special case due to the layout.
           (translate-bit-aset (in (third inputs)) (in (first inputs)) (in (second inputs))))
          ((member et '(ext:byte2 ext:integer2 ext:byte4 ext:integer4))
           (error "BUG: Inline array access for 2/4-bit arrays has not been implemented"))
          (t
           (let ((addr
                   (gen-vector-effective-address (in (first inputs)) (in (second inputs))
                                                 et (%default-int-type abi)))
                 (v (in (third inputs))))
             ;; FIXME: To do atomic stores with other types, we need to know alignment.
             (if (eq et 't)
                 (cmp:irc-store-atomic v addr)
                 (cmp:irc-store v addr)))))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:acas-instruction) return-value abi function-info)
  (declare (ignore return-value function-info))
  (let ((et (cleavir-ir:element-type instruction))
        (inputs (cleavir-ir:inputs instruction)))
    (out
     (cmp:irc-cmpxchg
      ;; This will err if et = bit or the like.
      (gen-vector-effective-address (in (first inputs)) (in (second inputs))
                                    et (%default-int-type abi))
      (in (third inputs)) (in (fourth inputs)))
     (first (cleavir-ir:outputs instruction)))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:vector-length-instruction) return-value abi function-info)
  (declare (ignore return-value function-info))
  (out (cmp::gen-vector-length (in (first (cleavir-ir:inputs instruction))))
       (first (cleavir-ir:outputs instruction))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:displacement-instruction) return-value abi function-info)
  (declare (ignore return-value function-info abi))
  (out (cmp:irc-real-array-displacement (in (first (cleavir-ir:inputs instruction))))
       (first (cleavir-ir:outputs instruction))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:displaced-index-offset-instruction) return-value abi function-info)
  (declare (ignore return-value function-info abi))
  (out (cmp:irc-tag-fixnum
        (cmp:irc-real-array-index-offset (in (first (cleavir-ir:inputs instruction)))))
       (first (cleavir-ir:outputs instruction))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:array-total-size-instruction) return-value abi function-info)
  (declare (ignore return-value function-info abi))
  (out (cmp:irc-tag-fixnum
        (cmp:irc-array-total-size (in (first (cleavir-ir:inputs instruction)))))
       (first (cleavir-ir:outputs instruction))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:array-rank-instruction) return-value abi function-info)
  (declare (ignore return-value function-info abi))
  (out (cmp:irc-tag-fixnum (cmp:irc-array-rank (in (first (cleavir-ir:inputs instruction)))))
       (first (cleavir-ir:outputs instruction))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:array-dimension-instruction) return-value abi function-info)
  (declare (ignore return-value function-info abi))
  (let ((inputs (cleavir-ir:inputs instruction))
        (output (first (cleavir-ir:outputs instruction))))
    (out (cmp:gen-%array-dimension (in (first inputs)) (in (second inputs)))
         output)))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:header-stamp-instruction) return-value abi function-info)
  (declare (ignore return-value abi function-info))
  (out (cmp:irc-header-stamp (in (first (cleavir-ir:inputs instruction))))
       (first (cleavir-ir:outputs instruction))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:rack-stamp-instruction) return-value abi function-info)
  (declare (ignore return-value abi function-info))
  (out (cmp:irc-rack-stamp (in (first (cleavir-ir:inputs instruction))))
       (first (cleavir-ir:outputs instruction))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:wrapped-stamp-instruction) return-value abi function-info)
  (declare (ignore return-value abi function-info))
  (out (cmp:irc-wrapped-stamp (in (first (cleavir-ir:inputs instruction))))
       (first (cleavir-ir:outputs instruction))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:derivable-stamp-instruction) return-value abi function-info)
  (declare (ignore return-value abi function-info))
  (out (cmp:irc-derivable-stamp (in (first (cleavir-ir:inputs instruction))))
       (first (cleavir-ir:outputs instruction))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:vaslist-pop-instruction) return-value abi function-info)
  (declare (ignore return-value function-info))
  (out (cmp:gen-vaslist-pop (in (first (cleavir-ir:inputs instruction))))
       (first (cleavir-ir:outputs instruction))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:vaslist-length-instruction) return-value abi function-info)
  (declare (ignore return-value function-info))
  (out (cmp:gen-vaslist-length (in (first (cleavir-ir:inputs instruction))))
       (first (cleavir-ir:outputs instruction))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:slot-read-instruction) return-value abi function-info)
  (declare (ignore return-value abi function-info))
  (let ((inputs (cleavir-ir:inputs instruction)))
    (out (cmp::gen-instance-ref (in (first inputs)) (in (second inputs)))
         (first (cleavir-ir:outputs instruction)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:slot-write-instruction) return-value abi function-info)
  (declare (ignore return-value abi function-info))
  (let ((inputs (cleavir-ir:inputs instruction)))
    (cmp::gen-instance-set (in (first inputs)) (in (second inputs)) (in (third inputs)))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:slot-cas-instruction) return-value abi function-info)
  (declare (ignore return-value abi function-info))
  (let ((inputs (cleavir-ir:inputs instruction)))
    (out (cmp::gen-instance-cas (in (first inputs)) (in (second inputs))
                                (in (third inputs)) (in (fourth inputs)))
         (first (cleavir-ir:outputs instruction)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:memref2-instruction) return-value abi function-info)
  (declare (ignore return-value abi function-info))
  (out (cmp:irc-load-atomic
        (cmp::gen-memref-address (in (first (cleavir-ir:inputs instruction)))
                                 (cleavir-ir:offset instruction)))
       (first (cleavir-ir:outputs instruction))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:memset2-instruction) return-value abi function-info)
  (declare (ignore return-value abi function-info))
  (let ((inputs (cleavir-ir:inputs instruction)))
    (cmp:irc-store-atomic
     (in (second inputs) "memset2-val")
     (cmp::gen-memref-address (in (first inputs))
                              (cleavir-ir:offset instruction)))))

(defmethod translate-simple-instruction
    ((instruction cc-mir:memcas2-instruction) return-value abi function-info)
  (declare (ignore return-value abi function-info))
  (let* ((inputs (cleavir-ir:inputs instruction))
         (cons (first inputs))
         (old (second inputs))
         (new (third inputs)))
    (out (cmp:irc-cmpxchg
          (cmp::gen-memref-address (in cons) (cleavir-ir:offset instruction))
          (in old) (in new))
         (first (cleavir-ir:outputs instruction)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:box-instruction) return-value abi function-info)
  (declare (ignore return-value abi function-info))
  (let ((intrinsic
          (ecase (cleavir-ir:element-type instruction)
            ((base-char) "to_object_claspChar")
            ((character) "to_object_claspCharacter")
            ((ext:byte8) "to_object_uint8")
            ((ext:integer8) "to_object_int8")
            ((ext:byte16) "to_object_uint16")
            ((ext:integer16) "to_object_int16")
            ((ext:byte32) "to_object_uint32")
            ((ext:integer32) "to_object_int32")
            ((fixnum) "to_object_fixnum")
            ((ext:byte64) "to_object_uint64")
            ((ext:integer64) "to_object_int64")
            ((single-float) "to_object_float")
            ((double-float) "to_object_double")))
        (output (first (cleavir-ir:outputs instruction))))
    (out
     (%intrinsic-invoke-if-landing-pad-or-call
      intrinsic (list (in (first (cleavir-ir:inputs instruction))))
      (datum-name-as-string output))
     output)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:unbox-instruction) return-value abi function-info)
  (declare (ignore return-value abi function-info))
  (let ((intrinsic
          (ecase (cleavir-ir:element-type instruction)
            ((base-char) "from_object_claspChar")
            ((character) "from_object_claspCharacter")
            ((ext:byte8) "from_object_uint8")
            ((ext:integer8) "from_object_int8")
            ((ext:byte16) "from_object_uint16")
            ((ext:integer16) "from_object_int16")
            ((ext:byte32) "from_object_uint32")
            ((ext:integer32) "from_object_int32")
            ((fixnum) "from_object_fixnum")
            ((ext:byte64) "from_object_uint64")
            ((ext:integer64) "from_object_int64")
            ((single-float) "from_object_float")
            ((double-float) "from_object_double")))
        (output (first (cleavir-ir:outputs instruction))))
    (out
     (%intrinsic-invoke-if-landing-pad-or-call
      intrinsic (list (in (first (cleavir-ir:inputs instruction))))
      (datum-name-as-string output))
     output)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:the-instruction) return-value abi function-info)
  (declare (ignore return-value abi function-info)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:the-values-instruction) return-value abi function-info)
  (declare (ignore return-value abi function-info)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:dynamic-allocation-instruction) return-value abi function-info)
  (declare (ignore return-value abi)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:unreachable-instruction) return-value abi function-info)
  (declare (ignore return-value abi function-info))
  (cmp:irc-unreachable))


;;; Floating point arithmetic


(defmacro define-fp-binop (instruction-class-name op)
  `(defmethod translate-simple-instruction
       ((instruction ,instruction-class-name) return-value abi function-info)
     (declare (ignore return-value abi function-info))
     (let ((inputs (cleavir-ir:inputs instruction))
           (output (first (cleavir-ir:outputs instruction))))
       (out (,op (in (first inputs)) (in (second inputs))
                 (datum-name-as-string output))
            output))))

;;; As it happens, we do the same IR generation for singles and doubles.
;;; This is because the LLVM value descriptors have associated types,
;;; so fadd of doubles is different from fadd of singles.
;;; The generated boxes and unboxes are where actual types are more critical.

(define-fp-binop cleavir-ir:float-add-instruction %fadd)
(define-fp-binop cleavir-ir:float-sub-instruction %fsub)
(define-fp-binop cleavir-ir:float-mul-instruction %fmul)
(define-fp-binop cleavir-ir:float-div-instruction %fdiv)


;;; Arithmetic conversion

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:coerce-instruction) return-value abi function-info)
  (declare (ignore return-value abi function-info))
  (let* ((input (in (first (cleavir-ir:inputs instruction))))
         (output (first (cleavir-ir:outputs instruction)))
         (label (datum-name-as-string output)))
    (out
     (ecase (cleavir-ir:from-type instruction)
       ((single-float)
        (ecase (cleavir-ir:to-type instruction)
          ((double-float)
           (%fpext input cmp::%double% label)))))
     output)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on TRANSLATE-BRANCH-INSTRUCTION.

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:eq-instruction) return-value successors abi function-info)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (left (in (first inputs))) (right (in (second inputs)))
         (ceq (cmp:irc-icmp-eq left right)))
    (cmp:irc-cond-br ceq (first successors) (second successors))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:consp-instruction) return-value successors abi function-info)
  (cmp:compile-tag-check (in (first (cleavir-ir:inputs instruction)))
                         cmp:+immediate-mask+ cmp:+cons-tag+
                         (first successors) (second successors)))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnump-instruction) return-value successors abi function-info)
  (cmp:compile-tag-check (in (first (cleavir-ir:inputs instruction)))
                         cmp:+fixnum-mask+ cmp:+fixnum-tag+
                         (first successors) (second successors)))

(defmethod translate-branch-instruction
    ((instruction cc-mir:characterp-instruction) return-value successors abi function-info)
  (cmp:compile-tag-check (in (first (cleavir-ir:inputs instruction)))
                         cmp:+immediate-mask+ cmp:+character-tag+
                         (first successors) (second successors)))

(defmethod translate-branch-instruction
    ((instruction cc-mir:single-float-p-instruction) return-value successors abi function-info)
  (cmp:compile-tag-check (in (first (cleavir-ir:inputs instruction)))
                         cmp:+immediate-mask+ cmp:+single-float-tag+
                         (first successors) (second successors)))

(defmethod translate-branch-instruction
    ((instruction cc-mir:generalp-instruction) return-value successors abi function-info)
  (declare (ignore return-value abi function-info))
  (cmp:compile-tag-check (in (first (cleavir-ir:inputs instruction)))
                         cmp:+immediate-mask+ cmp:+general-tag+
                         (first successors) (second successors)))

(defmethod translate-branch-instruction
    ((instruction cc-mir:headerq-instruction) return-value successors abi function-info)
  (declare (ignore return-value outputs abi function-info))
  (cmp:compile-header-check
   (cc-mir:header-value-min-max instruction)
   (in (first (cleavir-ir:inputs instruction))) (first successors) (second successors)))

(defmethod translate-branch-instruction
    ((instruction clasp-cleavir-hir:header-stamp-case-instruction)
     return-value successors abi function-info)
  (let* ((stamp (in (first (cleavir-ir:inputs instruction))))
         (stamp-i64 (cmp:irc-ptr-to-int stamp cmp:%i64%))
         (where (cmp:irc-and stamp-i64 (%i64 cmp:+where-tag-mask+)))
         (defaultb (cmp:irc-basic-block-create "impossible-default"))
         (sw (cmp:irc-switch where defaultb 4)))
    (cmp:irc-add-case sw (%i64 cmp:+derivable-where-tag+) (first successors))
    (cmp:irc-add-case sw (%i64 cmp:+rack-where-tag+) (second successors))
    (cmp:irc-add-case sw (%i64 cmp:+wrapped-where-tag+) (third successors))
    (cmp:irc-add-case sw (%i64 cmp:+header-where-tag+) (fourth successors))
    (cmp:irc-begin-block defaultb)
    (cmp:irc-unreachable)))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:unwind-instruction) return-value successors abi function-info)
  (declare (ignore successors function-info))
  ;; we don't use the second input to the unwind - the dynenv - at the moment.
  ;; Save whatever is in return-vals in the multiple-value array
  (save-multiple-value-0 return-value)
  (let ((static-index
          (instruction-go-index
           (nth (cleavir-ir:unwind-index instruction)
                (cleavir-ir:successors (cleavir-ir:destination instruction))))))
    (%intrinsic-call "cc_unwind" (list (in (first (cleavir-ir:inputs instruction)))
                                       (%size_t static-index))))
  (cmp:irc-unreachable))

;;; This is not a real branch: The real successors are only for convenience elsewhere.
;;; (HIR analysis, basically.)
;;; Also, the frame marker is unique to the frame, not to the catch, so we get it from
;;; the function-info.
(defmethod translate-branch-instruction
    ((instruction cleavir-ir:catch-instruction) return-value successors abi function-info)
  (out (frame-value function-info)
       (first (cleavir-ir:outputs instruction)) "frame-marker")
  ;; So, Cleavir treats the dynamic-environment as an actual runtime thing, as it would be
  ;; in some implementations. Not in Clasp- we just use the location to find the chain of
  ;; catch-instructions. But the location still exists.
  ;; So we fill it with an undef in enter, and then pass it along here. LLVM will remove it,
  ;; since nothing actually uses it.
  (out (cleavir-ir:dynamic-environment instruction)
       (second (cleavir-ir:outputs instruction))
       "sham-dynamic-environment")
  ;; unconditionally go to first successor
  (cmp:irc-br (first successors)))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:return-instruction) return-value successors abi function-info)
  (declare (ignore successors))
  (cmp:irc-ret (load-return-value return-value)))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:funcall-no-return-instruction)
     return-value successors (abi abi-x86-64) function-info)
  (declare (ignore successors))
  (translate-funcall instruction return-value abi function-info)
  (cmp:irc-unreachable))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:unreachable-instruction) return-value successors abi function-info)
  (declare (ignore return-value successors abi function-info))
  (cmp:irc-unreachable))


(defmethod translate-branch-instruction
    ((instruction clasp-cleavir-hir:throw-instruction) return-value successors abi function-info)
  (declare (ignore successors))
  (let ((inputs (cleavir-ir:inputs instruction)))
    (%intrinsic-call "cc_throw" (list (in (first inputs)) (in (second inputs)))))
  (cmp:irc-unreachable))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Fixnum comparison instructions

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-add-instruction) return-value successors abi function-info)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (x (cmp:irc-ptr-to-int (in (first inputs)) (%default-int-type abi)))
         (y (cmp:irc-ptr-to-int (in (second inputs)) (%default-int-type abi)))
         (result-with-overflow (%sadd.with-overflow x y abi)))
    (let ((val (cmp:irc-extract-value result-with-overflow (list 0) "result"))
          (overflow (cmp:irc-extract-value result-with-overflow (list 1) "overflow"))
          (output (first (cleavir-ir:outputs instruction))))
      (out (cmp:irc-int-to-ptr val cmp:%t*% (datum-name-as-string output)) output)
      (cmp:irc-cond-br overflow (second successors) (first successors)))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-sub-instruction) return-value successors abi function-info)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (x (cmp:irc-ptr-to-int (in (first inputs)) (%default-int-type abi)))
         (y (cmp:irc-ptr-to-int (in (second inputs)) (%default-int-type abi)))
         (result-with-overflow (%ssub.with-overflow x y abi)))
    (let ((val (cmp:irc-extract-value result-with-overflow (list 0) "result"))
          (overflow (cmp:irc-extract-value result-with-overflow (list 1) "overflow"))
          (output (first (cleavir-ir:outputs instruction))))
      (out (cmp:irc-int-to-ptr val cmp:%t*% (datum-name-as-string output)) output)
      (cmp:irc-cond-br overflow (second successors) (first successors)))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-less-instruction) return-value successors abi function-info)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (cmp-lt (cmp:irc-icmp-slt (in (first inputs)) (in (second inputs)))))
      (cmp:irc-cond-br cmp-lt (first successors) (second successors))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-not-greater-instruction) return-value successors abi function-info)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (cmp-lt (cmp:irc-icmp-sle (in (first inputs)) (in (second inputs)))))
      (cmp:irc-cond-br cmp-lt (first successors) (second successors))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-equal-instruction) return-value successors abi function-info)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (cmp-lt (cmp:irc-icmp-eq (in (first inputs)) (in (second inputs)))))
      (cmp:irc-cond-br cmp-lt (first successors) (second successors))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Float comparison instructions

;;; Thanks to LLVM semantics, we translate these identically for all
;;; float types. Types are more important for un/boxing.

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:float-less-instruction) return-value successors abi function-info)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (x (in (first inputs)))
         (y (in (second inputs)))
         (cmp (%fcmp-olt x y)))
    (cmp:irc-cond-br cmp (first successors) (second successors))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:float-not-greater-instruction) return-value successors abi function-info)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (x (in (first inputs)))
         (y (in (second inputs)))
         (cmp (%fcmp-ole x y)))
    (cmp:irc-cond-br cmp (first successors) (second successors))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:float-equal-instruction) return-value successors abi function-info)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (x (in (first inputs)))
         (y (in (second inputs)))
         (cmp (%fcmp-oeq x y)))
    (cmp:irc-cond-br cmp (first successors) (second successors))))
