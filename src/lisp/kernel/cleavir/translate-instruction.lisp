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
  (let* ((lambda-list                   (cleavir-ir:lambda-list instr))
         (vaslist                       (first (cleavir-ir:inputs instr)))
         (vaslist-value                 (in vaslist))
         (src-remaining-nargs*          (%intrinsic-call "cc_vaslist_remaining_nargs_address" (list vaslist-value)))
         (src-va_list*                  (%intrinsic-call "cc_vaslist_va_list_address" (list vaslist-value) "vaslist_address"))
         (local-va_list*                (alloca-va_list "local-va_list"))
         (_                             (%intrinsic-call "llvm.va_copy" (list (%pointer-cast local-va_list* cmp:%i8*%)
                                                                              (%pointer-cast src-va_list* cmp:%i8*%))))
         (callconv                      (cmp:make-calling-convention-impl :nargs (%load src-remaining-nargs*)
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
         (value (%load (%gep-variable (cmp:ltv-global) (list (%size_t 0) (%i64 index)) label))))
    (out value (first (cleavir-ir:outputs instruction)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:fixed-to-multiple-instruction) return-value (abi abi-x86-64) function-info)
  (let ((inputs (cleavir-ir:inputs instruction)))
    ;; Write the first return value into the result
    (with-return-values (return-values return-value abi)
      (%store (%size_t (length inputs)) (number-of-return-values return-values))
      (dotimes (i (length inputs))
        (%store (in (elt inputs i)) (return-value-elt return-values i))))))

(defmethod translate-simple-instruction
    ((instr cleavir-ir:multiple-to-fixed-instruction) return-value (abi abi-x86-64) function-info)
  ;; We put in a switch on the number of return values. There's one case for each output, plus one.
  ;; The blocks out of the switch branch to a final block which has a phi for each output.
  ;; So if we have an MTF with one output, we'd have
  ;; switch number-of-return-values { case 0: go zero; default: go default;}
  ;; zero: out = nil; go final;
  ;; default: out = return-value-0; go final;
  (with-return-values (return-vals return-value abi)
    (let* ((outputs (cleavir-ir:outputs instr))
           (nouts (length outputs))
           (rets (loop for i below nouts collect (return-value-elt return-vals i)))
           (default (cmp:irc-basic-block-create "mtf-enough"))
           (switch (cmp:irc-switch (%load (number-of-return-values return-vals)) default nouts))
           (final (cmp:irc-basic-block-create "mtf-final"))
           (default-vars (prog2 (cmp:irc-begin-block default)
                             (mapcar #'%load rets)
                           (cmp:irc-br final)))
           (blocks-and-vars
             (loop for retn below nouts
                   for block = (cmp:irc-basic-block-create (format nil "mtf-~d" retn))
                   do (llvm-sys:add-case switch (%size_t retn) block)
                   do (cmp:irc-begin-block block)
                   collect (cons block
                                 (loop for ret in rets
                                       for i below nouts
                                       collect (if (< i retn) (%load ret) (%nil))))
                   do (cmp:irc-br final))))
      (cmp:irc-begin-block final)
      (loop for out in outputs
            for i from 0
            for phi = (cmp:irc-phi cmp:%t*% (1+ nouts) (datum-name-as-string out))
            do (loop for (block . vars) in blocks-and-vars
                     do (cmp:irc-phi-add-incoming phi (elt vars i) block))
               (cmp:irc-phi-add-incoming phi (elt default-vars i) default)
               (out phi out)))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:multiple-value-foreign-call-instruction) return-value (abi abi-x86-64) function-info)
  (check-type (clasp-cleavir-hir:function-name instruction) string)
  (clasp-cleavir:unsafe-multiple-value-foreign-call
   (clasp-cleavir-hir:function-name instruction)
   return-value (mapcar #'in (cleavir-ir:inputs instruction)) abi))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:foreign-call-instruction) return-value (abi abi-x86-64) function-info)
  ;; FIXME:  If this function has cleanup forms then this needs to be an INVOKE
  (let ((output (first (cleavir-ir:outputs instruction))))
    (out
     (clasp-cleavir:unsafe-foreign-call :call (clasp-cleavir-hir:foreign-types instruction)
                                        (clasp-cleavir-hir:function-name instruction)
                                        (mapcar #'in (cleavir-ir:inputs instruction)) abi
                                        :label (datum-name-as-string output))
     output)))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:foreign-call-pointer-instruction) return-value (abi abi-x86-64) function-info)
  ;; FIXME:  If this function has cleanup forms then this needs to be an INVOKE
  (let ((inputs (cleavir-ir:inputs instruction))
        (output (first (cleavir-ir:outputs instruction))))
    (out
     (clasp-cleavir:unsafe-foreign-call-pointer
      :call (clasp-cleavir-hir:foreign-types instruction) (in (first inputs))
      (mapcar #'in (rest inputs)) abi
      :label (datum-name-as-string output))
     output)))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:defcallback-instruction) return-value (abi abi-x86-64) function-info)
  (let* ((args (clasp-cleavir-hir:defcallback-args instruction))
         (closure (in (first (cleavir-ir:inputs instruction)))))
    (cmp::gen-defcallback
     (first args) (second args) (third args) (fourth args)
     (fifth args) (sixth args) (seventh args) (eighth args)
     closure)))

;;; shared between funcall and funcall-no-return
(defun translate-funcall (instruction return-value abi function-info)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (function (first inputs))
         (dynamic-environment (cleavir-ir:dynamic-environment instruction))
         (arguments (cdr inputs)))
    (cmp:with-landing-pad (landing-pad dynamic-environment return-value abi *tags* function-info)
      (closure-call-or-invoke (in function) return-value (mapcar #'in arguments) abi))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:funcall-instruction) return-value (abi abi-x86-64) function-info)
  (translate-funcall instruction return-value abi function-info))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:nop-instruction) return-value abi function-info)
  (declare (ignore return-value inputs outputs abi function-info)))

;;; Again, note that the frame-value is in the function-info rather than an actual location.
(defmethod translate-simple-instruction
    ((instruction cc-mir:save-frame-instruction) return-value abi function-info)
  (setf (frame-value function-info)
        (%intrinsic-call "cc_pushLandingPadFrame" nil "FRAME")))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:create-cell-instruction) return-value abi function-info)
  (let* ((output (first (cleavir-ir:outputs instruction)))
         (result (%intrinsic-invoke-if-landing-pad-or-call
                  "cc_makeCell" nil (datum-name-as-string output))))
    (out result output)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:write-cell-instruction) return-value abi function-info)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (cell (in (first inputs) "cell"))
         (val (in (second inputs) "val")))
    (%intrinsic-call "cc_writeCell" (list cell val))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:read-cell-instruction) return-value abi function-info)
  (let* ((cell (in (first (cleavir-ir:inputs instruction)) "cell"))
         (output (first (cleavir-ir:outputs instruction)))
         (result (%intrinsic-call
                  "cc_readCell" (list cell) (datum-name-as-string output))))
    (out result output)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:fetch-instruction) return-value abi function-info)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (output (first (cleavir-ir:outputs instruction)))
         (env (in (first inputs) "env"))
         (idx (cmp:irc-ptr-to-int (in (second inputs)) cmp:%size_t% "idx")))
    (out (%intrinsic-call "cc_fetch" (list env idx) (datum-name-as-string output)) output)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:fdefinition-instruction) return-value abi function-info)
  ;; How do we figure out if we should use safe or unsafe version
  (let* ((cell (in (first (cleavir-ir:inputs instruction)) "func-name"))
         (output (first (cleavir-ir:outputs instruction)))
         (result (%intrinsic-invoke-if-landing-pad-or-call
                  "cc_fdefinition" (list cell) (datum-name-as-string output))))
      (out result output)))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:debug-message-instruction) return-value abi function-info)
  (let ((msg (cmp:jit-constant-unique-string-ptr (clasp-cleavir-hir:debug-message instruction))))
    (%intrinsic-call "debugMessage" (list msg))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:debug-break-instruction) return-value abi function-info)
  (%intrinsic-call "debugBreak"))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:setf-fdefinition-instruction) return-value abi function-info)
  (let* ((cell (in (first (cleavir-ir:inputs instruction)) "setf-func-name"))
         (output (first (cleavir-ir:outputs instruction)))
         (result (%intrinsic-invoke-if-landing-pad-or-call
                  "cc_setfdefinition" (list cell) (datum-name-as-string output))))
    (out result output)))

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
    ((instruction cleavir-ir:enclose-instruction) return-value abi function-info)
  (let* ((enter-instruction (cleavir-ir:code instruction))
         (lambda-name (get-or-create-lambda-name enter-instruction))
         (enclosed-function (memoized-layout-procedure enter-instruction lambda-name abi))
         (function-description (llvm-sys:get-named-global cmp:*the-module* (cmp::function-description-name enclosed-function)))
         (loaded-inputs (mapcar (lambda (x) (in x "cell")) (cleavir-ir:inputs instruction)))
         (ninputs (length loaded-inputs))
         (dx-p (cleavir-ir:dynamic-extent-p instruction))
         (enclose-args
           (list* enclosed-function
                  (cmp:irc-bit-cast function-description cmp:%i8*%)
                  (%size_t ninputs)
                  loaded-inputs))
         (result
           (progn
             (cond
               (dx-p
                ;; Closure is dynamic extent, so we can use stack storage.
                (%intrinsic-call
                 "cc_stack_enclose"
                 (list* (alloca-i8 (core:closure-with-slots-size ninputs) "stack-allocated-closure")
                        enclose-args)
                 (format nil "closure->~a" lambda-name)))
               (t
                ;; General case.
                (%intrinsic-invoke-if-landing-pad-or-call "cc_enclose" enclose-args
                                                          (format nil "closure->~a" lambda-name)))))))
    (out result (first (cleavir-ir:outputs instruction)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:multiple-value-call-instruction) return-value abi function-info)
  (with-return-values (return-vals return-value abi)
    ;; second input is the dynamic-environment argument.
    (cmp:with-landing-pad (landing-pad (cleavir-ir:dynamic-environment instruction)
                                       return-value abi *tags* function-info)
      (let ((call-result (%intrinsic-invoke-if-landing-pad-or-call
                          "cc_call_multipleValueOneFormCallWithRet0" 
                          (list (in (first (cleavir-ir:inputs instruction))) (%load return-value)))))
        (%store call-result return-value)
        (cc-dbg-when *debug-log*
                     (format *debug-log*
                             "    translate-simple-instruction multiple-value-call-instruction: ~a~%" 
                             (cc-mir:describe-mir instruction))
                     (format *debug-log* "     instruction --> ~a~%" call-result))))))

(defun gen-vector-effective-address (array index element-type fixnum-type)
  (let* ((type (llvm-sys:type-get-pointer-to (cmp::simple-vector-llvm-type element-type)))
         (cast (%bit-cast array type))
         (var-offset (%ptrtoint index fixnum-type))
         (untagged (%lshr var-offset cmp::+fixnum-shift+ :exact t :label "untagged fixnum")))
    ;; 0 is for LLVM reasons, that pointers are C arrays. or something.
    ;; For layout of the vector, check simple-vector-llvm-type's definition.
    ;; untagged is the actual offset.
    (%gep-variable cast (list (%i32 0) (%i32 cmp::+simple-vector-data-slot+) untagged) "aref")))

(defun translate-bit-aref (array index &optional (label ""))
  (let* ((var-offset (%ptrtoint index cmp:%size_t% "variable-offset"))
         (untagged (%lshr var-offset cmp::+fixnum-shift+ :exact t :label "untagged-offset"))
         (bit (%intrinsic-call "cc_simpleBitVectorAref" (list array untagged) "bit-aref"))
         (tagged-bit (%shl bit cmp::+fixnum-shift+ :nuw t :label "tagged-bit")))
    ;; inttoptr intrinsically zexts, according to docs.
    (%inttoptr tagged-bit cmp:%t*% label)))

(defun translate-bit-aset (value array index)
  (let* ((var-offset (%ptrtoint index cmp:%size_t% "variable-offset"))
         (offset (%lshr var-offset cmp::+fixnum-shift+ :exact t :label "untagged-offset"))
         (uint-value (%ptrtoint value cmp::%uint%))
         (untagged-value (%lshr uint-value cmp::+fixnum-shift+ :exact t :label "untagged-value")))
    ;; Note: We cannot label void calls, because then they'll get a variable
    (%intrinsic-call "cc_simpleBitVectorAset" (list array offset untagged-value))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:aref-instruction) return-value abi function-info)
  (let* ((et (cleavir-ir:element-type instruction))
         (inputs (cleavir-ir:inputs instruction))
         (output (first (cleavir-ir:outputs instruction)))
         (label (datum-name-as-string output)))
    (out
     (if (eq et 'bit) ; have to special case due to the layout.
         (translate-bit-aref (in (first inputs)) (in (second inputs)) label)
         (%load (gen-vector-effective-address (in (first inputs)) (in (second inputs))
                                              (cleavir-ir:element-type instruction)
                                              (%default-int-type abi))
                label))
     output)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:aset-instruction) return-value abi function-info)
  (let ((et (cleavir-ir:element-type instruction))
        (inputs (cleavir-ir:inputs instruction)))
    (if (eq et 'bit) ; ditto above
        (translate-bit-aset (in (third inputs)) (in (first inputs)) (in (second inputs)))
        (%store (in (third inputs))
                (gen-vector-effective-address (in (first inputs)) (in (second inputs))
                                              (cleavir-ir:element-type instruction)
                                              (%default-int-type abi))))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir::vector-length-instruction) return-value abi function-info)
  (declare (ignore return-value function-info))
  (out (cmp::gen-vector-length (in (first (cleavir-ir:inputs instruction))))
       (first (cleavir-ir:outputs instruction))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir::displacement-instruction) return-value abi function-info)
  (declare (ignore return-value function-info abi))
  (let ((output (first (cleavir-ir:outputs instruction))))
    (out (%intrinsic-call "cc_realArrayDisplacement"
                          (list (in (first (cleavir-ir:inputs instruction))))
                          (datum-name-as-string output))
         output)))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir::displaced-index-offset-instruction) return-value abi function-info)
  (declare (ignore return-value function-info abi))
  (let ((output (first (cleavir-ir:outputs instruction))))
    (out (%inttoptr
          (%shl
           (%intrinsic-call "cc_realArrayDisplacedIndexOffset"
                            (list (in (first (cleavir-ir:inputs instruction)))))
           cmp::+fixnum-shift+
           :label "fixnum" :nuw t)
          cmp:%t*%
          (datum-name-as-string output))
         output)))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir::array-total-size-instruction) return-value abi function-info)
  (declare (ignore return-value function-info abi))
  (let ((output (first (cleavir-ir:outputs instruction))))
    (out (%inttoptr
          (%shl
           (%intrinsic-call "cc_arrayTotalSize"
                            (list (in (first (cleavir-ir:inputs instruction)))))
           cmp::+fixnum-shift+
           :label "fixnum" :nuw t)
          cmp:%t*%
          (datum-name-as-string output))
         output)))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir::array-rank-instruction) return-value abi function-info)
  (declare (ignore return-value function-info abi))
  (let ((output (first (cleavir-ir:outputs instruction))))
    (out (%inttoptr
          (%shl
           (%intrinsic-call "cc_arrayRank"
                            (list (in (first (cleavir-ir:inputs instruction)))))
           cmp::+fixnum-shift+
           :label "fixnum" :nuw t)
          cmp:%t*%
          (datum-name-as-string output))
         output)))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir::array-dimension-instruction) return-value abi function-info)
  (declare (ignore return-value function-info abi))
  (let ((inputs (cleavir-ir:inputs instruction))
        (output (first (cleavir-ir:outputs instruction))))
    (out (cmp::gen-%array-dimension (in (first inputs)) (in (second inputs)))
         output)))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:vaslist-pop-instruction) return-value abi function-info)
  (declare (ignore return-value function-info))
  (let ((input (first (cleavir-ir:inputs instruction)))
        (output (first (cleavir-ir:outputs instruction))))
    (out (%intrinsic-call "cx_vaslist_pop" (list (in input))) output)))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:instance-stamp-instruction) return-value abi function-info)
  (declare (ignore return-value function-info))
  (let ((input (first (cleavir-ir:inputs instruction)))
        (output (first (cleavir-ir:outputs instruction))))
    (out (%intrinsic-call "cx_read_stamp" (list (in input))) output)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:memref2-instruction) return-value abi function-info)
  (declare (ignore return-value abi function-info))
  (out (%load
        (cmp::gen-memref-address (in (first (cleavir-ir:inputs instruction)))
                                 (cleavir-ir:offset instruction)))
       (first (cleavir-ir:outputs instruction))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:memset2-instruction) return-value abi function-info)
  (declare (ignore return-value abi function-info))
  (let ((inputs (cleavir-ir:inputs instruction)))
    (%store (in (second inputs) "memset2-val")
            (cmp::gen-memref-address (in (first inputs))
                                     (cleavir-ir:offset instruction)))))

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
  (let* ((x (in (first (cleavir-ir:inputs instruction))))
         (tag (%and (%ptrtoint x cmp::%i32%) (%i32 cmp:+tag-mask+) "tag-only"))
         (cmp (%icmp-eq tag (%i32 cmp:+cons-tag+) "consp-test")))
    (%cond-br cmp (first successors) (second successors) :likely-true t)))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnump-instruction) return-value successors abi function-info)
  (let* ((x (in (first (cleavir-ir:inputs instruction))))
         (tag (%and (%ptrtoint x cmp::%i32%) (%i32 cmp:+fixnum-mask+) "fixnum-tag-only"))
         (cmp (%icmp-eq tag (%i32 cmp:+fixnum-tag+) "fixnump-test")))
    (%cond-br cmp (first successors) (second successors) :likely-true t)))

(defmethod translate-branch-instruction
    ((instruction cc-mir:characterp-instruction) return-value successors abi function-info)
  (let* ((value     (in (first (cleavir-ir:inputs instruction))))
         (tag       (%and (%ptrtoint value cmp:%uintptr_t%)
                          (%uintptr_t cmp:+immediate-mask+) "character-tag-only"))
         (cmp (%icmp-eq tag (%uintptr_t cmp:+character-tag+))))
    (%cond-br cmp (first successors) (second successors) :likely-true t)))

(defmethod translate-branch-instruction
    ((instruction cc-mir:single-float-p-instruction) return-value successors abi function-info)
  (let* ((value     (in (first (cleavir-ir:inputs instruction))))
         (tag       (%and (%ptrtoint value cmp:%uintptr_t%)
                    (%uintptr_t cmp:+immediate-mask+) "single-float-tag-only"))
         (cmp       (%icmp-eq tag (%uintptr_t cmp:+single-float-tag+))))
    (%cond-br cmp (first successors) (second successors) :likely-true t)))


(defmethod translate-branch-instruction
    ((instruction cc-mir:headerq-instruction) return-value successors abi function-info)
  (declare (ignore return-value outputs abi function-info))
  (cmp::compile-header-check
   (cc-mir:header-value-min-max instruction)
   (in (first (cleavir-ir:inputs instruction))) (first successors) (second successors)))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:unwind-instruction) return-value successors abi function-info)
  (declare (ignore successors function-info))
  ;; we don't use the second input to the unwind - the dynenv - at the moment.
  (with-return-values (return-values return-value abi)
    ;; Save whatever is in return-vals in the multiple-value array
    (%intrinsic-call "cc_saveMultipleValue0" (list return-value))
    (let ((static-index
            (instruction-go-index
             (nth (cleavir-ir:unwind-index instruction)
                  (cleavir-ir:successors (cleavir-ir:destination instruction))))))
      (%intrinsic-call "cc_unwind" (list (in (first (cleavir-ir:inputs instruction)))
                                         (%size_t static-index))))
    (cmp:irc-unreachable)))

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
  (%ret (%load return-value)))

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
         (x (%ptrtoint (in (first inputs)) (%default-int-type abi)))
         (y (%ptrtoint (in (second inputs)) (%default-int-type abi)))
         (result-with-overflow (%sadd.with-overflow x y abi)))
    (let ((val (%extract result-with-overflow 0 "result"))
          (overflow (%extract result-with-overflow 1 "overflow"))
          (output (first (cleavir-ir:outputs instruction))))
      (out (%inttoptr val cmp:%t*% (datum-name-as-string output)) output)
      (%cond-br overflow (second successors) (first successors)))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-sub-instruction) return-value successors abi function-info)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (x (%ptrtoint (in (first inputs)) (%default-int-type abi)))
         (y (%ptrtoint (in (second inputs)) (%default-int-type abi)))
         (result-with-overflow (%ssub.with-overflow x y abi)))
    (let ((val (%extract result-with-overflow 0 "result"))
          (overflow (%extract result-with-overflow 1 "overflow"))
          (output (first (cleavir-ir:outputs instruction))))
      (out (%inttoptr val cmp:%t*% (datum-name-as-string output)) output)
      (%cond-br overflow (second successors) (first successors)))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-less-instruction) return-value successors abi function-info)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (cmp-lt (%icmp-slt (in (first inputs)) (in (second inputs)))))
      (%cond-br cmp-lt (first successors) (second successors))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-not-greater-instruction) return-value successors abi function-info)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (cmp-lt (%icmp-sle (in (first inputs)) (in (second inputs)))))
      (%cond-br cmp-lt (first successors) (second successors))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-equal-instruction) return-value successors abi function-info)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (cmp-lt (%icmp-eq (in (first inputs)) (in (second inputs)))))
      (%cond-br cmp-lt (first successors) (second successors))))

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
    (%cond-br cmp (first successors) (second successors))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:float-not-greater-instruction) return-value successors abi function-info)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (x (in (first inputs)))
         (y (in (second inputs)))
         (cmp (%fcmp-ole x y)))
    (%cond-br cmp (first successors) (second successors))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:float-equal-instruction) return-value successors abi function-info)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (x (in (first inputs)))
         (y (in (second inputs)))
         (cmp (%fcmp-oeq x y)))
    (%cond-br cmp (first successors) (second successors))))
