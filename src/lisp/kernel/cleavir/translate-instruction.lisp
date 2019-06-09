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
         (src-remaining-nargs* (%intrinsic-call "cc_vaslist_remaining_nargs_address" (list vaslist-value)))
         (src-va_list*         (%intrinsic-call "cc_vaslist_va_list_address" (list vaslist-value)
                                                "vaslist_address"))
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
  (let ((inputs (cleavir-ir:inputs instruction)))
    ;; Write the first return value into the result
    (with-return-values (return-value abi nret ret-regs)
      (cmp:irc-store (%size_t (length inputs)) nret)
      (dotimes (i (length inputs))
        (cmp:irc-store (in (elt inputs i)) (return-value-elt ret-regs i))))))

(defmethod translate-simple-instruction
    ((instr cleavir-ir:multiple-to-fixed-instruction) return-value (abi abi-x86-64) function-info)
  ;; We put in a switch on the number of return values. There's one case for each output, plus one.
  ;; The blocks out of the switch branch to a final block which has a phi for each output.
  ;; So if we have an MTF with one output, we'd have
  ;; switch number-of-return-values { case 0: go zero; default: go default;}
  ;; zero: out = nil; go final;
  ;; default: out = return-value-0; go final;
  (with-return-values (return-value abi nret return-regs)
    (let* ((outputs (cleavir-ir:outputs instr))
           (nouts (length outputs))
           (rets (loop for i below nouts collect (return-value-elt return-regs i)))
           (default (cmp:irc-basic-block-create "mtf-enough"))
           (switch (cmp:irc-switch (cmp:irc-load nret) default nouts))
           (final (cmp:irc-basic-block-create "mtf-final"))
           (default-vars (prog2 (cmp:irc-begin-block default)
                             (mapcar #'cmp:irc-load rets)
                           (cmp:irc-br final)))
           (blocks-and-vars
             (loop for retn below nouts
                   for block = (cmp:irc-basic-block-create (format nil "mtf-~d" retn))
                   do (llvm-sys:add-case switch (%size_t retn) block)
                   do (cmp:irc-begin-block block)
                   collect (cons block
                                 (loop for ret in rets
                                       for i below nouts
                                       collect (if (< i retn) (cmp:irc-load ret) (%nil))))
                   do (cmp:irc-br final))))
      (cmp:irc-begin-block final)
      (let ((phis (loop for out in outputs
                        for i from 0
                        for phi = (cmp:irc-phi cmp:%t*% (1+ nouts) (datum-name-as-string out))
                        do (loop for (block . vars) in blocks-and-vars
                                 do (cmp:irc-phi-add-incoming phi (elt vars i) block))
                           (cmp:irc-phi-add-incoming phi (elt default-vars i) default)
                        collect phi)))
        (loop for phi in phis
              for out in outputs
              do (out phi out))))))

(defmethod translate-simple-instruction
    ((instr clasp-cleavir-hir:save-values-instruction) return-value abi function-info)
  (declare (ignore abi function-info))
  (let* ((outputs (cleavir-ir:outputs instr))
         (nvals-loc (first outputs))
         (vals-loc (second outputs))
         ;; Get the values.
         (ret (cmp:irc-load return-value))
         ;; Get the parts we need.
         (primary (cmp:irc-extract-value ret '(0) "primary"))
         (nvals (cmp:irc-extract-value ret '(1) "nvals"))
         ;; Allocate storage. Note this is in-line.
         (mv-temp (cmp:alloca-temp-values nvals)))
    ;; Do the actual storing into mv-temp
    (%intrinsic-call "cc_save_values" (list nvals primary mv-temp))
    ;; Put the stuff in the outputs
    (out nvals nvals-loc)
    (out mv-temp vals-loc)))

(defmethod translate-simple-instruction
    ((instr clasp-cleavir-hir:load-values-instruction) return-value abi function-info)
  (declare (ignore abi function-info))
  (let* ((inputs (cleavir-ir:inputs instr))
         (nvals-loc (first inputs))
         (vals-loc (second inputs)))
    (cmp:irc-store
     (%intrinsic-call "cc_load_values" (list (in nvals-loc) (in vals-loc)))
     return-value)))

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
      (closure-call-or-invoke (in function) return-value (mapcar #'in arguments)))))

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
  (cmp:with-landing-pad (landing-pad (cleavir-ir:dynamic-environment instruction)
                                     return-value abi *tags* function-info)
    (let ((call-result (%intrinsic-invoke-if-landing-pad-or-call
                        "cc_call_multipleValueOneFormCallWithRet0" 
                        (list (in (first (cleavir-ir:inputs instruction)))
                              (cmp:irc-load return-value)))))
      ;; call-result is a T_mv, and return-valuea  T_mv*
      (cmp:irc-store call-result return-value)
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
    (cmp:irc-tag-fixnum bit "bit")))

(defun translate-bit-aset (value array index)
  (let ((offset (cmp:irc-untag-fixnum index cmp:%size_t% "untagged-offset"))
        (untagged-value (cmp:irc-untag-fixnum value cmp::%uint% "untagged-value")))
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
         (cmp:irc-load (gen-vector-effective-address (in (first inputs)) (in (second inputs))
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
        (cmp:irc-store
         (in (third inputs))
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
    (out (cmp:irc-tag-fixnum
          (%intrinsic-call "cc_realArrayDisplacedIndexOffset"
                           (list (in (first (cleavir-ir:inputs instruction)))))
          (datum-name-as-string output))
         output)))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir::array-total-size-instruction) return-value abi function-info)
  (declare (ignore return-value function-info abi))
  (let ((output (first (cleavir-ir:outputs instruction))))
    (out (cmp:irc-tag-fixnum
          (%intrinsic-call "cc_arrayTotalSize"
                           (list (in (first (cleavir-ir:inputs instruction)))))
          (datum-name-as-string output))
         output)))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir::array-rank-instruction) return-value abi function-info)
  (declare (ignore return-value function-info abi))
  (let ((output (first (cleavir-ir:outputs instruction))))
    (out (cmp:irc-tag-fixnum
          (%intrinsic-call "cc_arrayRank"
                           (list (in (first (cleavir-ir:inputs instruction)))))
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
  (let* ((input (first (cleavir-ir:inputs instruction)))
         (output (first (cleavir-ir:outputs instruction))))
    (if cmp:*test-ir*
        (let ((new-stamp (cmp:irc-read-stamp (in input))))
          (out (%intrinsic-call "cx_read_stamp" (list (in input) new-stamp)) output))
        (out (%intrinsic-call "cx_read_stamp" (list (in input) (cmp:jit-constant-i64 0))) output))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:slot-read-instruction) return-value abi function-infoO)
  (declare (ignore return-value abi function-info))
  (let ((inputs (cleavir-ir:inputs instruction)))
    (out (cmp::gen-instance-ref (in (first inputs)) (in (second inputs)))
         (first (cleavir-ir:outputs instruction)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:slot-write-instruction) return-value abi function-infoO)
  (declare (ignore return-value abi function-info))
  (let ((inputs (cleavir-ir:inputs instruction)))
    (cmp::gen-instance-set (in (first inputs)) (in (second inputs)) (in (third inputs)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:memref2-instruction) return-value abi function-info)
  (declare (ignore return-value abi function-info))
  (out (cmp:irc-load
        (cmp::gen-memref-address (in (first (cleavir-ir:inputs instruction)))
                                 (cleavir-ir:offset instruction)))
       (first (cleavir-ir:outputs instruction))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:memset2-instruction) return-value abi function-info)
  (declare (ignore return-value abi function-info))
  (let ((inputs (cleavir-ir:inputs instruction)))
    (cmp:irc-store
     (in (second inputs) "memset2-val")
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
         (tag (cmp:irc-and (cmp:irc-ptr-to-int x cmp:%i32%) (%i32 cmp:+tag-mask+) "tag-only"))
;;;         (_ (core:debug-message "In consp"))
         (cmp (cmp:irc-icmp-eq tag (%i32 cmp:+cons-tag+) "consp-test")))
    (cmp:irc-cond-br cmp (first successors) (second successors))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnump-instruction) return-value successors abi function-info)
  (let* ((x (in (first (cleavir-ir:inputs instruction))))
         (tag (cmp:irc-and (cmp:irc-ptr-to-int x cmp:%i32%) (%i32 cmp:+fixnum-mask+) "fixnum-tag-only"))
         (cmp (cmp:irc-icmp-eq tag (%i32 cmp:+fixnum-tag+) "fixnump-test")))
    (cmp:irc-cond-br cmp (first successors) (second successors))))

(defmethod translate-branch-instruction
    ((instruction cc-mir:characterp-instruction) return-value successors abi function-info)
  (let* ((value     (in (first (cleavir-ir:inputs instruction))))
         (tag       (cmp:irc-and (cmp:irc-ptr-to-int value cmp:%uintptr_t%)
                                 (%uintptr_t cmp:+immediate-mask+)
                                 "character-tag-test"))
         (cmp (cmp:irc-icmp-eq tag (%uintptr_t cmp:+character-tag+))))
    (cmp:irc-cond-br cmp (first successors) (second successors))))

(defmethod translate-branch-instruction
    ((instruction cc-mir:single-float-p-instruction) return-value successors abi function-info)
  (let* ((value     (in (first (cleavir-ir:inputs instruction))))
         (tag       (cmp:irc-and (cmp:irc-ptr-to-int value cmp:%uintptr_t%)
                                 (%uintptr_t cmp:+immediate-mask+)
                                 "single-float-tag-test"))
         (cmp       (cmp:irc-icmp-eq tag (%uintptr_t cmp:+single-float-tag+))))
    (cmp:irc-cond-br cmp (first successors) (second successors))))


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
  ;; Save whatever is in return-vals in the multiple-value array
  (%intrinsic-call "cc_saveMultipleValue0" (list return-value))
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
  (cmp:irc-ret (cmp:irc-load return-value)))

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
