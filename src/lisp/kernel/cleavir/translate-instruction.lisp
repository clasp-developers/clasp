(in-package #:clasp-cleavir)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on TRANSLATE-SIMPLE-INSTRUCTION.

(defmethod translate-simple-instruction
    ((instr cleavir-ir:enter-instruction) return-value inputs outputs (abi abi-x86-64) function-info)
  (let* ((lambda-list (cleavir-ir:lambda-list instr))
         (closed-env-dest (first outputs))
         (calling-convention (calling-convention function-info)))
    (%store (cmp:calling-convention-closure calling-convention) closed-env-dest)
    (let ((args (cdr (cleavir-ir:outputs instr))))
      ;; We used to change the landing pad here, so that it skipped unwinds
      ;; (which after all can't possibly be from the lambda list code)
      ;; But it substantially complicates the code and it's not that important.
      ;; Better usage of INVOKE might be able to restore the situation.
      (cmp:compile-lambda-list-code lambda-list args calling-convention
                                    :translate-datum #'translate-datum))))

(defmethod translate-simple-instruction
    ((instr clasp-cleavir-hir:bind-va-list-instruction) return-value inputs outputs (abi abi-x86-64) function-info)
  (let* ((lambda-list                   (cleavir-ir:lambda-list instr))
         (vaslist                       (car inputs))
         (src-remaining-nargs*          (%intrinsic-call "cc_vaslist_remaining_nargs_address" (list (%load vaslist))))
         (src-va_list*                  (%intrinsic-call "cc_vaslist_va_list_address" (list (%load vaslist)) "vaslist_address"))
         (local-remaining-nargs*        (alloca-size_t "local-remaining-nargs"))
         (local-va_list*                (alloca-va_list "local-va_list"))
         (_                             (%store (%load src-remaining-nargs*) local-remaining-nargs*))
         (_                             (%intrinsic-call "llvm.va_copy" (list (%pointer-cast local-va_list* cmp:%i8*%)
                                                                              (%pointer-cast src-va_list* cmp:%i8*%))))
         (callconv                      (cmp:make-calling-convention-impl :nargs (%load src-remaining-nargs*)
                                                                          :va-list* local-va_list*
                                                                          :remaining-nargs* local-remaining-nargs*)))
    (cmp:compile-lambda-list-code lambda-list outputs callconv
                                  :translate-datum (lambda (datum)
                                                     (if (typep datum 'cleavir-ir:lexical-location)
                                                         (translate-datum datum)
                                                         datum)))))


(defmethod translate-simple-instruction
    ((instruction cleavir-ir:instruction) return-value inputs outputs abi function-info)
  (error "Implement instruction: ~a for abi: ~a~%" instruction abi)
  (format t "--------------- translate-simple-instruction ~a~%" instruction)
  (format t "    inputs: ~a~%" inputs)
  (format t "    outputs: ~a~%" outputs))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:assignment-instruction) return-value inputs outputs abi function-info)
  (let ((input (first inputs))
        (output (first outputs)))
    (cond
      ((typep input 'immediate-literal)
       (%store (cmp:irc-int-to-ptr (%i64 (immediate-literal-tagged-value input)) cmp:%t*%) output "immediate"))
      ((typep input 'llvm-sys:constant-int)
       (let ((val (%inttoptr input cmp:%t*%)))
         (%store val output "constant-int")))
      (t
       (let ((load (%load input)))
         (%store load output "default"))))))

(defun safe-llvm-name (obj)
  "Generate a name for the object that can be used as a variable label in llvm"
  (cond
    ((stringp obj) obj)
    ((symbolp obj) (string obj))
    ((listp obj) "list")
    ((arrayp obj) "array")
    (t "object")))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:precalc-value-instruction) return-value inputs outputs abi function-info)
  (let* ((literal (first inputs))
         (value (etypecase literal
                  (llvm-sys:constant-int
                   (warn "llvm-sys:constant-int ~s should not be input for precalc-value-instruction"
                         literal)
                   (draw-hir (enter-instruction function-info))
                   (break "breaking")
                   (%inttoptr literal cmp:%t*%)) ; where are these coming from
                  (immediate-literal
                   (immediate-literal-tagged-value literal))
                  (arrayed-literal
                   (let* ((idx (arrayed-literal-index literal))
                          (label (safe-llvm-name (clasp-cleavir-hir:precalc-value-instruction-original-object instruction))))
                     (%load (%gep-variable (cmp:ltv-global) (list (%size_t 0) (%i64 idx)) label)))))))
    (%store value (first outputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:fixed-to-multiple-instruction) return-value inputs outputs (abi abi-x86-64) function-info)
  ;; Write the first return value into the result
  (with-return-values (return-values return-value abi)
    (%store (%size_t (length inputs)) (number-of-return-values return-values))
    (dotimes (i (length inputs))
      (%store (%load (elt inputs i)) (return-value-elt return-values i)))
    #+(or)(cmp:irc-intrinsic "cc_saveThreadLocalMultipleValues" (sret-arg return-values) (first outputs))
    ))

(defmethod translate-simple-instruction
    ((instr cleavir-ir:multiple-to-fixed-instruction) return-value inputs outputs (abi abi-x86-64) function-info)
  ;; Create a basic block for each output
  (with-return-values (return-vals return-value abi)
    #+(or)(cmp:irc-intrinsic "cc_loadThreadLocalMultipleValues" (sret-arg return-vals) (first inputs))
    (let* ((blocks (let (b) (dotimes (i (1+ (length outputs))) (push (cmp:irc-basic-block-create (format nil "mvn~a-" i)) b)) (nreverse b)))
	   (final-block (cmp:irc-basic-block-create "mvn-final"))
	   (switch (cmp:irc-switch (%load (number-of-return-values return-vals)) (car (last blocks)) (length blocks))))
      (dotimes (n (length blocks))
	(let ((block (elt blocks n)))
	  (cmp:irc-begin-block block)
	  (llvm-sys:add-case switch (%size_t n) block)
	  (dotimes (i (length outputs))
	    (if (< i n)
		(%store (%load (return-value-elt return-vals i)) (elt outputs i))
		(%store (%nil) (elt outputs i))))
	  (cmp:irc-br final-block)))
      (cmp:irc-begin-block final-block))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:multiple-value-foreign-call-instruction) return-value inputs outputs (abi abi-x86-64) function-info)
  (check-type (clasp-cleavir-hir:function-name instruction) string)
  (clasp-cleavir:unsafe-multiple-value-foreign-call (clasp-cleavir-hir:function-name instruction) return-value inputs abi))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:foreign-call-instruction) return-value inputs outputs (abi abi-x86-64) function-info)
  ;; FIXME:  If this function has cleanup forms then this needs to be an INVOKE
  (clasp-cleavir:unsafe-foreign-call :call (clasp-cleavir-hir:foreign-types instruction)
                                     (clasp-cleavir-hir:function-name instruction)
                                     (car outputs) inputs abi))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:foreign-call-pointer-instruction) return-value inputs outputs (abi abi-x86-64) function-info)
  ;; FIXME:  If this function has cleanup forms then this needs to be an INVOKE
  (clasp-cleavir:unsafe-foreign-call-pointer
   :call (clasp-cleavir-hir:foreign-types instruction) (%load (car inputs))
   (car outputs) (cdr inputs) abi))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:funcall-instruction) return-value inputs outputs (abi abi-x86-64) function-info)
  (closure-call-or-invoke (first inputs) return-value (cdr inputs) abi))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:nop-instruction) return-value inputs outputs abi function-info)
  (declare (ignore return-value inputs outputs abi function-info)))

(defmethod translate-simple-instruction
    ((instruction cc-mir:save-frame-instruction) return-value inputs outputs abi function-info)
  ;; FIXME: rename the intrinsic!!
  (%store (%intrinsic-call "cc_pushLandingPadFrame" nil) (first outputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:create-cell-instruction) return-value inputs outputs abi function-info)
  (let ((result (%intrinsic-invoke-if-landing-pad-or-call "cc_makeCell" nil)))
    (%store result (first outputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:write-cell-instruction) return-value inputs outputs abi function-info)
  (let ((cell (llvm-sys:create-load-value-twine cmp:*irbuilder* (first inputs) "cell"))
	(val (llvm-sys:create-load-value-twine cmp:*irbuilder* (second inputs) "val")))
    (%intrinsic-call "cc_writeCell" (list cell val))))


(defmethod translate-simple-instruction
    ((instruction cleavir-ir:read-cell-instruction) return-value inputs outputs abi function-info)
  (let ((cell (llvm-sys:create-load-value-twine cmp:*irbuilder* (first inputs) "cell")))
    (let ((result (%intrinsic-call "cc_readCell" (list cell))))
      (%store result (first outputs)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:fetch-instruction) return-value inputs outputs abi function-info)
  (let ((env (%load (first inputs) "env"))
	(idx (second inputs)))
    (let ((result (%intrinsic-call "cc_fetch" (list env idx))))
      (%store result (first outputs)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:fdefinition-instruction) return-value inputs outputs abi function-info)
  ;; How do we figure out if we should use safe or unsafe version
  (let ((cell (%load (first inputs) "func-name")))
    (let ((result (%intrinsic-invoke-if-landing-pad-or-call "cc_fdefinition" (list cell))))
      (%store result (first outputs)))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:debug-message-instruction) return-value inputs outputs abi function-info)
  (let ((msg (cmp:jit-constant-unique-string-ptr (clasp-cleavir-hir:debug-message instruction))))
    (%intrinsic-call "debugMessage" (list msg))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:debug-break-instruction) return-value inputs outputs abi function-info)
  (%intrinsic-call "debugBreak"))


(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir:setf-fdefinition-instruction) return-value inputs outputs abi function-info)
  (let ((cell (%load (first inputs) "setf-func-name")))
    (let ((result (%intrinsic-invoke-if-landing-pad-or-call "cc_setfdefinition" (list cell))))
      (%store result (first outputs)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:symbol-value-instruction) return-value inputs outputs abi function-info)
  (let ((sym (%load (first inputs) "sym-name")))
    (let ((result (%intrinsic-invoke-if-landing-pad-or-call "cc_safe_symbol_value" (list sym))))
      (%store result (first outputs)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:set-symbol-value-instruction) return-value inputs outputs abi function-info)
  (let ((sym (%load (first inputs) "sym-name"))
	(val (%load (second inputs) "value")))
    (%intrinsic-invoke-if-landing-pad-or-call "cc_setSymbolValue" (list sym val))))


(defmethod translate-simple-instruction
    ((instruction cleavir-ir:enclose-instruction) return-value inputs outputs abi function-info)
  (let* ((enter-instruction (cleavir-ir:code instruction))
         (lambda-name (get-or-create-lambda-name enter-instruction))
         (enclosed-function (memoized-layout-procedure enter-instruction lambda-name abi))
         (function-description (llvm-sys:get-named-global cmp:*the-module* (cmp::function-description-name enclosed-function)))
         (loaded-inputs (mapcar (lambda (x) (%load x "cell")) inputs))
         (dx-p (cleavir-ir:dynamic-extent-p instruction))
         (enclose-args
           (list* enclosed-function
                  (cmp:irc-bit-cast function-description cmp:%i8*%)
                  (%size_t (length inputs))
                  loaded-inputs))
         (result
           (progn
             (cond
               (dx-p
                ;; Closure is dynamic extent, so we can use stack storage.
                (%intrinsic-call
                 "cc_stack_enclose"
                 (list* (alloca-i8 (core:closure-with-slots-size (length inputs)) "stack-allocated-closure")
                        enclose-args)
                 (format nil "closure->~a" lambda-name)))
               (t
                ;; General case.
                (%intrinsic-invoke-if-landing-pad-or-call "cc_enclose" enclose-args
                                                          (format nil "closure->~a" lambda-name)))))))
    (cc-dbg-when *debug-log*
                 (format *debug-log* "~:[cc_enclose~;cc_stack_enclose~] with ~a cells~%"
                         dx-p (length inputs))
                 (format *debug-log* "    inputs: ~a~%" inputs))
    (%store result (first outputs) nil)))


(defmethod translate-simple-instruction
    ((instruction cleavir-ir:multiple-value-call-instruction) return-value inputs outputs abi function-info)
  (with-return-values (return-vals return-value abi)
 ;   (%intrinsic-call "cc_saveMultipleValue0" (list return-value)) ;; (sret-arg return-vals))
    ;; NOTE: (NOT A FIXME)  This instruction is explicitly for calls.
    (let ((call-result (%intrinsic-invoke-if-landing-pad-or-call
                        "cc_call_multipleValueOneFormCallWithRet0" 
                        (list (%load (first inputs)) (%load return-value)))))
      (%store call-result return-value)
      (cc-dbg-when *debug-log*
                   (format *debug-log* "    translate-simple-instruction multiple-value-call-instruction: ~a~%" 
                           (cc-mir:describe-mir instruction))
                   (format *debug-log* "     instruction --> ~a~%" call-result)))))

(defun gen-vector-effective-address (array index element-type fixnum-type)
  (let* ((array (%load array)) (index (%load index))
         (type (llvm-sys:type-get-pointer-to (cmp::simple-vector-llvm-type element-type)))
         (cast (%bit-cast array type))
         (var-offset (%ptrtoint index fixnum-type))
         (untagged (%lshr var-offset cmp::+fixnum-shift+ :exact t :label "untagged fixnum")))
    ;; 0 is for LLVM reasons, that pointers are C arrays. or something.
    ;; 1 gets us to the "data" slot of the struct.
    ;; untagged is the actual offset.
    (%gep-variable cast (list (%i32 0) (%i32 1) untagged) "aref")))

(defun translate-bit-aref (output array index)
  (let* ((array (%load array)) (index (%load index))
         (var-offset (%ptrtoint index cmp:%size_t% "variable-offset"))
         (untagged (%lshr var-offset cmp::+fixnum-shift+ :exact t :label "untagged-offset"))
         (bit (%intrinsic-call "cc_simpleBitVectorAref" (list array untagged) "bit-aref"))
         (tagged-bit (%shl bit cmp::+fixnum-shift+ :nuw t :label "tagged-bit"))
         ;; inttoptr intrinsically zexts, according to docs.
         (fixnum (%inttoptr tagged-bit cmp:%t*% "bit-aref-result")))
    (%store fixnum output)))

(defun translate-bit-aset (value array index)
  (let* ((value (%load value)) (array (%load array)) (index (%load index))
         (var-offset (%ptrtoint index cmp:%size_t% "variable-offset"))
         (offset (%lshr var-offset cmp::+fixnum-shift+ :exact t :label "untagged-offset"))
         (uint-value (%ptrtoint value cmp::%uint%))
         (untagged-value (%lshr uint-value cmp::+fixnum-shift+ :exact t :label "untagged-value")))
    ;; Note: We cannot label void calls, because then they'll get a variable
    (%intrinsic-call "cc_simpleBitVectorAset" (list array offset untagged-value))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:aref-instruction) return-value inputs outputs abi function-info)
  (let ((et (cleavir-ir:element-type instruction)))
    (if (eq et 'bit) ; have to special case due to the layout.
        (translate-bit-aref (first outputs) (first inputs) (second inputs))
        (%store (%load (gen-vector-effective-address (first inputs) (second inputs)
                                                     (cleavir-ir:element-type instruction)
                                                     (%default-int-type abi)))
                (first outputs)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:aset-instruction) return-value inputs outputs abi function-info)
  (let ((et (cleavir-ir:element-type instruction)))
    (if (eq et 'bit) ; ditto above
        (translate-bit-aset (third inputs) (first inputs) (second inputs))
        (%store (%load (third inputs))
                (gen-vector-effective-address (first inputs) (second inputs)
                                              (cleavir-ir:element-type instruction)
                                              (%default-int-type abi))))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir::vector-length-instruction)
     return-value inputs outputs abi function-info)
  (declare (ignore return-value function-info))
  (let* ((tptr (%load (first inputs)))
         (ui-offset (%uintptr_t (- cmp::+simple-vector._length-offset+ cmp:+general-tag+)))
         (ui-tptr (%ptrtoint tptr cmp:%uintptr_t%)))
    (let* ((uiptr (%add ui-tptr ui-offset))
           (ptr (%inttoptr uiptr cmp:%t**%))
           (read-val (%ptrtoint (%load ptr) (%default-int-type abi)))
           ;; now we just make it a fixnum.
           (fixnum (%shl read-val cmp::+fixnum-shift+ :nuw t :label "tag fixnum")))
      (%store (%inttoptr fixnum cmp:%t*%) (first outputs)))))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir::displacement-instruction)
     return-value inputs outputs abi function-info)
  (declare (ignore return-value function-info abi))
  (%store (%intrinsic-call "cc_realArrayDisplacement"
                           (list (%load (first inputs))))
          (first outputs)))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir::displaced-index-offset-instruction)
     return-value inputs outputs abi function-info)
  (declare (ignore return-value function-info abi))
  (%store (%inttoptr
           (%shl
            (%intrinsic-call "cc_realArrayDisplacedIndexOffset"
                             (list (%load (first inputs))))
            cmp::+fixnum-shift+
            :label "fixnum" :nuw t)
           cmp:%t*%)
          (first outputs)))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir::array-total-size-instruction)
     return-value inputs outputs abi function-info)
  (declare (ignore return-value function-info abi))
  (%store (%inttoptr
           (%shl
            (%intrinsic-call "cc_arrayTotalSize"
                             (list (%load (first inputs))))
            cmp::+fixnum-shift+
            :label "fixnum" :nuw t)
           cmp:%t*%)
          (first outputs)))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir::array-rank-instruction)
     return-value inputs outputs abi function-info)
  (declare (ignore return-value function-info abi))
  (%store (%inttoptr
           (%shl
            (%intrinsic-call "cc_arrayRank"
                             (list (%load (first inputs))))
            cmp::+fixnum-shift+
            :label "fixnum" :nuw t)
           cmp:%t*%)
          (first outputs)))

(defmethod translate-simple-instruction
    ((instruction clasp-cleavir-hir::array-dimension-instruction)
     return-value inputs outputs abi function-info)
  (declare (ignore return-value function-info))
  (%store (%inttoptr
           (%shl
            (%intrinsic-call "cc_arrayDimension"
                             (list (%load (first inputs))
                                   (%lshr (%ptrtoint (%load (second inputs)) (%default-int-type abi))
                                          cmp::+fixnum-shift+ 
                                          :exact t :label "untagged fixnum")))
            cmp::+fixnum-shift+
            :label "fixnum" :nuw t)
           cmp:%t*%)
          (first outputs)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:memref2-instruction) return-value inputs outputs abi function-info)
  (let* ((tptr (%load (first inputs)))
         (offset (second inputs))
         (ui-tptr (%ptrtoint tptr cmp:%uintptr_t%))
         (ui-offset (%bit-cast offset cmp:%uintptr_t%)))
    (let* ((uiptr (%add ui-tptr ui-offset))
           (ptr (%inttoptr uiptr cmp::%t**%))
           (read-val (%load ptr)))
      (%store read-val (first outputs)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:memset2-instruction) return-value inputs outputs abi function-info)
  (let* ((tptr (%load (first inputs)))
         (offset (second inputs))
         (ui-tptr (%ptrtoint tptr cmp:%uintptr_t%))
         (ui-offset (%bit-cast offset cmp:%uintptr_t%)))
    (let* ((uiptr (%add ui-tptr ui-offset))
           (dest (%inttoptr uiptr cmp::%t**% "memset2-dest"))
           (val (%load (third inputs) "memset2-val")))
      (%store val dest))))


(defmethod translate-simple-instruction
    ((instruction cleavir-ir:box-instruction) return-value inputs outputs abi function-info)
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
            ((double-float) "to_object_double"))))
    (%store
     (%intrinsic-invoke-if-landing-pad-or-call intrinsic (list (%load (first inputs))))
     (first outputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:unbox-instruction) return-value inputs outputs abi function-info)
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
            ((double-float) "from_object_double"))))
    (%store
     (%intrinsic-invoke-if-landing-pad-or-call intrinsic (list (%load (first inputs))))
     (first outputs))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:the-instruction) return-value inputs outputs abi function-info)
  (declare (ignore return-value inputs outputs abi function-info)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:the-values-instruction) return-value inputs outputs abi function-info)
  (declare (ignore return-value inputs outputs abi function-info)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:dynamic-allocation-instruction)
     return-value inputs outputs abi function-info)
  (declare (ignore return-value inputs outputs abi)))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:unreachable-instruction) return-value inputs outputs abi function-info)
  (declare (ignore return-value inputs outputs abi function-info))
  (cmp:irc-unreachable))


;;; Floating point arithmetic


(defmacro define-fp-binop (instruction-class-name op)
  `(defmethod translate-simple-instruction
       ((instruction ,instruction-class-name) return-value inputs outputs abi function-info)
     (declare (ignore return-value abi function-info))
     (%store (,op (%load (first inputs)) (%load (second inputs)))
             (first outputs))))

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
    ((instruction cleavir-ir:coerce-instruction) return-value inputs outputs abi function-info)
  (declare (ignore return-value abi function-info))
  (let ((input (%load (first inputs))) (output (first outputs)))
    (%store
     (ecase (cleavir-ir:from-type instruction)
       ((single-float)
        (ecase (cleavir-ir:to-type instruction)
          ((double-float)
           (%fpext input cmp::%double%)))))
     output)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on TRANSLATE-BRANCH-INSTRUCTION.

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:eq-instruction) return-value inputs outputs successors abi function-info)
  (let ((ceq (cmp:irc-icmp-eq (cmp:irc-load (first inputs)) (cmp:irc-load (second inputs)))))
    (cmp:irc-cond-br ceq (first successors) (second successors))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:consp-instruction) return-value inputs outputs successors abi function-info)
  (let* ((x (%load (first inputs)))
         (tag (%and (%ptrtoint x cmp::%i32%) (%i32 cmp:+tag-mask+) "tag-only"))
         (cmp (%icmp-eq tag (%i32 cmp:+cons-tag+) "consp-test")))
    (%cond-br cmp (first successors) (second successors) :likely-true t)))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnump-instruction) return-value inputs outputs successors abi function-info)
  (let* ((x (%load (first inputs)))
         (tag (%and (%ptrtoint x cmp::%i32%) (%i32 cmp:+fixnum-mask+) "fixnum-tag-only"))
         (cmp (%icmp-eq tag (%i32 cmp:+fixnum-tag+) "fixnump-test")))
    (%cond-br cmp (first successors) (second successors) :likely-true t)))

(defmethod translate-branch-instruction
    ((instruction cc-mir:characterp-instruction) return-value inputs outputs successors abi function-info)
  (let* ((value     (%load (first inputs)))
         (tag       (%and (%ptrtoint value cmp:%uintptr_t%)
                          (%uintptr_t cmp:+immediate-mask+) "character-tag-only"))
         (cmp (%icmp-eq tag (%uintptr_t cmp:+character-tag+))))
    (%cond-br cmp (first successors) (second successors) :likely-true t)))


(defmethod translate-branch-instruction
    ((instruction cc-mir:single-float-p-instruction) return-value inputs outputs successors abi function-info)
  (let* ((value     (%load (first inputs)))
         (tag       (%and (%ptrtoint value cmp:%uintptr_t%)
                    (%uintptr_t cmp:+immediate-mask+) "single-float-tag-only"))
         (cmp       (%icmp-eq tag (%uintptr_t cmp:+single-float-tag+))))
    (%cond-br cmp (first successors) (second successors) :likely-true t)))


(defmethod translate-branch-instruction
    ((instruction cc-mir:headerq-instruction) return-value inputs outputs successors abi function-info)
  (declare (ignore return-value outputs abi function-info))
  (cmp::compile-header-check
   (cc-mir:header-value-min-max instruction)
   (%load (first inputs)) (first successors) (second successors)))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:unwind-instruction)
     return-value inputs outputs successors abi function-info)
  (declare (ignore successors))
  (with-return-values (return-values return-value abi)
    ;; Save whatever is in return-vals in the multiple-value array
    (%intrinsic-call "cc_saveMultipleValue0" (list return-value))
    (maybe-gen-cleanup-invocation-history function-info)
    (let ((static-index (cc-mir:go-index (cleavir-ir:destination instruction))))
      (%intrinsic-call "cc_unwind" (list (%load (first inputs)) (%size_t static-index))))
    (cmp:irc-unreachable)))

;;; This is not a real branch: it only has two successors for convenience elsewhere.
;;; See comment in mir.lisp.
;;; The second branch's code will be reachable from the function's landing pad.
(defmethod translate-branch-instruction
    ((instruction cc-mir:assign-catch-instruction)
     return-value inputs outputs successors abi function-info)
  (%store (%load (first inputs)) (first outputs) "frame-marker")
  (cmp:irc-br (first successors)))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:return-instruction) return-value inputs outputs successors abi function-info)
  (declare (ignore successors))
  (maybe-gen-cleanup-invocation-history function-info)
  (%ret (%load return-value)))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:funcall-no-return-instruction) return-value inputs outputs successors (abi abi-x86-64) function-info)
  (declare (ignore successors))
  (closure-call-or-invoke (first inputs) return-value (cdr inputs) abi)
  (cmp:irc-unreachable))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:unreachable-instruction) return-value inputs outputs successors abi function-info)
  (declare (ignore return-value inputs output successors abi function-info))
  (cmp:irc-unreachable))


(defmethod translate-branch-instruction
    ((instruction clasp-cleavir-hir:throw-instruction) return-value inputs outputs successors abi function-info)
  (declare (ignore successors))
  (maybe-gen-cleanup-invocation-history function-info)
  (%intrinsic-call "cc_throw" (list (%load (first inputs)) (%load (second inputs))))
  (cmp:irc-unreachable))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Fixnum comparison instructions

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-add-instruction) return-value inputs outputs successors abi function-info)
  (let* ((x (%ptrtoint (%load (first inputs)) (%default-int-type abi)))
         (y (%ptrtoint (%load (second inputs)) (%default-int-type abi)))
         (result-with-overflow (%sadd.with-overflow x y abi)))
    (let ((val (%extract result-with-overflow 0 "result"))
          (overflow (%extract result-with-overflow 1 "overflow")))
      (%store (%inttoptr val cmp:%t*%) (first outputs))
      (%cond-br overflow (second successors) (first successors)))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-sub-instruction) return-value inputs outputs successors abi function-info)
  (let* ((x (%ptrtoint (%load (first inputs)) (%default-int-type abi)))
         (y (%ptrtoint (%load (second inputs)) (%default-int-type abi)))
         (result-with-overflow (%ssub.with-overflow x y abi)))
    (let ((val (%extract result-with-overflow 0 "result"))
          (overflow (%extract result-with-overflow 1 "overflow")))
      (%store (%inttoptr val cmp:%t*%) (first outputs))
      (%cond-br overflow (second successors) (first successors)))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-less-instruction) return-value inputs outputs successors abi function-info)
  (let* ((x (%ptrtoint (%load (first inputs)) (%default-int-type abi)))
         (y (%ptrtoint (%load (second inputs)) (%default-int-type abi)))
         (cmp-lt (%icmp-slt x y)))
      (%cond-br cmp-lt (first successors) (second successors))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-not-greater-instruction) return-value inputs outputs successors abi function-info)
  (let* ((x (%ptrtoint (%load (first inputs)) (%default-int-type abi)))
         (y (%ptrtoint (%load (second inputs)) (%default-int-type abi)))
         (cmp-lt (%icmp-sle x y)))
      (%cond-br cmp-lt (first successors) (second successors))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:fixnum-equal-instruction) return-value inputs outputs successors abi function-info)
  (let* ((x (%ptrtoint (%load (first inputs)) (%default-int-type abi)))
         (y (%ptrtoint (%load (second inputs)) (%default-int-type abi)))
         (cmp-lt (%icmp-eq x y)))
      (%cond-br cmp-lt (first successors) (second successors))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Float comparison instructions

;;; Thanks to LLVM semantics, we translate these identically for all
;;; float types. Types are more important for un/boxing.

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:float-less-instruction) return-value inputs outputs successors abi function-info)
  (let* ((x (%load (first inputs)))
         (y (%load (second inputs)))
         (cmp (%fcmp-olt x y)))
    (%cond-br cmp (first successors) (second successors))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:float-not-greater-instruction)
     return-value inputs outputs successors abi function-info)
  (let* ((x (%load (first inputs)))
         (y (%load (second inputs)))
         (cmp (%fcmp-ole x y)))
    (%cond-br cmp (first successors) (second successors))))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:float-equal-instruction)
     return-value inputs outputs successors abi function-info)
  (let* ((x (%load (first inputs)))
         (y (%load (second inputs)))
         (cmp (%fcmp-oeq x y)))
    (%cond-br cmp (first successors) (second successors))))
