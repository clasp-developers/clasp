(defpackage #:clasp-cleavir-translate-bir
  (:use #:cl))

(in-package #:clasp-cleavir-translate-bir)

(defvar *tags*)
(defvar *datum-values*)
(defvar *variable-allocas*)
(defvar *unwind-ids*)
(defvar *compiled-enters*)
(defvar *function-enclose-lists*)

(defun iblock-tag (iblock)
  (or (gethash iblock *tags*)
      (error "BUG: No tag for iblock: ~a" iblock)))

(defun in (datum)
  (check-type datum (or cleavir-bir:phi cleavir-bir:ssa))
  (or (gethash datum *datum-values*)
      (error "BUG: No variable for datum: ~a" datum)))

(defun variable-in (variable)
  (check-type variable cleavir-bir:variable)
  (ecase (cleavir-bir:extent variable)
    (:local (let ((alloca (or (gethash variable *variable-allocas*)
                              (error "BUG: Variable missing: ~a" variable))))
              (cmp:irc-load alloca)))
    (:indefinite (let ((cell (or (gethash variable *datum-values*)
                                 (error "BUG: Cell missing: ~a" variable)))
                       (offset (- cmp:+cons-car-offset+ cmp:+cons-tag+)))
                   (cmp:irc-load-atomic (cmp::gen-memref-address cell offset))))))

(defun out (value datum)
  (check-type datum cleavir-bir:transfer)
  (assert (not (gethash datum *datum-values*)))
  (setf (gethash datum *datum-values*) value))

(defun phi-out (value datum iblock)
  (check-type datum cleavir-bir:phi)
  (unless (eq (cleavir-bir:rtype datum) :multiple-values)
    (llvm-sys:add-incoming (in datum) value (iblock-tag iblock))))

(defun variable-out (value variable)
  (check-type variable cleavir-bir:variable)
  (ecase (cleavir-bir:extent variable)
    (:local
     (let ((alloca (or (gethash variable *variable-allocas*)
                       (error "BUG: Variable missing: ~a" variable))))
       (cmp:irc-store value alloca)))
    (:indefinite
     (let ((cell (or (gethash variable *datum-values*)
                     (error "BUG: Cell missing: ~a" variable)))
           (offset (- cmp:+cons-car-offset+ cmp:+cons-tag+)))
       (cmp:irc-store-atomic
        value
        (cmp::gen-memref-address cell offset))))))

(defun get-destination-id (iblock)
  (or (gethash iblock *unwind-ids*)
      (error "Missing unwind ID for ~a" iblock)))

(defun function-enclose-list (code)
  (or (gethash code *function-enclose-lists*)
      (setf (gethash code *function-enclose-lists*)
            (cleavir-set:filter 'list
                                (lambda (variable)
                                  (and (not (eq (cleavir-bir:owner variable) code))
                                       (not (eq (cleavir-bir:extent variable) :local))))
                                (cleavir-bir:variables code)))))

;;; For a computation, return its llvm value (for the :around method).
;;; For other instructions, return value is unspecified/irrelevant.
(defgeneric translate-simple-instruction (instruction return-value abi))

;;; Ditto
(defgeneric translate-terminator (instruction return-value abi next))

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

(defmethod translate-terminator ((instruction cleavir-bir:jump)
                                 return-value abi next)
  (declare (ignore return-value abi))
  (assert (= (length next) 1))
  (loop with ib = (cleavir-bir:iblock instruction)
        for in in (cleavir-bir:inputs instruction)
        for out in (cleavir-bir:outputs instruction)
        unless (eq (cleavir-bir:rtype out) :multiple-values)
          do (phi-out (in in) out ib))
  (cmp:irc-br (first next)))

(defmethod translate-terminator ((instruction cleavir-bir:eqi)
                                 return-value abi next)
  (declare (ignore return-value abi))
  (let ((inputs (cleavir-bir:inputs instruction)))
    (cmp:irc-cond-br
     (cmp:irc-icmp-eq (in (first inputs)) (in (second inputs)))
     (first next) (second next))))

(defun bind-if-necessary (var binder)
  (when (eq (cleavir-bir:binder var) binder)
    (ecase (cleavir-bir:extent var)
      (:local ; just an alloca
       (setf (gethash var *variable-allocas*)
             (cmp:alloca-t*)))
      (:indefinite ; make a cell
       (setf (gethash var *datum-values*)
             (clasp-cleavir::%intrinsic-invoke-if-landing-pad-or-call
              "cc_makeCell" nil ""))))))

(defmethod translate-terminator ((instruction cleavir-bir:catch)
                                 return-value abi next)
  (declare (ignore return-value abi))
  ;; Bind the variable if needed.
  (let ((u (cleavir-bir:use instruction))) ; avoid stupid check-type warning
    (check-type u cleavir-bir:writevar))
  (bind-if-necessary (first (cleavir-bir:outputs (cleavir-bir:use instruction)))
                     instruction)
  ;; Return the frame pointer for use in unwinds.
  (prog1 (clasp-cleavir::%intrinsic-call
          "llvm.frameaddress" (list (clasp-cleavir::%i32 0)) "frame")
    ;; Unconditional branch to the normal successor; dynamic environment stuff
    ;; is handled in layout-iblock.
    (cmp:irc-br (first next))))

(defmethod translate-terminator ((instruction cleavir-bir:unwind)
                                 return-value abi next)
  (declare (ignore abi next))
  (let* ((inputs (cleavir-bir:inputs instruction))
         (cont (first inputs))
         (rv (second inputs)))
    ;; We can only transmit multiple values, so make sure the adapter in
    ;; bir.lisp forced that properly
    (assert (and (= (length inputs) 2)
                 (cleavir-bir:rtype= (cleavir-bir:rtype rv) :multiple-values)))
    ;; Transmit those values
    (clasp-cleavir::save-multiple-value-0 return-value)
    ;; unwind
    (cmp:with-landing-pad (never-entry-landing-pad
                           (cleavir-bir:dynamic-environment instruction)
                           return-value)
      (clasp-cleavir::%intrinsic-invoke-if-landing-pad-or-call
       "cc_unwind"
       (list (in cont)
             (clasp-cleavir::%size_t
              (get-destination-id (cleavir-bir:destination instruction)))))))
  (cmp:irc-unreachable))

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
    (prog1 enclose
      (clasp-cleavir::%intrinsic-invoke-if-landing-pad-or-call
       "cc_initialize_closure"
       (list* enclose sninputs
              (mapcar (lambda (var)
                        (or (gethash var *datum-values*)
                            (error "BUG: Cell missing: ~a" var)))
                      enclose-list))))))

(defmethod translate-simple-instruction ((instruction cleavir-bir:writevar)
                                         return-value abi)
  (declare (ignore return-value abi))
  (variable-out (in (first (cleavir-bir:inputs instruction)))
                (first (cleavir-bir:outputs instruction))))

(defmethod translate-simple-instruction ((instruction cleavir-bir:readvar)
                                         return-value abi)
  (declare (ignore return-value abi))
  (variable-in (first (cleavir-bir:inputs instruction))))

(defmethod translate-simple-instruction ((instruction cleavir-bir:call)
                                         return-value abi)
  (declare (ignore abi))
  (let ((inputs (cleavir-bir:inputs instruction)))
    (clasp-cleavir::closure-call-or-invoke
     (in (first inputs)) return-value (mapcar #'in (rest inputs)))))

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

(defmethod translate-simple-instruction ((inst cleavir-bir:vprimop)
                                         return-value abi)
  (declare (ignore return-value abi))
  (let* ((info (cleavir-bir::info inst))
         (name (cleavir-bir::name info)))
    (ecase name
      (fdefinition
       (let ((symbol (in (first (cleavir-bir:inputs inst)))))
         (cmp:irc-fdefinition symbol))))))

(defmethod translate-simple-instruction ((inst cc-bir::precalc-value)
                                         return-value abi)
  (declare (ignore return-value abi))
  (let* ((index (cc-bir::precalc-value-index inst))
         (label (clasp-cleavir::safe-llvm-name
                 (cleavir-bir:constant-value inst))))
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
    (loop with end = (cleavir-bir:end iblock)
          for instruction = (cleavir-bir:start iblock)
            then (cleavir-bir:successor instruction)
          until (eq instruction end)
          do (translate-simple-instruction instruction return-value abi)
          finally (translate-terminator
                   instruction return-value abi
                   (mapcar #'iblock-tag (cleavir-bir:next end))))))

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
                  (closure-vec (first (llvm-sys:get-argument-list the-function))))
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
             (lambda (ib) (layout-iblock ib return-value abi))
             (cleavir-bir:start ir)))))))
  ;; Finish up by jumping from the entry block to the body block
  (cmp:with-irbuilder (cmp:*irbuilder-function-alloca*)
    (cmp:irc-br body-block))
  the-function)

(defun function-source-pos-info (irfunction)
  (declare (ignore irfunction))
  (core:make-source-pos-info "no-source-info-available" 999905 999905 999905))

(defun calculate-function-info (irfunction lambda-name)
  (cmp:make-function-info :function-name lambda-name
                          :lambda-list nil
                          :docstring nil
                          :declares nil
                          :form nil
                          :spi (core:make-source-pos-info
                                "no-source-info-available"
                                999905 999905 999905)))

(defun layout-procedure (ir lambda-name abi
                         &key (linkage 'llvm-sys:internal-linkage))
  ;;(print (cleavir-bir:disassemble ir))
  (let* ((*tags* (make-hash-table :test #'eq))
         (*datum-values* (make-hash-table :test #'eq))
         (*variable-allocas* (make-hash-table :test #'eq))
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
          (cleavir-bir:map-iblocks
           (lambda (ib)
             (setf (gethash ib *tags*)
                   (cmp:irc-basic-block-create "iblock"))
             (initialize-iblock-translation ib))
           ir)
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
  (declare (ignore bir))
  'top-level)

(defun translate (bir &key abi linkage)
  (let ((*function-enclose-lists* (make-hash-table :test #'eq))
        (*unwind-ids* (make-hash-table :test #'eq))
        (*compiled-enters* (make-hash-table :test #'eq))
        (lambda-name (get-or-create-lambda-name bir)))
    (memoized-layout-procedure bir lambda-name abi :linkage linkage)))

(defun ast->bir (ast)
  (cleavir-ast-to-bir:compile-toplevel ast))

(defun bir->bmir (ir env)
  (declare (ignore env))
  (cleavir-bir-transformations:process-captured-variables ir)
  (cleavir-bir-transformations:delete-temporary-variables ir)
  ir)

(defun bir-compile (form env pathname &key (linkage 'llvm-sys:internal-linkage))
  (let* (function
         ordered-raw-constants-list constants-table startup-fn shutdown-fn
         (cleavir-cst-to-ast:*compiler* 'cl:compile)
         (cst (cst:cst-from-expression form))
         (ast (clasp-cleavir::cst->ast cst env)))
    (cmp:with-debug-info-generator (:module cmp:*the-module* :pathname pathname)
      (multiple-value-setq (ordered-raw-constants-list constants-table startup-fn shutdown-fn)
        (literal:with-rtv
          (let* ((ast (clasp-cleavir::hoist-ast ast env))
                 (bir (ast->bir ast))
                 (bmir (bir->bmir bir env)))
            (cleavir-bir:verify bmir)
            (setq function
                  (translate bmir :abi clasp-cleavir::*abi-x86-64*
                                  :linkage linkage))))))
    (unless function
      (error "There was no function returned by translate-ast"))
    (llvm-sys:dump-module cmp:*the-module* *standard-output*)
    (cmp:jit-add-module-return-function
     cmp:*the-module*
     function startup-fn shutdown-fn ordered-raw-constants-list)))
