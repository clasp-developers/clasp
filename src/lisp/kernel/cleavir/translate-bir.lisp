(defpackage #:clasp-cleavir-translate-bir
  (:use #:cl))

(in-package #:clasp-cleavir-translate-bir)

(defvar *tags*)
(defvar *datum-variables*)
(defvar *datum-allocas*)
(defvar *compiled-enters*)

(defun iblock-tag (iblock)
  (or (gethash iblock *tags*)
      (error "BUG: No tag for iblock: ~a" iblock)))

(defun in (datum)
  (check-type datum (or cleavir-bir:phi cleavir-bir:ssa))
  (or (gethash datum *datum-variables*)
      (error "BUG: No variable for datum: ~a" datum)))

(defun out (value datum)
  (check-type datum cleavir-bir:transfer)
  (assert (not (gethash datum *datum-variables*)))
  (setf (gethash datum *datum-variables*) value))

(defun phi-out (value datum iblock)
  (check-type datum cleavir-bir:phi)
  (unless (eq (cleavir-bir:rtype datum) :multiple-values)
    (llvm-sys:add-incoming (in datum) value (iblock-tag iblock))))

;;; For a computation, return its llvm value (for the :around method).
;;; For other instructions, return value is unspecified/irrelevant.
(defgeneric translate-simple-instruction (instruction return-value abi))

;;; Return value unspecified/irrelevant.
(defgeneric translate-terminator (instruction return-value abi next))

;;; Output the computation's value.
(defmethod translate-simple-instruction :around
    ((instruction cleavir-bir:computation) return-value abi)
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
              do (setf (gethash phi *datum-variables*)
                       (cmp:irc-phi cmp:%t*% ndefinitions))))))

(defun layout-iblock (iblock return-value abi)
  (cmp:irc-begin-block (iblock-tag iblock))
  (loop with end = (cleavir-bir:end iblock)
        for instruction = (cleavir-bir:start iblock)
          then (cleavir-bir:successor instruction)
        until (eq instruction end)
        do (translate-simple-instruction instruction return-value abi)
        finally (translate-terminator
                 instruction return-value abi
                 (mapcar #'iblock-tag (cleavir-bir:next end)))))

(defun layout-procedure* (the-function ir calling-convention
                          body-irbuilder body-block
                          abi &key (linkage 'llvm-sys:internal-linkage))
  (cmp:with-irbuilder (cmp:*irbuilder-function-alloca*)
    (let ((return-value (clasp-cleavir::alloca-return)))
      (clasp-cleavir::with-return-values (return-value abi nret ret-regs)
        (declare (ignore ret-regs))
        (cmp:irc-store (clasp-cleavir::%size_t 0) nret))
      (cmp:with-irbuilder (body-irbuilder)
        (cmp:irc-begin-block body-block)
        ;; Parse lambda list.
        (cmp:compile-lambda-list-code (cleavir-bir:lambda-list ir)
                                      calling-convention
                                      :argument-out #'out)
        ;; Branch to the start block.
        (cmp:irc-br (iblock-tag (cleavir-bir:start ir)))
        ;; Lay out blocks.
        (cleavir-bir:map-iblocks
         (lambda (ib) (layout-iblock ib return-value abi))
         ir))))
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
  (let* ((llvm-function-name (cmp:jit-function-name lambda-name))
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
  (print (cleavir-bir:disassemble bir))
  (or (gethash bir *compiled-enters*)
      (setf (gethash bir *compiled-enters*)
            (layout-procedure bir lambda-name abi :linkage linkage))))

(defun get-or-create-lambda-name (bir)
  (declare (ignore bir))
  'top-level)

(defun translate (bir &key abi linkage)
  (let ((*tags* (make-hash-table :test #'eq))
        (*datum-variables* (make-hash-table :test #'eq))
        (*datum-allocas* (make-hash-table :test #'eq))
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
    (cmp:jit-add-module-return-function
     cmp:*the-module*
     function startup-fn shutdown-fn ordered-raw-constants-list)))