(defpackage #:clasp-bytecode-to-bir
  (:use #:cl)
  (:local-nicknames (#:bt #:clasp-bytecode-tablegen)
                    (#:bir #:cleavir-bir)
                    (#:set #:cleavir-set)
                    (#:ctype #:cleavir-ctype)
                    ;; FIXME: Move inserter stuff to its own small system
                    (#:ast-to-bir #:cleavir-ast-to-bir))
  (:export #:compile-function #:compile-hook))

(in-package #:clasp-bytecode-to-bir)

(defvar *active-annotations*)

(defun symbolicate (&rest components)
  ;; FIXME: Probably just use concatenate
  (let* ((strings (mapcar #'string components))
         (length (reduce #'+ strings :key #'length))
         (name (make-array length :element-type 'character)))
    (let ((index 0))
      (dolist (string strings (make-symbol name))
        (replace name string :start1 index)
        (incf index (length string))))))

(defun function-spi (function)
  (multiple-value-bind (path filepos lineno column)
      (core:function-source-pos function)
    (if path
        (core:make-source-pos-info :filename (namestring path)
                                   :filepos filepos
                                   :lineno lineno :column column)
        nil)))

(defun make-bir-function (bytecode-function module)
  (let* ((lambda-list (core:function-lambda-list bytecode-function))
         (function (make-instance 'bir:function
                     :returni nil ; set by :return compilation
                     :name (core:function-name bytecode-function)
                     :lambda-list nil
                     :docstring (core:function-docstring bytecode-function)
                     :original-lambda-list lambda-list
                     :origin (function-spi bytecode-function)
                     :policy nil
                     :attributes nil
                     :module module)))
    (set:nadjoinf (bir:functions module) function)
    function))

(defun next-arg (argspec bytecode opip ip nbytes)
  (cond
    ((cmpref::constant-arg-p argspec)
     (cons :constant (cmpref::bc-unsigned bytecode ip nbytes)))
    ((cmpref::label-arg-p argspec)
     (cons :label (+ opip (cmpref::bc-signed bytecode ip nbytes))))
    ((cmpref::keys-arg-p argspec)
     (cons :keys (cmpref::bc-unsigned bytecode ip nbytes)))
    (t (cons :operand (cmpref::bc-unsigned bytecode ip nbytes)))))

(defun bcfun/entry (bcfun)
  (core:global-bytecode-simple-fun/entry-pc-n bcfun))
(defun bcfun/locals-size (bcfun)
  (core:global-bytecode-simple-fun/locals-frame-size bcfun))
(defun bcfun/nvars (bcfun)
  (core:global-bytecode-simple-fun/environment-size bcfun))

(defun make-irblock (irfun &optional name)
  (let ((block (make-instance 'bir:iblock
                 :name name :function irfun :dynamic-environment irfun
                 :inputs ())))
    (set:nadjoinf (bir:scope irfun) block)
    block))

(defun make-start-block (irfun bcfun)
  (make-irblock irfun (symbolicate (write-to-string
                                    (core:function-name bcfun))
                                   '#:-start)))

(defun bytecode-module/functions (bytecode-module)
  (let ((list '()))
    (map nil (lambda (info)
               (when (typep info 'core:global-bytecode-simple-fun)
                 (setf list (merge 'list list
                                   (list info)
                                   #'<
                                   :key #'core:global-bytecode-simple-fun/entry-pc-n))))
         (core:bytecode-module/debug-info bytecode-module))
    list))

(defgeneric stack-push (datum context)
  (:argument-precedence-order context datum))
(defgeneric stack-pop (context))

(defclass context ()
  (;; Vector of conses of BIR:LEXICALs mapping to stack locations.
   ;; Each cons is (lexical . bool). The bool is true if the stack location
   ;; holds a cell.
   (%locals :initarg :locals :reader context-locals
            :type simple-vector)
   ;; Linear datum for current multiple values.
   (%mv :initarg :mv :initform nil :accessor context-mv
        :type (or null bir:linear-datum))
   ;; List of data representing the stack.
   ;; These are either linear data (I think all outputs), or, if a cell,
   ;; a datum (posibly a variable) wrapped in a list.
   (%stack :initarg :stack :initform nil :accessor context-stack
           :type list)
   ;; Block entries for successors to this block.
   (%successors :initarg :successors :accessor context-successors
                :type list)
   ;; Dynamic environment for this instruction.
   ;; Note that this may be out of sync with the block's dynamic environment
   ;; as it will be modified by terminators or semi-terminators like mvcall.
   (%dynamic-environment :initarg :dynamic-environment
                         :accessor context-dynamic-environment
                         :type bir:dynamic-environment)))

(defmethod stack-push (datum (context context))
  (push datum (context-stack context)))
(defmethod stack-pop ((context context))
  (let ((stack (context-stack context)))
    (if stack
        (pop (context-stack context))
        ;; This can happen in unreachable code, e.g.
        ;; the call in (block nil (car (return x)))
        (make-instance 'bir:output :name '#:unreachable))))

(defun %make-context (locals successors dynamic-environment)
  (make-instance 'context :locals locals :successors successors
                 :dynamic-environment dynamic-environment))

(defun copy-context (context)
  (make-instance 'context
    :locals (context-locals context) :mv (context-mv context)
    :stack (context-stack context)
    :successors (context-successors context)
    :dynamic-environment (context-dynamic-environment context)))

;;; Context for a block that has not been dominated by any blocks
;;; already processed. This can happen due to unreachable code, or
;;; from TAGBODY.
(defun make-nondom-context (context block)
  ;; The context passed in is for whatever block happens to precede
  ;; the nondominated block. I am not totally confident in this.
  ;; I think that it should work in practice, since said block will
  ;; be either a pseudo-predecessor for unreachable code
  ;; (e.g. in (foo 4) (return x) (foo 5), the first foo call's)
  ;; or the block establishing the tagbody dynenv.
  (make-instance 'context
    :locals (context-locals context) :mv nil :stack nil
    :successors (bt:block-entry-successors block)
    :dynamic-environment (context-dynamic-environment context)))

(defun make-context (function block annots)
  (let* ((bcfun (bt:function-entry-bcfun function))
         (irfun (bt:function-entry-extra function))
         (nlocals (bcfun/locals-size bcfun))
         (locals (make-array nlocals))
         (successors (bt:block-entry-successors block)))
    (%make-context locals successors irfun)))

;;; Alist of IR functions to sequences of variables they close over,
;;; used for CLOSURE instructions. Set by MAKE-CLOSURE and INITIALIZE-CLOSURE
;;; instructions.
;;; Each element of the sequence is either a BIR:LEXICAL or a cons
;;; (<a lexical> . t). The latter indicates it's stored in a cell.
(defvar *closures*)

(defun closure-variables (irfun)
  (let ((pair (assoc irfun *closures*)))
    (assert pair)
    (cdr pair)))

(defun new-closure-variables (irfun variables)
  (assert (null (assoc irfun *closures*)))
  (push (cons irfun variables) *closures*))

(defun fixup-jump (block phis maybe-mv inputs)
  (let ((jump (bir:end block)))
    (assert (null (bir:inputs jump)))
    (etypecase jump
      (bir:jump
       (let ((inputs (if maybe-mv (cons maybe-mv inputs) inputs)))
         (setf (bir:inputs jump) (copy-list inputs)
               (bir:outputs jump) (copy-list phis))))
      (bir:unwind
       (assert (null inputs))
       (let ((inputs (list maybe-mv)))
         (setf (bir:inputs jump) inputs
               (bir:outputs jump) (copy-list phis)))))))

;;; This function does the work of setting up the initial context for a block.
;;; This gets complicated when the block has multiple predecessors which
;;; push values (or set the MV). In that case, we initially
;;; (in compile-bytecode) set up jumps/blocks with no inputs, and then as soon
;;; as we see another predecessor we check how many stack values differ with
;;; the first predecessor's, if any.
;;; It's all pretty involved! Might be better to store this information in the
;;; bytecode itself somehow?
;;; Each entry is a list (block prev-block context).
;;; Initially, the context is nil. As soon as a jump from B to A is hit,
;;; prev-block is set as B and context is set as a copy of the context at
;;; the end of B. When a second jump, from C to A is hit, prev-block is
;;; set to NIL, A is given phis, and B and C's terminating jumps are
;;; rewritten to use those phis. When a third jump and thereafter is hit,
;;; the new block is rewritten.
(defun assign-block-context (entry new-predecessor new-context)
  (let* ((block-entry (first entry))
         (old-predecessor (second entry))
         (irblock (bt:block-entry-extra block-entry))
         (existing-inputs (bir:inputs irblock))
         (existing-context (third entry)))
    (cond
      ((null existing-context)
       ;; never before seen
       (assert (null old-predecessor))
       (let ((new-context (copy-context new-context)))
         (setf (context-successors new-context)
               (bt:block-entry-successors block-entry)
               (second entry) new-predecessor
               (third entry) new-context)))
      (t
       ;; Subsequently. We have to rewrite if old-predecessor is not NIL
       ;; (i.e. this is the second time through)
       (let* ((unwindp (typep (bir:end (bt:block-entry-extra
                                        new-predecessor))
                              'bir:unwind))
              (existing-mv (context-mv existing-context))
              (new-mv (context-mv new-context))
              (existing-stack (context-stack existing-context))
              (new-stack (context-stack new-context))
              (diff-mv (if new-mv (not (eql new-mv existing-mv)) nil))
              (diff-stack-pos
                (if unwindp
                    0
                    (or (mismatch existing-stack new-stack
                                  :test-not #'eql)
                        (length existing-stack))))
              (existing-diff-stack
                (subseq existing-stack 0 diff-stack-pos))
              (new-diff-stack (subseq new-stack 0 diff-stack-pos)))
         (assert (or unwindp
                     (eq (context-dynamic-environment new-context)
                         (context-dynamic-environment existing-context))))
         (assert (or unwindp
                     (= (length existing-stack) (length new-stack))))
         (assert (or (and new-mv existing-mv)
                     (and (null new-mv) (null existing-mv)))
                 (new-mv existing-mv)
                 "BUG: Inconsistency in MV from predecessor blocks: ~s vs. ~s"
                 new-mv existing-mv)
         ;; Make PHIs for whatever is not shared.
         (let ((inputs
                 (if old-predecessor
                     (let ((inputs1
                             (loop repeat diff-stack-pos
                                   collect (make-instance 'bir:phi
                                             :iblock irblock))))
                       (if diff-mv
                           (cons (make-instance 'bir:phi :iblock irblock)
                                 inputs1)
                           inputs1))
                     existing-inputs)))
           ;; If not already done, rewire users of the existing stuff
           ;; to use the PHIs instead.
           ;; Also, update the existing context.
           (when old-predecessor
             (setf (bir:inputs irblock) inputs)
             (cond
               (diff-mv
                (bir:replace-uses (first inputs) existing-mv)
                (loop for phi in (rest inputs)
                      for existing in existing-diff-stack
                      do (bir:replace-uses phi existing))
                (setf (context-mv existing-context) (first inputs)
                      (context-stack existing-context)
                      (append (rest inputs)
                              (nthcdr diff-stack-pos
                                      (context-stack existing-context)))))
               (t
                (loop for phi in inputs
                      for existing in existing-diff-stack
                      do (bir:replace-uses phi existing))
                (setf (context-stack existing-context)
                      (append inputs
                              (nthcdr diff-stack-pos
                                      (context-stack existing-context)))))))
           (when inputs
             (fixup-jump (bt:block-entry-extra new-predecessor) inputs
                         (when diff-mv new-mv) new-diff-stack)
             (when old-predecessor
               (fixup-jump (bt:block-entry-extra old-predecessor) inputs
                           (when diff-mv existing-mv)
                           existing-diff-stack)))
           ;; Mark that we've gone through twice and so don't need to do
           ;; rewrites again.
           (setf (second entry) nil)))))))

(defgeneric compile-instruction (mnemonic inserter
                                 annotation context &rest args))

;;; Need to refactor this stuff. We should _actually_ parse annotations
;;; as we go, in compile-bytecode or something, so we don't need
;;; to constantly reparse them like this.
(defun declared-var-ctype (varname &optional (annotations *active-annotations*))
  (loop with env = clasp-cleavir:*clasp-env*
        with sys = clasp-cleavir:*clasp-system*
        with ctype = (ctype:top sys)
        for annot in annotations
        when (typep annot 'core:bytecode-ast-decls)
          do (loop for (spec . rest)
                     in (core:bytecode-ast-decls/decls annot)
                   do (case spec
                        ((type)
                         (let ((nt (first rest)) (vars (rest rest)))
                           (when (member varname vars)
                             (let ((ct (cleavir-env:parse-type-specifier
                                        nt env sys)))
                               (setf ctype (ctype:conjoin sys ctype ct))))))
                        ;; FIXME: Hardcoded. Bad
                        ((dynamic-extent ftype ignorable ignore inline
                                         notinline optimize special
                                         core:lambda-name core:lambda-list))
                        (otherwise ; assume a type
                         (when (member varname rest)
                           (let ((ct (cleavir-env:parse-type-specifier
                                      spec env sys)))
                             (setf ctype (ctype:conjoin sys ctype ct)))))))
        finally (return ctype)))

(defun %read-variable (variable inserter)
  (bir:record-variable-ref variable)
  (let ((readvar-out (make-instance 'bir:output :name (bir:name variable))))
    (ast-to-bir:insert inserter 'bir:readvar
                       :inputs (list variable) :outputs (list readvar-out))
    readvar-out))

(defun read-variable (variable inserter)
  (let ((declared-ctype (declared-var-ctype (bir:name variable))))
    (compile-type-decl inserter :variable declared-ctype
                       (%read-variable variable inserter))))

(defun %bind-variable (variable value inserter)
  (let ((binder (ast-to-bir:insert inserter 'bir:leti
                                   :inputs (list value)
                                   :outputs (list variable))))
    (set:nadjoinf (bir:variables (bir:function binder)) variable)
    (setf (bir:binder variable) binder)))

(defun bind-variable (variable value inserter annots)
  (let* ((name (bir:name variable))
         (outer-ctype (declared-var-ctype name))
         (inner-ctype (declared-var-ctype name annots))
         (declared-ctype (ctype:conjoin clasp-cleavir:*clasp-system*
                                        outer-ctype inner-ctype)))
    (%bind-variable variable
                    (compile-type-decl inserter :setq declared-ctype value)
                    inserter)))

(defun %write-variable (variable value inserter)
  (assert (slot-boundp variable 'bir::%binder))
  (bir:record-variable-set variable)
  (ast-to-bir:insert inserter 'bir:writevar
                     :inputs (list value) :outputs (list variable)))

(defun write-variable (variable value inserter)
  (let ((declared-ctype (declared-var-ctype (bir:name variable))))
    (%write-variable variable
                     (compile-type-decl inserter :setq declared-ctype value)
                     inserter)))

(defmethod compile-instruction ((mnemonic (eql :ref)) inserter
                                annot context &rest args)
  (declare (ignore annot))
  (assert (= (length args) 1))
  (let ((varindex (first args)))
    (check-type varindex (integer 0))
    (let* ((varinfo (aref (context-locals context) varindex))
           (var (car varinfo))
           (cellp (cdr varinfo)))
      (stack-push
       (etypecase var
         (bir:variable
          (if cellp varinfo (read-variable var inserter)))
         (bir:come-from var))
       context))))

(defun inserter-module (inserter)
  ;; FIXME: Export a-t-b:iblock or something.
  (bir:module (bir:function (ast-to-bir::iblock inserter))))

(defun inserter-constant (value inserter)
  (bir:constant-in-module value (inserter-module inserter)))
(defun inserter-vcell (symbol inserter)
  (bir:variable-cell-in-module symbol (inserter-module inserter)))
(defun inserter-fcell (fname inserter)
  (bir:function-cell-in-module fname (inserter-module inserter)))

(defun compile-constant (value inserter)
  (let* ((const (inserter-constant value inserter))
         (constref-out (make-instance 'bir:output)))
    (ast-to-bir:insert inserter 'bir:constant-reference
                       :inputs (list const) :outputs (list constref-out))
    constref-out))

(defmethod compile-instruction ((mnemonic (eql :const)) inserter
                                annot context &rest args)
  (declare (ignore annot))
  (assert (= (length args) 1))
  (stack-push (compile-constant (first args) inserter) context))

(defmethod compile-instruction ((mnemonic (eql :closure)) inserter
                                annot context &rest args)
  (declare (ignore annot))
  (destructuring-bind (index) args
    (let* ((iblock (ast-to-bir::iblock inserter))
           (ifun (bir:function iblock))
           (vars (closure-variables ifun))
           (var (elt vars index)))
      (stack-push (etypecase var
                    ((cons bir:variable (eql t)) var) ; cell
                    (bir:variable (read-variable var inserter))
                    (bir:come-from var))
                  context))))

(defun gather (context n)
  (loop with args = nil
        repeat n
        do (push (stack-pop context) args)
        finally (return args)))

(defmethod compile-instruction ((mnemonic (eql :call)) inserter
                                annot context &rest args)
  (declare (ignore annot))
  (destructuring-bind (nargs) args
    (let ((args (gather context nargs))
          (callee (stack-pop context))
          (out (make-instance 'bir:output)))
      (ast-to-bir:insert inserter 'bir:call
                         :inputs (list* callee args)
                         :outputs (list out))
      (setf (context-mv context) out))))

(defmethod compile-instruction ((mnemonic (eql :call-receive-one))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (destructuring-bind (nargs) args
    (let ((args (gather context nargs))
          (callee (stack-pop context))
          (out (make-instance 'bir:output)))
      (ast-to-bir:insert inserter 'bir:call
                         :inputs (list* callee args)
                         :outputs (list out))
      (setf (context-mv context) nil) ; invalidate for self-consistency checks
      (stack-push out context))))

(defmethod compile-instruction ((mnemonic (eql :call-receive-fixed))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (destructuring-bind (nargs nvals) args
    (assert (zerop nvals)) ; FIXME
    (let ((args (gather context nargs))
          (callee (stack-pop context))
          (out (make-instance 'bir:output)))
      (setf (context-mv context) nil) ; invalidate for self-consistency checks
      (ast-to-bir:insert inserter 'bir:call
                         :inputs (list* callee args)
                         :outputs (list out)))))

(defun variable-ignore (varname annots)
  (loop for annot in annots
        when (typep annot 'core:bytecode-ast-decls)
          do (loop for (spec . rest) in (core:bytecode-ast-decls/decls annot)
                   do (case spec
                        ((ignore)
                         (when (member varname rest)
                           (return-from variable-ignore 'ignore)))
                        ((ignorable)
                         (when (member varname rest)
                           (return-from variable-ignore 'ignorable))))))
  nil)

;; A binding index annotation is (name . location)
;; and location is either an integer index or a list of an integer index,
;; the latter meaning it's closed over.
;; This function just grabs the actual index.
(defun annot-binding-index (info)
  (let ((location (cdr info)))
    (if (consp location) (car location) location)))

(defmethod compile-instruction ((mnemonic (eql :bind))
                                inserter annots context &rest args)
  (let* ((varannot (find-if (lambda (a) (typep a 'core:bytecode-debug-vars))
                            annots))
         (_ (assert varannot))
         (prim (core:bytecode-debug-vars/bindings varannot))
         ;; For BIND, the value for the last binding is the
         ;; most recently pushed, so we map to the bindings
         ;; in reverse order. This is a bit inefficient, though.
         (bindings (sort (copy-list prim) #'> :key #'annot-binding-index)))
    (declare (ignore _))
    (destructuring-bind (nvars base) args
      (declare (ignore base))
      (assert (= nvars (length bindings)))
      (loop with locals = (context-locals context)
            for (varname . location) in bindings
            for cellp = (consp location)
            for index = (if cellp (car location) location)
            for ignore = (variable-ignore varname annots)
            for var = (make-instance 'bir:variable
                        :ignore ignore :name varname)
            for value = (stack-pop context)
            for rvalue = (if cellp (car value) value)
            do (setf (aref locals index) (cons var cellp))
               (bind-variable var rvalue inserter annots)))))

(defmethod compile-instruction ((mnemonic (eql :set))
                                inserter annots context &rest args)
  (destructuring-bind (base) args
    (let* ((varannot (find-if (lambda (a) (typep a 'core:bytecode-debug-vars))
                              annots))
           (bindings
             (and varannot (core:bytecode-debug-vars/bindings varannot)))
           (locals (context-locals context))
           (varcons (aref locals base)))
      (cond
        ((find base bindings :key #'annot-binding-index)
         (let* ((binding (rassoc base bindings))
                (name (car binding))
                (ignore (variable-ignore name annots))
                (var (make-instance 'bir:variable
                       :ignore ignore :name name))
                (value (stack-pop context))
                (cellp (consp value))
                (rvalue (if cellp (car value) value)))
           (check-type rvalue bir:linear-datum)
           (setf (aref locals base) (cons var cellp))
           (bind-variable var rvalue inserter annots)))
        (t
         (let* ((var (car varcons))
                (value (stack-pop context))
                (cellp (consp value))
                (rvalue (if cellp (car value) value)))
           (check-type var bir:variable)
           ;; This is necessary because the attributes are sometimes attached
           ;; after a later SET. E.g. if a LET binds two cells.
           (setf (cdr varcons) cellp)
           (write-variable var rvalue inserter)))))))

(defmethod compile-instruction ((mnemonic (eql :make-cell))
                                inserter annot context &rest args)
  (declare (ignore annot inserter))
  (assert (null args))
  (let ((top (stack-pop context)))
    (check-type top bir:linear-datum)
    ;; Indicate that this is a cell.
    ;; We could save a cons here by just mutating, but that would make things
    ;; more confusing when we need to save the stack for savesp and stuff.
    (stack-push (list top) context)))

(defmethod compile-instruction ((mnemonic (eql :cell-ref))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (assert (null args))
  (let ((cell (stack-pop context)))
    ;; make sure it's a bound cell (not directly from make-cell, which would be
    ;; illegal)
    (check-type cell (cons bir:variable))
    (stack-push (read-variable (car cell) inserter) context)))

(defmethod compile-instruction ((mnemonic (eql :cell-set))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (assert (null args))
  (let ((cell (stack-pop context))
        (val (stack-pop context)))
    (check-type cell (cons bir:variable))
    (check-type val bir:linear-datum)
    (write-variable (car cell) val inserter)))

(defun variable-from-output (output)
  (let ((def (bir:definition output)))
    (etypecase def
      (bir:readvar (bir:input def))
      (bir:thei
       (let ((tdef (bir:definition (bir:input def))))
         (check-type tdef bir:readvar)
         (bir:input tdef))))))

(defun resolve-closed (v)
  ;; Each thing on the stack is either a come-from, a list
  ;; (variable . t) if there's a cell, or the output of a readvar for
  ;; variables with no cell.
  (etypecase v
    (bir:come-from v)
    (bir:output (variable-from-output v))
    ((cons bir:variable (eql t)) v)))

(defmethod compile-instruction ((mnemonic (eql :make-closure))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (destructuring-bind (template) args
    (let* ((irfun (bc->ir template))
           (enclose-out (make-instance 'bir:output))
           (enclose (make-instance 'bir:enclose
                      :code irfun :outputs (list enclose-out)))
           (nclosed (bcfun/nvars template))
           (closed (nreverse (subseq (context-stack context) 0 nclosed)))
           (real-closed (mapcar #'resolve-closed closed)))
      (assert (every (lambda (v) (typep v '(or bir::lexical
                                            (cons bir::lexical (eql t)))))
                     real-closed))
      (setf (context-stack context)
            (nthcdr nclosed (context-stack context)))
      (new-closure-variables irfun real-closed)
      (setf (bir:enclose irfun) enclose)
      (ast-to-bir:insert inserter enclose)
      (stack-push enclose-out context))))

(defmethod compile-instruction ((mnemonic (eql :make-uninitialized-closure))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (destructuring-bind (template) args
    (let* ((irfun (bc->ir template))
           (enclose-out (make-instance 'bir:output))
           (enclose (make-instance 'bir:enclose
                      :code irfun :outputs (list enclose-out))))
      (setf (bir:enclose irfun) enclose)
      (ast-to-bir:insert inserter enclose)
      (stack-push enclose-out context))))

(defmethod compile-instruction ((mnemonic (eql :initialize-closure))
                                inserter annot context &rest args)
  (declare (ignore inserter annot))
  (destructuring-bind (index) args
    (destructuring-bind (var . cellp)
        (aref (context-locals context) index)
      (check-type var bir:variable)
      (assert (not cellp))
      (let ((leti (bir:binder var)))
        (check-type leti bir:leti)
        (let* ((dat (bir:input leti))
               (enclose (bir:definition dat)))
          (check-type enclose bir:enclose)
          (let* ((irfun (bir:code enclose))
                 (template (ir->bc irfun))
                 (nclosed (bcfun/nvars template))
                 (closed (nreverse
                          (subseq (context-stack context) 0 nclosed)))
                 (real-closed (mapcar #'resolve-closed closed)))
            (setf (context-stack context)
                  (nthcdr nclosed (context-stack context)))
            (new-closure-variables irfun real-closed)))))))

(defmethod compile-instruction ((mnemonic (eql :return))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (assert (null args))
  ;; KLUDGE
  (unless (slot-boundp (ast-to-bir::iblock inserter) 'bir::%end)
    (let ((input (context-mv context)))
      (check-type input bir:linear-datum)
      (let ((returni (make-instance 'bir:returni :inputs (list input))))
        (setf (bir:returni (bir:function (ast-to-bir::iblock inserter))) returni)
        (ast-to-bir:terminate inserter returni)))))

(defmethod compile-instruction ((mnemonic (eql :bind-required-args))
                                inserter annots context &rest args)
  (destructuring-bind (nreq) args
    (let* ((varannot
             (find-if (lambda (a) (typep a 'core:bytecode-debug-vars))
                      annots))
           (bindings
             (and varannot (core:bytecode-debug-vars/bindings varannot)))
           (iblock (ast-to-bir::iblock inserter))
           (ifun (bir:function iblock)))
      (assert (= (length bindings) nreq))
      (loop with locals = (context-locals context)
            for i from 0
            for (name . index) in bindings
            for cellp = (consp index)
            for rindex = (if cellp (car index) index)
            for ignore = (variable-ignore name annots)
            for arg = (make-instance 'bir:argument
                        :name name :function ifun)
            for var = (make-instance 'bir:variable
                        :name name :ignore ignore)
            do (assert (= i rindex))
               (setf (aref locals i) (cons var cellp))
               (bind-variable var arg inserter annots)
            collect arg into args
            finally (setf (bir:lambda-list ifun) args)))))

(defmethod compile-instruction ((mnemonic (eql :bind-optional-args))
                                inserter annots context &rest args)
  (destructuring-bind (start nopt) args
    (let* ((iblock (ast-to-bir::iblock inserter))
           (ifun (bir:function iblock))
           (ll (bir:lambda-list ifun)))
      (loop with locals = (context-locals context)
            repeat nopt
            for i from start
            for arg = (make-instance 'bir:argument :function ifun)
            for -p = (make-instance 'bir:argument :function ifun)
            for var = (make-instance 'bir:variable :ignore nil)
            ;; We use %bind rather than bind because we don't want
            ;; to assert the type of a possibly unprovided argument.
            do (setf (aref locals i) (cons var nil))
               (%bind-variable var arg inserter)
            collect (list arg -p) into ll-app
            finally (setf (bir:lambda-list ifun)
                          (append ll '(&optional) ll-app))))))

;;; FIXME: Why does this instruction not put it immediately into a var
(defmethod compile-instruction ((mnemonic (eql :listify-rest-args))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (destructuring-bind (start) args
    (let* ((iblock (ast-to-bir::iblock inserter))
           (ifun (bir:function iblock))
           (ll (bir:lambda-list ifun))
           (rarg (make-instance 'bir:argument :function ifun)))
      (setf (bir:lambda-list ifun) (append ll `(&rest ,rarg)))
      (stack-push rarg context))))
(defmethod compile-instruction ((mnemonic (eql :vaslistify-rest-args))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (destructuring-bind (start) args
    (let* ((iblock (ast-to-bir::iblock inserter))
           (ifun (bir:function iblock))
           (ll (bir:lambda-list ifun))
           (rarg (make-instance 'bir:argument :function ifun)))
      (setf (bir:lambda-list ifun) (append ll `(core:&va-rest ,rarg)))
      (stack-push rarg context))))

(defmethod compile-instruction ((mnemonic (eql :parse-key-args))
                                inserter annots context &rest args)
  (destructuring-bind (start (key-count . aokp) keys frame-start) args
    (declare (ignore key-count))
    (let* ((iblock (ast-to-bir::iblock inserter))
           (ifun (bir:function iblock))
           (ll (bir:lambda-list ifun)))
      (loop with locals = (context-locals context)
            for i from frame-start
            for key in keys
            for arg = (make-instance 'bir:argument :function ifun)
            for -p = (make-instance 'bir:argument :function ifun)
            for var = (make-instance 'bir:variable :name key :ignore nil)
            do (setf (aref locals i) (cons var nil))
               ;; %bind to avoid asserting type of unprovided arg
               (%bind-variable var arg inserter)
            collect (list key arg -p) into ll-app
            finally (setf (bir:lambda-list ifun)
                          (append ll `(&key ,@ll-app)
                                  (if aokp
                                      '(&allow-other-keys)
                                      ())))))))

(defun compile-jump (inserter context destination)
  (declare (ignore context))
  (ast-to-bir:terminate inserter 'bir:jump
                        :next (list (bt:block-entry-extra destination))))

(defmethod compile-instruction ((mnemonic (eql :jump-8))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (compile-jump inserter context (first args)))
(defmethod compile-instruction ((mnemonic (eql :jump-16))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (compile-jump inserter context (first args)))
(defmethod compile-instruction ((mnemonic (eql :jump-24))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (compile-jump inserter context (first args)))

(defun compile-jump-if (inserter context condition true-entry)
  (let* ((successors (context-successors context))
         (false-entry (if (eq true-entry (first successors))
                          (second successors)
                          (first successors)))
         (true-dest (bt:block-entry-extra true-entry))
         (false-dest (bt:block-entry-extra false-entry)))
    (ast-to-bir:terminate inserter 'bir:ifi
                          :inputs (list condition)
                          :next (list true-dest false-dest))))

(defmethod compile-instruction ((mnemonic (eql :jump-if-8))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (compile-jump-if inserter context (stack-pop context) (first args)))
(defmethod compile-instruction ((mnemonic (eql :jump-if-16))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (compile-jump-if inserter context (stack-pop context) (first args)))
(defmethod compile-instruction ((mnemonic (eql :jump-if-24))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (compile-jump-if inserter context (stack-pop context) (first args)))                                                   
(defun compile-jump-if-supplied (inserter context index true-entry)
  (destructuring-bind (var . cellp)
      (aref (context-locals context) index)
    (assert (not cellp))
    ;; Find and branch on the corresponding -p argument.
    (let* ((binder (bir:binder var))
           (arg (bir:input binder))
           (iblock (ast-to-bir::iblock inserter))
           (ifun (bir:function iblock))
           (-p (loop for e in (bir:lambda-list ifun)
                     when (consp e)
                       do (ecase (length e)
                            (2 (when (eq (first e) arg)
                                 (return (second e))))
                            (3 (when (eq (second e) arg)
                                 (return (third e))))))))
      (assert (typep arg 'bir:argument))
      (compile-jump-if inserter context -p true-entry))))

(defmethod compile-instruction ((mnemonic (eql :jump-if-supplied-8))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (destructuring-bind (index true-entry) args
    (compile-jump-if-supplied inserter context index true-entry)))
(defmethod compile-instruction ((mnemonic (eql :jump-if-supplied-16))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (destructuring-bind (index true-entry) args
    (compile-jump-if-supplied inserter context index true-entry)))

;;; Lambda lists are handled by other means.
(defmethod compile-instruction ((mnemonic (eql :check-arg-count-le))
                                inserter annot context &rest args)
  (declare (ignore inserter annot context args)))
(defmethod compile-instruction ((mnemonic (eql :check-arg-count-ge))
                                inserter annot context &rest args)
  (declare (ignore inserter annot context args)))
(defmethod compile-instruction ((mnemonic (eql :check-arg-count-eq))
                                inserter annot context &rest args)
  (declare (ignore inserter annot context args)))

(defmethod compile-instruction ((mnemonic (eql :push-values))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (assert (null args))
  (let ((mv (context-mv context))
        (save-out (make-instance 'bir:output :name '#:saved-values))
        (next (mapcar #'bt:block-entry-extra
                      (context-successors context))))
    (check-type mv bir:linear-datum)
    (let ((save (ast-to-bir:terminate inserter 'bir:values-save
                                      :inputs (list mv)
                                      :outputs (list save-out)
                                      :next next)))
      (setf (context-mv context) nil
            (context-dynamic-environment context) save)
      (stack-push (list :multiple-values save-out) context))))

(defmethod compile-instruction ((mnemonic (eql :append-values))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (assert (null args))
  (let ((mv (context-mv context))
        (save-out (make-instance 'bir:output :name '#:saved-values))
        (next (mapcar #'bt:block-entry-extra
                      (context-successors context)))
        (previous (stack-pop context)))
    (check-type mv bir:linear-datum)
    (check-type previous (cons (eql :multiple-values)))
    (let ((save (ast-to-bir:terminate inserter 'bir:values-save
                                      :inputs (list mv)
                                      :outputs (list save-out)
                                      :next next)))
      (setf (context-mv context) nil
            (context-dynamic-environment context) save)
      (stack-push (list* :multiple-values save-out (cdr previous))
                  context))))

(defmethod compile-instruction ((mnemonic (eql :pop-values))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (assert (null args))
  (let ((previous (stack-pop context)))
    (check-type previous (cons (eql :multiple-values)
                               (cons bir:linear-datum null)))
    (let* ((mv (second previous))
           (save (bir:definition mv))
           (read-out (make-instance 'bir:output
                       :name '#:restored-values))
           (old-de (bir:dynamic-environment save)))
      (check-type save bir:values-save)
      (ast-to-bir:insert inserter 'bir:values-restore
                         :inputs (list mv) :outputs (list read-out))
      (setf (context-mv context) read-out
            (context-dynamic-environment context) old-de))))

(defun compile-mv-call (inserter context)
  (let ((previous (stack-pop context))
        (callee (stack-pop context))
        (out (make-instance 'bir:output)))
    (check-type previous (cons (eql :multiple-values) cons))
    (let* ((last-arg (second previous))
           (lastdef (bir:definition last-arg))
           (mv (bir:output lastdef))
           (args (reverse (rest previous)))
           (firstdef (bir:definition (first args)))
           (old-de (bir:dynamic-environment firstdef)))
      (check-type lastdef bir:values-save)
      ;; Morph the most recent values-save into a -collect
      ;; so that we have a proper mv call
      (change-class lastdef 'bir:values-collect
                    :inputs (append (butlast args)
                                    (bir:inputs lastdef)))
      ;; Generate the actual call
      (ast-to-bir:insert inserter 'bir:mv-call
                         :inputs (list callee mv)
                         :outputs (list out))
      (setf (context-dynamic-environment context) old-de))
    out))

(defmethod compile-instruction ((mnemonic (eql :mv-call))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (assert (null args))
  (setf (context-mv context) (compile-mv-call inserter context)))
(defmethod compile-instruction ((mnemonic (eql :mv-call-receive-one))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (assert (null args))
  (stack-push (compile-mv-call inserter context) context)
  (setf (context-mv context) nil))
(defmethod compile-instruction ((mnemonic (eql :mv-call-receive-fixed))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (destructuring-bind (nvals) args
    (assert (zerop nvals)) ; FIXME
    (compile-mv-call inserter context)
    (setf (context-mv context) nil)))

(defmethod compile-instruction ((mnemonic (eql :save-sp))
                                inserter annot context &rest args)
  (declare (ignore inserter annot))
  (destructuring-bind (index) args
    (setf (aref (context-locals context) index)
          (cons (context-stack context) nil))))

(defmethod compile-instruction ((mnemonic (eql :restore-sp))
                                inserter annot context &rest args)
  (declare (ignore inserter annot))
  (destructuring-bind (index) args
    (destructuring-bind (stack . cellp) (aref (context-locals context) index)
      (assert (not cellp))
      (assert (listp stack))
      (setf (context-stack context) stack))))

(defmethod compile-instruction ((mnemonic (eql :entry))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (destructuring-bind (index) args
    (let* ((next (mapcar #'bt:block-entry-extra
                         (context-successors context)))
           (cf (ast-to-bir:terminate inserter 'bir:come-from
                                     :next next))
           (function (bir:function (ast-to-bir::iblock inserter))))
      (set:nadjoinf (bir:come-froms function) cf)
      (setf (aref (context-locals context) index) (cons cf nil)
            (context-dynamic-environment context) cf))))

(defun compile-exit (inserter context destination)
  (let ((cf (stack-pop context))
        (dest (bt:block-entry-extra destination)))
    (check-type cf bir:come-from)
    (let ((uw (ast-to-bir:terminate inserter 'bir:unwind
                                    :inputs () :outputs ()
                                    :come-from cf
                                    :destination dest)))
      ;; Don't add duplicate NEXT entries
      ;; (obscure NLX uses can hit this, like CORE::PACKAGES-ITERATOR)
      (unless (eql dest (first (bir:next cf)))
        (pushnew dest (rest (bir:next cf))))
      (set:nadjoinf (bir:predecessors dest) (bir:iblock cf))
      (set:nadjoinf (bir:unwinds cf) uw)
      (set:nadjoinf (bir:entrances dest)
                    (ast-to-bir::iblock inserter)))))

(defmethod compile-instruction ((mnemonic (eql :exit-8))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (destructuring-bind (destination) args
    (compile-exit inserter context destination)))
(defmethod compile-instruction ((mnemonic (eql :exit-16))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (destructuring-bind (destination) args
    (compile-exit inserter context destination)))
(defmethod compile-instruction ((mnemonic (eql :exit-24))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (destructuring-bind (destination) args
    (compile-exit inserter context destination)))

(defmethod compile-instruction ((mnemonic (eql :entry-close))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (assert (null args))
  (let ((next (mapcar #'bt:block-entry-extra
                      (context-successors context))))
    (ast-to-bir:terminate inserter 'bir:jump
                          :inputs () :outputs () :next next)
    (let ((de (context-dynamic-environment context)))
      (assert (typep de 'bir:come-from))
      (setf (context-dynamic-environment context)
            (bir:dynamic-environment de)))))

(defmethod compile-instruction ((mnemonic (eql :special-bind))
                                 inserter annot context &rest args)
  (declare (ignore annot))
  (destructuring-bind (vcell) args
    (let* ((next (mapcar #'bt:block-entry-extra
                         (context-successors context)))
           (const (inserter-vcell (core:variable-cell/name vcell) inserter))
           (value (stack-pop context))
           (bind (ast-to-bir:terminate inserter 'bir:constant-bind
                                       :inputs (list const value)
                                       :next next)))
      (setf (context-dynamic-environment context) bind))))

(defmethod compile-instruction ((mnemonic (eql :symbol-value))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (destructuring-bind (vcell) args
    (let* ((symbol (core:variable-cell/name vcell))
           (const (inserter-vcell symbol inserter))
           (out (make-instance 'bir:output :name symbol)))
      (ast-to-bir:insert inserter 'bir:constant-symbol-value
                         :inputs (list const) :outputs (list out))
      (stack-push out context))))

(defmethod compile-instruction ((mnemonic (eql :symbol-value-set))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (destructuring-bind (vcell) args
    (let ((const (inserter-vcell (core:variable-cell/name vcell) inserter))
          (in (stack-pop context)))
      (ast-to-bir:insert inserter 'bir:set-constant-symbol-value
                         :inputs (list const in)))))

(defmethod compile-instruction ((mnemonic (eql :unbind))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (assert (null args))
  (let ((next (mapcar #'bt:block-entry-extra (context-successors context))))
    (ast-to-bir:terminate inserter 'bir:jump
                          :inputs () :outputs () :next next)
    (let ((de (context-dynamic-environment context)))
      (assert (typep de 'bir:constant-bind))
      (setf (context-dynamic-environment context)
            (bir:dynamic-environment de)))))

(defmethod compile-instruction ((mnemonic (eql :fdefinition))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (destructuring-bind (fcell) args
    (let* (;; FIXME: May not be a sufficiently reliable way to get
           ;; the name from the cell in all cases? Probably ok though
           (fname (core:function-name fcell))
           (const (inserter-fcell fname inserter))
           (attributes (clasp-cleavir::function-attributes fname))
           (fdef-out (make-instance 'bir:output
                       :name fname :attributes attributes)))
      (ast-to-bir:insert inserter 'bir:constant-fdefinition
                         :inputs (list const) :outputs (list fdef-out))
      (stack-push fdef-out context))))

;; Identical to the above, but BIR should maybe have a
;; CONSTANT-CALLED-FDEFINITION for this.
(defmethod compile-instruction ((mnemonic (eql :called-fdefinition))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (destructuring-bind (fcell) args
    (let* ((fname (core:function-name fcell))
           (const (inserter-fcell fname inserter))
           (attributes (clasp-cleavir::function-attributes fname))
           (fdef-out (make-instance 'bir:output
                       :name fname :attributes attributes)))
      (ast-to-bir:insert inserter 'bir:constant-fdefinition
                         :inputs (list const) :outputs (list fdef-out))
      (stack-push fdef-out context))))

(defmethod compile-instruction ((mnemonic (eql :fdesignator))
                                inserter annot context &rest args)
  ;; Just call CORE:COERCE-CALLED-FDESIGNATOR.
  (declare (ignore annot))
  (let* ((desig (stack-pop context))
         (fname 'core:coerce-called-fdesignator)
         (const (inserter-fcell fname inserter))
         (attributes (clasp-cleavir::function-attributes fname))
         (fdef-out (make-instance 'bir:output
                     :name fname :attributes attributes))
         (out (make-instance 'bir:output :name '#:callee)))
    (ast-to-bir:insert inserter 'bir:constant-fdefinition
                       :inputs (list const) :outputs (list fdef-out))
    (ast-to-bir:insert inserter 'bir:call
                       :inputs (list fdef-out desig)
                       :outputs (list out))
    (stack-push out context)))

(defmethod compile-instruction ((mnemonic (eql :nil))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (assert (null args))
  (stack-push (compile-constant 'nil inserter) context))

(defmethod compile-instruction ((mnemonic (eql :push))
                                inserter annots context &rest args)
  (declare (ignore inserter annots))
  (assert (null args))
  (let ((mv (context-mv context)))
    (etypecase mv
      (null) ; happens in unreachable code
      (bir:linear-datum
       (setf (context-mv context) nil)
       (stack-push mv context)))))

(defmethod compile-instruction ((mnemonic (eql :pop))
                                inserter annot context &rest args)
  (declare (ignore inserter annot args))
  (let ((mv (stack-pop context)))
    (check-type mv bir:linear-datum)
    (setf (context-mv context) mv)))

(defmethod compile-instruction ((mnemonic (eql :dup))
                                inserter annot context &rest args)
  (declare (ignore annot args))
  (let ((var (make-instance 'bir:variable :ignore nil)))
    (%bind-variable var (stack-pop context) inserter)
    (stack-push (%read-variable var inserter) context)
    (stack-push (%read-variable var inserter) context)))

(defun compile-type-decl (inserter which ctype datum)
  (let ((sys clasp-cleavir:*clasp-system*)
        (sv-ctype-p (member which '(:variable :argument :setq))))
    (if (if sv-ctype-p
            (ctype:top-p ctype sys)
            (clasp-cleavir::values-top-p ctype sys))
        datum ; too boring
        (let* ((vctype (ecase which
                         ((:the :return) ctype)
                         ((:variable) (ctype:single-value ctype sys))
                         ((:argument :setq)
                          (ctype:coerce-to-values ctype sys))))
               (out (make-instance 'bir:output
                      :name (bir:name datum)))
               (policy bir:*policy*)
               (type-check-function
                 (ecase (clasp-cleavir::insert-type-checks-level policy which)
                   ((0) :trusted)
                   ((1) nil)
                   ((2 3)
                    (compile-bcfun-into-module
                     (cmp:bytecompile
                      (clasp-cleavir::make-type-check-fun which ctype sys))
                     (bir:module (bir:function
                                  (ast-to-bir::iblock inserter))))))))
          (ast-to-bir:insert inserter 'bir:thei
                             :inputs (list datum)
                             :outputs (list out)
                             :asserted-type vctype
                             :type-check-function type-check-function)
          out))))

;;; Return values: mnemonic, parsed/resolved arguments, IP of next instruction
(defun parse-instruction (bytecode literals block-alist longp ip)
  (let* ((opip ip)
         (op (cmpref::decode-instr (aref bytecode ip)))
         ;; FIXME: Use symbol instead of strings in decode-instr
         (mnemonic (intern (string-upcase (first op)) "BYTECODE-TO-BIR")))
    (incf ip) ; move past opcode itself
    (flet ((arg (spec)
             (multiple-value-bind (arg nbytes)
                 (next-arg spec bytecode literals block-alist opip ip)
               (incf ip nbytes)
               arg)))
      (values mnemonic (mapcar #'arg (if longp (fourth op) (third op))) ip))))

(defun make-inserter (block)
  (let ((ins (make-instance 'ast-to-bir:inserter)))
    (ast-to-bir:begin ins block)
    ins))

(defun find-block (ip block-entries)
  (or (find ip block-entries :key #'bt:block-entry-start)
      (error "BUG: IP ~d does not correspond to block" ip)))

(defun function-entry-nlocals (function-entry)
  (core:global-bytecode-simple-fun/locals-frame-size
   (bt:function-entry-bcfun function-entry)))

(defun dynamic-environment-for-block (block-entry irfun)
  (let ((predecessors (bt:block-entry-predecessors block-entry)))
    (cond ((= (length predecessors) 1)
           (let* ((pblock (bt:block-entry-extra (first predecessors)))
                  (term (bir:end pblock)))
             (if (typep term 'bir:dynamic-environment)
                 term
                 (bir:dynamic-environment pblock))))
          ((null predecessors)
           (assert (typep irfun 'bir:function))
           irfun)
          (t
           (assert (reduce #'eql predecessors
                           :key (lambda (entry)
                                  (bir:dynamic-environment
                                   (bt:block-entry-extra entry)))))
           (bir:dynamic-environment
            (bt:block-entry-extra (first predecessors)))))))

(defun maybe-compile-the (annots inserter context)
  ;; TODO: Handle multiple THE annotations at same position
  (let ((the
          (find-if (lambda (a) (typep a 'core:bytecode-ast-the))
                   annots)))
    (when the
      (let ((type (core:bytecode-ast-the/type the))
            (receiving (core:bytecode-ast-the/receiving the)))
        (case receiving
          ((1) (stack-push
                (compile-type-decl inserter :the
                                   (cleavir-env:parse-values-type-specifier
                                    type clasp-cleavir:*clasp-env*
                                    clasp-cleavir:*clasp-system*)
                                   (stack-pop context))
                context))
          ((-1)
           (setf (context-mv context)
                 (compile-type-decl inserter :the
                                   (cleavir-env:parse-values-type-specifier
                                    type clasp-cleavir:*clasp-env*
                                    clasp-cleavir:*clasp-system*)
                                   (context-mv context))))
          ;; TODO: Something for 0/single values?
          (otherwise))))))

;;; Find all annotations with a scope starting at the next instruction.
(defun find-next-annotations (ip annotations astart)
  (loop for index from astart below (length annotations)
        for annot = (aref annotations index)
        for start = (core:bytecode-debug-info/start annot)
        until (> start ip) ; annotations are sorted
        collect annot))

(defun find-origin (active-annotations)
  ;; Since the tightest annotations are first, just return the origin
  ;; for the first source info.
  (loop for annot in active-annotations
        when (typep annot 'core:bytecode-debug-location)
          return (let ((spi (core:bytecode-debug-location/location annot)))
                   ;; KLUDGE KLUDGE KLUDGE
                   ;; BIR currently checks that all source infos are
                   ;; (source) CSTs. We don't have those.
                   ;; Probably BIR needs adjustment.
                   (make-instance 'cst:atom-cst
                     :raw nil :source (cons spi spi)))))

(defun compute-optimize (active-annotations)
  (loop with optimize = nil
        for annot in active-annotations
        when (typep annot 'core:bytecode-ast-decls)
          do (loop for (spec . rest)
                     in (core:bytecode-ast-decls/decls annot)
                   when (eq spec 'cl:optimize)
                     do (loop for optim in rest
                              for roptim = (if (consp optim)
                                               optim
                                               `(,optim 3))
                              unless (assoc (car roptim) optimize)
                                do (push roptim optimize)))
        finally (return
                  ;; If the declarations don't have enough OPTIMIZE qualities
                  ;; to cover the gamut (arguably they always should), fill
                  ;; in the rest from the global values.
                  (loop for optim in cmp:*optimize*
                        unless (assoc (car optim) optimize)
                          do (push optim optimize)
                        finally (return optimize)))))

(defun update-annotations (active-annotations ip annots index
                           optimize policy exit-contexts context)
  (let ((recompute-optimize nil))
    ;; Remove obsolete annotations.
    (setf active-annotations
          (delete-if (lambda (annot)
                       (let ((result
                               (<= (core:bytecode-debug-info/end annot) ip)))
                         (when result
                           (typecase annot
                             (core:bytecode-ast-decls
                              (setf recompute-optimize t))))
                         result))
                     active-annotations))
    ;; Add new ones and update the index.
    (loop with len = (length annots)
          while (< index len)
          while (eql ip (core:bytecode-debug-info/start
                         (aref annots index)))
          do (let* ((annot (aref annots index))
                    (aend (core:bytecode-debug-info/end annot)))
               (incf index)
               ;; degenerate annotations happen sometimes
               ;; this could be tightened up in the bc compiler,
               ;; but checking for it is easy.
               ;; Also, THE are intentionally degenerate.
               (unless (eql ip aend)
                 (push annot active-annotations)
                 (typecase annot
                   (core:bytecode-ast-decls
                    (setf recompute-optimize t))
                   (core:bytecode-ast-exit
                    ;; if we're already in an exit that ends at the same spot,
                    ;; ignore this nested one.
                    (unless (assoc aend exit-contexts)
                      ;; Make a new context with whatever values appended.
                      (let ((r (core:bytecode-ast-exit/receiving annot))
                            (c (copy-context context)))
                        (if (minusp r)
                            (setf (context-mv c)
                                  (make-instance 'bir:output
                                    :name '#:unreachable))
                            (loop repeat r
                                  for o = (make-instance 'bir:output
                                            :name '#:unreachable)
                                  do (stack-push o c)))
                        (push (cons aend c) exit-contexts))))))))
    ;; Recompute optimize and policy if necessary.
    (when recompute-optimize
      (setf optimize (compute-optimize active-annotations)
            policy (cleavir-policy:compute-policy optimize clasp-cleavir:*clasp-env*)))
    (values active-annotations index optimize policy exit-contexts)))

(defun compute-args (args literals all-block-entries)
  (loop for (type . value) in args
        collect (ecase type
                  ((:constant) (aref literals value))
                  ((:label)
                   (find-block value all-block-entries))
                  ((:keys)
                   ;; not actually used, so whatever
                   value)
                  ((:operand) value))))

(defun compute-pka-args (args literals)
  (let ((more-args (cdr (first args)))
        (key-count-info (cdr (second args)))
        (key-literals-start (cdr (third args)))
        (key-frame-start (cdr (fourth args))))
    (list more-args key-count-info
          (loop for i from key-literals-start
                repeat (car key-count-info)
                collect (aref literals i))
          key-frame-start)))

(defun compile-bytecode (bytecode literals function-entries
                         block-entries annotations)
  (let* ((all-function-entries function-entries)
         (all-block-entries block-entries)
         (function (pop function-entries))
         (block (pop block-entries))
         (block-contexts
           (mapcar (lambda (block) (list block nil nil)) block-entries))
         (inserter (make-inserter (bt:block-entry-extra block)))
         (next-annotation-index 0)
         (*active-annotations* nil)
         (annots (find-next-annotations 0 annotations next-annotation-index))
         (optimize (compute-optimize annots))
         (bir:*policy*
           (cleavir-policy:compute-policy optimize clasp-cleavir:*clasp-env*))
         (context (make-context function block annots))
         ;; list of (end-ip . context) for exit annotations.
         (exit-contexts nil)
         ;; iblocks that are obviously unreachable
         (unreachable nil))
    (declare (ignore all-function-entries))
    (assert (zerop (bt:function-entry-start function)))
    (setf (bir:start (bt:function-entry-extra function))
          (bt:block-entry-extra block))
    (core::do-instructions (mnemonic args opip ip) (bytecode)
      ;; Update annotations.
      ;; Note that we keep tighter annotations at the front.
      (setf (values *active-annotations* next-annotation-index
                    optimize bir:*policy* exit-contexts)
            (update-annotations *active-annotations* opip annotations
                                next-annotation-index optimize bir:*policy*
                                exit-contexts context))
      ;; Compile the instruction.
      (let ((annots (find-next-annotations ip annotations
                                           next-annotation-index))
            (bir:*origin* (find-origin *active-annotations*))
            (bir:*policy*
              (cleavir-policy:compute-policy
               (compute-optimize *active-annotations*)
               clasp-cleavir:*clasp-env*))
            (args (if (eq mnemonic :parse-key-args)
                      (compute-pka-args args literals)
                      (compute-args args literals all-block-entries))))
        (apply #'compile-instruction mnemonic inserter annots context args)
        (maybe-compile-the annots inserter context)
        ;; Update current block and function if required.
        (when (and block-entries
                   (eql ip (bt:block-entry-start (first block-entries))))
          (let ((successors (bt:block-entry-successors block)))
            (when (and (= (length successors) 1)
                       ;; KLUDGE
                       (not (slot-boundp (bt:block-entry-extra block)
                                         'bir::%end)))
              (ast-to-bir:terminate inserter 'bir:jump
                                    :next (mapcar #'bt:block-entry-extra
                                                  successors)))
            (loop for successor in successors
                  for bcontext = (assoc successor block-contexts)
                  do (assign-block-context bcontext block context)))
          (setf block (pop block-entries))
          (if (and function-entries
                   (eql ip (bt:function-entry-start
                            (first function-entries))))
              (setf function (pop function-entries)
                    (bir:start (bt:function-entry-extra function))
                    (bt:block-entry-extra block)
                    context (make-context function block annots))
              (setf context
                    (let ((bcontext (third (assoc block block-contexts))))
                      (cond (bcontext (copy-context bcontext))
                            ;; after an exit
                            ((assoc ip exit-contexts)
                             (let ((ec (assoc ip exit-contexts)))
                               (when (null (bt:block-entry-predecessors block))
                                 (push (bt:block-entry-extra block)
                                       unreachable))
                               (setf exit-contexts (delete ec exit-contexts))
                               (setf (context-successors (cdr ec))
                                     (bt:block-entry-successors block))
                               (cdr ec)))
                            ;; Just a nondominated block, from tagbody.
                            (t (make-nondom-context context block))))))
          (setf (bir:dynamic-environment (bt:block-entry-extra block))
                (context-dynamic-environment context))
          (ast-to-bir:begin inserter (bt:block-entry-extra block)))))
    (mapc #'bir:delete-iblock unreachable)))

(defvar *function-entries*)

(defun bc->ir (bcfun)
  ;; Given a bytecode simple fun, return its corresponding IR function.
  ;; Used when compiling closures.
  (bt:function-entry-extra
   (find bcfun *function-entries* :key #'bt:function-entry-bcfun)))
(defun ir->bc (irfun)
  ;; Inverse of above
  (bt:function-entry-bcfun
   (find irfun *function-entries* :key #'bt:function-entry-extra)))

(defun sortedp (sequence predicate &key key)
  (if (zerop (length sequence))
      t
      (let ((previous (funcall key (elt sequence 0))))
        (every (lambda (e)
                 (let ((n (funcall key e)))
                   (prog1 (not (funcall predicate n previous))
                     (setf previous n))))
               sequence))))

(defun compile-bcfun-into-module (function irmodule)
  (check-type function core:global-bytecode-simple-fun)
  (let* ((bytecode-module
           (core:global-bytecode-simple-fun/code function))
         (bytecode (core:bytecode-module/bytecode bytecode-module))
         (literals (core:bytecode-module/literals bytecode-module))
         (functions (bytecode-module/functions bytecode-module))
         (fpos (position function functions))
         (annotations
           (core:bytecode-module/debug-info bytecode-module))
         (*closures* nil))
    (assert (sortedp annotations #'<
                     :key #'core:bytecode-debug-info/start))
    (multiple-value-bind (*function-entries* block-entries)
        (apply #'bt:compute-control-flow-table bytecode annotations
               functions)
      (loop for fun in functions
            for entry in *function-entries*
            for birfun = (make-bir-function fun irmodule)
            do (setf (bt:function-entry-extra entry) birfun))
      (loop for entry in block-entries
            for block = (make-irblock (bt:function-entry-extra
                                       (bt:block-entry-function entry))
                                      (bt:block-entry-name entry))
            do (setf (bt:block-entry-extra entry) block))
      (compile-bytecode bytecode literals *function-entries* block-entries annotations)
      (loop for entry in *function-entries*
            for irfun = (bt:function-entry-extra entry)
            do (bir:compute-iblock-flow-order irfun))
      (bt:function-entry-extra (elt *function-entries* fpos)))))

(defun compile-function (function &key (abi clasp-cleavir:*abi-x86-64*)
                                    (linkage 'llvm-sys:internal-linkage)
                                    (system clasp-cleavir:*clasp-system*)
                                    (disassemble nil))
  (let* ((module (make-instance 'bir:module))
         (bir (compile-bcfun-into-module function module))
         (cleavir-cst-to-ast:*compiler* 'cl:compile)
         ;; necessary for bir->function debug info to work. KLUDGE
         (*load-pathname* (core:function-source-pos function)))
    (bir:verify module)
    (when disassemble
      (cleavir-bir-disassembler:display module))
    (clasp-cleavir::bir-transformations module system)
    (bir:verify module)
    (clasp-cleavir::bir->function bir :abi abi :linkage linkage)))

;;; To be bound to cmp:*btb-compile-hook*.
(defun compile-hook (definition environment)
  (declare (ignore environment))
  (handler-case (compile-function definition)
    (error (e)
      (warn "BUG: Error during BTB compilation: ~a" e)
      definition)))
