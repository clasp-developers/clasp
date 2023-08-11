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

(defun bind-lambda-list-arguments (lambda-list function)
  (flet ((argument (symbol)
           (make-instance 'bir:argument
             :name symbol :function function)))
    (multiple-value-bind (required optional rest keyp keys aokp aux varestp)
        (core:process-lambda-list lambda-list 'cl:function)
      (declare (ignore aux))
      (nconc (mapcar #'argument (cdr required))
             (unless (zerop (car optional)) (list '&optional))
             (loop for (var default -p) on (cdr optional)
                   by #'cdddr
                   collect (list (argument var) (argument -p)))
             (when rest
               (list (if varestp 'core:&va-rest '&rest) (argument rest)))
             (when keyp (list '&key))
             (loop for (key var default -p) on (cdr keys)
                   by #'cddddr
                   collect (list key (argument var) (argument -p)))
             (when aokp (list '&allow-other-keys))))))

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
                     :docstring (core:function-docstring bytecode-function)
                     :original-lambda-list lambda-list
                     :origin (function-spi bytecode-function)
                     :policy nil
                     :attributes nil
                     :module module)))
    (set:nadjoinf (bir:functions module) function)
    (setf (bir:lambda-list function)
          (bind-lambda-list-arguments lambda-list function))
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

(defmacro do-instructions ((mnemonic args opip ip)
                           (bytecode &key (start 0) end)
                           &body body)
  (let ((bsym (gensym "BYTECODE"))
        (gend (gensym "END"))
        (longp (gensym "LONGP"))
        (op (gensym "OP")))
    `(loop with ,bsym = ,bytecode
           with ,ip = ,start
           with ,longp = nil
           with ,gend = ,(or end `(+ ,ip (length ,bsym)))
           for ,op = (cmpref::decode-instr (aref ,bsym ,ip))
           for ,mnemonic = (intern (string-upcase (first ,op)) "KEYWORD")
           if (eql ,mnemonic :long)
             do (setf ,longp t)
           else
             do (let ((,opip ,ip))
                  (incf ,ip)
                  (let ((,args
                          (loop for argspec
                                  in (if ,longp (fourth ,op) (third ,op))
                                for nbytes = (logandc2 argspec
                                                       cmpref::+mask-arg+)
                                collect (next-arg argspec ,bsym ,opip ,ip
                                                  nbytes)
                                do (incf ,ip nbytes))))
                    (declare (ignorable ,args ,ip))
                    ,@body
                    (setf ,longp nil)))
           until (>= ,ip ,gend))))

;;; FIXME: Should be encoded in *full-codes* probably.
(defparameter *dynenv-instructions*
  '(:push-values :append-values :pop-values :entry :catch-8 :catch-16
    :special-bind :progv
    ;; terminators as well
    ;; these are necessary in case of multiple arguments
    ;; we could track things to only do that if we need to, but
    ;; extra blocks should be pretty harmless
    :mv-call :mv-call-receive-one :mv-call-receive-fixed
    :cdentry-close :catch-close :unbind))

;;; FIXME: Also one for full-codes
(defparameter *branch-instructions*
  '(:jump-8 :jump-16 :jump-24
    :jump-if-8 :jump-if-16 :jump-if-24
    :jump-if-supplied-8 :jump-if-supplied-16))

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

(defun install-ll-vars (locals lambda-list annots)
  (multiple-value-bind (required optional rest keyp keys)
      (core:process-lambda-list lambda-list 'cl:function)
    (declare (ignore keyp))
    (let ((index 0))
      ;; The bytecode compiler sets out the indices as follows:
      ;; 1) required variables
      ;; 2) optional
      ;; 3) key
      ;; 4) optional -p
      ;; 5) rest
      ;; 6) key -p
      (flet ((newvar (name)
               (setf (aref locals index)
                     (cons (make-instance 'bir:variable
                             :name name
                             :ignore (variable-ignore name annots))
                           nil))
               (incf index)))
        (mapc #'newvar (rest required))
        (loop for (opt) on (rest optional) by #'cdddr
              do (newvar opt))
        (loop for (key kvar) on (rest keys) by #'cddddr
              do (newvar kvar))
        (loop for (_ _1 -p) on (rest optional) by #'cdddr
              when -p do (incf index)) ; -p variables are bound by SET.
        (when rest (incf index)) ; since we bind &rest vars with SET, no var needed here.
        (loop for (_ _1 _2 -p) on (rest keys) by #'cddddr
              when -p do (incf index))))))

(defun make-context (function block annots)
  (let* ((bcfun (bt:function-entry-bcfun function))
         (irfun (bt:function-entry-extra function))
         (nlocals (bcfun/locals-size bcfun))
         (locals (make-array nlocals))
         (successors (bt:block-entry-successors block)))
    (install-ll-vars locals (core:function-lambda-list bcfun) annots)
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
        when (typep annot 'core:bytecode-debug-decls)
          do (loop for (spec . rest)
                     in (core:bytecode-debug-decls/decls annot)
                   do (case spec
                        ((type)
                         (let ((nt (first rest)) (vars (rest rest)))
                           (when (member varname vars)
                             (let ((ct (cleavir-env:parse-type-specifier
                                        nt env sys)))
                               (setf ctype (ctype:conjoin sys ctype ct))))))
                        ;; FIXME: Hardcoded. Bad
                        ((dynamic-extent ftype ignorable ignore inline
                                         notinline optimize special))
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

(defun inserter-constant (value inserter)
  (let* ((iblock (ast-to-bir::iblock inserter)) ; FIXME
         (ifun (bir:function iblock))
         (module (bir:module ifun)))
    (bir:constant-in-module value module)))

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
        when (typep annot 'core:bytecode-debug-decls)
          do (loop for (spec . rest) in (core:bytecode-debug-decls/decls annot)
                   do (case spec
                        ((ignore)
                         (when (member varname rest)
                           (return-from variable-ignore 'ignore)))
                        ((ignorable)
                         (when (member varname rest)
                           (return-from variable-ignore 'ignorable))))))
  nil)

(defmethod compile-instruction ((mnemonic (eql :bind))
                                inserter annots context &rest args)
  (let* ((varannot (find-if (lambda (a) (typep a 'core:bytecode-debug-vars))
                            annots))
         (_ (assert varannot))
         (prim (core:bytecode-debug-vars/bindings varannot))
         ;; For BIND, the value for the last binding is the
         ;; most recently pushed, so we map to the bindings
         ;; in reverse order. This is a bit inefficient, though.
         (bindings (sort (copy-list prim) #'> :key #'cdr)))
    (declare (ignore _))
    (destructuring-bind (nvars base) args
      (declare (ignore base))
      (assert (= nvars (length bindings)))
      (loop with locals = (context-locals context)
            for (varname . index) in bindings
            for ignore = (variable-ignore varname annots)
            for var = (make-instance 'bir:variable
                        :ignore ignore :name varname)
            do (setf (aref locals index) (cons var nil))
               (bind-variable var (stack-pop context) inserter annots)))))

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
        ((find base bindings
               :key (lambda (x)
                      (if (consp (cdr x)) (cadr x) (cdr x))))
         (let* ((binding (rassoc base bindings))
                (name (car binding))
                (ignore (variable-ignore name annots))
                (var (make-instance 'bir:variable
                       :ignore ignore :name name))
                (value (stack-pop context))
                (cellp (consp value))
                (rvalue (if cellp (car value) value)))
           (setf (aref locals base) (cons var cellp))
           (bind-variable var rvalue inserter annots)))
        (t
         (let ((var (car varcons)))
           (check-type var bir:variable)
           (write-variable var (stack-pop context) inserter)))))))

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
    (let* ((iblock (ast-to-bir::iblock inserter))
           (ifun (bir:function iblock))
           (ll (bir:lambda-list ifun))
           (args (subseq ll 0 nreq)))
      (loop with locals = (context-locals context)
            for i from 0
            for arg in args
            for (var . cellp) = (aref locals i)
            do (bind-variable var arg inserter annots)))))

(defmethod compile-instruction ((mnemonic (eql :bind-optional-args))
                                inserter annots context &rest args)
  (destructuring-bind (start nopt) args
    (let* ((iblock (ast-to-bir::iblock inserter))
           (ifun (bir:function iblock))
           (ll (bir:lambda-list ifun))
           (args (subseq (cdr (member '&optional ll)) 0 nopt)))
      (loop with locals = (context-locals context)
            for i from start
            for (arg -p) in args
            for (var . cellp) = (aref locals i)
            ;; We use %bind rather than bind because we don't want
            ;; to assert the type of a possibly unprovided argument.
            do (%bind-variable var arg inserter)))))

;;; FIXME: Why does this instruction not put it immediately into a var
(defmethod compile-instruction ((mnemonic (eql :listify-rest-args))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (destructuring-bind (start) args
    (declare (ignore start))
    (let* ((iblock (ast-to-bir::iblock inserter))
           (ifun (bir:function iblock))
           (ll (bir:lambda-list ifun))
           (rarg (second (member '&rest ll))))
      (check-type rarg bir:argument)
      (stack-push rarg context))))
(defmethod compile-instruction ((mnemonic (eql :vaslistify-rest-args))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (destructuring-bind (start) args
    (declare (ignore start))
    (let* ((iblock (ast-to-bir::iblock inserter))
           (ifun (bir:function iblock))
           (ll (bir:lambda-list ifun))
           (rarg (second (member 'core:&va-rest ll))))
      (check-type rarg bir:argument)
      (stack-push rarg context))))

(defmethod compile-instruction ((mnemonic (eql :parse-key-args))
                                inserter annots context &rest args)
  (destructuring-bind (start key-count-info key-start frame-start) args
    (declare (ignore start key-count-info key-start))
    (let* ((iblock (ast-to-bir::iblock inserter))
           (ifun (bir:function iblock))
           (ll (bir:lambda-list ifun))
           (args (cdr (member '&key ll))))
      (loop with locals = (context-locals context)
            for i from frame-start
            for spec in args
            for (var . cellp) = (aref locals i)
            while (consp spec)
            do (destructuring-bind (key arg -p) spec
                 (declare (ignore key -p))
                 ;; %bind to avoid asserting type of unprovided arg
                 (%bind-variable var arg inserter))))))

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
      (push dest (rest (bir:next cf)))
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
  (destructuring-bind (symbol) args
    (let* ((next (mapcar #'bt:block-entry-extra
                         (context-successors context)))
           (const (inserter-constant symbol inserter))
           (value (stack-pop context))
           (bind (ast-to-bir:terminate inserter 'bir:constant-bind
                                       :inputs (list const value)
                                       :next next)))
      (setf (context-dynamic-environment context) bind))))

(defmethod compile-instruction ((mnemonic (eql :symbol-value))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (destructuring-bind (symbol) args
    (let ((const (inserter-constant symbol inserter))
          (out (make-instance 'bir:output :name symbol)))
      (ast-to-bir:insert inserter 'bir:constant-symbol-value
                         :inputs (list const) :outputs (list out))
      (stack-push out context))))

(defmethod compile-instruction ((mnemonic (eql :symbol-value-set))
                                inserter annot context &rest args)
  (declare (ignore annot))
  (destructuring-bind (symbol) args
    (let ((const (inserter-constant symbol inserter))
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
  (destructuring-bind (fname) args
    (let* ((iblock (ast-to-bir::iblock inserter))
           (ifun (bir:function iblock))
           (module (bir:module ifun))
           (const (bir:constant-in-module fname module))
           (attributes (clasp-cleavir::function-attributes fname))
           (fdef-out (make-instance 'bir:output
                       :name fname :attributes attributes)))
      (ast-to-bir:insert inserter 'bir:constant-fdefinition
                         :inputs (list const) :outputs (list fdef-out))
      (stack-push fdef-out context))))

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

(defmethod compile-instruction ((mnemonic (eql :drop-mv))
                                inserter annots context &rest args)
  (declare (ignore inserter annots))
  (assert (null args))
  (check-type (context-mv context) bir:linear-datum)
  (setf (context-mv context) nil))

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
          (find-if (lambda (a) (typep a 'core:bytecode-debug-the))
                   annots)))
    (when the
      (let ((type (core:bytecode-debug-the/type the))
            (receiving (core:bytecode-debug-the/receiving the)))
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
        when (typep annot 'core:bytecode-debug-decls)
          do (loop for (spec . rest)
                     in (core:bytecode-debug-decls/decls annot)
                   when (eq spec 'cl:optimize)
                     do (loop for optim in rest
                              unless (assoc (car optim) optimize)
                                do (push optim optimize)))
        finally (return
                  ;; If the declarations don't have enough OPTIMIZE qualities
                  ;; to cover the gamut (arguably they always should), fill
                  ;; in the rest from the global values.
                  (loop for optim in cmp:*optimize*
                        unless (assoc (car optim) optimize)
                          do (push optim optimize)
                        finally (return optimize)))))

(defun update-annotations (active-annotations ip annots index
                           optimize policy)
  (let ((recompute-optimize nil))
    ;; Remove obsolete annotations.
    (setf active-annotations
          (delete-if (lambda (annot)
                       (let ((result
                               (<= (core:bytecode-debug-info/end annot) ip)))
                         (when (and result (typep annot 'core:bytecode-debug-decls))
                           (setf recompute-optimize t))
                         result))
                     active-annotations))
    ;; Add new ones and update the index.
    (loop with len = (length annots)
          while (< index len)
          while (eql ip (core:bytecode-debug-info/start
                         (aref annots index)))
          do (let ((annot (aref annots index)))
               (incf index)
               ;; degenerate annotations happen sometimes
               ;; this could be tightened up in the bc compiler,
               ;; but checking for it is easy.
               ;; Also, THE are intentionally degenerate.
               (unless (eql ip (core:bytecode-debug-info/end annot))
                 (push annot active-annotations)
                 (when (typep annot 'core:bytecode-debug-decls)
                   (setf recompute-optimize t)))))
    ;; Recompute optimize and policy if necessary.
    (when recompute-optimize
      (setf optimize (compute-optimize active-annotations)
            policy (cleavir-policy:compute-policy optimize clasp-cleavir:*clasp-env*)))
    (values active-annotations index optimize policy)))

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
         (context (make-context function block annots)))
    (declare (ignore all-function-entries))
    (assert (zerop (bt:function-entry-start function)))
    (setf (bir:start (bt:function-entry-extra function))
          (bt:block-entry-extra block))
    (do-instructions (mnemonic args opip ip) (bytecode)
      ;; Update annotations.
      ;; Note that we keep tighter annotations at the front.
      (setf (values *active-annotations* next-annotation-index
                    optimize bir:*policy*)
            (update-annotations *active-annotations* opip annotations
                                next-annotation-index optimize bir:*policy*))
      ;; Compile the instruction.
      (let ((annots (find-next-annotations ip annotations
                                           next-annotation-index))
            (bir:*origin* (find-origin *active-annotations*))
            (bir:*policy*
              (cleavir-policy:compute-policy
               (compute-optimize *active-annotations*)
               clasp-cleavir:*clasp-env*)))
        (apply #'compile-instruction mnemonic inserter annots context
               (compute-args args literals all-block-entries))
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
          ;; When this block is unreachable,
          ;; give a sham assignment of the current context.
          ;; This ensures that in e.g. (foo (return x) y), the call to FOO can be
          ;; generated correctly before being deleted later.
          (when (null (bt:block-entry-predecessors block))
            (assign-block-context (assoc block block-contexts) nil context))
          (if (and function-entries
                   (eql ip (bt:function-entry-start
                            (first function-entries))))
              (setf function (pop function-entries)
                    (bir:start (bt:function-entry-extra function))
                    (bt:block-entry-extra block)
                    context (make-context function block annots))
              (let ((bcontext (third (assoc block block-contexts))))
                (if bcontext
                    (setf context (copy-context bcontext))
                    (setf context (make-nondom-context context block)))))
          (setf (bir:dynamic-environment (bt:block-entry-extra block))
                (context-dynamic-environment context))
          (ast-to-bir:begin inserter (bt:block-entry-extra block)))))))

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
         ;; bit circuitous to work around a bug in compile-file that
         ;; doesn't maintain identities of module bcfuns in the
         ;; debug info. FIXME
         (fpos (position (bcfun/entry function) functions
                         :key #'bcfun/entry))
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
  (compile-function definition))
