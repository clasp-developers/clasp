(defpackage #:clasp-bytecode-to-bir
  (:use #:cl)
  (:local-nicknames (#:bir #:cleavir-bir)
                    (#:set #:cleavir-set)
                    (#:ctype #:cleavir-ctype)
                    (#:env #:cleavir-env)
                    (#:policy #:cleavir-compilation-policy)
                    ;; FIXME: Move inserter stuff to its own small system
                    (#:ast-to-bir #:cleavir-ast-to-bir))
  (:export #:compile-function #:compile-hook))

(in-package #:clasp-bytecode-to-bir)

;;; To be bound to cmp:*btb-compile-hook*
(defun compile-hook (definition environment)
  (declare (ignore environment))
  (handler-case (compile-function definition)
    (error (e)
      (warn "BUG: Error during BTB compilation: ~a" e)
      definition)))

(defun bcfun-p (annotation)
  (typep annotation 'core:global-bytecode-simple-fun))

(defun bcfun/locals-size (bcfun)
  (core:global-bytecode-simple-fun/locals-frame-size bcfun))
(defun bcfun/nvars (bcfun)
  (core:global-bytecode-simple-fun/environment-size bcfun))

(defgeneric compile-instruction (mnemonic inserter context &rest args))
(defgeneric start-annotation (annotation inserter context))
(defgeneric end-annotation (annotation inserter context))

(defun compile-bcmodule (bcmodule)
  (let ((irmodule (make-instance 'bir:module)))
    (values irmodule (compile-bcmodule-into bcmodule irmodule))))

(defun compile-bcmodule-into (bcmodule irmodule)
  (let* ((literals (core:bytecode-module/literals bcmodule))
         (blockmap (make-blockmap)) (funmap (make-funmap))
         (context (make-context irmodule blockmap funmap))
         (inserter (make-instance 'ast-to-bir:inserter))
         ;; annotations starting at opip.
         (opannots (initial-annotations
                    (core:bytecode-module/debug-info bcmodule)))
         ;; all annotations currently in scope.
         (annots
           (sort (copy-list opannots) #'<
                 :key #'core:bytecode-debug-info/end)))
    ;; Compile.
    (core:do-module-instructions (mnemonic args opip ip next-annots)
        (bcmodule)
      (let ((bir:*policy* (policy context))
            (bir:*origin* (first (origin-stack context))))
        ;; End annotations coming out of effect.
        (setf annots (end-annotations opip annots inserter context))
        ;; Set up stuff for annotations coming into effect.
        (start-annotations opannots inserter context)
        ;; Start a block or function maybe.
        (maybe-begin-new opip opannots inserter context)
        ;; If this code is unreachable, we don't bother generating
        ;; anything. This makes consistency a bit easier but is a
        ;; little wacky? Maybe we want to generate IR for unreachable
        ;; code anyway so it can be deleted properly?
        (when (reachablep context)
          (let ((args (if (eq mnemonic :parse-key-args)
                          (compute-pka-args args literals)
                          (compute-args args literals))))
            (apply #'compile-instruction mnemonic inserter context args)))
        (setf annots (add-annotations annots next-annots)
              opannots next-annots)))
    ;; Compute all iblock flow orders.
    (loop for entry in (fmap funmap)
          do (bir:compute-iblock-flow-order (finfo-irfun entry)))
    ;; Delete any delayed iblocks that turned out to be unreachable
    ;; (e.g. the -after of (tagbody loop ... (go loop))).
    ;; We have to do this manually because compute-iblock-flow-order, which
    ;; normally deletes unused blocks, doesn't even know about them.
    (loop for entry in (bmap blockmap)
          do (bir:maybe-delete-iblock (binfo-irblock entry)))
    ;; Return a mapping from bcfuns to BIR functions.
    (loop for entry in (fmap funmap)
          for irfun = (finfo-irfun entry)
          collect (cons (finfo-bcfun entry) irfun))))

;;; If the instruction begins a new iblock and/or function,
;;; set everything up for that.
;;; Return value irrelevant. Mutates CONTEXT.
(defun maybe-begin-new (opip opannots inserter context)
  ;; Do we have a function start in the annotations?
  (let ((bcfun (find-if #'bcfun-p opannots)))
    (when bcfun
      (let* ((existing (find-bcfun bcfun (funmap context)))
             ;; We might have made this function earlier for ENCLOSE.
             (irfun (if existing
                        (finfo-irfun existing)
                        (let ((new
                                (make-bir-function bcfun inserter
                                                   (module context))))
                          (push (list bcfun new nil) (fmap (funmap context)))
                          new))))
        (ast-to-bir:begin inserter (bir:start irfun))
        (context-new-function context bcfun))))
  ;; Maybe we've determined earlier that this IP begins a block?
  (let ((binfo (find-block opip (blockmap context))))
    (when binfo
      ;; If we're falling through from an existing block
      ;; compile in an implicit jump.
      (when (and (reachablep context)
                 ;; KLUDGE
                 (not (slot-boundp
                       (ast-to-bir::iblock inserter)
                       'bir::%end)))
        (%compile-jump inserter context binfo))
      ;; Start new block.
      (ast-to-bir:begin inserter (binfo-irblock binfo))
      (context-new-block context (binfo-context binfo)))))

;;; Given a bytecode function, return a compiled native function.
(defun compile-function (function
                         &key (abi clasp-cleavir:*abi-x86-64*)
                           (linkage 'llvm-sys:internal-linkage)
                           (system clasp-cleavir:*clasp-system*)
                           (disassemble nil))
  (multiple-value-bind (module funmap)
      (compile-bcmodule (core:simple-fun-code function))
    (bir:verify module)
    (when disassemble
      (cleavir-bir-disassembler:display module))
    (clasp-cleavir::bir-transformations module system)
    (let ((cleavir-cst-to-ast:*compiler* 'cl:compile)
          ;; necessary for bir->function debug info to work. KLUDGE
          (*load-pathname* (core:function-source-pos function))
          (bir (cdr (assoc function funmap))))
      (clasp-cleavir::bir->function bir :abi abi :linkage linkage))))

;;; Given a bytecode function, compile it into the given IR module.
;;; that is, this does NOT finish the compilation process.
;;; the BIR:FUNCTION is returned.
;;; Used in compile-type-decl.
(defun compile-bcfun-into (function irmodule)
  (let ((fmap (compile-bcmodule-into (core:simple-fun-code function)
                                     irmodule)))
    (cdr (assoc function fmap))))

;;; Return a list of all annotations that start at IP 0.
(defun initial-annotations (annotations)
  (loop for annot across annotations
        if (= (core:bytecode-debug-info/start annot) 0)
          collect annot
        else do (loop-finish)))

(defun start-annotations (annotations inserter context)
  (loop for a in annotations
        do (start-annotation a inserter context)))

(defun add-annotations (annots next-annots)
  ;; We keep ANNOTS sorted by end IP.
  ;; FIXME? This could be done without consing, but it would be uglier.
  (let ((sna (sort (copy-list next-annots) #'<
                   :key #'core:bytecode-debug-info/end)))
    (merge 'list annots sna #'< :key #'core:bytecode-debug-info/end)))

(defun end-annotations (ip annots inserter context)
  ;; ANNOTS are kept sorted by bdi/end, so this is easy.
  (loop with result = annots
        for a in annots
        while (<= (core:bytecode-debug-info/end a) ip)
        do (end-annotation a inserter context)
           (setf result (cdr result))
        finally (return result)))

(defun make-context (module blockmap funmap)
  (make-instance 'context
    :module module :blockmap blockmap :funmap funmap
    :optimize-stack (list cmp:*optimize*) :policy cmp:*policy*))

(defun context-new-function (context bcfun)
  (setf (stack context) ()
        (locals context) (make-array (bcfun/locals-size bcfun))
        (mvals context) nil
        (reachablep context) t)
  context)

(defun context-new-block (context old-context)
  ;; Basically mutate context to be old-context.
  ;; locals are handled fine by debug-vars annotations.
  (setf (stack context) (stack old-context)
        (mvals context) (mvals old-context)
        (reachablep context) t))

(defun copy-context (context)
  (make-instance 'context
    :stack (copy-list (stack context))
    :mvals (mvals context) :module (module context)
    :blockmap (blockmap context) :funmap (funmap context)
    :reachablep (reachablep context)))

(defun compute-args (args literals)
  (loop for (type . value) in args
        collect (ecase type
                  ((:constant) (aref literals value))
                  ((:label) value)
                  ((:keys)
                   ;; not actually used, so whatever
                   value)
                  ((:operand) value))))

;;; Mapping from bytecode functions to IR functions.
(defclass funmap ()
  (;; Alist.
   (%map :initform nil :accessor fmap)))

(defun make-funmap () (make-instance 'funmap))

(defun find-bcfun (bcfun funmap)
  (find bcfun (fmap funmap) :key #'finfo-bcfun))
(defun find-irfun (irfun funmap)
  (find irfun (fmap funmap) :key #'finfo-irfun))

(defun finfo-bcfun (finfo) (first finfo))
(defun finfo-irfun (finfo) (second finfo))
(defun finfo-closure (finfo) (third finfo))
(defun (setf finfo-closure) (new finfo) (setf (third finfo) new))

(defun add-function (context bcfun irfun closure)
  (push (list bcfun irfun closure) (fmap (funmap context))))

;;; Mapping from IPs to IR blocks. Used throughout compilation of
;;; a bytecode module due to nonlocal exits.
(defclass blockmap ()
  (;; List of (ip irblock context).
   ;; the context is the context that should be used when compilation
   ;; reaches this point.
   (%map :initform nil :accessor bmap)))

(defun make-blockmap () (make-instance 'blockmap))

(defun find-block (ip irblocks)
  (assoc ip (bmap irblocks)))

(defun binfo-ip (binfo) (first binfo))
(defun binfo-irblock (binfo) (second binfo))
(defun binfo-context (binfo) (third binfo))
(defun binfo-receiving (binfo) (fourth binfo))

(defun %add-block (context ip iblock &optional (receiving 0))
  (let ((ncontext (copy-context context)))
    ;; Fix up the context with block inputs
    (if (= receiving -1)
        (setf (mvals ncontext) (first (bir:inputs iblock)))
        (loop with inputs = (bir:inputs iblock)
                initially (assert (= (length inputs) receiving))
              for i in (bir:inputs iblock)
              do (stack-push i ncontext)))
    ;; Record info
    (push (list ip iblock ncontext receiving)
          (bmap (blockmap context))))
  iblock)

;;; Set up and return a new iblock for a later IP.
;;; If an iblock has already been set up (e.g. from nested IF), check
;;; for consistency but return the existing block.
;;; NOTE: Alternately we could create a block anyway that just jumps to the
;;; next. This would preserve block names. But who cares about those?
(defun delay-block (inserter context ip
                    &key name (receiving 0)
                      (dynamic-environment
                       (ast-to-bir::dynamic-environment inserter)))
  (let ((einfo (find-block ip (blockmap context))))
    (cond (einfo
           (assert (= receiving (binfo-receiving einfo)))
           (binfo-irblock einfo))
          (t
           (%add-block context ip
                       (make-iblock-r inserter name
                                      :receiving receiving
                                      :dynamic-environment dynamic-environment)
                       receiving)))))

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

(defun make-bir-function (bytecode-function inserter
                          &optional (module (inserter-module inserter)))
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
                     :module module))
         (start (make-start-block inserter function bytecode-function)))
    (setf (bir:start function) start)
    (set:nadjoinf (bir:functions module) function)
    function))

(defun function-spi (function)
  (multiple-value-bind (path filepos lineno column)
      (core:function-source-pos function)
    (if path
        (core:make-source-pos-info :filename (namestring path)
                                   :filepos filepos
                                   :lineno lineno :column column)
        nil)))

(defclass context ()
  ((%stack :initform nil :initarg :stack :accessor stack)
   (%locals :initarg :locals :accessor locals)
   (%mvals :initform nil :initarg :mvals :accessor mvals)
   (%reachablep :initform t :initarg :reachablep :accessor reachablep)
   ;; A stack of normalized optimize specifications.
   ;; entering a bytecode-ast-decls pushes one, exiting pops.
   (%optimize-stack :initarg :optimize-stack :accessor optimize-stack :type list)
   (%policy :initarg :policy :accessor policy)
   (%typemap-stack :initform nil :initarg :typemap-stack
                   :accessor typemap-stack :type list)
   (%origin-stack :initform nil :accessor origin-stack :type list)
   (%module :initarg :module :reader module :type bir:module)
   (%funmap :initarg :funmap :reader funmap :type funmap)
   (%blockmap :initarg :blockmap :reader blockmap :type blockmap)))

(defun stack-push (datum context)
  (push datum (stack context)))
(defun stack-pop (context)
  (assert (stack context))
  (pop (stack context)))

(defun make-start-block (inserter irfun bcfun)
  (ast-to-bir::make-iblock
   inserter
   :name (symbolicate (write-to-string (core:function-name bcfun))
                      '#:-start)
   :function irfun :dynamic-environment irfun))

(defun symbolicate (&rest components)
  ;; FIXME: Probably just use concatenate
  (let* ((strings (mapcar #'string components))
         (length (reduce #'+ strings :key #'length))
         (name (make-array length :element-type 'character)))
    (let ((index 0))
      (dolist (string strings (make-symbol name))
        (replace name string :start1 index)
        (incf index (length string))))))

(defmethod compile-instruction ((mnemonic (eql :ref))
                                inserter context &rest args)
  (destructuring-bind (varindex) args
    (let* ((varinfo (aref (locals context) varindex))
           (var (car varinfo))
           (cellp (cdr varinfo)))
      (stack-push
       (etypecase var
         (bir:variable
          (if cellp varinfo (read-variable var inserter context)))
         (bir:come-from var)
         (bir:argument var)) ; happens from e.g. encelled parameters.)
       context))))

(defun read-variable (variable inserter context)
  (let ((ctype (declared-ctype (bir:name variable) context)))
    (compile-type-decl :variable ctype
                       (%read-variable variable inserter)
                       inserter context)))

(defun declared-ctype (name context)
  (loop for map in (typemap-stack context)
        for pair = (assoc name map)
        when pair return (cdr pair)
        finally (return (ctype:top clasp-cleavir:*clasp-system*))))

(defun %read-variable (variable inserter)
  (bir:record-variable-ref variable)
  (let ((readvar-out (make-instance 'bir:output :name (bir:name variable))))
    (ast-to-bir:insert inserter 'bir:readvar
                       :inputs (list variable) :outputs (list readvar-out))
    readvar-out))

(defmethod compile-instruction ((mnemonic (eql :const))
                                inserter context &rest args)
  (destructuring-bind (value) args
    (stack-push (compile-constant value inserter) context)))

(defun compile-constant (value inserter)
  (let* ((const (inserter-constant value inserter))
         (cref-out (make-instance 'bir:output)))
    (ast-to-bir:insert inserter 'bir:constant-reference
                       :inputs (list const) :outputs (list cref-out))
    cref-out))

(defmethod compile-instruction ((mnemonic (eql :closure)) inserter
                                context &rest args)
  (destructuring-bind (index) args
    (let* ((ifun (inserter-function inserter))
           (lexes (finfo-closure (find-irfun ifun (funmap context))))
           (lex (elt lexes index)))
      (stack-push (etypecase lex
                    ((cons bir:variable (eql t)) lex) ; cell
                    ((cons bir:variable (eql nil))
                     (read-variable (car lex) inserter context))
                    (bir:come-from lex))
                  context))))

(defmethod compile-instruction ((mnemonic (eql :call)) inserter
                                context &rest args)
  (destructuring-bind (nargs) args
    (let ((args (gather context nargs))
          (callee (stack-pop context))
          (out (make-instance 'bir:output)))
      (ast-to-bir:insert inserter 'bir:call
                         :inputs (list* callee args)
                         :outputs (list out))
      (setf (mvals context) out))))

(defmethod compile-instruction ((mnemonic (eql :call-receive-one))
                                inserter context &rest args)
  (destructuring-bind (nargs) args
    (let ((args (gather context nargs))
          (callee (stack-pop context))
          (out (make-instance 'bir:output)))
      (ast-to-bir:insert inserter 'bir:call
                         :inputs (list* callee args)
                         :outputs (list out))
      (setf (mvals context) nil) ; invalidate for self-consistency checks
      (stack-push out context))))

(defmethod compile-instruction ((mnemonic (eql :call-receive-fixed))
                                inserter context &rest args)
  (destructuring-bind (nargs nvals) args
    (assert (zerop nvals)) ; FIXME
    (let ((args (gather context nargs))
          (callee (stack-pop context))
          (out (make-instance 'bir:output)))
      (setf (mvals context) nil) ; invalidate for self-consistency checks
      (ast-to-bir:insert inserter 'bir:call
                         :inputs (list* callee args)
                         :outputs (list out)))))

(defun gather (context n)
  (loop with args = nil
        repeat n
        do (push (stack-pop context) args)
        finally (return args)))

(defmethod compile-instruction ((mnemonic (eql :bind))
                                inserter context &rest args)
  (destructuring-bind (nvars base) args
    (loop with locals = (locals context)
          for i from (+ base nvars -1) downto base
          for value = (stack-pop context)
          do (set-local locals i value inserter context))))

(defun set-local (locals index value inserter context)
  (let ((local (aref locals index)))
    (if (null local)
        (setf (aref locals index) (cons value nil)) ; FIXME cell
        (let ((var (car local)))
          (if (typep var 'bir:variable)
              (write-variable var value inserter context)
              ;; happens when bindings are closed over.
              (setf (aref locals index)
                    (etypecase value
                      (bir:linear-datum (cons value nil))
                      ((cons bir:linear-datum)
                       (cons (car value) t)))))))))

(defmethod compile-instruction ((mnemonic (eql :set))
                                inserter context &rest args)
  (destructuring-bind (index) args
    (set-local (locals context) index (stack-pop context) inserter context)))

(defun %write-variable (variable value inserter)
  (assert (slot-boundp variable 'bir::%binder))
  (bir:record-variable-set variable)
  (ast-to-bir:insert inserter 'bir:writevar
                     :inputs (list value) :outputs (list variable)))

(defun write-variable (variable value inserter context)
  (let ((ctype (declared-ctype (bir:name variable) context)))
    (%write-variable variable
                     (compile-type-decl :setq ctype value inserter context)
                     inserter)))

(defmethod compile-instruction ((mnemonic (eql :make-cell))
                                inserter context &rest args)
  (declare (ignore inserter))
  (destructuring-bind () args
    (let ((top (stack-pop context)))
      (check-type top bir:linear-datum)
      ;; Mark as a cell.
      (stack-push (list top) context))))

(defmethod compile-instruction ((mnemonic (eql :cell-ref))
                                inserter context &rest args)
  (destructuring-bind () args
    (let ((cell (stack-pop context)))
      ;; Make sure it's a bound cell
      ;; (not a linear datum directly from make-cell; that's illegal bytecode)
      (check-type cell (cons bir:variable))
      (stack-push (read-variable (car cell) inserter context) context))))

(defmethod compile-instruction ((mnemonic (eql :cell-set))
                                inserter context &rest args)
  (destructuring-bind () args
    (let ((cell (stack-pop context)) (val (stack-pop context)))
      (check-type cell (cons bir:variable))
      (check-type val bir:linear-datum)
      (write-variable (car cell) val inserter context))))

(defmethod compile-instruction ((mnemonic (eql :make-closure))
                                inserter context &rest args)
  (destructuring-bind (template) args
    (let* ((irfun (make-bir-function template inserter))
           (enclose-out (make-instance 'bir:output
                          :name (core:function-name template)))
           (enclose
             (ast-to-bir:insert inserter 'bir:enclose
                                :code irfun :outputs (list enclose-out)))
           (nclosed (bcfun/nvars template))
           (closed (nreverse (subseq (stack context) 0 nclosed)))
           (real-closed (mapcar #'resolve-closed closed)))
      (assert (every (lambda (v) (typep v '(or bir:come-from
                                            (cons bir:variable))))
                     real-closed))
      (add-function context template irfun real-closed)
      (setf (stack context) (nthcdr nclosed (stack context))
            (bir:enclose irfun) enclose)
      (stack-push enclose-out context))))

(defmethod compile-instruction ((mnemonic (eql :make-uninitialized-closure))
                                inserter context &rest args)
  ;; Set up an ir function for the funmap and generate an enclose,
  ;; but leave the closure for initialize-closure.
  (destructuring-bind (template) args
    (let* ((irfun (make-bir-function template inserter))
           (enclose-out (make-instance 'bir:output
                          :name (core:function-name template)))
           (enclose
             (ast-to-bir:insert inserter 'bir:enclose
                                :code irfun :outputs (list enclose-out))))
      (add-function context template irfun nil)
      (setf (bir:enclose irfun) enclose)
      (stack-push enclose-out context))))

(defmethod compile-instruction ((mnemonic (eql :initialize-closure))
                                inserter context &rest args)
  ;; The function has already been put in the funmap and enclosed,
  ;; so just set up its closure before it's generated.
  (declare (ignore inserter))
  (destructuring-bind (index) args
    (destructuring-bind (var . cellp) (aref (locals context) index)
      (check-type var bir:variable) (assert (not cellp))
      (let* ((enclose (bir:definition (bir:input (bir:binder var))))
             (finfo (find-irfun (bir:code enclose) (funmap context)))
             (template (finfo-bcfun finfo))
             (nclosed (bcfun/nvars template))
             (closed (nreverse (subseq (stack context) 0 nclosed)))
             (real-closed (mapcar #'resolve-closed closed)))
        (setf (stack context) (nthcdr nclosed (stack context))
              (finfo-closure finfo) real-closed)))))

(defun resolve-closed (v)
  ;; Each thing on the stack is either a come-from, a list
  ;; (variable . t) if there's a cell, or the output of a readvar for
  ;; variables with no cell.
  (etypecase v
    (bir:come-from v)
    (bir:output (cons (variable-from-output v) nil))
    ((cons bir:variable (eql t)) v)))

(defun variable-from-output (output)
  (let ((def (bir:definition output)))
    (etypecase def
      (bir:readvar (bir:input def))
      (bir:thei
       (let ((tdef (bir:definition (bir:input def))))
         (check-type tdef bir:readvar)
         (bir:input tdef))))))

(defmethod compile-instruction ((mnemonic (eql :return))
                                inserter context &rest args)
  (destructuring-bind () args
    ;; KLUDGE
    (unless (slot-boundp (ast-to-bir::iblock inserter) 'bir::%end)
      (let ((input (mvals context)))
        (check-type input bir:linear-datum)
        (let ((returni (make-instance 'bir:returni :inputs (list input))))
          (setf (bir:returni (bir:function (ast-to-bir::iblock inserter))) returni)
          (ast-to-bir:terminate inserter returni))))))

(defmethod compile-instruction ((mnemonic (eql :bind-required-args))
                                inserter context &rest args)
  (destructuring-bind (nreq) args
    (loop with ifun = (inserter-function inserter)
          with locals = (locals context)
          for i from 0 below nreq
          for arg = (make-instance 'bir:argument :function ifun)
          do (setf (aref locals i) (cons arg nil))
          collect arg into args
          finally (setf (bir:lambda-list ifun) args))))

(defmethod compile-instruction ((mnemonic (eql :bind-optional-args))
                                inserter context &rest args)
  (destructuring-bind (start nopt) args
    (let* ((ifun (inserter-function inserter))
           (ll (bir:lambda-list ifun)))
      (loop with locals = (locals context)
            repeat nopt
            for i from start
            for arg = (make-instance 'bir:argument :function ifun)
            for -p = (make-instance 'bir:argument :function ifun)
            do (setf (aref locals i) (cons arg nil))
            collect (list arg -p) into ll-app
            finally (setf (bir:lambda-list ifun)
                          (append ll '(&optional) ll-app))))))

(defmethod compile-instruction ((mnemonic (eql :listify-rest-args))
                                inserter context &rest args)
  (destructuring-bind (start) args
    (declare (ignore start))
    (let* ((ifun (inserter-function inserter))
           (ll (bir:lambda-list ifun))
           (rarg (make-instance 'bir:argument :function ifun)))
      (setf (bir:lambda-list ifun) (append ll `(&rest ,rarg)))
      (stack-push rarg context))))
(defmethod compile-instruction ((mnemonic (eql :vaslistify-rest-args))
                                inserter context &rest args)
  (destructuring-bind (start) args
    (declare (ignore start))
    (let* ((ifun (inserter-function inserter))
           (ll (bir:lambda-list ifun))
           (rarg (make-instance 'bir:argument :function ifun)))
      (setf (bir:lambda-list ifun) (append ll `(core:&va-rest ,rarg)))
      (stack-push rarg context))))

(defmethod compile-instruction ((mnemonic (eql :parse-key-args))
                                inserter context &rest args)
  (destructuring-bind (start (key-count . aokp) keys frame-start) args
    (declare (ignore start key-count))
    (let* ((ifun (inserter-function inserter))
           (ll (bir:lambda-list ifun)))
      (loop with locals = (locals context)
            for i from frame-start
            for key in keys
            for arg = (make-instance 'bir:argument :function ifun)
            for -p = (make-instance 'bir:argument :function ifun)
            do (setf (aref locals i) (cons arg nil))
            collect (list key arg -p) into ll-app
            finally (setf (bir:lambda-list ifun)
                          (append ll '(&key) ll-app
                                  (if aokp '(&allow-other-keys) ())))))))

(defun compile-jump (inserter context destination)
  ;; The destination must have already been put in.
  (let ((binfo (find-block destination (blockmap context))))
    (assert binfo)
    (%compile-jump inserter context binfo))
  (setf (reachablep context) nil))

(defun %compile-jump (inserter context binfo)
  (let* ((irblock (binfo-irblock binfo))
         (receiving (binfo-receiving binfo))
         (inputs (if (eql receiving -1)
                     (list (mvals context))
                     (loop with result
                           repeat receiving
                           do (push (stack-pop context) result)
                           finally (return result))))
         (outputs (copy-list (bir:inputs irblock))))
    (assert (loop for i in inputs always (typep i 'bir:linear-datum)))
    (ast-to-bir:terminate inserter 'bir:jump
                          :next (list irblock)
                          :inputs inputs :outputs outputs)))

(defmethod compile-instruction ((mnemonic (eql :jump-8))
                                inserter context &rest args)
  (apply #'compile-jump inserter context args))
(defmethod compile-instruction ((mnemonic (eql :jump-16))
                                inserter context &rest args)
  (apply #'compile-jump inserter context args))
(defmethod compile-instruction ((mnemonic (eql :jump-24))
                                inserter context &rest args)
  (apply #'compile-jump inserter context args))

(defun compile-jump-if (inserter context destination)
  (let* ((condition (stack-pop context))
         (then-dest (delay-block inserter context destination
                                 :name '#:if-then))
         (else-dest (ast-to-bir::make-iblock
                     inserter :name '#:if-else)))
    (ast-to-bir:terminate inserter 'bir:ifi
                          :inputs (list condition)
                          :next (list then-dest else-dest))
    ;; The else block we just start here.
    (ast-to-bir:begin inserter else-dest)))

(defmethod compile-instruction ((mnemonic (eql :jump-if-8))
                                inserter context &rest args)
  (apply #'compile-jump-if inserter context args))
(defmethod compile-instruction ((mnemonic (eql :jump-if-16))
                                inserter context &rest args)
  (apply #'compile-jump-if inserter context args))
(defmethod compile-instruction ((mnemonic (eql :jump-if-24))
                                inserter context &rest args)
  (apply #'compile-jump-if inserter context args))

(defun compile-jump-if-supplied (inserter context index true-dest)
  (destructuring-bind (arg . cellp)
      (aref (locals context) index)
    (assert (null cellp))
    (check-type arg bir:argument) ; from bind-optional-args
    ;; Now find the corresponding -p argument and branch on it.
    (let* ((ifun (inserter-function inserter))
           (-p (loop for e in (bir:lambda-list ifun)
                     when (consp e)
                       do (ecase (length e)
                            (2 (when (eq (first e) arg)
                                 (return (second e))))
                            (3 (when (eq (second e) arg)
                                 (return (third e)))))))
           (thenb (delay-block inserter context true-dest
                               :name '#:if-supplied))
           (elseb (ast-to-bir::make-iblock inserter :name '#:if-unsupplied)))
      (ast-to-bir:terminate inserter 'bir:ifi
                            :inputs (list -p)
                            :next (list thenb elseb))
      (ast-to-bir:begin inserter elseb))))

(defmethod compile-instruction ((mnemonic (eql :jump-if-supplied-8))
                                inserter context &rest args)
  (apply #'compile-jump-if-supplied inserter context args))
(defmethod compile-instruction ((mnemonic (eql :jump-if-supplied-16))
                                inserter context &rest args)
  (apply #'compile-jump-if-supplied inserter context args))

(defmethod compile-instruction ((mnemonic (eql :check-arg-count-le))
                                inserter context &rest args)
  (declare (ignore inserter context args)))
(defmethod compile-instruction ((mnemonic (eql :check-arg-count-ge))
                                inserter context &rest args)
  (declare (ignore inserter context args)))
(defmethod compile-instruction ((mnemonic (eql :check-arg-count-eq))
                                inserter context &rest args)
  (declare (ignore inserter context args)))

(defmethod compile-instruction ((mnemonic (eql :push-values))
                                inserter context &rest args)
  (destructuring-bind () args
    (let* ((mv (mvals context))
           (during (ast-to-bir::make-iblock inserter :name '#:save-values))
           (save-out (make-instance 'bir:output :name '#:saved-values))
           (save (ast-to-bir::terminate inserter 'bir:values-save
                                        :inputs (list mv)
                                        :outputs (list save-out)
                                        :next (list during))))
      (setf (bir:dynamic-environment during) save
            (mvals context) nil)
      (stack-push (list :multiple-values save-out) context)
      (ast-to-bir::begin inserter during))))

(defmethod compile-instruction ((mnemonic (eql :append-values))
                                inserter context &rest args)
  (destructuring-bind () args
    (let* ((mv (mvals context))
           (during (ast-to-bir::make-iblock inserter :name '#:save-values))
           (save-out (make-instance 'bir:output :name '#:saved-values))
           (save (ast-to-bir::terminate inserter 'bir:values-save
                                        :inputs (list mv)
                                        :outputs (list save-out)
                                        :next (list during)))
           (previous (stack-pop context)))
      (check-type previous (cons (eql :multiple-values)))
      (setf (bir:dynamic-environment during) save (mvals context) nil)
      (stack-push (list* :multiple-values save-out (cdr previous)) context)
      (ast-to-bir::begin inserter during))))

(defmethod compile-instruction ((mnemonic (eql :pop-values))
                                inserter context &rest args)
  (destructuring-bind () args
    (let ((previous (stack-pop context)))
      (check-type previous (cons (eql :multiple-values)
                                 (cons bir:linear-datum null)))
      (let* ((mv (second previous))
             (save (bir:definition mv))
             (read-out (make-instance 'bir:output
                         :name '#:restored-values))
             (old-de (bir:dynamic-environment save))
             (after (ast-to-bir::make-iblock inserter
                                             :name '#:mv-prog1-after
                                             :dynamic-environment old-de)))
        (check-type save bir:values-save)
        (setf (mvals context) read-out)
        (ast-to-bir:insert inserter 'bir:values-restore
                           :inputs (list mv) :outputs (list read-out))
        (ast-to-bir:terminate inserter 'bir:jump
                              :inputs () :outputs () :next (list after))
        (ast-to-bir::begin inserter after)))))

(defmethod compile-instruction ((mnemonic (eql :mv-call))
                                inserter context &rest args)
  (destructuring-bind () args
    (setf (mvals context) (compile-mv-call inserter context))))
(defmethod compile-instruction ((mnemonic (eql :mv-call-receive-one))
                                inserter context &rest args)
  (destructuring-bind () args
    (stack-push (compile-mv-call inserter context) context)
    (setf (mvals context) nil)))
(defmethod compile-instruction ((mnemonic (eql :mv-call-receive-fixed))
                                inserter context &rest args)
  (destructuring-bind (nvals) args
    (assert (zerop nvals)) ; FIXME
    (compile-mv-call inserter context)
    (setf (mvals context) nil)))

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
           (old-de (bir:dynamic-environment firstdef))
           (after (ast-to-bir::make-iblock inserter :name '#:mv-call-after
                                           :dynamic-environment old-de)))
      (check-type lastdef bir:values-save)
      ;; Morph the most recent values-save into a -collect
      ;; so that we have a proper mv call
      (change-class lastdef 'bir:values-collect
                    :inputs (append (butlast args) (bir:inputs lastdef)))
      ;; Generate the actual call
      (ast-to-bir:insert inserter 'bir:mv-call
                         :inputs (list callee mv) :outputs (list out))
      (ast-to-bir:terminate inserter 'bir:jump
                            :inputs () :outputs () :next (list after))
      (ast-to-bir:begin inserter after))
    out))

(defmethod compile-instruction ((mnemonic (eql :save-sp))
                                inserter context &rest args)
  (declare (ignore inserter))
  (destructuring-bind (index) args
    (setf (aref (locals context) index) (cons (stack context) nil))))

(defmethod compile-instruction ((mnemonic (eql :restore-sp))
                                inserter context &rest args)
  (declare (ignore inserter))
  (destructuring-bind (index) args
    (destructuring-bind (stack . cellp) (aref (locals context) index)
      (assert (not cellp)) (assert (listp stack))
      (setf (stack context) stack))))

(defmethod compile-instruction ((mnemonic (eql :entry))
                                inserter context &rest args)
  (destructuring-bind (index) args
    (let* ((during (ast-to-bir::make-iblock inserter :name '#:block))
           (cf (ast-to-bir:terminate inserter 'bir:come-from
                                     :next (list during)))
           (function (inserter-function inserter)))
      (set:nadjoinf (bir:come-froms function) cf)
      (setf (bir:dynamic-environment during) cf
            (aref (locals context) index) (cons cf nil))
      (ast-to-bir:begin inserter during))))

(defmethod compile-instruction ((mnemonic (eql :exit-8))
                                inserter context &rest args)
  (destructuring-bind (destination) args
    (compile-exit inserter context destination)))
(defmethod compile-instruction ((mnemonic (eql :exit-16))
                                inserter context &rest args)
  (destructuring-bind (destination) args
    (compile-exit inserter context destination)))
(defmethod compile-instruction ((mnemonic (eql :exit-24))
                                inserter context &rest args)
  (destructuring-bind (destination) args
    (compile-exit inserter context destination)))

(defun compile-exit (inserter context destination)
  (let* ((dinfo (find-block destination (blockmap context)))
         (dest (binfo-irblock dinfo))
         (receiving (binfo-receiving dinfo))
         (cf (stack-pop context))
         (inputs (if (= receiving -1)
                     (prog1 (list (mvals context)) (setf (mvals context) nil))
                     (loop repeat receiving collect (stack-pop context)))))
    (check-type cf bir:come-from)
    (let ((uw (ast-to-bir:terminate inserter 'bir:unwind
                                    :inputs inputs
                                    :outputs (copy-list (bir:inputs dest))
                                    :come-from cf
                                    :destination dest)))
      ;; Don't add duplicate NEXT entries
      ;; (obscure NLX uses can hit this, like CORE::PACKAGES-ITERATOR)
      (unless (eql dest (first (bir:next cf)))
        (pushnew dest (rest (bir:next cf)))
        (set:nadjoinf (bir:predecessors dest) (bir:iblock cf)))
      (set:nadjoinf (bir:unwinds cf) uw)
      (set:nadjoinf (bir:entrances dest) (ast-to-bir::iblock inserter))))
  (setf (reachablep context) nil))

;;; FIXME: The iblocks generated here are often kind of pointless -
;;; i.e. only reachable by unwinding and then all they do is unwind more.
;;; Cleavir could maybe optimize such blocks away.
(defmethod compile-instruction ((mnemonic (eql :entry-close))
                                inserter context &rest args)
  (declare (ignore context))
  (destructuring-bind () args
    (let* ((de (bir:parent (ast-to-bir::dynamic-environment inserter)))
           (ib (ast-to-bir::make-iblock
                inserter :name '#:entry-close :dynamic-environment de)))
      (ast-to-bir:terminate inserter 'bir:jump
                            :inputs () :outputs () :next (list ib))
      (ast-to-bir:begin inserter ib))))

(defmethod compile-instruction ((mnemonic (eql :special-bind))
                                inserter context &rest args)
  (destructuring-bind (vcell) args
    (let* ((vname (core:variable-cell/name vcell))
           (bname (symbolicate '#:bind- vname))
           (next (ast-to-bir::make-iblock inserter :name bname))
           (const (inserter-vcell vname inserter))
           (value (stack-pop context))
           (bind (ast-to-bir::terminate inserter 'bir:constant-bind
                                        :inputs (list const value)
                                        :next (list next))))
      (setf (bir:dynamic-environment next) bind)
      (ast-to-bir:begin inserter next))))

(defmethod compile-instruction ((mnemonic (eql :symbol-value))
                                inserter context &rest args)
  (destructuring-bind (vcell) args
    (let* ((vname (core:variable-cell/name vcell))
           (const (inserter-vcell vname inserter))
           (out (make-instance 'bir:output :name vname)))
      (ast-to-bir:insert inserter 'bir:constant-symbol-value
                         :inputs (list const) :outputs (list out))
      (stack-push out context))))

(defmethod compile-instruction ((mnemonic (eql :symbol-value-set))
                                inserter context &rest args)
  (destructuring-bind (vcell) args
    (let ((const (inserter-vcell (core:variable-cell/name vcell) inserter))
          (in (stack-pop context)))
      (ast-to-bir:insert inserter 'bir:set-constant-symbol-value
                         :inputs (list const in)))))

(defmethod compile-instruction ((mnemonic (eql :unbind))
                                inserter context &rest args)
  (declare (ignore context))
  (destructuring-bind () args
    (let* ((bind (ast-to-bir::dynamic-environment inserter))
           (vname (bir:variable-name (first (bir:inputs bind))))
           (ib (ast-to-bir::make-iblock
                inserter :name (symbolicate '#:unbind- vname)
                :dynamic-environment (bir:parent bind))))
      (ast-to-bir:terminate inserter 'bir:jump
                            :inputs () :outputs ()
                            :next (list ib))
      (ast-to-bir:begin inserter ib))))

(defmethod compile-instruction ((mnemonic (eql :fdefinition))
                                inserter context &rest args)
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
                                inserter context &rest args)
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
                                inserter context &rest args)
  ;; Just call CORE:COERCE-CALLED-FDESIGNATOR.
  (destructuring-bind () args
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
      (stack-push out context))))

(defun inserter-module (inserter)
  ;; FIXME: Export a-t-b:iblock or something.
  (bir:module (bir:function (ast-to-bir::iblock inserter))))

(defun inserter-function (inserter)
  (bir:function (ast-to-bir::iblock inserter)))

(defun inserter-constant (value inserter)
  (bir:constant-in-module value (inserter-module inserter)))
(defun inserter-vcell (symbol inserter)
  (bir:variable-cell-in-module symbol (inserter-module inserter)))
(defun inserter-fcell (fname inserter)
  (bir:function-cell-in-module fname (inserter-module inserter)))

(defmethod compile-instruction ((mnemonic (eql :nil))
                                inserter context &rest args)
  (destructuring-bind () args
    (stack-push (compile-constant 'nil inserter) context)))

(defmethod compile-instruction ((mnemonic (eql :push))
                                inserter context &rest args)
  (declare (ignore inserter))
  (destructuring-bind () args
    (let ((mv (mvals context)))
      (setf (mvals context) nil)
      (stack-push mv context))))

(defmethod compile-instruction ((mnemonic (eql :pop))
                                inserter context &rest args)
  (declare (ignore inserter))
  (destructuring-bind () args
    (let ((mv (stack-pop context)))
      (check-type mv bir:linear-datum)
      (setf (mvals context) mv))))

(defmethod compile-instruction ((mnemonic (eql :dup))
                                inserter context &rest args)
  (destructuring-bind () args
    (let ((var (make-instance 'bir:variable :ignore nil)))
      (%bind-variable var (stack-pop context) inserter)
      (stack-push (%read-variable var inserter) context)
      (stack-push (%read-variable var inserter) context))))

(defmethod start-annotation ((annotation core:bytecode-debug-vars)
                             inserter context)
  (when (degenerate-annotation-p annotation)
    (return-from start-annotation))
  (loop with bir:*policy* = (policy context)
        for bdv in (core:bytecode-debug-vars/bindings annotation)
        for name = (core:bytecode-debug-var/name bdv)
        for cellp = (core:bytecode-debug-var/cellp bdv)
        for index = (core:bytecode-debug-var/frame-index bdv)
        for ctype = (declared-variable-ctype
                     (core:bytecode-debug-var/decls bdv) (consp name))
        for (datum) = (aref (locals context) index)
        ;; We make all variables IGNORABLE because the bytecode compiler
        ;; has already warned about any semantically unused variables
        ;; (and variables declared IGNORE but then used).
        ;; Also, some uses in the original source are not preserved by the
        ;; bytecode compiler, e.g. (progn x nil). So doing ignore stuff
        ;; here results in spurious warnings.
        for variable = (make-instance 'bir:variable
                         :ignore 'cl:ignorable :name name)
        do (etypecase datum
             (bir:linear-datum
              (bind-variable variable datum ctype inserter context))
             ((cons bir:linear-datum) ; cell
              (bind-variable variable (car datum) ctype inserter context)))
           (setf (aref (locals context) index)
                 (cons variable cellp))
        collect (cons name ctype) into typemap
        finally (push typemap (typemap-stack context))))

(defun degenerate-annotation-p (annotation)
  ;; These can arise naturally from code like
  ;; (progn (let ((y x)) y) more-code)
  ;; or just from THE or something. But for bytecode-debug-vars they pose
  ;; an issue, as we would end the annotation before it begins. So we skip 'em.
  (= (core:bytecode-debug-info/start annotation)
     (core:bytecode-debug-info/end annotation)))

(defun declared-variable-ctype (decls functionp)
  ;; FIXME: Function types will take a little thought, since we want to
  ;; declare types of arguments/return values rather than of the function itself.
  (when functionp (return-from declared-variable-ctype t))
  (loop with env = clasp-cleavir:*clasp-env*
        with sys = clasp-cleavir:*clasp-system*
        for decl in decls
        ;; just take the first type decl - FIXME?
        when (and (consp decl) (eq (first decl) 'cl:type))
          return (env:parse-type-specifier (second decl) env sys)
        finally (return (ctype:top sys))))

(defun bind-variable (variable value ctype inserter context)
  (let ((typed (compile-type-decl :setq ctype value inserter context)))
    (%bind-variable variable typed inserter)))

(defun %bind-variable (variable value inserter)
  (let ((binder (ast-to-bir:insert inserter 'bir:leti
                                   :inputs (list value)
                                   :outputs (list variable))))
    (set:nadjoinf (bir:variables (bir:function binder)) variable)
    (setf (bir:binder variable) binder)))

(defmethod end-annotation ((annot core:bytecode-debug-vars)
                           inserter context)
  (declare (ignore inserter))
  (when (degenerate-annotation-p annot)
    (return-from end-annotation))
  ;; End the extent of all variables.
  (loop for bdv in (core:bytecode-debug-vars/bindings annot)
        for index = (core:bytecode-debug-var/frame-index bdv)
        do (setf (aref (locals context) index) nil))
  ;; And type declarations.
  (pop (typemap-stack context)))

(defmethod start-annotation ((annot core:bytecode-ast-if)
                             inserter context)
  ;; Record the merge block for later jumps.
  (delay-block inserter context (core:bytecode-debug-info/end annot)
               :name '#:if-merge
               :receiving (core:bytecode-ast-if/receiving annot)))

(defmethod start-annotation ((annot core:bytecode-ast-tagbody)
                             inserter context)
  (loop for (name . ip) in (core:bytecode-ast-tagbody/tags annot)
        for iblock = (delay-block inserter context ip :name name)
        finally
           ;; Establish a block for after the end.
           (delay-block inserter context
                        (core:bytecode-debug-info/end annot)
                        :name '#:tagbody-after)))

(defun make-iblock-r (inserter name
                      &key (receiving 0)
                        (dynamic-environment
                         (ast-to-bir::dynamic-environment inserter)))
  (let* ((iblock (ast-to-bir::make-iblock
                  inserter :name name
                           :dynamic-environment dynamic-environment))
         (phis
           (if (eql receiving -1) ; multiple values
               (list (make-instance 'bir:phi :iblock iblock))
               (loop repeat receiving
                     collect (make-instance 'bir:phi
                               :iblock iblock)))))
    (setf (bir:inputs iblock) phis)
    iblock))

(defmethod start-annotation ((annot core:bytecode-ast-block)
                             inserter context)
  (let* ((receiving (core:bytecode-ast-block/receiving annot))
         (freceiving (if (= receiving 1) -1 receiving))
         (name (core:bytecode-ast-block/name annot))
         (end (core:bytecode-debug-info/end annot)))
    (delay-block inserter context end
                 :name (symbolicate name '#:-after)
                 :receiving freceiving)
    ;; this and FRECEIVING are to take care of the ugly code we generate
    ;; when a block is in a one-value context. See bytecode_compiler.cc.
    ;; Basically, we have entry -> [body] -> jump normal; exit: push; normal:
    ;; exits jump to the exit label. This is done so that nonlocal return
    ;; values can always be put in the MV vector, but it sure looks ugly.
    (when (= receiving 1)
      (delay-block inserter context (1+ end) ; 1+ for the push.
                   :name (symbolicate name '#:after-push)
                   :receiving receiving))))

(defmethod start-annotation ((the core:bytecode-ast-the) inserter context)
  (let* ((type (core:bytecode-ast-the/type the))
         (ptype (env:parse-values-type-specifier
                 type clasp-cleavir:*clasp-env* clasp-cleavir:*clasp-system*))
         (receiving (core:bytecode-ast-the/receiving the)))
    (case receiving
      ((1)
       (stack-push (compile-type-decl :the ptype (stack-pop context)
                                      inserter context)
                   context))
      ((-1)
       (setf (mvals context)
             (compile-type-decl :the ptype (mvals context) inserter context)))
      ;; TODO: Something for 0/single values?
      (otherwise))))

(defun compile-type-decl (which ctype datum inserter context)
  (let ((sys clasp-cleavir:*clasp-system*)
        (sv-ctype-p (member which '(:variable :argument :setq))))
    (if (if sv-ctype-p
            (ctype:top-p ctype sys)
            (clasp-cleavir::values-top-p ctype sys))
        datum ; too boring a type annotation to bother with
        (let* ((bir:*policy* (policy context))
               (vctype (ecase which
                         ((:the :return) ctype)
                         ((:variable) (ctype:single-value ctype sys))
                         ((:argument :setq)
                          (ctype:coerce-to-values ctype sys))))
               (out (make-instance 'bir:output
                      :name (bir:name datum)))
               (type-check-function
                 (ecase (clasp-cleavir::insert-type-checks-level bir:*policy* which)
                   ((0) :trusted)
                   ((1) nil)
                   ((2 3) (bytecompile-type-check which ctype sys
                                                  (inserter-module inserter))))))
          (ast-to-bir:insert inserter 'bir:thei
                             :inputs (list datum)
                             :outputs (list out)
                             :asserted-type vctype
                             :type-check-function type-check-function)
          out))))

;;; TODO? Probably could cache this, at least for standard types.
(defun bytecompile-type-check (which ctype sys module)
  (compile-bcfun-into
   (cmp:bytecompile (clasp-cleavir::make-type-check-fun which ctype sys))
   module))

;;; FIXME: To get declaration scope right w/ bytecode-debug-vars we'll probably
;;; need to ensure that decls appear before vars in the annotations.
(defmethod start-annotation ((annot core:bytecode-ast-decls)
                             inserter context)
  (declare (ignore inserter))
  (when (degenerate-annotation-p annot)
    (return-from start-annotation))
  (loop with opt = (first (optimize-stack context))
        for (spec . rest) in (core:bytecode-ast-decls/decls annot)
        do (case spec
             ((cl:optimize)
              (setf opt
                    (policy:normalize-optimize
                     (append (copy-list rest) opt) clasp-cleavir:*clasp-env*))))
        finally (push opt (optimize-stack context))
                (setf (policy context)
                      (policy:compute-policy opt clasp-cleavir:*clasp-env*))))
(defmethod end-annotation ((annot core:bytecode-ast-decls)
                           inserter context)
  (declare (ignore inserter))
  (when (degenerate-annotation-p annot)
    (return-from end-annotation))
  (pop (optimize-stack context))
  (setf (policy context) (policy:compute-policy (first (optimize-stack context))
                                                clasp-cleavir:*clasp-env*)))

(defmethod start-annotation ((annot core:bytecode-debug-location)
                             inserter context)
  (declare (ignore inserter))
  (when (degenerate-annotation-p annot)
    (return-from start-annotation))
  (push (kludge-spi (core:bytecode-debug-location/location annot))
        (origin-stack context)))
(defmethod end-annotation ((annot core:bytecode-debug-location)
                           inserter context)
  (declare (ignore inserter))
  (when (degenerate-annotation-p annot)
    (return-from end-annotation))
  (pop (origin-stack context)))

;;; Most processing is handled by maybe-begin-new, but there's still source info.
(defmethod start-annotation ((annot core:global-bytecode-simple-fun)
                             inserter context)
  (declare (ignore inserter))
  (let ((spi (function-spi annot)))
    (when spi
      (push (kludge-spi spi) (origin-stack context)))))
(defmethod end-annotation ((annot core:global-bytecode-simple-fun)
                           inserter context)
  (declare (ignore inserter))
  (when (function-spi annot)
    (pop (origin-stack context))))

(defun kludge-spi (spi)
  ;; KLUDGE KLUDGE KLUDGE
  ;; BIR currently checks that all source infos are
  ;; (source) CSTs. We don't have those.
  ;; Probably BIR needs adjustment.
  (if spi
      (make-instance 'cst:atom-cst :raw nil :source (cons spi spi))
      nil))

;;; default methods: irrelevant to compilation. ignore.
(defmethod start-annotation ((annot core:bytecode-debug-info)
                             inserter context)
  (declare (ignore inserter context)))
(defmethod end-annotation ((annot core:bytecode-debug-info)
                           inserter context)
  (declare (ignore inserter context)))
