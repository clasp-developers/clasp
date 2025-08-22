(defpackage #:clasp-bytecode-to-bir
  (:use #:cl)
  (:local-nicknames (#:bir #:cleavir-bir)
                    (#:set #:cleavir-set)
                    (#:ctype #:cleavir-ctype)
                    (#:env #:cleavir-env)
                    (#:policy #:cleavir-compilation-policy)
                    (#:build #:cleavir-bir-builder))
  (:export #:compile-function #:compile-hook))

(in-package #:clasp-bytecode-to-bir)

;;; To be bound to cmp:*btb-compile-hook*
(defun compile-hook (definition environment)
  (declare (ignore environment))
  (handler-case (compile-function definition)
    (error (e)
      (warn "BUG: Error during BTB compilation: ~a" e)
      definition)))

;;; During file compilation we will have a cmp:cfunction rather than an
;;; actual function.
(defun bcfun-p (annotation)
  (typep annotation '(or cmp:cfunction core:bytecode-simple-fun)))

(defgeneric bcfun/locals-size (bcfun))
(defmethod bcfun/locals-size ((bcfun core:bytecode-simple-fun))
  (core:bytecode-simple-fun/locals-frame-size bcfun))
(defmethod bcfun/locals-size ((bcfun cmp:cfunction))
  (cmp:cfunction/nlocals bcfun))
(defgeneric bcfun/nvars (bcfun))
(defmethod bcfun/nvars ((bcfun core:bytecode-simple-fun))
  (core:bytecode-simple-fun/environment-size bcfun))
(defmethod bcfun/nvars ((bcfun cmp:cfunction))
  (length (cmp:cfunction/closed bcfun)))
(defgeneric bcfun/fname (bcfun))
(defmethod bcfun/fname ((bcfun core:bytecode-simple-fun))
  (core:function-name bcfun))
(defmethod bcfun/fname ((bcfun cmp:cfunction))
  (cmp:cfunction/name bcfun))
(defgeneric bcfun/docstring (bcfun))
(defmethod bcfun/docstring ((bcfun core:bytecode-simple-fun))
  (core:function-docstring bcfun))
(defmethod bcfun/docstring ((bcfun cmp:cfunction))
  (cmp:cfunction/doc bcfun))
(defgeneric bcfun/spi (bcfun))
(defmethod bcfun/spi ((bcfun core:bytecode-simple-fun))
  (function-spi bcfun))
(defmethod bcfun/spi ((bcfun cmp:cfunction))
  (cmp:cfunction/source-pos-info bcfun))

(defgeneric compile-instruction (mnemonic inserter context &rest args))
(defgeneric start-annotation (annotation inserter context))
(defgeneric end-annotation (annotation inserter context))

(defun compile-bcmodule (bcmodule)
  (let ((irmodule (make-instance 'bir:module)))
    (values irmodule (compile-bcmodule-into bcmodule irmodule))))

;;; Process a bytecode module's literals into something easy to turn into
;;; BIR. Note that we avoid making constants/LTVs ahead of time, since
;;; BTB or Cleavir may optimize them away (e.g. in unreachable code).
;;; TODO?: If the bytecode recorded immutable LTVs as well, and the forms (or
;;; bytecode functions) that produced the values, we could dump an actual
;;; bytecode function into a FASL with zero information loss.
(defun compute-runtime-literals (literals mutables)
  (loop with nlits = (length literals)
        with result = (make-array nlits)
        for i from 0 below nlits
        for value = (aref literals i)
        do (setf (aref result i)
                 (if (member i mutables) ; FIXME: a bit inefficient
                     (cons `',value :ltv-mutable)
                     (cons value nil)))
        finally (return result)))

(defun compile-bcmodule-into (bcmodule irmodule)
  (let ((literals (core:bytecode-module/literals bcmodule))
        (mutables (core:bytecode-module/mutable-literals bcmodule)))
    (compile-bytecode-into (core:bytecode-module/bytecode bcmodule)
                           (core:bytecode-module/debug-info bcmodule)
                           (compute-runtime-literals literals mutables)
                           irmodule)))

;;; Now, for compile file
(defgeneric compute-compile-literal (info))
(defmethod compute-compile-literal ((info cmp:constant-info))
  (cons (cmp:constant-info/value info) nil))
(defmethod compute-compile-literal ((info cmp:cfunction))
  (cons info nil))
(defmethod compute-compile-literal ((info cmp:variable-cell-info))
  (cons info nil))
(defmethod compute-compile-literal ((info cmp:function-cell-info))
  (cons info nil))
(defmethod compute-compile-literal ((info cmp:load-time-value-info))
  (cons (cmp:load-time-value-info/form info)
        (if (cmp:load-time-value-info/read-only-p info)
            :ltv-readonly
            :ltv-mutable)))

(defun compute-compile-literals (literals)
  (map 'vector #'compute-compile-literal literals))

;;; Shared entry point for both runtime and compile-file-time.
(defun compile-bytecode-into (bytecode annotations literals irmodule)
  (let* ((blockmap (make-blockmap)) (funmap (make-funmap))
         (context (make-context irmodule blockmap funmap))
         (inserter (make-instance 'build:inserter))
         ;; annotations starting at opip.
         (opannots (initial-annotations annotations))
         ;; all annotations currently in scope.
         (annots
           (sort (copy-list opannots) #'<
                 :key #'core:bytecode-debug-info/end)))
    ;; Compile.
    (#+building-clasp cmp::do-instructions
     #-building-clasp core:do-instructions (mnemonic args opip ip next-annots)
        (bytecode :annotations annotations)
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
    ;; Return the funmap.
    funmap))

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
        (build:begin inserter (bir:start irfun))
        (context-new-function context bcfun))))
  ;; Maybe we've determined earlier that this IP begins a block?
  (let ((binfo (find-block opip (blockmap context))))
    (when binfo
      ;; If we're falling through from an existing block
      ;; compile in an implicit jump.
      (when (and (reachablep context)
                 (not (bir:terminatedp (bir:iblock inserter))))
        (%compile-jump inserter context binfo))
      ;; Start new block.
      (build:begin inserter (binfo-irblock binfo))
      (context-new-block context (binfo-context binfo)))))

;;; Given a bytecode function, return a compiled native function.
;;; Used for CL:COMPILE.
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
          ;; Ensure any closures have the same layout as original
          ;; bytecode closures, so the simple fun can be swapped
          ;; out transparently.
          (clasp-cleavir::*fixed-closures*
            (fixed-closures-map (fmap funmap)))
          (bir (finfo-irfun (find-bcfun function funmap))))
      (clasp-cleavir::bir->function bir :abi abi :linkage linkage))))

;;; COMPILE-FILE interface: take the components of a CMP:MODULE and return
;;; an NMODULE. The NMODULE contains everything COMPILE-FILE needs to dump
;;; native code such that the loader can use it:
;;; 1) The actual native code as bytes (an encoded ELF object)
;;; 2) A mapping from cfunctions to llvm-function-infos. These provide
;;;    information that can be communicated to the loader to let it
;;;    reconstruct functions.
;;; 3) A vector of literals. Each entry is either:
;;;    - an integer, indicating the constant at that position of the
;;;      cmp:module's literals. This may include cfunctions.
;;;    - a cmp:constant-info or etc., indicating a constant not in the
;;;      cmp:module's literals.
;;;    - a LATE-FUNCTION, meaning a function introduced by BTB/translate,
;;;      e.g. by transforms. Maybe. Not sure this happens, TODO
(defclass nmodule ()
  ((%code :initarg :code :reader nmodule-code)
   (%fmap :initarg :fmap :reader nmodule-fmap)
   (%literals :initarg :literals :reader nmodule-literals)))

(defgeneric bir-constant->cmp (bir))
(defmethod bir-constant->cmp ((constant bir:constant))
  (cmp:constant-info/make (bir:constant-value constant)))
(defmethod bir-constant->cmp ((ltv bir:load-time-value))
  (cmp:load-time-value-info/make (bir:form ltv) (bir:read-only-p ltv)))
(defmethod bir-constant->cmp ((fcell bir:function-cell))
  (cmp:function-cell-info/make (bir:function-name fcell)))
(defmethod bir-constant->cmp ((vcell bir:variable-cell))
  (cmp:variable-cell-info/make (bir:variable-name vcell)))

(defun compute-nliterals (cliterals constants)
  ;; CONSTANTS is the hash table produced by translation.
  ;; Keys are BIR things, and values are indices into the eventual
  ;; literals vector.
  ;; CLITERALS is the vector produced by BTB, below.
  ;; This consists of conses where the CDR is a placeholder or BIR thing.
  ;; CONSTANTS may include constants not in CLITERALS and vice versa.
  ;; This function produces a vector explaining to the file compiler how
  ;; to produce the machine literals. There is one element for every index
  ;; in the machine literals. Each element is of the format described in
  ;; the comment above NMODULE, above.
  (let* ((num (loop for value being the hash-values of constants
                    maximizing (if (integerp value) (1+ value) 0)))
         (nliterals (make-array num)))
    (loop for key being the hash-keys of constants
            using (hash-value indexoid)
          when (integerp indexoid) ; not an immediate
            do (setf (aref nliterals indexoid)
                     ;; Check if the bytecode already had this constant.
                     (or (position key cliterals :key #'cdr)
                         ;; No, so translate back into CMP terms.
                         (bir-constant->cmp key))))
    nliterals))

(defun fixed-closures-map (fmap)
  (loop for entry in fmap
        for ir = (finfo-irfun entry)
        for clos = (loop for thing in (finfo-closure entry)
                         when (consp thing)
                           collect (car thing)
                         else
                           collect thing)
        collect (cons ir clos)))

;;; Given a bytecode function, compile it into the given IR module.
;;; that is, this does NOT finish the compilation process.
;;; the BIR:FUNCTION is returned.
;;; Used in compile-type-decl.
(defun compile-bcfun-into (function irmodule)
  (let ((fmap (compile-bcmodule-into (core:simple-fun-code function)
                                     irmodule)))
    (finfo-irfun (find-bcfun function fmap))))

;;; Return a list of all annotations that start at IP 0.
(defun initial-annotations (annotations)
  (loop for annot across annotations
        if (= (core:bytecode-debug-info/start annot) 0)
          collect annot
        else do (loop-finish)))

(defun start-annotations (annotations inserter context)
  (loop for a in annotations
        do (start-annotation a inserter context)))

(defun annotation< (annot1 annot2)
  ;; We keep ANNOTS sorted by end IP.
  ;; If they tie on end IP, we use the reverse order of the starts.
  ;; Imagine we have an annotation 5-13 and another 11-13.
  ;; We want to start the 5-13, then start the 11-13, then end the 11-13.
  ;; That's why we reverse order of the starts.
  ;; If two annotations start and end at the same point there may be a
  ;; problem. I don't think this can actually arise.
  (let ((end1 (core:bytecode-debug-info/end annot1))
        (end2 (core:bytecode-debug-info/end annot2)))
    (cond ((< end1 end2) t)
          ((= end1 end2)
           (> (core:bytecode-debug-info/start annot1)
              (core:bytecode-debug-info/start annot2)))
          (t nil))))

(defun add-annotations (annots next-annots)
  ;; We keep ANNOTS sorted by end IP.
  ;; FIXME? This could be done without consing, but it would be uglier.
  (let ((sna (sort (copy-list next-annots) #'annotation<)))
    (merge 'list annots sna #'annotation<)))

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
                  ((:constant)
                   ;; (value . ltvp)
                   ;; if an ltv, VALUE is a bir:load-time-value
                   ;; otherwise it's whatever unadorned object
                   (aref literals value))
                  ((:label) value)
                  ((:keys)
                   ;; not actually used, so whatever
                   value)
                  ((:operand) value))))

;;; Mapping from bytecode functions to IR functions.
(defclass funmap ()
  (;; List of (bcfun irfun closure); see finfo- accessors below.
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
                       (bir:dynamic-environment inserter)))
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
        (key-literals-start (cdr (third args))))
    (list more-args key-count-info
          (loop for i from key-literals-start
                repeat (car key-count-info)
                collect (car (aref literals i))))))

(defun make-bir-function (bytecode-function inserter
                          &optional (module (bir:module inserter)))
  (let* ((lambda-list (ext:function-lambda-list bytecode-function))
         (function (make-instance 'bir:function
                     :returni nil ; set by :return compilation
                     :name (bcfun/fname bytecode-function)
                     :lambda-list nil
                     :docstring (bcfun/docstring bytecode-function)
                     :original-lambda-list lambda-list
                     :origin (bcfun/spi bytecode-function)
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
   (%variable-stack :initform nil :initarg :variable-stack
                    :accessor variable-stack :type list)
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
  (build:make-iblock
   inserter
   :name (symbolicate (write-to-string (bcfun/fname bcfun))
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
  (loop for (_ . map) in (variable-stack context)
        for pair = (assoc name map)
        when pair return (cdr pair)
        finally (return (ctype:top clasp-cleavir:*clasp-system*))))

(defun %read-variable (variable inserter)
  (let ((readvar-out (make-instance 'bir:output :name (bir:name variable))))
    (build:insert inserter 'bir:readvar
                  :inputs (list variable) :outputs (list readvar-out))
    readvar-out))

(defmethod compile-instruction ((mnemonic (eql :const))
                                inserter context &rest args)
  (destructuring-bind (c) args
    (let ((output (make-instance 'bir:output))
          (value (car c)) (existing (cdr c)))
      (etypecase existing
        (null ; constant, not yet processed
         (let ((const (build:constant inserter value)))
           (setf (cdr c) const)
           (build:insert inserter 'bir:constant-reference
                         :inputs (list const) :outputs (list output))))
        ((member :ltv-mutable :ltv-readonly)
         (let ((ltv (bir:load-time-value-in-module
                     value (eql existing :ltv-readonly)
                     (bir:module inserter))))
           (setf (cdr c) ltv)
           (build:insert inserter 'bir:load-time-value-reference
                         :inputs (list ltv) :outputs (list output))))
        (bir:constant
         (build:insert inserter 'bir:constant-reference
                       :inputs (list existing) :outputs (list output)))
        (bir:load-time-value
         (build:insert inserter 'bir:load-time-value-reference
                       :inputs (list existing) :outputs (list output))))
      (stack-push output context))))

(defun compile-constant (value inserter)
  (let* ((const (build:constant inserter value))
         (cref-out (make-instance 'bir:output)))
    (build:insert inserter 'bir:constant-reference
                  :inputs (list const) :outputs (list cref-out))
    cref-out))

(defmethod compile-instruction ((mnemonic (eql :closure)) inserter
                                context &rest args)
  (destructuring-bind (index) args
    (let* ((ifun (bir:function inserter))
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
    (setf (mvals context) (compile-call nargs inserter context))))

(defmethod compile-instruction ((mnemonic (eql :call-receive-one))
                                inserter context &rest args)
  (destructuring-bind (nargs) args
    (setf (mvals context) nil) ; invalidate for self-consistency checks
    (stack-push (compile-call nargs inserter context) context)))

(defmethod compile-instruction ((mnemonic (eql :call-receive-fixed))
                                inserter context &rest args)
  (destructuring-bind (nargs nvals) args
    (assert (zerop nvals)) ; FIXME
    (setf (mvals context) nil) ; invalidate for self-consistency checks
    (compile-call nargs inserter context)))

(defun compile-call (nargs inserter context)
  (let* ((args (gather context nargs))
         (callee (stack-pop context))
         (ftype (callee-ftype callee inserter))
         (rargs (type-wrap-arguments ftype args inserter context))
         (out (make-instance 'bir:output))
         (sys clasp-cleavir::*clasp-system*))
    (build:insert inserter 'bir:call
                  :inputs (list* callee rargs)
                  :outputs (list out))
    (compile-type-decl :return (ctype:function-values ftype sys) out
                       inserter context)))

(defun gather (context n)
  (loop with args = nil
        repeat n
        do (push (stack-pop context) args)
        finally (return args)))

;;; Get the ftype of an output.
;;; Right now this is trivial, but - TODO - might be more involved when
;;; we support local ftype declarations.
(defun callee-ftype (callee context)
  (declare (ignore context))
  (let* ((sys clasp-cleavir:*clasp-system*)
         ;; We use the asserted type rather than the derived because
         ;; that's what :fdefinition etc put in, and also because we've yet to
         ;; actually derive anything.
         (vct (bir:asserted-type callee))
         (ct (ctype:primary vct sys)))
    (if (ctype:functionp ct sys)
        ct
        (ctype:function-top sys))))

;;; Given an ftype and a list of argument outputs, return a new list of argument
;;; outputs that are type wrapped if that's useful.
;;; Also warn on argument count mismatch.
(defun type-wrap-arguments (ftype args inserter context)
  (let* ((sys clasp-cleavir:*clasp-system*)
         (req (ctype:function-required ftype sys))
         (opt (ctype:function-optional ftype sys))
         (rest (ctype:function-rest ftype sys))
         (rrest (cond ((not (ctype:bottom-p rest sys)) rest)
                      ;; Arguably FIXME that we can have &rest nil with keys.
                      ((ctype:function-keysp ftype sys) (ctype:top sys))
                      (t rest)))
         (min (length req))
         (max (if (not (ctype:bottom-p rrest sys))
                  nil
                  (+ min (length opt))))
         (nargs (length args)))
    (when (or (< nargs min) (and max (> nargs max)))
      (warn 'cmp:wrong-argcount-warning
            :given-nargs nargs :min-nargs min :max-nargs max
            :origin (cst:source bir:*origin*))
      (return-from type-wrap-arguments args))
    (loop for arg in args
          for ctype = (cond (req (pop req))
                            (opt (pop opt))
                            (t rrest))
          collect (compile-type-decl :argument ctype arg inserter context))))

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
  (build:insert inserter 'bir:writevar
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

(defmethod compile-instruction ((mnemonic (eql :encell))
                                inserter context &rest args)
  (declare (ignore inserter))
  (destructuring-bind (varindex) args
    (let* ((varinfo (aref (locals context) varindex))
           (arg (car varinfo)) (cellp (cdr varinfo)))
      ;; encell is only generated for arguments; ergo
      (check-type arg bir:argument)
      (assert (not cellp))
      (setf (aref (locals context) varindex) (cons arg t)))))

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

;;; used by make-closure and protect instructions.
(defun make-closure (template inserter context)
  (let* ((irfun (make-bir-function template inserter))
         (enclose-out (make-instance 'bir:output
                        :name (core:function-name template)))
         (nclosed (bcfun/nvars template))
         (closed (nreverse (subseq (stack context) 0 nclosed)))
         (real-closed (mapcar #'resolve-closed closed)))
    (assert (every (lambda (v) (typep v '(or bir:come-from
                                          (cons bir:variable))))
                   real-closed))
    (build:insert inserter 'bir:enclose
                  :code irfun :outputs (list enclose-out))
    (add-function context template irfun real-closed)
    (setf (stack context) (nthcdr nclosed (stack context)))
    enclose-out))

(defmethod compile-instruction ((mnemonic (eql :make-closure))
                                inserter context &rest args)
  (destructuring-bind ((template)) args
    (stack-push (make-closure template inserter context) context)))

(defmethod compile-instruction ((mnemonic (eql :make-uninitialized-closure))
                                inserter context &rest args)
  ;; Set up an ir function for the funmap and generate an enclose,
  ;; but leave the closure for initialize-closure.
  (destructuring-bind ((template)) args
    (let ((irfun (make-bir-function template inserter))
          (enclose-out (make-instance 'bir:output
                         :name (core:function-name template))))
      (build:insert inserter 'bir:enclose
                    :code irfun :outputs (list enclose-out))
      (add-function context template irfun nil)
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
    (unless (bir:terminatedp (bir:iblock inserter))
      (let ((input (mvals context)))
        (check-type input bir:linear-datum)
        (build:terminate inserter 'bir:returni :inputs (list input))))))

(defmethod compile-instruction ((mnemonic (eql :bind-required-args))
                                inserter context &rest args)
  (destructuring-bind (nreq) args
    (loop with ifun = (bir:function inserter)
          with locals = (locals context)
          for i from 0 below nreq
          for arg = (make-instance 'bir:argument :function ifun)
          do (setf (aref locals i) (cons arg nil))
          collect arg into args
          finally (setf (bir:lambda-list ifun) args))))

(defmethod compile-instruction ((mnemonic (eql :bind-optional-args))
                                inserter context &rest args)
  (destructuring-bind (start nopt) args
    (declare (ignore start))
    (let* ((ifun (bir:function inserter))
           (ll (bir:lambda-list ifun)))
      (loop for arg = (make-instance 'bir:argument :function ifun)
            for -p = (make-instance 'bir:argument :function ifun)
            repeat nopt
            do (stack-push arg context)
            collect (list arg -p) into ll-app
            finally (setf (bir:lambda-list ifun)
                          (append ll '(&optional) ll-app))))))

(defmethod compile-instruction ((mnemonic (eql :listify-rest-args))
                                inserter context &rest args)
  (destructuring-bind (start) args
    (declare (ignore start))
    (let* ((ifun (bir:function inserter))
           (ll (bir:lambda-list ifun))
           (rarg (make-instance 'bir:argument :function ifun)))
      (setf (bir:lambda-list ifun) (append ll `(&rest ,rarg)))
      (stack-push rarg context))))
(defmethod compile-instruction ((mnemonic (eql :vaslistify-rest-args))
                                inserter context &rest args)
  (destructuring-bind (start) args
    (let* ((ifun (bir:function inserter))
           (ll (bir:lambda-list ifun))
           (rarg (make-instance 'bir:argument :function ifun)))
      (setf (bir:lambda-list ifun) (append ll `(core:&va-rest ,rarg))
            (aref (locals context) start) (cons rarg nil)))))

(defmethod compile-instruction ((mnemonic (eql :parse-key-args))
                                inserter context &rest args)
  (destructuring-bind (start (key-count . aokp) keys) args
    (declare (ignore start key-count))
    (let* ((ifun (bir:function inserter))
           (ll (bir:lambda-list ifun)))
      ;; The keys are put on the stack such that
      ;; the leftmost key is the _last_ pushed, etc.
      (loop for key in keys
            for arg = (make-instance 'bir:argument :function ifun)
            for -p = (make-instance 'bir:argument :function ifun)
            collect (list key arg -p) into ll-app
            finally (setf (bir:lambda-list ifun)
                          (append ll '(&key) ll-app
                                  (if aokp '(&allow-other-keys) ())))
                    (loop for (_1 arg _2) in (nreverse ll-app)
                          do (stack-push arg context))))))

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
    (build:terminate inserter 'bir:jump
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
         (else-dest (build:make-iblock
                     inserter :name '#:if-else)))
    (build:terminate inserter 'bir:ifi
                     :inputs (list condition)
                     :next (list then-dest else-dest))
    ;; The else block we just start here.
    (build:begin inserter else-dest)))

(defmethod compile-instruction ((mnemonic (eql :jump-if-8))
                                inserter context &rest args)
  (apply #'compile-jump-if inserter context args))
(defmethod compile-instruction ((mnemonic (eql :jump-if-16))
                                inserter context &rest args)
  (apply #'compile-jump-if inserter context args))
(defmethod compile-instruction ((mnemonic (eql :jump-if-24))
                                inserter context &rest args)
  (apply #'compile-jump-if inserter context args))

(defun compile-jump-if-supplied (inserter context true-dest)
  (let ((arg (stack-pop context)))
    (check-type arg bir:argument) ; from bind-optional-args
    ;; Now find the corresponding -p argument and branch on it.
    (let* ((ifun (bir:function inserter))
           (-p (loop for e in (bir:lambda-list ifun)
                     when (consp e)
                       do (ecase (length e)
                            (2 (when (eq (first e) arg)
                                 (return (second e))))
                            (3 (when (eq (second e) arg)
                                 (return (third e)))))))
           (thenb (build:make-iblock inserter :name '#:if-supplied))
           (elseb (build:make-iblock inserter :name '#:if-unsupplied))
           (endb (delay-block inserter context true-dest
                              :name '#:if-supplied
                              :receiving 1)))
      (build:terminate inserter 'bir:ifi
                       :inputs (list -p)
                       :next (list thenb elseb))
      (build:begin inserter thenb)
      ;; This block does nothing but jump immediately to the end,
      ;; sending the arg.
      (build:terminate inserter 'bir:jump
                       :inputs (list arg) :outputs (bir:inputs endb)
                       :next (list endb))
      (build:begin inserter elseb))))

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
    (let ((mv (mvals context))
          (during (build:make-iblock inserter :name '#:save-values))
          (save-out (make-instance 'bir:output :name '#:saved-values)))
      (build:terminate inserter 'bir:values-save
                       :inputs (list mv) :outputs (list save-out)
                       :next (list during))
      (setf (mvals context) nil)
      (stack-push (list :multiple-values save-out) context)
      (build:begin inserter during))))

(defmethod compile-instruction ((mnemonic (eql :append-values))
                                inserter context &rest args)
  (destructuring-bind () args
    (let ((mv (mvals context))
          (during (build:make-iblock inserter :name '#:save-values))
          (save-out (make-instance 'bir:output :name '#:saved-values))
          (previous (stack-pop context)))
      (check-type previous (cons (eql :multiple-values)))
      (build:terminate inserter 'bir:values-save
                       :inputs (list mv) :outputs (list save-out)
                       :next (list during))
      (setf (mvals context) nil)
      (stack-push (list* :multiple-values save-out (cdr previous)) context)
      (build:begin inserter during))))

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
             (after (build:make-iblock inserter
                                       :name '#:mv-prog1-after
                                       :dynamic-environment old-de)))
        (check-type save bir:values-save)
        (setf (mvals context) read-out)
        (build:insert inserter 'bir:values-restore
                      :inputs (list mv) :outputs (list read-out))
        (build:terminate inserter 'bir:jump
                         :inputs () :outputs () :next (list after))
        (build:begin inserter after)))))

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
           (after (build:make-iblock inserter :name '#:mv-call-after
                                              :dynamic-environment old-de)))
      (check-type lastdef bir:values-save)
      ;; Morph the most recent values-save into a -collect
      ;; so that we have a proper mv call
      (change-class lastdef 'bir:values-collect
                    :inputs (append (butlast args) (bir:inputs lastdef)))
      ;; Generate the actual call
      (build:insert inserter 'bir:mv-call
                    :inputs (list callee mv) :outputs (list out))
      (build:terminate inserter 'bir:jump
                       :inputs () :outputs () :next (list after))
      (build:begin inserter after))
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
    (let* ((during (build:make-iblock inserter :name '#:block))
           (cf (build:terminate inserter 'bir:come-from
                                :next (list during))))
      (setf (aref (locals context) index) (cons cf nil))
      (build:begin inserter during))))

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
    (build:terminate inserter 'bir:unwind
                     :inputs inputs
                     :outputs (copy-list (bir:inputs dest))
                     :come-from cf
                     :destination dest)
    ;; Don't add duplicate NEXT entries
    ;; (obscure NLX uses can hit this, like CORE::PACKAGES-ITERATOR)
    (unless (eql dest (first (bir:next cf)))
      (pushnew dest (rest (bir:next cf)))
      (set:nadjoinf (bir:predecessors dest) (bir:iblock cf))))
  (setf (reachablep context) nil))

;;; FIXME: The iblocks generated here are often kind of pointless -
;;; i.e. only reachable by unwinding and then all they do is unwind more.
;;; Cleavir could maybe optimize such blocks away.
(defmethod compile-instruction ((mnemonic (eql :entry-close))
                                inserter context &rest args)
  (declare (ignore context))
  (destructuring-bind () args
    (let* ((de (bir:parent (bir:dynamic-environment inserter)))
           (ib (build:make-iblock
                inserter :name '#:entry-close :dynamic-environment de)))
      (build:terminate inserter 'bir:jump
                       :inputs () :outputs () :next (list ib))
      (build:begin inserter ib))))

(defgeneric vcell/name (vcell))
(defmethod vcell/name ((vcell core:variable-cell))
  (core:variable-cell/name vcell))
(defmethod vcell/name ((vcell cmp:variable-cell-info))
  (cmp:variable-cell-info/vname vcell))

(defmethod compile-instruction ((mnemonic (eql :special-bind))
                                inserter context &rest args)
  (destructuring-bind (entry) args
    (let* ((vcell (car entry)) (existing (cdr entry))
           (vname (vcell/name vcell))
           (const (or existing (build:vcell inserter vname)))
           (bname (symbolicate '#:bind- vname))
           (next (build:make-iblock inserter :name bname))
           (value (stack-pop context)))
      (setf (cdr entry) const)
      (build:terminate inserter 'bir:constant-bind
                       :inputs (list const value)
                       :next (list next))
      (build:begin inserter next))))

(defmethod compile-instruction ((mnemonic (eql :symbol-value))
                                inserter context &rest args)
  (destructuring-bind (entry) args
    (let* ((vcell (car entry)) (existing (cdr entry))
           (vname (vcell/name vcell))
           (const (or existing (build:vcell inserter vname)))
           (out (make-instance 'bir:output :name vname)))
      (setf (cdr entry) const)
      (build:insert inserter 'bir:constant-symbol-value
                    :inputs (list const) :outputs (list out))
      (stack-push out context))))

(defmethod compile-instruction ((mnemonic (eql :symbol-value-set))
                                inserter context &rest args)
  (destructuring-bind (entry) args
    (let* ((vcell (car entry)) (existing (cdr entry))
           (const (or existing
                      (setf (cdr entry)
                            (build:vcell inserter (vcell/name vcell)))))
           (in (stack-pop context)))
      (build:insert inserter 'bir:set-constant-symbol-value
                    :inputs (list const in)))))

(defmethod compile-instruction ((mnemonic (eql :unbind))
                                inserter context &rest args)
  (declare (ignore context))
  (destructuring-bind () args
    (let* ((bind (bir:dynamic-environment inserter))
           (vname (bir:variable-name (first (bir:inputs bind))))
           (ib (build:make-iblock
                inserter :name (symbolicate '#:unbind- vname)
                :dynamic-environment (bir:parent bind))))
      (build:terminate inserter 'bir:jump
                       :inputs () :outputs ()
                       :next (list ib))
      (build:begin inserter ib))))

(defgeneric fcell/name (fcell))
(defmethod fcell/name ((fcell core:function-cell))
  ;; FIXME: May not be a sufficiently reliable way to get
  ;; the name from the cell in all cases? Probably ok though
  (core:function-name fcell))
(defmethod fcell/name ((fcell cmp:function-cell-info))
  (cmp:function-cell-info/fname fcell))

(defmethod compile-instruction ((mnemonic (eql :fdefinition))
                                inserter context &rest args)
  (destructuring-bind (entry) args
    (let* ((fcell (car entry)) (existing (cdr entry))
           (fname (fcell/name fcell))
           (const (or existing
                      (setf (cdr entry)
                            (build:fcell inserter fname))))
           (attributes (clasp-cleavir::function-attributes fname))
           (ftype (ctype:single-value (clasp-cleavir::global-ftype fname)
                                      clasp-cleavir:*clasp-system*))
           (fdef-out (make-instance 'bir:output
                       :name fname :asserted-type ftype :attributes attributes)))
      (build:insert inserter 'bir:constant-fdefinition
                    :inputs (list const) :outputs (list fdef-out))
      (stack-push fdef-out context))))

;; Identical to the above, but BIR should maybe have a
;; CONSTANT-CALLED-FDEFINITION for this.
(defmethod compile-instruction ((mnemonic (eql :called-fdefinition))
                                inserter context &rest args)
  (destructuring-bind (entry) args
    (let* ((fcell (car entry)) (existing (cdr entry))
           (fname (fcell/name fcell))
           (const (or existing
                      (setf (cdr entry)
                            (build:fcell inserter fname))))
           (attributes (clasp-cleavir::function-attributes fname))
           (ftype (ctype:single-value (clasp-cleavir::global-ftype fname)
                                      clasp-cleavir::*clasp-system*))
           (fdef-out (make-instance 'bir:output
                       :name fname :asserted-type ftype :attributes attributes)))
      (build:insert inserter 'bir:constant-fdefinition
                    :inputs (list const) :outputs (list fdef-out))
      (stack-push fdef-out context))))

(defmethod compile-instruction ((mnemonic (eql :fdesignator))
                                inserter context &rest args)
  ;; Just call CORE:COERCE-CALLED-FDESIGNATOR.
  (destructuring-bind (env) args
    (declare (ignore env))
    (let* ((desig (stack-pop context))
           (fname 'core:coerce-called-fdesignator)
           (const (build:fcell inserter fname))
           (attributes (clasp-cleavir::function-attributes fname))
           (fdef-out (make-instance 'bir:output
                       :name fname :attributes attributes))
           (out (make-instance 'bir:output :name '#:callee)))
      (build:insert inserter 'bir:constant-fdefinition
                    :inputs (list const) :outputs (list fdef-out))
      (build:insert inserter 'bir:call
                    :inputs (list fdef-out desig)
                    :outputs (list out))
      (stack-push out context))))

(defmethod compile-instruction ((mnemonic (eql :protect))
                                inserter context &rest args)
  (destructuring-bind ((template)) args
    (let ((cleanup (make-closure template inserter context))
          (body (build:make-iblock inserter :name '#:protect)))
      (build:terminate inserter 'bir:unwind-protect
                       :inputs (list cleanup) :next (list body))
      (build:begin inserter body))))

(defmethod compile-instruction ((mnemonic (eql :cleanup))
                                inserter context &rest args)
  (declare (ignore context))
  (destructuring-bind () args
    (let* ((protect (bir:dynamic-environment inserter))
           (ib (build:make-iblock
                inserter :name '#:post-protection
                         :dynamic-environment (bir:parent protect))))
      (build:terminate inserter 'bir:jump
                       :inputs () :outputs ()
                       :next (list ib))
      (build:begin inserter ib))))

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
  (when (and (reachablep context)
             (not (degenerate-annotation-p annotation)))
    (loop with bir:*policy* = (policy context)
          for bdv in (core:bytecode-debug-vars/bindings annotation)
          for name = (core:bytecode-debug-var/name bdv)
          for cellp = (core:bytecode-debug-var/cellp bdv)
          for index = (core:bytecode-debug-var/frame-index bdv)
          for ctype = (declared-variable-ctype
                       (core:bytecode-debug-var/decls bdv) (consp name))
          for (datum) = (aref (locals context) index)
          ;; We make all variables IGNORABLE because the bytecode compiler
          ;; has already warned about any syntactically unused variables
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
          finally (push (cons annotation typemap) (variable-stack context)))))

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
  (build:insert inserter 'bir:leti
                :inputs (list value)
                :outputs (list variable)))

(defmethod end-annotation ((annot core:bytecode-debug-vars)
                           inserter context)
  (declare (ignore inserter))
  ;; Check if start-annotation actually did anything. If it didn't,
  ;; there's nothing for us to undo.
  (when (eq annot (first (first (variable-stack context))))
    ;; End the extent of all variables.
    (loop for bdv in (core:bytecode-debug-vars/bindings annot)
          for index = (core:bytecode-debug-var/frame-index bdv)
          do (setf (aref (locals context) index) nil))
    ;; And type declarations.
    (pop (variable-stack context))))

(defmethod start-annotation ((annot core:bytecode-ast-if)
                             inserter context)
  (when (reachablep context)
    ;; Record the merge block for later jumps.
    (delay-block inserter context (core:bytecode-debug-info/end annot)
                 :name '#:if-merge
                 :receiving (core:bytecode-ast-if/receiving annot))))

(defmethod start-annotation ((annot core:bytecode-ast-tagbody)
                             inserter context)
  (when (reachablep context)
    (loop for (name . ip) in (core:bytecode-ast-tagbody/tags annot)
          for iblock = (delay-block inserter context ip :name name)
          finally
             ;; Establish a block for after the end.
             (delay-block inserter context
                          (core:bytecode-debug-info/end annot)
                          :name '#:tagbody-after))))

(defun make-iblock-r (inserter name
                      &key (receiving 0)
                        (dynamic-environment
                         (bir:dynamic-environment inserter)))
  (build:make-iblock inserter :name name
                              :dynamic-environment dynamic-environment
                              ;; -1 means multiple values means one phi.
                              :ninputs (if (eql receiving -1) 1 receiving)))

(defmethod start-annotation ((annot core:bytecode-ast-block)
                             inserter context)
  (when (reachablep context)
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
                     :receiving receiving)))))

(defmethod start-annotation ((the core:bytecode-ast-the) inserter context)
  (when (reachablep context)
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
        (otherwise)))))

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
                                                  (bir:module inserter))))))
          (build:insert inserter 'bir:thei
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
(defmethod start-annotation ((annot core:bytecode-simple-fun)
                             inserter context)
  (declare (ignore inserter))
  (let ((spi (function-spi annot)))
    (when spi
      (push (kludge-spi spi) (origin-stack context)))))
(defmethod end-annotation ((annot core:bytecode-simple-fun)
                           inserter context)
  (declare (ignore inserter))
  (when (function-spi annot)
    (pop (origin-stack context))))

(defmethod start-annotation ((annot cmp:cfunction) inserter context)
  (declare (ignore inserter))
  (let ((spi (cmp:cfunction/source-pos-info annot)))
    (when spi
      (push (kludge-spi spi) (origin-stack context)))))
(defmethod end-annotation ((annot cmp:cfunction) inserter context)
  (declare (ignore inserter))
  (when (cmp:cfunction/source-pos-info annot)
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
