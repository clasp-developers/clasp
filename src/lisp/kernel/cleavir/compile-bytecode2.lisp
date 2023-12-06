(defpackage #:clasp-bytecode-to-bir2
  (:use #:cl)
  (:local-nicknames (#:bt #:clasp-bytecode-tablegen)
                    (#:bir #:cleavir-bir)
                    (#:set #:cleavir-set)
                    (#:ctype #:cleavir-ctype)
                    ;; FIXME: Move inserter stuff to its own small system
                    (#:ast-to-bir #:cleavir-ast-to-bir))
  (:export #:compile-function #:compile-hook))

(in-package #:clasp-bytecode-to-bir2)

(defun bcfun-p (annotation)
  (typep annotation 'core:global-bytecode-simple-fun))

(defun bcfun/locals-size (bcfun)
  (core:global-bytecode-simple-fun/locals-frame-size bcfun))

(defgeneric compile-instruction (mnemonic inserter context &rest args))
(defgeneric compile-annotation (annotation inserter context))

(defun compile-bcmodule (bcmodule)
  (let ((funmap nil) ; map from bcfuns to irfuns; returned.
        (irmodule (make-instance 'bir:module))
        (literals (core:bytecode-module/literals bcmodule))
        bcfun irfun context irblocks
        (inserter (make-instance 'ast-to-bir:inserter))
        ;; annotations starting at opip.
        (opannots
          (initial-annotations
           (core:bytecode-module/debug-info bcmodule))))
    ;; Compile.
    (core:do-module-instructions (mnemonic args opip ip annots)
        (bcmodule)
      (let ((new-bcfun (find-if #'bcfun-p opannots))
            (irblock (find-block opip irblocks)))
        (cond (new-bcfun
               (setf bcfun new-bcfun
                     irfun (make-bir-function bcfun irmodule)
                     context (make-context bcfun))
               (let ((start (make-start-block irfun bcfun)))
                 (setf (bir:start irfun) start)
                 (ast-to-bir:begin inserter start))
               (push (cons bcfun irfun) funmap))
              (irblock
               (ast-to-bir:begin inserter irblock))))
      (let ((args (if (eq mnemonic :parse-key-args)
                      (compute-pka-args args literals)
                      (compute-args args literals irblocks))))
        (apply #'compile-instruction mnemonic inserter context args)
        (compile-annotations annots inserter context))
      (setf opannots annots))
    ;; Compute all iblock flow orders.
    (loop for (bc . ir) in funmap
          do (bir:compute-iblock-flow-order ir))
    ;; Return
    (values irmodule funmap)))

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

;;; Return a list of all annotations that start at IP 0.
(defun initial-annotations (annotations)
  (loop for annot across annotations
        if (= (core:bytecode-debug-info/start annot) 0)
          collect annot
        else do (loop-finish)))

(defun compile-annotations (annotations inserter context)
  (loop for a in annotations
        do (compile-annotation a inserter context)))

(defun make-context (bcfun)
  (make-instance 'context
    :locals (make-array (bcfun/locals-size bcfun))))

(defun compute-args (args literals irblocks)
  (loop for (type . value) in args
        collect (ecase type
                  ((:constant) (aref literals value))
                  ((:label)
                   (find-block value irblocks))
                  ((:keys)
                   ;; not actually used, so whatever
                   value)
                  ((:operand) value))))

(defun find-block (ip irblocks)
  (cdr (assoc ip irblocks)))

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
   (%locals :initarg :locals :reader locals)
   (%mvals :accessor mvals)))

(defun stack-push (datum context)
  (push datum (stack context)))
(defun stack-pop (context) (pop (stack context)))

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
          (if cellp varinfo (read-variable var inserter)))
         (bir:come-from var))
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
    (let ((ifun (bir:function (ast-to-bir::iblock inserter))))
      (loop with locals = (locals context)
            for i from 0 below nreq
            for arg = (make-instance 'bir:argument :function ifun)
            do (setf (aref locals i) (cons arg nil))
            collect arg into args
            finally (setf (bir:lambda-list ifun) args)))))

(defmethod compile-instruction ((mnemonic (eql :check-arg-count-le))
                                inserter context &rest args)
  (declare (ignore inserter context args)))
(defmethod compile-instruction ((mnemonic (eql :check-arg-count-ge))
                                inserter context &rest args)
  (declare (ignore inserter context args)))
(defmethod compile-instruction ((mnemonic (eql :check-arg-count-eq))
                                inserter context &rest args)
  (declare (ignore inserter context args)))

(defun read-variable (variable inserter)
  (bir:record-variable-ref variable)
  (let ((readvar-out (make-instance 'bir:output :name (bir:name variable))))
    (ast-to-bir:insert inserter 'bir:readvar
                       :inputs (list variable) :outputs (list readvar-out))
    readvar-out))

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

(defun inserter-module (inserter)
  ;; FIXME: Export a-t-b:iblock or something.
  (bir:module (bir:function (ast-to-bir::iblock inserter))))

(defun inserter-fcell (fname inserter)
  (bir:function-cell-in-module fname (inserter-module inserter)))

(defmethod compile-instruction ((mnemonic (eql :pop))
                                inserter context &rest args)
  (destructuring-bind () args
    (let ((mv (stack-pop context)))
      (check-type mv bir:linear-datum)
      (setf (mvals context) mv))))

(defmethod compile-annotation ((annotation core:bytecode-debug-vars)
                               inserter context)
  (loop for (name . index)
          in (core:bytecode-debug-vars/bindings annotation)
        for cellp = (consp index)
        for rindex = (if cellp (car index) index)
        for (datum) = (aref (locals context) rindex)
        for variable = (make-instance 'bir:variable
                         :ignore nil :name name)
        do (check-type datum bir:linear-datum)
           (bind-variable variable datum inserter)
           (setf (aref (locals context) rindex)
                 (cons variable cellp))))

(defun bind-variable (variable value inserter)
  (let ((binder (ast-to-bir:insert inserter 'bir:leti
                                   :inputs (list value)
                                   :outputs (list variable))))
    (set:nadjoinf (bir:variables (bir:function binder)) variable)
    (setf (bir:binder variable) binder)))
