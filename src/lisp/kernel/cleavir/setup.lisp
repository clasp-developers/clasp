(in-package :clasp-cleavir)

#+(or)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *echo-repl-read* t))

(defvar *debug-cleavir* nil
  "controls if graphs are generated as forms are being compiled.")
(defvar *debug-cleavir-literals* nil
  "controls if cleavir debugging is carried out on literal compilation. 
when this is t a lot of graphs will be generated.")

(defvar *form* nil)
(defvar *ast* nil)
(defvar *hir* nil)
(defvar *save-hir* nil)
(defvar *mir* nil)

;;; FIXME: Move this earlier
;; changed by de/proclaim
(defvar *ftypes* (make-hash-table :test #'equal))

(defun global-ftype (name)
  (multiple-value-bind (value presentp) (gethash name *ftypes*)
    (if presentp value 'function)))

(defun (setf global-ftype) (type name)
  (setf (gethash name *ftypes*) type))

(defmethod cst:reconstruct :around (expression cst (client clasp) &key (default-source nil default-source-p))
  (call-next-method expression cst client :default-source (if default-source-p
                                                              default-source
                                                              (cst:source cst))))

#-cst
(defmethod cleavir-generate-ast:convert-constant-to-immediate
    ((n integer) environment (system clasp))
  (declare (ignore environment))
  ;; convert fixnum into immediate but bignums return nil
  (core:create-tagged-immediate-value-or-nil n))

#-cst
(defmethod cleavir-generate-ast:convert-constant-to-immediate
    ((n character) environment (system clasp))
  (declare (ignore environment))
  ;; convert character to an immediate
  (core:create-tagged-immediate-value-or-nil n))

#-cst
(defmethod cleavir-generate-ast:convert-constant-to-immediate
    ((n float) environment (system clasp))
  (declare (ignore environment))
  ;; single-float's can be converted to immediates, anything else will return nil
  (core:create-tagged-immediate-value-or-nil n))

(defmethod cleavir-env:variable-info ((environment clasp-global-environment) symbol)
  (core:stack-monitor)
  (cond (;; We can check whether this symbol names a constant variable
	 ;; by checking the return value of CONSTANTP. 
	 (constantp symbol)
	 ;; If it is a constant variable, we can get its value by
	 ;; calling SYMBOL-VALUE on it.
	 (make-instance 'cleavir-env:constant-variable-info
	   :name symbol
	   :value (symbol-value symbol)))
        (;; Use Clasp's core:specialp test to determine if it is special.
         ;; Note that in Clasp constants are also special (FIXME?) so we
         ;; have to do this test after checking for constantness.
         (ext:specialp symbol)
	 (make-instance 'cleavir-env:special-variable-info
            :name symbol
            :global-p t))
	(;; Maybe it's a symbol macro.
	 (ext:symbol-macro symbol)
	 (make-instance 'cleavir-env:symbol-macro-info
	   :name symbol
	   :expansion (macroexpand-1 symbol)))
	(;; Otherwise, this symbol does not have any variable
	 ;; information associated with it.
	 t
	 ;; Return NIL as the protocol stipulates.
	 nil)))

(defmethod cleavir-env:variable-info ((environment null) symbol)
  (cleavir-env:variable-info *clasp-env* symbol))

(defvar *fn-attributes* (make-hash-table :test #'equal))
(defvar *fn-transforms* (make-hash-table :test #'equal))

(macrolet ((set-dyn-calls (&rest names)
             `(progn ,@(loop for name in names
                             collect `(set-dyn-call ,name))))
           (set-dyn-call (name)
             `(setf (gethash ',name *fn-attributes*)
                    (cleavir-attributes:make-attributes :dyn-call)))
           (set-dx-calls (&rest names)
             `(progn ,@(loop for name in names
                             collect `(set-dx-call ,name))))
           (set-dx-call (name)
             `(setf (gethash ',name *fn-attributes*)
                    (cleavir-attributes:make-attributes :dx-call))))
  (set-dyn-calls
   apply funcall
   every some notevery notany
   sublis nsublis subst-if subst-if-not nsubst-if nsubst-if-not
   member member-if member-if-not
   mapc mapcar mapcan mapl maplist mapcon
   assoc assoc-if assoc-if-not
   rassoc rassoc-if rassoc-if-not
   intersection nintersection adjoin
   set-difference nset-difference
   set-exclusive-or nset-exclusive-or subsetp union nunion
   ;; Can't do most sequence functions, as ext sequence
   ;; functions can do arbitrary things.
   map map-into merge
   cleavir-ast:map-ast-depth-first-preorder
   cleavir-bir:map-iblocks
   cleavir-bir:map-iblock-instructions
   cleavir-ir:map-instructions
   cleavir-ir:map-instructions-with-owner
   cleavir-ir:map-instructions-arbitrary-order
   cleavir-ir:filter-instructions
   cleavir-ir:map-local-instructions
   cleavir-ir:filter-local-instructions)
  (set-dx-calls
   core:progv-function
   core:catch-function
   core:funwind-protect
   ;; FIXME: Can't do this for many things like APPLY, FUNCALL, etc.
   ;; because we don't distinguish between *which* functional argument
   ;; is DX.
   every some notevery notany
   sublis nsublis subst-if subst-if-not nsubst-if nsubst-if-not
   member member-if member-if-not
   mapc mapcar mapcan mapl maplist mapcon
   assoc assoc-if assoc-if-not
   rassoc rassoc-if rassoc-if-not
   intersection nintersection adjoin
   set-difference nset-difference
   set-exclusive-or nset-exclusive-or subsetp union nunion
   map map-into merge
   cleavir-ast:map-ast-depth-first-preorder
   cleavir-bir:map-iblocks
   cleavir-bir:map-iblock-instructions
   cleavir-ir:map-instructions
   cleavir-ir:map-instructions-with-owner
   cleavir-ir:map-instructions-arbitrary-order
   cleavir-ir:filter-instructions
   cleavir-ir:map-local-instructions
   cleavir-ir:filter-local-instructions))

(defun treat-as-special-operator-p (name)
  (cond
    ((cmp:treat-as-special-operator-p name) t)
    ((eq name 'cleavir-primop::call-with-variable-bound) nil)
    ((eq name 'unwind-protect) t)
    ((eq name 'core::vector-length) t)
    ((eq name 'core::%displacement) t)
    ((eq name 'core::%displaced-index-offset) t)
    ((eq name 'core::%array-total-size) t)
    ((eq name 'core::%array-rank) t)
    ((eq name 'core::%array-dimension) t)
    ((eq name 'core::acas) t)
    ((eq name 'core::bind-va-list) t)
    ((eq (symbol-package name) (find-package :cleavir-primop)) t)
    (t nil)))

;;; Store inline ASTs in the environment, keyed to their name.
;;; FIXME: Fix sysprops for setf names
(defun inline-ast (name)
  (core:get-sysprop name 'inline-ast))
(defun (setf inline-ast) (ast name)
  (core:put-sysprop name 'inline-ast ast))

;;; So that we can dump ASTs (for DEFUNs with an inline expansion)
(defmethod make-load-form ((ast cleavir-ast:ast) &optional environment)
  (values `(allocate-instance ,(class-of ast))
          `(initialize-instance
            ,ast
            ,@(loop for (keyword reader)
                    in (cleavir-io:save-info ast)
                    for value = (funcall reader ast)
                    collect `(quote ,keyword)
                    collect `(quote ,value)))))

(defmethod cleavir-env:function-info ((environment clasp-global-environment) function-name)
  (cond
    ((and (symbolp function-name) (treat-as-special-operator-p function-name))
     (make-instance 'cleavir-env:special-operator-info
		    :name function-name))
    ;; If the function name is the name of a macro, then
    ;; MACRO-FUNCTION returns something other than NIL.
    ((and (symbolp function-name) (not (null (macro-function function-name))))
     (make-instance 'cleavir-env:global-macro-info ; we're global, so the macro must be global.
		    :name function-name
		    :expander (macro-function function-name)
		    :compiler-macro (compiler-macro-function function-name)))
    ((fboundp function-name)
     (let* ((cleavir-ast (inline-ast function-name))
            (inline-status (core:global-inline-status function-name))
            (attr (or (gethash function-name *fn-attributes*)
                      (cleavir-attributes:default-attributes)))
            (transforms (gethash function-name *fn-transforms*)))
       (make-instance 'cleavir-env:global-function-info
                      :name function-name
                      :type (global-ftype function-name)
                      :compiler-macro (compiler-macro-function function-name)
                      :inline inline-status
                      :ast cleavir-ast
                      :attributes attr
                      :transforms transforms)))
    ;; A top-level defun for the function has been seen.
    ;; The expansion calls cmp::register-global-function-def at compile time,
    ;; which is hooked up so that among other things this works.
    ((cmp:known-function-p function-name)
     (make-instance 'cleavir-env:global-function-info
                    :name function-name
                    :compiler-macro (compiler-macro-function function-name)
                    :inline (core:global-inline-status function-name)
                    :ast (inline-ast function-name)))
    ( ;; If it is neither of the cases above, then this name does
     ;; not have any function-info associated with it.
     t
     ;; Return NIL as the protocol stipulates.
     nil)))

;;; The toplevel shell may have a bclasp environment - ignore it
(defmethod cleavir-env:symbol-macro-expansion (symbol (environment core:value-frame))
  (cleavir-environment:symbol-macro-expansion symbol *clasp-env*))

(defmethod cleavir-env:function-info ((environment null) symbol)
  (cleavir-env:function-info *clasp-env* symbol))

(defmethod cleavir-env:function-info ((environment core:value-environment) symbol)
  (cleavir-env:function-info (core:get-parent-environment environment) symbol))

(defmethod cleavir-env:function-info ((environment core:value-frame) symbol)
  (cleavir-env:function-info (core:get-parent-environment environment) symbol))

(defmethod cleavir-env:variable-info ((environment core:value-frame) symbol)
  (cleavir-env:variable-info (core:get-parent-environment environment) symbol))

(defmethod cleavir-env:variable-info ((environment core:value-environment) symbol)
  (cleavir-env:variable-info (core:get-parent-environment environment) symbol))

(defmethod cleavir-env:declarations ((environment null))
  (cleavir-env:declarations *clasp-env*))

(defmethod cleavir-env:declarations ((environment core:value-environment))
  (cleavir-env:declarations (core:get-parent-environment environment)))

;;; TODO: Handle (declaim (declaration ...))
(defmethod cleavir-env:declarations
    ((environment clasp-global-environment))
  '(;; behavior as in convert-form.lisp
    core:lambda-name))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *global-optimize*
    ;; initial value, changed by de/proclaim
    '((compilation-speed 1)
      (debug 1)
      (space 1)
      (speed 1)
      (safety 1))))

(eval-when (:compile-toplevel)
  (format t "about to compute-policy~%"))

(defvar *global-policy*
  '#.(cleavir-policy:compute-policy *global-optimize* *clasp-env*))

(defmethod cleavir-env:optimize-info ((environment clasp-global-environment))
  ;; The default values are all 3.
  (make-instance 'cleavir-env:optimize-info
                 :optimize *global-optimize*
                 :policy *global-policy*))

(defmethod cleavir-env:optimize-info ((environment NULL))
  (cleavir-env:optimize-info *clasp-env*))

(defmethod cleavir-env:optimize-info ((environment core:value-environment))
  (cleavir-env:optimize-info (core:get-parent-environment environment)))


(defmethod cleavir-environment:macro-function (symbol (environment clasp-global-environment))
  (cl:macro-function symbol))

(defmethod cleavir-environment:macro-function (symbol (environment null))
  (cl:macro-function symbol))

#+(or)(defmethod cleavir-environment:macro-function (symbol (environment core:environment))
	(cl:macro-function symbol environment))

#+(or)(defun cl:macro-function (symbol &optional (environment nil environment-p))
	(cond
	  ((typep environment 'core:environment)
	   (cl:macro-function symbol environment))
	  (environment
	   (cleavir-environment:macro-function symbol environment))
	  (t (cleavir-environment:macro-function symbol *clasp-env*))))

(defmethod cleavir-environment:symbol-macro-expansion (symbol (environment clasp-global-environment))
  (macroexpand symbol nil))

(defmethod cleavir-environment:symbol-macro-expansion (symbol (environment NULL))
  (macroexpand symbol nil))

;;; Used by ext:symbol-macro
(defun core:cleavir-symbol-macro (symbol environment)
  (let ((info (cleavir-env:variable-info environment symbol)))
    (if (typep info 'cleavir-env:symbol-macro-info)
        (let ((expansion (cleavir-env:expansion info)))
          (lambda (form env)
            (declare (ignore form env)
                     (core:lambda-name cleavir-symbol-macro-function))
            expansion))
        nil)))

;;; Used by core:operator-shadowed-p
(defun core:cleavir-operator-shadowed-p (name environment)
  (typep (cleavir-env:function-info environment name)
         '(or cleavir-env:local-function-info
           cleavir-env:local-macro-info)))

(defun type-expand-1 (type-specifier &optional env)
  (let (head)
    (etypecase type-specifier
      (class (return-from type-expand-1 (values type-specifier nil)))
      (symbol (setf head type-specifier))
      (cons (setf head (first type-specifier))))
    (let ((def (ext:type-expander head)))
      (if def
          (values (funcall def type-specifier env) t)
          (values type-specifier nil)))))

(defmethod cleavir-env:type-expand ((environment clasp-global-environment) type-specifier)
  (loop with ever-expanded = nil
        do (multiple-value-bind (expansion expanded)
               (type-expand-1 type-specifier environment)
             (if expanded
                 (setf ever-expanded t type-specifier expansion)
                 (return (values type-specifier ever-expanded))))))
(defmethod cleavir-env:type-expand ((environment null) type-specifier)
  (cleavir-env:type-expand clasp-cleavir:*clasp-env* type-specifier))

;;; Needed because the default method ends up with classes,
;;; and that causes bootstrapping issues.
(defmethod cleavir-env:parse-expanded-type-specifier
    ((type-specifier symbol) environment (system clasp))
  (declare (ignore environment))
  type-specifier)

(defmethod cleavir-env:has-extended-char-p ((environment clasp-global-environment))
  #+unicode t #-unicode nil)
(defmethod cleavir-env:has-extended-char-p ((environment null))
  (cleavir-env:has-extended-char-p clasp-cleavir:*clasp-env*))

(defmethod cleavir-env:float-types ((environment clasp-global-environment))
  '(#+short-float short-float single-float double-float #+long-float long-float))
(defmethod cleavir-env:float-types ((environment null))
  (cleavir-env:float-types clasp-cleavir:*clasp-env*))

(defmethod cleavir-env:upgraded-complex-part-types ((environment clasp-global-environment))
  ;; "ECL does not have specialized complex part types" according to upgraded-complex-part-type source.
  '(real))
(defmethod cleavir-env:upgraded-complex-part-types ((environment null))
  (cleavir-env:upgraded-complex-part-types clasp-cleavir:*clasp-env*))

(defmethod cleavir-env:upgraded-array-element-types ((environment clasp-global-environment))
  core::+upgraded-array-element-types+)
(defmethod cleavir-env:upgraded-array-element-types ((environment null))
  (cleavir-env:upgraded-array-element-types clasp-cleavir:*clasp-env*))

(defun cleavir-env->interpreter (env)
  ;; Convert a cleavir ENTRY (or null) into an environment clasp's interpreter can use.
  ;; Only for compile time environments, so it's symbol macros, macros, and declarations.
  ;; The interpreter doesn't use declarations besides SPECIAL.
  (etypecase env
    (clasp-global-environment nil)
    (null env)
    (cleavir-env:special-variable
     (core:make-value-environment-for-locally-special-entries
      (list (cleavir-env:name env))
      (cleavir-env->interpreter (cleavir-env::next env))))
    (cleavir-env:symbol-macro
     (let ((result (core:make-symbol-macrolet-environment
                    (cleavir-env->interpreter (cleavir-env::next env)))))
       (core:add-symbol-macro
        result (cleavir-env:name env)
        (constantly (cleavir-env:expansion env)))
       result))
    (cleavir-env:macro
     (let ((result (core:make-macrolet-environment
                    (cleavir-env->interpreter (cleavir-env::next env)))))
       (core:add-macro result (cleavir-env:name env)
                       (cleavir-env:expander env))
       result))
    (cleavir-env::entry (cleavir-env->interpreter (cleavir-env::next env)))))

(defvar *use-ast-interpreter* t)

(defmethod cleavir-environment:eval (form env (dispatch-env NULL))
  "Evaluate the form in Clasp's top level environment"
  (cleavir-environment:eval form env *clasp-env*))

(defmethod cleavir-environment:eval (form env (dispatch-env clasp-global-environment))
  (cleavir-environment:cst-eval (cst:cst-from-expression form) env dispatch-env nil))

(defvar *use-cst-eval* t)

(defmethod cleavir-environment:cst-eval (cst env (dispatch-env clasp-global-environment)
                                         system)
  (declare (ignore system))
  ;; NOTE: We want the interpreter to deal with CSTs when we care about source info.
  ;; That is mainly when saving inline definitions.
  ;; At the moment, only simple-eval-cst and ast-interpret-cst actually deal with the CST,
  ;; so we want to be using one of those cases when saving definitions.
  (if *use-cst-eval*
      (simple-eval-cst cst env
                       (cond (core:*use-interpreter-for-eval*
                              (lambda (cst env)
                                (core:interpret (cst:raw cst) (cleavir-env->interpreter env))))
                             (*use-ast-interpreter* #'ast-interpret-cst)
                             (t (lambda (cst env)
                                  (cclasp-eval-with-env (cst:raw cst) env)))))
      (cond (core:*use-interpreter-for-eval*
             (core:interpret (cst:raw cst) (cleavir-env->interpreter env)))
            (*use-ast-interpreter* (ast-interpret-cst cst env))
            (t (cclasp-eval-with-env (cst:raw cst) env)))))

(defmethod cleavir-environment:cst-eval (cst env (dispatch-env null) system)
  (cleavir-environment:cst-eval cst env *clasp-env* system))

(defmethod cmp:compiler-condition-origin
    ((condition cleavir-conditions:program-condition))
  ;; FIXME: ignore-errors is a bit paranoid
  (let ((origin (cleavir-conditions:origin condition)))
    (ignore-errors (if (consp origin) (car origin) origin))))

(defun build-and-draw-ast (filename cst)
  (let ((ast (cleavir-cst-to-ast:cst-to-ast cst *clasp-env* *clasp-system*)))
    (cleavir-ast-graphviz:draw-ast ast filename)
    ast))

(defun build-and-draw-hir (filename cst)
  (let* ((ast (cleavir-cst-to-ast:cst-to-ast cst *clasp-env*))
	 (hir (cleavir-ast-to-hir:compile-toplevel ast)))
    (with-open-file (stream filename :direction :output)
      (cleavir-ir-graphviz:draw-flowchart hir stream))))

(defun quick-hir-pathname (&optional (file-name-modifier "hir"))
  (when *debug-cleavir*
    (make-pathname :type "dot" :defaults (cmp::quick-module-pathname file-name-modifier))))

(defun quick-draw-hir (hir &optional (file-name-modifier "hir"))
  (when *debug-cleavir*
    (let ((pn (make-pathname :type "dot" :defaults (cmp::quick-module-pathname file-name-modifier))))
      (with-open-file (stream pn :direction :output)
        (cleavir-ir-graphviz:draw-flowchart hir stream))
      pn)))

(defun draw-ast (&optional (ast *ast*) filename)
  (unless filename (setf filename (pathname (core:mkstemp "/tmp/ast"))))
  (let* ((filename (merge-pathnames filename))
         (dot-pathname (make-pathname :type "dot" :defaults (pathname filename)))
	 (pdf-pathname (make-pathname :type "pdf" :defaults dot-pathname)))
    (with-open-file (stream filename :direction :output)
      (cleavir-ast-graphviz:draw-ast ast dot-pathname))
    (ext:system (format nil "dot -Tpdf -o~a ~a" (namestring pdf-pathname) (namestring dot-pathname)))))

(defun draw-hir (&optional (hir *hir*) filename)
  (unless filename (setf filename (pathname (core:mkstemp "/tmp/hir"))))
  (let* ((filename (merge-pathnames filename))
         (dot-pathname (make-pathname :type "dot" :defaults (pathname filename)))
	 (pdf-pathname (make-pathname :type "pdf" :defaults dot-pathname)))
    (with-open-file (stream dot-pathname :direction :output)
      (cleavir-ir-graphviz:draw-flowchart hir stream))
    (ext:system (format nil "dot -Tpdf -o~a ~a" (namestring pdf-pathname) (namestring filename)))
    (ext:system (format nil "open -n ~a" (namestring pdf-pathname)))))

(defun draw-mir (&optional (mir *mir*) (filename "/tmp/mir.dot"))
  (with-open-file (stream filename :direction :output)
    (cleavir-ir-graphviz:draw-flowchart mir stream))
  (ext:system (format nil "dot -Teps -o/tmp/mir.eps ~a" filename))
  (ext:system "open -n /tmp/mir.eps"))

(defvar *hir-single-step* nil)
(defun hir-single-step (&optional (on t))
  (setq *hir-single-step* on))


(define-condition continue-hir (condition) ())

(defun do-continue-hir ()
  (format t "Continuing processing forms~%")
  (signal 'continue-hir))

(defun dump-hir (initial-instruction &optional (stream t))
  (let ((all-basic-blocks (cleavir-basic-blocks:basic-blocks initial-instruction))
        initials)
    (cleavir-ir:map-instructions
     (lambda (instr)
       (when (typep instr 'cleavir-ir:enter-instruction)
         (push instr initials))) initial-instruction)
    (dolist (procedure-initial initials)
      (format stream "====== Procedure: ~a~%" (cc-mir:describe-mir procedure-initial))
      (let ((basic-blocks (remove procedure-initial
                                  all-basic-blocks
                                  :test-not #'eq :key #'cleavir-basic-blocks:owner)))
        (dolist (bb basic-blocks)
          (with-accessors ((first cleavir-basic-blocks:first-instruction)
                           (last cleavir-basic-blocks:last-instruction)
                           (owner cleavir-basic-blocks:owner))
              bb
            (format stream "-------------------basic-block owner: ~a~%" 
                    (cc-mir:describe-mir owner))
            (loop for instruction = first
               then (first (cleavir-ir:successors instruction))
               until (eq instruction last)
               do (format stream "~a~%" (cc-mir:describe-mir instruction)))
            (format stream "~a~%" (cc-mir:describe-mir last))))))))

;;; These should be set up in Cleavir code
;;; Remove them once beach implements them
(defmethod cleavir-remove-useless-instructions:instruction-may-be-removed-p
    ((instruction cleavir-ir:rplaca-instruction))
  nil)

(defmethod cleavir-remove-useless-instructions:instruction-may-be-removed-p
    ((instruction cleavir-ir:rplacd-instruction))
  nil)

(defmethod cleavir-remove-useless-instructions:instruction-may-be-removed-p
    ((instruction cleavir-ir:set-symbol-value-instruction)) nil)



(setf (fdefinition 'cleavir-primop:call-with-variable-bound) 
            (fdefinition 'core:call-with-variable-bound))

#++
(defmacro cleavir-primop:call-with-variable-bound (symbol value thunk)
  `(clasp-cleavir-hir:multiple-value-foreign-call-instruction "call_with_variable_bound" ,symbol ,value ,thunk))
