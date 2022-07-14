(in-package :clasp-cleavir)

;;; FIXME: Move this earlier
;; changed by de/proclaim
(defvar *ftypes* (make-hash-table :test #'equal))

(defun global-ftype (name)
  (multiple-value-bind (value presentp) (gethash name *ftypes*)
    (if presentp
        value
        (load-time-value (cleavir-ctype:function-top *clasp-system*)))))

(defun (setf global-ftype) (type name)
  (setf (gethash name *ftypes*)
        (env:parse-type-specifier type *clasp-env* *clasp-system*)))

(defvar *vtypes* (make-hash-table :test #'eq))
(defun global-type (name)
  (multiple-value-bind (value presentp) (gethash name *vtypes*)
    (if presentp
        value
        (load-time-value (cleavir-ctype:top *clasp-system*)))))
(defun (setf global-type) (type name)
  (setf (gethash name *vtypes*)
        (env:parse-type-specifier type *clasp-env* *clasp-system*)))

(defmethod env:variable-info ((system clasp)
                              (environment clasp-global-environment) symbol)
  (core:stack-monitor)
  (cond (;; We can check whether this symbol names a constant variable
	 ;; by checking the return value of CONSTANTP. 
	 (constantp symbol)
	 ;; If it is a constant variable, we can get its value by
	 ;; calling SYMBOL-VALUE on it.
	 (make-instance 'env:constant-variable-info
	   :name symbol
	   :value (symbol-value symbol)))
        (;; Use Clasp's core:specialp test to determine if it is special.
         ;; Note that in Clasp constants are also special (FIXME?) so we
         ;; have to do this test after checking for constantness.
         (ext:specialp symbol)
	 (make-instance 'env:special-variable-info
           :name symbol
           :global-p t
           :type (global-type symbol)))
	(;; Maybe it's a symbol macro.
	 (ext:symbol-macro symbol)
	 (make-instance 'env:symbol-macro-info
	   :name symbol
	   :expansion (macroexpand-1 symbol)
           :type (global-type symbol)))
	(;; Otherwise, this symbol does not have any variable
	 ;; information associated with it.
	 t
	 ;; Return NIL as the protocol stipulates.
	 nil)))

(defmethod env:variable-info ((system clasp) (environment null) symbol)
  (env:variable-info system *clasp-env* symbol))

(defvar *fn-flags* (make-hash-table :test #'equal))
(defvar *fn-transforms* (make-hash-table :test #'equal))
(defvar *derivers* (make-hash-table :test #'equal))
(defvar *folds* (make-hash-table :test #'equal))

(macrolet ((define-function-flags (name &rest attributes)
             `(setf (gethash ',name *fn-flags*)
                    (cleavir-attributes:make-flags ,@attributes))))
  ;; FIXME: Can't do DX-call for many things like APPLY, FUNCALL, etc.
  ;; because we don't distinguish between *which* functional argument
  ;; is DX.
  (define-function-flags apply :dyn-call)
  (define-function-flags funcall :dyn-call)
  (define-function-flags every :dyn-call :dx-call)
  (define-function-flags some :dyn-call :dx-call)
  (define-function-flags notevery :dyn-call :dx-call)
  (define-function-flags notany :dyn-call :dx-call)
  (define-function-flags sublis :dyn-call :dx-call)
  (define-function-flags nsublis :dyn-call :dx-call)
  (define-function-flags subst-if :dyn-call :dx-call)
  (define-function-flags subst-if-not :dyn-call :dx-call)
  (define-function-flags nsubst-if :dyn-call :dx-call)
  (define-function-flags nsubst-if-not :dyn-call :dx-call)
  (define-function-flags member :dyn-call :dx-call)
  (define-function-flags member-if :dyn-call :dx-call)
  (define-function-flags member-if-not :dyn-call :dx-call)
  (define-function-flags mapc :dyn-call :dx-call)
  (define-function-flags mapcar :dyn-call :dx-call)
  (define-function-flags mapcan :dyn-call :dx-call)
  (define-function-flags mapl :dyn-call :dx-call)
  (define-function-flags maplist :dyn-call :dx-call)
  (define-function-flags mapcon :dyn-call :dx-call)
  (define-function-flags assoc :dyn-call :dx-call)
  (define-function-flags assoc-if :dyn-call :dx-call)
  (define-function-flags assoc-if-not :dyn-call :dx-call)
  (define-function-flags rassoc :dyn-call :dx-call)
  (define-function-flags rassoc-if :dyn-call :dx-call)
  (define-function-flags rassoc-if-not :dyn-call :dx-call)
  (define-function-flags intersection :dyn-call :dx-call)
  (define-function-flags nintersection :dyn-call :dx-call)
  (define-function-flags adjoin :dyn-call :dx-call)
  (define-function-flags set-difference :dyn-call :dx-call)
  (define-function-flags nset-difference :dyn-call :dx-call)
  (define-function-flags set-exclusive-or :dyn-call :dx-call)
  (define-function-flags nset-exclusive-or :dyn-call :dx-call)
  (define-function-flags subsetp :dyn-call :dx-call)
  (define-function-flags union :dyn-call :dx-call)
  (define-function-flags nunion :dyn-call :dx-call)
  (define-function-flags map :dyn-call :dx-call)
  (define-function-flags map-into :dyn-call :dx-call)
  (define-function-flags merge :dyn-call :dx-call)
  (define-function-flags ast:map-ast-depth-first-preorder :dyn-call
    :dx-call)
  (define-function-flags bir:map-iblocks :dyn-call :dx-call)
  (define-function-flags bir:map-iblock-instructions :dyn-call :dx-call)
  ;; Can't do DYN-CALL for most sequence functions, as ext sequence
  ;; functions can do arbitrary things.
  (define-function-flags core:progv-function :dx-call)
  (define-function-flags core:funwind-protect :dx-call)
  (define-function-flags maphash :dx-call)
  (define-function-flags remove :dx-call)
  (define-function-flags remove-if :dx-call)
  (define-function-flags remove-if-not :dx-call)
  (define-function-flags delete :dx-call)
  (define-function-flags delete-if :dx-call)
  (define-function-flags delete-if-not :dx-call)
  (define-function-flags reduce :dx-call)
  (define-function-flags remove-duplicates :dx-call)
  (define-function-flags delete-duplicates :dx-call)
  (define-function-flags substitute :dx-call)
  (define-function-flags substitute-if :dx-call)
  (define-function-flags substitute-if-not :dx-call)
  (define-function-flags nsubstitute :dx-call)
  (define-function-flags nsubstitute-if :dx-call)
  (define-function-flags nsubstitute-if-not :dx-call)
  (define-function-flags count :dx-call)
  (define-function-flags count-if :dx-call)
  (define-function-flags count-if-not :dx-call)
  (define-function-flags find :dx-call)
  (define-function-flags find-if :dx-call)
  (define-function-flags find-if-not :dx-call)
  (define-function-flags position :dx-call)
  (define-function-flags position-if :dx-call)
  (define-function-flags position-if-not :dx-call)
  (define-function-flags mismatch :dx-call)
  (define-function-flags search :dx-call)
  (define-function-flags sort :dx-call)
  (define-function-flags stable-sort :dx-call)

  (define-function-flags core::map-into-sequence :dyn-call :dx-call)
  (define-function-flags core::map-into-sequence/1 :dyn-call :dx-call)
  (define-function-flags core::map-for-effect :dyn-call :dx-call)
  (define-function-flags core::map-for-effect/1 :dyn-call :dx-call)
  (define-function-flags core::map-to-list :dyn-call :dx-call)
  (define-function-flags core::map-to-list/1 :dyn-call :dx-call)
  (define-function-flags core::every/1 :dyn-call :dx-call)
  (define-function-flags core::some/1 :dyn-call :dx-call))

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
    ((eq name 'core::atomic-vref) t)
    ((eq name 'core::atomic-vset) t)
    ((eq name 'core::vcas) t)
    ((eq name 'core::bind-vaslist) t)
    ((eq name 'core::primop) t)
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
  (declare (ignore environment))
  (values `(allocate-instance ,(class-of ast))
          `(initialize-instance
            ,ast
            ,@(loop for (keyword reader)
                      in (cleavir-io:save-info ast)
                    for value = (funcall reader ast)
                    collect `(quote ,keyword)
                    collect `(quote ,value)))))

(defmethod make-load-form ((cst cst:cst) &optional environment)
  (make-load-form-saving-slots cst :environment environment))

(defmethod env:function-info ((sys clasp)
                              (environment clasp-global-environment)
                              function-name)
  (cond
    ((and (symbolp function-name) (treat-as-special-operator-p function-name))
     (make-instance 'env:special-operator-info
       :name function-name))
    ;; If the function name is the name of a macro, then
    ;; MACRO-FUNCTION returns something other than NIL.
    ((and (symbolp function-name) (not (null (macro-function function-name))))
     (make-instance 'env:global-macro-info ; we're global, so the macro must be global.
       :name function-name
       :inline (core:global-inline-status function-name)
       :expander (macro-function function-name)
       :compiler-macro (compiler-macro-function function-name)))
    ((fboundp function-name)
     (let* ((cleavir-ast (inline-ast function-name))
            (inline-status (core:global-inline-status function-name))
            (flags (gethash function-name *fn-flags*))
            (transforms (gethash function-name *fn-transforms*))
            (derivers (gethash function-name *derivers*))
            (folds (gethash function-name *folds*))
            (vaslistablep (cc-vaslist:vaslistablep function-name))
            (attributes (if (or flags transforms folds derivers)
                            (make-instance 'cleavir-attributes:attributes
                              :flags (or flags (cleavir-attributes:make-flags))
                              :identities (if (or transforms folds
                                                  derivers vaslistablep)
                                              (list function-name)
                                              nil))
                            (cleavir-attributes:default-attributes))))
       (make-instance 'env:global-function-info
         :name function-name
         :type (global-ftype function-name)
         :compiler-macro (compiler-macro-function function-name)
         :inline inline-status
         :ast cleavir-ast
         :attributes attributes)))
    ;; A top-level defun for the function has been seen.
    ;; The expansion calls cmp::register-global-function-def at compile time,
    ;; which is hooked up so that among other things this works.
    ((cmp:known-function-p function-name)
     (make-instance 'env:global-function-info
       :name function-name
       :type (global-ftype function-name)
       :compiler-macro (compiler-macro-function function-name)
       :inline (core:global-inline-status function-name)
       :ast (inline-ast function-name)))
    ( ;; If it is neither of the cases above, then this name does
     ;; not have any function-info associated with it.
     t
     ;; Return NIL as the protocol stipulates.
     nil)))

;;; The toplevel shell may have a bclasp environment - ignore it
(defmethod env:symbol-macro-expansion (symbol (environment core:value-frame))
  (cleavir-environment:symbol-macro-expansion symbol *clasp-env*))

(defmethod env:function-info ((sys clasp) (environment null) symbol)
  (env:function-info sys *clasp-env* symbol))

(defmethod env:function-info ((sys clasp)
                              (environment core:value-environment) symbol)
  (env:function-info sys (core:get-parent-environment environment) symbol))

(defmethod env:function-info ((sys clasp)
                              (environment core:value-frame) symbol)
  (env:function-info sys (core:get-parent-environment environment) symbol))

(defmethod env:variable-info ((sys clasp)
                              (environment core:value-frame) symbol)
  (env:variable-info sys (core:get-parent-environment environment) symbol))

(defmethod env:variable-info ((sys clasp)
                              (environment core:value-environment) symbol)
  (env:variable-info sys (core:get-parent-environment environment) symbol))

(defmethod env:declarations ((environment null))
  (env:declarations *clasp-env*))

(defmethod env:declarations ((environment core:value-environment))
  (env:declarations (core:get-parent-environment environment)))

;;; TODO: Handle (declaim (declaration ...))
(defmethod env:declarations
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
  '#.(policy:compute-policy *global-optimize* *clasp-env*))

(defmethod env:optimize-info ((environment clasp-global-environment))
  ;; The default values are all 3.
  (make-instance 'env:optimize-info
    :optimize *global-optimize*
    :policy *global-policy*))

(defmethod env:optimize-info ((environment NULL))
  (env:optimize-info *clasp-env*))

(defmethod env:optimize-info ((environment core:value-environment))
  (env:optimize-info (core:get-parent-environment environment)))


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
  (let ((info (env:variable-info *clasp-system* environment symbol)))
    (if (typep info 'env:symbol-macro-info)
        (let ((expansion (env:expansion info)))
          (lambda (form env)
            (declare (ignore form env)
                     (core:lambda-name cleavir-symbol-macro-function))
            expansion))
        nil)))

;;; Used by core:operator-shadowed-p
(defun core:cleavir-operator-shadowed-p (name environment)
  (typep (env:function-info *clasp-system* environment name)
         '(or env:local-function-info
           env:local-macro-info)))

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

(defmethod env:type-expand ((environment clasp-global-environment) type-specifier)
  ;; BEWARE: bclasp is really bad at unwinding, and mvb creates a
  ;; lambda, so we write this loop in a way that avoids RETURN. cclasp
  ;; will contify this and produce more efficient code anyway.
  (labels ((expand (type-specifier ever-expanded)
             (multiple-value-bind (expansion expanded)
                 (type-expand-1 type-specifier environment)
               (if expanded
                   (expand expansion t)
                   (values type-specifier ever-expanded)))))
    (expand type-specifier nil)))

(defmethod env:type-expand ((environment null) type-specifier)
  (env:type-expand clasp-cleavir:*clasp-env* type-specifier))

;;; Needed because the default method ends up with classes,
;;; and that causes bootstrapping issues.
(defmethod env:find-class (name environment (system clasp) &optional errorp)
  (declare (ignore environment errorp))
  name)

(defun cleavir-env->interpreter (env)
  ;; Convert a cleavir ENTRY (or null) into an environment clasp's interpreter can use.
  ;; Only for compile time environments, so it's symbol macros, macros, and declarations.
  ;; The interpreter doesn't use declarations besides SPECIAL.
  (etypecase env
    (clasp-global-environment nil)
    (null env)
    (env:special-variable
     (core:make-value-environment-for-locally-special-entries
      (list (env:name env))
      (cleavir-env->interpreter (env::next env))))
    (env:symbol-macro
     (let ((result (core:make-symbol-macrolet-environment
                    (cleavir-env->interpreter (env::next env)))))
       (core:add-symbol-macro
        result (env:name env)
        (constantly (env:expansion env)))
       result))
    (env:macro
     (let ((result (core:make-macrolet-environment
                    (cleavir-env->interpreter (env::next env)))))
       (core:add-macro result (env:name env)
                       (env:expander env))
       result))
    (env::entry (cleavir-env->interpreter (env::next env)))))

(defvar *use-ast-interpreter* t)

(defmethod cleavir-environment:eval (form env (dispatch-env NULL))
  "Evaluate the form in Clasp's top level environment"
  (cleavir-environment:eval form env *clasp-env*))

(defmethod cleavir-environment:eval (form env (dispatch-env clasp-global-environment))
  (cleavir-environment:cst-eval (cst:cst-from-expression form) env dispatch-env nil))

(defvar *use-cst-eval* t)

(defun wrap-cst (cst)
  (cst:quasiquote (cst:source cst)
                  (lambda () (cst:unquote cst))))

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
                                  (funcall (bir-compile-cst-in-env (wrap-cst cst) env))))))
      (cond (core:*use-interpreter-for-eval*
             (core:interpret (cst:raw cst) (cleavir-env->interpreter env)))
            (*use-ast-interpreter* (ast-interpret-cst cst env))
            (t (funcall (bir-compile-cst-in-env (wrap-cst cst) env))))))

(defmethod cleavir-environment:cst-eval (cst env (dispatch-env null) system)
  (cleavir-environment:cst-eval cst env *clasp-env* system))

(defmethod cmp:compiler-condition-origin
    ((condition cleavir-conditions:program-condition))
  ;; FIXME: ignore-errors is a bit paranoid
  (let ((source (origin-source (cleavir-conditions:origin condition))))
    (ignore-errors (if (consp source) (car source) source))))
