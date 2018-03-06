(in-package :clasp-cleavir)

#+(or)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *echo-repl-read* t))

(defvar *save-compile-file-info* nil
  "When T causes AST's and HIR to be saved in *saved-compile-file-info* during compile-file")
(defvar *saved-compile-file-info* nil)
(defvar *llvm-metadata*)
(defvar *current-compile-file-source-pos-info* nil)

(defvar *current-function-entry-basic-block*)

(defmethod make-load-form ((object core:source-pos-info) &optional environment)
  `(core:make-cxx-object 'core:source-pos-info
                         :sfi (core:decode (core:make-cxx-object 'core:source-file-info)
                                           ',(core:encode (core:source-file-info
                                                           (core:source-pos-info-file-handle object))))
                         :fp ,(core:source-pos-info-filepos object)
                         :l ,(core:source-pos-info-lineno object)
                         :c ,(core:source-pos-info-column object)))

;;; Save top level forms for source tracking
(defmethod cleavir-generate-ast::convert-form :around (form info env system)
  (let ((core:*top-level-form-stack* (cons form core:*top-level-form-stack*)))
    (call-next-method)))

(defmethod cst:reconstruct :around (expression cst (client clasp) &key (default-source nil default-source-p))
  (call-next-method expression cst client :default-source (if default-source-p
                                                              default-source
                                                              (cst:source cst))))

(defmethod cleavir-generate-ast:convert-constant-to-immediate ((n integer) environment (system clasp))
  ;; convert fixnum into immediate but bignums return nil
  (let ((result (core:create-tagged-immediate-value-or-nil n)))
    (if result
        (make-instance 'immediate-literal :value n :tagged-value result)
        nil)))

(defmethod cleavir-generate-ast:convert-constant-to-immediate ((n character) environment (system clasp))
  ;; convert character to an immediate
  (let ((result (core:create-tagged-immediate-value-or-nil n)))
    (if result
        (make-instance 'immediate-literal :value n :tagged-value result)
        nil)))

(defmethod cleavir-generate-ast:convert-constant-to-immediate ((n float) environment (system clasp))
  ;; single-float's can be converted to immediates, anything else will return nil
  (let ((result (core:create-tagged-immediate-value-or-nil n)))
    (if result
        (make-instance 'immediate-literal :value n :tagged-value result)
        nil)))

;;; ------------------------------------------------------------
;;;
;;; cst-to-ast methods for convert-constant-to-immediate
(defmethod cleavir-cst-to-ast:convert-constant-to-immediate ((n integer) environment (system clasp))
  ;; convert fixnum into immediate but bignums return nil
  (let ((result (core:create-tagged-immediate-value-or-nil n)))
    (if result
        (make-instance 'immediate-literal :value n :tagged-value result)
        nil)))

(defmethod cleavir-cst-to-ast:convert-constant-to-immediate ((n character) environment (system clasp))
  ;; convert character to an immediate
  (let ((result (core:create-tagged-immediate-value-or-nil n)))
    (if result
        (make-instance 'immediate-literal :value n :tagged-value result)
        nil)))

(defmethod cleavir-cst-to-ast:convert-constant-to-immediate ((n float) environment (system clasp))
  ;; single-float's can be converted to immediates, anything else will return nil
  (let ((result (core:create-tagged-immediate-value-or-nil n)))
    (if result
        (make-instance 'immediate-literal :value n :tagged-value result)
        nil)))

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
	(;; If it is not a constant variable, we can check whether
	 ;; macroexpanding it alters it.
	 (not (eq symbol (macroexpand-1 symbol)))
	 ;; Clearly, the symbol is defined as a symbol macro.
	 (make-instance 'cleavir-env:symbol-macro-info
	   :name symbol
	   :expansion (macroexpand-1 symbol)))
        (;; If it is not bound, it could still be special.
         ;; Use Clasp's core:specialp test to determine if it is special.
         ;; If so, assume that it is of type T
         (core:specialp symbol)
	 ;; It is a special variable.  However, we don't know its
	 ;; type, so we assume it is T, which is the default.
	 (make-instance 'cleavir-env:special-variable-info
            :name symbol
            :global-p t))
        #+(or)( ;; If it is not bound, it could still be special.  If so, it
               ;; might have a restricted type on it.  It will then likely
               ;; fail to bind it to an object of some type that we
               ;; introduced, say our bogus environment.  It is not fool
               ;; proof because it could have the type STANDARD-OBJECT.  But
               ;; in the worst case, we will just fail to recognize it as a
               ;; special variable.
               (null (ignore-errors
                       (eval `(let ((,symbol (make-instance 'clasp-global-environment)))
                                t))))
               ;; It is a special variable.  However, we don't know its
               ;; type, so we assume it is T, which is the default.
               (make-instance 'cleavir-env:special-variable-info
                              :name symbol))
	#+(or)( ;; If the previous test fails, it could still be special
               ;; without any type restriction on it.  We can try to
               ;; determine whether this is the case by checking whether the
               ;; ordinary binding (using LET) of it is the same as the
               ;; dynamic binding of it.  This method might fail because the
               ;; type of the variable may be restricted to something we
               ;; don't know and that we didn't catch before, 
               (ignore-errors
                 (eval `(let ((,symbol 'a))
                          (progv '(,symbol) '(b) (eq ,symbol (symbol-value ',symbol))))))
               ;; It is a special variable.  However, we don't know its
               ;; type, so we assume it is T, which is the default.
               (make-instance 'cleavir-env:special-variable-info
                              :name symbol))
	(;; Otherwise, this symbol does not have any variable
	 ;; information associated with it.
	 t
	 ;; Return NIL as the protocol stipulates.
	 nil)))

(defmethod cleavir-env:variable-info ((environment null) symbol)
  (cleavir-env:variable-info *clasp-env* symbol))

(defun treat-as-special-operator-p (name)
  (cond
    ((cmp:treat-as-special-operator-p name) t)
    ((eq name 'cleavir-primop::call-with-variable-bound) nil)
    ((eq name 'core::vector-length) t)
    ((eq name 'core::%displacement) t)
    ((eq name 'core::%displaced-index-offset) t)
    ((eq name 'core::%array-total-size) t)
    ((eq name 'core::%array-rank) t)
    ((eq name 'core::%array-dimension) t)
    ((eq name 'core::bind-va-list) t)
    ((eq (symbol-package name) (find-package :cleavir-primop)) t)
    (t nil)))



(defmethod cleavir-env:function-info ((environment clasp-global-environment) function-name)
  (cond
    ( ;; If it is not the name of a macro, it might be the name of
     ;; a special operator.  This can be checked by calling
     ;; special-operator-p.
     (and (symbolp function-name) (treat-as-special-operator-p function-name))
     (make-instance 'cleavir-env:special-operator-info
		    :name function-name))
    ( ;; If the function name is the name of a macro, then
     ;; MACRO-FUNCTION returns something other than NIL.
     (and (symbolp function-name) (not (null (macro-function function-name))))
     ;; If so, we know it is a global macro.  It is also safe to
     ;; call COMPILER-MACRO-FUNCTION, because it returns NIL if
     ;; there is no compiler macro associated with this function
     ;; name.
     (make-instance 'cleavir-env:global-macro-info
		    :name function-name
		    :expander (macro-function function-name)
		    :compiler-macro (compiler-macro-function function-name)))
    ( ;; If it is neither the name of a macro nor the name of a
     ;; special operator, it might be the name of a global
     ;; function.  We can check this by calling FBOUNDP.  Now,
     ;; FBOUNDP returns true if it is the name of a macro or a
     ;; special operator as well, but we have already checked for
     ;; those cases.
     (fboundp function-name)
     ;; In that case, we return the relevant info
     ;; Check if we should inline the function
     (let* ((cleavir-ast (core:cleavir-ast (fdefinition function-name)))
            (inline-status (core:global-inline-status function-name)))
       (make-instance 'cleavir-env:global-function-info
                      :name function-name
                      :compiler-macro (compiler-macro-function function-name)
                      :inline inline-status
                      :ast cleavir-ast)))
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

(defvar *global-optimize*
  ;; initial value, changed by de/proclaim
  '((compilation-speed 1)
    (debug 1)
    (space 1)
    (speed 1)
    (safety 1)))

(eval-when (:compile-toplevel)
  (format t "abut to compute-policy~%"))

(defvar *global-policy*
  (cleavir-policy:compute-policy *global-optimize* *clasp-env*))

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

(defun type-expand-1 (type-specifier)
  (let (head tail)
    (etypecase type-specifier
      (class (return-from type-expand-1 (values type-specifier nil)))
      (symbol (setf head type-specifier tail nil))
      (cons (setf head (first type-specifier) tail (rest type-specifier))))
    (let ((def (get-sysprop head 'core::deftype-definition)))
      (if def
          (values (apply def tail) t)
          (values type-specifier nil)))))

(defmethod cleavir-env:type-expand ((environment clasp-global-environment) type-specifier)
  (loop with ever-expanded = nil
        do (multiple-value-bind (expansion expanded)
               (type-expand-1 type-specifier)
             (if expanded
                 (setf ever-expanded t type-specifier expansion)
                 (return (values type-specifier ever-expanded))))))
(defmethod cleavir-env:type-expand ((environment null) type-specifier)
  (cleavir-env:type-expand clasp-cleavir:*clasp-env* type-specifier))

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

(defmethod cleavir-environment:eval (form env (dispatch-env clasp-global-environment))
  (if core:*use-interpreter-for-eval*
      (core:interpret form (cleavir-env->interpreter env))
      (cclasp-eval form env)))

(defmethod cleavir-environment:eval (form env (dispatch-env NULL))
  "Evaluate the form in Clasp's top level environment"
  (cleavir-environment:eval form env *clasp-env*))

#+(or)
(defmacro ext::lambda-block (name (&rest lambda-list) &body body &environment env)
  `(lambda ,lambda-list (block ,(if (listp name) (second name) name) ,@body)))


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
    
(defun draw-hir (&optional (hir *hir*) filename)
  (unless filename (setf filename (pathname (core:mkstemp "/tmp/hir"))))
  (with-open-file (stream filename :direction :output)
    (cleavir-ir-graphviz:draw-flowchart hir stream))
  (let* ((png-pn (make-pathname :type "png" :defaults filename)))
    (ext:system (format nil "dot -Tpng -o~a ~a" (namestring png-pn) (namestring filename)))
    (ext:system (format nil "open -n ~a" (namestring png-pn)))))

(defun draw-mir (&optional (mir *mir*) (filename "/tmp/mir.dot"))
  (with-open-file (stream filename :direction :output)
    (cleavir-ir-graphviz:draw-flowchart mir stream))
  (ext:system (format nil "dot -Teps -o/tmp/mir.eps ~a" filename))
  (ext:system "open -n /tmp/mir.eps"))

(defun draw-ast (&optional (ast *ast*) filename)
  (unless filename (setf filename (pathname (core:mkstemp "/tmp/ast"))))
  (let* ((dot-pathname (pathname filename))
	 (png-pathname (make-pathname :type "png" :defaults dot-pathname)))
    (with-open-file (stream filename :direction :output)
      (cleavir-ast-graphviz:draw-ast ast filename))
    (ext:system (format nil "dot -Tpng -o~a ~a" (namestring png-pathname) (namestring dot-pathname)))
    (ext:system (format nil "open -n ~a" (namestring png-pathname)))))

(defvar *hir-single-step* nil)
(defun hir-single-step (&optional (on t))
  (setq *hir-single-step* on))


(define-condition continue-hir (condition) ())

(defun do-continue-hir ()
  (format t "Continuing processing forms~%")
  (signal 'continue-hir))


(defconstant *hir-commands*
  '(("HIR commands"
     ((:c :continue) do-continue-hir nil
      ":c(ontinue) Continue processing forms"
      "Stuff"))))




(defvar *form* nil)
(defvar *ast* nil)
(defvar *hir* nil)
(defvar *mir* nil)


(defmacro with-ir-function ((lisp-function-name
			     &key (function-type cmp:%fn-prototype% function-type-p)
			     (linkage 'llvm-sys:internal-linkage))
			       &rest body)
  (let ((fn-gs (gensym "FUNCTION-")))
    `(let ((,fn-gs (cmp:irc-function-create 
		   ,function-type
		   ',linkage
		    (cmp:jit-function-name ,lisp-function-name)
		    cmp:*the-module*)))
       (llvm-sys:set-personality-fn (cmp:irc-personality-function))
       ,@body
       ,fn-gs)))



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
				  :test-not #'eq :key #'third)))
	(dolist (bb basic-blocks)
	  (destructuring-bind (first last owner) bb
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
