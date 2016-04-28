(in-package :clasp-cleavir)



(defvar *current-function-entry-basic-block*)

(defmethod cleavir-generate-ast:convert-constant-to-immediate ((n integer) environment clasp)
  ;; convert fixnum into immediate but bignums return nil
  (core:create-tagged-immediate-value-or-nil n))

(defmethod cleavir-generate-ast:convert-constant-to-immediate ((n character) environment clasp)
  ;; convert character to an immediate
  (core:create-tagged-immediate-value-or-nil n))

(defmethod cleavir-generate-ast:convert-constant-to-immediate ((n float) environment clasp)
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
	(;; If it is not a constant variable, we can check whether
	 ;; macroexpanding it alters it.
	 (not (eq symbol (macroexpand-1 symbol)))
	 ;; Clearly, the symbol is defined as a symbol macro.
	 (make-instance 'cleavir-env:symbol-macro-info
	   :name symbol
	   :expansion (macroexpand-1 symbol)))
	(;; If it is neither a constant variable nor a symbol macro,
	 ;; it might be a special variable.  We can start by checking
	 ;; whether it is bound.
	 (boundp symbol)
	 ;; In that case, it is definitely special.
	 (make-instance 'cleavir-env:special-variable-info
	   :name symbol))
        (;; If it is not bound, it could still be special.
         ;; Use Clasp's core:specialp test to determine if it is special.
         ;; If so, assume that it is of type T
         (core:specialp symbol)
	 ;; It is a special variable.  However, we don't know its
	 ;; type, so we assume it is T, which is the default.
	 (make-instance 'cleavir-env:special-variable-info
	   :name symbol))
        #+(or)(;; If it is not bound, it could still be special.  If so, it
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
     (make-instance 'cleavir-env:global-function-info
                    :name function-name
                    :compiler-macro (compiler-macro-function function-name)
                    :inline (core:global-inline-status function-name)
                    :ast (core:cleavir-ast (fdefinition function-name))))
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

(defmethod cleavir-env:optimize-info ((environment clasp-global-environment))
  ;; The default values are all 3.
  (make-instance 'cleavir-env:optimize-info))

(defmethod cleavir-env:optimize-info ((environment NULL))
  ;; The default values are all 3.
  (cleavir-env:optimize-info *clasp-env*))


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


(setq cl:*macroexpand-hook* (lambda (macro-function macro-form environment)
			      (cond
				((typep environment 'core:environment)
				 (core:macroexpand-default macro-function macro-form environment))
				((or (null environment) (typep environment 'clasp-global-environment))
				 (core:macroexpand-default macro-function macro-form nil))
				((typep environment 'cleavir-environment::entry)
                                 (funcall macro-function macro-form environment))
                                (t
                                 (error "Add support to macroexpand of ~a using non-bclasp, non-top-level environment like: ~a" macro-form environment)))))


(defmethod cleavir-environment:eval (form env (dispatch-env clasp-global-environment))
  (cclasp-eval form env))

(defmethod cleavir-environment:eval (form env (dispatch-env NULL))
  "Evaluate the form in Clasp's top level environment"
  (cleavir-environment:eval form env *clasp-env*))

#+(or)
(defmacro ext::lambda-block (name (&rest lambda-list) &body body &environment env)
  `(lambda ,lambda-list (block ,(if (listp name) (second name) name) ,@body)))


(defun build-and-draw-ast (filename code)
  (let ((ast (cleavir-generate-ast:generate-ast code *clasp-env* *clasp-system*)))
    (cleavir-ast-graphviz:draw-ast ast filename)
    ast))

(defun build-and-draw-hir (filename code)
  (let* ((ast (cleavir-generate-ast:generate-ast code *clasp-env*))
	 (hir (cleavir-ast-to-hir:compile-toplevel ast)))
    (with-open-file (stream filename :direction :output)
      (cleavir-ir-graphviz:draw-flowchart hir stream))))


(defun draw-hir (&optional (hir *hir*) (filename #P"/tmp/hir.dot"))
  (setq filename (pathname filename))
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

(defun draw-ast (&optional (ast *ast*) (filename "/tmp/ast.dot"))
  (let* ((dot-pathname (pathname filename))
	 (png-pathname (make-pathname :type "png" :defaults dot-pathname)))
    (with-open-file (stream filename :direction :output)
      (cleavir-ast-graphviz:draw-ast ast filename))
    (ext:system (format nil "dot -Tpng -o~a ~a" (namestring png-pathname) (namestring dot-pathname)))
    (ext:system (format nil "open -n ~a" (namestring png-pathname)))))

(defparameter *code1* '(let ((x 1) (y 2)) (+ x y)))
(defparameter *code2* '(let ((x 10)) (if (> x 5) 1 2)))
#+(or)(defparameter *code3* 
        '(defun cl:macro-function (symbol &optional (environment nil environment-p))
          (cond
            ((typep environment 'core:environment)
             (core:macro-function symbol environment))
            (environment
             (cleavir-environment:macro-function symbol environment))
            (t (cleavir-environment:macro-function symbol *clasp-env*)))))


(defun generate-asts-for-clasp-source (start end)
  (let* ((parts (core::select-source-files end :first-file start :system core:*system-files*))
	 (pathnames (mapcar (lambda (part) (core:build-pathname part)) parts))
	 (eof (gensym)))
    (loop for file in pathnames
	 do (with-open-file (stream file :direction :input)
	      (loop for form = (read stream nil eof)
		 until (eq form eof)
		 do (format t "FORM: ~a~%" form)
		 do (let ((ast (cleavir-generate-ast:generate-ast form *clasp-env*)))
		      )
	      )))))



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

(defun ast-form (form)
  (let ((ast (cleavir-generate-ast:generate-ast form *clasp-env* *clasp-system*)))
    (setf *form* form
	  *ast* ast)
    (draw-ast ast)
    ast))

(defun hoisted-ast-form (form)
  (let* ((ast (cleavir-generate-ast:generate-ast form *clasp-env* *clasp-system*))
	 (hoisted (clasp-cleavir-ast:hoist-load-time-value ast)))
    (setf *form* form
	  *ast* hoisted)
    (draw-ast hoisted "/tmp/hoisted.dot")))


(defun hir-form (form)
  (let* ((ast (cleavir-generate-ast:generate-ast form *clasp-env* *clasp-system*))
         (hoisted-ast (clasp-cleavir-ast:hoist-load-time-value ast))
         (hir (cleavir-ast-to-hir:compile-toplevel hoisted-ast))
         (clasp-inst *clasp-system*))
;;    (cleavir-hir-transformations:hir-transformations hir clasp-inst nil nil)
    ;;    (cleavir-ir:hir-to-mir hir clasp-inst nil nil)
    (setf *form* form
          *ast* hoisted-ast
          *hir* hir)
    (draw-hir hir)
    hir))

(defun my-hir-transformations (initial-instruction implementation processor os)
  (cleavir-hir-transformations:type-inference initial-instruction)
  (cleavir-hir-transformations:eliminate-typeq initial-instruction)
  (cleavir-hir-transformations:eliminate-superfluous-temporaries initial-instruction)
  (cleavir-hir-transformations:process-captured-variables initial-instruction))

(defun mir-form (form)
  (let ((hir (hir-form form))
	(clasp-inst *clasp-system*))
    (my-hir-transformations hir clasp-inst nil nil)
    (cleavir-ir:hir-to-mir hir clasp-inst nil nil)
    (draw-mir hir)
    (setq *mir* hir)))


(defun hoisted-mir-form (form)
  (let* ((ast (cleavir-generate-ast:generate-ast form *clasp-env*))
	 (hoisted-ast (cleavir-ast-transformations:hoist-load-time-value ast))
	 (hir (cleavir-ast-to-hir:compile-toplevel hoisted-ast))
	 (clasp-inst *clasp-system*))
    (cleavir-hir-transformations:hir-transformations hir clasp-inst nil nil)
    (cleavir-ir:hir-to-mir hir clasp-inst nil nil)
    (setf *form* form
	  *ast* hoisted-ast
	  *hir* hir)
    (draw-hir hir)
    hir))


(defvar *form* nil)
(defvar *ast* nil)
(defvar *hir* nil)
(defvar *mir* nil)
(defun generate-hir-for-clasp-source (&optional (start :init) (end :all) skip-errors)
  (declare (special cleavir-generate-ast:*compiler*))
  (let* ((cleavir-generate-ast:*compiler* 'cl:compile-file)
	 (parts (core::select-source-files end :first-file start :system core:*system-files*))
	 (pathnames (mapcar (lambda (part) (core:build-pathname part)) parts))
	 (eof (gensym)))
    (loop for file in pathnames
       do (with-open-file (stream file :direction :input)
	    (loop for form = (read stream nil eof)
	       until (eq form eof)
	       do (format t "FORM: ~a~%" form)
	       do (if (and (eq form :pause-hir) (not *hir-single-step*))
		      (hir-tpl)
		      (let* ((ast (cleavir-generate-ast:generate-ast form *clasp-env*))
			     (hir (cleavir-ast-to-hir:compile-toplevel ast)))
			(setf *form* form
			      *ast* ast
			      *hir* hir)
			(if *hir-single-step*
			    (hir-tpl)))))))))



(defmacro with-ir-function ((lisp-function-name
			     &key (function-type cmp:+fn-prototype+ function-type-p)
			     (linkage 'llvm-sys:internal-linkage))
			       &rest body)
  (let ((fn-gs (gensym "FUNCTION-")))
    `(let ((,fn-gs (llvm-sys:function-create 
		   ,function-type
		   ',linkage
		    (cmp:jit-function-name ,lisp-function-name)
		    cmp:*the-module*)))
       (llvm-ir:set-personality-fn (cmp:irc-personality-function))
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

  
