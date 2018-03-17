(in-package :cc-generate-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Expand local-macro-info definitions
;;; use the macro WITH-EARLY-ACCESSORS - it creates
;;; macrolet macros that substitute for functions - this confuses cleavir
;;; Treat LOCAL-MACRO-INFO function names as globals
(defmethod cleavir-generate-ast:convert-function
    ((info cleavir-env:local-macro-info) env (system clasp-cleavir:clasp))
  (format t "In kernel/cleavir/convert-special.lisp -- cleavir-generate-ast:convert-function for: ~a~%" info)
  (cleavir-generate-ast:convert-global-function info (cleavir-env:global-environment env) system))



#+(or)(defmethod cleavir-generate-ast::convert-special
	  ((symbol (eql 'unwind-protect)) form env)
	(let* ((ast (cc-ast:make-unwind-protect-ast nil nil))
	       (new-env (cc-env:add-unwind-protect env ast))
	       (cleanup-forms (cleavir-generate-ast::convert-sequence (cddr form) env)))
	  (setf (cc-ast:cleanup-ast ast)
		(cleavir-ast:make-progn-ast cleanup-forms))
	  (let ((protected-form (cleavir-generate-ast::convert (cadr form) new-env)))
	    (setf (cc-ast:protected-ast ast) protected-form)
	    ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Define a "macro" for the Cleavir compiler.
;;; This shouldn't be necessary, but I'd like to
;;; avoid overwriting any bclasp definitions.
;;; Don't use &environment or &whole.

(defmacro defsmacro (name lambda-list &body body)
  (let ((head (gensym "HEAD"))
        (form (gensym "FORM"))
        (environment (gensym "ENVIRONMENT"))
        (system (gensym "SYSTEM")))
    `(defmethod cleavir-generate-ast:convert-special
         ((,head (eql ',name)) ,form ,environment (,system clasp-cleavir:clasp))
       (cleavir-generate-ast:convert
        (destructuring-bind ,lambda-list (rest ,form) ,@body)
        ,environment ,system))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MULTIPLE-VALUE-CALL.
;;;
;;; In the case where there is one multiple-value form this gets converted
;;; into a multiple-value-call-ast.  In the general case with multiple forms
;;; it gets converted into a function call to CORE:MULTIPLE-VALUE-FUNCALL.
;;;
(defsmacro multiple-value-call (function-form &rest forms)
  ;;; Technically we could convert the 0-forms case to FUNCALL, but it's
  ;;; probably not a big deal. (In practice, almost all MULTIPLE-VALUE-CALLs
  ;;; result from MULTIPLE-VALUE-BIND, which uses only have one argument form.)
  (if (eql (length forms) 1)
      `(cleavir-primop:multiple-value-call (core::coerce-fdesignator ,function-form) ,@forms)
      `(core:multiple-value-funcall
        (core::coerce-fdesignator ,function-form)
        ,@(mapcar (lambda (x) `#'(lambda () (progn ,x))) forms))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MULTIPLE-VALUE-PROG1
;;;
;;; This is converted into a call to core:multiple-value-prog1-function func1 func2
;;; Func1 is evaluated and the multiple values are saved and then func2 is evaluated
;;; and the multiple values returned from func1 are restored
;;;
(defsmacro multiple-value-prog1 (first-form &rest forms)
  (if (null forms) ; skip it. This can come up in generated code, e.g. method combinations
      first-form
      `(core:multiple-value-prog1-function (lambda () (progn ,first-form)) (lambda () (progn ,@forms)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; convert-special-binding
;;;
;;; Convert a special binding to a BIND-AST
;;;
;;; Use the primop
#+(or)(defmethod cleavir-generate-ast::convert-special-binding
    (variable value-ast next-ast global-env (system clasp-cleavir:clasp))
  (cleavir-ast:make-bind-ast
   (cleavir-ast:make-load-time-value-ast `',variable t)
   value-ast
   next-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CORE:DEBUG-MESSAGE
;;;
;;; This is converted into a call to print a message
;;;
(defmethod cleavir-generate-ast::convert-special
    ((symbol (eql 'core:debug-message)) form environment (system clasp-cleavir:clasp))
  (make-instance 'clasp-cleavir-ast:debug-message-ast :debug-message (cadr form)))

(defmethod cleavir-generate-ast::check-special-form-syntax ((head (eql 'core:debug-message)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CORE:multiple-value-foreign-CALL
;;;
;;; This is converted into an intrinsic call
;;;
(defmethod cleavir-generate-ast::convert-special
    ((symbol (eql 'core:multiple-value-foreign-call)) form environment (system clasp-cleavir:clasp))
  (check-type (second form) string)
  (make-instance 'clasp-cleavir-ast:multiple-value-foreign-call-ast
                 :function-name (second form)
                 :argument-asts (cleavir-generate-ast:convert-sequence (cddr form) environment system)))

(defmethod cleavir-generate-ast::check-special-form-syntax ((head (eql 'core:multiple-value-foreign-call)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CORE:FOREIGN-call
;;;
;;; This is converted into a pointer call
;;;
(defmethod cleavir-generate-ast::convert-special
    ((symbol (eql 'core:foreign-call)) form environment (system clasp-cleavir:clasp))
                                        ;  (format t "convert-special form: ~a~%"  form)
  (check-type (second form) list)
  (check-type (third form) string)
  (make-instance 'clasp-cleavir-ast:foreign-call-ast
                 :foreign-types (second form)
                 :function-name (third form)
                 :argument-asts (cleavir-generate-ast:convert-sequence (cdddr form) environment system)))

(defmethod cleavir-generate-ast::check-special-form-syntax ((head (eql 'core:foreign-call)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 2 nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CORE:foreign-call-pointer
;;;
;;; This is converted into a pointer call
;;;
(defmethod cleavir-generate-ast::convert-special
    ((symbol (eql 'core:foreign-call-pointer)) form environment (system clasp-cleavir:clasp))
;  (format t "convert-special form: ~a~%"  form)
  (check-type (second form) list)
  (make-instance 'clasp-cleavir-ast:foreign-call-pointer-ast
                 :foreign-types (second form)
                 :argument-asts (cleavir-generate-ast:convert-sequence (cddr form) environment system)))

(defmethod cleavir-generate-ast::check-special-form-syntax ((head (eql 'core:foreign-call-pointer)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 2 nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CORE:VECTOR-LENGTH
;;;
;;; Gets the length of a vector like CL:LENGTH.
;;;
(defmethod cleavir-generate-ast::convert-special
    ((symbol (eql 'core::vector-length)) form environment (system clasp-cleavir:clasp))
  (destructuring-bind (vector) (rest form)
    (make-instance 'clasp-cleavir-ast::vector-length-ast
                   :vector (cleavir-generate-ast:convert vector environment system))))

(defmethod cleavir-generate-ast:check-special-form-syntax ((head (eql 'core::vector-length)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CORE::%DISPLACEMENT
;;;
;;; Gets the underlying (simple-array * (*)) for
;;; any MDArray, including SimpleMDArrays.
;;;
(defmethod cleavir-generate-ast:convert-special
    ((symbol (eql 'core::%displacement)) form environment (system clasp-cleavir:clasp))
  (destructuring-bind (mdarray) (rest form)
    (make-instance 'clasp-cleavir-ast::displacement-ast
                   :mdarray (cleavir-generate-ast:convert mdarray environment system))))

(defmethod cleavir-generate-ast:check-special-form-syntax ((head (eql 'core::%displacement)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CORE::%DISPLACED-INDEX-OFFSET
;;;
;;; Gets the underlying DIO for
;;; any MDArray, including SimpleMDArrays.
;;;
(defmethod cleavir-generate-ast:convert-special
    ((symbol (eql 'core::%displaced-index-offset)) form environment (system clasp-cleavir:clasp))
  (destructuring-bind (mdarray) (rest form)
    (make-instance 'clasp-cleavir-ast::displaced-index-offset-ast
                   :mdarray (cleavir-generate-ast:convert mdarray environment system))))

(defmethod cleavir-generate-ast:check-special-form-syntax ((head (eql 'core::%displaced-index-offset)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CORE::%ARRAY-TOTAL-SIZE
;;;
;;; Gets the array total size for
;;; any MDArray, including SimpleMDArrays.
;;;
(defmethod cleavir-generate-ast:convert-special
    ((symbol (eql 'core::%array-total-size)) form environment (system clasp-cleavir:clasp))
  (destructuring-bind (mdarray) (rest form)
    (make-instance 'clasp-cleavir-ast::array-total-size-ast
                   :mdarray (cleavir-generate-ast:convert mdarray environment system))))

(defmethod cleavir-generate-ast:check-special-form-syntax ((head (eql 'core::%array-total-size)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CORE::%ARRAY-RANK
;;;
;;; ARRAY-RANK for MDArrays
(defmethod cleavir-generate-ast:convert-special
    ((symbol (eql 'core::%array-rank)) form environment (system clasp-cleavir:clasp))
  (destructuring-bind (mdarray) (rest form)
    (make-instance 'clasp-cleavir-ast::array-rank-ast
                   :mdarray (cleavir-generate-ast:convert mdarray environment system))))

(defmethod cleavir-generate-ast:check-special-form-syntax ((head (eql 'core::%array-rank)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CORE::%ARRAY-DIMENSION
;;;
;;; ARRAY-DIMENSION for MDArrays
(defmethod cleavir-generate-ast:convert-special
    ((symbol (eql 'core::%array-dimension)) form environment (system clasp-cleavir:clasp))
  (destructuring-bind (mdarray axis) (rest form)
    (make-instance 'clasp-cleavir-ast::array-dimension-ast
                   :mdarray (cleavir-generate-ast:convert mdarray environment system)
                   :axis (cleavir-generate-ast:convert axis environment system))))

(defmethod cleavir-generate-ast:check-special-form-syntax ((head (eql 'core::%array-dimension)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 2 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CATCH
;;;
;;; Convert catch into a call
(defsmacro catch (tag &body body)
  `(core:catch-function ,tag (lambda () (declare (core:lambda-name catch-lambda)) (progn ,@body))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting THROW
;;;
;;; Convert throw into a call
(defmethod cleavir-generate-ast::convert-special
    ((symbol (eql 'cl:throw)) form environment (system clasp-cleavir:clasp))
  (destructuring-bind (tag result-form) 
      (rest form)
    ;; If I don't use a throw-ast node use the following
    #+(or)
    (cleavir-generate-ast::convert `(core:throw-function ,tag ,result-form) environment system)
    ;; If I decide to go with a throw-ast node use the following
    (clasp-cleavir-ast:make-throw-ast
     (cleavir-generate-ast::convert tag environment system)
     (cleavir-generate-ast::convert result-form environment system))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CL:PROGV
;;;
;;; Convert this into a function call
(defsmacro progv (symbols values &body forms)
  `(core:progv-function ,symbols ,values (lambda () (progn ,@forms))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CORE::BIND-VA-LIST
;;;

(defmethod cleavir-generate-ast:convert-special
    ((symbol (eql 'core::bind-va-list)) form environment (system clasp-cleavir:clasp))
  (cleavir-generate-ast:db origin (op lambda-list va-list-form . body) form
    (declare (ignore op))
    (let* ((parsed-lambda-list
             (cleavir-code-utilities:parse-ordinary-lambda-list lambda-list))
           (required (cleavir-code-utilities:required parsed-lambda-list)))
      (multiple-value-bind (declarations documentation forms)
          (cleavir-code-utilities:separate-function-body body)
        (declare (ignore documentation))
        (let ((canonicalized-dspecs
                (cleavir-code-utilities:canonicalize-declaration-specifiers
                 (reduce #'append (mapcar #'cdr declarations))
                 (cleavir-env:declarations environment))))
          (multiple-value-bind (idspecs rdspecs)
              (cleavir-generate-ast::itemize-declaration-specifiers
               (cleavir-generate-ast::itemize-lambda-list parsed-lambda-list)
               canonicalized-dspecs)
            (multiple-value-bind (ast lexical-lambda-list)
                (cleavir-generate-ast::process-required
                 required
                 parsed-lambda-list
                 idspecs
                 (cleavir-generate-ast::make-body rdspecs forms nil nil)
                 environment
                 system)
              (cc-ast:make-bind-va-list-ast
                lexical-lambda-list
                (cleavir-generate-ast::convert va-list-form environment system)
                ast))))))))

(defmethod cleavir-generate-ast:check-special-form-syntax
    ((head (eql 'core::bind-va-list)) form)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 2 nil)
  (assert (cleavir-code-utilities:proper-list-p (second form))))


(defmethod cleavir-generate-ast:convert-global-function (info global-env (system clasp-cleavir:clasp))
  (declare (ignore global-env))
  (let ((name (cleavir-env:name info)))
    (cond 
      ((and (consp name) (eq (car name) 'cl:setf))
       (clasp-cleavir-ast:make-setf-fdefinition-ast
	(cleavir-ast:make-load-time-value-ast `',(cadr name) t)))
      ((consp name)
       (error "Illegal name for function - must be (setf xxx)"))
      (t
       (cleavir-ast:make-fdefinition-ast
	(cleavir-ast:make-load-time-value-ast `',name t))))))
