(defpackage #:trucler-native-clasp
  (:use #:cl)
  (:import-from #:trucler-native #:client)
  (:export #:client))

(in-package #:trucler-native-clasp)

(defmethod trucler:describe-variable
    ((client client) (env clasp-cleavir:clasp-global-environment)
     symbol)
  (cond ((constantp symbol)
         (make-instance 'trucler:constant-variable-description
           :name symbol :value (symbol-value symbol)))
        ((ext:specialp symbol)
         (make-instance 'trucler:global-special-variable-description
           :name symbol :type (clasp-cleavir::global-type symbol)))
        ((ext:symbol-macro symbol)
         (make-instance 'trucler:global-symbol-macro-description
           :name symbol :expansion (macroexpand-1 symbol)
           :type (clasp-cleavir::global-type symbol)))
        (t nil)))

(defmethod trucler:describe-variable ((client client) (env null) symbol)
  (trucler:describe-variable client clasp-cleavir:*clasp-env* symbol))

(defmethod trucler:describe-variable
    ((client client) (env cmp:lexenv) symbol)
  (let ((info (cmp:var-info symbol env)))
    (etypecase info
      (null
       ;; Not locally bound: Check the global environment.
       (trucler:describe-variable
        client (cmp:lexenv/global env) symbol))
      (cmp:lexical-var-info
       ;; This will probably not go well - cleavir expects an identity, etc.
       (make-instance 'trucler:lexical-variable-description
         :name symbol :identity nil))
      (cmp:special-var-info
       (make-instance 'trucler:local-special-variable-description
         :name symbol))
      (cmp:symbol-macro-var-info
       (make-instance 'trucler:local-symbol-macro-description
         :name symbol
         :expansion (funcall (cmp:symbol-macro-var-info/expander info)
                             symbol env)))
      (cmp:constant-var-info
       (make-instance 'trucler:constant-variable-description
         :name symbol
         :value (ext:constant-form-value symbol env))))))

(defmethod trucler:describe-function
    ((client client)
     (environment clasp-cleavir:clasp-global-environment)
     function-name)
  (cond
    ((and (symbolp function-name)
          (clasp-cleavir::treat-as-special-operator-p function-name))
     (make-instance 'trucler:special-operator-description
       :name function-name))
    ;; If the function name is the name of a macro, then
    ;; MACRO-FUNCTION returns something other than NIL.
    ((and (symbolp function-name) (not (null (macro-function function-name))))
     ;; we're global, so the macro must be global.
     (make-instance 'trucler:global-macro-description
       :name function-name
       ;;:inline (clasp-cleavir::global-inline-status function-name)
       :expander (macro-function function-name)
       :compiler-macro (compiler-macro-function function-name)))
    ((fboundp function-name)
     (let* ((cleavir-ast (clasp-cleavir:inline-ast function-name))
            (inline-status (clasp-cleavir::global-inline-status function-name))
            (flags (gethash function-name clasp-cleavir::*fn-flags*))
            (transforms (gethash function-name clasp-cleavir::*fn-transforms*))
            (derivers (gethash function-name clasp-cleavir::*derivers*))
            (folds (gethash function-name clasp-cleavir::*folds*))
            (vaslistablep (cc-vaslist:vaslistablep function-name))
            (attributes (if (or flags transforms folds derivers)
                            (make-instance 'cleavir-attributes:attributes
                              :flags (or flags (cleavir-attributes:make-flags))
                              :identities (if (or transforms folds
                                                  derivers vaslistablep)
                                              (list function-name)
                                              nil))
                            (cleavir-attributes:default-attributes))))
       (declare (ignore attributes)) ; for now
       (make-instance 'trucler:global-function-description
         :name function-name
         :type (clasp-cleavir::global-ftype function-name)
         :compiler-macro (compiler-macro-function function-name)
         :inline inline-status
         :inline-data cleavir-ast
         #+(or):attributes #+(or) attributes)))
    ;; A top-level defun for the function has been seen.
    ;; The expansion calls cmp::register-global-function-def at compile time,
    ;; which is hooked up so that among other things this works.
    ((cmp:known-function-p function-name)
     (make-instance 'trucler:global-function-description
       :name function-name
       :type (global-ftype function-name)
       :compiler-macro (compiler-macro-function function-name)
       :inline (clasp-cleavir::global-inline-status function-name)
       :inline-data (inline-ast function-name)))
    (t nil)))

(defmethod trucler:describe-function ((client client) (env null) symbol)
  (trucler:describe-function client clasp-cleavir:*clasp-env* symbol))

(defmethod trucler:describe-function
    ((client client) (environment cmp:lexenv) symbol)
  (if (and (symbolp symbol)
           (clasp-cleavir::treat-as-special-operator-p symbol))
      ;; The bytecode compiler doesn't know about special operators.
      ;; (It might need to learn for Trucler, later.)
      (make-instance 'trucler:special-operator-description :name symbol)
      (let ((info (cmp:fun-info symbol environment)))
        (etypecase info
          (null ; check global
           (trucler:describe-function
            client (cmp:lexenv/global environment) symbol))
          (cmp:global-fun-info
           (make-instance 'trucler:global-function-description
             :name symbol
             :compiler-macro (cmp:global-fun-info/cmexpander info)))
          (cmp:local-fun-info
           ;; As with lexical variables, this may not end well
           ;; as there will be no identity or anything.
           (make-instance 'trucler:local-function-description
             :name symbol :identity nil))
          (cmp:global-macro-info
           (make-instance 'trucler:global-macro-description
             :name symbol :expander (cmp:global-macro-info/expander info)))
          (cmp:local-macro-info
           (make-instance 'trucler:local-macro-description
             :name symbol :expander (cmp:local-macro-info/expander info)))))))

(defmethod trucler:describe-declarations
    ((client client)
     (environment clasp-cleavir:clasp-global-environment))
  ;; FIXME: Support CL:DECLARATION
  '(;; Behavior as in convert-form.lisp
    core:lambda-name core:lambda-list))

(defmethod trucler:describe-declarations ((client client) (env null))
  (trucler:describe-declarations client clasp-cleavir:*clasp-env*))

(defmethod trucler:describe-declarations ((client client) (env cmp:lexenv))
  (trucler:describe-declarations client (cmp:lexenv/global env)))

(defmethod trucler:describe-optimize
    ((client client) (env clasp-cleavir:clasp-global-environment))
  (let ((opt cmp:*optimize*))
    (flet ((qual (name)
             (cleavir-compilation-policy:optimize-value opt name)))
      (declare (inline qual))
      (make-instance 'trucler:optimize-description
        :safety (qual 'safety) :space (qual 'space) :debug (qual 'debug)
        :compilation-speed (qual 'compilation-speed) :speed (qual 'speed)
        #+(or):optimize #+(or)opt #+(or):policy #+(or)cmp:*policy*))))

(defmethod trucler:describe-optimize ((client client) (env null))
  (trucler:describe-optimize client clasp-cleavir:*clasp-env*))

(defmethod trucler:describe-optimize ((client client) (env cmp:lexenv))
  ;; FIXME
  (trucler:describe-optimize client (cmp:lexenv/global env)))

(defmethod trucler:global-environment ((client client) (env cmp:lexenv))
  (cmp:lexenv/global env))

;;;

(defgeneric desc->info (desc))
(defmethod desc->info ((desc trucler:lexical-variable-description))
  (cmp:lexical-var-info/make (trucler:identity desc) nil))
(defmethod desc->info ((desc trucler:symbol-macro-description))
  (cmp:symbol-macro-var-info/make
   (let ((expansion (trucler:expansion desc)))
     (lambda (form env) (declare (ignore form env)) expansion))))
(defmethod desc->info
    ((desc trucler:local-special-variable-description))
  (cmp:special-var-info/make nil))
(defmethod desc->info
    ((desc trucler:global-special-variable-description))
  (cmp:special-var-info/make t))

(defmethod trucler:augment-with-variable-description
    ((client client) (env clasp-cleavir:clasp-global-environment)
     (desc trucler:variable-description))
  (cmp:lexenv/make
   (list (cons (trucler:name desc) (desc->info desc)))
   nil nil nil nil 0))

(defmethod trucler:augment-with-variable-description
    ((client client) (env null) (desc trucler:variable-description))
  (trucler:augment-with-variable-description
   client clasp-cleavir:*clasp-env* desc))

(defmethod trucler:augment-with-variable-description
    ((client client) (env cmp:lexenv)
     (desc trucler:variable-description))
  (cmp:lexenv/make
   (acons (trucler:name desc) (desc->info desc) (cmp:lexenv/vars env))
   (cmp:lexenv/tags env) (cmp:lexenv/blocks env) (cmp:lexenv/funs env)
   (cmp:lexenv/decls env) (cmp:lexenv/frame-end env)
   (cmp:lexenv/global env)))

(defmethod desc->info
    ((desc trucler:local-function-description))
  (cmp:local-fun-info/make (trucler:identity desc)))
(defmethod desc->info
    ((desc trucler:local-macro-description))
  (cmp:local-macro-info/make (trucler:expander desc)))

(defmethod trucler:augment-with-function-description
    ((client client) (env clasp-cleavir:clasp-global-environment)
     (desc trucler:function-description))
  (cmp:lexenv/make
   nil nil nil
   (list (cons (trucler:name desc) (desc->info desc)))
   (if (and (typep desc 'trucler:inline-mixin)
            (eq (trucler:inline desc) 'cl:notinline))
       (list (trucler:name desc))
       nil)
   0))

(defmethod trucler:augment-with-function-description
    ((client client) (env null) (desc trucler:function-description))
  (trucler:augment-with-function-description
   client clasp-cleavir:*clasp-env* desc))

(defmethod trucler:augment-with-function-description
    ((client client) (env cmp:lexenv)
     (desc trucler:function-description))
  (cmp:lexenv/make
   (cmp:lexenv/vars env) nil nil
   (acons (trucler:name desc) (desc->info desc) (cmp:lexenv/funs env))
   (if (and (typep desc 'trucler:inline-mixin)
            (eq (trucler:inline desc) 'cl:notinline))
       (list (trucler:name desc))
       nil)
   0 (cmp:lexenv/global env)))

(defmethod trucler:augment-with-block-description
    ((client client) (env clasp-cleavir:clasp-global-environment)
     (desc trucler:block-description))
  (cmp:lexenv/make
   nil nil
   (list (cons (trucler:name desc) (trucler:identity desc)))
   nil nil 0))

(defmethod trucler:augment-with-block-description
    ((client client) (env null) (desc trucler:block-description))
  (trucler:augment-with-block-description
   client clasp-cleavir:*clasp-env* desc))

(defmethod trucler:augment-with-block-description
    ((client client) (env cmp:lexenv) (desc trucler:block-description))
  (cmp:lexenv/make
   (cmp:lexenv/vars env) (cmp:lexenv/tags env)
   (acons (trucler:name desc) (trucler:identity desc)
          (cmp:lexenv/blocks env))
   (cmp:lexenv/funs env) (cmp:lexenv/decls env)
   (cmp:lexenv/frame-end env) (cmp:lexenv/global env)))

(defmethod trucler:augment-with-tag-description
    ((client client) (env clasp-cleavir:clasp-global-environment)
     (desc trucler:tag-description))
  (cmp:lexenv/make
   nil
   (list (cons (trucler:name desc) (trucler:identity desc)))
   nil nil nil 0))

(defmethod trucler:augment-with-tag-description
    ((client client) (env null) (desc trucler:tag-description))
  (trucler:augment-with-tag-description
   client clasp-cleavir:*clasp-env* desc))

(defmethod trucler:augment-with-tag-description
    ((client client) (env cmp:lexenv) (desc trucler:tag-description))
  (cmp:lexenv/make
   (cmp:lexenv/vars env)
   (acons (trucler:name desc) (trucler:identity desc)
          (cmp:lexenv/tags env))
   (cmp:lexenv/blocks env) (cmp:lexenv/funs env)
   (cmp:lexenv/decls env) (cmp:lexenv/frame-end env)
   (cmp:lexenv/global env)))

;;; The lexenv don't track optimize info. FIXME
;;; FIXME
(defmethod trucler:augment-with-optimize-description
    ((client client) env (desc trucler:optimize-description))
  env)
