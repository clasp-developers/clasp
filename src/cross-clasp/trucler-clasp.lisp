(defpackage #:trucler-native-clasp
  (:use #:cl)
  (:import-from #:trucler-native #:client)
  (:export #:client))

(in-package #:trucler-native-clasp)

(defmethod trucler:describe-variable
    ((client client) (env clasp-cleavir:clasp-global-environment)
     symbol)
  (trucler:describe-variable client nil symbol))

(defgeneric info->desc (info name))

(defmethod info->desc ((info null) name)
  (declare (ignore name))
  info)

(defmethod info->desc ((info cmp:constant-var-info) name)
  (make-instance 'trucler:constant-variable-description
    :name name :value (cmp:constant-var-info/value info)))
(defmethod info->desc ((info cmp:special-var-info) name)
  (if (cmp:special-var-info/globalp info)
      (make-instance 'trucler:global-special-variable-description
        :name name :type (clasp-cleavir::global-type name))
      (make-instance 'trucler:local-special-variable-description
        :name name)))
(defmethod info->desc ((info cmp:symbol-macro-var-info) name)
  (if (cmp:symbol-macro-var-info/globalp info)
      (make-instance 'trucler:global-symbol-macro-description
        :name name
        :expansion (funcall *macroexpand-hook*
                            (cmp:symbol-macro-var-info/expander info)
                            name nil))
      (make-instance 'trucler:local-symbol-macro-description
        :name name
        :expansion (funcall *macroexpand-hook*
                            (cmp:symbol-macro-var-info/expander info)
                            name nil))))
(defmethod info->desc ((info cmp:lexical-var-info) name)
  (make-instance 'trucler:lexical-variable-description
    :name name
    :identity (cmp:lexical-var-info/lex info)))

(defmethod trucler:describe-variable ((client client) (env null)
                                      symbol)
  (info->desc (cmp:var-info symbol env) symbol))

(defmethod trucler:describe-variable
    ((client client) (env cmp:lexenv) symbol)
  (info->desc (cmp:var-info symbol env) symbol))

(defmethod trucler:describe-function
    ((client client)
     (environment clasp-cleavir:clasp-global-environment)
     function-name)
  (trucler:describe-function client nil function-name))

(defmethod info->desc ((info cmp:special-operator-info) name)
  (make-instance 'trucler:special-operator-description :name name))
(defmethod info->desc ((info cmp:global-fun-info) name)
  (make-instance 'trucler:global-function-description
    :name name :inline (cmp:fun-info/inline-status info)
    :compiler-macro (cmp:global-fun-info/cmexpander info)
    :type (clasp-cleavir::global-ftype name)))
(defmethod info->desc ((info cmp:local-fun-info) name)
  (make-instance 'trucler:local-function-description
    :name name :inline (cmp:fun-info/inline-status info)
    :identity (cmp:local-fun-info/lex info)))
(defmethod info->desc ((info cmp:global-macro-info) name)
  (make-instance 'trucler:global-macro-description
    :name name :inline (cmp:fun-info/inline-status info)
    :expander (cmp:global-macro-info/expander info)))
(defmethod info->desc ((info cmp:local-macro-info) name)
  (make-instance 'trucler:local-macro-description
    :name name
    :expander (cmp:local-macro-info/expander info)))

(defmethod trucler:describe-function ((client client) (env null) name)
  (info->desc (cmp:fun-info name env) name))

(defmethod trucler:describe-function
    ((client client) (environment cmp:lexenv) name)
  (info->desc (cmp:fun-info name environment) name))

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
   (etypecase desc
     (trucler:global-symbol-macro-description t)
     (trucler:local-symbol-macro-description nil))
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
  (cmp:local-fun-info/make (trucler:inline desc) (trucler:identity desc)))
(defmethod desc->info
    ((desc trucler:local-macro-description))
  (cmp:local-macro-info/make (trucler:inline desc) (trucler:expander desc)))

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
