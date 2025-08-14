(in-package #:cross-clasp.cleavir)

(defmethod env:variable-info ((system cross:client)
                              (env clostrum:environment) symbol)
  (ecase (clostrum:variable-status client env symbol)
    ((:special)
     (make-instance 'env:special-variable-info
       :name symbol :global-p t
       :type (clostrum:variable-type client env symbol)))
    ((:constant)
     (make-instance 'env:constant-variable-info
       :name symbol :value (clostrum:symbol-value client env symbol)))
    ((:symbol-macro)
     (make-instance 'env:symbol-macro-info
       :name symbol
       :expansion (funcall (clostrum:variable-macro-expander client env symbol)
                           symbol env)))
    ((nil) nil)))

(defmethod env:variable-info ((system cross:client) (env null) symbol)
  (env:variable-info system cross::*build-ce* symbol))

(defmethod env:function-info ((system cross:client) (env clostrum:environment)
                              symbol)
  (ecase (clostrum:operator-status client env symbol)
    ((:function)
     (make-instance 'env:global-function-info
       :name symbol
       :type (clostrum:operator-ftype client env symbol)
       :inline (clostrum:operator-inline client env symbol)
       :compiler-macro (clostrum:compiler-macro-function client env symbol)
       :ast nil ; FIXME
       :attributes (clasp-cleavir::function-attributes symbol)))
    ((:macro)
     (make-instance 'env:global-macro-info
       :name symbol
       :inline (clostrum:operator-inline client env symbol)
       :expander (clostrum:macro-function client env symbol)
       :compiler-macro (clostrum:compiler-macro-function client env symbol)))
    ((:special-operator)
     (make-instance 'env:special-operator-info :name symbol))
    ((nil) nil)))

(defmethod env:function-info ((system cross:client) (env null) symbol)
  (env:function-info system cross::*build-ce* symbol))

(defmethod env:declarations ((system cross:client)
                             (environment clostrum:environment))
  ;; FIXME
  '(core:lambda-name core:lambda-list))

(defmethod env:optimize-info ((sys cross:client) (env clostrum:environment))
  (make-instance 'env:optimize-info
    :optimize (clostrum:optimize env)
    :policy nil)) ; FIXME

(defmethod env:type-expand ((sys cross:client) (env clostrum:environment)
                            specifier)
  (clostrum:type-expand sys env specifier))

(defmethod env:find-class (name (env clostrum:environment) (sys cross:client)
                           &optional (errorp t))
  (clostrum:find-class sys env name errorp))

;;; And here we diverge from what could really be considered
;;; environment access.

(defun cleavir-env->bytecode (client env)
  ;; Convert a cleavir ENTRY (or null/global) into a Maclina environment.
  ;; Only for compile time environments, so it's symbol macros, macros, and
  ;; declarations. But we ignore declarations other than specials and inline-
  ;; FIXME? Maclina does dump that stuff now... but we're just eval-ing here.
  (etypecase env
    ((or clostrum:environment null)
     (maclina.compile:make-null-lexical-environment env))
    (env:special-variable
     (trucler:add-local-special-variable
      client (cleavir-env->bytecode (env::next env)) (env:name env)))
    (env:symbol-macro
     (trucler:add-local-symbol-macro
      client (cleavir-env->bytecode (env::next env))
      (env:name env) (env:expansion env)))
    (env:macro
     (trucler:add-local-macro
      client (cleavir-env->bytecode (env::next env))
      (env:name env) (env:expander env)))
    (env:inline
     (trucler:add-inline
      client (cleavir-env->bytecode (env::next env))
      (env:name env) (env:inline env)))
    (env::entry (cleavir-env->bytecode (env::next env)))))

(defmethod env:eval (form env (sys cross:client))
  (maclina.compile:eval form (cleavir-env->bytecode sys env)))

(defmethod env:cst-eval (cst env (sys cross:client))
  ;; FIXME? Could set up source info from the CST
  (maclina.compile:eval (cst:raw cst) (cleavir-env->bytecode sys env)))
