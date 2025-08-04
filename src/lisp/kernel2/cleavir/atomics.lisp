(in-package #:mp)

(defmethod %get-atomic-expansion ((place symbol) environment keys)
  ;; KLUDGE: the cleavir interface may not be great for this.
  (let ((info (cleavir-env:variable-info
               clasp-cleavir:*clasp-system* environment place)))
    (etypecase info
      (cleavir-env:symbol-macro-info
       (apply #'get-atomic-expansion (macroexpand-1 place environment) keys))
      (cleavir-env:special-variable-info
       (apply #'get-atomic-expansion `(symbol-value ',place) keys))
      (cleavir-env:lexical-variable-info
       ;; TODO
       (error 'not-atomic :place place))
      (null
       (error "Unknown variable ~a" place)))))
