(in-package #:clasp-cleavir-translate-bir)

(defun replace-load-time-value-with-precalc (form index input)
  (let* ((use (cleavir-bir:use input))
         (cleavir-bir:*policy* (cleavir-bir:policy use))
         (cleavir-bir:*origin* (cleavir-bir:origin use)))
    (change-class input 'cc-bir:precalc-value
                  :form form :index index)
    (cleavir-bir:insert-instruction-before input use)))

(defun eliminate-load-time-value-input (input env)
  ;; Should only have load time value inputs in a COMPILE-FILE situation.
  (assert (eq cleavir-cst-to-ast:*compiler* 'cl:compile-file))
  (let* ((form (cleavir-bir:form input))
         (index (literal:with-load-time-value
                    (clasp-cleavir::compile-form form env))))
    (replace-load-time-value-with-precalc form index input)))

(defun eliminate-function-load-time-value-inputs (function system env)
  (cleavir-bir:map-local-instructions
   (lambda (instruction)
     (dolist (input (cleavir-bir:inputs instruction))
       (when (typep input 'cleavir-bir:load-time-value)
         (eliminate-load-time-value-input input env))))
   function))

(defun eliminate-load-time-value-inputs (module system env)
  (cleavir-set:doset (f (cleavir-bir:functions module))
    (eliminate-function-load-time-value-inputs f system env)))
