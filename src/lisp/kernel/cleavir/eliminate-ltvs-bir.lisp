(in-package #:clasp-cleavir-translate-bir)

(defun replace-constant-with-precalc (value index input)
  (let* ((use (cleavir-bir:use input))
         (cleavir-bir:*policy* (cleavir-bir:policy use))
         (cleavir-bir:*origin* (cleavir-bir:origin use)))
    (change-class input 'cc-bir:precalc-constant
                  :form `',value :index index :value value)
    (cleavir-bir:insert-instruction-before input use)))

(defun replace-constant-with-immediate (value input)
  (change-class input 'cleavir-bir:immediate :value value))

(defun eliminate-constant-input (input env)
  (multiple-value-bind (index-or-immediate immediatep)
      ;; kind of a dumb hack to throw it a constant.
      (cc-ast::process-ltv env `',(cleavir-bir:constant-value input) t)
    (if immediatep
        (replace-constant-with-immediate index-or-immediate input)
        (replace-constant-with-precalc (cleavir-bir:constant-value input)
                                       index-or-immediate input))))

(defun replace-load-time-value-with-precalc (form index input)
  (let* ((use (cleavir-bir:use input))
         (cleavir-bir:*policy* (cleavir-bir:policy use))
         (cleavir-bir:*origin* (cleavir-bir:origin use)))
    (change-class input 'cc-bir:precalc-value
                  :form form :index index)
    (cleavir-bir:insert-instruction-before input use)))

(defun replace-load-time-value-with-immediate (value input)
  (check-type value integer)
  (change-class input 'cleavir-bir:immediate :value value))

(defun eliminate-load-time-value-input (input env)
  (multiple-value-bind (index-or-immediate immediatep)
      (cc-ast::process-ltv env (cleavir-bir:form input)
                           (cleavir-bir:read-only-p input))
    (if immediatep
        (replace-load-time-value-with-immediate index-or-immediate input)
        (replace-load-time-value-with-precalc
         (cleavir-bir:form input) index-or-immediate input))))

(defun eliminate-function-load-time-value-inputs (function system env)
  (cleavir-bir:map-local-instructions
   (lambda (instruction)
     (loop for input in (cleavir-bir:inputs instruction)
           when (typep input '(and cleavir-bir:constant
                               (not cc-bir:precalc-value)))
             do (eliminate-constant-input input env)
           when (typep input 'cleavir-bir:load-time-value)
             do (eliminate-load-time-value-input input env)))
   function))

(defun eliminate-load-time-value-inputs (module system env)
  (cleavir-set:doset (f (cleavir-bir:functions module))
    (eliminate-function-load-time-value-inputs f system env)))
