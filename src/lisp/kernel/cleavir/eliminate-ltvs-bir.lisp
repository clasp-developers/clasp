(in-package #:clasp-cleavir-translate-bir)

(defun replace-constant-with-precalc (value index input)
  (let* ((use (cleavir-bir:use input))
         (cleavir-bir:*policy* (cleavir-bir:policy use))
         (cleavir-bir:*origin* (cleavir-bir:origin use)))
    (change-class input 'cc-bir:precalc-value
                  :form ',value :index index)
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

(defun eliminate-load-time-value-inputs (ir system env)
  (cleavir-bir:map-instructions
   (lambda (instruction)
     (loop for input in (cleavir-bir:inputs instruction)
           when (typep input 'cleavir-bir:constant)
             do (eliminate-constant-input input env)
           when (typep input 'cleavir-bir:load-time-value)
             do (eliminate-load-time-value-input input env)))
   ir))
