(cl:in-package #:clasp-cleavir)
;;;; This code was copied from cleavir's  eliminate-load-time-value-inputs.lisp
;;;;  I'm modifying it to work with Clasp's way of dealing with load-time-values

(defun replace-constant-with-precalc (value index input)
  (change-class input 'cleavir-ir:lexical-location :name (gensym "CV"))
  (loop for instr in (cleavir-ir:using-instructions input)
        do (let* ((cleavir-ir:*policy* (cleavir-ir:policy instr))
                  (cleavir-ir:*dynamic-environment*
                    (cleavir-ir:dynamic-environment instr))
                  (pvi (clasp-cleavir-hir:make-precalc-value-instruction
                        index input :original-object value
                        :origin (cleavir-ir:origin instr))))
             (cleavir-ir:insert-instruction-before pvi instr))))

(defun replace-constant-with-immediate (value input)
  (change-class input 'cleavir-ir:immediate-input :value value))

(defun eliminate-constant-input (input env)
  (multiple-value-bind (index-or-immediate immediatep)
      ;; kind of dumb hack to throw it a constant.
      (clasp-cleavir-ast::process-ltv
       env (list 'quote (cleavir-ir:value input)) t)
    (if immediatep
        (replace-constant-with-immediate index-or-immediate input)
        (replace-constant-with-precalc (cleavir-ir:value input) index-or-immediate input))))

(defun replace-load-time-value-with-precalc (form index input)
  (change-class input 'cleavir-ir:lexical-location :name (gensym "LTV"))
  (loop for instr in (cleavir-ir:using-instructions input)
        do (let* ((cleavir-ir:*policy* (cleavir-ir:policy instr))
                  (cleavir-ir:*dynamic-environment*
                    (cleavir-ir:dynamic-environment instr))
                  (pvi (clasp-cleavir-hir:make-precalc-value-instruction
                        index input :original-object form
                        :origin (cleavir-ir:origin instr))))
             (cleavir-ir:insert-instruction-before pvi instr))))

(defun replace-load-time-value-with-immediate (value input)
  (check-type value integer)
  (change-class input 'cleavir-ir:immediate-input :value value))

(defun eliminate-load-time-value-input (input env)
  (multiple-value-bind (index-or-immediate immediatep)
      (clasp-cleavir-ast::process-ltv
       env (cleavir-ir:form input) (cleavir-ir:read-only-p input))
    (if immediatep
        (replace-load-time-value-with-immediate index-or-immediate input)
        (replace-load-time-value-with-precalc
         (cleavir-ir:form input) index-or-immediate input))))

;;; Main entry point.
(defun eliminate-load-time-value-inputs (initial-instruction system env)
  (assert (typep initial-instruction 'cleavir-ir:top-level-enter-instruction))
  (cleavir-ir:reinitialize-data initial-instruction)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (instruction)
     (loop for input in (cleavir-ir:inputs instruction)
           when (typep input 'cleavir-ir:constant-input)
             do (eliminate-constant-input input env)
           when (typep input 'cleavir-ir:load-time-value-input)
             do (eliminate-load-time-value-input input env)))
   initial-instruction)
  (cleavir-ir:reinitialize-data initial-instruction))
