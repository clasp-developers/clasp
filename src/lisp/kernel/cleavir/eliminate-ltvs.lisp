(cl:in-package #:clasp-cleavir)
;;;; This code was copied from cleavir's  eliminate-load-time-value-inputs.lisp
;;;;  I'm modifying it to work with Clasp's way of dealing with load-time-values
;;;;
;;;; If the LOAD-TIME-VALUE-INPUT to be replaced has a form of type
;;;; (QUOTE constant), then CONVERT-CONSTANT-TO-IMMEDIATE is called,
;;;; passing the constant and the SYSTEM as arguments.  If that call
;;;; returns an integer, then instead of adding a form to the initial
;;;; instruction, this transformation replaces the input by an
;;;; immediate input instead.  If that call returns NIL, the normal
;;;; transformation is performed.
;;;;
;;;; Client code can add a method on CONVERT-CONSTANT-TO-IMMEDIATE
;;;; that should then specialize on the second parameter, i.e. the
;;;; SYSTEM.

(defgeneric convert-constant-to-immediate (constant system))

(defmethod convert-constant-to-immediate (constant system)
  (cleavir-generate-ast:convert-constant-to-immediate constant *clasp-env* system))


(defun replace-constant-with-lexical (initial-instruction input env)
  (loop for instr in (cleavir-ir:using-instructions input)
        do (let ((index (clasp-cleavir-ast::generate-new-precalculated-value-index
                         env
                         `',(cleavir-ir:value input)
                         t #|constant-value are read-only-p|#)))
             (check-type index clasp-cleavir:arrayed-literal)
             (let* ((index-input (cleavir-ir:make-immediate-input
                                  index))
                    (cleavir-ir:*policy* (cleavir-ir:policy instr)) 
                    (pvi (clasp-cleavir-hir:make-precalc-value-instruction
                          index-input
                          input
;;;                       :successor instr
                          :original-object `',(cleavir-ir:value input))))
               (cleavir-ir:insert-instruction-before pvi instr))))
  (let ((new-sym (gensym "CV")))
    (change-class input 'cleavir-ir:lexical-location
                  :name new-sym)))

(defun replace-constant-with-immediate (value input)
  (change-class input 'cleavir-ir:immediate-input :value value))

(defun eliminate-constant-input (initial-instruction input system env)
  (let ((immediate (convert-constant-to-immediate (cleavir-ir:value input) system)))
    (if (null immediate)
        (replace-constant-with-lexical initial-instruction input env)
        (replace-constant-with-immediate immediate input))))

(defun replace-load-time-value-with-lexical (initial-instruction input env)
  (loop for instr in (cleavir-ir:using-instructions input)
     do (let ((index (clasp-cleavir-ast::generate-new-precalculated-value-index
                              env
                              (cleavir-ir:form input)
                              (cleavir-ir:read-only-p input))))
          (let* ((index-input (cleavir-ir:make-immediate-input
                               index))
                 (cleavir-ir:*policy* (cleavir-ir:policy instr)) 
                 (pvi (clasp-cleavir-hir:make-precalc-value-instruction
                       index-input
                       input
                       :successor instr
                       :original-object (cleavir-ir:form input))))
            (cleavir-ir:insert-instruction-before pvi instr))))
  (change-class input 'cleavir-ir:lexical-location
                :name (gensym)))

(defun replace-load-time-value-with-immediate (value input)
  (check-type value integer)
  (change-class input 'cleavir-ir:immediate-input :value value))

(defun eliminate-load-time-value-input (initial-instruction input system env)
  (if (cleavir-hir-transformations:load-time-value-is-constant-p input)
      (let* ((constant (cleavir-hir-transformations:load-time-value-constant input))
	     (immediate (convert-constant-to-immediate constant system)))
	(if (null immediate)
	    (replace-load-time-value-with-lexical initial-instruction input env)
	    (replace-load-time-value-with-immediate immediate input)))
      (replace-load-time-value-with-lexical initial-instruction input)))

;;; Main entry point.
(defun eliminate-load-time-value-inputs (initial-instruction system env)
  (assert (typep initial-instruction 'cleavir-ir:top-level-enter-instruction))
  (cleavir-ir:reinitialize-data initial-instruction)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (instruction)
     (loop for input in (cleavir-ir:inputs instruction)
        when (typep input 'cleavir-ir:constant-input)
        do (eliminate-constant-input
            initial-instruction input system env)
        when (typep input 'cleavir-ir:load-time-value-input)
        do (eliminate-load-time-value-input
            initial-instruction input system env)))
   initial-instruction)
  (cleavir-ir:reinitialize-data initial-instruction))
