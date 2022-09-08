(in-package :core)

(eval-when (:execute)
  (format t "Setting core:*echo-repl-read* to T~%")
  (setq core:*echo-repl-read* t))

#+bytecode
(eval-when (:execute :load-toplevel)


;;; Wipe out macros we can't have for cleavir

  (fmakunbound 'cleavir-primop:eq)
  (fmakunbound 'cleavir-primop:typeq)
  (fmakunbound 'cleavir-primop:car)
  (fmakunbound 'cleavir-primop:cdr)
;;;  (fmakunbound 'core::header-stamp-case)
  )

;;; From opt-type.lisp

#+bytecode
(eval-when (:execute :load-toplevel)
  (define-compiler-macro typep (&whole whole object type &optional environment
                                       &environment macro-env)
    (unless (and (constantp type macro-env) (null environment))
      (return-from typep whole))
    (let* ((type (ext:constant-form-value type macro-env))
           (expanded (typep-expansion type macro-env whole)))
      (if (eq expanded whole)
          whole                         ; failure
          `(let ((object ,object))
             (declare (ignorable object)) ; e.g. for type T
             ,expanded))))
  )

;;; From opt-condition.lisp

#|
#+bytecode
(eval-when (:execute :load-toplevel)

  (define-compiler-macro error (&whole whole datum &rest arguments &environment env)
    (let ((datum (and (constantp datum env) (ext:constant-form-value datum env)))
          ;; This is special cased instead of relying on a type declaration, because
          ;; a function declared to not return may proceed in safe code to a
          ;; call to ERROR, which certainly does not return.
          (default `(locally (declare (notinline error))
                      ,whole
                      (cleavir-primop:unreachable))))
      (case datum
        ((type-error)
         ;; FIXME: If the initargs are bad, don't err here in the compiler.
         (destructuring-bind (&key (datum nil datump) (expected-type nil expp))
             arguments
           (if (and datump expp)
               `(progn (core:multiple-value-foreign-call
                        "cc_error_type_error" ,datum ,expected-type)
                       (cleavir-primop:unreachable))
               default)))
        ((core::array-out-of-bounds)
         (destructuring-bind (&key (datum nil datump) (expected-type nil expp)
                                (array nil arrayp))
             arguments
           (if (and datump expp arrayp)
               `(progn (core:multiple-value-foreign-call
                        "cc_error_array_out_of_bounds" ,datum ,expected-type ,array)
                       (cleavir-primop:unreachable))
               default)))
        ;; this will include the non-constant case (datum = nil)
        (otherwise default))))

  )
|#

;;; From opt-condition.lisp

#|
#+bytecode
(eval-when (:execute :load-toplevel)
  (define-compiler-macro core::etypecase-error (&whole whole value types &environment env)
    (if (constantp types env)
        (let ((types (ext:constant-form-value types env)))
          `(core:multiple-value-foreign-call
            "cc_error_case_failure"
            ,value '(or ,@types) 'etypecase ',types))
        whole))
)
|#
