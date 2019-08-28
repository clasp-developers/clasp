(in-package #:cmp)

;;; This macro basically exists to reduce code size in several very heavily used
;;; functions, such as CAR, which in cclasp will be inlined just about everywhere.
(define-compiler-macro error (&whole whole datum &rest arguments &environment env)
  (let ((datum (and (constantp datum env) (ext:constant-form-value datum env))))
    (case datum
      ((type-error)
       ;; FIXME: If the initargs are bad, don't err here in the compiler.
       (destructuring-bind (&key (datum nil datump) (expected-type nil expp))
           arguments
         (if (and datump expp)
             `(progn (core:multiple-value-foreign-call
                      "cc_error_type_error" ,datum ,expected-type)
                     (cleavir-primop:unreachable))
             whole)))
      ((core::array-out-of-bounds)
       (destructuring-bind (&key (datum nil datump) (expected-type nil expp)
                              (array nil arrayp))
           arguments
         (if (and datump expp arrayp)
             `(progn (core:multiple-value-foreign-call
                      "cc_error_array_out_of_bounds" ,datum ,expected-type ,array)
                     (cleavir-primop:unreachable))
             whole)))
      ;; this will include the non-constant case (datum = nil)
      (otherwise whole))))

;; Ditto for ETYPECASE
(define-compiler-macro core::etypecase-error (&whole whole value types &environment env)
  (if (constantp types env)
      (let ((types (ext:constant-form-value types env)))
        `(core:multiple-value-foreign-call
          "cc_error_case_failure"
          ,value '(or ,@types) 'etypecase ',types))
      whole))
