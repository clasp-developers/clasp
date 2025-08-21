(in-package #:clos)

;;; FIXME: Define in config, or at least elsewhere
(defconstant +where-tag-mask+      #b11000)
(defconstant +derivable-where-tag+ #b00000)
(defconstant +rack-where-tag+      #b01000)
(defconstant +wrapped-where-tag+   #b10000)
(defconstant +header-where-tag+    #b11000)
(defconstant +fixnum-tag+ 342)
(defconstant +single-float-tag+ 310)
(defconstant +character-tag+ 1582)
(defconstant +cons-tag+ 30)

(defmacro core::header-stamp-case (stamp derivable rack wrapped header)
  `(case (logand (ash ,stamp 2) ,+where-tag-mask+)
     (,+derivable-where-tag+ ,derivable)
     (,+rack-where-tag+ ,rack)
     (,+wrapped-where-tag+ ,wrapped)
     (,+header-where-tag+ ,header)))

(defun instance-stamp (object)
  ;; This is way dumber than the eventual dfuns, but we can take advantage
  ;; of one cheat - nothing we're satiating here wants a non-general.
  (cond
    ((core:generalp object)
     (let ((hstamp (core::header-stamp object)))
       (core::header-stamp-case hstamp
         (core::derivable-stamp object)
         (core::rack-stamp object)
         (core::wrapped-stamp object)
         hstamp)))
    ((consp object) +cons-tag+)
    ((core:fixnump object) +fixnum-tag+)
    ((core:single-float-p object) +single-float-tag+)
    ((characterp object) +character-tag+)
    (t (error "Unknown object ~s" object))))

;;; Minimum needed to call generic functions.
;;; May be an overestimate since debugging my way down to a
;;; truly minimum set sounds like a terrible time.
(base-satiate generic-function-methods (standard-generic-function))
(base-satiate generic-function-a-p-o-function (standard-generic-function))
(base-satiate generic-function-argument-precedence-order (standard-generic-function))
(base-satiate generic-function-lambda-list (standard-generic-function))
(base-satiate generic-function-method-combination (standard-generic-function))
(base-satiate generic-function-specializer-profile (standard-generic-function))

(base-satiate method-specializers (standard-method)
              (standard-reader-method)
              (standard-writer-method))
(base-satiate method-qualifiers (standard-method)
              (standard-reader-method) (standard-writer-method)
              (effective-reader-method) (effective-writer-method))
(base-satiate method-function (standard-method))
(base-satiate accessor-method-slot-definition
              (standard-reader-method) (standard-writer-method)
              (effective-reader-method) (effective-writer-method))
(base-satiate effective-accessor-method-location
              (effective-reader-method) (effective-writer-method))

(base-satiate contf (%contf-method-function))

(base-satiate slot-definition-name
              (standard-direct-slot-definition) (standard-effective-slot-definition))
(base-satiate slot-definition-location
              (standard-direct-slot-definition) (standard-effective-slot-definition))

(base-satiate stamp-for-instances
              (standard-class) (funcallable-standard-class)
              (built-in-class))
(base-satiate class-precedence-list
              (standard-class) (funcallable-standard-class))
(base-satiate class-slots
              (standard-class) (funcallable-standard-class))

(base-satiate eql-specializer-p
              (eql-specializer) (standard-class) (funcallable-standard-class)
              (built-in-class))
(base-satiate specializer-accepts-p
              (standard-class t) (funcallable-standard-class t)
              (built-in-class t) (eql-specializer t))
(base-satiate compute-applicable-methods-using-classes
              (standard-generic-function t))
(base-satiate compute-applicable-methods (standard-generic-function t))

(base-satiate method-combination-compiler (method-combination))
(base-satiate method-combination-options (method-combination))

(base-satiate perform-outcome
              (optimized-slot-reader t) (optimized-slot-writer t)
              (effective-method-outcome t))
(base-satiate outcome-methods
              (optimized-slot-reader) (optimized-slot-writer) (effective-method-outcome))
(base-satiate optimized-slot-accessor-index
              (optimized-slot-reader) (optimized-slot-writer))
(base-satiate optimized-slot-accessor-slot-name
              (optimized-slot-reader) (optimized-slot-writer))
(base-satiate optimized-slot-accessor-class
              (optimized-slot-reader) (optimized-slot-writer))
(base-satiate effective-method-outcome-form (effective-method-outcome))
(base-satiate effective-method-outcome-function (effective-method-outcome))

(base-satiate compute-effective-method (standard-generic-function t t))

(base-satiate c++-class-p (built-in-class)
              (standard-class) (funcallable-standard-class))

(base-satiate dtree-op-name (dtree-op))
(base-satiate dtree-op-arguments (dtree-op))
(base-satiate dtree-op-long-arguments (dtree-op))
(base-satiate dtree-op-label-argument-indices (dtree-op))

(base-satiate dtree-index (dtree-test) (bc-instruction) (bc-register-arg))
(base-satiate (setf dtree-index) (t bc-label-arg))
(base-satiate dtree-next (dtree-skip))

(base-satiate bc-instruction-name (bc-instruction))
(base-satiate bc-instruction-code (bc-instruction))
(base-satiate bc-lip (bc-instruction) (bc-label-arg))
(base-satiate bc-constant-arg-value (bc-constant-arg))
(base-satiate bc-constant-ref-ref (bc-constant-ref))
(base-satiate bc-label-arg-delta (bc-label-arg))
(base-satiate (setf bc-label-arg-delta) (t bc-label-arg))
