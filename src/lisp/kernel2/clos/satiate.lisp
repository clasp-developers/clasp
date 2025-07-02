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
(satiate generic-function-methods (standard-generic-function))
(satiate generic-function-a-p-o-function (standard-generic-function))
(satiate generic-function-lambda-list (standard-generic-function))
(satiate generic-function-method-combination (standard-generic-function))
(satiate generic-function-call-history (standard-generic-function))
(satiate generic-function-specializer-profile (standard-generic-function))

(satiate method-specializers (standard-method)
         (standard-reader-method)
         (standard-writer-method))
(satiate method-qualifiers (standard-method)
         (standard-reader-method) (standard-writer-method)
         (effective-reader-method) (effective-writer-method))
(satiate method-function (standard-method))
(satiate accessor-method-slot-definition
         (standard-reader-method) (standard-writer-method)
         (effective-reader-method) (effective-writer-method))
(satiate effective-accessor-method-location
         (effective-reader-method) (effective-writer-method))

(satiate contf (%contf-method-function))

(satiate slot-definition-name
         (standard-direct-slot-definition) (standard-effective-slot-definition))
(satiate slot-definition-location
         (standard-direct-slot-definition) (standard-effective-slot-definition))

(satiate stamp-for-instances
         (standard-class) (funcallable-standard-class)
         (built-in-class))
(satiate class-precedence-list
         (standard-class) (funcallable-standard-class))
(satiate class-slots
         (standard-class) (funcallable-standard-class))

(satiate eql-specializer-p
         (eql-specializer) (standard-class) (funcallable-standard-class)
         (built-in-class))
(satiate specializer-accepts-p
         (standard-class t) (funcallable-standard-class t)
         (built-in-class t) (eql-specializer t))
(satiate compute-applicable-methods-using-classes
         (standard-generic-function t))
(satiate compute-applicable-methods (standard-generic-function t))

(satiate method-combination-compiler (method-combination))
(satiate method-combination-options (method-combination))

(satiate perform-outcome
         (optimized-slot-reader t) (optimized-slot-writer t)
         (effective-method-outcome t))
(satiate outcome-methods
         (optimized-slot-reader) (optimized-slot-writer) (effective-method-outcome))
(satiate optimized-slot-accessor-index
         (optimized-slot-reader) (optimized-slot-writer))
(satiate optimized-slot-accessor-slot-name
         (optimized-slot-reader) (optimized-slot-writer))
(satiate optimized-slot-accessor-class
         (optimized-slot-reader) (optimized-slot-writer))
(satiate effective-method-outcome-form (effective-method-outcome))
(satiate effective-method-outcome-function (effective-method-outcome))

(satiate compute-effective-method (standard-generic-function t t))

#|
(satiate expand-apply-method
         (standard-method t t t)
         (effective-reader-method t t t)
         (effective-writer-method t t t))
|#

(satiate c++-class-p (built-in-class)
         (standard-class) (funcallable-standard-class))

(satiate dtree-op-name (dtree-op))
(satiate dtree-op-arguments (dtree-op))
(satiate dtree-op-long-arguments (dtree-op))
(satiate dtree-op-label-argument-indices (dtree-op))

(satiate dtree-index (dtree-test) (bc-instruction) (bc-register-arg))
(satiate (setf dtree-index) (t bc-label-arg))
(satiate dtree-next (dtree-skip))

(satiate bc-instruction-name (bc-instruction))
(satiate bc-instruction-code (bc-instruction))
(satiate bc-lip (bc-instruction) (bc-label-arg))
(satiate bc-constant-arg-value (bc-constant-arg))
(satiate bc-constant-ref-ref (bc-constant-ref))
(satiate bc-label-arg-delta (bc-label-arg))
(satiate (setf bc-label-arg-delta) (t bc-label-arg))
