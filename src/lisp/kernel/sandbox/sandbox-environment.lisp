(in-package #:sandbox-environment)

;;;; A global environment that things can be evaluated in.

(defclass sandbox-environment (sicl-simple-environment:simple-environment) ())

;;; The trick is that everything flows from +global-environment+, and there is no real way to
;;; look up anything - including the symbol value of +global-environment+! - without it. I
;;; believe in SICL the plan is to provide a few things like +global-environment+ as arguments
;;; to the FASL run function, but clasp's loader does not work that way.
;;; Along with function-cell, what I do instead is resolve references to those using the old
;;; global machinery of symbol-value- and fdefinition- ast. More ideally, they should only be
;;; special-cased within load-time-value, but this is hard to pull off with the ltv architecture.
;;; Or rather that's what I did for clasp. For the alien version, I'll have the compiler
;;; special case load-time-value where the form is one of these important calls.

;;; The right way to do it might be to have function-cell-read and get-environment AST/instructions
;;; that only work in ltv context. For example, the loader could put the environment and
;;; #'sicl-genv:function-cell in the first two slots of the ltv vector before doing anything else,
;;; and then the instructions just read those slots.

(defmethod cleavir-generate-ast:convert-global-function
    ((info cleavir-env:global-function-info) (env sandbox-environment) system)
  (declare (ignore system))
  (cleavir-ast:make-car-ast
   (cleavir-ast:make-load-time-value-ast
    `(sicl-genv:function-cell
      ',(cleavir-env:name info)
      sicl-genv:*global-environment*))))

(defmethod cleavir-generate-ast:convert-special-variable
    ((info cleavir-env:special-variable-info) (env sandbox-environment) system)
  (declare (ignore system))
  (cleavir-ast:make-call-ast
   (cleavir-ast:make-car-ast
    (cleavir-ast:make-load-time-value-ast
     '(sicl-genv:function-cell
       #+clasp 'core:symbol-value-from-cell
       #-clasp 'sandbox-environment:symbol-value-from-cell
       sicl-genv:*global-environment*)))
   (list (cleavir-ast:make-load-time-value-ast
          `',(cleavir-env:name info)
          t)
         (cleavir-ast:make-load-time-value-ast
          `(sicl-genv:variable-cell ',(cleavir-env:name info) sicl-genv:*global-environment*)
          t)
         (cleavir-ast:make-load-time-value-ast
          `(sicl-genv:variable-unbound ',(cleavir-env:name info) sicl-genv:*global-environment*)))))

(defmethod cleavir-generate-ast:convert-setq-special-variable
    ((info cleavir-env:special-variable-info) var form-ast (env sandbox-environment) system)
  (declare (ignore var))
  (declare (ignore system))
  (cleavir-ast:make-call-ast
   (cleavir-ast:make-car-ast
    (cleavir-ast:make-load-time-value-ast
     '(sicl-genv:function-cell
       #+clasp 'core:setf-symbol-value-from-cell
       #-clasp 'sandbox-environment:symbol-value-from-cell
       sicl-genv:*global-environment*)))
   (list (cleavir-ast:make-load-time-value-ast
	  `',(cleavir-env:name info)
	  t)
         form-ast
	 (cleavir-ast:make-load-time-value-ast
	  `(sicl-genv:variable-cell ',(cleavir-env:name info) sicl-genv:*global-environment*)
	  nil))))
