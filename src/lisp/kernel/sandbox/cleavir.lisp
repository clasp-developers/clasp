(in-package #:clasp-sandbox)

;;;; Customization of cleavir for our environments

(defmethod cleavir-policy:compute-policy-quality
    ((quality (eql 'cleavir-kildall-type-inference:insert-type-checks))
     optimize
     (environment sandbox-environment))
  (> (cleavir-policy:optimize-value optimize 'safety)
     (cleavir-policy:optimize-value optimize 'speed)))

(defmethod cleavir-policy:compute-policy-quality
    ((quality (eql 'cleavir-escape:trust-dynamic-extent))
     optimize
     (environment sandbox-environment))
  (> (cleavir-policy:optimize-value optimize 'space)
     (cleavir-policy:optimize-value optimize 'safety)))

;;; The trick is that everything flows from +global-environment+, and there is no real way to
;;; look up anything - including the symbol value of +global-environment+! - without it. I
;;; believe in SICL the plan is to provide a few things like +global-environment+ as arguments
;;; to the FASL run function, but clasp's loader does not work that way.
;;; Along with function-cell, what I do instead is resolve references to those using the old
;;; global machinery of symbol-value- and fdefinition- ast. More ideally, they should only be
;;; special-cased within load-time-value, but this is hard to pull off with the ltv architecture.

;;; The right way to do it might be to have function-cell-read and get-environment AST/instructions
;;; that only work in ltv context. For example, the loader could put the environment and
;;; #'sicl-genv:function-cell in the first two slots of the ltv vector before doing anything else,
;;; and then the instructions just read those slots.

(defmethod cleavir-generate-ast:convert-global-function
    ((info cleavir-env:global-function-info) (env sandbox-environment) system)
  (declare (ignore system))
  ;; We need to ensure that at least some functions can be "actually" referred to, in order to
  ;; prevent infinite regress. For now, we can use FDEFINITION-AST to get at the "really global"
  ;; SICL-GENV:FUNCTION-CELL.
  (if (eq (cleavir-env:name info) 'sicl-genv:function-cell)
      (cleavir-ast:make-fdefinition-ast
       (cleavir-ast:make-load-time-value-ast ''sicl-genv:function-cell t))
      (cleavir-ast:make-car-ast
       (cleavir-ast:make-load-time-value-ast
        `(sicl-genv:function-cell
          ',(cleavir-env:name info)
          sicl-genv:*global-environment*)))))

(defmethod cleavir-generate-ast:convert-special-variable
    ((info cleavir-env:special-variable-info) (env sandbox-environment) system)
  (declare (ignore system))
  (if (eq (cleavir-env:name info) 'sicl-genv:*GLOBAL-ENVIRONMENT*)
      (cleavir-ast:make-symbol-value-ast
       (cleavir-ast:make-load-time-value-ast ''sicl-genv:*global-environment*))
      (cleavir-ast:make-call-ast
       (cleavir-ast:make-car-ast
        (cleavir-ast:make-load-time-value-ast
         '(sicl-genv:function-cell 'core:symbol-value-from-cell sicl-genv:*global-environment*)))
       (list (cleavir-ast:make-load-time-value-ast
              `',(cleavir-env:name info)
              t)
             (cleavir-ast:make-load-time-value-ast
              `(sicl-genv:variable-cell ',(cleavir-env:name info) sicl-genv:*global-environment*)
              t)
             (cleavir-ast:make-load-time-value-ast
              `(sicl-genv:variable-unbound ',(cleavir-env:name info) sicl-genv:*global-environment*))))))

(defmethod cleavir-generate-ast:convert-setq-special-variable
    ((info cleavir-env:special-variable-info) var form-ast (env sandbox-environment) system)
  (declare (ignore var))
  (declare (ignore system))
  (cleavir-ast:make-call-ast
   (cleavir-ast:make-car-ast
    (cleavir-ast:make-load-time-value-ast
     '(sicl-genv:function-cell
       'core:setf-symbol-value-from-cell
       sicl-genv:*global-environment*)))
   (list (cleavir-ast:make-load-time-value-ast
	  `',(cleavir-env:name info)
	  t)
         form-ast
	 (cleavir-ast:make-load-time-value-ast
	  `(sicl-genv:variable-cell ',(cleavir-env:name info) sicl-genv:*global-environment*)
	  nil))))

(defmethod cleavir-env:eval (form environment (dispatch sandbox-environment))
  (eval-with-genv form environment))
