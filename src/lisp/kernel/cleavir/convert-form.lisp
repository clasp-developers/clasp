(in-package :cc-generate-ast)


(defmethod cleavir-generate-ast:convert-code (lambda-list body env (system clasp-cleavir:clasp) &optional block-name )
  (let ((function-ast (call-next-method)))
    (multiple-value-bind (declarations documentation forms)
        (cleavir-code-utilities:separate-function-body body)
      (let* ((dspecs (reduce #'append (mapcar #'cdr declarations)))
             (lambda-name (cadr (find 'core:lambda-name dspecs :key #'car))))
        (unless lambda-name (setq lambda-name 'cl:lambda))
    ;; Make the change here to a named-function-ast with lambda-name
        (change-class function-ast 'clasp-cleavir-ast:named-function-ast
                      :lambda-name lambda-name)))))


(defmethod cleavir-cst-to-ast:convert-code (lambda-list body env (system clasp-cleavir:clasp) &optional block-name )
  (let* ((cst:*ordinary-lambda-list-grammar* clasp-cleavir:*clasp-ordinary-lambda-list-grammar*)
         (function-ast (call-next-method)))
    (multiple-value-bind (declaration-csts documentation form-csts)
        (cst:separate-function-body body)
      (let* ((dspecs (loop for declaration-cst in declaration-csts
                           append (cdr (cst:listify declaration-cst))))
             (found (find 'core:lambda-name dspecs :key (lambda (cst) (cst:raw (cst:first cst)))))
             (lambda-name (when found (cadr (cst:raw found)))))
        (unless lambda-name (setq lambda-name 'lambda-from-convert-code))
    ;; Make the change here to a named-function-ast with lambda-name
        (change-class function-ast 'clasp-cleavir-ast:named-function-ast
                      :lambda-name lambda-name)))))


 
