(in-package :cc-generate-ast)


(defmethod cleavir-generate-ast:convert-code (lambda-list body env (system clasp-cleavir:clasp) &key block-name )
  (let ((function-ast (call-next-method)))
    (multiple-value-bind (declarations documentation forms)
        (cleavir-code-utilities:separate-function-body body)
      (let* ((dspecs (reduce #'append (mapcar #'cdr declarations)))
             (lambda-name (cadr (find 'core:lambda-name dspecs :key #'car)))
             (origin (or (cleavir-ast:origin function-ast) ; should be nil, but just in case.
                         core:*current-source-pos-info*)))
        (unless lambda-name (setq lambda-name 'cl:lambda))
        ;; Make the change here to a named-function-ast with lambda-name
        (change-class function-ast 'clasp-cleavir-ast:named-function-ast
                      :lambda-name lambda-name
                      :origin origin
                      :original-lambda-list lambda-list
                      :docstring documentation)))))

(defmethod cleavir-cst-to-ast:convert-code (lambda-list body
                                            env (system clasp-cleavir:clasp) &key block-name-cst origin)
  (let ((cst:*ordinary-lambda-list-grammar* clasp-cleavir:*clasp-ordinary-lambda-list-grammar*))
    (multiple-value-bind (declaration-csts documentation form-csts)
        (cst:separate-function-body body)
      (let* ((dspecs (loop for declaration-cst in declaration-csts
                           append (cdr (cst:listify declaration-cst))))
             (lambda-name-info (find 'core:lambda-name dspecs :key (lambda (cst) (cst:raw (cst:first cst)))))
             (lambda-name (if lambda-name-info
                              (car (cdr (cst:raw lambda-name-info))))))
        (unless lambda-name (setq lambda-name 'lambda))
        ;; Define the function-scope-info object and bind it to
        ;; the *current-function-scope-info* object
        (let ((origin (or (and (cst:source body)
                               (let ((source (cst:source body)))
                                 (if (consp source) (car source) source)))
                          core:*current-source-pos-info*))
              (function-ast (call-next-method)))
            ;; Make the change here to a named-function-ast with lambda-name
            (change-class function-ast 'clasp-cleavir-ast:named-function-ast
                          :lambda-name lambda-name
                          :origin origin
                          :original-lambda-list (if lambda-list (cst:raw lambda-list) nil)
                          :docstring (when documentation (cst:raw documentation))))))))


 
