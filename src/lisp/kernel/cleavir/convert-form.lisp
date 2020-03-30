(in-package :cc-generate-ast)


(defmethod cleavir-generate-ast:convert-code (lambda-list body env (system clasp-cleavir:clasp) &key block-name )
  (let ((function-ast (call-next-method)))
    (multiple-value-bind (declarations documentation forms)
        (cleavir-code-utilities:separate-function-body body)
      (declare (ignore documentation)) ; handled by cleavir
      (let* ((dspecs (reduce #'append (mapcar #'cdr declarations)))
             (lambda-name (or (cadr (find 'core:lambda-name dspecs :key #'car))
                              (cleavir-ast:name function-ast)))
             (rest-position (position '&rest lambda-list))
             ;; FIXME? for an invalid lambda list like (foo &rest) this could cause a weird error
             (restvar (and rest-position (elt lambda-list (1+ rest-position))))
             (rest-alloc (cmp:compute-rest-alloc restvar dspecs))
             (origin (or (cleavir-ast:origin function-ast) ; should be nil, but just in case.
                         core:*current-source-pos-info*)))
        (unless lambda-name
          (setq lambda-name (list 'cl:lambda (cmp::lambda-list-for-name lambda-list))))
        ;; Make the change here to a named-function-ast with lambda-name
        (change-class function-ast 'clasp-cleavir-ast:named-function-ast
                      :name lambda-name
                      :origin origin
                      :rest-alloc rest-alloc)))))

(defmethod cleavir-cst-to-ast:convert-code (lambda-list body
                                            env (system clasp-cleavir:clasp) &key block-name-cst origin)
  (let ((cst:*ordinary-lambda-list-grammar* clasp-cleavir:*clasp-ordinary-lambda-list-grammar*))
    (multiple-value-bind (declaration-csts documentation form-csts)
        (cst:separate-function-body body)
      (declare (ignore documentation)) ; handled by cleavir
      (let* ((dspecs (loop for declaration-cst in declaration-csts
                           append (cdr (cst:listify declaration-cst))))
             (lambda-name-info (find 'core:lambda-name dspecs :key (lambda (cst) (cst:raw (cst:first cst)))))
             (lambda-name (when lambda-name-info
                            (car (cdr (cst:raw lambda-name-info)))))
             (cmp:*track-inlinee-name* (cons lambda-name cmp:*track-inlinee-name*))
             (original-lambda-list (if lambda-list (cst:raw lambda-list) nil))
             (rest-position (position '&rest original-lambda-list))
             ;; ditto FIXME in c-g-a version
             (restvar (and rest-position (elt original-lambda-list (1+ rest-position))))
             (rest-alloc (cmp:compute-rest-alloc restvar dspecs)))
        ;; Define the function-scope-info object and bind it to
        ;; the *current-function-scope-info* object
        (let ((origin (let ((source (cst:source body)))
                        (cond ((consp source) (car source))
                              ((null source) core:*current-source-pos-info*)
                              (t source))))
              (function-ast (call-next-method)))
          (setf lambda-name
                (or lambda-name ; from declaration
                    (cleavir-ast:name function-ast) ; local functions named by cleavir
                    (list 'lambda (cmp::lambda-list-for-name original-lambda-list))))
          ;; Make the change here to a named-function-ast with lambda-name
          (change-class function-ast 'clasp-cleavir-ast:named-function-ast
                        :name lambda-name
                        :origin origin
                        :rest-alloc rest-alloc))))))
