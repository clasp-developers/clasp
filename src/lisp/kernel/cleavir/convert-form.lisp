(in-package #:clasp-cleavir)

(defmethod cleavir-cst-to-ast:convert-code (lambda-list body
                                            env (system clasp-cleavir:clasp)
                                            &key block-name-cst origin)
  (declare (ignore env block-name-cst origin))
  (let ((cst:*ordinary-lambda-list-grammar* clasp-cleavir:*clasp-ordinary-lambda-list-grammar*))
    (multiple-value-bind (declaration-csts documentation)
        (cst:separate-function-body body)
      (declare (ignore documentation)) ; handled by cleavir
      (let* ((dspecs (loop for declaration-cst in declaration-csts
                           append (cdr (cst:listify declaration-cst))))
             (lambda-name-info (find 'core:lambda-name dspecs
                                     :key (lambda (cst) (cst:raw (cst:first cst)))))
             (lambda-name (when lambda-name-info
                            (car (cdr (cst:raw lambda-name-info)))))
             (cmp:*track-inlinee-name* (cons lambda-name cmp:*track-inlinee-name*))
             (original-lambda-list (if lambda-list (cst:raw lambda-list) nil)))
        ;; Define the function-scope-info object and bind it to
        ;; the *current-function-scope-info* object
        (let ((origin (let ((source (cst:source body)))
                        (cond ((consp source) (car source))
                              ((null source) core:*current-source-pos-info*)
                              (t source))))
              (function-ast (call-next-method)))
          (setf (cleavir-ast:origin function-ast) origin
                (cleavir-ast:name function-ast)
                (or lambda-name ; from declaration
                    (cleavir-ast:name function-ast) ; local functions named by cleavir
                    (list 'lambda (cmp::lambda-list-for-name original-lambda-list))))
          function-ast)))))
