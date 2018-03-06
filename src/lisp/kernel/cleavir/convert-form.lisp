(in-package :cc-generate-ast)


(defmethod cleavir-generate-ast:convert-code (lambda-list body env (system clasp-cleavir:clasp) &key block-name )
  (let ((function-ast (call-next-method)))
    (multiple-value-bind (declarations documentation forms)
        (cleavir-code-utilities:separate-function-body body)
      (let* ((dspecs (reduce #'append (mapcar #'cdr declarations)))
             (lambda-name (cadr (find 'core:lambda-name dspecs :key #'car))))
        (unless lambda-name (setq lambda-name 'cl:lambda))
        ;; Make the change here to a named-function-ast with lambda-name
        (change-class function-ast 'clasp-cleavir-ast:named-function-ast
                      :lambda-name lambda-name)))))

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
        #+(or)(format *debug-io* "lambda-name -> ~a~%" lambda-name)
        (unless lambda-name (setq lambda-name 'lambda))
        ;; Define the function-scope-info object and bind it to
        ;; the *current-function-scope-info* object
        #+(or)(progn
                (format t "About to get source for ~a~%" body)
                (format t "*current-compile-file-source-pos-info* -> ~a~%" clasp-cleavir::*current-compile-file-source-pos-info*))
        (let ((origin (or (and (cst:source body)
                               (let ((source (cst:source body)))
                                 (if (consp source) (car source) source)))
                          clasp-cleavir::*current-compile-file-source-pos-info*
                          (core:make-source-pos-info "-nowhere-" 0 0 0))))
          (let ((function-ast (call-next-method)))
            (setf (cleavir-ast:origin function-ast) origin)
            ;; Make the change here to a named-function-ast with lambda-name
            (change-class function-ast 'clasp-cleavir-ast:named-function-ast
                          :lambda-name lambda-name)))))))


 
