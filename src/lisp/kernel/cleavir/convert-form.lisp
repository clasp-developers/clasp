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

(defparameter *body* nil)
(defmethod cleavir-cst-to-ast:convert-code (lambda-list body
                                            env (system clasp-cleavir:clasp) &optional block-name )
  (let ((cst:*ordinary-lambda-list-grammar* clasp-cleavir:*clasp-ordinary-lambda-list-grammar*))
    (multiple-value-bind (declaration-csts documentation form-csts)
        (cst:separate-function-body body)
      (let* ((dspecs (loop for declaration-cst in declaration-csts
                           append (cdr (cst:listify declaration-cst))))
             (lambda-name-info (find 'core:lambda-name dspecs :key (lambda (cst) (cst:raw (cst:first cst)))))
             (lambda-name (if lambda-name-info
                              (cdr (cst:raw lambda-name-info)))))
        (unless lambda-name (setq lambda-name 'lambda))
        ;; Define the function-scope-info object and bind it to
        ;; the *current-function-scope-info* object
        #+(or)(progn
                (format t "About to get source for ~a~%" body)
                (format t "*current-compile-file-source-pos-info* -> ~a~%" clasp-cleavir::*current-compile-file-source-pos-info*))
        (let* ((spi (or (when (cst:source body)
                          (clasp-cleavir:source-pos-info (cst:source body)))
                        clasp-cleavir::*current-compile-file-source-pos-info*
                        (core:make-source-pos-info "-nowhere-" 0 0 0)))
               #+(or)(_ (format t "About to make instance spi -> ~a~%" spi))
               (scope (make-instance 'clasp-cleavir:function-scope
                                     :scope-function-name lambda-name
                                     :source-pos-info spi))
               (clasp-cleavir:*current-function-scope-info* scope)
               (origin (make-instance 'clasp-cleavir:scoped-source-pos-info
                                      :source-pos-info spi
                                      :scope scope)))
          (let ((function-ast (call-next-method)))
            (setf (cleavir-ast:origin function-ast) origin)
            ;; Make the change here to a named-function-ast with lambda-name
            (change-class function-ast 'clasp-cleavir-ast:named-function-ast
                          :lambda-name lambda-name)))))))


 
