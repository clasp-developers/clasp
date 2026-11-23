(in-package #:clasp-tests)

;;; Exercise the registration macro without changing the running compiler's
;;; deriver table. Both ordinary and SETF function names must survive intact.
(test-true bulk-deriver-imports
  (let ((clasp-cleavir::*derivers* (make-hash-table :test #'equal)))
    (eval '(clasp-cleavir::import-derivers eq identity (setf aref)))
    (and (gethash 'eq clasp-cleavir::*derivers*)
         (gethash '(setf aref) clasp-cleavir::*derivers*)
         (not (gethash ''eq clasp-cleavir::*derivers*))
         ;; IDENTITY must preserve the argument type, not default to TOP.
         (let* ((sys clasp-cleavir::*clasp-system*)
                (arg (cleavir-ctype:member sys :same))
                (result (funcall (gethash 'identity clasp-cleavir::*derivers*)
                                 (cleavir-ctype:values
                                  (list arg) nil
                                  (cleavir-ctype:bottom sys) sys))))
           (cleavir-ctype:values-subtypep
            result (cleavir-ctype:single-value
                    arg sys) sys)))))

(defun derive-default-array-type-p (operator expected)
  (let* ((sys clasp-cleavir::*clasp-system*)
         (result (funcall (gethash operator clasp-cleavir::*derivers*)
                          (cleavir-ctype:single-value
                           (cleavir-ctype:range 'integer 3 3 sys) sys)))
         (expected (cleavir-ctype:single-value
                    (cleavir-env:parse-type-specifier expected nil sys) sys)))
    ;; A bottom result would also be a subtype, but is not a valid answer.
    (and (not (cleavir-ctype:values-subtypep
               result (cleavir-ctype:values-bottom sys) sys))
         (cleavir-ctype:values-subtypep result expected sys))))

;;; These intentionally remain failing until the obsolete keyword defaults
;;; are replaced by type objects. Runtime MAKE-ARRAY/MAKE-STRING tests alone
;;; would miss the lost element-type information.
(test-true make-array-deriver-default-element-type
  (derive-default-array-type-p 'make-array '(simple-array t (*))))
(test-true make-string-deriver-default-element-type
  (derive-default-array-type-p 'make-string '(simple-array character (*))))

(defun check-type-combination-expansion (arity)
  (loop for operator in '(cleavir-ctype:conjoin cleavir-ctype:disjoin
                          cleavir-ctype:wdisjoin cleavir-ctype:values-conjoin
                          cleavir-ctype:values-disjoin
                          cleavir-ctype:values-wdisjoin)
        always
        (let* ((sys clasp-cleavir::*clasp-system*)
               (valuesp (member operator '(cleavir-ctype:values-conjoin
                                           cleavir-ctype:values-disjoin
                                           cleavir-ctype:values-wdisjoin)))
               (subtypep (if valuesp #'cleavir-ctype:values-subtypep
                             #'cleavir-ctype:subtypep))
               (type (if valuesp
                         (cleavir-ctype:values-top sys)
                         (cleavir-ctype:top sys)))
               (form `(,operator (progn (push :system trace) client)
                                 ,@(loop for i below arity
                                         collect `(progn (push ,i trace) type))))
               (expansion (funcall (compiler-macro-function operator) form nil))
               (probe (compile nil
                               `(lambda (client type)
                                  (declare (ignorable client type))
                                  (let ((trace nil))
                                    (let ((result ,expansion))
                                      (values result (reverse trace)))))))
               (expected (apply (symbol-function operator)
                                sys (make-list arity :initial-element type))))
          (multiple-value-bind (actual trace) (funcall probe sys type)
            (and (equal trace (cons :system (loop for i below arity collect i)))
                 (funcall subtypep actual expected sys)
                 (funcall subtypep expected actual sys))))))

(test-true type-combination-empty-expansion
  (check-type-combination-expansion 0))
(test-true type-combination-single-expansion
  (check-type-combination-expansion 1))
(test-true type-combination-multiple-expansion
  (check-type-combination-expansion 3))
