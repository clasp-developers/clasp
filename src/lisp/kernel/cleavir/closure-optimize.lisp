(in-package :clasp-cleavir)

(defmacro predicate-let* ((setup test-form) &rest cont-forms)
  `(let* ,setup
     (when ,test-form
       ,@cont-forms)))

(defun find-potential-stack-enclose (top-instruction)
  (let (encloses)
    (cleavir-ir:map-instructions
     (lambda (i)
       (let ((enclose
              (predicate-let*
               (()
                (typep i 'cleavir-ir:funcall-instruction))
               (predicate-let*
                (((input1 (car (cleavir-ir:inputs i)))
                  (input4 (fourth (cleavir-ir:inputs i)))
                  (fdefs (cleavir-ir:defining-instructions input1))
                  (fencs (and input4 (cleavir-ir:defining-instructions input4)))
                  (fdef (car fdefs))
                  (enclose (car fencs)))
                 (and (= (length fdefs) 1)
                      (typep fdef 'cleavir-ir:fdefinition-instruction)
                      (= (length fencs) 1)
                      (typep enclose 'cleavir-ir:enclose-instruction)))
                (predicate-let*
                 (((fdef-input (car (cleavir-ir:inputs fdef)))
                   (cwvb (car (cleavir-ir:defining-instructions fdef-input))))
                  (typep cwvb 'clasp-cleavir-hir:precalc-symbol-instruction))
                 (let ((orig (cadr (clasp-cleavir-hir:precalc-symbol-instruction-original-object cwvb))))
                   (and (typep cwvb 'clasp-cleavir-hir:precalc-symbol-instruction)
                        (eq orig
                            'cleavir-primop:call-with-variable-bound))))
                enclose))))
         (when enclose
           (format t "   found enclose: ~a~%" i)
           (push enclose encloses))))
     top-instruction)
    encloses))


(defun optimize-stack-enclose (top-instruction)
  (let ((encloses (find-potential-stack-enclose top-instruction)))
    (dolist (e encloses)
      (change-class e 'cc-mir:stack-enclose-instruction))))
