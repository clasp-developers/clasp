(in-package :clasp-cleavir)

;;; Copied from Alexandria
(defmacro when-let* (bindings &body forms)
  "Creates new variable bindings, and conditionally executes FORMS.
BINDINGS must be either single binding of the form:
 (variable initial-form)
or a list of bindings of the form:
 ((variable-1 initial-form-1)
  (variable-2 initial-form-2)
  ...
  (variable-n initial-form-n))
Each initial-form is executed in turn, and the variable bound to the
corresponding value. Initial-form expressions can refer to variables
previously bound by the WHEN-LET*.
Execution of WHEN-LET* stops immediately if any initial-form evaluates to NIL.
If all initial-forms evaluate to true, then FORMS are executed as an implicit
PROGN."
  (let ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                          (list bindings)
                          bindings)))
    (labels ((bind (bindings forms)
               (if bindings
                   `((let (,(car bindings))
                       (when ,(caar bindings)
                         ,@(bind (cdr bindings) forms))))
                   forms)))
      `(let (,(car binding-list))
         (when ,(caar binding-list)
           ,@(bind (cdr binding-list) forms))))))


(defun optimize-stack-enclose (top-instruction)
  (let (encloses)
    (cleavir-ir:map-instructions
     (lambda (i)
       (when-let*
        ((is-funcall-inst (typep i 'cleavir-ir:funcall-instruction))
         (input1 (car (cleavir-ir:inputs i)))
         (input4 (fourth (cleavir-ir:inputs i)))
         (fdefs (cleavir-ir:defining-instructions input1))
         (fenclose (and input4 (cleavir-ir:defining-instructions input4)))
         (fdef (car fdefs))
         (enclose (car fenclose))
         (only-one-fdefs (= (length fdefs) 1))
         (is-fdef-ins (typep fdef 'cleavir-ir:fdefinition-instruction))
         (only-one-enclose (= (length fenclose) 1))
         (fdef-found (typep enclose 'cleavir-ir:enclose-instruction))
         (fdef-input (car (cleavir-ir:inputs fdef)))
         (cwvb (car (cleavir-ir:defining-instructions fdef-input)))
         (precalc (typep cwvb 'clasp-cleavir-hir:precalc-symbol-instruction))
         (orig (cadr (clasp-cleavir-hir:precalc-symbol-instruction-original-object cwvb)))
         (is-precalc (typep cwvb 'clasp-cleavir-hir:precalc-symbol-instruction))
         (found-cwvb (eq orig 'cleavir-primop:call-with-variable-bound)))
        (format t "Found enclose ~a~%" enclose)
        (change-class enclose 'cc-mir:stack-enclose-instruction)))
     top-instruction)))
