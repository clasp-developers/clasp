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

(defun maybe-mark-enclose (input)
  (when-let* ((definers (cleavir-ir:defining-instructions input))
              (only-one-definer (= (length definers) 1))
              (definer (first definers))
              (is-enclose (typep definer 'cleavir-ir:enclose-instruction)))
    (setf (cleavir-ir:dynamic-extent-p definer) t)))

;;; This function finds calls to certain functions that we generate for special operators,
;;; and marks their thunk arguments as stack allocatable (dynamic extent).
;;; TODO/FIXME: A more principled way to do this, in Cleavir.
;;; Current functions: cleavir-primop:call-with-variable-bound,
;;;                    core:progv-function,
;;;                    core:catch-function, core:throw-function,
;;;                    core:funwind-protect, core:multiple-value-prog1-function
(defun optimize-stack-enclose (top-instruction)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (i)
     (when-let* ((is-funcall (typep i 'cleavir-ir:funcall-instruction))
                 (input1 (first (cleavir-ir:inputs i)))
                 (fdefs (cleavir-ir:defining-instructions input1))
                 (only-one-fdef (= (length fdefs) 1))
                 (fdef (first fdefs))
                 (is-fdef (typep fdef 'cleavir-ir:fdefinition-instruction))
                 (fdef-input (first (cleavir-ir:inputs fdef)))
                 (fdef-input-definers (cleavir-ir:defining-instructions fdef-input))
                 (only-one-fdef-input-definer (= (length fdef-input-definers) 1))
                 (precalc (first fdef-input-definers))
                 (is-precalc (progn
                               (unless (and (typep precalc 'clasp-cleavir-hir:precalc-value-instruction)
                                            (clasp-cleavir-hir:precalc-value-instruction-p precalc))
                                 (error "The typep test for ~s failed to match the predicate" precalc))
                               (clasp-cleavir-hir:precalc-value-instruction-p precalc)))
                 (callee (let ((original-object (clasp-cleavir-hir:precalc-value-instruction-original-object precalc)))
                           (if (consp original-object)
                               (second original-object)
                               (error "The original-object ~s must be consp - precalc object ~s" original-object precalc)))))
                (case callee
                  ((core:progv-function
                    cleavir-primop:call-with-variable-bound)
                   (maybe-mark-enclose (fourth (cleavir-ir:inputs i))))
                  ((core:catch-function
                    core:throw-function)
                   (maybe-mark-enclose (third (cleavir-ir:inputs i))))
                  ((core:funwind-protect
                    core:multiple-value-prog1-function)
                   (maybe-mark-enclose (second (cleavir-ir:inputs i)))
                   (maybe-mark-enclose (third (cleavir-ir:inputs i)))))))
   top-instruction))
