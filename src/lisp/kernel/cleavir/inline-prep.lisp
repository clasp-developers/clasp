(in-package :clasp-cleavir)

(defun proclaim-ftype (type fnames)
  (loop for fname in fnames
        do (setf (global-ftype fname) type)))

(defun proclaim-hook (decl)
  (let ((head (car decl)))
    (cond
      ((eq head 'cl:ftype)
       (destructuring-bind (type &rest fnames) (cdr decl)
         (proclaim-ftype type fnames)))
      ((eq head 'cl:optimize)
       (setf *global-optimize*
             (cleavir-policy:normalize-optimize
              (append (rest decl) *global-optimize*)
              *clasp-env*)
             *global-policy*
             (cleavir-policy:compute-policy *global-optimize*
                                            *clasp-env*)))
      ;; Add other clauses here
      (t #+(or)(warn "Add support for proclaim ~s~%" decl)))))

(defvar *code-walker* nil)

#+cst
(defmethod cleavir-cst-to-ast:convert :before (cst environment (system clasp-64bit))
  (declare (ignore system))
  (when *code-walker*
    (let ((form (cst:raw cst)))
      (funcall *code-walker* form environment))))

#-cst
(defmethod cleavir-generate-ast:convert :before (form environment (system clasp-64bit))
  (declare (ignore system))
  (when *code-walker*
    (funcall *code-walker* form environment)))

(defun code-walk-using-cleavir (code-walker-function form env)
  (let* ((cleavir-generate-ast:*compiler* 'cl:compile)
         (core:*use-cleavir-compiler* t)
         (*code-walker* code-walker-function))
    (handler-bind
        ((cleavir-env:no-variable-info
           (lambda (condition)
             (invoke-restart #+cst 'cleavir-cst-to-ast:consider-special
                             #-cst 'cleavir-generate-ast:consider-special)))
         (cleavir-env:no-function-info
           (lambda (condition)
             (invoke-restart #+cst 'cleavir-cst-to-ast:consider-global
                             #-cst 'cleavir-generate-ast:consider-global)))
         ;; No point printing warnings twice (now, and when the method body
         ;; is actually compiled)
         (warning #'muffle-warning)
         ;; Upon other error just give up.
         ;; (Returning NIL means code could not be walked.)
         (error (lambda (e)
                  (declare (ignore e))
                  (return-from code-walk-using-cleavir nil))))
      #+cst
      (cleavir-cst-to-ast:cst-to-ast (cst:cst-from-expression form) env *clasp-system*)
      #-cst
      (cleavir-generate-ast:generate-ast form env *clasp-system*)))
  t)

(export 'code-walk-using-cleavir)

(defun defun-inline-hook (name function-form env)
  (when (core:declared-global-inline-p name)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (when (core:declared-global-inline-p ',name)
         (setf (inline-ast ',name)
               (cleavir-primop:cst-to-ast ,function-form))))))

(export '(*code-walker*))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (setq core:*proclaim-hook* 'proclaim-hook))
