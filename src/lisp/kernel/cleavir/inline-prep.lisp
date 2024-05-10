(in-package :clasp-cleavir)

(defun proclaim-ftype (type fnames)
  (loop for fname in fnames
        do (setf (global-ftype fname) type)))

(defun proclaim-type (type vnames)
  (loop for vname in vnames
        do (setf (global-type vname) type)))

(defun proclaim-hook (decl)
  (let ((head (car decl)))
    (cond
      ((eq head 'cl:ftype)
       (destructuring-bind (type &rest fnames) (cdr decl)
         (proclaim-ftype type fnames)))
      ((eq head 'cl:type)
       (destructuring-bind (type &rest vnames) (cdr decl)
         (proclaim-type type vnames)))
      ((eq head 'cl:optimize)
       (setf cmp:*optimize*
             (policy:normalize-optimize
              (append (rest decl) cmp:*optimize*) *clasp-env*)
             cmp:*policy*
             (policy:compute-policy cmp:*optimize* *clasp-env*)))
      ;; Add other clauses here
      (t #+(or)(warn "Add support for proclaim ~s~%" decl)))))

;;; Bound by cst->ast to preserve source info.
(defvar *compiling-cst* nil)

(defmacro compute-inline-ast (form compile-file-semantics-p)
  (let ((cleavir-cst-to-ast:*compiler*
          (if compile-file-semantics-p
              'cl:compile-file
              'cl:compile))
        (cmp:*cleavir-compile-hook* 'bir-compile)
        ;; FIXME: This will mess up inline definitions within macrolets etc.
        (env nil)
        (cst (if *compiling-cst*
                 (cst:reconstruct *clasp-system* form *compiling-cst*)
                 (cst:cst-from-expression form))))
    (cst->ast cst env)))

;;; Incorporated into DEFUN expansion (see lsp/evalmacros.lisp)
(defun defun-inline-hook (name function-form env)
  (declare (ignore env))
  (when (core:declared-global-inline-p name)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (when (core:declared-global-inline-p ',name)
         (setf (inline-ast ',name)
               ;; Must use file compilation semantics here to compile
               ;; load-time-value correctly.
               (compute-inline-ast ,function-form t))))))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (setq core:*proclaim-hook* 'proclaim-hook))
