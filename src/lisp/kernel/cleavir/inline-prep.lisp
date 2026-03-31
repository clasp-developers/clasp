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
              *clasp-system* (append (rest decl) cmp:*optimize*))
             cmp:*policy*
             (policy:compute-policy *clasp-system* cmp:*optimize*)))
      ;; Add other clauses here
      (t #+(or)(warn "Add support for proclaim ~s~%" decl)))))

(setq core::*proclaim-hook* 'proclaim-hook)
