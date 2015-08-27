(in-package :clasp-cleavir)

(defvar *function-inline-asts* (make-hash-table :test #'equal))
(export '(*function-inline-asts*))

(defun proclaim-hook (decl)
  (let ((head (car decl)))
    (cond
      ((eq head 'cl:ftype)
       (format t "*** Do something with proclaim ftype ~s~%" decl))
      ;; Add other clauses here
      (t (warn "Add support for proclaim ~s~%" decl)))))

(defun global-function-inline-ast (name)
  (gethash name *function-inline-asts*))

(defun do-inline-hook (name function)
  (when (core:declared-global-inline-p name)
    (let ((ast (cleavir-generate-ast:generate-ast 
                function
                clasp-cleavir:*clasp-env* 
                clasp-cleavir:*clasp-system*)))
      (setf (gethash name *function-inline-asts*) ast))))
  
;; Generate an AST and save it for inlining if the
;; function is proclaimed as inline
(defun defun-inline-hook (name function)
  (let ((ast-gs (gensym "AST")))
    (when (core:declared-global-inline-p name)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (when core:*do-inline-hook*
           (funcall core:*do-inline-hook* (QUOTE ,name) (QUOTE ,function)))))))
    
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq core:*defun-inline-hook* #'defun-inline-hook)
  (setq core:*do-inline-hook* #'do-inline-hook)
  (setq core:*proclaim-hook* #'proclaim-hook))
