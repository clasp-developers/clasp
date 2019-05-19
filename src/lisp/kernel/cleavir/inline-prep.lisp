(in-package :clasp-cleavir)

(defun proclaim-hook (decl)
  (let ((head (car decl)))
    (cond
      ((eq head 'cl:ftype)
       #+verbose-compiler(warn "*** Do something with proclaim ftype ~s~%" decl))
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

(defparameter *simple-environment* nil)
(defvar *code-walker* nil)

(defun mark-env-as-function ()
  (push 'si::function-boundary *simple-environment*))

(defun local-function-form-p (form)
  #+clc(warn "Convert this to use predicate ext:local_function_p")
  (and (listp form) (member (first form) '(flet labels))))

#+cst
(defmethod cleavir-cst-to-ast:convert :around (cst environment (system clasp-64bit))
  (declare (ignore system))
  (let ((*simple-environment* *simple-environment*))
    (when *code-walker*
      (let ((form (cst:raw cst)))
        (when (local-function-form-p form)
          (mark-env-as-function))
        (funcall *code-walker* form *simple-environment*)))
    (call-next-method)))

#-cst
(defmethod cleavir-generate-ast:convert :around (form environment (system clasp-64bit))
  (declare (ignore system))
  (let ((*simple-environment* *simple-environment*))
    (when *code-walker*
      (when (local-function-form-p form)
        (mark-env-as-function))
      (funcall *code-walker* form *simple-environment*))
    (call-next-method)))

(defun code-walk-using-cleavir (form env &key code-walker-function)
  (let* ((cleavir-generate-ast:*compiler* 'cl:compile)
         (core:*use-cleavir-compiler* t)
         (clasp-cleavir:*code-walker* code-walker-function))
    (handler-bind
        ((cleavir-env:no-variable-info
           (lambda (condition)
             (invoke-restart #+cst 'cleavir-cst-to-ast:consider-special
                             #-cst 'cleavir-generate-ast:consider-special)))
         (cleavir-env:no-function-info
           (lambda (condition)
             (invoke-restart #+cst 'cleavir-cst-to-ast:consider-global
                             #-cst 'cleavir-generate-ast:consider-global))))
      #+cst
      (cleavir-cst-to-ast:cst-to-ast (cst:cst-from-expression form) env *clasp-system*)
      #-cst
      (cleavir-generate-ast:generate-ast form env *clasp-system*))))

(export 'code-walk-using-cleavir)

(defun defun-inline-hook (name function-form env)
  (when (core:declared-global-inline-p name)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (when (and (core:declared-global-inline-p ',name)
                  (fboundp ',name)) ; FIXME: why?
         (setf (inline-ast ',name)
               (cleavir-primop:cst-to-ast ,function-form))))))

(export '(*simple-environment* *code-walker*))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (setq core:*proclaim-hook* 'proclaim-hook))
