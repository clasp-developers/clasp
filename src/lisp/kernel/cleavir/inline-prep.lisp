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


(defmethod make-load-form ((ast cleavir-ast:ast) &optional environment)
  (values `(allocate-instance ',(class-of ast))
          `(initialize-instance
            ,ast
            ,@(loop for (keyword reader)
                    in (cleavir-io:save-info ast)
                    for value = (funcall reader ast)
                    collect `(quote ,keyword)
                    collect `(quote ,value)))))

;; Store

;; Generate an AST and save it for inlining if the
;; function is proclaimed as inline
(defun defun-inline-hook (name function-form env)
  (when (core:declared-global-inline-p name)
    (let* ((cleavir-generate-ast:*compiler* 'cl:compile)
           ;; we have to be tricky, because
           ;; (let ((x ...)) (defun foo ...))
           ;; means we can't inline, whereas
           ;; (defun foo ...y...) [where y is unknown]
           ;; should treat y as a special variable.
           ;; CLEAVIR-ENV:COMPILE-TIME strips out lexical parts of
           ;; the environment, so we generate the AST in the stripped
           ;; environment, and if we run into a gap, check to see
           ;; whether the real environment has it. If it does,
           ;; it was stripped, so we're in the first situation and
           ;; give up.
           (ast (handler-bind
                    ((cleavir-env:no-variable-info
                       (lambda (condition)
                         (if (cleavir-env:variable-info
                              env (cleavir-env:name condition))
                             ;; we could potentially leave a note.
                             (return-from defun-inline-hook nil)
                             (invoke-restart #+cst 'cleavir-cst-to-ast:consider-special
                                             #-cst 'cleavir-generate-ast:consider-special))))
                     (cleavir-env:no-function-info
                       (lambda (condition)
                         (if (cleavir-env:function-info
                              env (cleavir-env:name condition))
                             (return-from defun-inline-hook nil)
                             (invoke-restart #+cst 'cleavir-cst-to-ast:consider-global
                                             #-cst 'cleavir-generate-ast:consider-global)))))
                  #+cst
                  (cleavir-cst-to-ast:cst-to-ast
                   (cst:cst-from-expression function-form)
                   (cleavir-env:compile-time env) *clasp-system*)
                  #-cst
                  (cleavir-generate-ast:generate-ast
                   function-form (cleavir-env:compile-time env) *clasp-system*))))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (when (core:declared-global-inline-p ',name)
           (when (fboundp ',name)
             (setf (inline-ast ',name) ,ast)))))))


(export '(*simple-environment* *code-walker*))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (setq core:*defun-inline-hook* 'defun-inline-hook)
  (setq core:*proclaim-hook* 'proclaim-hook))
