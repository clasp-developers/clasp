;;;; Satiate relevant Cleavir functions so that
;;;; startup and the first compile go a little faster.
;;;; (This file is thus essentially optional.)

(in-package #:clasp-cleavir)

;;; Initialization stuff.
;;; Note that we'll include some abstract classes that are never actually instantiated.
;;; Just putting them in the call history is no problem, though.
(eval-when (:load-toplevel)
  (macrolet ((frob ()
               `(clos:satiate-initialization
                 ;; AST initialization - speeds up loading inline.lisp.
                 ,@(clos:subclasses* (find-class 'cleavir-ast:ast))
                 ;; the remainder only speeds up the first compile
                 ,@(clos:subclasses* (find-class 'cleavir-ir:instruction))
                 ,@(clos:subclasses* (find-class 'cleavir-env::entry))
                 'cleavir-env:lexical-variable-info
                 'cleavir-env:special-variable-info
                 'cleavir-env:constant-variable-info
                 'cleavir-env:symbol-macro-info
                 'cleavir-env:local-function-info
                 'cleavir-env:global-function-info
                 'cleavir-env:local-macro-info
                 'cleavir-env:special-operator-info
                 'cleavir-env:block-info
                 'cleavir-env:tag-info
                 'cleavir-env:optimize-info)))
    (frob)))

;;; Functions go here (TODO)

;;; cleavir-env
(eval-when (:load-toplevel)
  (clos:satiate #'cleavir-env:name
                '(cleavir-env:function) '(cleavir-env:block) '(cleavir-env:macro) '(cleavir-env:tag)
                '(cleavir-env:lexical-variable) '(cleavir-env:special-variable)
                '(cleavir-env:variable-ignore) '(cleavir-env:variable-type)
                '(cleavir-env:local-function-info) '(cleavir-env:global-function-info)
                '(cleavir-env:global-macro-info) '(cleavir-env:special-variable-info)
                '(cleavir-env:lexical-variable-info))
  (clos:satiate #'cleavir-env:policy '(cleavir-env:optimize-info))
  (clos:satiate #'cleavir-env:ast '(cleavir-env:local-function-info) '(cleavir-env:global-function-info))
  (clos:satiate #'cleavir-env:expander
                '(cleavir-env:local-macro-info) '(cleavir-env:global-macro-info) '(cleavir-env:macro))
  (clos:satiate #'cleavir-env:compiler-macro
                '(cleavir-env:global-function-info) '(cleavir-env:global-function-info))
  (clos:satiate #'cleavir-env:identity
                '(cleavir-env:local-function-info) '(cleavir-env:block-info) '(cleavir-env:tag-info)
                '(cleavir-env:lexical-variable-info) '(cleavir-env:lexical-variable)
                '(cleavir-env:function) '(cleavir-env:block) '(cleavir-env:tag))
  (clos:satiate #'cleavir-env:inline '(cleavir-env:local-function-info) '(cleavir-env:global-function-info))
  (clos:satiate #'cleavir-env:type
                '(cleavir-env:variable-type)
                '(cleavir-env:local-function-info) '(cleavir-env:lexical-variable-info)
                '(cleavir-env:global-function-info) '(cleavir-env:special-variable-info))
  (clos:satiate #'cleavir-env:ignore
                '(cleavir-env:variable-ignore)
                '(cleavir-env:lexical-variable-info)
                '(cleavir-env:local-function-info) '(cleavir-env:global-function-info))
  (macrolet ((for-entries (name)
               `(clos:satiate #',name '(null) '(clasp-cleavir:clasp-global-environment)
                              ,@(mapcar (lambda (class)
                                          `'(,class))
                                        (clos:subclasses* (find-class 'cleavir-env::entry))))))
    (for-entries cleavir-env:global-environment)
    (for-entries cleavir-env:declarations)
    (for-entries cleavir-env:compile-time)))

;;; cleavir-generate-ast
#-cst
(eval-when (:load-toplevel)
  ;; convert-special is probably the most involved.
  (macrolet ((satiate-special ()
               (let* ((special-operators
                        ;; Just grab them from the function itself, rather than maintain another damn list
                        (loop for method
                                in (clos:generic-function-methods #'cleavir-generate-ast:convert-special)
                              for specs = (clos:method-specializers method)
                              for espec = (first specs)
                              when (typep espec 'clos:eql-specializer)
                                collect espec))
                      ;; ENTRY itself is abstract, so we can exclude it (with REST)
                      (entries (rest (clos:subclasses* (find-class 'cleavir-env::entry))))
                      (lists (loop for sp in special-operators
                                   nconcing (loop for entry in entries
                                                  collect `'(,sp cons ,entry clasp-cleavir::clasp-64bit)))))
                 `(clos:satiate #'cleavir-generate-ast:convert-special ,@lists))))
    (satiate-special))
  ;; check-special-form-syntax is rather involved as well, though.
  (macrolet ((satiate-check ()
               (let ((special-operators
                       (loop for method
                               in (clos:generic-function-methods #'cleavir-generate-ast:check-special-form-syntax)
                             for specs = (clos:method-specializers method)
                             for espec = (first specs)
                             when (typep espec 'clos:eql-specializer)
                               collect espec)))
                 `(clos:satiate #'cleavir-generate-ast:check-special-form-syntax
                                ,@(loop for sp in special-operators
                                        collect `'(,sp cons))))))
    (satiate-check)))
