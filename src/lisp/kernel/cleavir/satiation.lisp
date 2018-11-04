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
