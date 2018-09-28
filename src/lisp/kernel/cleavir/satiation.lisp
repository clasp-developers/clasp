;;;; Satiate relevant Cleavir functions so that
;;;; startup and the first compile go a little faster.
;;;; (This file is thus essentially optional.)

(in-package #:clasp-cleavir)

;;; Initialization stuff.
;;; Note that we'll include some abstract classes that are never actually instantiated.
;;; Just putting them in the call history is no problem, though.
(defmacro satiate-subclasses*-initialization (class-name)
  `(eval-when (:load-toplevel) ; boot time only
     (macrolet ((frob ()
                  `(clos:satiate-initialization
                    ,@(clos:subclasses* (find-class ',class-name)))))
       (frob))))

;;; AST initialization - helps with inline.lisp speed.
(satiate-subclasses*-initialization cleavir-ast:ast)
