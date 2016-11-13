(in-package #:cleavir-llvm-ir)

(defmacro with-ir-function ((lisp-function-name
			     &key (function-type +fn-prototype+ function-type-p)
			     (linkage 'llvm-sys:internal-linkage))
			       &rest body)
  (let ((fn-gs (gensym "FUNCTION-")))
    `(let (,fn-gs (cmp:irc-function-create 
		   function-type
		    linkage
		    (jit-function-name ,function-name)
		    *the-module*))
       ,@body
       ,fn-gs)))
