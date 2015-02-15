(require :asdf)

(print "Hello")


(asdf:load-system :clasp-cleavir)

(in-package :clasp-cleavir)


(ast-form '(lambda (x) (+ 1 x)))
(hoisted-ast-form '(lambda (x) (+ 1 (- 123123434182312310 x))))
(hoisted-hir-form '(lambda (x) (+ 1 x)))

(hoisted-hir-form '(lambda (x) #'(lambda (y) (+ x y))))

(trace (setf cleavir-ir:predecessors))

(cleavir-compile 'a '(function (lambda (x) #'(lambda (y) (+ x y 1)))))

*debug-basic-blocks*

(node-predecessors *hir*)


(print *hir*)
(typep *hir* 'cleavir-ir:enter-instruction)
*hir*q

(apropos "enter-instruction")


(class-of *hir*)
(print *debug-basic-blocks*)

(cleavir-basic-blocks:basic-blocks *hir*)


(defun node-predecessors (top)
  (let ((table (make-hash-table :test #'eq)))
    (labels ((traverse (node)
	       (when (null (gethash node table))
		 (setf (gethash node table) t)
		 (format t "node: ~a   predecessors: ~a~%" node (cleavir-ir:predecessors node))
		 (let ((succs (cleavir-ir:successors node)))
		   (if (typep node 'cleavir-ir:unwind-instruction)
		       (traverse (first succs))
		       (loop for successor in (cleavir-ir:successors node)
			  do (traverse successor)))
		   (when (typep node 'cleavir-ir:enclose-instruction)
		     (traverse (cleavir-ir:code node)))))))
      (traverse top))))



(defun node-with-write-cell-predecessors (top)
  (let ((table (make-hash-table :test #'eq)))
    (labels ((traverse (node)
	       (when (null (gethash node table))
		 (setf (gethash node table) t)
		 (format t "node: ~a   predecessors: ~a~%" node (cleavir-ir:predecessors node))
		 (let ((succs (cleavir-ir:successors node)))
		   (if (typep node 'cleavir-ir:unwind-instruction)
		       (traverse (first succs))
		       (loop for successor in (cleavir-ir:successors node)
			  do (traverse successor)))
		   (when (typep node 'cleavir-ir:enclose-instruction)
		     (traverse (cleavir-ir:code node)))))))
      (traverse top))))


(node-predecessors *hir*)


(typep *hir* 'cleavir-ir:enter-instruction)

(apropos "debug-basic-blocks")

clasp-cleavir:*basic-blocks*

(core:debug-hash-table t)
(core:getpid)

(defclass foo () ())
(defparameter v1 (make-instance 'foo))
(defparameter v2 (make-instance 'foo))
(defparameter ht (make-hash-table :test #'equal))
(setf (gethash (cons v1 v2) ht) '1-2)
(setf (gethash (cons v1 v1) ht) '1-1)
(setf (gethash (cons v2 v1) ht) '2-1)
(setf (gethash (cons v2 v2) ht) '2-2)
(progn (core:debug-hash-table t)(prog1 (gethash (cons v2 v1) ht) (core:debug-hash-table nil))) ; --> |2-1|

(gethash (cons v1 v1) ht) ; --> |1-1|



ht
ht

(untrace gethash)

core:*assert-failure-test-form*


(format t "~S" (every (lambda (x) )))
(core::assert-failure '(every (lambda (x))))
(apropos "assert-failure")
(apropos "*print-")

(pprint '(progn 
	  (load "testing") 
	  (defclass foo () ((a :initarg :a :accessor foo-a)))
	  (defun foo () (assert (every (lambda (x) (typep x 'integer)) '(1 2 3 a))))
	  (apropos "enter-instruction")))
(find-class 'cleavir-ir:top-level-enter-instruction)#<COMMON-LISP:STANDARD-CLASS CLEAVIR-IR:TOP-LEVEL-ENTER-INSTRUCTION 0x10db804c8>


(core:getpid)
(print "Hello")

(in-package :clasp-cleavir)

(cleavir-compile nil '(lambda (x) (+ x x)))


(llvm-sys:dump cmp::*the-module*)


cmp::*dbg-current-file*
cmp::*dbg-generate-dwarf*

(apropos "cleavir-compile")

(apropos "cleavir")


(apropos "internal-linkage")

(core:low-level-backtrace)


(asdf:load-system :clasp-cleavir)

(generate-hir-for-clasp-source)
*hir-single-step*

(hir-form 1)
(translate *hir*)

*basic-blocks*
*tags*
*vars*




(hir-form '(lambda (x &optional (y 0)) (+ x y)))

(draw-hir)

(draw-mir)

*mir*


(core:getpid)

(hir-form '(let ((y 100) (z 200) ) #'(lambda (x) (list x y z))))

(defparameter *a* 1)
(defparameter *b* 2)

(mir-form '(lambda (x y) (list x y)))
(hir-form '(let ((x 100)) (tagbody top (setq x (1- x)) (if (eql x 0) (go done)) (go top) done)))
*hir*

(hir-form 1)

(defun foo (x x) x)

(foo 1 2)
