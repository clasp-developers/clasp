(require :asdf)
(asdf:load-system :clasp-cleavir)

(apropos "enter-instruction")

(list-all-packages)
(core:getpid)

(in-package :clasp-cleavir)

(trace cmp::codegen-rtv/all)
(cleavir-compile 't2 '(lambda (x) (* x 2)))
(core:load-time-values-dump-symbols "<compile>" 523)
(t2 16) -> 32

(cleavir-compile 'hyp '(lambda (x y) (sqrt (+ (* x x) (* y y)))))
(hyp 2 3) --> 3.60555

(cleavir-compile 'cmp '(lambda (x y) (if (eq x y) "same" "different")))
(cmp 'a 'a) 

(cleavir-compile 'cloop '(lambda (x) (dotimes (i x) (print i))))
(cloop 10)

(cleavir-compile 'uwpr '(lambda (x) (unwind-protect (print "A") (print "B"))))

(defvar *a* 1)

(ast-form '(lambda (x) (let ((*a* 2)) (format t "inner *a* = ~a~%" *a*)) (format t "outer *a*=~a~%" *a*)))

(trace cleavir-generate-ast:convert-special-binding)

(cleavir-compile 'spectest '(lambda (x) (let ((*a* 2)) (format t "inner *a* = ~a~%" *a*)) (format t "outer *a*=~a~%" *a*)))
(llvm-sys:cxx-data-structures-info)

(core:low-level-backtrace)

(a 1)


(cleavir-compile 'a '(lambda (x y) (let ((res (+ x y))) res)))
(cleavir-compile 'a '(lambda () (multiple-value-bind () nil)))

(a) -->  42324823482938492834982343234234

(core:load-time-values-dump-values "<default>" 1853)
(print cmp::*run-time-literal-holder*)
(apropos "run-time")
(core:load-time-values-ids)

(ast-form '(lambda (x) (+ 1 x)))
(hoisted-ast-form '(lambda (x) (+ 1 (- 123123434182312310 x))))
(hoisted-mir-form '(lambda (x) (+ 1 x)))
(hoisted-hir-form '(lambda (x) #'(lambda (y) (+ x y 1))))
(trace (setf cleavir-ir:predecessors))



(compile 'a '1)

(apropos "run-time-literal")
cmp::*run-time-literals-external-name*
(core:load-time-values-dump "globalRunTime")


(trace cleavir-ast-graphviz:label)

(print "Hello")

(with-output-to-string (s) (loop for c across "\"abcdef\"" do (if (eql c #\") (princ "\"" s) (princ c s))))
(with-output-to-string (s) (loop for c across "\"abcdef\"" do (when (member c '(#\\ #\")) (princ #\\ s)) (princ c s)))

(constantp "this is")


*debug-basic-blocks*

(node-predecessors *hir*)


(print *hir*)
(typep *hir* 'cleavir-ir:enter-instruction)
*hir*

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
