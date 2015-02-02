(require :asdf)


(asdf:load-system :clasp-cleavir)

(getpid)

(in-package :clasp-cleavir)

(cleavir-compile nil '(lambda () 1))

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
