

(generate-hir-for-clasp-source)
*hir-single-step*

(draw-hir)

(hir-form '(core:*fset 'a (lambda (x y) (+ x y))))

(hir-form '(lambda (x) x))

(defparameter *a* 1)

(hir-form '(let ((*a* 1) (y 2)) (+ *a* y)))
*hir*
(cleavir-ir:enter-instruction
