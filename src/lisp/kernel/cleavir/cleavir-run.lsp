

(generate-hir-for-clasp-source)
*hir-single-step*

(draw-hir)
(getpid)
(hir-form '(lambda (x x) (+ x x)))

(hir-form '(let ((y 100) (z 200) ) #'(lambda (x) (list x y z))))

(defparameter *a* 1)
(defparameter *b* 2)

(hir-form '(lambda (*a* *b*) (list *a* *b*)))
(hir-form '(let ((x 100)) (tagbody top (setq x (1- x)) (if (eql x 0) (go done)) (go top) done)))
*hir*

(hir-form 1)

(defun foo (x x) x)

(foo 1 2)
