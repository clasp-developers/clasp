
(in-package :cmp)

(progn
  (load "sys:kernel;cmp;cmpclutil.lsp")
  (load "sys:kernel;cmp;cmpast.lsp")
  (format t "Done~%"))


(generate-ast '(block foo 1 (if 1 2 3) 4 5 (tagbody (go a) a)) nil)
(ext:source-location 'core::separate-ordinary-body t)
(generate-ast '(let ((x 1) (y 2)) (+ x y)) nil)

(apropos "variable-info")

(apropos "separate-ordinary")

