(clos:add-extra-classes)
(defclass xxx (ast-tooling:b-adaptor) (x))
(defparameter *a* (make-instance 'xxx))
(load "sys:kernel;lsp;defvirtual.lsp")
(core:defvirtual ast-tooling:x ((x xxx)) (print "in ast-tooling:x for xxx"))

;;(ast-tooling:dox *a*)
