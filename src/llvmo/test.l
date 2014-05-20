
(defparameter *c* (llvmo::get-global-context))
(defparameter a (llvmo:make-module "test" *c*))
(defparameter b (llvmo:make-builder *c*))
(defparameter f (llvmo:make-apfloat 23.3))
(defparameter g (llvmo:constantfp-get *c* f))


(print a)
(print b)
(print f)
(print g)

(llvmo:createfadd b g g "add")
