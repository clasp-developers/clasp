(trace clos::compute-applicable-methods-using-classes)
(trace cl:compute-applicable-methods)
(trace clos::std-compute-applicable-methods)
(trace clos::std-compute-effective-method)
(trace clos::method-combination-compiler)
(trace clos::method-combination-options)
(trace clos::standard-compute-effective-method)
(trace clos::standard-main-effective-method)
(trace clos::effective-method-function)

(defmethod bar ((x integer)) (print "i"))
(defmethod bar ((x string)) (print "s"))
(defmethod bar ((x complex)) (print "c"))

(clear-gfun-hash t)

(list-all-packages)


(defmethod x (a b))
(defmethod x :around (a b) (call-next-method))
(defmethod x :before (a b))
(trace clos::standard-compute-effective-method)
(trace clos::standard-main-effective-method)
(clear-gfun-hash t)
(format t "About to call method~%")
(x 1 2)
