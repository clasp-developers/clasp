(make-package "temp")
(in-package "temp")
(COMMON-LISP:use-package "COMMON-LISP")

(print "running")
(print (package-shadowing-symbols "temp"))
(do-symbols (v) (print v))