(in-package :cl-user)
(print *package*)
(cl:format t "Hi there ~a~%" (length (clos::all-generic-functions)))
(cl:format t "There are ~a generic functions~%" (length (clos::all-generic-functions)))
(dolist (gf (clos::all-generic-functions))
  (cl:format t "~10a  ~10a -> ~s~%"
             (length (clos::generic-function-call-history gf))
             (length (clos::generic-function-methods gf))
             (clos::generic-function-name gf)))
