(declaim (optimize (debug 3)))


(load "prepare_af_method_templates.lisp")



(defparameter *maxarity* 16) ;; 10

(with-open-file (sout (si:argv 3) :direction :output :if-exists :supersede)
  (format sout "#ifdef af_external_methodptrt_H~%")
  (do ((i *maxarity* (1- i)))
   ((< i 0))
    (create-methodPtrT-class sout "RT" i *maxarity* :master (eql i *maxarity*) :external "ExternalObject")
    (create-methodPtrT-class sout "void" i *maxarity* :external "ExternalObject" )
    (create-methodPtrT-class sout "RT" i *maxarity* :master (eql i *maxarity*) :const "const" :external "ExternalObject" )
    (create-methodPtrT-class sout "void" i *maxarity* :const "const" :external "ExternalObject" )
    (create-methodPtrT-class sout "RT" i *maxarity* :master (eql i *maxarity*) :external "ExternalIndirect")
    (create-methodPtrT-class sout "void" i *maxarity* :external "ExternalIndirect")
    (create-methodPtrT-class sout "RT" i *maxarity* :master (eql i *maxarity*) :const "const" :external "ExternalIndirect" )
    (create-methodPtrT-class sout "void" i *maxarity* :const "const" :external "ExternalIndirect" )
    )

  (create-methodptr-functoid sout *maxarity* :external "ExternalObject")
  (create-methodptr-functoid sout *maxarity* :const "const" :external "ExternalObject")
  (create-methodptr-functoid sout *maxarity* :external "ExternalIndirect")
  (create-methodptr-functoid sout *maxarity* :const "const" :external "ExternalIndirect")

  (format sout "#endif // af_external_methodptrt_H~%")

  (format sout "#ifdef af_external_method_def_H~%")
  (dotimes (i (1+ *maxarity*)) (create-def sout i :external "ExternalObject"))
  (dotimes (i (1+ *maxarity*)) (create-def sout i :const "const" :external "ExternalObject"))
  (dotimes (i (1+ *maxarity*)) (create-def sout i :external "ExternalIndirect"))
  (dotimes (i (1+ *maxarity*)) (create-def sout i :const "const" :external "ExternalIndirect"))
;;  (dotimes (i 4) (create-def-accessor sout i))
  (format sout "#endif // af_external_method_def_H~%")
)
