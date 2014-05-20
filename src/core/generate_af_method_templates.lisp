(load "prepare_af_method_templates.lisp")




(defparameter *maxarity* 10) ;; 10

(with-open-file (sout (si:argv 3) :direction :output :if-exists :supersede)
  (format sout "#ifdef af_methodptrt_H~%")
  (do
   ((i *maxarity* (1- i)))
   ((< i 0))
    (create-methodPtrT-class sout "RT" i *maxarity* :master (eql i *maxarity*)))
  (do
   ((i *maxarity* (1- i)))
   ((< i 0))
    (create-methodPtrT-class sout "void" i *maxarity* ))

  (do
   ((i *maxarity* (1- i)))
   ((< i 0))
    (create-methodPtrT-class sout "RT" i *maxarity* :master (eql i *maxarity*) :const "const"))
  (do
   ((i *maxarity* (1- i)))
   ((< i 0))
    (create-methodPtrT-class sout "void" i *maxarity* :const "const" ))


  (create-methodptr-functoid sout *maxarity*)
  (create-methodptr-functoid sout *maxarity* :const "const")
  (format sout "#endif // af_methodptrt_H~%")

  (format sout "#ifdef af_method_def_H~%")
  (dotimes (i (1+ *maxarity*)) (create-def sout i))
  (dotimes (i (1+ *maxarity*)) (create-def sout i :const "const"))
  (dotimes (i 4) (create-def-accessor sout i))
  (format sout "#endif // af_method_def_H~%")
)





