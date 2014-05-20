
(format t "*load-truename* = ~S~%" *load-truename*)
(format t "*load-pathname* = ~S~%" *load-pathname*)

(format t "(eq *load-pathname* *load-truename*) = ~A~%" (eq *load-truename* *load-pathname*))
