(in-package :clos)


;;; Move documentation for C++ objects into the correct places
;;; Add documentation for C++ classes
#|
(let ((documentation (car core:*documentation-pool*)))
  (maphash (lambda (key value)
             (loop for entry in value
                   do (loop for docs in value
                            for header = (car docs)
                            for docstring = (cdr docs)
                            when (equal header '(documentation . class))
                              do (let ((class (if (symbolp key)
                                                  (find-class key)
                                                  (if (classp key)
                                                      key
                                                      (error "key ~s is not a class" key)))))
                                   (setf (documentation class t) 
                                                    (
                              do (setf (documentation (find-class 
                            do (format t "       docs -> ~s~%" docs))))
           documentation))
             
|#
