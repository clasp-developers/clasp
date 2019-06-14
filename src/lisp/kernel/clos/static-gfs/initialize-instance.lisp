(in-package #:static-gfs)

(defun initialize-instance-form (class iform keys params)
  (let ((patch-list
          (list
           (cons (find-method #'initialize-instance nil (list (find-class 't)))
                 #'standard-initialize-instance-form)))
        (methods (compute-applicable-methods #'initialize-instance
                                             (list (clos:class-prototype class)))))
    (if (can-static-effective-method-p methods patch-list)
        (static-effective-method
         #'initialize-instance methods (list class iform keys params) patch-list
         (list* iform (reconstruct-arguments keys params)))
        (default-initialize-instance-form iform keys params))))

(defun default-initialize-instance-form (iform keys params)
  `(locally
       (declare (notinline initialize-instance))
     (initialize-instance ,iform ,@(reconstruct-arguments keys params))))

(defun standard-initialize-instance-form (class iform keys params)
  (shared-initialize-form class iform keys params))
