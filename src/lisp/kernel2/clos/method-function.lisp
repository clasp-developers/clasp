(in-package #:clos)

(defclass %no-next-method-continuation (funcallable-standard-object)
  ()
  (:metaclass funcallable-standard-class))

(defclass %contf-method-function (funcallable-standard-object)
  ((%contf :initarg :contf :accessor contf))
  (:metaclass funcallable-standard-class))

(defun make-%no-next-method-continuation (method)
  (let ((nnmc (early-make-instance %no-next-method-continuation)))
    (set-funcallable-instance-function
     nnmc
     (if (null method)
         (lambda (&rest args)
           (declare (ignore args))
           (error "No next method"))
         (lambda (&rest args)
           (apply #'no-next-method (method-generic-function method) method args))))
    nnmc))

(defun make-%contf-method-function (contf)
  (let ((mf (early-make-instance %contf-method-function
                                 :contf contf)))
    (set-funcallable-instance-function
     mf
     (let (;; FIXME: Method not available yet :(
           (nnmc (make-%no-next-method-continuation nil)))
       (lambda (args next-methods)
         (apply contf
                (if (null next-methods)
                    nnmc
                    (lambda (&rest args)
                      (funcall (method-function (first next-methods))
                               args (rest next-methods))))
                args))))
    mf))

(defun make-%leaf-method-function (fmf)
  (let ((mf (early-make-instance %leaf-method-function
                                 :fmf fmf)))
    (set-funcallable-instance-function
     mf
     (lambda (arguments next-methods)
       (declare (ignore next-methods))
       ;; FIXME: Avoid coerce-fdesignator in apply here
       (apply fmf arguments)))
    mf))
