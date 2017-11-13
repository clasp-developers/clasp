(defun coerce:function-designator (fdesignator)
  (etypecase fdesignator
    (symbol (symbol-function fdesignator))
    (function fdesignator)))

#+(or)
(define-compiler-macro coerce:function-designator (&whole whole fdesignator)
  (if (and (consp fdesignator)
           (consp (cdr fdesignator))
           (null (cddr fdesignator)))
      (cond ((eq (car fdesignator) 'quote)
             `(symbol-function ,fdesignator))
            ((eq (car fdesignator) 'function)
             fdesignator)
            (t whole))
      whole))

(defun coerce:string-designator (sdesignator)
  (etypecase sdesignator
    (string sdesignator)
    (symbol (symbol-name sdesignator))
    (character (make-string 1 :initial-element sdesignator))))

(defun coerce:package-designator (package-designator)
  (if (packagep package-designator)
      package-designator
      (find-package (coerce-string-designator package-designator))))

(defun coerce:list-designator (list-designator)
  (typecase list-designator
    (null list-designator)
    (cons list-designator)
    (t (list list-designator))))

(defun coerce:class-designator (class-designator)
  (etypecase class-designator
    (symbol (find-class class-designator))
    (class class-designator)))

;;; CLHS 9.1.2.1
(defun coerce:condition (simple-type datum &rest arguments)
  (etypecase datum
    (symbol
     (apply #'make-condition datum arguments))
    ((or string function) ; format control
     (make-condition simple-type
                     :format-control datum
                     :format-arguments arguments))
    (condition
     (assert (null arguments))
     datum)))
