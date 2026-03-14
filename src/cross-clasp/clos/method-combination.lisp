(in-package #:cross-clasp.clasp.clos)

(defun invalid-qualified (methods allowed empty-ok-p)
  (loop for method in methods
        for qual = (qualifiers method)
        unless (or (and empty-ok-p (null qual))
                 (and (= (length qual) 1)
                   (member (first qual) allowed)))
          collect method))

(defun check-qualifiers (generic methods allowed &optional (empty-ok-p t))
  (let ((invalid (invalid-qualified methods allowed empty-ok-p)))
    (unless (null invalid)
      (error "Invalid qualifiers for methods on ~s: ~s"
             (name generic) invalid))))

(defun qualified-methods (qualifiers methods)
  (loop for method in methods
        when (equal qualifiers (qualifiers method))
          collect method))

(defun compute-standard-effective-method (gf methods)
  (check-qualifiers gf methods '(:around :before :after))
  (let* ((around (qualified-methods '(:around) methods))
         (before (qualified-methods '(:before) methods))
         (primary (qualified-methods () methods))
         (after (qualified-methods '(:after) methods)))
    (when (null primary)
      (error "Missing primary methods on ~s" (name gf)))
    (flet ((call-methods (methods)
             (loop for method in methods
                   collecting `(call-method ,method))))
      (let* ((wprimary `(call-method ,(first primary) (,@(rest primary))))
             (wafter (if after
                         `(multiple-value-prog1 ,wprimary ,(call-methods after))
                         wprimary))
             (wbefore (if before
                          `(progn ,(call-methods before) ,wafter)
                          wafter))
             (waround (cond ((not around) wbefore)
                            ((or before after)
                             `(call-method ,(first around)
                                           (,@(rest around)
                                            (make-method ,wbefore))))
                            (t `(call-method ,(first around)
                                             (,@(rest around) ,@primary))))))
        waround))))

(defun compute-effective-method (generic methods)
  (let ((mc (gf-method-combination generic)))
    (unless (and (eq (name mc) 'standard) (null (options mc)))
      (error "Method combination (~s~{~s~^ ~}) unsupported"
             (name mc) (options mc)))
    (compute-standard-effective-method generic methods)))

(defmacro with-effective-method-parameters ((&rest spreadable) &body body)
  `(symbol-macrolet ((+emf-params+ (,@spreadable))) ,@body))

(defun emf-params (env) (cross-clasp:build-macroexpand-1 '+emf-params+ env))

;;; This is only present during build. So we skip details. In particular,
;;; we don't make an actual method object for make-method, because we know
;;; our methods never examine their next-methods as methods.
;;; We don't keep method objects around at all - this makes dumping
;;; call-method forms much easier.
(defmacro %call-method (method &optional next-methods &environment env)
  (etypecase method
    (effective-reader
     (let* ((args (emf-params env))
            (arg (if (rest args) (first args) `(first ,(first args))))
            (valuef
              (ecase (allocation method)
                ((:instance) `(standard-instance-access arg ',(location method))))))
       `(let* ((arg ,arg)
               (value ,valuef))
          (if (eq value (core:unbound))
              (slot-unbound (class-of arg) arg ',(name (slot (original method))))
              value))))
    (effective-writer
     (let* ((args (emf-params env))
            (value (if (rest args) (first args) `(first ,(first args))))
            (obj (cond ((null (rest args)) `(second ,(first args)))
                       ((null (cddr args)) `(first ,(second args)))
                       (t (second args)))))
       (ecase (allocation method)
         ((:instance)
          `(setf (standard-instance-access ,obj ',(location method)) ,value)))))
    (compiler-accessor (error "Unreplaced accessor method: ~s" method))
    ((cons (eql make-method) (cons t null)) (second method))
    (compiler-method
     (ecase (first (method-function method))
       ((leaf)
        ;; Some (leaf ...) will not have a function form - those for accessors -
        ;; but they ought to be handled in the above cases.
        (assert (third (method-function method)))
        `(apply ,(third (method-function method)) ,@(emf-params env)))
       ((contf)
        `(apply ,(third (method-function method))
                ,(if (null next-methods)
                     `(lambda (&rest args)
                        (declare (ignore args))
                        (error "BUG: no next method"))
                     (let* ((args (emf-params env))
                            (req (butlast args))
                            (rest (first (last args))))
                       `(lambda (,@req ,@(when rest `(&rest ,rest)))
                          (call-method ,(first next-methods)
                                       ,(rest next-methods)))))
                ,@(emf-params env)))))))

