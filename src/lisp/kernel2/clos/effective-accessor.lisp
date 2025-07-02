(in-package "CLOS")

;;; needed for compute-effective-method
(defmethod method-qualifiers ((method effective-accessor-method))
  (with-early-accessors (effective-accessor-method)
    (method-qualifiers (effective-accessor-method-original method))))
;;; for compute-outcome
(defmethod accessor-method-slot-definition ((method effective-accessor-method))
  (with-early-accessors (effective-accessor-method)
    (accessor-method-slot-definition (effective-accessor-method-original method))))

(defun make-effective-reader-method (method location function)
  (early-make-instance effective-reader-method
                       :original method :location location
                       :function function))

(defun make-effective-writer-method (method location function)
  (early-make-instance effective-writer-method
                       :original method :location location
                       :function function))

(defun make-effective-writer-function (location)
  (make-%leaf-method-function
   (if (consp location)
       (lambda (new object)
         (declare (core:lambda-name
                   effective-class-writer)
                  (ignore object))
         (setf (car location) new))
       (lambda (new object)
         (declare (core:lambda-name
                   effective-instance-writer))
         ;; FIXME: funcallable-
         (setf (standard-instance-access object location) new)))))

(defun make-effective-reader-function (location slot-name)
  (make-%leaf-method-function
   (if (consp location)
       (lambda (object)
         (declare (core:lambda-name
                   effective-class-reader))
         (let ((val (car location)))
           (if (eq val (core:unbound))
               (slot-unbound (class-of object) object slot-name)
               val)))
       (lambda (object)
         (declare (core:lambda-name
                   effective-instance-reader))
         (let ((val (standard-instance-access object location)))
           (if (eq val (core:unbound))
               (slot-unbound (class-of object) object slot-name)
               val))))))

;;; These can be called (through final-methods) from many threads simultaneously
;;; and so must be thread-safe.

(defun intern-effective-reader (method location)
  ;; We could write things in terms of slot-value but that would require some
  ;; nontrivial macro work to get CAS of slot-value. FIXME
  (with-early-accessors (standard-direct-slot-definition)
    (loop with direct-slotd = (accessor-method-slot-definition method)
          with table = (mp:atomic (%effective-readers direct-slotd))
          for existing = (cdr (assoc location table))
          when existing
            return existing
          do (let* ((eff
                      (make-effective-reader-method
                       method location
                       (make-effective-reader-function
                        location (slot-definition-name direct-slotd))))
                    (new-table
                      (mp:cas (%effective-readers direct-slotd)
                              table (acons location eff table))))
               (if (eq new-table table)
                   (return eff)
                   (setf table new-table))))))

(defun intern-effective-writer (method location)
  (with-early-accessors (standard-direct-slot-definition)
    (loop with direct-slotd = (accessor-method-slot-definition method)
          for table = (mp:atomic (%effective-writers direct-slotd))
          for existing = (cdr (assoc location table))
          when existing
            return existing
          do (let* ((eff
                      (make-effective-writer-method
                       method location
                       (make-effective-writer-function location)))
                    (new-table
                      (mp:cas (%effective-writers direct-slotd)
                              table (acons location eff table))))
               (if (eq table new-table)
                   (return eff)
                   (setf table new-table))))))
