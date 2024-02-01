(in-package "CLOS")

(defun make-effective-accessor-method (class method location function)
  (with-early-make-instance +effective-accessor-method-slots+
    (dam class
         :function function
         :original method
         :location location)
    dam))

(defun make-effective-writer-function (location)
  (make-%method-function-fast
   (if (consp location)
       (lambda (new object)
         (declare (core:lambda-name
                   effective-class-writer)
                  (ignore object))
         (setf (car location) new))
       (lambda (new object)
         (declare (core:lambda-name
                   effective-instance-writer))
         (setf (si:instance-ref object location) new)))))

(defun make-effective-reader-function (location slot-name)
  (make-%method-function-fast
   (if (consp location)
       (lambda (object)
         (declare (core:lambda-name
                   effective-class-reader))
         (let ((val (car location)))
           (if (cleavir-primop:eq val (core:unbound))
               (slot-unbound (class-of object) object slot-name)
               val)))
       (lambda (object)
         (declare (core:lambda-name
                   effective-instance-reader))
         (let ((val (si:instance-ref object location)))
           (if (cleavir-primop:eq val (core:unbound))
               (slot-unbound (class-of object) object slot-name)
               val))))))

;;; moved here for bootstrapping reasons.
;;; functions are defined a bit later in slotvalue.lisp.
#+threads
(mp:define-atomic-expander slot-value-using-class (class object slotd)
  (&rest keys)
  "Same requirements as STANDARD-INSTANCE-ACCESS, except the slot can have
allocation :class.
Also, methods on SLOT-VALUE-USING-CLASS, SLOT-BOUNDP-USING-CLASS, and
(SETF SLOT-VALUE-USING-CLASS) are ignored (not invoked).
In the future, the CAS behavior may be customizable with a generic function."
  (declare (ignore keys))
  (let ((gclass (gensym "CLASS")) (gobject (gensym "OBJECT"))
        (gslotd (gensym "SLOTD")) (oldv (gensym "OLD")) (newv (gensym "NEWV")))
    (values (list gclass gobject gslotd) (list class object slotd) oldv newv
            `(atomic-slot-value-using-class ,gclass ,gobject ,gslotd)
            `(setf (atomic-slot-value-using-class ,gclass ,gobject ,gslotd)
                   ,newv)
            `(cas-slot-value-using-class ,oldv ,newv
                                         ,gclass ,gobject ,gslotd))))

;;; These can be called (through final-methods) from many threads simultaneously
;;; and so must be thread-safe.

(defun intern-effective-reader (method location)
  (loop with direct-slotd = (accessor-method-slot-definition method)
        for table = (mp:atomic (slot-value direct-slotd '%effective-readers))
        for existing = (cdr (assoc location table))
        when existing
          return existing
        do (let ((eff
                   (make-effective-accessor-method
                    (find-class 'effective-reader-method)
                    method location
                    (make-effective-reader-function
                     location (slot-definition-name direct-slotd)))))
             (when (eq (mp:cas (slot-value direct-slotd '%effective-readers)
                               table (acons location eff table))
                       table)
               (return eff)))))

(defun early-intern-effective-reader (method location)
  (with-early-accessors (+standard-accessor-method-slots+
                         +direct-slot-definition-slots+)
    (loop with direct-slotd = (accessor-method-slot-definition method)
          for table = (mp:atomic (%direct-slotd-effective-readers direct-slotd))
          for existing = (cdr (assoc location table))
          when existing
            return existing
          do (let ((eff
                     (make-effective-accessor-method
                      (find-class 'effective-reader-method)
                      method location
                      (make-effective-reader-function
                       location (slot-definition-name direct-slotd)))))
               (when (eq (mp:cas (%direct-slotd-effective-readers direct-slotd)
                                 table (acons location eff table))
                         table)
                 (return eff))))))

(defun intern-effective-writer (method location)
  (loop with direct-slotd = (accessor-method-slot-definition method)
        for table = (mp:atomic (slot-value direct-slotd '%effective-writers))
        for existing = (cdr (assoc location table))
        when existing
          return existing
        do (let ((eff
                   (make-effective-accessor-method
                    (find-class 'effective-writer-method)
                    method location
                    (make-effective-writer-function location))))
             (when (eq (mp:cas (slot-value direct-slotd '%effective-writers)
                               table (acons location eff table))
                       table)
               (return eff)))))

(defun early-intern-effective-writer (method location)
  (with-early-accessors (+standard-accessor-method-slots+
                         +direct-slot-definition-slots+)
    (loop with direct-slotd = (accessor-method-slot-definition method)
          for table = (mp:atomic (%direct-slotd-effective-writers direct-slotd))
          for existing = (cdr (assoc location table))
          when existing
            return existing
          do (let ((eff
                     (make-effective-accessor-method
                      (find-class 'effective-writer-method)
                      method location
                      (make-effective-writer-function location))))
               (when (eq (mp:cas (%direct-slotd-effective-writers direct-slotd)
                                 table (acons location eff table))
                         table)
                 (return eff))))))
