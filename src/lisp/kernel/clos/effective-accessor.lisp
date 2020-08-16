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
         (si:instance-set object location new)))))

(defun make-effective-reader-function (location slot-name)
  (make-%method-function-fast
   (if (consp location)
       (lambda (object)
         (declare (core:lambda-name
                   effective-class-reader))
         (let ((val (car location)))
           (if (cleavir-primop:eq val (core:unbound))
               (slot-unbound (class-of val) object slot-name)
               val)))
       (lambda (object)
         (declare (core:lambda-name
                   effective-instance-reader))
         (let ((val (si:instance-ref object location)))
           (if (cleavir-primop:eq val (core:unbound))
               (slot-unbound (class-of val) object slot-name)
               val))))))

;;; Hash tables from (direct-slotd . location) to methods.
;;; This is important so that comparing methods to get identical
;;; effective methods works.
;;; FIXME: Should be weak-value.
(defvar *cached-effective-readers* (make-hash-table :test #'equal))
(defvar *cached-effective-writers* (make-hash-table :test #'equal))

(defun intern-effective-reader (method location)
  (let* ((direct-slotd (accessor-method-slot-definition method))
         (key (cons direct-slotd location)))
    (or (gethash key *cached-effective-readers*)
        (setf (gethash key *cached-effective-readers*)
              (make-effective-accessor-method
               (find-class 'effective-reader-method)
               method location
               (make-effective-reader-function
                location (slot-definition-name direct-slotd)))))))

(defun early-intern-effective-reader (method location)
  (with-early-accessors (+standard-accessor-method-slots+
                         +slot-definition-slots+)
    (let* ((direct-slotd (accessor-method-slot-definition method))
           (key (cons direct-slotd location)))
      (or (gethash key *cached-effective-readers*)
          (setf (gethash key *cached-effective-readers*)
                (make-effective-accessor-method
                 (find-class 'effective-reader-method)
                 method location
                 (make-effective-reader-function
                  location (slot-definition-name direct-slotd))))))))

(defun intern-effective-writer (method location)
  (let* ((direct-slotd (accessor-method-slot-definition method))
         (key (cons direct-slotd location)))
    (or (gethash key *cached-effective-writers*)
        (setf (gethash key *cached-effective-writers*)
              (make-effective-accessor-method
               (find-class 'effective-writer-method)
               method location
               (make-effective-writer-function location))))))

(defun early-intern-effective-writer (method location)
  (with-early-accessors (+standard-accessor-method-slots+
                         +slot-definition-slots+)
    (let* ((direct-slotd (accessor-method-slot-definition method))
           (key (cons direct-slotd location)))
      (or (gethash key *cached-effective-writers*)
          (setf (gethash key *cached-effective-writers*)
                (make-effective-accessor-method
                 (find-class 'effective-writer-method)
                 method location
                 (make-effective-writer-function location)))))))
