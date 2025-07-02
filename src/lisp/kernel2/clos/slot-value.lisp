(in-package #:clos)

;;; This works on both class locations (conses) and instance ones.
(defun standard-location-access (instance location)
  (if (core:fixnump location)
      (core:rack-ref (core:instance-rack instance) location)
      (car location)))

(defun (setf standard-location-access) (val instance location)
  (if (core:fixnump location)
      (setf (core:rack-ref (core:instance-rack instance) location) val)
      (setf (car location) val)))

;; FIND is not defined yet so we use this.
(defun %find-slot (class slot-name)
  (loop for prospect in (class-slots class)
        for prospect-name = (slot-definition-name prospect)
        when (eql prospect-name slot-name)
          return prospect))

(defun slot-value (object slot-name)
  (let* ((class (class-of object))
         (slotd (%find-slot class slot-name)))
    (if slotd
        (slot-value-using-class class object slotd)
        ;; Only the primary value of SLOT-MISSING is returned.
        (values (slot-missing class object slot-name 'slot-value)))))

(defun (setf slot-value) (value object slot-name)
  (let* ((class (class-of object))
         (slotd (%find-slot class slot-name)))
    (if slotd
        (setf (slot-value-using-class class object slotd) value)
        (slot-missing class object slot-name 'setf value)))
  ;; 7.7.12: value of slot-missing is ignored for setf.
  value)

(defun slot-boundp (object slot-name)
  (let* ((class (class-of object))
         (slotd (%find-slot class slot-name)))
    (if slotd
        (slot-boundp-using-class class object slotd)
        (values (slot-missing class object slot-name 'slot-boundp)))))

(defun slot-makunbound (object slot-name)
  (let* ((class (class-of object))
         (slotd (%find-slot class slot-name)))
    (if slotd
        (slot-makunbound-using-class class object slotd)
        (slot-missing class object slot-name 'slot-makunbound)))
  object)

(defgeneric slot-value-using-class (class object slot-definition))
(defgeneric (setf slot-value-using-class) (value class object slot-definition))
(defgeneric slot-boundp-using-class (class object slot-definition))
(defgeneric slot-makunbound-using-class (class object slot-definition))

(defmethod slot-value-using-class ((class std-class) object slotd)
  (let* ((location (slot-definition-location slotd))
         (value (standard-location-access object location)))
    (if (core:sl-boundp value)
        value
        (values (slot-unbound class object (slot-definition-name slotd))))))

(defmethod (setf slot-value-using-class) (value (class std-class) object slotd)
  (setf (standard-location-access object (slot-definition-location slotd)) value))

(defmethod slot-boundp-using-class (class object slotd)
  (core:sl-boundp (standard-location-access object (slot-definition-location slotd))))

(defmethod slot-makunbound-using-class (class object slotd)
  (setf (standard-location-access object (slot-definition-location slotd))
        (core:unbound)))

(defgeneric slot-missing (class object slot-name operation &optional new-value))
(defmethod slot-missing ((class t) object slot-name operation &optional new-value)
  (declare (ignore operation new-value class))
  (error "~a is not a slot of ~a" slot-name object))

(defgeneric slot-unbound (class instance slot-name))
(defmethod slot-unbound ((class t) instance slot-name)
  (error 'unbound-slot :instance instance :name slot-name))

(defun slot-exists-p (object slot-name)
  (find slot-name (class-slots (class-of object)) :key #'slot-definition-name))

(defmacro with-slots (slot-entries instance-form &body body)
  (let* ((temp (gensym))
	 (accessors
	  (do ((scan slot-entries (cdr scan))
	       (res))
	      ((null scan) (nreverse res))
            (let ((entry (first scan)))
              (ext:with-current-source-form (entry)
                (etypecase entry
                  (symbol
                   (push `(,entry (slot-value ,temp ',entry)) res))
                  ((cons symbol (cons symbol null))
                   (push `(,(first entry)
                           (slot-value ,temp ',(second entry)))
                         res))))))))
    `(let ((,temp ,instance-form))
       (symbol-macrolet ,accessors ,@body))))

(defmacro with-accessors (slot-accessor-pairs instance-form &body body)
  (let* ((temp (gensym))
	 (accessors (do ((scan slot-accessor-pairs (cdr scan))
                         (res))
                        ((null scan) (nreverse res))
                      (let ((entry (car scan)))
                        (ext:with-current-source-form (entry)
                          (unless (and (listp entry)
                                       (= (length entry) 2))
                            (error "Malformed WITH-ACCESSORS syntax."))
                          (push `(,(car entry) (,(cadr entry) ,temp)) res))))))
    `(let ((,temp ,instance-form))
       (symbol-macrolet ,accessors ,@body))))
