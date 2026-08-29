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

;;; --- zero-cons fast path -------------------------------------------------------------------
;;; The generic SLOT-VALUE/-BOUNDP path consed ~192 B/call: %FIND-SLOT and the standard
;;; SLOT-VALUE-USING-CLASS method call the metaobject readers SLOT-DEFINITION-NAME /
;;; SLOT-DEFINITION-LOCATION, which are un-optimized STANDARD-READER-METHODs on bootstrap MOP
;;; classes and allocate ~96 B each.  The standard case is routed through the class's
;;; LOCATION-TABLE (name->location, rack slot 17 on STD-CLASS -- hierarchy.lisp) so the hot path
;;; allocates 0.  Fallback to the full MOP dispatch when the class has no populated table
;;; (built-in / unfinalized -> the rack slot is not a hash-table) or when *OPTIMIZE-SLOT-VALUE*
;;; is disabled (e.g. a program defines a custom SLOT-VALUE-USING-CLASS and wants it honored).
(defvar *optimize-slot-value* t)
(defvar *slot-value-not-found* (list '#:not-found))
(defconstant +class-location-table-rack-index+ 17) ; STD-CLASS LOCATION-TABLE slot, hierarchy.lisp:83
(defconstant +stamp-for-instances-rack-index+ 18)  ; STD-CLASS STAMP-FOR-INSTANCES slot, hierarchy.lisp:84

(declaim (inline %instance-fast-table))
(defun %instance-fast-table (object)
  ;; OBJECT's class name->location hash IFF OBJECT is a CURRENT standard instance, else NIL so the
  ;; caller takes the full MOP path.  Read the class rack slots directly (the CLASS-LOCATION-TABLE /
  ;; STAMP-FOR-INSTANCES readers are themselves un-optimized consing metaobject readers).  The
  ;; currency check (instance stamp = class STAMP-FOR-INSTANCES, cf. MAYBE-UPDATE-INSTANCE) is
  ;; MANDATORY: an OBSOLETE instance must take the generic path, whose SLOT-VALUE-USING-CLASS dispatch
  ;; runs the stamp check + UPDATE-INSTANCE; skipping it reads a stale rack (wrong slots on a redefined class).
  (when (core:instancep object)
    (let* ((class (core:instance-class object))
           (rack (core:instance-rack class))
           (table (core:rack-ref rack +class-location-table-rack-index+)))
      (when (and (hash-table-p table)
                 (eql (core:instance-stamp object)
                      (core:rack-ref rack +stamp-for-instances-rack-index+)))
        table))))

(defun %slot-value-generic (class object slot-name)
  (let ((slotd (%find-slot class slot-name)))
    (if slotd
        (slot-value-using-class class object slotd)
        ;; Only the primary value of SLOT-MISSING is returned.
        (values (slot-missing class object slot-name 'slot-value)))))

(defun slot-value (object slot-name)
  (let ((table (and *optimize-slot-value* (%instance-fast-table object))))
    (if table
        (let ((location (gethash slot-name table *slot-value-not-found*)))
          (if (eq location *slot-value-not-found*)
              (%slot-value-generic (class-of object) object slot-name)
              (let ((value (standard-location-access object location)))
                (if (core:sl-boundp value)
                    value
                    (values (slot-unbound (class-of object) object slot-name))))))
        (%slot-value-generic (class-of object) object slot-name))))

(defun %setf-slot-value-generic (value class object slot-name)
  (let ((slotd (%find-slot class slot-name)))
    (if slotd
        (setf (slot-value-using-class class object slotd) value)
        (slot-missing class object slot-name 'setf value)))
  ;; 7.7.12: value of slot-missing is ignored for setf.
  value)

(defun (setf slot-value) (value object slot-name)
  (let ((table (and *optimize-slot-value* (%instance-fast-table object))))
    (if table
        (let ((location (gethash slot-name table *slot-value-not-found*)))
          (if (eq location *slot-value-not-found*)
              (%setf-slot-value-generic value (class-of object) object slot-name)
              (progn (setf (standard-location-access object location) value)
                     value)))
        (%setf-slot-value-generic value (class-of object) object slot-name))))

(defun %slot-boundp-generic (class object slot-name)
  (let ((slotd (%find-slot class slot-name)))
    (if slotd
        (slot-boundp-using-class class object slotd)
        (values (slot-missing class object slot-name 'slot-boundp)))))

(defun slot-boundp (object slot-name)
  (let ((table (and *optimize-slot-value* (%instance-fast-table object))))
    (if table
        (let ((location (gethash slot-name table *slot-value-not-found*)))
          (if (eq location *slot-value-not-found*)
              (%slot-boundp-generic (class-of object) object slot-name)
              (core:sl-boundp (standard-location-access object location))))
        (%slot-boundp-generic (class-of object) object slot-name))))

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
