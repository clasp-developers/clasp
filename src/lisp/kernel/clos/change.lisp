;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOS -*-
;;;;
;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "CLOS")

;;; ----------------------------------------------------------------------
;;; INSTANCE UPDATE PROTOCOL
;;;
;;;
;;; PART 1: CHANGING THE CLASS OF AN INSTANCE
;;;
;;; The method CHANGE-CLASS performs most of the work.
;;;
;;;     a) A copy of the instance, sharing its class and rack, is
;;;        created, along with a new rack.
;;;     b) The new rack's slots are filled from the old rack's slots.
;;;        Only the name is used, so that a new local slot may get the
;;;        value of old slots regardless of those slots' allocation.
;;;     c) The instance's rack is atomically set to the new rack,
;;;        ensuring that instance racks are always self consistent.
;;;        The class of the instance is also changed; it's likely
;;;        some problems are possible if CHANGE-CLASS is called on
;;;        the same object in multiple threads simultaneously (FIXME).
;;;	d) Finally, UPDATE-INSTANCE-FOR-DIFFERENT-CLASS is invoked
;;;	   with a copy of the instance as it looked before the change,
;;;	   the changed instance and enough information to perform any
;;;	   extra processing.
;;;

;;; NOTE: Right now this works for funcallable instances,
;;; but if funcallable instances e.g. had the rack in a different place,
;;; it would be n.g.
(defmethod update-instance-for-different-class
    ((old-data standard-object) (new-data standard-object) &rest initargs)
  (declare (dynamic-extent initargs))
  (let ((added-slots
          (loop with old-slotds = (si:instance-sig old-data)
                for new-slotd in (si:instance-sig new-data)
                for new-slotd-name = (slot-definition-name new-slotd)
                when (and (eq (slot-definition-allocation new-slotd)
                              :instance)
                          (not (member new-slotd-name old-slotds
                                       :key #'slot-definition-name
                                       :test #'eq)))
                  collect new-slotd-name)))
    (when initargs
      (check-initargs-uncached
       (class-of new-data) initargs
       (list (list #'update-instance-for-different-class
                   (list old-data new-data))
             (list #'shared-initialize (list new-data added-slots)))))
    (apply #'shared-initialize new-data added-slots initargs)))

;;; Mutate new-rack based on old-rack, for change-class.
;;; Abstracted out so it can be used regardless of instance structure.
(defun change-class-aux (old-rack new-rack old-class old-instance)
  (let ((old-slotds (si:rack-sig old-rack))
        (new-slotds (si:rack-sig new-rack)))
    ;; "The values of local slots specified by both the class Cto and
    ;; Cfrom are retained.  If such a local slot was unbound, it remains
    ;; unbound."
    ;; "The values of slots specified as shared in the class Cfrom and
    ;; as local in the class Cto are retained."
    (dolist (new-slotd new-slotds)
      (when (eq (slot-definition-allocation new-slotd) :instance)
        (let* ((name (slot-definition-name new-slotd))
               (old-slotd (find name old-slotds
                                :key #'slot-definition-name)))
          (when old-slotd
            (if (eq (slot-definition-allocation old-slotd) :instance)
                ;; We just copy over values,
                ;; whether they're unbound markers or not.
                (setf (si:rack-ref new-rack (slot-definition-location new-slotd))
                      (si:rack-ref old-rack (slot-definition-location old-slotd)))
                ;; Use a slow path for custom or class allocation.
                ;; We could consult the location directly for class allocation,
                ;; but speed shouldn't be super concerning.
                (when (slot-boundp-using-class old-class old-instance old-slotd)
                  (setf (si:rack-ref new-rack (slot-definition-location new-slotd))
                        (slot-value-using-class old-class old-instance old-slotd)))))))))
  (values))

(defmethod change-class ((instance standard-object) (new-class standard-class)
                         core:&va-rest initargs)
  (let* ((old-rack (core:instance-rack instance))
         (old-class (class-of instance))
         (copy (core:allocate-raw-instance old-class old-rack))
         (new-rack (make-rack-for-class new-class)))
    (change-class-aux old-rack new-rack old-class copy)
    (setf (core:instance-rack instance) new-rack
          (core:instance-class instance) new-class)
    ;; If U-I-F-D-C signals an error or otherwise nonlocally exits, roll back
    ;; the instance. This does not seem to be required by the standard, but
    ;; is a nice thing to do for the rare user that actually specializes
    ;; U-I-F-D-C.
    (let ((aborted t))
      (unwind-protect
           (progn (apply #'update-instance-for-different-class
                         copy instance initargs)
                  (setf aborted nil))
        (when aborted
          (setf (core:instance-rack instance) old-rack
                (core:instance-class instance) old-class))))
    instance))

(defmethod change-class ((instance funcallable-standard-object)
                         (new-class funcallable-standard-class)
                         core:&va-rest initargs)
  (let* ((old-rack (core:instance-rack instance))
         (old-class (class-of instance))
         (copy (core:allocate-raw-funcallable-instance old-class old-rack))
         (new-rack (make-rack-for-class new-class)))
    (change-class-aux old-rack new-rack old-class copy)
    (setf (core:instance-rack instance) new-rack
          (core:instance-class instance) new-class)
    (let ((aborted t))
      (unwind-protect
           (progn (apply #'update-instance-for-different-class
                         copy instance initargs)
                  (setf aborted nil))
        (when aborted
          (setf (core:instance-rack instance) old-rack
                (core:instance-class instance) old-class))))
    instance))

;;;
;;; PART 2: UPDATING AN INSTANCE THAT BECAME OBSOLETE
;;;
;;; Each instance's rack contains a "sig", which is a list of the slotds its
;;; class had when that rack was allocated. When an instance is updated, its
;;; rack is replaced with a new rack based on the new definition of the class
;;; and its new stamp and slotds.
;;; An instance is obsolete if it's rack's stamp is out of sync with its
;;; class's stamp.
;;;
;;; There are two circumstances under which a instance may become obsolete:
;;; either the class has been modified using REINITIALIZE-INSTANCE (and thus
;;; the list of slots changed), or MAKE-INSTANCES-OBSOLETE has been used.
;;;
;;; The function UPDATE-INSTANCE (hidden to the user) does the job of
;;; updating an instance that has become obsolete.
;;;
;;;     a) A new rack is allocated based on the new class definition.
;;;     b) The new rack's slots are filled in from the old rack's slots;
;;;        also, the lists of added slots, discarded slots, and the
;;;        property list of discarded values are computed based on the
;;;        two racks' sigs.
;;;     c) The instance's rack is atomically replaced with the new one.
;;;	d) Finally, UPDATE-INSTANCE-FOR-REDEFINED-CLASS is invoked
;;;	   with enough information to perform any extra initialization,
;;;	   for instance of new slots.
;;;
;;; UPDATE-INSTANCE is invoked whenever a generic function dispatch misses.
;;;

(defmethod update-instance-for-redefined-class
    ((instance standard-object) added-slots discarded-slots property-list
     &rest initargs)
  (declare (dynamic-extent initargs))
  ;; Since this function is "not intended to be called by programmers",
  ;; and the instance updater doesn't pass it any initargs (CLHS 4.3.6.2),
  ;; in practice the initargs will always be NIL.
  ;; I guess they're there in case a programmer calls this function
  ;; manually?
  (when initargs
    (check-initargs-uncached
     (class-of instance) initargs
     (list (list #'update-instance-for-redefined-class
                 (list instance added-slots discarded-slots property-list))
           (list #'shared-initialize (list instance added-slots)))))
  (apply #'shared-initialize instance added-slots initargs))

;;; This function works on racks directly rather than instances,
;;; and implements the behavior that's independent of the location of
;;; the rack in the instance. Which is almost all of it.
(defun update-instance-aux (old-rack new-rack)
  (let* ((old-slotds (si:rack-sig old-rack))
         (new-slotds (si:rack-sig new-rack))
         (old-local-slotds (remove :instance old-slotds :test-not #'eq
                                   :key #'slot-definition-allocation))
         (new-local-slotds (remove :instance new-slotds :test-not #'eq
                                   :key #'slot-definition-allocation))
         (discarded-slots '())
         (added-slots '())
         (property-list '()))
    (dolist (slotd old-local-slotds)
      (let* ((name (slot-definition-name slotd))
             (new (find name new-local-slotds :key #'slot-definition-name)))
        (let ((val (si:rack-ref old-rack
                                (slot-definition-location slotd))))
          (when (si:sl-boundp val)
            (cond (new
                   (setf (si:rack-ref new-rack (slot-definition-location new))
                         val))
                  (t
                   (push (cons name val) property-list)
                   (push name discarded-slots)))))))
    (dolist (new-slot new-local-slotds)
      (let* ((name (slot-definition-name new-slot))
             (old (find name old-local-slotds :key #'slot-definition-name)))
        (unless old
          (push name added-slots))))
    (values added-slots discarded-slots property-list)))

;;; Used for both instances and funcallable instances due to their
;;; identical rack locations.
(defun update-instance (instance)
  (let ((old-rack (si:instance-rack instance))
        (new-rack (make-rack-for-class (class-of instance))))
    (multiple-value-bind (added-slots discarded-slots property-list)
        (update-instance-aux (si:instance-rack instance) new-rack)
      (setf (si:instance-rack instance) new-rack)
      ;; If U-I-F-R-C signals an error or otherwise nonlocally exits, roll back
      ;; the instance. This does not seem to be required by the standard, but
      ;; is a nice thing to do for the rare user that actually specializes
      ;; U-I-F-R-C.
      (let ((aborted t))
        (unwind-protect
             (progn (update-instance-for-redefined-class
                     instance added-slots discarded-slots property-list)
                    (setf aborted nil))
          (when aborted
            (setf (si:instance-rack instance) old-rack)))))))

;;; ----------------------------------------------------------------------
;;; CLASS REDEFINITION PROTOCOL

(defmethod reinitialize-instance :before ((class class) &rest initargs &key)
  (declare (ignore initargs))
  (let ((name (class-name class)))
    (when (member name '(CLASS BUILT-IN-CLASS) :test #'eq)
      (error "The kernel CLOS class ~S cannot be changed." name)))

  ;; remove previous defined accessor methods
  (when (class-finalized-p class)
    (remove-optional-slot-accessors class)))

(defun slots-unchanged-p (old-slots new-slots)
  ;; M-I-O is called "[when] the set of local slots accessible in an instance has changed
  ;; or the order of slots in storage has changed", where "local" means accessible only
  ;; in the instance it's allocated in, which we take as meaning :allocation :instance.
  ;; However, because we're pretty aggressive with slot accessors, I think we need to be
  ;; conscientious of class slots as well. Here we just deny identity if there are any
  ;; class slots; better safe than sorry, but maybe could be improved. With custom
  ;; allocations, who knows. We could define a slot-equivalent-p generic-function.
  ;; NOTE/TODO?: We kind of don't actually care about the order of slots IN THE CLASS,
  ;; just in the instance. So we could go through and do a real set comparison. But this
  ;; is enough to cover an identical defclass being executed.
  (and (= (length old-slots) (length new-slots))
       (every (lambda (slot1 slot2)
                (and
                 (eq (slot-definition-name slot1) (slot-definition-name slot2))
                 (eq (slot-definition-allocation slot1) :instance)
                 (eq (slot-definition-allocation slot1) (slot-definition-allocation slot2))
                 (= (slot-definition-location slot1) (slot-definition-location slot2))))
              old-slots new-slots)))

(defmethod reinitialize-instance :after ((class class) &rest initargs)
  (declare (dynamic-extent initargs))
  ;; AMOP Ch. 5 "Reinitialization of class metaobjects" specifies that
  ;; the class is unconditionally re-finalized. This means that the
  ;; addition of forward referenced classes is an error.
  (setf (%class-finalized-p class) nil)
  (finalize-inheritance class)

  (update-dependents class initargs))

(defun remove-optional-slot-accessors (class)
  (declare (class class)
	   (optimize (safety 0)))
  (let ((class-name (class-name class)))
    (dolist (slotd (class-slots class))
      ;; remove previously defined reader methods
      (dolist (reader (slot-definition-readers slotd))
	(let* ((gf-object (fdefinition reader))
	       found)
          (when gf-object ; fmakunbound or otherwise could have removed
            ;; primary method
            (when (setq found (find-method gf-object nil (list class-name) nil))
              (remove-method gf-object found))
            ;; before method
            ;; not sure whether removing these is a good idea. Couldn't a user have defined them?
            ;; And e.g. have a subclass that retains the accessor.
            (when (setq found (find-method gf-object (list :before) (list class-name) nil))
              (remove-method gf-object found))
            ;; after method
            (when (setq found (find-method gf-object (list :after) (list class-name) nil))
              (remove-method gf-object found))
            ;; This is unnecessary but kind of nice?
            ;; Other implementations have different behavior.
            ;; The user could have defined the generic function specially, so whether this is
            ;; the right thing to do is ambiguous.
            (when (null (generic-function-methods gf-object))
              (fmakunbound reader)))))
      ;; remove previously defined writer methods
      (dolist (writer (slot-definition-writers slotd))
	(let* ((gf-object (fdefinition writer))
	       found)
          (when gf-object
            ;; primary method
            (when (setq found (find-method gf-object nil (list 'T class-name) nil))
              (remove-method gf-object found))
            ;; before method
            (when (setq found (find-method gf-object (list :before) (list 'T class-name) nil))
              (remove-method gf-object found))
            ;; after method
            (when (setq found (find-method gf-object (list :after) (list 'T class-name) nil))
              (remove-method gf-object found))
            (when (null (generic-function-methods gf-object))
              (fmakunbound writer))))))))

;;; ----------------------------------------------------------------------
;;; BANS
;;; Some change operations AMOP expressly prohibits.
;;; Specifically, reinitialize-instance HAS to signal an error, but for the
;;; others the behavior doesn't seem to be defined: it just says portable
;;; programs can't do this.
;;; Relatedly, we don't signal an error if a metaobject class is redefined,
;;; although AMOP says portable programs can't do so. We may be able to
;;; support that kind of redefinition.
;;; We also don't actually signal an error if an object is change-class'd
;;; into a metaobject, but that's more because we can't check with just
;;; discrimination as below, and we'd have to be more manual, and for
;;; almost every call to change-class.

(defmethod reinitialize-instance ((instance method) &rest initargs)
  (declare (ignore initargs))
  (error "Cannot reinitialize method metaobject ~a per AMOP Ch. 6"
         instance))

(defmethod reinitialize-instance ((instance slot-definition) &rest initargs)
  (declare (ignore initargs))
  (error "Cannot reinitialize slot definition metaobject ~a per AMOP Ch. 6"
         instance))

;;; Not mentioned by AMOP, but seems obvious
(defmethod reinitialize-instance ((instance eql-specializer) &rest initargs)
  (declare (ignore initargs))
  (error "Cannot reinitialize eql specializer metaobject ~a per AMOP Ch. 6"
         instance))

(defmethod change-class ((instance class) new-class &rest initargs)
  (declare (ignore new-class initargs))
  (if (forward-referenced-class-p instance)
      (call-next-method)
      (error "The metaclass of a class metaobject ~a cannot be changed per AMOP Ch. 6"
             instance)))

(defmethod change-class ((instance generic-function) new-class &rest initargs)
  (declare (ignore new-class initargs))
  (error "The metaclass of a generic function metaobject ~a cannot be changed per AMOP Ch. 6"
         instance))

(defmethod change-class ((instance method) new-class &rest initargs)
  (declare (ignore new-class initargs))
  (error "The metaclass of a method metaobject ~a cannot be changed per AMOP Ch. 6"
         instance))

(defmethod change-class ((instance slot-definition) new-class &rest initargs)
  (declare (ignore new-class initargs))
  (error "The metaclass of a slot definition metaobject ~a cannot be changed per AMOP Ch. 6"
         instance))

;;; Also not in AMOP
(defmethod change-class ((instance eql-specializer) new-class &rest initargs)
  (declare (ignore new-class initargs))
  (error "The metaclass of an eql specializer metaobject ~a cannot be changed per AMOP Ch. 6"
         instance))

;;; Not specified by AMOP, as far as I can tell, but required by the fact that
;;; they have different underlying layouts in Clasp.
(defmethod change-class ((instance standard-object)
                         (class funcallable-standard-class)
                         &rest initargs)
  (declare (ignore initargs))
  (error "The standard instance ~s cannot be transmuted into a funcallable standard instance of class ~s."
         instance class))

(defmethod change-class ((instance funcallable-standard-object)
                         (class standard-class)
                         &rest initargs)
  (declare (ignore initargs))
  (error "The funcallable standard instance ~s cannot be transmuted into a standard instance of class ~s."
         instance class))
