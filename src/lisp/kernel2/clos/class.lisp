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

(defgeneric direct-slot-definition-class (class &rest canonicalized-slot))
(defgeneric effective-slot-definition-class (class &rest canonicalized-slot))
(defgeneric make-instances-obsolete (class))
(defgeneric add-direct-subclass (parent child))
(defgeneric remove-direct-subclass (parent child))
(defgeneric validate-superclass (class superclass))
(defgeneric finalize-inheritance (class))
(defgeneric compute-slots (class))
(defgeneric compute-effective-slot-definition-initargs (class direct-slotds))
(defgeneric compute-effective-slot-definition (class name direct-slotds))
(defgeneric compute-default-initargs (class))
;; AMOP specifies some of the keys. FIXME? Dunno if we should bother
(defgeneric ensure-class-using-class (class name &key &allow-other-keys))
(defgeneric reader-method-class (class direct-slot &rest initargs))
(defgeneric writer-method-class (class direct-slot &rest initargs))
(defgeneric (setf class-name) (new-name class))

(defun compute-instance-size (slots)
  ;; could just use cl:count, but type inference is bad atm
  (loop for slotd in slots
        when (eq (slot-definition-allocation slotd) :instance)
          sum 1))

;;; ----------------------------------------------------------------------
;;; CLASSES INITIALIZATION AND REINITIALIZATION
;;;

(defmethod direct-slot-definition-class ((class T) &rest canonicalized-slot)
  (declare (ignore class canonicalized-slot))
  (find-class 'standard-direct-slot-definition nil))

(defmethod effective-slot-definition-class ((class T) &rest canonicalized-slot)
  (declare (ignore class canonicalized-slot))
  (find-class 'standard-effective-slot-definition nil))

(defun unfinalizablep (class)
  (or (typep class 'forward-referenced-class)
      (and (not (class-finalized-p class))
           (some #'unfinalizablep (class-direct-superclasses class)))))

(defun finalize-unless-forward (class)
  (unless (unfinalizablep class)
    (finalize-inheritance class)))

(defmethod make-instances-obsolete ((class class))
  ;; This changes what stamp new instances of the class get- which also means obsolete
  ;; instances will have a different stamp from their class, which is how the system
  ;; determines obsolesence (in MAYBE-UPDATE-INSTANCES).
  (core:class-new-stamp class)
  ;; Now this removes the class from all generic function fast paths. This ensures that
  ;; if a generic function is specialized where an obsolete instance is, it will go to
  ;; the slow path, which will call MAYBE-UPDATE-INSTANCES.
  (invalidate-generic-functions-with-class-selector class)
  #+(or); static-gfs
  (static-gfs:invalidate-class-reinitializers* class)
  #+(or) ;static-gfs
  (static-gfs:invalidate-changers* class)
  class)

(defmethod make-instances-obsolete ((class symbol))
  (make-instances-obsolete (find-class class)))

(defmethod initialize-instance :after
    ((class class) &rest initargs &key direct-slots)
  (declare (dynamic-extent initargs) ; see NOTE in reinitialize-instance/T
           (ignore initargs direct-slots))
  (finalize-unless-forward class)
  ;; In this case we are assigning the stamp for the first time.
  (core:class-new-stamp class))

(defmethod shared-initialize ((class class) slot-names
                              &rest initargs
                              &key (direct-superclasses () dscp)
                                (direct-slots nil direct-slots-p))
  (declare (dynamic-extent initargs)) ; see NOTE in reinitialize-instance/T
  ;;; Convert the list of direct slots into actual slot definitions.
  (when direct-slots-p
    (setf initargs
          (list* :direct-slots
                 (loop for s in direct-slots
                       collect (canonical-slot-to-direct-slot class s))
                 initargs)))
  (let* ((old-direct-superclasses
           (when dscp
             (if (slot-boundp class 'direct-superclasses)
                 (class-direct-superclasses class)
                 nil)))
         ;; validate superclasses or provide default
         (direct-superclasses
           (when dscp
             (check-direct-superclasses class direct-superclasses)))
         (initargs (if dscp
                       (list* :direct-superclasses direct-superclasses
                              initargs)
                       initargs))
         (class (apply #'call-next-method class slot-names initargs)))
    ;; Call add/remove-direct-subclass appropriately
    (when dscp
      (loop for c in old-direct-superclasses
            unless (member c direct-superclasses :test #'eq)
              do (remove-direct-subclass c class))
      (loop for c in direct-superclasses
            unless (member c old-direct-superclasses :test #'eq)
              do (add-direct-subclass c class)))

    ;; initialize the default allocator for the new class
    ;; It is inherited from the direct-superclasses - if they are all 
    ;; regular classes then it will get an Instance allocator
    ;; If one of them is a ClbindClass then this will inherit a
    ;; duplicate of its allocator
    (setf (creator class) (sys:compute-instance-creator class (class-of class) direct-superclasses))
    class))

(defun freeze-class-slot-initfunction (slotd)
  (when (eq (getf slotd :allocation) :class)
    (let ((initfunc (getf slotd :initfunction)))
      (when initfunc
        (setf (getf slotd :initfunction)
              (constantly (funcall initfunc))))))
  slotd)

(defun canonical-slot-to-direct-slot (class slotd)
  ;; Class slot init functions must be called right away
  (let ((slotd (freeze-class-slot-initfunction slotd)))
    (apply #'make-instance
	   (apply #'direct-slot-definition-class class slotd)
	   slotd)))

(defmethod add-direct-subclass ((parent class) child)
  (pushnew child (%class-direct-subclasses parent)))

(defmethod remove-direct-subclass ((parent class) child)
  (setf (%class-direct-subclasses parent)
	(remove child (%class-direct-subclasses parent))))

(defun check-direct-superclasses (class supplied-superclasses)
  (if supplied-superclasses
      (loop for superclass in supplied-superclasses
	 ;; Until we process streams.lisp there are some invalid combinations
	 ;; using built-in-class, which here we simply ignore.
	 unless (validate-superclass class superclass)
	 do (error "Class ~A is not a valid superclass for ~A" superclass class))
      (setf supplied-superclasses
	    (list (find-class (typecase class
				(STANDARD-CLASS 'STANDARD-OBJECT)
				(STRUCTURE-CLASS 'STRUCTURE-OBJECT)
				(FUNCALLABLE-STANDARD-CLASS 'FUNCALLABLE-STANDARD-OBJECT)
				(otherwise (error "No :DIRECT-SUPERCLASS ~
argument was supplied for metaclass ~S." (class-of class))))))))
  ;; FIXME!!! Here should come the invocation of VALIDATE-SUPERCLASS!
  ;; FIXME!!! We should check that structures and standard objects are
  ;; not mixed, and that STANDARD-CLASS, or STANDARD-GENERIC-FUNCTION,
  ;; etc, are the first classes.
  supplied-superclasses)

(defmethod validate-superclass ((class class) (superclass class))
  (or (let ((c1 (class-of class))
	    (c2 (class-of superclass)))
	(or (eq c1 c2)
	  (and (eq c1 #.(find-class 'standard-class))
            (eq c2 #.(find-class 'funcallable-standard-class)))
	  (and (eq c2 #.(find-class 'standard-class))
            (eq c1 #.(find-class 'funcallable-standard-class)))))
      (or (typep class 'forward-referenced-class)
          (typep superclass 'forward-referenced-class))))

;;; NOTE: SBCL defines its own "SYSTEM-CLASS" to mean classes that are like
;;; built-in-classes but also subclassable. This may be worth consideration.
(defmethod validate-superclass ((class class) (superclass built-in-class))
  (or (eq superclass #.(find-class 't)) ; required by AMOP
      ;; FIXME: Should gray streams go here?
      ;; Extensible sequences
    (eq superclass #.(find-class 'sequence))))

(defmethod add-dependent ((c class) dep)
  (pushnew dep (class-dependents c)))

(defmethod remove-dependent ((c class) dep)
  (setf (class-dependents c)
        (remove dep (class-dependents c))))

(defmethod map-dependents ((c class) function)
  (dolist (d (class-dependents c))
    (funcall function d)))

(defmethod (setf class-name) (new-name (class class))
  (reinitialize-instance class :name new-name)
  new-name)

;;; ----------------------------------------------------------------------
;;; GENERIC FUNCTION INVALIDATION

(defun call-history-entry-key-contains-specializers-p (key specializer)
  (find specializer key :test #'eq))

(defun generic-function-call-history-separate-entries-with-specializer
    (call-history specializer)
  (loop for entry in call-history
        for key = (car entry)
        if (call-history-entry-key-contains-specializers-p key specializer)
          collect entry into removed
        else
          collect entry into keep
        finally (return (values keep removed))))

;; Remove all call entries referring directly to a class, and invalidate or
;; force their discriminating functions.
(defun invalidate-generic-functions-with-class-selector (class)
  (loop for gf in (specializer-call-history-generic-functions class)
        do (mp:atomic-update (generic-function-call-history gf)
            #'generic-function-call-history-separate-entries-with-specializer
            class)
           ;; We don't force the dispatcher, because when a class with
           ;; subclasses is redefined, we may end up here repeatedly.
           ;; Eagerness would result in pointless compilation.
           (invalidate-discriminating-function gf)))

;;; ----------------------------------------------------------------------
;;; FINALIZATION OF CLASS INHERITANCE
;;;

(defmethod finalize-inheritance ((class class))
  ;; FINALIZE-INHERITANCE computes the guts of what defines a class: the
  ;; slots, the list of parent class, etc. It is called when either the
  ;; class was not finalized before, or when one of the parents has been
  ;; modified.
  ;;
  (let ((cpl (compute-class-precedence-list class)))
    ;; A class cannot be finalized if any of its parents is either
    ;; a not yet defined class or it has not yet been finalized.
    ;; In the first case we can just signal an error...
    ;;
    (let ((x (find-if (lambda (c) (typep c 'forward-referenced-class)) (rest cpl))))
      (when x
	(error "Cannot finish building the class~%  ~A~%~
because it contains a reference to the undefined class~%  ~A"
	       (class-name class) (class-name x))))
    ;;
    ;; ... and in the second case we just finalize the top-most class
    ;; which is not yet finalized and rely on the fact that this
    ;; class will also try to finalize all of its children.
    ;;
    (let ((x (find-if-not #'class-finalized-p cpl :from-end t)))
      (unless (or (null x) (eq x class))
	(return-from finalize-inheritance
	  (finalize-inheritance x))))
    (setf (%class-precedence-list class) cpl)
    (let* ((old-slots-p (slot-boundp class 'slots))
           (old-slots (when old-slots-p (class-slots class)))
           (new-slots (compute-slots class)))
      (setf (%class-slots class) new-slots
	    (class-size class) (compute-instance-size new-slots)
	    (%class-default-initargs class) (compute-default-initargs class)
	    (%class-finalized-p class) t)
      ;; If there's been no substantial change to the slots, i.e. we have
      ;; slots with the same names and allocations in the same order, we
      ;; skip MAKE-INSTANCES-OBSOLETE. This is explicitly allowed
      ;; (see CLHS M-I-O). It reduces needless compilation in fastgf
      ;; dispatch, and is actually required to support evaluating defstruct
      ;; forms with no :type multiple times.
      ;; We also don't need to M-I-O if there were no slots previously,
      ;; since that implies there are no instances to make obsolete.
      (when (and old-slots-p
                 (not (slots-unchanged-p old-slots new-slots)))
        (make-instances-obsolete class))))
  ;;
  ;; We have to clear the different type caches
  ;; for type comparisons and so on.
  ;;
  (si::subtypep-clear-cache)
  ;; As mentioned above, when a parent is finalized, it is responsible for
  ;; invoking FINALIZE-INHERITANCE on all of its children. Obviously,
  ;; this only makes sense when the class has been defined.
  (dolist (subclass (reverse (class-direct-subclasses class)))
    (finalize-unless-forward subclass))
  ;;
  ;; We create various caches to more rapidly find the slot locations and
  ;; slot definitions from slot-value.
  (std-create-slots-table class))

;;;
;;; Clasp classes store slots in a hash table for faster access. The
;;; following functions create the cache and allow us to locate the
;;; slots rapidly.
;;;
(defun std-create-slots-table (class)
  (with-slots ((all-slots slots)
	       (location-table location-table))
      class
    (let ((size (max 32 (* 2 (length all-slots))))
          (metaclass (si::instance-class class))
          (locations nil))
	(when (or (eq metaclass #.(find-class 'standard-class))
		  (eq metaclass #.(find-class 'funcallable-standard-class))
		  (eq metaclass #.(find-class 'structure-class)))
	  (setf locations (make-hash-table :size size))
	  (dolist (slotd all-slots)
	    (setf (gethash (slot-definition-name slotd) locations)
		  (slot-definition-location slotd))))
      (setf location-table locations))))

;;; KLUDGE: Dummy definition, redefined in static-gfs
#+static-gfs
(defun static-gfs:invalidate-class-constructors (class)
  (declare (ignore class)))

(defmethod finalize-inheritance :after ((class std-class))
  #+static-gfs
  (static-gfs:invalidate-class-constructors class)
  #+(or) ;static-gfs
  (static-gfs:invalidate-changers* class)
  #+(or) ;static-gfs
  (static-gfs:invalidate-class-reinitializers* class)
  (std-class-generate-accessors class))

(defun std-class-generate-accessors (standard-class)
  ;;
  ;; The accessors are closures, which are generated every time the
  ;; slots of the class change. The accessors are safe: they check that
  ;; the slot is bound after retreiving the value, and they may take
  ;; the liberty of using SI:INSTANCE-REF because they know the class of
  ;; the instance.
  ;;
  (dolist (slotd (slot-value standard-class 'direct-slots))
    (with-slots ((name name) (allocation allocation) (location location)
		 (readers readers) (writers writers))
	slotd
      (multiple-value-bind (reader writer) (std-class-accessors name)
	(let* ((options (list :slot-definition slotd
                              :source-position (class-source-position
                                                standard-class)))
	       (reader-args (list* :function reader
				   :generic-function nil
				   :qualifiers nil
				   :lambda-list '(object)
				   :specializers `(,standard-class)
				   options))
	       (reader-class (apply #'reader-method-class standard-class slotd
				    reader-args))
	       (writer-args (list* :function writer
				   :generic-function nil
				   :qualifiers nil
				   :lambda-list '(value object)
				   :specializers `(,(find-class t) ,standard-class)
				   options))
	       (writer-class (apply #'writer-method-class standard-class slotd
				    writer-args)))
	  (dolist (fname readers)
	    (let ((method (make-method reader-class nil `(,standard-class) '(object)
				       reader
				       options)))
	      (add-method (ensure-generic-function fname) method)))
	  (dolist (fname writers)
	    (let ((method (make-method writer-class nil
				       `(,(find-class t) ,standard-class) '(value object)
				       writer
				       options)))
	      (add-method (ensure-generic-function fname) method))))))))

(defun std-class-accessors (slot-name)
  (values (make-%leaf-method-function
           #'(lambda (self)
               (declare (core:lambda-name std-class-accessors.reader.lambda))
               (slot-value self slot-name)))
          (make-%leaf-method-function
           #'(lambda (new-value self)
               (declare (core:lambda-name std-class-accessors.writer.lambda))
               (setf (slot-value self slot-name) new-value)))))

(defmacro mapappend (fun &rest args)
  `(reduce #'append (mapcar ,fun ,@args)))

(defmethod compute-slots ((class class))
  ;; INV: for some classes ECL expects that the order of the inherited slots is
  ;; preserved. The following code ensures that, if C1 is after C2 in the
  ;; class precedence list, and the slot S1 appears both in C1 and C2,
  ;; the slot S1 will appear the new class before the slots of C2; and
  ;; whenever possible, in the same position as in C1.
  ;;
  (do* ((all-slots (mapappend #'class-direct-slots (reverse (class-precedence-list class))))
	(all-names (nreverse (mapcar #'slot-definition-name all-slots)))
	(output '())
	(scan all-names (cdr scan)))
       ((endp scan) output)
    (let ((name (first scan)))
      (unless (find name (rest scan))
	(push (compute-effective-slot-definition
	       class name (delete name (reverse all-slots) :key #'slot-definition-name
				  :test-not #'eq))
	      output)))))

(defun slot-definition-to-plist (slotd)
  (list :name (slot-definition-name slotd)
	:initform (slot-definition-initform slotd)
	:initfunction (slot-definition-initfunction slotd)
	:type (slot-definition-type slotd)
	:allocation (slot-definition-allocation slotd)
	:initargs (slot-definition-initargs slotd)
	:readers (slot-definition-readers slotd)
	:writers (slot-definition-writers slotd)
	:documentation (slot-definition-documentation slotd)
	:location (slot-definition-location slotd)))

(defmethod compute-effective-slot-definition-initargs ((class class) direct-slotds)
  ;;; See CLHS 7.5.3 for the explanation of how slot options are inherited.
  (let (name initform initfunction allocation documentation
        (readers nil) (writers nil) (initargs nil) location
        (type t)
        (namep nil) (initp nil) (documentationp nil) (allocp nil))
    (dolist (slotd direct-slotds)
      (unless namep (setf name (slot-definition-name slotd)))
      (unless initp
        (let ((f (slot-definition-initfunction slotd)))
          (when f
            (setf initp t initfunction f
                  initform (slot-definition-initform slotd)))))
      (unless documentationp
        (let ((doc (slot-definition-documentation slotd)))
          (when doc
            (setf documentationp t documentation doc))))
      (unless allocp
        (setf allocp t allocation (slot-definition-allocation slotd)))
      (setf initargs (union (slot-definition-initargs slotd) initargs)
            readers (union (slot-definition-readers slotd) readers)
            writers (union (slot-definition-writers slotd) writers)
            ;; FIXME! we should be more smart then this:
            type (let ((new-type (slot-definition-type slotd)))
                   (cond ((subtypep new-type type) new-type)
                         ((subtypep type new-type) type)
                         (T `(and ,new-type ,type)))))
      ;;; Clasp extension: :location can be specified.
      (let ((new-loc (slot-definition-location slotd)))
        (if location
            (when new-loc
              (unless (eql location new-loc)
                (error 'simple-error
                       :format-control "You have specified two conflicting slot locations:~%~D and ~F~%for slot ~A"
                       :format-arguments (list location new-loc name)))))
        (setf location new-loc)))
    (list :name name
          :initform initform
          :initfunction initfunction
          :type type
          :allocation allocation
          :initargs initargs
          :readers readers :writers writers
          :documentation documentation
          :location location)))

(defmethod compute-effective-slot-definition ((class class) name direct-slots)
  (declare (ignore name))
  (let* ((initargs (compute-effective-slot-definition-initargs class direct-slots))
         (slotd-class (apply #'effective-slot-definition-class class initargs)))
    (apply #'make-instance slotd-class initargs)))

(defmethod compute-default-initargs ((class class))
  (let ((all-initargs (mapappend #'class-direct-default-initargs
				 (class-precedence-list class))))
    ;; We have to use this trick because REMOVE-DUPLICATES on
    ;; ((:foo x) (:faa y) (:foo z)) would produce ((:faa y) (:foo z))
    ;; and we want ((:foo x) (:faa y))
    (nreverse (remove-duplicates (reverse all-initargs) :key #'first))))

;;; ======================================================================
;;; STANDARD-CLASS specializations
;;;
;;; IMPORTANT: The following implementation of ENSURE-CLASS-USING-CLASS is
;;; shared by the metaclasses STANDARD-CLASS and STRUCTURE-CLASS.
;;;
(defmethod ensure-class-using-class ((class class) name &rest rest
				     &key direct-slots direct-default-initargs
                                       &allow-other-keys)
  (declare (ignore direct-default-initargs direct-slots))
  (multiple-value-bind (metaclass direct-superclasses options)
      (apply #'help-ensure-class rest)
    (declare (ignore direct-superclasses))
    (cond ((typep class 'forward-referenced-class)
	   (change-class class metaclass))
	  ((not (eq (class-of class) metaclass))
	   (error "When redefining a class, the metaclass can not change.")))
    (setf class (apply #'reinitialize-instance class :name name options))
    (when name
      (si:create-type-name name)
      (setf (find-class name) class))
    class))

(defmethod ensure-class-using-class ((class null) name &rest rest)
  (multiple-value-bind (metaclass direct-superclasses options)
      (apply #'help-ensure-class rest)
    (declare (ignore direct-superclasses))
    (when name
      (si:create-type-name name)
      (setf (find-class name)
            (apply #'make-instance metaclass :name name options)))))

(defun coerce-to-class (class-or-symbol &optional (fail nil))
  (cond ((classp class-or-symbol) class-or-symbol)
	((not (symbolp class-or-symbol))
	 (error "~a is not a valid class specifier." class-or-symbol))
	((find-class class-or-symbol fail))
	(t
	 (warn 'si::simple-style-warning
	       :format-control "Class ~A has been forward referenced."
	       :format-arguments (list class-or-symbol))
	 (ensure-class class-or-symbol
		       :metaclass 'forward-referenced-class
		       :direct-superclasses (list (find-class 'standard-object))
		       :direct-slots '()))))

(defun help-ensure-class (&rest options
			  &key (metaclass 'standard-class) direct-superclasses
			  &allow-other-keys)
  (remf options :metaclass)
  (remf options :direct-superclasses)
  (setf metaclass (coerce-to-class metaclass t)
	direct-superclasses (mapcar #'coerce-to-class direct-superclasses))
  (values metaclass direct-superclasses
	  (list* :direct-superclasses direct-superclasses options)))

(defmethod reader-method-class ((class std-class)
				(direct-slot direct-slot-definition)
				&rest initargs)
  (declare (ignore class direct-slot initargs))
  (find-class 'standard-reader-method))

(defmethod writer-method-class ((class std-class)
				(direct-slot direct-slot-definition)
				&rest initargs)
  (declare (ignore class direct-slot initargs))
  (find-class 'standard-writer-method))

;;; ----------------------------------------------------------------------
;;; Around methods for COMPUTE-SLOTS which assign locations to each slot.
;;;

(defun class-compute-slots (class slots)
  ;; This an Clasp extension. We are allowed to specify the location of
  ;; a direct slot. Consequently we have to first sort the ones which
  ;; have been predefined and then assign locations _after_ the last
  ;; assigned slot. Note the generalized comparison, which pushes all
  ;; slots without a defined location to the end of the list.
  ;; We also use this internally for some system slots accessed from C++.
  ;; See :location use in hierarchy.lisp.
  ;; Originally from ECL.
  (let* ((size (compute-instance-size slots))
	 (instance-slots (remove :instance slots :key #'slot-definition-allocation
						 :test-not #'eq))
	 (numbered-slots (remove-if-not #'slot-definition-location instance-slots))
	 (other-slots (remove-if #'slot-definition-location instance-slots))
	 (aux (make-array size :initial-element nil)))
    (loop for i in numbered-slots
          do (let ((loc (slot-definition-location i)))
               (when (aref aux loc)
                 (error "Slots ~A and ~A are said to have the same location in class ~A."
                        (aref aux loc) i class))
               (setf (aref aux loc) i)))
    (loop for i in other-slots
          with index = 0
          do (loop while (aref aux index)
                   do (incf index)
                   finally (setf (aref aux index) i
                                 (%slot-definition-location i) index)))
    slots))

(defmethod compute-slots :around ((class class))
  (class-compute-slots class (call-next-method)))

(defun std-class-compute-slots (class slots)
  (let* ((direct-slots (class-direct-slots class)))
    (dolist (slotd slots)
      (let* ((name (slot-definition-name slotd))
	     (allocation (slot-definition-allocation slotd)))
	(cond ((not (eq (slot-definition-allocation slotd) :class)))
	      ((find name direct-slots :key #'slot-definition-name) ; new shared slot
	       (let* ((initfunc (slot-definition-initfunction slotd))
	              (value (if initfunc (funcall initfunc) (si:unbound))))
	         (setf (%slot-definition-location slotd) (list value))))
	      (t			; inherited shared slot
	       (dolist (c (rest (class-precedence-list class)))
		 (let ((other (find (slot-definition-name slotd)
				    (class-slots c)
				    :key #'slot-definition-name)))
		   (when (and other
			      (eq (slot-definition-allocation other) allocation)
                              ;; Only use this location if it's directly
                              ;; specified. See #1392.
                              ;; We don't just search through the direct
                              ;; slots because they don't have locations set.
                              (find (slot-definition-name slotd)
                                    (class-direct-slots c)
                                    :key #'slot-definition-name)
			      (setf (%slot-definition-location slotd)
				    (slot-definition-location other)))
		     (return))))))))
    slots))

(defmethod compute-slots :around ((class std-class))
  (std-class-compute-slots class (call-next-method)))

;;; ----------------------------------------------------------------------
;;;
;;; (PARSE-SLOTS slot-definition-form) => slot-definition-object
;;;
;;; This routine is the one responsible for parsing the definition of
;;; a slot in DEFCLASS.
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun parse-slot (slot)
  (if (symbolp slot)
      `(list :name ',slot)
      (do* (output
	    (options (rest slot))
	    (extra nil)
            (initfunction))
	   ((null options)
            (let ((result (nconc output extra)))
              (if initfunction
                  `(list* :name ',(first slot) :initfunction ,initfunction ',result)
                  `(list* :name ',(first slot) ',result))))
	(let ((option (pop options)))
	  (when (endp options)
	    (si::simple-program-error
	     "In the slot description ~S,~%the option ~S is missing an argument"
	     slot option))
	  (let ((value (pop options)))
	    (when (and (member option '(:allocation :initform :type :documentation))
		       (getf options option))
	      (si::simple-program-error
	       "In the slot description ~S,~%the option ~S is duplicated"
	       slot option))
	    (case option
	      (:initarg    (push value (getf output :initargs)))
	      (:initform   (setf (getf output :initform) value
                                 initfunction
				 `(lambda () ,value)))
	      (:accessor   (push value (getf output :readers))
                           (push `(setf ,value) (getf output :writers)))
	      (:reader     (push value (getf output :readers)))
	      (:writer     (push value (getf output :writers)))
	      (:allocation (setf (getf output :allocation) value))
	      (:type       (setf (getf output :type) value))
	      (:documentation  (push value (getf output :documentation)))
	      (otherwise   (if (or (getf extra option)
				   (getf options option))
			       (push value (getf extra option))
			       (setf (getf extra option) value)))))))))

(defun parse-slots (slots)
  (do ((scan slots (cdr scan))
       (collect))
      ((null scan)
       `(list ,@(nreverse collect)))
    (let* ((slotd (parse-slot (first scan)))
	   (name (getf (cdr slotd) :name)))
      (dolist (other-slotd collect)
        ;;; name might be (quote <name>) so test with eq or eql does not work 
	(when (equal name (getf (cdr other-slotd) :name))
	  (si::simple-program-error
	   "A definition for the slot ~S appeared twice in a DEFCLASS form"
	   name)))
      (push slotd collect))))

;;; ----------------------------------------------------------------------
;;; DEFCLASS

(defun parse-default-initargs (default-initargs)
  (do* ((output-list nil)
	(scan default-initargs (cddr scan))
	(already-supplied '()))
       ((endp scan) `(list ,@(nreverse output-list)))
    (when (endp (rest scan))
      (si::simple-program-error "Wrong number of elements in :DEFAULT-INITARGS option."))
    (let ((slot-name (first scan))
	  (initform (second scan)))
      (if (member slot-name already-supplied)
	  (si::simple-program-error "~S is duplicated in :DEFAULT-INITARGS form ~S"
				    slot-name default-initargs)
	  (push slot-name already-supplied))
      (push `(list ',slot-name ',initform (lambda ()
                                            (declare (core:lambda-name parse-default-initargs.lambda))
                                            ,initform))
	    output-list))))

(defun gen-note-accessors (slots)
  (flet ((gen-note (name)
           `(cmp::register-global-function-def 'defmethod ',name)))
    (loop with result = nil
          for slot in slots
          when (consp slot)
            do (loop for (key value) on (rest slot) by #'cddr
                     do (case key
                          ((:reader :writer)
                           (push (gen-note value) result))
                          ((:accessor)
                           (push (gen-note value) result)
                           (push (gen-note `(setf ,value)) result))))
          finally (return result))))

(defun process-class-options (class-args)
  (let ((options '())
	(processed-options '()))
    (dolist (option class-args options)
      (unless (consp option)
        (si:simple-program-error
         "Option ~s for DEFCLASS has invalid syntax: not a cons" option))
      (let ((option-name (first option))
	    option-value)
        (unless (symbolp option-name)
          (si:simple-program-error
           "~s is not a valid DEFCLASS option: not a symbol" option-name))
	(if (member option-name processed-options)
	    (si:simple-program-error
	     "Option ~s for DEFCLASS specified more than once"
	     option-name)
	    (push option-name processed-options))
	(setq option-value
	      (case option-name
		((:metaclass :documentation)
		 `',(second option))
;;                ((:source-position) (second option)) ; see FIXME above
		(:default-initargs
		 (setf option-name :direct-default-initargs)
		 (parse-default-initargs (rest option)))
		(otherwise
		 `',(rest option)))
	      options (list* `',option-name option-value options))))))
) ; eval-when

(defmacro defclass (name superclasses slots &rest options)
  (let (;; Throw in source info if there is any.
        (options (if (ext:current-source-location)
                     (list* (cons :source-position (ext:current-source-location)) options)
                     options)))
    (unless (and (listp superclasses) (listp slots))
      (si::simple-program-error "Illegal defclass form: superclasses and slots should be lists"))
    (unless (and (symbolp name) (every #'symbolp superclasses))
      (si::simple-program-error "Illegal defclass form: superclasses and class name are not valid"))
    (let ((parsed-slots (parse-slots slots))
          (processed-class-options (process-class-options options)))
      `(progn
         (eval-when (:compile-toplevel)
           ,@(gen-note-accessors slots)
           (setf (core::class-info ',name) t))
         (eval-when (:load-toplevel :execute)
           (ensure-class ',name :direct-superclasses ',superclasses
                         :direct-slots ,parsed-slots
                         ,@processed-class-options))))))

;;; ----------------------------------------------------------------------
;;; ENSURE-CLASS
;;;
(defun ensure-class (name &rest initargs)
  (apply #'ensure-class-using-class
         (let ((class (and name
                           (find-class name nil))))
           ;; Only classes which have a PROPER name are redefined. If a class
           ;; with the same name is registered, but the name of the class does not
           ;; correspond to the registered name, a new class is returned.
           ;; [Hyperspec 7.7 for DEFCLASS]
           (when (and class (eq name (class-name class)))
             class))
         name initargs))

;;; ----------------------------------------------------------------------
;;; (SETF FIND-CLASS)
;;;

;;; KLUDGE: Dummy definition, redefined in static-gfs
#+static-gfs
(defun static-gfs:invalidate-designated-constructors (name)
  (declare (ignore name)))

(defun (setf find-class) (new-value name &optional errorp env)
  (declare (ignore errorp env))
  (let ((old-class (find-class name nil)))
    (cond
      ((and old-class
	    (or (typep old-class 'built-in-class)
		(member name '(class built-in-class) :test #'eq)))
       (unless (eq new-value old-class)
	 (error "The class associated to the CL specifier ~S cannot be changed."
		name)))
      ((or (classp new-value) (null new-value))
       (core:setf-find-class new-value name)
       #+static-gfs
       (static-gfs:invalidate-designated-constructors name)
       #+(or) ;static-gfs
       (static-gfs:invalidate-designated-changers name))
      (t (error 'simple-type-error :datum new-value :expected-type '(or class null)
                                   :format-control "~A is not a valid class for (setf find-class)"
                                   :format-arguments (list new-value)))))
  new-value)

;;; ----------------------------------------------------------------------
;;; class info

#|
CLHS specifies that

"If a defclass form appears as a top level form, the compiler must make the class name be recognized as a valid type name in subsequent declarations (as for deftype) and be recognized as a valid class name for defmethod parameter specializers and for use as the :metaclass option of a subsequent defclass. The compiler must make the class definition available to be returned by find-class when its environment argument is a value received as the environment parameter of a macro."

Our DEFMETHOD and :metaclass do not need any compile time info. We do want to know what classes are classes for the TYPEP compiler macro.

The CLHS's last requirement about find-class is a problem. We can't fully make classes at compile time. There might be methods on validate-superclass, ensure-class-using-class, *-slot-definition-class, etc., without which a class definition will be invalid, and which won't necessarily be defined at compile time. I am writing this comment because of such a problem with validate-superclass in a real library (bug #736).

Partway making a class probably isn't valid either. We definitely can't make an actual instance of any specified metaclass, or actual slot definitions, for the above reasons, etc, etc.

So we just ignore the CLHS requirement here and use a CLASS-INFO mechanism. This is a function that returns compile-time information about a class. A toplevel DEFCLASS form will, at compile time, register the class in the class-info table.

Right now the only such information is that it exists. In the future I'd like to include real information (e.g. unparsed class options or slot definitions) for use in optimization or to the user.
|#

(defvar *class-infos* (make-hash-table :test #'eq :thread-safe t))

(defun core::class-info (name &optional env)
  (or (find-class name nil env)
      (values (gethash name *class-infos*))))

(defun (setf core::class-info) (value name &optional env)
  (declare (ignore env))
  (if (null value)
      (progn (remhash name *class-infos*) value)
      (setf (gethash name *class-infos*) value)))
