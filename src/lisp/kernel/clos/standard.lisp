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
;;; INSTANCES INITIALIZATION AND REINITIALIZATION
;;;

(defmethod initialize-instance ((instance T) core:&va-rest initargs)
  (apply #'shared-initialize instance 'T initargs))

(defmethod reinitialize-instance ((instance T ) &rest initargs)
  (declare (dynamic-extent initargs))
  ;; NOTE: This dynamic extent declaration relies on the fact clasp's APPLY
  ;; does not reuse rest lists. If it did, a method on #'shared-initialize,
  ;; or whatever, could potentially let the rest list escape.
  (when initargs
    (check-initargs-uncached
     (class-of instance) initargs
     (list (list #'reinitialize-instance (list instance))
           (list #'shared-initialize (list instance t)))))
  (apply #'shared-initialize instance '() initargs))

(defmethod shared-initialize ((instance T) slot-names core:&va-rest initargs)
  ;;
  ;; initialize the instance's slots is a two step process
  ;;   1 A slot for which one of the initargs in initargs can set
  ;;      the slot, should be set by that initarg.  If more than
  ;;      one initarg in initargs can set the slot, the leftmost
  ;;      one should set it.
  ;;
  ;;   2 Any slot not set by step 1, may be set from its initform
  ;;      by step 2.  Only those slots specified by the slot-names
  ;;      argument are set.  If slot-names is:
  ;;       T
  ;;            any slot not set in step 1 is set from its
  ;;            initform
  ;;       <list of slot names>
  ;;            any slot in the list, and not set in step 1
  ;;            is set from its initform
  ;;
  ;;       ()
  ;;            no slots are set from initforms
  ;;
  (let* ((class (class-of instance)))
    ;; initialize-instance slots
    (dolist (slotd (class-slots class))
      (core:vaslist-rewind (core:validate-vaslist initargs))
      (core:validate-vaslist initargs)
      (let* ((slot-initargs (slot-definition-initargs slotd))
             (slot-name (slot-definition-name slotd)))
        (or
         ;; Try to initialize the slot from one of the initargs.
         (do ((largs (core:validate-vaslist initargs))
              initarg
              val)
             ((progn
                (= (core:vaslist-length (core:validate-vaslist largs)) 0))
              (progn nil))
           (setf initarg (core:vaslist-pop (core:validate-vaslist largs)))
           (core:validate-vaslist largs)
           #+(or)(when (endp largs) (core:simple-program-error "Wrong number of keyword arguments for SHARED-INITIALIZE, ~A" initargs))
           (when (= (core:vaslist-length (core:validate-vaslist largs)) 0)
             (core:simple-program-error "Wrong number of keyword arguments for SHARED-INITIALIZE, ~A"
                                        (progn
                                          (core:vaslist-rewind initargs)
                                          (core:list-from-vaslist initargs))))
           (unless (symbolp initarg)
             (core:simple-program-error "Not a valid initarg: ~A" initarg))
           (setf val #+(or)(pop l) (core:vaslist-pop (core:validate-vaslist largs)))
           (when (member initarg slot-initargs :test #'eq)
             (setf (slot-value instance slot-name) val)
             (return t)))
         (when (and slot-names
                    (or (eq slot-names 'T)
                        (member slot-name slot-names))
                    (not (slot-boundp instance slot-name)))
           (let ((initfun (slot-definition-initfunction slotd)))
             (when initfun
               (setf (slot-value instance slot-name) (funcall initfun)))))))))
  instance)

(defun compute-instance-size (slots)
  ;; could just use cl:count, but type inference is bad atm
  (loop for slotd in slots
        when (eq (slot-definition-allocation slotd) :instance)
          sum 1))

(defun make-rack-for-class (class)
  (let (;; FIXME: Read this information from the class in one go, atomically.
        (slotds (class-slots class))
        (size (class-size class))
        (stamp (core:class-stamp-for-instances class)))
    (core:make-rack size slotds stamp (core:unbound))))

(defmethod allocate-instance ((class standard-class) &rest initargs)
  (declare (ignore initargs))
  ;; CLHS says allocate-instance finalizes the class first.
  ;; Dr. Strandh argues that this is impossible since the initargs should be the
  ;; defaulted initargs, which cannot be computed without the class being finalized.
  ;; More fundamentally but less legalistically, allocate-instance is not usually
  ;; called except from make-instance, which checks finalization itself.
  ;; If allocate-instance is nonetheless somehow called on an unfinalized class,
  ;; class-size (also computed during finalization) will be unbound and error
  ;; before anything terrible can happen.
  ;; So we don't finalize here.
  (core:allocate-raw-instance class (make-rack-for-class class)))

(defmethod allocate-instance ((class core:derivable-cxx-class) &rest initargs)
  (declare (ignore initargs))
  (core:allocate-raw-general-instance class (make-rack-for-class class)))

(defun uninitialized-funcallable-instance-closure (funcallable-instance)
  (lambda (core:&va-rest args)
    (declare (core:lambda-name uninitialized-funcallable-instance))
    (declare (ignore args))
    (error "The funcallable instance ~a has not been initialized with a function"
           funcallable-instance)))

(defmethod allocate-instance ((class funcallable-standard-class) &rest initargs)
  (declare (ignore initargs))
  (let ((instance (core:allocate-raw-funcallable-instance
                   class (make-rack-for-class class))))
    ;; MOP says if you call a funcallable instance before setting its function,
    ;; the effects are undefined. (In the entry for set-funcallable-instance-function.)
    ;; But we can be nice.
    (set-funcallable-instance-function
     instance (uninitialized-funcallable-instance-closure instance))
    instance))

(defmethod make-instance ((class class) &rest initargs)
  (declare (dynamic-extent initargs)) ; see NOTE in reinitialize-instance/T
  ;; Without finalization we can not find initargs.
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  ;; We add the default-initargs first, because one of these initargs might
  ;; be (:allow-other-keys t), which disables the checking of the arguments.
  ;; (Paul Dietz's ANSI test suite, test CLASS-24.4)
  (setf initargs (add-default-initargs class initargs))
  (let ((keywords (if (slot-boundp class 'valid-initargs)
		      (progn
			(class-valid-initargs class))
		      (progn
			(precompute-valid-initarg-keywords class)))))
    (check-initargs class initargs keywords))
  (let ((instance (apply #'allocate-instance class initargs)))
    (apply #'initialize-instance instance initargs)
    instance))

(defun delete-keyword (keyword list)
  (loop until (eq (getf list keyword list) list)
     do (remf list keyword))
  list)

(defun add-default-initargs (class initargs)
  ;; Here, for each slot which is not mentioned in the initialization
  ;; arguments, but which has a value associated with :DEFAULT-INITARGS,
  ;; we compute the value and add it to the list of initargs.
  (let ((output '()))
    (dolist (scan (class-default-initargs class))
      (let* ((initarg (first scan))
             (value (third scan))
             (supplied-value (si::search-keyword initargs initarg)))
        (when (or (eq supplied-value '+initform-unsupplied+)
                  (eq supplied-value 'si::missing-keyword))
          (when (eq supplied-value '+initform-unsupplied+)
            (setf initargs (delete-keyword initarg initargs)))
          (setf output (list* (funcall value) initarg output)))))
    (if output
        (append initargs (nreverse output))
        initargs)))

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
  (or (forward-referenced-class-p class)
      (and (not (class-finalized-p class))
           (some #'unfinalizablep (class-direct-superclasses class)))))

(defun finalize-unless-forward (class)
  (unless (unfinalizablep class)
    (finalize-inheritance class)))


#+(or)(eval-when (:compile-toplevel :execute)
  (format t "std-slot-value-lsp *print-implicit-compile-form* t~%")
  (setf cmp::*print-implicit-compile-form* t))


#+(or)(eval-when (:compile-toplevel :execute)
  (gctools:wait-for-user-signal "About to standard-instance-access"))

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

(defun precompute-valid-initarg-keywords (class)
  (setf (class-valid-initargs class)
        (loop with methods = (nconc
                              (compute-applicable-methods
                               #'allocate-instance (list class))
                              (compute-applicable-methods
                               #'initialize-instance (list (class-prototype class)))
                              (compute-applicable-methods
                               #'shared-initialize (list (class-prototype class) t)))
              for m in methods
              for k = (method-keywords m)
              for aok-p = (method-allows-other-keys-p m)
              when aok-p return t
                else append k)))

(defun update-dependents (object initargs)
  (when *clos-booted*
    (map-dependents
     object
     #'(lambda (dep) (apply #'update-dependent object dep initargs)))))

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
	 unless (or (validate-superclass class superclass)
		    (not (eq *clos-booted* T)))
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
	    (and (eq c1 +the-standard-class+) (eq c2 +the-funcallable-standard-class+))
	    (and (eq c2 +the-standard-class+) (eq c1 +the-funcallable-standard-class+))))
      (or (forward-referenced-class-p class)
          (forward-referenced-class-p superclass))))

;;; NOTE: SBCL defines its own "SYSTEM-CLASS" to mean classes that are like
;;; built-in-classes but also subclassable. This may be worth consideration.
(defmethod validate-superclass ((class class) (superclass built-in-class))
  (or (eq superclass +the-t-class+) ; FIXME: necessary?
      ;; FIXME: Should gray streams go here?
      ;; Extensible sequences
      (eq superclass (find-class 'sequence))))

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
    (let ((x (find-if #'forward-referenced-class-p (rest cpl))))
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


(defmethod finalize-inheritance :after ((class std-class))
  #+static-gfs
  (static-gfs:invalidate-class-constructors class)
  #+(or) ;static-gfs
  (static-gfs:invalidate-changers* class)
  #+(or) ;static-gfs
  (static-gfs:invalidate-class-reinitializers* class)
  (std-class-generate-accessors class))

(defmethod compute-class-precedence-list ((class class))
  (compute-clos-class-precedence-list class (class-direct-superclasses class)))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defmacro mapappend (fun &rest args)
    `(reduce #'append (mapcar ,fun ,@args))))

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

(defun safe-slot-definition-location (slotd &optional default)
  (if (or (listp slotd) (slot-boundp slotd 'location))
      (slot-definition-location slotd)
      default))

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
      (let ((new-loc (safe-slot-definition-location slotd)))
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
(defmethod ensure-class-using-class ((class class) name core:&va-rest rest
				     &key direct-slots direct-default-initargs
                                       &allow-other-keys)
  (declare (ignore direct-default-initargs direct-slots))
  (clos::gf-log "In ensure-class-using-class (class class) %N")
  (clos::gf-log "     name -> {}%N" name)
  (multiple-value-bind (metaclass direct-superclasses options)
      (apply #'help-ensure-class rest)
    (declare (ignore direct-superclasses))
    (cond ((forward-referenced-class-p class)
	   (change-class class metaclass))
	  ((not (eq (class-of class) metaclass))
	   (error "When redefining a class, the metaclass can not change.")))
    (setf class (apply #'reinitialize-instance class :name name options))
    (when name
      (si:create-type-name name)
      (setf (find-class name) class))
    (clos::gf-log "Returning from ensure-class-using-class (class class)%N")
    class))

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
	 (numbered-slots (remove-if-not #'safe-slot-definition-location instance-slots))
	 (other-slots (remove-if #'safe-slot-definition-location instance-slots))
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

;;; ======================================================================
;;; STANDARD-OBJECT
;;;
;;; Standard-object has no slots and inherits only from t:
;;; (defclass standard-object (t) ())

(defun describe-slots (object stream)
  (let* ((class (class-of object))
         (slotds (class-slots class))
         (max-slot-name-length 24)
         (plist nil))
    ;; Go through the slots getting a max slot name length,
    ;; and also sorting the slots by :allocation.
    ;; (This code is based off of SBCL's SB-IMPL::DESCRIBE-INSTANCE.)
    (dolist (slotd slotds)
      (setf max-slot-name-length
            (max max-slot-name-length
                 (length (symbol-name
                          (slot-definition-name slotd)))))
      (push slotd (getf plist (slot-definition-allocation slotd))))
    ;; Now dump the info.
    (loop for (allocation slotds) on plist by #'cddr
          do (format stream "~&Slots with ~s allocation:" allocation)
             (dolist (slotd (nreverse slotds)) ; keep original order
               (let ((slot-name (slot-definition-name slotd)))
                 (format stream "~&  ~va: ~a"
                         max-slot-name-length slot-name
                         (if (slot-boundp object slot-name)
                             (slot-value object slot-name)
                             "Unbound"))))))
  object)

(defmethod describe-object ((obj standard-object) (stream t))
  (let* ((class (class-of obj)))
    (format stream "~&~S - ~S"
	    obj (class-name class))
    (describe-slots obj stream))
  obj)

;;; ----------------------------------------------------------------------
;;; CHECK INITARGS
;;;
;;; There are different sets of initialization arguments. First we have
;;; those coming from the :INITARG option in the slots. Then we have
;;; all declared initargs which are keyword arguments to methods defined
;;; on SHARED-INITIALIZE, REINITIALIZE-INSTANCE, etc. (See ANSI 7.1.2)
;;;

(defun valid-keywords-from-methods (&rest method-lists)
  (declare (dynamic-extent method-lists))
  (loop for methods in method-lists
     when (member t methods :key #'method-keywords)
     return t
     nconc methods))

(defun check-initargs (class initargs cached-keywords
		       &optional (slots (class-slots class)))
  ;; First get all initargs which have been declared in the given
  ;; methods, then check the list of initargs declared in the slots
  ;; of the class.
  (unless (eq cached-keywords t)
    (do* ((name-loc initargs (cddr name-loc))
	  (allow-other-keys nil)
	  (allow-other-keys-found nil)
	  (unknown-key-names nil))
	 ((null name-loc)
	  (when (and (not allow-other-keys) unknown-key-names)
            (core:simple-program-error "Unknown initialization options ~S for class ~A."
                                       (nreverse unknown-key-names) class)))
      (let ((name (first name-loc)))
	(cond ((null (cdr name-loc))
	       (core:simple-program-error
                "No value supplied for the init-name ~S." name))
	      ;; This check must be here, because :ALLOW-OTHER-KEYS is a valid
	      ;; slot-initarg.
	      ((and (eql name :ALLOW-OTHER-KEYS)
		    (not allow-other-keys-found))
	       (setf allow-other-keys (second name-loc)
		     allow-other-keys-found t))
	      ;; Check if the arguments is associated with a slot
	      ((member name slots :test #'member :key #'slot-definition-initargs))
	      ;; The initialization argument has been declared in some method
              ((member name cached-keywords))
	      (t
	       (push name unknown-key-names)))))))

(defun check-initargs-uncached (class initargs
                                &optional calls (slots (class-slots class)))
  ;; We try to avoid calling compute-applicable-methods since that's work.
  ;; (In simple tests, avoiding it gave a speedup of 2-3 times.)
  ;; So we first check if all the initargs correspond to slots. If they do,
  ;; great. If not we compute-applicable-methods to get more valid keywords.
  ;; This assumes that the likely case is all the initargs corresponding to
  ;; slots, but it shouldn't really be any slower if they don't.
  ;; CALLS is a list of (function arglist). These can be passed directly
  ;; to compute-applicable-methods.
  (do* ((name-loc initargs (cddr name-loc))
        (allow-other-keys nil)
        (allow-other-keys-found nil)
        (unknown-key-names nil)
        (methods nil)
        (methods-initialized-p nil))
       ((null name-loc)
        (when (and (not allow-other-keys) unknown-key-names)
          (core:simple-program-error
           "Unknown initialization options ~S for class ~A."
           (nreverse unknown-key-names) class)))
    (let ((name (first name-loc)))
      (cond ((null (cdr name-loc))
             (core:simple-program-error "No value supplied for the init-name ~S."
                                        name))
            ;; This check must be here, because :ALLOW-OTHER-KEYS is a valid
            ;; slot-initarg.
            ((and (eql name :ALLOW-OTHER-KEYS)
                  (not allow-other-keys-found))
             (setf allow-other-keys (second name-loc)
                   allow-other-keys-found t))
            ;; Check if the arguments is associated with a slot
            ((member name slots :test #'member :key #'slot-definition-initargs))
            ;; OK, doesn't correspond to a slot, so check the methods.
            ((progn
               (unless methods-initialized-p
                 (setf methods-initialized-p t
                       methods
                       (loop for call in calls
                             for methods
                               = (apply #'compute-applicable-methods call)
                             for methods2 = (valid-keywords-from-methods
                                             methods)
                             when (eq methods2 t) ; allow-other-keys
                               do (return-from check-initargs-uncached)
                             nconcing methods2)))
               (member name methods :test #'member :key #'method-keywords)))
            (t (push name unknown-key-names))))))

