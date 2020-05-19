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

#+(or)
(defmacro dbg-standard (fmt &rest args)
  `(format t ,fmt ,@args))
(defmacro dbg-standard (fmt &rest args))

;;; ----------------------------------------------------------------------
;;; INSTANCES INITIALIZATION AND REINITIALIZATION
;;;

(defmethod initialize-instance ((instance T) core:&va-rest initargs)
  (dbg-standard "standard.lsp:29  initialize-instance unbound instance ->~a~%" (eq (core:unbound) instance))
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

(defmethod shared-initialize ((instance T) slot-names #+(or)&rest core:&va-rest initargs)
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
      (core:vaslist-rewind initargs)
      (let* ((slot-initargs (slot-definition-initargs slotd))
             (slot-name (slot-definition-name slotd)))
        (or
         ;; Try to initialize the slot from one of the initargs.
         (do ((largs initargs)
              initarg
              val)
           (#+(or)(null largs) (= (core:vaslist-length largs) 0)
              (progn nil))
           (setf initarg #+(or)(pop largs) (core:vaslist-pop largs))
           #+(or)(when (endp largs) (simple-program-error "Wrong number of keyword arguments for SHARED-INITIALIZE, ~A" initargs))
           (when (= (core:vaslist-length largs) 0)
             (simple-program-error "Wrong number of keyword arguments for SHARED-INITIALIZE, ~A"
                                   (progn
                                     (core:vaslist-rewind initargs)
                                     (core:list-from-va-list initargs))))
           (unless (symbolp initarg)
             (simple-program-error "Not a valid initarg: ~A" initarg))
           (setf val #+(or)(pop l) (core:vaslist-pop largs))
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
  (dbg-standard "About to allocate-new-instance class->~a~%" class)
  (let ((x (core:allocate-new-instance class (class-size class))))
    (dbg-standard "Done allocate-new-instance unbound x ->~a~%" (eq (core:unbound) x))
    (mlog "In allocate-instance  x -> %s\n" x)
    x))

(defmethod allocate-instance ((class derivable-cxx-class) &rest initargs)
  (declare (ignore initargs))
  ;; derivable cxx objects are Instance_O's, so this is _probably_ okay.
  ;; (And allocate-new-instance uses the creator, so it'll do any C++ junk.)
  (core:allocate-new-instance class (class-size class)))

(defun uninitialized-funcallable-instance-closure (funcallable-instance)
  (lambda (core:&va-rest args)
    (declare (core:lambda-name uninitialized-funcallable-instance))
    (declare (ignore args))
    (error "The funcallable instance ~a has not been initialized with a function"
           funcallable-instance)))

(defmethod allocate-instance ((class funcallable-standard-class) &rest initargs)
  (declare (ignore initargs))
  (dbg-standard "About to allocate-new-funcallable-instance class->~a~%" class)
  (let ((x (core:allocate-new-funcallable-instance class (class-size class))))
    (dbg-standard "Done allocate-new-funcallable-instance unbound x ->~a~%" (eq (core:unbound) x))
    (mlog "In allocate-instance  x -> %s\n" x)
    ;; MOP says if you call a funcallable instance before setting its function,
    ;; the effects are undefined. (In the entry for set-funcallable-instance-function.)
    ;; But we can be nice.
    (set-funcallable-instance-function x (uninitialized-funcallable-instance-closure x))
    x))

(defmethod make-instance ((class class) &rest initargs)
  (declare (dynamic-extent initargs)) ; see NOTE in reinitialize-instance/T
  (dbg-standard "standard.lsp:128  make-instance class ->~a~%" class)
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
    #+mlog(or instance (error "allocate-instance returned NIL!!!!!!! class -> ~a initargs -> ~a" class initargs))
    (dbg-standard "standard.lsp:143   allocate-instance class -> ~a  instance checking if unbound -> ~a~%" class (eq instance (core:unbound)))
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

(defun has-forward-referenced-parents (class)
  (or (forward-referenced-class-p class)
      (and (not (class-finalized-p class))
           (some #'has-forward-referenced-parents
                 (class-direct-superclasses class)))))

(defun finalize-unless-forward (class)
  (unless (or
           ;; We used to have all forward-referenced classes "finalized", weirdly.
           (forward-referenced-class-p class)
           (find-if #'has-forward-referenced-parents (class-direct-superclasses class)))
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
  class)

(defmethod initialize-instance ((class class) &rest initargs &key direct-slots)
  (declare (dynamic-extent initargs)) ; see NOTE in reinitialize-instance/T
  (dbg-standard "standard.lsp:196  initialize-instance class->~a~%" class)
  ;; convert the slots from lists to direct slots
  (apply #'call-next-method class
         :direct-slots
         (loop for s in direct-slots
            collect (canonical-slot-to-direct-slot class s))
         initargs)
  (finalize-unless-forward class)
  ;; In this case we are assigning the stamp for the first time.
  (core:class-new-stamp class)
  class)

(defmethod shared-initialize ((class class) slot-names
                              &rest initargs &key direct-superclasses)
  (declare (dynamic-extent initargs)) ; see NOTE in reinitialize-instance/T
  ;; verify that the inheritance list makes sense
  (dbg-standard "standard.lsp:200 shared-initialize of class-> ~a direct-superclasses-> ~a~%" class direct-superclasses)
  (let* ((class (apply #'call-next-method class slot-names
		       :direct-superclasses
		       (if (slot-boundp class 'direct-superclasses)
			   (slot-value class 'direct-superclasses)
			   nil)
		       initargs))
	 (direct-superclasses (check-direct-superclasses class direct-superclasses)))
    (loop for c in (class-direct-superclasses class)
          unless (member c direct-superclasses :test #'eq)
            do (remove-direct-subclass c class))
    (setf (%class-direct-superclasses class) direct-superclasses)
    (loop for c in direct-superclasses
          do (add-direct-subclass c class))

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
  (dbg-standard "check-direct-superclasses class -> ~a  supplied-superclasses->~a  (type-of ~a) -> ~a~%" class supplied-superclasses class (type-of class))
  (if supplied-superclasses
      (loop for superclass in supplied-superclasses
	 ;; Until we process streams.lsp there are some invalid combinations
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

;;; Should it be standard-class only?
(defmethod validate-superclass ((class class) (superclass core:derivable-cxx-class)) t)

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
    (let ((slots (compute-slots class)))
      (setf (%class-slots class) slots
	    (class-size class) (compute-instance-size slots)
	    (%class-default-initargs class) (compute-default-initargs class)
	    (%class-finalized-p class) t)))
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
  (std-class-generate-accessors class))

(defmethod compute-class-precedence-list ((class class))
  (compute-clos-class-precedence-list class (class-direct-superclasses class)))

(eval-when (:compile-toplevel :execute #+clasp-boot :load-toplevel)
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

(defmethod compute-effective-slot-definition ((class class) name direct-slots)
  (flet ((direct-to-effective (old-slot)
	   (if (consp old-slot)
	       (copy-list old-slot)
	       (let ((initargs (slot-definition-to-plist old-slot)))
		 (apply #'make-instance
			(apply #'effective-slot-definition-class class initargs)
			initargs))))
	 (combine-slotds (new-slotd old-slotd)
           ;; NOTE: This may be the only place slot definition objects are
           ;; actually modified. Might want to change that, i.e. just
           ;; accumulate the properties as we go...
           ;; OK compute-slots does set the location also.
	   (let* ((new-type (slot-definition-type new-slotd))
		  (old-type (slot-definition-type old-slotd))
		  (loc1 (safe-slot-definition-location new-slotd))
		  (loc2 (safe-slot-definition-location old-slotd)))
	     (when loc2
	       (if loc1
		   (unless (eql loc1 loc2)
		     (error 'simple-error
			    :format-control "You have specified two conflicting slot locations:~%~D and ~F~%for slot ~A"
			    :format-arguments (list loc1 loc2 name)))
		   (progn
		     #+(or)
		     (format t "~%Assigning a default location ~D for ~A in ~A."
			     loc2 name (class-name class))
		     (setf (%slot-definition-location new-slotd) loc2))))
	     (setf (%slot-definition-initargs new-slotd)
		   (union (slot-definition-initargs new-slotd)
			  (slot-definition-initargs old-slotd)))
	     (unless (slot-definition-initfunction new-slotd)
	       (setf (%slot-definition-initform new-slotd)
		     (slot-definition-initform old-slotd)
		     (%slot-definition-initfunction new-slotd)
		     (slot-definition-initfunction old-slotd)))
	     (setf (%slot-definition-readers new-slotd)
		   (union (slot-definition-readers new-slotd)
			  (slot-definition-readers old-slotd))
		   (%slot-definition-writers new-slotd)
		   (union (slot-definition-writers new-slotd)
			  (slot-definition-writers old-slotd))
		   (%slot-definition-type new-slotd)
		   ;; FIXME! we should be more smart then this:
		   (cond ((subtypep new-type old-type) new-type)
			 ((subtypep old-type new-type) old-type)
			 (T `(and ,new-type ,old-type))))
	     new-slotd)))
    (reduce #'combine-slotds (rest direct-slots)
	    :initial-value (direct-to-effective (first direct-slots)))))

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
  (clos::gf-log "     name -> %s%N" name)
  (multiple-value-bind (metaclass direct-superclasses options)
      (apply #'help-ensure-class rest)
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
  ;; See :location use in hierarchy.lsp.
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
	              (value (if initfunc (funcall initfunc) (unbound))))
	         (setf (%slot-definition-location slotd) (list value))))
	      (t			; inherited shared slot
	       (dolist (c (class-precedence-list class))
		 (unless (eql c class)
		   (let ((other (find (slot-definition-name slotd)
				      (class-slots c)
				      :key #'slot-definition-name)))
		     (when (and other
				(eq (slot-definition-allocation other) allocation)
				(setf (%slot-definition-location slotd)
				      (slot-definition-location other)))
		       (return)))))))))
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
  (let* ((class (si:instance-class obj))
	 (slotds (class-slots class))
	 slotname has-shared-slots)
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
            (simple-program-error "Unknown initialization options ~S for class ~A."
                                  (nreverse unknown-key-names) class)))
      (let ((name (first name-loc)))
	(cond ((null (cdr name-loc))
	       (simple-program-error "No value supplied for the init-name ~S." name))
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
          (simple-program-error "Unknown initialization options ~S for class ~A."
                                (nreverse unknown-key-names) class)))
    (let ((name (first name-loc)))
      (cond ((null (cdr name-loc))
             (simple-program-error "No value supplied for the init-name ~S." name))
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

;;; ----------------------------------------------------------------------
;;; Methods

(defmethod describe-object ((obj std-class) (stream t))
  (let ((slotds (class-slots (si:instance-class obj))))
    (format stream "~%~A is an instance of class ~A"
	    obj (class-name (si:instance-class obj)))
    (do ((scan slotds (cdr scan))
	 (i 0 (1+ i)))
	((null scan))
      (declare (fixnum i))
      (print (slot-definition-name (car scan)) stream)
      (princ ":	" stream)
      (case (slot-definition-name (car scan))
	    ((SUPERIORS INFERIORS PRECEDENCE-LIST)
	     (princ "(" stream)
	     (do* ((scan (si:instance-ref obj i) (cdr scan))
		   (e (car scan) (car scan)))
		  ((null scan))
		  (prin1 (class-name e) stream)
		  (when (cdr scan) (princ " " stream)))
	     (princ ")"))
	    (otherwise (prin1 (si:instance-ref obj i) stream)))))
  obj)


