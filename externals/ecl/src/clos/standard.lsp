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

(defmethod initialize-instance ((instance T) &rest initargs)
  (apply #'shared-initialize instance 'T initargs))

(defmethod reinitialize-instance ((instance T) &rest initargs)
  (check-initargs (class-of instance) initargs
		  (valid-keywords-from-methods
                   (compute-applicable-methods
                    #'reinitialize-instance (list instance))
                   (compute-applicable-methods
                    #'shared-initialize (list instance t))))
  (apply #'shared-initialize instance '() initargs))

(defmethod shared-initialize ((instance T) slot-names &rest initargs)
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
      (let* ((slot-initargs (slot-definition-initargs slotd))
	     (slot-name (slot-definition-name slotd)))
	(or
	 ;; Try to initialize the slot from one of the initargs.
	 (do ((l initargs) initarg val)
	     ((null l) nil)
	   (setf initarg (pop l))
	   (when (endp l)
	     (simple-program-error "Wrong number of keyword arguments for SHARED-INITIALIZE, ~A"
				   initargs))
	   (unless (symbolp initarg)
	     (simple-program-error "Not a valid initarg: ~A" initarg))
	   (setf val (pop l))
	   (when (member initarg slot-initargs :test #'eq)
	     (setf (slot-value instance slot-name) val)
	     (return t)))
	 ;; Try to initialize the slot from its initform.
	 (when (and slot-names
		    (or (eq slot-names 'T)
			(member slot-name slot-names))
		    (not (slot-boundp instance slot-name)))
	   (let ((initfun (slot-definition-initfunction slotd)))
	     (when initfun
	       (setf (slot-value instance slot-name) (funcall initfun))))))
	)))
  instance)

;;; ----------------------------------------------------------------------
;;; CLASSES INITIALIZATION AND REINITIALIZATION
;;;

(defun compute-instance-size (slots)
  (loop for slotd in slots
     with last-location = 0
     with num-slots = 0
     when (eq (slot-definition-allocation slotd) :instance)
     do (let ((new-loc (safe-slot-definition-location slotd)))
	  (incf num-slots)
	  (when (and new-loc (> new-loc last-location))
	    (setf last-location new-loc)))
     finally (return (max num-slots (1+ last-location)))))

(defmethod allocate-instance ((class class) &rest initargs)
  (declare (ignore initargs))
  ;; FIXME! Inefficient! We should keep a list of dependent classes.
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (let ((x (si::allocate-raw-instance nil class (class-size class))))
    (si::instance-sig-set x)
    x))

(defmethod make-instance ((class class) &rest initargs)
  ;; Without finalization we can not find initargs.
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  ;; We add the default-initargs first, because one of these initargs might
  ;; be (:allow-other-keys t), which disables the checking of the arguments.
  ;; (Paul Dietz's ANSI test suite, test CLASS-24.4)
  (setf initargs (add-default-initargs class initargs))
  (let ((keywords (if (slot-boundp class 'valid-initargs)
		      (class-valid-initargs class)
		      (precompute-valid-initarg-keywords class))))
    (check-initargs class initargs nil (class-slots class) keywords))
  (let ((instance (apply #'allocate-instance class initargs)))
    (apply #'initialize-instance instance initargs)
    instance))

(defun delete-keyword (keyword list)
  (loop until (eq (getf list keyword list) list)
     do (remf list keyword))
  list)

(defun add-default-initargs (class initargs)
  (declare (si::c-local))
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
  (unless (find-if #'has-forward-referenced-parents (class-direct-superclasses class))
    (finalize-inheritance class)))

(defmethod initialize-instance ((class class) &rest initargs &key direct-slots)
  (declare (ignore sealedp))
  ;; convert the slots from lists to direct slots
  (apply #'call-next-method class
         :direct-slots
	 (loop for s in direct-slots
	    collect (canonical-slot-to-direct-slot class s))
         initargs)
  (finalize-unless-forward class)
  class)

(defmethod shared-initialize ((class class) slot-names &rest initargs &key direct-superclasses)
  ;; verify that the inheritance list makes sense
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
    (setf (class-direct-superclasses class) direct-superclasses)
    (loop for c in direct-superclasses
       do (add-direct-subclass c class))
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
           when (eq k t)
           return t
           append k)))

(defun update-dependents (object initargs)
  (when *clos-booted*
    (map-dependents
     object
     #'(lambda (dep) (apply #'update-dependent object dep initargs)))))

(defmethod shared-initialize ((class std-class) slot-names &rest initargs &key
                              (optimize-slot-access (list *optimize-slot-access*))
                              sealedp)
  (declare (ignore initargs slot-names))
  (setf (slot-value class 'optimize-slot-access) (first optimize-slot-access)
	(slot-value class 'sealedp) (and sealedp t))
  (setf class (call-next-method))
  (update-dependents class initargs)
  class)

(defmethod add-direct-subclass ((parent class) child)
  (pushnew child (class-direct-subclasses parent)))

(defmethod remove-direct-subclass ((parent class) child)
  (setf (class-direct-subclasses parent)
	(remove child (class-direct-subclasses parent))))

(defun check-direct-superclasses (class supplied-superclasses)
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
  (or (eq superclass +the-t-class+)
      (let ((c1 (class-of class))
	    (c2 (class-of superclass)))
	(or (eq c1 c2)
	    (and (eq c1 +the-standard-class+) (eq c2 +the-funcallable-standard-class+))
	    (and (eq c2 +the-standard-class+) (eq c1 +the-funcallable-standard-class+))
	    ))
      (forward-referenced-class-p superclass)
      ))

;;; ----------------------------------------------------------------------
;;; FINALIZATION OF CLASS INHERITANCE
;;;
(defun forward-referenced-class-p (x)
  (let ((y (find-class 'FORWARD-REFERENCED-CLASS nil)))
    (and y (si::subclassp (class-of x) y))))

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
    (setf (class-precedence-list class) cpl)
    (let ((slots (compute-slots class)))
      (setf (class-slots class) slots
	    (class-size class) (compute-instance-size slots)
	    (class-default-initargs class) (compute-default-initargs class)
	    (class-finalized-p class) t))
    ;;
    ;; When a class is sealed we rewrite the list of direct slots to fix
    ;; their locations. This may imply adding _new_ direct slots.
    ;;
    (when (class-sealedp class)
      (let* ((free-slots (delete-duplicates (mapcar #'slot-definition-name (class-slots class))))
	     (all-slots (class-slots class)))
	;;
	;; We first search all slots that belonged to unsealed classes and which
	;; therefore have no fixed position.
	;;
	(loop for c in cpl
	   do (loop for slotd in (class-direct-slots c)
		 when (safe-slot-definition-location slotd)
		 do (setf free-slots (delete (slot-definition-name slotd) free-slots))))
	;;
	;; We now copy the locations of the effective slots in this class to
	;; the class direct slots.
	;;
	(loop for slotd in (class-direct-slots class)
	   do (let* ((name (slot-definition-name slotd))
		     (other-slotd (find name all-slots :key #'slot-definition-name)))
		(setf (slot-definition-location slotd)
		      (slot-definition-location other-slotd)
		      free-slots (delete name free-slots))))
	;;
	;; And finally we add one direct slot for each inherited slot that did
	;; not have a fixed location.
	;;
	(loop for name in free-slots
	   with direct-slots = (class-direct-slots class)
	   do (let* ((effective-slotd (find name all-slots :key #'slot-definition-name))
		     (def (direct-slot-to-canonical-slot effective-slotd)))
		(push (apply #'make-instance (direct-slot-definition-class class def)
			     def)
		      direct-slots))
	   finally (setf (class-direct-slots class) direct-slots))))
    ;;
    ;; This is not really needed, because when we modify the list of slots
    ;; all instances automatically become obsolete (See change.lsp)
    ;(make-instances-obsolete class)
    ;;
    ;; But this is really needed: we have to clear the different type caches
    ;; for type comparisons and so on.
    ;;
    (si::subtypep-clear-cache)
    )
  ;; As mentioned above, when a parent is finalized, it is responsible for
  ;; invoking FINALIZE-INHERITANCE on all of its children. Obviously,
  ;; this only makes sense when the class has been defined.
  (dolist (subclass (reverse (class-direct-subclasses class)))
    (finalize-unless-forward subclass))
  ;;
  ;; We create various caches to more rapidly find the slot locations and
  ;; slot definitions.
  (std-create-slots-table class)
  )

(defmethod finalize-inheritance ((class std-class))
  (call-next-method)
  (std-class-generate-accessors class))

(defmethod compute-class-precedence-list ((class class))
  (compute-clos-class-precedence-list class (class-direct-superclasses class)))

(eval-when (:compile-toplevel :execute)
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
		     (setf (slot-definition-location new-slotd) loc2))))
	     (setf (slot-definition-initargs new-slotd)
		   (union (slot-definition-initargs new-slotd)
			  (slot-definition-initargs old-slotd)))
	     (unless (slot-definition-initfunction new-slotd)
	       (setf (slot-definition-initform new-slotd)
		     (slot-definition-initform old-slotd)
		     (slot-definition-initfunction new-slotd)
		     (slot-definition-initfunction old-slotd)))
	     (setf (slot-definition-readers new-slotd)
		   (union (slot-definition-readers new-slotd)
			  (slot-definition-readers old-slotd))
		   (slot-definition-writers new-slotd)
		   (union (slot-definition-writers new-slotd)
			  (slot-definition-writers old-slotd))
		   (slot-definition-type new-slotd)
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
(defmethod ensure-class-using-class ((class class) name &rest rest
				     &key direct-slots direct-default-initargs)
  (declare (ignore direct-default-initargs direct-slots))
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
    class))

(defun coerce-to-class (class-or-symbol &optional (fail nil))
  (cond ((si:instancep class-or-symbol) class-or-symbol)
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
  ;; This an ECL extension. We are allowed to specify the location of
  ;; a direct slot. Consequently we have to first sort the ones which
  ;; have been predefined and then assign locations _after_ the last
  ;; assigned slot. Note the generalized comparison, which pushes all
  ;; slots without a defined location to the end of the list.
  (let* ((size (compute-instance-size slots))
	 (instance-slots (remove :instance slots :key #'slot-definition-allocation
						 :test-not #'eq))
	 (numbered-slots (remove-if-not #'safe-slot-definition-location instance-slots))
	 (other-slots (remove-if #'safe-slot-definition-location instance-slots))
	 (aux (make-array size :element-type 't :adjustable nil :initial-element nil)))
    (loop for i in numbered-slots
       do (let ((loc (slot-definition-location i)))
	    (when (aref aux loc)
	      (error 'simple-error
		     :format-control "Slots ~A and ~A are said to have the same location in class ~A."
		     :format-ars (list (aref aux loc) i class)))
	    (setf (aref aux loc) i)))
    (loop for i in other-slots
       with index = 0
       do (loop while (aref aux index)
	       do (incf index)
	       finally (setf (aref aux index) i
			     (slot-definition-location i) index)))
    slots))

(defmethod compute-slots :around ((class class))
  (class-compute-slots class (call-next-method)))

(defun std-class-compute-slots (class slots)
  (declare (si::c-local))
  (let* ((direct-slots (class-direct-slots class)))
    (dolist (slotd slots)
      (let* ((name (slot-definition-name slotd))
	     (allocation (slot-definition-allocation slotd)))
	(cond ((not (eq (slot-definition-allocation slotd) :class)))
	      ((find name direct-slots :key #'slot-definition-name) ; new shared slot
	       (let* ((initfunc (slot-definition-initfunction slotd))
	              (value (if initfunc (funcall initfunc) (unbound))))
	         (setf (slot-definition-location slotd) (list value))))
	      (t			; inherited shared slot
	       (dolist (c (class-precedence-list class))
		 (unless (eql c class)
		   (let ((other (find (slot-definition-name slotd)
				      (class-slots c)
				      :key #'slot-definition-name)))
		     (when (and other
				(eq (slot-definition-allocation other) allocation)
				(setf (slot-definition-location slotd)
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

(defmethod describe-object ((obj standard-object) (stream t))
  (let* ((class (si:instance-class obj))
	 (slotds (class-slots class))
	 slotname has-shared-slots)
    (format stream "~%~S is an instance of class ~A"
	    obj (class-name class))
    (when slotds
      ;; print instance slots
      (format stream "~%it has the following instance slots")
      (dolist (slot slotds)
	(setq slotname (slot-definition-name slot))
	(case (slot-definition-allocation slot)
	  (:INSTANCE
	   (format stream "~%~A:~24,8T~A"
		   slotname
		   (if (slot-boundp obj slotname)
		       (slot-value obj slotname) "Unbound")))
	  ;; :CLASS
	  (T (setq has-shared-slots t))))
      (when has-shared-slots
	;; print class slots
	(format stream "~%it has the following class slots")
	(dolist (slot slotds)
	  (setq slotname (slot-definition-name slot))
	  (unless (eq (slot-definition-allocation slot) :INSTANCE)
	    (format stream "~%~A:~24,8T~A"
		    slotname
		    (if (slot-boundp obj slotname)
			(slot-value obj slotname) "Unbound")))))))
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
  (loop for methods in method-lists
     when (member t methods :key #'method-keywords)
     return t
     nconc methods))

(defun check-initargs (class initargs &optional methods
		       (slots (class-slots class))
                       cached-keywords)
  ;; First get all initargs which have been declared in the given
  ;; methods, then check the list of initargs declared in the slots
  ;; of the class.
  (unless (or (eq methods t) (eq cached-keywords t))
    (do* ((name-loc initargs (cddr name-loc))
	  (allow-other-keys nil)
	  (allow-other-keys-found nil)
	  (unknown-key nil))
	 ((null name-loc)
	  (when (and (not allow-other-keys) unknown-key)
	    (simple-program-error "Unknown initialization option ~S for class ~A"
				  unknown-key class)))
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
	      ((and methods (member name methods :test #'member :key #'method-keywords)))
	      (t
	       (setf unknown-key name)))))))

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
