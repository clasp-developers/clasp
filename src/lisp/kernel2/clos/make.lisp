(in-package #:clos)

(defgeneric allocate-instance (class &rest initargs &key &allow-other-keys))
(defgeneric make-instance (class &rest initargs &key &allow-other-keys))
(defgeneric initialize-instance (object &rest initargs &key &allow-other-keys))
(defgeneric reinitialize-instance (object &rest initargs &key &allow-other-keys))
(defgeneric shared-initialize (object slot-names &rest initargs
                               &key &allow-other-keys))

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

(defmethod allocate-instance ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (core:allocate-raw-instance class (make-rack-for-class class)))

(defun uninitialized-funcallable-instance-closure (funcallable-instance)
  (lambda (&rest args)
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

(defmethod make-instance ((class std-class) &rest initargs)
  (declare (dynamic-extent initargs)) ; see NOTE in reinitialize-instance/T
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
    (check-initargs class initargs keywords))
  (let ((instance (apply #'allocate-instance class initargs)))
    (apply #'initialize-instance instance initargs)
    instance))

(defmethod make-instance ((name symbol) &rest initargs)
  (apply #'make-instance (find-class name) initargs))

(defun add-default-initargs (class initargs)
  ;; Here, for each slot which is not mentioned in the initialization
  ;; arguments, but which has a value associated with :DEFAULT-INITARGS,
  ;; we compute the value and add it to the list of initargs.
  (loop for (initkey _ valuef) in (class-default-initargs class)
        when (eq 'core::missing-keyword
                 (getf initargs initkey 'core::missing-keyword))
          collect initkey into defaulted
          and collect (funcall valuef) into defaulted
        finally (return (if (null defaulted)
                            initargs
                            (append initargs defaulted)))))

(defmethod initialize-instance ((instance t) &rest initargs)
  (apply #'shared-initialize instance t initargs))

(defmethod reinitialize-instance ((instance t) &rest initargs)
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

(defmethod shared-initialize ((instance t) slot-names &rest initargs)
  (unless (zerop (mod (length initargs) 2))
    (core:simple-program-error "Odd number of keyword arguments for ~a"
                               'shared-initialize))
  (dolist (slotd (class-slots (class-of instance)))
    (let ((slot-initargs (slot-definition-initargs slotd))
          (slot-name (slot-definition-name slotd)))
      ;; Initialize the slot from an initarg, if one was provided.
      (loop for (key val) on initargs by #'cddr
            ;; FIXME: This is both inefficient and insufficiently correct.
            ;; Inefficient because the same key can be checked multiple times
            ;; as we loop over slots. Incorrect in that all slots may be resolved
            ;; before a bad keyword appears.
            ;; The generic function should probably check instead.
            unless (symbolp key)
              do (core:simple-program-error "Not a valid initarg: ~A" key)
            when (member key slot-initargs)
              do (setf (slot-value instance slot-name) val)
                 (go initialized))
      ;; If it hasn't been initialized yet, and is in slot-names,
      ;; use the initform.
      (when (and (or (eq slot-names 't) (member slot-name slot-names))
              (not (slot-boundp instance slot-name)))
        (let ((initfun (slot-definition-initfunction slotd)))
          (when initfun
            (setf (slot-value instance slot-name) (funcall initfun))))))
    ;; implicit tagbody
    initialized)
  instance)
