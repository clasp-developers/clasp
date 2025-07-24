(in-package #:cross-clasp.clasp.clos)

(defclass specializer () ())

;; These aren't actual host classes. That's because working around MOP's requirements
;; for class initialization is more trouble than it's worth.
;; (We never want to make instances of instances of COMPILER-CLASS, but that doesn't
;;  really help since MOP doesn't much contemplate such a thing.)
;; We do use MOP accessors so that e.g. anatomicl has an easier time dealing with
;; what we have.
;; Possibly we could inherit from CLASS instead of STANDARD-CLASS, but then
;; a) we'd need to put in all these slots ourselves anyway, and
;; b) nobody ever does that so hosts may get stupid.
(defclass compiler-class (specializer)
  ((%name :initarg :name :accessor class-name :reader name)
   (%supers :initarg :supers :reader mop:class-direct-superclasses)
   (%subs :initform nil :accessor direct-subclasses :reader mop:class-direct-subclasses)
   (%class-precedence-list :accessor class-precedence-list
                           :reader mop:class-precedence-list)
   (%direct-slots :initarg :direct-slots :reader mop:class-direct-slots)
   (%slots :accessor slots :reader mop:class-slots)
   ;; These two are not like MOP - they are literal plists.
   (%direct-default-initargs :initarg :direct-default-initargs
                             :reader direct-default-initargs)
   (%default-initargs :accessor default-initargs)
   (%metaclass :initarg :metaclass :reader metaclass)))

;;; Used in build environment
(defun core::subclassp (class1 class2)
  (assert (and (typep class1 'compiler-class)
            (typep class2 'compiler-class)))
  (member class2 (class-precedence-list class1)))

;;; Needed for Anatomicl
(defmethod closer-mop:class-finalized-p ((class compiler-class)) t)

(defun fake-initfunction ()
  (error "Somehow called a cross-clasp fake initfunction!"))

(defmethod closer-mop:class-direct-default-initargs ((class compiler-class))
  (loop for (key form) on (direct-default-initargs class) by #'cddr
        collect (list key form #'fake-initfunction)))
(defmethod closer-mop:class-default-initargs ((class compiler-class))
  (loop for (key form) on (default-initargs class) by #'cddr
        collect (list key form #'fake-initfunction)))

(defmethod print-object ((o compiler-class) stream)
  (print-unreadable-object (o stream :type t)
    (write (name o) :stream stream))
  o)

(defclass eql-specializer (specializer)
  ((%object :initarg :object :reader mop:eql-specializer-object)))

(defvar *eql-specializers* (make-hash-table))
(defun intern-eql-specializer (object)
  ;; Not sure this function is actually required
  ;; (versus making multiple eql specializers)
  ;; but it's trivial so why not.
  (multiple-value-bind (spec presentp) (gethash object *eql-specializers*)
    (if presentp
        spec
        (setf (gethash object *eql-specializers*) (make-instance 'eql-specializer
                                                    :object object)))))

(defclass compiler-slotd ()
  ((%name :initarg :name :reader name :reader closer-mop:slot-definition-name)
   (%initform :initarg :initform :reader initform)
   (%initformp :initarg :initformp :reader initformp :type boolean)
   (%initargs :initarg :initargs :initform () :reader initargs)
   (%readers :initarg :readers :initform () :reader readers)
   (%writers :initarg :writers :initform () :reader writers)
   (%type :initarg :type :initform t :reader stype
          :reader closer-mop:slot-definition-type)
   (%allocation :initarg :allocation :initform :instance :reader allocation)
   (%location :initarg :location :initform nil :reader location)))
(defclass direct-slotd (compiler-slotd)
  (;; only relevant for direct slots
   (%effective-readers :initform nil :accessor effective-readers)
   (%effective-writers :initform nil :accessor effective-writers)))
(defclass effective-slotd (compiler-slotd) ())

(defmethod print-object ((o compiler-slotd) stream)
  (print-unreadable-object (o stream :type t)
    (write (name o) :stream stream))
  o)

(defclass compiler-method-combination ()
  ((%name :initarg :name :reader name)
   (%options :initarg :options :reader options)))

;;; we could cache these but meh
(defun ensure-method-combination (spec)
  (make-instance 'compiler-method-combination
    :name (first spec) :options (rest spec)))

(defclass compiler-generic ()
  ((%name :initarg :name :reader name)
   (%lambda-list :initarg :lambda-list :reader lambda-list)
   (%required-parameters :initarg :reqargs :reader required-parameters)
   ;; this is broader than whether there's a &rest - it's whether there are
   ;; any more parameters after the required parameters at all.
   ;; so it's also true with &optional or &key.
   (%restp :initarg :restp :reader restp)
   (%apo :initarg :apo :reader apo) ; argument precedence order
   ;; a vector with T if a parameter is specialized at all, otherwise NIL.
   (%specializer-profile :initarg :specializer-profile
                         :accessor specializer-profile)
   (%method-combination :initarg :method-combination
                        :reader gf-method-combination
                        :type compiler-method-combination)
   (%method-class :initarg :method-class :reader method-class)
   (%declarations :initarg :declarations :reader declarations)
   (%gf-class :initarg :class :reader gf-class)
   (%methods :initarg :methods :initform () :accessor methods)))

(defmethod initialize-instance :after ((i compiler-generic) &rest initargs)
  (declare (ignore initargs))
  (setf (specializer-profile i)
        (make-array (length (apo i)) :initial-element nil)))

(defun update-specializer-profile (generic specializers
                                   &optional (find-class #'cross-clasp:find-compiler-class))
  (loop with tclass = (funcall find-class 't)
        with sprof = (specializer-profile generic)
        for i from 0
        for spec in specializers
        unless (eq spec tclass)
          do (setf (aref sprof i) t)))

(defmethod print-object ((o compiler-generic) stream)
  (print-unreadable-object (o stream :type t)
    (write (name o) :stream stream))
  o)

(defclass compiler-method ()
  ((%gf :initarg :gf :reader gf)
   (%lambda-list :initarg :lambda-list :reader lambda-list)
   (%specializers :initarg :specializers :reader specializers)
   (%qualifiers :initarg :qualifiers :reader qualifiers)
   (%mclass :initarg :class :reader mclass)
   (%keywords :initarg :keywords :reader method-keywords)
   (%aok-p :initarg :aok-p :reader method-allows-other-keys-p)
   ;; A cons (LEAF mform &optional fform) or (CONTF mform fform)
   ;; mform, at load time, will be evaluated to produce the
   ;; method function. fform, if present, evaluates to
   ;; the raw leaf or contf function. The latter is only used
   ;; in effective methods (method-combination.lisp).
   (%function :initarg :function-form :reader method-function
              :type (cons (member leaf contf)))))

(defmethod print-object ((o compiler-method) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "~s ~{~s ~}(~{~s~^ ~})"
            (name (gf o)) (qualifiers o)
            (mapcar #'name (specializers o))))
  o)

(defclass compiler-accessor (compiler-method)
  ((%slot :initarg :slot :reader slot)
   (%keywords :initform nil)
   (%aok-p :initform nil)))
(defclass compiler-reader (compiler-accessor) ())
(defclass compiler-writer (compiler-accessor) ())

(defclass effective-accessor (compiler-method)
  ((%original :initarg :original :reader original)
   (%allocation :initform :instance :reader allocation)
   (%location :initarg :location :reader location)
   (%keywords :initform nil)
   (%aok-p :initform nil)))
(defclass effective-reader (effective-accessor) ())
(defclass effective-writer (effective-accessor) ())

(defgeneric sclass (effective-accessor))
(defmethod sclass ((e effective-reader)) (first (specializers e)))
(defmethod sclass ((e effective-writer)) (second (specializers e)))

(defun parse-slot-specifier (slot-specifier)
  (etypecase slot-specifier
    (symbol (values slot-specifier nil nil nil nil nil t :instance nil))
    (cons (loop with name = (first slot-specifier)
                with initargs = ()
                with initform
                with initformp = nil
                with readers = ()
                with writers = ()
                with type = t with typep = nil
                with allocation = :instance with allocationp = nil
                with location = nil
                for (key value) on (rest slot-specifier) by #'cddr
                do (ecase key
                     ((:initform)
                      (if initformp
                          (error "duplicate initform")
                          (setf initform value initformp t)))
                     ((:initarg)
                      (check-type value symbol "a valid initarg")
                      (push value initargs))
                     ((:reader)
                      (check-type value symbol "a valid reader name")
                      (push value readers))
                     ((:writer)
                      (check-type value
                                  (or symbol
                                    (cons (eql setf) (cons symbol null)))
                                  "a valid writer name")
                      (push value writers))
                     ((:accessor)
                      (check-type value symbol "a valid accessor name")
                      (push value readers)
                      (push `(setf ,value) writers))
                     ((:type)
                      (if typep
                          (error "duplicate type")
                          (setf type value typep t)))
                     ((:allocation)
                      (cond (allocationp (error "duplicate allocation"))
                            ((and (eq value :class) location)
                             (error "~s ~s and ~s are incompatible"
                                    :class :allocation :location))
                            (t (setf allocation value allocationp t))))
                     ((:location)
                      (check-type value (integer 0) "a slot location")
                      (cond (location (error "duplicate location"))
                            ((eq allocation :class)
                             (error "~s ~s and ~s are incompatible"
                                    :class :allocation :location))
                            (t (setf location value)))))
                finally (return (values name
                                        initform initformp initargs
                                        readers writers type
                                        allocation location))))))

(defun make-direct-slotd (slot-specifier)
  (multiple-value-bind (name initform initformp initargs
                        readers writers type allocation location)
      (parse-slot-specifier slot-specifier)
    (make-instance 'direct-slotd
      :name name :initform initform :initformp initformp
      :initargs initargs :readers readers :writers writers
      :type type :allocation allocation :location location)))

(defun parse-class-options (options)
  (loop with metaclass with documentation
        with default-initargs with default-initargs-p
        for (key . value) in options
        do (ecase key
             ((:metaclass)
              (if metaclass
                  (error "Duplicate ~s option" :metaclass)
                  (destructuring-bind (clname) value
                    (setf metaclass clname))))
             ((:documentation)
              (if documentation
                  (error "Duplicate ~s option" :documentation)
                  (destructuring-bind (docstring) value
                    (setf documentation docstring))))
             ((:default-initargs)
              (if default-initargs-p
                  (error "Duplicate ~s option" :default-initargs)
                  (setf default-initargs value
                        default-initargs-p t))))
        finally (return (values metaclass
                                documentation default-initargs))))

(defun compute-effective-slot (slotds location)
  (flet ((app (reader)
           (remove-duplicates
            (mapcan (lambda (x) (copy-list (funcall reader x))) slotds)
            ;; EQUAL for setf writer names
            ;; i mean this rem-dup might be pointless anyway
            :test #'equal)))
    (multiple-value-bind (initform initformp)
        (loop for slotd in slotds
              when (initformp slotd)
                return (values (initform slotd) t)
              finally (return (values nil nil)))
      ;; Verify location
      (loop with locp = nil
            for slotd in slotds
            for loc = (location slotd)
            when loc
              do (cond (locp
                        (error "Duplicate ~s" :location))
                       ((= loc location))
                       (t (error "Location mismatch for ~s"
                                 (name (first slotds)))))
                 (setf locp t))
      ;; Verify allocation
      (loop with allocation = (allocation (first slotds))
            for slotd in (rest slotds)
            for alloc = (allocation slotd)
            when alloc
              do (unless (eq alloc allocation)
                   (error "Allocation mismatch for ~s"
                          (name (first slotds)))))
      ;; Make the slotd
      (make-instance 'effective-slotd
        :name (name (first slotds)) :initform initform :initformp initformp
        :initargs (app #'initargs)
        :readers (app #'readers) :writers (app #'writers)
        :type `(and ,@(loop for slotd in slotds
                            for type = (stype slotd)
                            unless (eq type 't) ; who care
                              collect type))
        :allocation (allocation (first slotds))
        :location (ecase (allocation (first slotds))
                    (:instance location)
                    (:class nil))))))

(defun compute-slots (cpl)
  (let* (;; An alist from slot names to lists of direct slotds
         ;; of the same name in order from most to least specific.
         ;; The different slotds are sorted least specific first.
         (direct-slots
           (loop with result = nil
                 for class in cpl
                 do (loop for slotd in (reverse (mop:class-direct-slots class))
                          for name = (name slotd)
                          for existing = (assoc name result)
                          when existing
                            do (push slotd (cdr existing))
                          else
                            do (push (list name slotd) result))
                 finally (return result))))
    (loop for (_ . direct-slotds) in direct-slots
          for i from 0
          ;; now make the slotds most specific first.
          collect (compute-effective-slot (reverse direct-slotds) i))))

(defun compute-default-initargs (cpl)
  ;; We didn't do the canonicalization stuff out of laziness and
  ;; because initfunctions don't make much sense here, so this is
  ;; just append with duplicates removed.
  (loop with seen = ()
        for class in cpl
        for inits = (direct-default-initargs class)
        nconc (loop for (k v) on inits by #'cddr
                    unless (member k seen)
                      do (push k seen)
                      and collect k
                      and collect v)))

(defun finalizedp (class)
  (slot-boundp class '%class-precedence-list))

(defun finalize-inheritance (class)
  (let* ((supers (mop:class-direct-superclasses class))
         (_ (loop for sup in supers
                  unless (finalizedp sup)
                    do (finalize-inheritance sup)))
         (cpl (compute-class-precedence-list class supers))
         (effective-slotds (compute-slots cpl))
         (default-initargs (compute-default-initargs cpl)))
    (declare (ignore _))
    (setf (class-precedence-list class) cpl
          (slots class) effective-slotds
          (default-initargs class) default-initargs))
  (values))

(defun initialize-compiler-class (class name supers slotds options
                                  &key (find-class
                                        #'cross-clasp:find-compiler-class))
  (multiple-value-bind (metaclass documentation default-initargs)
      (parse-class-options options)
    (declare (ignore documentation)) ; FIXME?
    (let* ((metaclass (or metaclass 'standard-class))
           (supers (or supers
                     (ecase metaclass
                       ((standard-class) '(standard-object))
                       ((funcallable-standard-class)
                        '(funcallable-standard-object))
                       ((built-in-class)
                        (if (eq name t) ; weird special case
                            ()
                            '(t))))))
           (supers (mapcar find-class supers))
           (slotds (mapcar #'make-direct-slotd slotds))
           (rmetaclass (funcall find-class metaclass)))
      (reinitialize-instance
       class
       :supers supers :direct-slots slotds
       :direct-default-initargs default-initargs
       :metaclass rmetaclass)
      (finalize-inheritance class)
      (loop for super in supers
            do (push class (direct-subclasses super)))
      class)))

(defun make-compiler-class (name supers slotds options)
  (initialize-compiler-class (make-instance 'compiler-class
                               :name name)
                             name supers slotds options))

(defun primitive-accessor (class)
  (ecase (name (metaclass class))
    (standard-class 'standard-instance-access)
    (funcallable-standard-class 'funcallable-standard-instance-access)))

(defun expand-early-allocate-instance (class)
  (let ((funcallablep (find (cross-clasp:find-compiler-class
                             'funcallable-standard-object)
                            (mop:class-precedence-list class))))
    `(let* ((class (find-class ',(name class)))
            (slotds (with-early-accessors (std-class)
                      (class-slots class)))
            (size (length slotds))
            (stamp (core:class-stamp-for-instances class)))
       ;; note that we don't set the funcallable instance function
       ;; for funcallables. that's because we're going to set those up
       ;; later anyway.
       (,(if funcallablep
             'core:allocate-raw-funcallable-instance
             'core:allocate-raw-instance)
        class
        (core:make-rack
         size slotds stamp (core:unbound))))))

(defmacro early-allocate-instance (class-name)
  (expand-early-allocate-instance (cross-clasp:find-compiler-class class-name)))

;;; Used to make slots in the weird early parts.
;;; They expand into this. The delay is important since when they expand
;;; into this, the compiler class doesn't have slots yet. Or something.
(defmacro earlier-allocate-instance (class-name)
  `(core:allocate-standard-instance
    (find-class ',class-name)
    ,(length (mop:class-slots (cross-clasp:find-compiler-class class-name)))))

(defmacro early-initialize-instance (class-name object &rest initargs)
  (let* ((class (cross-clasp:find-compiler-class class-name))
         (sia (primitive-accessor class))
         (slots (mop:class-slots class))
         (o (gensym "OBJECT")))
    `(let ((,o ,object))
       (setf
        ,@(loop with invalid-keys
                for (key val) on initargs by #'cddr
                for slotd = (loop for slotd in slots
                                  when (member key (initargs slotd))
                                    return slotd
                                  finally (push key invalid-keys)
                                          (return nil))
                when slotd
                  collect `(,sia ,o ,(location slotd))
                  and collect val
                  and do (setf slots (remove slotd slots))
                finally (unless (null invalid-keys)
                          (error "Unrecognized or duplicate initargs: ~s"
                                 invalid-keys)))
        ;; FIXME: Doesn't detect initargs that genuinely correspond to no slot.
        ;; Oh well!
        ,@(loop for (key form) in (default-initargs class)
                for slotd = (loop for slotd in slots
                                  when (member key (initargs slotd))
                                    return slotd)
                when slotd
                  collect `(,sia ,o ,(location slotd))
                  and collect form
                  and do (setf slots (remove slotd slots)))
        ;; Initialize other slots with initforms, if they have em.
        ,@(loop for slotd in slots
                when (initformp slotd)
                  collect `(,sia ,o ,(location slotd))
                  and collect (initform slotd)))
       ,o)))

(defmacro early-make-instance (class-name &rest initargs)
  `(early-initialize-instance
    ,class-name
    (early-allocate-instance ,class-name)
    ,@initargs))

;; returns a bunch of bindings for macrolet.
(defun early-accessors (class)
  (loop with sia = (primitive-accessor class)
        for slotd in (mop:class-slots class)
        for loc = (location slotd)
        ;; for accessors we just use all possible readers, even if
        ;; there's no corresponding writers.
        ;; this is early! chaos reigns!
        for accessors = (readers slotd)
        nconc (loop for acc in accessors
                    collect `(,acc (object)
                               (list ',sia object ',loc)))))

(defmacro with-early-accessors ((&rest class-names) &body body)
  `(macrolet (,@(loop for name in class-names
                      for class = (cross-clasp:find-compiler-class name)
                      nconc (early-accessors class)))
     ,@body))

(defun build-direct-slot-form (slotd)
  `(early-initialize-instance
    standard-direct-slot-definition
    (earlier-allocate-instance standard-direct-slot-definition)
    :name ',(name slotd)
    :initform ,(if (initformp slotd)
                   `',(initform slotd)
                   '+initform-unsupplied+)
    :initfunction ,(if (initformp slotd)
                       `#'(lambda ()
                            ,(initform slotd))
                       nil)
    :initargs ',(initargs slotd)
    :readers ',(readers slotd)
    :writers ',(writers slotd)
    :type ',(stype slotd)
    :allocation ',(allocation slotd)
    :location ',(location slotd)))

(defun build-slot-form (compiler-slotd)
  `(early-initialize-instance
    standard-effective-slot-definition
    (earlier-allocate-instance standard-effective-slot-definition)
    :name ',(name compiler-slotd)
    :initform ,(if (initformp compiler-slotd)
                   `',(initform compiler-slotd)
                   '+initform-unsupplied+)
    :initfunction ,(if (initformp compiler-slotd)
                       `#'(lambda ()
                            ,(initform compiler-slotd))
                       nil)
    :initargs ',(initargs compiler-slotd)
    :readers ',(readers compiler-slotd)
    :writers ',(writers compiler-slotd)
    :type ',(stype compiler-slotd)
    :allocation ',(allocation compiler-slotd)
    :location ,(ecase (allocation compiler-slotd)
                 (:instance `',(location compiler-slotd))
                 (:class `(list ,(if (initformp compiler-slotd)
                                     (initform compiler-slotd)
                                     '(core:unbound)))))))

(defun canonicalized-default-initargs-form (default-initargs)
  `(list
    ,@(loop for (k v) on default-initargs by #'cddr
            collect `(list ',k ',v #'(lambda () ,v)))))

(defun initialize-class-form (var class)
  `(progn
     ;; EARLY-INITIALIZE-INSTANCE takes care of initforms
     ;; and such. This also reduces special casing, hopefully.
     (early-initialize-instance
      ,(name (metaclass class)) ,var
      :name ',(name class)
      :direct-superclasses (list ,@(loop for super
                                           in (mop:class-direct-superclasses class)
                                         for sname = (name super)
                                         collect `(find-class ',sname)))
      :direct-slots (list ,@(loop for slot in (mop:class-direct-slots class)
                                  collect (build-direct-slot-form
                                           slot)))
      ;; since default-initargs is set separately there can be
      ;; duplicate initfunctions, but I do not care.
      :direct-default-initargs ,(canonicalized-default-initargs-form
                                 (direct-default-initargs class)))
     (with-early-accessors (std-class)
       (setf (%class-slots ,var)
             (list ,@(loop for slot in (mop:class-slots class)
                           collect (build-slot-form slot)))
             (%class-precedence-list ,var)
             (list ,@(loop for s in (mop:class-precedence-list class)
                           collect `(find-class ',(name s))))
             (%class-default-initargs ,var)
             ,(canonicalized-default-initargs-form
               (default-initargs class))
             (%class-direct-subclasses ,var)
             (list ,@(loop for sub in (mop:class-direct-subclasses class)
                           for sname = (name sub)
                           collect `(find-class ',sname)))
             (%class-finalized-p ,var) t
             (class-size ,var) ,(length (mop:class-slots class))))
     ,var))

;;; FIXME? These are pretty verbose because make-%leaf-method-function
;;; is defined later in method-function.lisp. We'd have to interleave
;;; it in the with-mutual-defclass form which I think would be a little
;;; tricky so I'll hold off.
(defun reader-mf-form (slot-name)
  `(let ((i (early-make-instance %leaf-method-function
              :fmf (lambda (object)
                     (slot-value object ',slot-name)))))
     (set-funcallable-instance-function
      i
      (lambda (args next)
        (declare (ignore next))
        ;; argcount has been checked by gf already
        (slot-value (first args) ',slot-name)))
     i))
(defun writer-mf-form (slot-name)
  `(let ((i (early-make-instance %leaf-method-function
              :fmf (lambda (value object)
                     (setf (slot-value object ',slot-name) value)))))
     (set-funcallable-instance-function
      i
      (lambda (args next)
        (declare (ignore next))
        ;; argcount has been checked by gf already
        (setf (slot-value (second args) ',slot-name) (first args))))
     i))

;;; Return a list of accessor methods for a defclass form.
(defun build-accessors (class &optional (find-class
                                         #'cross-clasp:find-compiler-class))
  (loop with rll = (list (name class))
        with wll = (list 'new (name class))
        for slot in (mop:class-direct-slots class)
        nconc (loop for reader in (readers slot)
                    for egf = (cross-clasp:gf-info reader)
                    for gf = (or egf
                               (make-instance 'compiler-generic
                                 :name reader :lambda-list rll
                                 :reqargs rll :restp nil
                                 :apo rll
                                 :method-combination
                                 (ensure-method-combination '(standard))
                                 :method-class (funcall find-class 'standard-method)
                                 :declarations ()
                                 :class (funcall find-class 'standard-generic-function)))
                    for fmf = (reader-mf-form (name slot))
                    for method = (make-instance 'compiler-reader
                                   :gf gf :lambda-list rll
                                   :qualifiers () :slot slot
                                   :specializers (list class)
                                   :function-form `(leaf ,fmf)
                                   :class (funcall find-class 'standard-reader-method))
                    collect method)
        nconc (loop for writer in (writers slot)
                    for egf = (cross-clasp:gf-info writer)
                    for gf = (or egf
                               (make-instance 'compiler-generic
                                 :name writer :lambda-list wll
                                 :reqargs wll :restp nil
                                 :apo wll
                                 :method-combination
                                 (ensure-method-combination '(standard))
                                 :method-class (funcall find-class 'standard-method)
                                 :declarations ()
                                 :class (funcall find-class 'standard-generic-function)))
                    for fmf = (writer-mf-form (name slot))
                    for method = (make-instance 'compiler-writer
                                   :gf gf :lambda-list wll
                                   :qualifiers () :slot slot
                                   :specializers (list (funcall find-class 't)
                                                       class)
                                   :function-form `(leaf ,fmf)
                                   :class (funcall find-class 'standard-writer-method))
                    collect method)))

;;; note-generic is defined in environment.lisp
(defun note-method (compiler-generic compiler-method)
  ;; TODO: Sanity checks
  (push compiler-method (methods compiler-generic))
  (update-specializer-profile compiler-generic (specializers compiler-method))
  (values))

(defun build-note-accessors (accessors)
  (loop for method in accessors
        for gf = (gf method)
        unless (cross-clasp:gf-info (name gf))
          collect `(note-generic ',(name gf) ,gf)
        collect `(note-method ,gf ,method)))

(defun build-install-accessors (accessors)
  (loop for method in accessors
        for gf = (gf method)
        for name = (name gf)
        for defgf = (not (cross-clasp:gf-info name))
        for gfv = (gensym "GENERIC-FUNCTION")
        ;; build-gf-form and build-method-form
        ;; are defined in clos-generics.lisp.
        collect `(let ((,gfv ,(if defgf
                                  (build-gf-form gf)
                                  `(fdefinition ',name))))
                   ,@(when defgf
                       `((setf (fdefinition ',name) ,gfv)))
                   (with-early-accessors (standard-generic-function)
                     ,(etypecase method
                        (compiler-reader
                         `(setf (aref (generic-function-specializer-profile ,gfv) 0)
                                t))
                        (compiler-writer
                         `(setf (aref (generic-function-specializer-profile ,gfv) 1)
                                t)))
                     (push ,(build-method-form method)
                           (%generic-function-methods ,gfv))))))

(defun expand-early-defclass (class)
  (let ((name (name class))
        (accessors (build-accessors class)))
    `(progn
       (eval-when (:compile-toplevel)
         (setf (find-class ',name) ,class)
         ,@(build-note-accessors accessors))
       (eval-when (:load-toplevel :execute)
         (let* ((old-class (find-class ',name nil))
                (class
                  (or old-class
                    ,(expand-early-allocate-instance (metaclass class)))))
           ;; Install class.
           ;; we do this first so the CPL can refer to the class.
           (core:setf-find-class class ',name)
           ;; Initialize rack slots.
           ,(initialize-class-form 'class class)
           ;; Install stamp.
           (unless old-class
             (core:class-new-stamp class)))
         ,@(build-install-accessors accessors)
         ',name))))

(defmacro early-defclass (name (&rest supers) (&rest slotds) &rest options)
  ;; ignore redefinitions - some pop up from the generated cxx-classes.lisp
  (if (cross-clasp:find-compiler-class name nil)
      'nil
      (expand-early-defclass (make-compiler-class name supers slotds options))))

;;; Welcome to the deep magic.
;;; This macro allows defmacro forms as its toplevel to refer to each
;;; other kind of like letrec. For example it's okay to have classes
;;; with themselves as a metaclass, or with a metaclass that's defined
;;; later, bla bla bla. This is all needed to describe CLOS.
;;; The basic goal here is for all the base class definitions to be
;;; meaningfully described by simple and clear DEFCLASS forms.
;;; This includes being able to _change_ these definitions in the future,
;;; so the magic does _not_ include that kind of special casing.
;;; Here's what we do:
;;; 1) at compile time: make all compiler classes, then fill them in
;;; 2) at load time: first, make all classes
;;; 3) fill them in
;;; 4) compute sigs so all the class instances are not obsolete.
(defmacro with-mutual-defclass (&body body)
  (loop for form in body
        unless (and (consp form) (eq (car form) 'defclass))
          do (error "Only DEFCLASS forms are allowed here."))
  ;; Before any expansion we set up compiler classes.
  ;; These are dumped as literals, but only into :compile-toplevel code
  ;; so they don't end up in the FASL.
  ;; Note that we refer back to this list rather than doing
  ;; (setf find-class) at macroexpansion time, so that expanding this
  ;; macro does not have or rely on side effects.
  (let* ((compiler-classes
           (loop for (_ name) in body
                 for class = (make-instance 'compiler-class :name name)
                 collect (cons name class)))
         (find-class (lambda (name)
                       (or (cdr (assoc name compiler-classes))
                         (error "No class: ~s" name))))
         accessors)
    ;; initialize the classes
    (loop for (_1 _2 supers slotds . options) in body
          for (name . cclass) in compiler-classes
          do (initialize-compiler-class
              cclass name supers slotds options
              :find-class find-class))
    ;; and finalize their inheritance.
    (loop for (_ . cclass) in compiler-classes
          unless (finalizedp cclass) do (finalize-inheritance cclass))
    ;; Generate accessor methods & generics.
    (setf accessors
          (loop for (_ . cclass) in compiler-classes
                nconc (build-accessors cclass find-class)))
    ;; All compiler classes are done, let's expand.
    `(progn
       (eval-when (:compile-toplevel)
         ,@(loop for (name . class) in compiler-classes
                 collect `(setf (find-class ',name) ',class))
         ,@(build-note-accessors accessors))
       (eval-when (:load-toplevel :execute)
         ;; Here is what the FASL is actually going to do.
         ;; First, make all the classes. We already have all class sizes,
         ;; so this isn't much of a problem.
         ;; Note that the primordial image already has several classes
         ;; defined, such as STANDARD-CLASS. This makes our job here easier.
         ;; With fewer primordial classes, we'd need to do some toposorting
         ;; or something, and special case standard-class.
         ,@(loop for (name . class) in compiler-classes
                 for metaclass = (metaclass class)
                 collect `(unless (find-class ',name nil)
                            (core:class-new-stamp
                             (core:setf-find-class
                              (core:allocate-standard-instance
                               (find-class ',(name metaclass))
                               ,(length (mop:class-slots metaclass)))
                              ',name)
                             ',name)))
         ;; Now we fill in all the classes. We can use the compile-time
         ;; CPLs and effective slots. The standard-direct-slot-definition
         ;; etc classes have already been made so we can make instances of
         ;; them without an issue.
         ,@(loop for (name . class) in compiler-classes
                 for metaclass = (metaclass class)
                 collect `(let ((class (find-class ',name)))
                            ,(initialize-class-form 'class class)))
         ;; Define accessor functions.
         ,@(build-install-accessors accessors)
         ;; Finally, go through the classes setting the sigs of
         ;; their slotds, which did not exist when they were created.
         ;; Also set the sigs of the classes themselves. This is
         ;; necessary because they were created when their classes
         ;; didn't yet have slots. I don't understand how the existing
         ;; bootstrap code does or avoids this.
         (with-early-accessors (std-class)
           ,@(loop for (name . class) in compiler-classes
                   collect `(let ((class (find-class ',name)))
                              (core:instance-sig-set class)
                              (loop for s in (class-slots class)
                                    do (core:instance-sig-set s))
                              (loop for s in (class-direct-slots class)
                                    do (core:instance-sig-set s)))))))))
