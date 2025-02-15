(in-package #:clasp-analyzer)

(defparameter *log-path* nil)

(defun ensure-list (x)
  (unless (listp x)
    (error "The argument ~a must be a list" x))
  x)

(defparameter *errors* nil
  "Keep track of errors discovered during analysis")

(defmacro analysis-error (fmt &rest body)
  `(push (format nil ,fmt ,@body) *errors*))

;;; --------------------------------------------------
;;; --------------------------------------------------
;;; Should not need to modify below here
;;; --------------------------------------------------
;;; --------------------------------------------------
;;;#+(or)
(defmacro gclog (fmt &rest args)
  (declare (ignore fmt args)))
#+(or)
(progn
  (format t "Turning on gclog~%")
  (defmacro gclog (fmt &rest args) `(format *debug-io* ,fmt ,@args)))

(defun c++identifier (str)
  "* Arguments
- str :: A string designator.
* Description 
Convert the string into a C++ identifier, convert non-alphanumeric characters to underscores."
  (substitute-if #\_ (complement #'alphanumericp) (string str)))

;; ----------------------------------------------------------------------
;;
;; Housekeeping structs
;;
;; ----------------------------------------------------------------------

(define-condition unsupported-type (error)
  ((type :initarg :type :accessor unsupported-type)))

;; ----------------------------------------------------------------------
;;
;; Class structs
;;
;; ----------------------------------------------------------------------

(defstruct cclass
  "Store info on a class "
  key
  template-specializer
  definition-data ; Store info about class extracted from class definition like isPolymorphic (list of symbols)
  lisp-base
  bases
  vbases
  fields)

(defstruct instance-field
  access
  ctype)

(defstruct (instance-variable (:include instance-field))
  field-name)

(defstruct (instance-array-element (:include instance-field))
  index)

(defgeneric instance-field-as-string (x first)
  (:documentation "Return a string that describes this instance-field"))

(defmethod instance-field-as-string ((x instance-variable) first)
  (format nil "~a~a"
          (if first "" ".")
          (instance-variable-field-name x)))

(defmethod instance-field-as-string ((x instance-array-element) first)
  (declare (ignore first))
  (format nil "[~a]" (instance-array-element-index x)))

(defstruct alloc
  key
  name ;; decl name
  ctype)

;; Abstract allocations for abstract or template parent classes
(defstruct (abstractalloc (:include alloc)))

(defstruct (lispalloc (:include alloc)))

(defstruct (classalloc (:include alloc)))

(defstruct (rootclassalloc (:include alloc)))

(defstruct (containeralloc (:include alloc)))

(defclass class-layout ()
  ((layout-class :initarg :layout-class :accessor layout-class)
   (fixed-part :initarg :fixed-part :accessor fixed-part)
   (variable-part :initarg :variable-part :accessor variable-part)))

(defstruct stamp
  key
  value%
  wtag
  root-cclass
  cclass
  species ;; can be nil
  ;; only generate TaggedCast entry for those in hierarchy
  in-hierarchy)

(defstruct (simple-stamp (:include stamp))
  alloc)

(defstruct (templated-stamp (:include stamp))
  all-allocs)

;; ----------------------------------------------------------------------
;;
;; Project and analysis classes
;;
;; ----------------------------------------------------------------------

(defstruct project
  "Store the results of matching to the entire codebase"
  ;; All class information. The size is enough to include Cando's classes.
  (classes (make-hash-table :test #'equal :size 500000))
  ;; Different allocs of objects(classes)
  (lispallocs (make-hash-table :test #'equal))   ; exposed to Lisp
  (classallocs (make-hash-table :test #'equal)) ; regular classes
  (rootclassallocs (make-hash-table :test #'equal)) ; regular root classes
  (containerallocs (make-hash-table :test #'equal))) ; containers

(defun hash-table-union (a b)
  (loop for key being the hash-keys of b
          using (hash-value value)
        unless (gethash key a)
          do (setf (gethash key a) value)))

(defun merge-projects (union one)
  (hash-table-union (project-classes union) (project-classes one))
  (hash-table-union (project-lispallocs union) (project-lispallocs one))
  (hash-table-union (project-classallocs union) (project-classallocs one))
  (hash-table-union (project-rootclassallocs union) (project-rootclassallocs one))
  (hash-table-union (project-containerallocs union) (project-containerallocs one)))

(defun summarize-project (project)
  (format t "project-classes ~a~%" (project-classes project)))

(defclass stamp-value-generator ()
  ((unused-builtin-stamps :initform (let ((ht (make-hash-table :test #'equal)))
                                      (loop for (name . stamp) in (gctools:get-stamp-name-map)
                                            do (setf (gethash name ht) stamp))
                                      ht)
                          :accessor unused-builtin-stamps)
   (used-builtin-stamps :initform (make-hash-table :test #'equal)
                        :accessor used-builtin-stamps)
   (next-stamp-value :initform 0
                     :accessor next-stamp-value)))

(defstruct analysis
  project
  manager
  inline
  (stamp-value-generator (make-instance 'stamp-value-generator))
  (forwards (make-hash-table :test #'equal))
  stamp-value-problems
  (stamps (make-hash-table :test #'equal))
  stamp-children
  sorted-stamps
  stamp-roots)

(defun reset-stamp-value-generator (analysis)
  (setf (analysis-stamp-value-generator analysis) (make-instance 'stamp-value-generator)))

(defparameter *wtag-shift*
  cmp::+wtag-width+)

(defparameter *derivable-wtag*
  (ash cmp::+derivable-wtag+ (- cmp::+general-mtag-width+)))

(defparameter *rack-wtag*
  (ash cmp::+rack-wtag+ (- cmp::+general-mtag-width+)))

(defparameter *wrapped-wtag*
  (ash cmp::+wrapped-wtag+ (- cmp::+general-mtag-width+)))

(defparameter *header-wtag*
  (ash cmp::+header-wtag+ (- cmp::+general-mtag-width+)))

(defparameter *max-wtag*
  (ash cmp::+max-wtag+ (- cmp::+general-mtag-width+)))

(defparameter +rack-classes+
  '("core::Instance_O" "core::FuncallableInstance_O" "clbind::ClassRep_O"))

(defparameter +wrapped-classes+
  '("core::WrappedPointer_O"))

(defparameter +derivable-classes+
  '("core::DerivableCxxObject_O"))

(defmethod calculate-stamp-where-unshifted (stamp-value cclass)
  "Shift the stamp-value by *wtag-shift* and or in the where tag based
on the cclass. 
This could change the value of stamps for specific classes - but that would break quick typechecks like (typeq x Number)"
  (cond ((member (cclass-key cclass) +rack-classes+ :test #'string=)
         (values (logior (ash stamp-value *wtag-shift*) *rack-wtag*) *rack-wtag*))
        ((member (cclass-key cclass) +wrapped-classes+ :test #'string=)
         (values (logior (ash stamp-value *wtag-shift*) *wrapped-wtag*) *wrapped-wtag*))
        ((member (cclass-key cclass) +derivable-classes+ :test #'string=)
         (values (logior (ash stamp-value *wtag-shift*) *derivable-wtag*) *derivable-wtag*))
        (t
         (values (logior (ash stamp-value *wtag-shift*) *header-wtag*) *header-wtag*))))

(defun assign-stamp-value (analysis stamp operation)
  (let ((stamp-value (stamp-value% stamp))
        (generator (analysis-stamp-value-generator analysis))
        (stamp-name (get-stamp-name stamp)))
    (if stamp-value
        (push (format nil "Attempted to assign stamp to class ~a due to ~a but it already has one due to inheritance from ~a"
                      (stamp-key stamp)
                      operation
                      (stamp-root-cclass stamp))
              (analysis-stamp-value-problems analysis))
        (let ((scraper-stamp (gethash stamp-name (unused-builtin-stamps generator))))
          (when scraper-stamp
            (remhash stamp-name (unused-builtin-stamps generator))
            (setf (gethash stamp-name (used-builtin-stamps generator)) scraper-stamp))
          (setf stamp-value (incf (next-stamp-value generator)))))
    (if (integerp stamp-value)
        (calculate-stamp-where-unshifted stamp-value (stamp-cclass stamp))
        stamp-value)))

(defmethod print-object ((object analysis) stream)
  (format stream "#<analysis>"))

;; Return "inline" if the function should be inlined
;; For now it's really simple, inline if it's in a list and don't if it's not
(defun inline-analysis (key analysis)
  (if (member key (analysis-inline analysis) :test #'string=)
    "ALWAYS_INLINE"
    "MAYBE_INLINE"))

;; ----------------------------------------------------------------------
;;
;; Class hierarchy
;;
;; ----------------------------------------------------------------------

(defun notify-base-names (class-name base-names analysis)
  (when (> (length base-names) 1)
    (format t "!!!WARNING: Class ~a is probably a derivable_class- and so it has multiple bases: ~s - clasp has to special case this~%" class-name base-names)
    (dolist (try-remove '("core::Instance_O" "core::General_O"))
      (let ((found-instance (find try-remove base-names :test #'string=)))
        (when found-instance
          (setf base-names (remove found-instance base-names :test #'string=))
          (format t "       Removed ~s from base-names -> ~s~%" found-instance base-names)))))
  (dolist (class-base-name base-names)
    (multiple-value-bind (parent-stamp parent-stamp-p)
        (gethash class-base-name (analysis-stamps analysis))
      (when parent-stamp-p
        (push class-name (gethash parent-stamp (analysis-stamp-children analysis)))))))
  
(defun notify-parents (class-name analysis)
  (let* ((project (analysis-project analysis))
         (class (gethash class-name (project-classes project)))
         (base-names (append (cclass-bases class) (cclass-vbases class))))
    (if (and base-names (not (string= class-name "core::T_O")))
        (notify-base-names class-name base-names analysis)
        (progn
          (format t "Adding as a root - class-name ~a~%" class-name)
          (format t "       It's base-names -> ~a~%" base-names)
          (push class-name (analysis-stamp-roots analysis))))))

(defun build-hierarchy (analysis)
  (maphash #'(lambda (node-name stamp)
               (declare (ignore stamp))
               (notify-parents node-name analysis))
           (analysis-stamps analysis)))

(defun no-stamp-value (analysis stamp operation)
  (declare (ignore analysis stamp operation))
  "Some classes don't get a stamp because they inherit it from their base."
  :no-stamp-value)

(defun traverse-inheritance-tree (name analysis operation root-cclass assign-stamp-value-function)
  (unless  (member assign-stamp-value-function '(assign-stamp-value no-stamp-value))
    (error "bad assign-stamp-function"))
  (let* ((stamp (let ((stamp (gethash name (analysis-stamps analysis))))
                  (unless stamp
                    (error "Could not find stamp for ~a" name))
                  stamp)))
    (multiple-value-bind (stamp-value stamp-wtag)
        (funcall assign-stamp-value-function analysis stamp operation)
      (setf (stamp-value% stamp) stamp-value)
      (setf (stamp-wtag stamp) stamp-wtag)
      (setf (stamp-root-cclass stamp) root-cclass)
      (setf (stamp-in-hierarchy stamp) t)
      (dolist (child (gethash stamp (analysis-stamp-children analysis)))
        (traverse-inheritance-tree child analysis operation root-cclass assign-stamp-value-function)))))

(defun children-of-class-named (name analysis)
  (let ((stamp (gethash name (analysis-stamps analysis))))
    (gethash stamp (analysis-stamp-children analysis))))

(defun initialize-stamp-values (analysis)
  (setf (analysis-stamp-children analysis) (make-hash-table))
  (setf (analysis-stamp-roots analysis) nil)
  (build-hierarchy analysis)
  (maphash (lambda (k v) (declare (ignore k)) (setf (stamp-value% v) nil))
           (analysis-stamps analysis))
  (reset-stamp-value-generator analysis))

(defun assign-stamp-values-to-hierarchy (analysis)
  (initialize-stamp-values analysis)
  ;; classes that inherit from core::DerivableCxxObject_O will not get their own stamp value - they will get
  ;;   the stamp value of core::DerivableCxxObject_O and virtual functions will be invoked to get the extended stamp
  ;;   the Instance_O offset and the size of the object

  (let ((derivable-cxx-object-stamp (gethash "core::DerivableCxxObject_O" (analysis-stamps analysis))))
    (if derivable-cxx-object-stamp
        (let ((children (gethash derivable-cxx-object-stamp (analysis-stamp-children analysis))))
          (dolist (child children)
            (traverse-inheritance-tree child analysis "inherit from core::DerivableCxxObject_O" "core::DerivableCxxObject_O"
                                       'no-stamp-value)))
        (warn "For a full run - there must be a class named core::DerivableCxxObject_O - why is it missing!!!!!")))
  (traverse-inheritance-tree "core::T_O" analysis "inherit from core::T_O" "core::T_O" 'assign-stamp-value)
  (dolist (root (remove "core::T_O" (analysis-stamp-roots analysis) :test #'string=))
    (traverse-inheritance-tree root analysis (format nil "inherit from ~a" root) root 'assign-stamp-value)))

(defun assign-stamp-values-to-those-without (analysis)
  (maphash (lambda (name stamp)
             (declare (ignore name))
             (when (eq (stamp-value% stamp) :unassigned)
               (let ((stamp-value (assign-stamp-value analysis stamp)))
                 (setf (stamp-value% stamp) stamp-value
                       (stamp-in-hierarchy stamp) nil))))
           (analysis-stamps analysis)))

(defun analyze-hierarchy (analysis)
  (assign-stamp-values-to-hierarchy analysis)
  (assign-stamp-values-to-those-without analysis))

;; ----------------------------------------------------------------------
;;
;; Clang type classes
;;
;; ----------------------------------------------------------------------

(defstruct gc-template-argument 
  index
  ctype
  integral-value)

;; A ctype is holds the name of a C++ type
(defstruct ctype key)

(defstruct (bitunit-ctype (:include ctype))
  bitunit-width
  unsigned-type
  signed-type)

(defstruct (simple-ctype (:include ctype)))

(defstruct (basic-string-ctype (:include ctype))
  name)

(defstruct (std-map-ctype (:include ctype))
  name)

(defstruct (shared-mutex-ctype (:include ctype))
  name)

(defstruct (mutex-ctype (:include ctype))
  name)

(defstruct (function-proto-ctype (:include ctype)))

(defstruct (lvalue-reference-ctype (:include ctype)))

(defstruct (template-type-parm-ctype (:include ctype)))

(defstruct (rvalue-reference-ctype (:include ctype)))

(defstruct (dependent-name-ctype (:include ctype)))

(defstruct (enum-ctype (:include ctype)))

(defstruct (builtin-ctype (:include ctype)))

(defstruct (unclassified-ctype (:include simple-ctype)))

(defstruct (uninteresting-ctype (:include simple-ctype)))

(defstruct (unknown-ctype (:include simple-ctype)))

(defstruct (template-specialization-ctype (:include simple-ctype)))

(defstruct (unclassified-template-specialization-ctype (:include template-specialization-ctype))
  description
  template-name)

(defstruct (record-ctype (:include ctype))
  name)

(defstruct (smart-ptr-ctype (:include ctype))
  specializer)

(defstruct (tagged-pointer-ctype (:include ctype))
  specializer)

(defstruct (weak-ptr-ctype (:include ctype)))
(defstruct (ephemeron-ctype (:include ctype)))

(defstruct (pointer-ctype (:include ctype))
  pointee)

(defstruct (constant-array-ctype (:include ctype))
  array-size
  element-type)

(defstruct (incomplete-array-ctype (:include ctype))
  element-type)

(defstruct (cxxrecord-ctype (:include record-ctype)))

(defstruct (class-template-specialization-ctype (:include record-ctype))
  arguments)

(defstruct (injected-class-name-ctype (:include record-ctype)))

(defstruct (pointer-to-record-ctype (:include ctype))
  description)

(defstruct (unique-ptr-ctype (:include ctype))
  name arguments)

(defstruct (atomic-ctype (:include ctype))
  name argument)

(defstruct (dont-expose-ctype (:include ctype))
  name
  argument)

(defstruct (dont-analyze-ctype (:include ctype))
  name
  argument)

(defstruct (container (:include class-template-specialization-ctype)))

(defstruct (gcvector-moveable-ctype (:include container)))

(defstruct (gcarray-moveable-ctype (:include container)))

(defstruct (gcbitunitarray-moveable-ctype (:include container)))

;; //////////////////////////////////////////////////////////////////////
;;
;; Compile linear layouts of classes
;;
;; //////////////////////////////////////////////////////////////////////

(defun linearize-code-for-field (field base analysis)
  (let ((code (ensure-list (linearize-class-layout-impl field base analysis))))
    (unless (listp code)
      (error "The result of linearize-class-layout-impl MUST be a LIST"))
    (mapc (lambda (onecode) (push-prefix-field field onecode)) code)
    code))

(defun fix-code-for-field (field analysis)
  (let ((code (fixable-instance-variables-impl field analysis)))
    (cond ((null code)
           nil)
          ((atom code)
           (list (list field code)))
          ((listp code)
           (mapcar (lambda (f) (cons field f)) code))
          (t
           (error "Don't know what to do with ~a in fix-code-for-field." code)))))

(defclass offset ()
  ((offset-type :initarg :offset-type
                :accessor offset-type)
   (fields :initform nil
           :initarg :fields
           :accessor fields)
   (base :initarg :base
         :accessor base)))

(defmethod print-object ((x offset) stream)
  (format stream "#<~a :fields ~a :offset-type ~a :base ~a>"
          (class-name (class-of x)) (fields x) (offset-type x) (base x)))

(defclass copyable-offset (offset)
  ())

(defclass atomic-smart-ptr-offset (copyable-offset)
  ())

(defclass atomic-pod-offset (copyable-offset)
  ())

(defclass dont-expose-offset (copyable-offset)
  ())

(defclass dont-analyze-offset (copyable-offset)
  ())

(defclass smart-ptr-offset (copyable-offset)
  ())

(defclass tagged-pointer-offset (copyable-offset)
  ())

(defclass weak-ptr-offset (copyable-offset)
  ())
(defclass ephemeron-offset (copyable-offset)
  ())

(defclass pointer-offset (copyable-offset)
  ())

(defclass raw-pointer-offset (copyable-offset)
  ())

(defclass pod-offset (copyable-offset)
  ())

(defclass cxx-fixup-offset (copyable-offset)
  ())

(defclass cxx-shared-mutex-offset (copyable-offset)
  ())

(defclass cxx-mutex-offset (copyable-offset)
  ())

(defun copy-offset (offset)
  "* Arguments
- offset :: A copyable-offset.
* Make a duplicate of the offset."
  (make-instance (class-of offset)
                 :offset-type (offset-type offset)
                 :fields (fields offset)
                 :base (base offset)))

(defmethod offset-type-c++-identifier ((x pod-offset))
  (let ((type (offset-type x)))
    (format nil "ctype_~a" (c++identifier (ctype-key type)))))

(defclass array-offset (offset)
  ((element-type :initarg :element-type
                 :accessor element-type)
   (elements :initarg :elements
             :accessor elements)))

(defclass constant-array-offset (array-offset)
  ((constant-array-size :initarg :constant-array-size
                        :accessor constant-array-size)))

(defclass container-offset (offset)
  ((fixed-fields :initarg :fixed-fields
                 :accessor fixed-fields)
   (elements-base :initarg :elements-base
                  :accessor elements-base)
   (elements :initarg :elements
             :accessor elements)))

(defclass gcarray-offset (container-offset)
  ())

(defclass gcbitunitarray-offset (container-offset)
  ())

(defclass gcvector-offset (container-offset)
  ())

(defmethod offset-type-c++-identifier ((x offset))
  (c++identifier (class-name (class-of x))))

(defmethod offset-type-c++-identifier ((x atomic-pod-offset))
  (c++identifier (concatenate 'string (string (class-name (class-of x)))
                              (format nil "_~a" (ctype-key (offset-type x))))))

(defmethod offset-ctype ((x offset))
  (ctype-key (offset-type x)))

(defmethod offset-base-ctype ((x offset))
  (cclass-key (base x)))

(defun layout-offset-field-names (off &key drop-last)
  "* Arguments
- off :: An offset.
- drop-last :: If the variable is atomic then leave off the last field
* Description
Generate a list of strings that represent nested field names for the offset."
  (loop for field in (fields off)
        for index below (if drop-last
                            (1- (length (fields off)))
                            (length (fields off)))
        collect (instance-field-as-string field (= index 0))))

(defmethod push-prefix-field (field (the-offset offset))
  (unless (typep field 'instance-field)
    (error "field can only be instance-variable got: ~a" field))
  (push field (fields the-offset)))

(defmethod push-prefix-field (field (the-offset container-offset))
  (unless (typep field 'instance-field)
    (error "field can only be instance-variable got: ~a" field))
  (call-next-method)
  (mapc (lambda (os) (push field (fields os))) (fixed-fields the-offset)))

(defun offset-field-with-name (fields name)
  (car (loop for x in fields
          when (string= (instance-variable-field-name (car (last (fields x)))) name)
          collect x)))

(defun variable-part-offset-field-names (fields)
  (loop for x in fields
     collect (instance-variable-field-name (car (last (fields x))))))

(defun maybe-fixup-type (type container-type)
  (if type
      type
      (cond
        ((string= container-type "gctools::GCVector_moveable<core::T_O *>")
         "core::T_O*")
        (t (warn "I don't know how to fixup type ~a in ~a" type container-type)
           type))))

(defun is-bad-special-case-variable-name (name-list)
  "* Arguments
- name-list :: A list of strings.
* Description
Return T if the name-list contains special-case variable names that we don't want
to expose."
  (cond
    ((find "NO-NAME" name-list :test #'string=) t)
    ((find "_LocationDependency" name-list :test #'string=) t)
    (t nil)))

(defun find-gcbitunit-array-moveable-ctype (offset)
  (loop for field in (fields offset)
        for field-ctype = (instance-variable-ctype field)
        when (typep field-ctype 'gcbitunitarray-moveable-ctype)
          return field-ctype))

(defun starts-with (seq start)
  (and (>= (length seq) (length start))
       (string= seq start :end1 (length start))))

(defun tags-for-variable-part (variable-fields analysis)
  (let* ((array (offset-field-with-name variable-fields "_Data"))
         (length (or (offset-field-with-name variable-fields "_Length")
                     (offset-field-with-name variable-fields "_MaybeSignedLength")
                     (offset-field-with-name variable-fields "_Capacity")))
         (end (or (offset-field-with-name variable-fields "_End") length))
         (gcbitunit-ctype (find-gcbitunit-array-moveable-ctype array)))
    (unless length
      (error "Could not find _Length or _MaybeSignedLength in the variable-fields: ~a with names: ~a of ~a"
             variable-fields (variable-part-offset-field-names variable-fields)
             (mapcar (lambda (x) (offset-type-c++-identifier x))
                     variable-fields)))
    (list* (if gcbitunit-ctype
                 (make-instance 'tags:variable-bit-array0
                                :integral-value (gc-template-argument-integral-value (find 0 (class-template-specialization-ctype-arguments gcbitunit-ctype)
                                                                                           :test #'eql :key #'gc-template-argument-index))
                                :offset-base-ctype (offset-base-ctype array)
                                :field-names (layout-offset-field-names array))
                 (make-instance 'tags:variable-array0
                                :offset-base-ctype (offset-base-ctype array)
                                :field-names (layout-offset-field-names array)))
           (make-instance 'tags:variable-capacity
                          :ctype (maybe-fixup-type (ctype-key (element-type array))
                                                   (offset-base-ctype array))
                          :offset-base-ctype (offset-base-ctype array)
                          :end-field-names (layout-offset-field-names end)
                          :length-field-names (layout-offset-field-names length))
           (loop for one in (elements array)
                 for field-names = (layout-offset-field-names one)
                 for ctype-key = (ctype-key (base one))
                 if (starts-with ctype-key "std::atomic<gctools::smart_ptr<")
                   collect (make-instance 'tags:variable-field-only
                                          :offset-type-cxx-identifier (offset-type-c++-identifier one)
                                          :fixup-type (maybe-fixup-type (ctype-key (element-type array))
                                                                        (offset-base-ctype array)))
                 else unless field-names
                   collect (make-instance 'tags:variable-field-only
                                          :offset-type-cxx-identifier (offset-type-c++-identifier one)
                                          :fixup-type (maybe-fixup-type (ctype-key (element-type array))
                                                                        (offset-base-ctype array)))
                 else unless (is-bad-special-case-variable-name (layout-offset-field-names one))
                   collect (make-instance 'tags:variable-field
                                          :offset-type-cxx-identifier (offset-type-c++-identifier one)
                                          :fixup-ctype-offset-type-key (maybe-fixup-type (ctype-key (offset-type one))
                                                                                         (ctype-key (base one)))
                                          :fixup-ctype-key (ctype-key (base one))
                                          :layout-offset-field-names (layout-offset-field-names one :drop-last (is-atomic one)))))))

(defun is-atomic (one)
  "Return T if the field in ONE is a std::atomic<Foo> type."
  (let* ((second-last-field (car (last (fields one) 2)))
         (second-last-field-type (instance-field-ctype second-last-field))
         (name (and (class-template-specialization-ctype-p second-last-field-type)
                    (class-template-specialization-ctype-name second-last-field-type))))
    (and name (string= "atomic" name))))

(defun tags-for-full (layout analysis)
  (nconc (loop for one in (fixed-part layout)
               for fixable = (fixable-instance-variables (car (last (fields one))) analysis)
               for public = (mapcar (lambda (iv) (eq (instance-field-access iv) 'clang-ast:as-public)) (fields one))
               for is-std-atomic = (is-atomic one)
               for good-name = (not (is-bad-special-case-variable-name (layout-offset-field-names one)))
               when good-name
                 collect (make-instance 'tags:fixed-field
                                        :offset-type-cxx-identifier (offset-type-c++-identifier one)
                                        :offset-ctype (or (offset-ctype one) "UnknownType")
                                        :offset-base-ctype (offset-base-ctype one)
                                        :layout-offset-field-names (layout-offset-field-names one :drop-last is-std-atomic)))
         (let ((variable-part (variable-part layout)))
           (when variable-part
             (tags-for-variable-part (fixed-fields variable-part) analysis)))))

(defun definition-data-as-string (definition-data)
  "Convert (:foo :bar) -> FOO | BAR"
  (format nil "~:[0~;~:*~{~a~^ | ~}~]" (mapcar #'c++identifier definition-data)))

(defun tags-for-lisp-layout (stamp key layout definition-data analysis)
  (list* (make-instance 'tags:class-kind
                        :stamp-name (get-stamp-name stamp)
                        :stamp-key key
                        :parent-class (first (cclass-bases (stamp-cclass stamp)))
                        :lisp-class-base (cclass-lisp-base (stamp-cclass stamp))
                        :root-class (stamp-root-cclass stamp)
                        :stamp-wtag (stamp-wtag stamp)
                        :definition-data (definition-data-as-string definition-data))
         (tags-for-full layout analysis)))

(defun tags-for-container-layout (stamp key layout definition-data analysis)
  (list* (make-instance 'tags:container-kind
                        :stamp-name (get-stamp-name stamp)
                        :stamp-key key
                        :parent-class (first (cclass-bases (stamp-cclass stamp)))
                        :lisp-class-base (cclass-lisp-base (stamp-cclass stamp))
                        :root-class (stamp-root-cclass stamp)
                        :stamp-wtag (stamp-wtag stamp)
                        :definition-data (definition-data-as-string definition-data))
         (tags-for-variable-part (fixed-part layout) analysis)))

(defun tags-for-bitunit-container-layout (stamp key layout definition-data analysis)
  (list* (make-instance 'tags:bitunit-container-kind
                        :stamp-name (get-stamp-name stamp)
                        :stamp-key key
                        :parent-class (first (cclass-bases (stamp-cclass stamp)))
                        :lisp-class-base (cclass-lisp-base (stamp-cclass stamp))
                        :root-class (stamp-root-cclass stamp)
                        :stamp-wtag (stamp-wtag stamp)
                        :bitwidth (species-bitwidth (stamp-species stamp))
                        :definition-data (definition-data-as-string definition-data))
         ;; There is no fixed part for bitunits
         (tags-for-variable-part (fixed-part layout) analysis)))

(defun tags-for-templated-layout (stamp key layout definition-data analysis)
  (list* (make-instance 'tags:templated-kind
                        :stamp-name (get-stamp-name stamp)
                        :stamp-key key
                        :parent-class (first (cclass-bases (stamp-cclass stamp)))
                        :lisp-class-base (cclass-lisp-base (stamp-cclass stamp))
                        :root-class (stamp-root-cclass stamp)
                        :stamp-wtag (stamp-wtag stamp)
                        :definition-data (definition-data-as-string definition-data))
         (tags-for-full layout analysis)))

(defgeneric linearize-class-layout-impl (x base analysis)
  (:documentation "* Arguments
- x :: A ctype, cclass or an instance variable.
- base :: The class we are linearizing (NOT SURE!).
- analysis :: An analysis
* Description
Recursively search X and return a LIST of OFFSET objects that represent instance variable offsets that we want
to expose to C++.
"))

(defmethod linearize-class-layout-impl ((x instance-variable) base analysis)
  (let ((offset (linearize-class-layout-impl (instance-field-ctype x) base analysis)))
    offset))

(defmethod linearize-class-layout-impl ((x gcarray-moveable-ctype) base analysis)
  (let ((nodes (call-next-method)))
    (let* ((arguments (gcarray-moveable-ctype-arguments x))
           (arg0 (find 0 arguments :test #'eql :key #'gc-template-argument-index))
           (arg0-ctype (gc-template-argument-ctype arg0)))
      (list (make-instance 'gcarray-offset
                           :base base
                           :fixed-fields nodes
                           :offset-type arg0-ctype
                           :elements-base arg0-ctype
                           :elements (ensure-list (linearize-class-layout-impl arg0-ctype arg0-ctype analysis)))))))

(defmethod linearize-class-layout-impl ((x gcbitunitarray-moveable-ctype) base analysis)
  (declare (ignore analysis))
  (let ((nodes (call-next-method)))
    (format t "linearize-class-layout-impl for gcbitunitarray-moveable-ctype  nodes -> ~a~%" nodes)
    (let* ((arguments (gcbitunitarray-moveable-ctype-arguments x))
           (_ (format t "gccbitunitarray-moveable-ctype-arguments: ~s~%" arguments))
           (arg0 (find 0 arguments :test #'eql :key #'gc-template-argument-index))
           (arg0-integral-value (gc-template-argument-integral-value arg0))
           (arg1 (find 1 arguments :test #'eql :key #'gc-template-argument-index))
           (arg1-ctype (gc-template-argument-ctype arg1))
           #+(or)(arg2 (find 2 arguments :test #'eql :key #'gc-template-argument-index))
           #+(or)(arg2-ctype (gc-template-argument-ctype arg2))
           )
      (declare (ignore _))
      (unless (member arg0-integral-value '("1" "2" "4") :test #'string=)
        (error "The argument ~s, which describes the bit width of a bitunit, must be a positive integer 1,2, or 4 - it is not" arg0-integral-value))
      (format t "bitunit arg0-integral-value -> ~s arg1-ctype -> ~s~%" arg0-integral-value arg1-ctype)
      (list (make-instance 'gcbitunitarray-offset
                           :base base
                           :fixed-fields nodes
                           :offset-type nil
                           :elements-base nil
                           :elements (make-bitunit-ctype :bitunit-width (parse-integer arg0-integral-value)
                                                         :unsigned-type arg1-ctype
                                                         #|:signed-type arg2-ctype|#))))))

(defmethod linearize-class-layout-impl ((x cclass) base analysis)
  (let* ((project (analysis-project analysis))
         (base-code (let (all-offsets)
                       (dolist (base-name (cclass-bases x))
                         (let ((base-fixers (ensure-list (linearize-class-layout-impl (gethash base-name (project-classes project)) base analysis))))
                           (when base-fixers
                             (setq all-offsets (append base-fixers all-offsets)))))
                       all-offsets))
         (vbase-code (let (all-offsets)
                       (dolist (vbase-name (cclass-vbases x))
                         (let ((vbase-fixers (ensure-list (linearize-class-layout-impl (gethash vbase-name (project-classes project)) base analysis))))
                           (when vbase-fixers
                             (setq all-offsets (append vbase-fixers all-offsets)))))
                       all-offsets))
         (field-code (let (all-offsets)
                       (dolist (field (cclass-fields x))
                         (let ((field-fixers (linearize-code-for-field field base analysis)))
                           (when field-fixers
                             (setq all-offsets (append field-fixers all-offsets)))))
                       all-offsets))
         result)
    (when base-code (setq result (append result base-code)))
    (when vbase-code (setq result (append result vbase-code)))
    (when field-code (setq result (append result field-code)))
    result))


(defmethod linearize-class-layout-impl ((x unclassified-ctype) base analysis)
  (declare (ignore analysis))
  (if (ignorable-ctype-p x)
      nil
      (list (make-instance 'pod-offset
                           :base base
                           :offset-type x))))

(defmethod linearize-class-layout-impl ((x lvalue-reference-ctype) base analysis)
  (declare (ignore base analysis))
  nil)

(defmethod linearize-class-layout-impl ((x enum-ctype) base analysis)
  (declare (ignore base analysis))
  nil)
(defmethod linearize-class-layout-impl ((x builtin-ctype) base analysis)
  (declare (ignore analysis))
  (if (ignorable-ctype-p x)
      nil
      (list (make-instance 'pod-offset
                           :base base
                           :offset-type x))))

(defmethod linearize-class-layout-impl ((x atomic-ctype) base analysis)
  (declare (ignore analysis))
  (cond
    ((or (smart-ptr-ctype-p (atomic-ctype-argument x))
         (tagged-pointer-ctype-p (atomic-ctype-argument x)))
     (list (make-instance 'atomic-smart-ptr-offset
                         :base base
                          :offset-type (atomic-ctype-argument x))))
    (t (list (make-instance 'atomic-pod-offset
                            :base base
                            :offset-type (atomic-ctype-argument x))))))

(defmethod linearize-class-layout-impl ((x basic-string-ctype) base analysis)
  (declare (ignore analysis))
  (if (ignorable-ctype-p x)
      nil
      (list (make-instance 'cxx-fixup-offset
                           :base base
                           :offset-type x))))

(defmethod linearize-class-layout-impl ((x std-map-ctype) base analysis)
  (declare (ignore analysis))
  (list (make-instance 'cxx-fixup-offset
                       :base base
                       :offset-type x)))

(defmethod linearize-class-layout-impl ((x shared-mutex-ctype) base analysis)
  (declare (ignore analysis))
  (list (make-instance 'cxx-shared-mutex-offset
                       :base base
                       :offset-type x)))

(defmethod linearize-class-layout-impl ((x mutex-ctype) base analysis)
  (declare (ignorable base) (ignore analysis))
  nil
  #+(or)(list (make-instance 'cxx-mutex-offset
                       :base base
                       :offset-type x)))

(defmethod linearize-class-layout-impl ((x unique-ptr-ctype) base analysis)
  (declare (ignore analysis))
  (if (ignorable-ctype-p x)
      nil
      (list (make-instance 'cxx-fixup-offset
                           :base base
                           :offset-type x))))

(defmethod linearize-class-layout-impl ((x dont-expose-ctype) base analysis)
  (declare (ignorable base) (ignore analysis))
  nil) ;;; (list (make-instance 'dont-expose-offset :base base :offset-type (gc-template-argument-ctype (first (dont-expose-ctype-argument x))))))

(defmethod linearize-class-layout-impl ((x dont-analyze-ctype) base analysis)
  (declare (ignore analysis))
  (list (make-instance 'dont-analyze-offset :base base :offset-type (gc-template-argument-ctype (first (dont-analyze-ctype-argument x))))))

(defmethod linearize-class-layout-impl ((x cxxrecord-ctype) base analysis)
  (let ((code (gethash (cxxrecord-ctype-key x) (project-classes (analysis-project analysis)))))
    (unless code
      (error "Could not find ~a in (project-classes (analysis-project analysis))" (cxxrecord-ctype-key x)))
    (linearize-class-layout-impl code base analysis)))

(defmethod linearize-class-layout-impl ((x class-template-specialization-ctype) base analysis)
  (cond
    ((string= (ctype-key x) "unsigned long") nil)
    (t
     (linearize-class-layout-impl (gethash (ctype-key x) (project-classes (analysis-project analysis))) base analysis))))

(defmethod linearize-class-layout-impl ((x smart-ptr-ctype) base analysis)
  (declare (ignore analysis))
  (list (make-instance 'smart-ptr-offset :base base :offset-type x)))

(defmethod linearize-class-layout-impl ((x tagged-pointer-ctype) base analysis)
  (declare (ignore analysis))
  (list (make-instance 'tagged-pointer-offset :base base :offset-type x)))

(defmethod linearize-class-layout-impl ((x weak-ptr-ctype) base analysis)
  (declare (ignore analysis))
  (list (make-instance 'weak-ptr-offset :base base :offset-type x)))
(defmethod linearize-class-layout-impl ((x ephemeron-ctype) base analysis)
  (declare (ignore analysis))
  (list (make-instance 'ephemeron-offset :base base :offset-type x)))

(defun linearize-constant-array-contents (array element-type elements)
  "* Arguments
- array :: A constant-array-ctype.
- element-type :: A ctype.
- elements :: A list of the fields to expose in element-type.
* Description
Generate offsets for every array element that exposes the fields in elements."
  (let ((number-of-elements (constant-array-ctype-array-size array)))
    (loop :for index :below number-of-elements
       :append (loop :for element :in elements
                  :collect (let ((element-copy (copy-offset element)))
                             (push-prefix-field (make-instance-array-element
                                                               :index index
                                                               :ctype element-type)
                                                element-copy)
                             element-copy)))))

(defmethod linearize-class-layout-impl ((x constant-array-ctype) base analysis)
  (let* ((element-type (constant-array-ctype-element-type x))
                                        ; The following requires some explaination
                                        ; Constant arrays of zero length will be assumed to be variable arrays at
                                        ; the end of a instance of either a GCVector or a GCArray
                                        ; and their offset is wrt the GCVector or GCArray start
                                        ; Non-zero length and their offset is calculated wrt the base
                                        ; There should be a better way to decide this
         (elements (if (= (constant-array-ctype-array-size x) 0)
                       (linearize-class-layout-impl element-type element-type analysis)
                       (linearize-class-layout-impl element-type base analysis))))
    (if (> (constant-array-ctype-array-size x) 0)
        (if (fixable-instance-variables element-type analysis)
            (linearize-constant-array-contents x element-type elements)
            nil)
        (list (make-instance 'constant-array-offset
                             :base base
                             :element-type element-type
                             :elements elements
                             :constant-array-size (constant-array-ctype-array-size x)
                             :offset-type x)))))


(defmethod linearize-class-layout-impl ((x incomplete-array-ctype) base analysis)
  (let* ((element-type (incomplete-array-ctype-element-type x))
         (elements (ensure-list (linearize-class-layout-impl element-type element-type analysis))))
    (warn "Either this should never be returned or I need to return something other than an ARRAY-OFFSET for fixing")
    (list (make-instance 'array-offset
                         :base base
                         :element-type element-type
                         :elements elements
                         :offset-type x))))

(defmethod linearize-class-layout-impl ((x pointer-ctype) base analysis )
  (let ((pointee (pointer-ctype-pointee x)))
    (cond
      ((or (gcvector-moveable-ctype-p pointee)
           (gcarray-moveable-ctype-p pointee))
       (list (make-instance 'pointer-offset :base base :offset-type x)))
      ((is-alloc-p (pointer-ctype-pointee x) (analysis-project analysis))
       (list (make-instance 'pointer-offset :base base :offset-type x)))
      ((fixable-pointee-p (pointer-ctype-pointee x))
       (list (make-instance 'pointer-offset :base base :offset-type x)))
      ((ignorable-ctype-p (pointer-ctype-pointee x)) nil)
      (t
       (list (make-instance 'raw-pointer-offset :base base :offset-type x))
       ))))

;; \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

(defun safe-get-name-decl (decl)
  (if (cast:get-identifier decl)
      (cast:get-name decl)
      "NO-NAME-SAFE"))

(defun decl-name (decl)
  (let ((result (cast:get-qualified-name-as-string decl)))
    result))

(defun template-arg-as-string (template-arg)
  (let* ((template-arg-kind (cast:get-kind template-arg)))
    (case template-arg-kind
      (clang-ast:argkind-type
;;       (cast:get-as-string (cast:get-as-type template-arg))
       (record-key (cast:get-type-ptr-or-null (cast:get-as-type template-arg))))
      (clang-ast:argkind-integral
       (llvm:apint-to-string (cast:get-as-integral template-arg) 10 t))
      (clang-ast:argkind-pack
       (let ((pack-size (cast:pack-size template-arg)))
         (if (eql pack-size 0)
             ""
             (format nil "TEMPLATE_ARG_PACK_SIZE~a" pack-size))))
      (clang-ast:argkind-template
       "TEMPLATE_ARG_AS_STRING::TEMPLATE")
      (clang-ast:argkind-expression
       "TEMPLATE_ARG_AS_STRING::EXPRESSION")
      (clang-ast:argkind-declaration
       "TEMPLATE_ARG_AS_STRING::DECLARATION")
      (otherwise
       (error "Add support for template-arg-as-string of kind: ~a" template-arg-kind)))))

(defgeneric record-key-impl (node))

(defmethod record-key-impl ((node cast:type))
  (cast:get-as-string (cast:make-qual-type node))) ;; get-as-string (cast:desugar node)))

(defmethod record-key-impl ((node cast:record-type))
  "Names of RecordType(s) are calculated using recursive calls to record-key.
This avoids the prefixing of 'class ' and 'struct ' to the names of classes and structs"
  (record-key-impl (cast:get-decl node)))

(defmethod record-key-impl ((decl-node cast:named-decl))
  "Return the name of the class/struct and whether it is a template specializer or not"
;;  (when (search "llvm::AssertingVH" (decl-name decl-node) ) (break "Caught AssertingVH"))
  (or decl-node (error "There is a problem, the decl-node is nil"))
  (case (type-of decl-node)
    ((cast:cxxrecord-decl
      cast:record-decl)
     (values (decl-name decl-node) nil))
    (cast:enum-decl
     (values (decl-name decl-node) nil))
    (cast:class-template-specialization-decl
     (let* ((template-args (cast:get-template-args decl-node))
            (template-args-as-list (loop :for i :from 0 :below (cast:size template-args)
                                        :collect (let* ((template-arg (cast:template-argument-list-get template-args i)))
                                                   (template-arg-as-string template-arg)))))
       (values (format nil "~a<~{~a~^,~}>" (decl-name decl-node) template-args-as-list) t)))
    (cast:class-template-partial-specialization-decl
     (let* ((template-args (cast:get-template-args decl-node))
            (template-args-as-list (loop :for i :from 0 :below (cast:size template-args)
                                      :for template-arg = (cast:template-argument-list-get template-args i)
                                      :for type-name = (template-arg-as-string template-arg)
                                      :collect type-name)))
       (values (format nil "~a<~{~a~^,~}>" (decl-name decl-node) template-args-as-list) t)))
    (otherwise
     (format t "Add support for record-key for ~a  get-name->~a" decl-node (decl-name decl-node))
     #+use-breaks(break "Check the decl-node"))))

(defun record-key (node)
  (multiple-value-bind (name template-specializer)
      (record-key-impl node)
    (values (remove-class-struct-noise name) template-specializer)))

(defun classify-template-args (decl)
  (let ((template-args (cast:get-template-args decl))
        args)
    (do* ((i (1- (cast:size template-args)) (1- i)))
         ((< i 0))
      (let* ((arg (cast:template-argument-list-get template-args i))
             (template-arg-kind (cast:get-kind arg))
             classified integral-value)
        (case template-arg-kind
          (clang-ast:argkind-type
           (let* ((qtarg (cast:get-as-type arg))
                  (tsty-new (cast:get-type-ptr-or-null qtarg)))
             (if (and qtarg tsty-new)
                 (progn
                   (gclog "classify-template-arg: ~a~%" (cast:get-as-string qtarg))
                   (setq classified (classify-ctype tsty-new)))
                 (format t "!!classify-template-args - could not classify arg: ~s  qtarg: ~s  (cast:get-as-string qtarg): ~s~%" arg qtarg (cast:get-as-string qtarg))
                 )))
          (clang-ast:argkind-integral
           (setf integral-value (template-arg-as-string arg)
                 classified nil))
          (otherwise
           #+use-breaks(break "Handle template-arg-kind: ~a" template-arg-kind)))
        (gclog "classified2 = ~a~%" classified)
        (push (make-gc-template-argument :index i :ctype classified :integral-value integral-value) args)))
    args))

(defun classify-decl (decl)
  "This is used during matching to classify C++ types into Common Lisp structures that
can be saved and reloaded within the project for later analysis"
  (let* ((name (cast:get-name decl))
         (decl-key (record-key decl)))
    ;;    (break "Check decl")
    ;; (typep decl 'cast:cxxrecord-decl)
    ;; (typep decl 'cast:class-template-specialization-decl)
    ;;  
    (case (type-of decl)
      (cast:class-template-specialization-decl
       (let ((args (cast:get-template-args decl)))
         (cond
           ((and (string= name "atomic")
                 (let* ((classified-args (classify-template-args decl)))
                   (gclog "          Found an atomic object~%")
                   (format t "ATOMIC - what do I do with decl-key: ~s  classified-args: ~s decl: ~s~%" decl-key classified-args decl)
                   (cond
                     ((and (= (length classified-args) 1)
                           (gc-template-argument-p (first classified-args)))
                      (let ((ctype (gc-template-argument-ctype (first classified-args))))
                        (format t "    ctype = ~s~%" ctype)
                        (make-atomic-ctype :key decl-key :name name :argument ctype)))
                     (t nil) ; punt to another clause
                     ))))
           ((string= name "smart_ptr")
            (assert (eql (cast:size args) 1) nil "Must have 1 argument")
            (let* ((arg (cast:template-argument-list-get args 0))
                   (qtarg (cast:get-as-type arg)))
              (gclog "          Found a smart_ptr~%")
              (let ((ctype (make-smart-ptr-ctype :key decl-key :specializer (cast:get-as-string qtarg))))
                (format t "Found smart_ptr: ~s~%" ctype)
                ctype
                )))
           ((string= name "tagged_pointer")
            (assert (eql (cast:size args) 1) nil "Must have 1 argument")
            (let* ((arg (cast:template-argument-list-get args 0))
                   (qtarg (cast:get-as-type arg)))
              (gclog "          Found a tagged_pointer~%")
              (make-tagged-pointer-ctype :key decl-key :specializer (cast:get-as-string qtarg))))
           ((string= name "GCVector_moveable")
            (let* ()
              (gclog "          Found a GCVector_moveable~%")
              (make-gcvector-moveable-ctype :key decl-key :name name :arguments (classify-template-args decl))))
           ((string= name "GCVector_atomic")
            (let* ()
              (gclog "          Found a GCVector_atomic~%")
              (make-gcvector-moveable-ctype :key decl-key :name name :arguments (classify-template-args decl))))
           ((string= name "GCBitUnitArray_moveable")
            (let* ((classified-args (classify-template-args decl)))
              (format t "Recognized GCBitUnitArray_moveable  decl: ~s classified-args: ~s~%" decl classified-args)
              (make-gcbitunitarray-moveable-ctype :key decl-key :name name :arguments classified-args)))
           ((string= name "GCArray_moveable")
            (gclog "          Found a GCArray_moveable~%")
            (make-gcarray-moveable-ctype :key decl-key :name name :arguments (classify-template-args decl)))
           ((string= name "GCArraySignedLength_moveable")
            (gclog "          Found a GCArraySignedLength_moveable~%")
            (make-gcarray-moveable-ctype :key decl-key :name name :arguments (classify-template-args decl)))
           ((string= name "GCArray_atomic")
              (gclog "          Found a GCArray_atomic~%")
              (make-gcarray-moveable-ctype :key decl-key :name name :arguments (classify-template-args decl)))
           ((string= name "map")
            (make-std-map-ctype :key decl-key :name name))
           ((string= name "basic_string")
            (make-basic-string-ctype :key decl-key :name name))
           ((string= name "unique_ptr")
            (make-unique-ptr-ctype :key decl-key :name name :arguments (classify-template-args decl)))
           ((search name "dont_expose")
            (let ((template-args (classify-template-args decl)))
              (format t "dont_expose type: ~a ~a ~a~%" decl-key name template-args)
              (make-dont-expose-ctype :key decl-key :name name :argument (classify-template-args decl))))
           ((string= name "dont_analyze")
            (format t "dont_analyze type: ~a~%" decl-key)
            (make-dont-analyze-ctype :key decl-key :name name :argument (classify-template-args decl)))
           (t
            #+(or)(warn "classify-ctype cast:record-type unhandled class-template-specialization-decl  key = ~a  name = ~a~%IGNORE-NAME ~a~%IGNORE-KEY ~a" decl-key name name decl-key)
            (make-class-template-specialization-ctype :key decl-key 
                                                      :name name
                                                      :arguments (classify-template-args decl)
                                                      )))))
      (cast:cxxrecord-decl
       (cond
         ((string= name "Mutex")
          (format t "Making a mutex-ctype name: ~s~%" name)
          (make-mutex-ctype :key decl-key :name name))
         ((string= name "SharedMutex")
          (make-shared-mutex-ctype :key decl-key :name name))
         ((string= name "WeakPointer")
          (make-weak-ptr-ctype :key decl-key))
         ((string= name "Ephemeron")
          (make-ephemeron-ctype :key decl-key))
         (t (make-cxxrecord-ctype :key decl-key :name name))))
      (cast:record-decl
       (warn "classify-decl found ~a decl-key: ~a name: ~a - this means that the mostDerivedType code isn't working!!!!" (type-of decl) decl-key name)
       (make-unknown-ctype :key decl-key ))
      (otherwise
       (warn "Add support for classify-ctype ~s name: ~s (type-of decl) -> ~s" decl-key name (type-of decl))
       (make-unknown-ctype :key decl-key)))))

(defgeneric classify-ctype (type))

(defmethod classify-ctype ((x cast:record-type))
  (let* ((decl (cast:get-decl x)))
    (classify-decl decl)))

(defmethod classify-ctype ((x cast:injected-class-name-type))
  (make-injected-class-name-ctype :key (record-key (cast:get-decl x))))

(defmethod classify-ctype ((tsty cast:template-specialization-type))
  "template-specialization-types are never interesting"
  (let* ((template-name (cast:get-template-name tsty))
         (template-decl (cast:get-as-template-decl template-name))
         (name (cast:get-name template-decl))
         (cxxrecord-decl (cast:get-as-cxxrecord-decl tsty)))
    (if (null cxxrecord-decl) 
        (make-unclassified-template-specialization-ctype
         :key (record-key tsty) ;; (cast:get-as-string (cast:desugar tsty))
         :template-name name)
        (error "Should never get here because template-specialization-types should never have cxxrecord-decl defined - its: ~a" cxxrecord-decl ))))

(defmethod classify-ctype ((x cast:typedef-type))
  (let* ((canonical-qual-type (cast:get-canonical-type-internal x))
         (canonical-type (cast:get-type-ptr-or-null canonical-qual-type)))
    (classify-ctype canonical-type)))

(defmethod classify-ctype ((x cast:elaborated-type))
  (let* ((canonical-qual-type (cast:get-canonical-type-internal x))
         (canonical-type (cast:get-type-ptr-or-null canonical-qual-type))
         )
    (classify-ctype canonical-type)))

(defmethod classify-ctype ((x cast:pointer-type))
  (make-pointer-ctype :pointee (classify-ctype (cast:get-type-ptr-or-null (cast:get-pointee-type x)))))

(defmethod classify-ctype ((x cast:constant-array-type))
  (let ((array-size (clang-ast:constant-array-get-size x))
        (element-type  (classify-ctype (cast:get-type-ptr-or-null (cast:get-element-type x)))))
    (make-constant-array-ctype
     :array-size array-size
     :element-type element-type)))

(defmethod classify-ctype ((x cast:incomplete-array-type))
  (let ((element-type (classify-ctype (cast:get-type-ptr-or-null (cast:get-element-type x)))))
    (make-incomplete-array-ctype :key (record-key x) :element-type element-type)))

(defmethod classify-ctype ((x cast:paren-type))
  (error "Should not be called")
  #+(or)(make-paren-ctype :inner (classify-ctype (cast:get-type-ptr-or-null (cast:get-inner-type x)))))

(defmethod classify-ctype ((x builtin-type))
  (make-builtin-ctype :key (record-key x)))

(defmethod classify-ctype ((x function-proto-type))
  (make-function-proto-ctype :key (record-key x)))

(defmethod classify-ctype ((x lvalue-reference-type))
  (make-lvalue-reference-ctype :key (record-key x)))

(defmethod classify-ctype ((x template-type-parm-type))
  (make-template-type-parm-ctype :key (record-key x)))

(defmethod classify-ctype ((x dependent-name-type))
  (make-dependent-name-ctype :key (record-key x)))

(defmethod classify-ctype ((x rvalue-reference-type))
  (make-rvalue-reference-ctype :key (record-key x)))

(defmethod classify-ctype ((x enum-type))
  (make-enum-ctype :key (record-key x)))

(defmethod classify-ctype ((x t))
  (let ((key (record-key x)))
    (warn "Add support for classify-ctype to recognize ~a key: ~a" x key)
    (make-unclassified-ctype :key key)))

;;; Convert a clang::Type or clang::QualType to the canonical clang::Type
(defgeneric to-canonical-type (x))

(defmethod to-canonical-type ((x cast:qual-type))
  (to-canonical-type (cast:get-type-ptr-or-null x)))

(defmethod to-canonical-type ((x cast:type))
  (let* ((canonical-qual-type (cast:get-canonical-type-internal x))
         (canonical-type (cast:get-type-ptr-or-null canonical-qual-type)))
    canonical-type))

;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------

(defun project-pathname (project-name project-type)
  (make-pathname :name project-name :type project-type :defaults (clang-tool:main-pathname)))

(defun save-data (run pathname)
  (format t "Saving data to ~a~%" pathname)
  (let ((*print-readably* t)
        (*print-array* t)
        (*print-circle* nil)
        (*print-right-margin* 140)
        (*print-pretty* nil))
    (with-open-file (fout pathname :direction :output)
      (prin1 run fout))))
  
(defun load-data (pathname)
  (with-open-file (fin pathname :direction :input)
    (read fin)))

(defun save-project (project)
  (core::with-print-readably
      (with-open-file (fout (project-pathname "project" "dat") :direction :output)
        (prin1 project fout))))

(defvar *project*)

(defun load-project (db &optional pathname)
  (let ((*package* (find-package :clasp-analyzer)))
    (clang-tool:with-compilation-tool-database db
      (let ((project-pathname (if pathname pathname (project-pathname "project" "dat"))))
        (format t "Loading project from: ~a~%" project-pathname)
        (let ((project (load-data project-pathname)))
          (setq *project* project)
          project)))))

(defun write-hash-table (name table stream)
  (unless (zerop (hash-table-count table))
    (loop for key being the hash-keys of table using (hash-value value)
          initially (prin1 name stream)
                    (fresh-line stream)
          finally (prin1 nil stream)
                  (fresh-line stream)
          do (prin1 key stream)
             (write-char #\Space stream)
             (prin1 value stream)
             (fresh-line stream))))

(defun save-current-project (output-path
                             &aux (*package* (find-package :clasp-analyzer))
                                  (*print-pretty* nil))                            
  (with-open-file (stream output-path :direction :output :if-exists :supersede)
    (write-hash-table :classes (project-classes *project*) stream)
    (write-hash-table :lispallocs (project-lispallocs *project*) stream)
    (write-hash-table :classallocs (project-classallocs *project*) stream)
    (write-hash-table :rootclassallocs (project-rootclassallocs *project*) stream)
    (write-hash-table :containerallocs (project-containerallocs *project*) stream)))

(defun read-hash-table (table stream)
  (loop for key = (read stream nil nil)
        while key
        if (gethash key table)
          do (read-line stream)
        else
          do (setf (gethash key table) (read stream))))

(defun load-into-current-project (input-path)
  (with-open-file (stream input-path :direction :input)
    (loop with *package* = (find-package :clasp-analyzer)
          for name = (read stream nil nil)
          while name
          do (read-hash-table (ecase name
                                (:classes (project-classes *project*))
                                (:lispallocs (project-lispallocs *project*))
                                (:classallocs (project-classallocs *project*))
                                (:rootclassallocs (project-rootclassallocs *project*))
                                (:containerallocs (project-containerallocs *project*)))
                              stream))))

;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;;
;; Search for every regular class - get base classes, fields and methods
;;

(defparameter *class-matcher*
  '(:bind :whole
    (:cxxrecord-decl
     ;; The class must be a definition
     (:is-definition))))
(clang-tool:compile-matcher *class-matcher*)

;;
;; The lispified version of this matcher: cxxRecordDecl(forEach(typedefNameDecl(hasName("Base")).bind("ClaspBase")))
(defparameter *clasp-base-submatcher-sexp*
  '(:cxxrecord-decl
    (:matches-name ".*_O"
     (:for-each
      (:typedef-name-decl
       (:has-name "Base")
       (:bind "ClaspBase" (:typedef-name-decl)))))))

(defparameter *clasp-base-submatcher* (ast-tooling:parse-dynamic-matcher "cxxRecordDecl(matchesName(\".*_O\"),forEach(typedefDecl(hasName(\"Base\"),hasType(type().bind(\"BaseType\"))).bind(\"ClaspBase\")))"))

#+(or)
(defparameter *clasp-base-submatcher* (ast-tooling:parse-dynamic-matcher "cxxRecordDecl(matchesName(\".*_O\"),forEach(typedefNameDecl(hasName(\"Base\"),hasType(builtinType().bind(\"BaseType\"))).bind(\"ClaspBase\")))"))
#+(or)(defparameter *clasp-base-submatcher* (clang-tool:compile-matcher *clasp-base-submatcher-sexp*))

(defparameter *base-class-submatcher*
  (clang-tool:compile-matcher '(:cxxrecord-decl
                     (:is-derived-from
                      (:for-each
                       (:cxxrecord-decl
                        (:bind :base-name (:cxxrecord-decl))
                        (:has-ancestor
                         (:namespace-decl
                          (:bind :base-ns (:namespace-decl))))
                        ))))))
(or *base-class-submatcher*
    (error "Problem encountered compiling *base-class-submatcher*"))


(defparameter *field-submatcher-sexp*
  '(:cxxrecord-decl
    (:for-each ;; -descendant
     (:field-decl
      (:bind :field (:field-decl))))))
(defparameter *field-submatcher*
  (clang-tool:compile-matcher *field-submatcher-sexp*))

(or *field-submatcher*
    (error "Problem encountered compiling *field-submatcher*"))

(defun setup-cclass-search (mtool)
  (symbol-macrolet
      ((results (project-classes (clang-tool:multitool-results mtool))))
    (labels ((%%new-class-callback
                 (match-info class-node record-key template-specializer)
               (declare (core:lambda-name %%new-class-callback))
               (gclog "Entered %%new-class-callback~%")
               (let (bases vbases fields)
                 (gclog "In let~%")
                 ;;
                 ;; Run a matcher to find the base classes and their namespaces
                 ;;
                 (gclog "Starting (ext:do-c++-iterator (it (cast:bases-iterator class-node))~%")
                 (multiple-value-bind (start end)
                     (cast:bases-iterator class-node)
                   (let* ((next1 (sys:iterator-step start))
                          (next2 (sys:iterator-step next1)))
                     (declare (ignorable next2))
                     (gclog "(cast:bases-iterator class-node) start == end -> ~s~%" (core:iterator= start end))
                     (gclog "(cast:bases-iterator class-node) next1 == end -> ~s~%" (core:iterator= next1 end))
                     (gclog "(cast:bases-iterator class-node) next2 == end -> ~s~%" (core:iterator= next2 end))))
                 (ext:do-c++-iterator (it (cast:bases-iterator class-node))
                   (gclog "In do-c++-iterator bases-iterator~%")
                   (gclog "In do-c++-iterator bases-iterator it -> ~s~%" it)
                   (unless it
                     (multiple-value-bind (start end)
                         (cast:bases-iterator class-node)
                       (format t "(cast:bases-iterator  ~s -> ~s~%" start end)
                       (error "bases-iterator is nil for class -> ~s" class-node)))
                   (gclog "About to try and get type~%")
                   (let* ((qty (let ((qty (cast:get-type it)))
                                 (gclog "Got type ~s~%" qty)
                                 (gclog "    get-as-string -> ~s~%" (cast:get-as-string qty))
                                 qty))
                          (canonical-qty (let ((x (cast:get-canonical-type-internal (cast:get-type-ptr-or-null qty))))
                                           (gclog "got canonical-qty ~s~%" x)
                                           x))
                          (base-decl (let ((x (cast:get-as-cxxrecord-decl (cast:get-type-ptr-or-null canonical-qty))))
                                       (gclog "got base-decl ~s~%" x)
                                       x)))
                     (gclog "In inner let~%")
                     (when base-decl
                       (push (record-key base-decl) bases))))
                 (gclog "Starting (ext:do-c++-iterator (it (cast:vbases-iterator class-node))~%")
                 (ext:do-c++-iterator (it (cast:vbases-iterator class-node))
                   (gclog "In do-c++-iterator vbases-iterator~%")
                   (gclog "In do-c++-iterator vbases-iterator it -> ~s~%" it)
                   (or it (error "vbases-iterator is nil for class -> ~s" class-node))
                   (let* ((qty (cast:get-type it))
                          (canonical-qty (cast:get-canonical-type-internal (cast:get-type-ptr-or-null qty)))
                          (base-decl (cast:get-as-cxxrecord-decl (cast:get-type-ptr-or-null canonical-qty))))
                     (when base-decl
                       (push (record-key base-decl) vbases))))
                 ;;
                 ;; Run a matcher to find the "Base" class as specified in the LISP_CLASS macro
                 ;;
                 (let (lisp-base)
                   (gclog "About to sub-match-run line 1748~%")
                   (clang-tool:sub-match-run
                    *clasp-base-submatcher*
                    *clasp-base-submatcher-sexp*
                    (clang-tool:mtag-node match-info :whole)
                    (clang-tool:ast-context match-info)
                    (lambda (minfo)
                      (declare (core:lambda-name %%class-base-callback.lambda))
                      (gclog "In %%clasp-base-callback.lambda callback~%")
                      (let* ((base-type (clang-tool:mtag-node minfo :BaseType)))
                        (format t "Clasp Base class for ~s is ~s  ->" record-key base-type)
                        (let ((name (cond
                                      ((typep base-type 'cast:record-type)
                                       (let* ((decl (cast:get-decl base-type))
                                              (decl-name (decl-name decl)))
                                         decl-name))
                                      ((typep base-type 'cast:elaborated-type)
                                       (let* ((qual-type (cast:get-named-type base-type))
                                              (as-string (cast:get-as-string qual-type))
                                              (space-pos (position #\space as-string))
                                              (base-only (subseq as-string (1+ space-pos))))
                                         base-only))
                                      (t (format t "Unknown type ~a~%" base-type)))))
                          (setf lisp-base name)))))
                   ;;
                   ;; Run a matcher to find the GC-scannable fields of this class
                   ;;
                   (gclog "About to sub-match-run line 1775~%")
                   (clang-tool:sub-match-run
                    *field-submatcher*
                    *field-submatcher-sexp*
                    (clang-tool:mtag-node match-info :whole)
                    (clang-tool:ast-context match-info)
                    (lambda (minfo)
                      (declare (core:lambda-name %%new-class-callback.lambda))
                      (gclog "In %%new-class-callback.lambda~%")
                      (let* ((field-node (clang-tool:mtag-node minfo :field))
                             (type (progn
                                     (or field-node (error "field-node is nil"))
                                     (to-canonical-type (cast:get-type field-node)))))
                        (gclog "      >> Field: ~30a~%" field-node)
                        (handler-case
                            (push (make-instance-variable :field-name (clang-tool:mtag-name minfo :field)
                                                          :access (clang-ast:get-access (clang-tool:mtag-node minfo :field))
                                                          :ctype (classify-ctype (to-canonical-type type)))
                                  fields)
                          (unsupported-type (err)
                            (declare (ignore err))
                            (error "Add support for classifying type: ~a (type-of type): ~a  source: ~a"
                                   type (type-of type) (clang-tool:mtag-source minfo :field)))))))
                   (gclog "Storing results~%")
                   (prog1
                       (setf (gethash record-key results)
                             (progn
                               (gclog "About to make-cclass ~s~%" record-key)
                               (make-cclass :key record-key
                                            :template-specializer template-specializer
                                            :definition-data (let ((definitions nil))
                                                               (gclog "About to cast:is-polymorphic~%")
                                                               (when (cast:is-polymorphic class-node)
                                                                 (pushnew :is-polymorphic definitions))
                                                               ;; There may be more in the future
                                                               definitions)
                                            :lisp-base lisp-base
                                            :bases bases
                                            :vbases vbases
                                            :fields fields)))
                     (gclog "Done storing results - returning~%")
                     ))))
             (%%class-callback (match-info)
               (declare (core:lambda-name %%class-callback))
               (gclog "In %%class-callback~%")
               (gclog "MATCH: ------------------~%")
               (gclog "    Start:~%~a~%" (clang-tool:mtag-loc-start match-info :whole))
               (gclog "    Name: ~a~%" (clang-tool:mtag-name match-info :whole))
               (let* ((class-node (clang-tool:mtag-node match-info :whole)))
                 (gclog "    Got class-node~%")
                 (gclog "       class-node: ~a~%" class-node)
                 (multiple-value-bind (record-key template-specializer)
                     (record-key class-node)
                   (gclog "    called record-key~%")
                   (gclog "    record-key -> ~a~%" record-key)
                   (unless (or (typep class-node 'cast:class-template-partial-specialization-decl) ; ignore partial specializations
                               (and (typep class-node 'cast:class-template-specialization-decl) ; ignore template specializations that have undeclared specialization alloc
                                    (eq (cast:get-specialization-kind class-node) 'clang-ast:tsk-undeclared))
                               (gethash record-key results)) ; ignore if we've seen it before
                     (gclog "About to call %%new-class-callback~%")
                     (%%new-class-callback match-info class-node record-key template-specializer))))))
      (clang-tool:multitool-add-matcher mtool
                                        :name :cclasses
                                        :matcher-sexp *class-matcher*
                                        :initializer (lambda () (setf results (make-hash-table :test #'equal)))
                                        :callback (make-instance 'clang-tool:code-match-callback
                                                                 :timer (make-instance 'clang-tool:code-match-timer
                                                                                       :name 'class-callback)
                                                                 :match-code (function %%class-callback))))))

;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;;
;; Search for lispallocs   as template parameters for GCObjectAllocator
;;

(defparameter *lispalloc-matcher*
  '(:cxxrecord-decl
    (:is-definition)
    (:is-template-instantiation)
    (:has-name "GCObjectAllocator")))

(defun setup-lispalloc-search (mtool)
  "Setup the TOOL (multitool) to look for instance variables that we want to garbage collect
and the inheritance hierarchy that the garbage collector will need"
  (symbol-macrolet ((class-results (project-lispallocs (clang-tool:multitool-results mtool))))
    (flet ((%%lispalloc-matcher-callback (match-info)
             (declare (core:lambda-name %%lispalloc-matcher-callback.lambda))
             "This function can only be called as a ASTMatcher callback"
             (gclog "Entered %%lispalloc-matcher-callback.lambda~%")
             (let* ((decl (clang-tool:mtag-node match-info :whole))
                    (args (cast:get-template-args decl))
                    (arg (cast:template-argument-list-get args 0))
                    (qtarg (cast:get-as-type arg))
                    (tsty-new (cast:get-type-ptr-or-null qtarg))
                    (class-key (record-key tsty-new))
                    (classified (classify-ctype tsty-new))
                    (arg-decl (cast:get-decl tsty-new)) ;; Should I convert to canonical type?????
                    (arg-name (cast:get-name arg-decl)))
               (unless (gethash class-key class-results)
                 (gclog "Adding class name: ~a~%" class-key)
                 ;; (break "Check locations")
                 (let ((lispalloc (make-lispalloc :key class-key
                                                  :name arg-name ;; XXXXXX (clang-tool:mtag-name :whole)
                                                  :ctype classified)))
                   (setf (gethash class-key class-results) lispalloc))))))
      ;; Initialize the class search
      (clang-tool:multitool-add-matcher mtool
                                        :name :lispallocs
                                        :matcher-sexp `(:bind :whole ,*lispalloc-matcher*)
                                        :initializer (lambda () (setf class-results (make-hash-table :test #'equal))) ; initializer
                                        :callback (make-instance 'clang-tool:code-match-callback
                                                                 :timer (make-instance 'clang-tool:code-match-timer
                                                                                       :name 'lisp-alloc-matcher)
                                                                 :match-code (function %%lispalloc-matcher-callback))))))

;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;;
;; Search for classallocs - they are template parameters for ClassAllocator
;;

(defparameter *classalloc-matcher*
  '(:cxxrecord-decl
    (:is-definition)
    (:is-template-instantiation)
    (:has-name "ClassAllocator")))

(defun setup-classalloc-search (mtool)
  "Setup the TOOL (multitool) to look for instance variables that we want to garbage collect
and the inheritance hierarchy that the garbage collector will need"
  (symbol-macrolet ((class-results (project-classallocs (clang-tool:multitool-results mtool))))
    (flet ((%%classalloc-matcher-callback (match-info)
             (declare (core:lambda-name %%claspalloc-matcher-callback.lambda))
             "This function can only be called as a ASTMatcher callback"
             (gclog "Entered %%claspalloc-matcher-callback.lambda~%")
             (let* ((decl (clang-tool:mtag-node match-info :whole))
                    (args (cast:get-template-args decl))
                    (arg (cast:template-argument-list-get args 0))
                    (qtarg (cast:get-as-type arg))
                    (tsty-new (cast:get-type-ptr-or-null qtarg))
                    (class-key (record-key tsty-new))
                    (classified (classify-ctype tsty-new))
                    (arg-decl (cast:get-decl tsty-new)) ;; Should I convert to canonical type?????
                    (arg-name (cast:get-name arg-decl)))
               (unless (gethash class-key class-results)
                 (gclog "Adding class name: ~a~%" class-key)
                 (let ((classalloc (make-classalloc :key class-key
                                                    :name arg-name ;;(clang-tool:mtag-name :whole)
                                                    :ctype classified)))
                   (setf (gethash class-key class-results) classalloc))))))
      ;; Initialize the class search
      (clang-tool:multitool-add-matcher mtool
                             :name :classallocs
                             :matcher-sexp `(:bind :whole ,*classalloc-matcher*)
                             :initializer (lambda () (setf class-results (make-hash-table :test #'equal))) ; initializer
                             :callback (make-instance 'clang-tool:code-match-callback
                                                      :timer (make-instance 'clang-tool:code-match-timer
                                                                            :name 'classalloc-matcher)
                                                      :match-code (function %%classalloc-matcher-callback))))))

;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;;
;; Search for rootclassallocs - they are template parameters for Rootclassallocator
;;

(defparameter *rootclassalloc-matcher*
  '(:cxxrecord-decl
    (:is-definition)
    (:is-template-instantiation)
    (:has-name "RootClassAllocator")))

(defun setup-rootclassalloc-search (mtool)
  "Setup the TOOL (multitool) to look for instance variables that we want to garbage collect
and the inheritance hierarchy that the garbage collector will need"
  (symbol-macrolet ((class-results (project-rootclassallocs (clang-tool:multitool-results mtool))))
    (flet ((%%rootclassalloc-matcher-callback (match-info)
             (declare (core:lambda-name %%rootalloc-matcher-callback.lambda))
             "This function can only be called as a ASTMatcher callback"
             (let* ((decl (clang-tool:mtag-node match-info :whole))
                    (args (cast:get-template-args decl))
                    (arg (cast:template-argument-list-get args 0))
                    (qtarg (cast:get-as-type arg))
                    (tsty-new (cast:get-type-ptr-or-null qtarg))
                    (class-key (record-key tsty-new))
                    (classified (classify-ctype tsty-new))
                    (arg-decl (cast:get-decl tsty-new)) ;; Should I convert to canonical type?????
                    (arg-name (cast:get-name arg-decl)))
               (unless (gethash class-key class-results)
                 (gclog "Adding class name: ~a~%" class-key)
                 (let ((rootclassalloc (make-rootclassalloc :key class-key
                                                  :name arg-name ;;(clang-tool:mtag-name :whole)
                                                  :ctype classified)))
                   (setf (gethash class-key class-results) rootclassalloc))))))
      ;; Initialize the class search
      (clang-tool:multitool-add-matcher mtool
                             :name :rootclassallocs
                             :matcher-sexp `(:bind :whole ,*rootclassalloc-matcher*)
                             :initializer (lambda () (setf class-results (make-hash-table :test #'equal))) ; initializer
                             :callback (make-instance 'clang-tool:code-match-callback
                                                      :timer (make-instance 'clang-tool:code-match-timer
                                                                            :name 'rootclassalloc-matcher)
                                                      :match-code (function %%rootclassalloc-matcher-callback))))))

;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;;
;; Search for containerallocs - they are template parameters for GCContainerAllocator
;;

(defparameter *containeralloc-matcher*
  '(:cxxrecord-decl
    (:is-definition)
    (:is-template-instantiation)
    (:is-same-or-derived-from
     (:cxxrecord-decl
      (:has-name "GCContainer")))))

(defun setup-containeralloc-search (mtool)
  "Setup the TOOL (multitool) to look for instance variables that we want to garbage collect
and the inheritance hierarchy that the garbage collector will need"
  (symbol-macrolet ((class-results (project-containerallocs (clang-tool:multitool-results mtool))))
    (flet ((%%containeralloc-matcher-callback (match-info)
             (declare (core:lambda-name %%containeralloc-matcher-callback.lambda))
             "This function can only be called as a ASTMatcher callback"
             (let* ((decl (clang-tool:mtag-node match-info :whole))
                    (class-key (record-key decl))
                    (classified (classify-decl decl)))
               (unless (gethash class-key class-results)
                 (let ((containeralloc (make-containeralloc :key class-key
                                                            :name (clang-tool:mtag-name match-info :whole)
                                                            :ctype classified)))
                   (setf (gethash class-key class-results) containeralloc))))))
      ;; Initialize the class search
      (clang-tool:multitool-add-matcher mtool
                             :name :containerallocs
                             :matcher-sexp `(:bind :whole ,*containeralloc-matcher*)
                             :initializer (lambda () (setf class-results (make-hash-table :test #'equal))) ; initializer
                             :callback (make-instance 'clang-tool:code-match-callback
                                                      :timer (make-instance 'clang-tool:code-match-timer
                                                                            :name 'containeralloc-matcher)
                                                      :match-code (function %%containeralloc-matcher-callback))))))

;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;;
;; stage: analysis
;; Carry out analysis on the project
;;
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------

(defparameter *contains-fixptr-impl-ht* nil)
(defun contains-fixptr-p (x project &optional (record (make-hash-table :test #'eq)))
  "The third argument may be a hash-table :test #'eq that keeps track of classes that contain-fixptr-p
so that they don't have to be constantly recalculated"
  (let ((*contains-fixptr-impl-ht* record))
    (contains-fixptr-impl-p x project)))

(defgeneric contains-fixptr-impl-p (x project)
  (:documentation "Returns true if x contains a fixptr by inheritance or directly or it contains a class that contains a fixptr"))

(defmethod contains-fixptr-impl-p :around (x project)
  (multiple-value-bind (precalc precalc-p)
      (gethash x *contains-fixptr-impl-ht*)
    (if precalc-p
        precalc
        (progn
          (setf (gethash x *contains-fixptr-impl-ht*) nil)
          (let ((res (call-next-method x project)))
            (setf (gethash x *contains-fixptr-impl-ht*) res)
            res)))))

(defmethod contains-fixptr-impl-p ((x cclass) project)
  (let* (result
         (all-classes (project-classes project)))
    (loop :for base-name :in (cclass-bases x)
       :do (when base-name
             (let ((base (gethash base-name all-classes)))
               (when base 
                 (let ((one-result (contains-fixptr-impl-p base project)))
                   (setq result (or result one-result)))))))
    (loop :for base-name :in (cclass-vbases x)
       :do (when base-name
             (let ((base (gethash base-name all-classes)))
               (when base 
                 (let ((one-result (contains-fixptr-impl-p base project)))
                   (setq result (or result one-result)))))))
    (loop :for field :in (cclass-fields x)
       :do (when field
             (let* ((field-ctype (instance-field-ctype field))
                    (one-result (contains-fixptr-impl-p field-ctype project)))
               (setq result (or result one-result)))))
    result))

(defmethod contains-fixptr-impl-p ((x class-template-specialization-ctype) project)
  (let ((c (gethash (ctype-key x) (project-classes project))))
    (if c
        (contains-fixptr-impl-p c project)
        (progn
          (warn "Could not find class for ~a in contains-fixptr-impl-p" (ctype-key x))
          nil))))


(defmethod contains-fixptr-impl-p ((x constant-array-ctype) project)
  (contains-fixptr-impl-p (constant-array-ctype-element-type x) project))

(defmethod contains-fixptr-impl-p ((x incomplete-array-ctype) project)
  (contains-fixptr-impl-p (incomplete-array-ctype-element-type x) project))

(defmethod contains-fixptr-impl-p ((x cxxrecord-ctype) project)
  (cond
    ((string= (cxxrecord-ctype-key x) "(my-anonymous-class-name)") nil)
    (t (let ((key (cxxrecord-ctype-key x)))
         (let ((c (gethash key (project-classes project))))
           (if c
               (contains-fixptr-impl-p c project)
               nil))))))

(macrolet ((defnot (name)
             `(defmethod contains-fixptr-impl-p ((x ,name) project)
                (declare (ignore project))
                nil))
           (defnots (&rest names)
             `(progn ,@(loop for name in names collect `(defnot ,name)))))
  (defnots basic-string-ctype std-map-ctype shared-mutex-ctype mutex-ctype
    unique-ptr-ctype injected-class-name-ctype unclassified-ctype
    builtin-ctype enum-ctype lvalue-reference-ctype function-proto-ctype
    unclassified-template-specialization-ctype))

(defmethod contains-fixptr-impl-p ((x smart-ptr-ctype) project)
  (declare (ignore project))
  t)
(defmethod contains-fixptr-impl-p ((x gc-template-argument) project)
  (let ((ctype (gc-template-argument-ctype x)))
     (contains-fixptr-impl-p ctype project)))
                                   
(defmethod contains-fixptr-impl-p ((x instance-variable) project)
  (let ((ctype (instance-field-ctype x)))
    (contains-fixptr-impl-p ctype project)))
(defmethod contains-fixptr-impl-p ((x gcarray-moveable-ctype) project)
  (let* ((arguments (gcarray-moveable-ctype-arguments x))
         (arg0 (loop :for arg :in arguments
                  :when (eq (gc-template-argument-index arg) 0)
                  :return arg)))
    (contains-fixptr-impl-p arg0 project)))
(defmethod contains-fixptr-impl-p ((x gcvector-moveable-ctype) project)
  (let* ((arguments (gcarray-moveable-ctype-arguments x))
         (arg0 (loop :for arg :in arguments
                  :when (eq (gc-template-argument-index arg) 0)
                  :return arg)))
    (contains-fixptr-impl-p arg0 project)))

(defmethod contains-fixptr-impl-p ((x atomic-ctype) project)
  (contains-fixptr-impl-p (atomic-ctype-argument x) project))

(defmethod contains-fixptr-impl-p ((x tagged-pointer-ctype) project)
  (declare (ignore project))
  t)
(defmethod contains-fixptr-impl-p ((x weak-ptr-ctype) project)
  (declare (ignore project))
  t)
(defmethod contains-fixptr-impl-p ((x ephemeron-ctype) project)
  (declare (ignore project))
  t)
(defmethod contains-fixptr-impl-p ((x pointer-ctype) project)
  (cond
    ((container-p (pointer-ctype-pointee x)) t)
    ((contains-fixptr-impl-p (pointer-ctype-pointee x) project) 
     t
     )
    ((null (pointer-ctype-key x)) nil)
    (t
     (warn "Handle contains-fixptr-impl-p for ~a" x)
     nil)))

(defun classes-that-contain-fixptrs (project)
  "This is not used at the moment for analysis"
  (let ((contain (make-hash-table :test #'equal))
        (record (make-hash-table :test #'eq)))
    (maphash (lambda (k v)
               (when (contains-fixptr-p v project record)
                 (setf (gethash k contain) v)))
             (project-classes project))
    contain))

(defun add-ctype (forwards key alloc-ctype)
  (when (string= key "class core::SequenceStepper *")
    (break "trap"))
  (setf (gethash key forwards) alloc-ctype))

;;; Most of these methods may be unnecessary now that we only consider simple allocs and not templated ones
(defgeneric expand-forwards-with-template-arguments (forwards alloc-ctype))
                                        ; don't need anything for cxxrecord-ctype
(defmethod expand-forwards-with-template-arguments (forwards (alloc-ctype t))
  (declare (ignore forwards))
  (warn "expand-forwards-with-template-arguments most general  alloc-ctype--> ~a~%" alloc-ctype))
(defmethod expand-forwards-with-template-arguments
    ((forwards t) (alloc-ctype cxxrecord-ctype))
  nil)
(defmethod expand-forwards-with-template-arguments
    ((forwards t) (alloc-ctype null))
  nil)
(defmethod expand-forwards-with-template-arguments
    ((forwards t) (alloc-ctype unclassified-ctype))
  nil)
(defmethod expand-forwards-with-template-arguments
    ((forwards t) (alloc-ctype smart-ptr-ctype))
  nil)
(defmethod expand-forwards-with-template-arguments
    ((forwards t) (alloc-ctype tagged-pointer-ctype))
  nil)
(defmethod expand-forwards-with-template-arguments
    ((forwards t) (alloc-ctype weak-ptr-ctype))
  nil)
(defmethod expand-forwards-with-template-arguments
    ((forwards t) (alloc-ctype ephemeron-ctype))
  nil)
(defmethod expand-forwards-with-template-arguments (forwards (alloc-ctype cxxrecord-ctype))
  (add-ctype forwards (ctype-key alloc-ctype) alloc-ctype))
(defmethod expand-forwards-with-template-arguments (forwards (alloc-ctype pointer-ctype))
  (expand-forwards-with-template-arguments forwards (pointer-ctype-pointee alloc-ctype)))

(defmethod expand-forwards-with-template-arguments (forwards (alloc-ctype class-template-specialization-ctype))
  (mapc (lambda (template-arg) (expand-forwards-with-template-arguments forwards (gc-template-argument-ctype template-arg)))
        (class-template-specialization-ctype-arguments alloc-ctype)))

(defun fill-forward-declarations (forwards stamp)
  (when (simple-stamp-p stamp)
    (expand-forwards-with-template-arguments forwards (alloc-ctype (simple-stamp-alloc stamp)))))

(defun generate-forward-declarations (analysis)
  (let* ((forwards (analysis-forwards analysis)))
    (maphash (lambda (key stamp) (declare (ignore key)) (fill-forward-declarations forwards stamp)) (analysis-stamps analysis))))

(defun compact-stamp-value (species-num alloc-num)
  (logior (ash species-num 16) alloc-num))

;;; Store memoized stamp names
(defvar *stamp-names* (make-hash-table))

(defun get-stamp-name (stamp)
  (or (gethash stamp *stamp-names*)
      (setf (gethash stamp *stamp-names*)
            (concatenate 'base-string "STAMPWTAG_" (c++identifier (stamp-key stamp))))))

(defun noop (&rest args)
  (declare (ignore args))
  nil)

(defstruct species
  name
  discriminator        ;; Function - takes one argument, returns a species index
  (bitwidth 0)         ;; bit(1),crumb(2),nibble(4),byte(8) etc containers length need this to be converted to bytes
  (tags #'noop)        ;; Function - generates tags for species
  index)

;;
;; The abstract-species is a species of class in the class hierarchy
;; that is never allocated - I currently define template classes in the
;; Array hierarchy that end up being abstract-species classes.
;; We don't want to generate certain code for abstract-species classes.
;;
(defstruct manager
  abstract-species
  (species nil) ;; a list of species
  (next-species-counter 0)
  ignore-discriminator)

(defun lookup-species (manager name)
  (dolist (sp (manager-species manager))
    (when (eq name (species-name sp))
      (return-from lookup-species sp)))
  (error "Could not find species ~a in manager" name))

(defun add-species (manager species)
  (setf (species-index species) (manager-next-species-counter manager))
  (incf (manager-next-species-counter manager))
  (push species (manager-species manager))
  species)

(defparameter *unknown-aclass* nil)

(defparameter *manager* nil)

(defun identify-species (manager aclass)
  (let ((hits (remove-if (lambda (species)
                           (not (funcall (species-discriminator species) aclass)))
                         (manager-species manager))))
    (cond ((> (length hits) 1)
           (format t "Identifies as multiple species: ~s~%" (mapcar #'species-name hits))
           (error "The class ~a could not be uniquely distinguished between the species: ~a"
                  aclass hits))
          ((eql (length hits) 1)
           (car hits))
          ((and (manager-ignore-discriminator manager)
                (funcall (manager-ignore-discriminator manager) aclass))
           nil)
          (t
           (setf *unknown-aclass* aclass
                 *manager* manager)
           (error " Could not identify species for ~a~%Check *unknown-aclass* and *manager*~%Species:~%~{~s~%~}"
                  aclass (manager-species manager))))))

(defun alloc-template-specializer-p (alloc analysis)
  (let ((alloc-class (gethash (alloc-key alloc)
                              (project-classes (analysis-project analysis)))))
    (when alloc-class
      (cclass-template-specializer alloc-class))))

(defun ensure-stamp-for-ancestors (class-key analysis)
  (when (and class-key
             (null (gethash class-key (analysis-stamps analysis))))
    (let* ((project (analysis-project analysis))
           (class (gethash class-key (project-classes project)))
           (base-classes (cclass-bases class))
           (base-class-key (car base-classes)))
      (when (and base-class-key (not (string= base-class-key "RootClass")))
        (ensure-stamp-for-ancestors base-class-key analysis))
      (format t "MAKE-STAMP ~a~%" class-key)
      (setf (gethash class-key (analysis-stamps analysis))
            (make-stamp :key class-key
                        :species (manager-abstract-species (analysis-manager analysis))
                        :cclass class)))))
  
(defun ensure-stamp-for-alloc-and-parent-classes (alloc analysis)
  (let* ((manager (analysis-manager analysis))
         (species (identify-species manager alloc))
         (class-key (alloc-key alloc))
         (project (analysis-project analysis))
         (alloc-stamp (gethash class-key (project-classes project)))
         (base-classes (cclass-bases alloc-stamp))
         (base-class-key (car base-classes)))
    (unless #+(or)base-class-key (string= base-class-key "RootClass")
      (ensure-stamp-for-ancestors base-class-key analysis))
    (cond
      ((and (not (containeralloc-p alloc))
            (alloc-template-specializer-p alloc analysis))
       ;; We have a templated type - see if we need to construct a templated stamp
       (let* ((class (gethash class-key (project-classes (analysis-project analysis))))
              (single-base-key (let ((bases (cclass-bases class)))
                                 (assert (eql (length bases) 1) nil "There can only be one base but class ~a has bases ~a" class bases) ;; there can be only one base
                                 (assert (null (cclass-vbases class))) ;; There can be no vbases
                                 (car bases)))
              (single-base (gethash single-base-key (project-classes (analysis-project analysis)))))
         (let* ((key (cclass-key single-base))
                (tstamp (multiple-value-bind (te present-p)
                           ;; get the templated-stamp
                           (gethash key (analysis-stamps analysis))
                         (if (and present-p (templated-stamp-p te))
                             ;; If its present and a templated-stamp then return it
                             te
                             ;; If there wasn't a templated-stamp in the hash-table - create one
                             (progn
                               (when (simple-stamp-p te)
                                 (warn "Since ~a is templated it must be a templated-stamp - but there is already a simple-stamp defined with this key - this error happened probably because you tried to allocate the TemplateBase of templated classes - don't do that!!" key))
                               (setf (gethash key (analysis-stamps analysis))
                                     (make-templated-stamp :key key
                                                          :value% :unassigned
                                                          :cclass single-base
                                                          :species species)))))))
           ;; save every alloc associated with this templated-stamp
           (push alloc (templated-stamp-all-allocs tstamp)))))
      (t ;; It's a simple-stamp
       (let* ((class (gethash class-key (project-classes (analysis-project analysis)))))
         (unless class ;; system allocs don't have classes - make a bogus one
           (setq class (make-cclass :key class-key)))
         (setf (gethash class-key (analysis-stamps analysis))
               (make-simple-stamp :key class-key
                                 :value% :unassigned
                                 :cclass class
                                 :alloc alloc
                                 :species species)))))))

(defun organize-allocs-into-species-and-create-stamps (analysis)
  "Every GCObject and GCContainer is assigned to a species and given a GCStamp stamp value."
  (let ((project (analysis-project analysis)))
    (flet ((ensure-stamps (k alloc)
             (declare (ignore k))
             (ensure-stamp-for-alloc-and-parent-classes alloc analysis)))
      (maphash #'ensure-stamps (project-lispallocs project))
      (maphash #'ensure-stamps (project-containerallocs project))
      (maphash #'ensure-stamps (project-classallocs project))
      (maphash #'ensure-stamps (project-rootclassallocs project)))))

(defun tags-for-lispallocs (stamp anal)
  (assert (simple-stamp-p stamp))
  (let* ((alloc (simple-stamp-alloc stamp))
         (key (alloc-key alloc)))
    (gclog "build-mps-scan-for-one-family -> inheritance key[~a]  value[~a]~%" key value)
    (let* ((class-node (gethash key (project-classes (analysis-project anal))))
           (layout (class-layout class-node anal))
           (definition-data (cclass-definition-data class-node)))
      (tags-for-lisp-layout stamp key layout definition-data anal))))

(defun tags-for-templated-lispallocs (stamp anal)
  (assert (templated-stamp-p stamp))
  (let* ((key (stamp-key stamp))
         (stamp-name (get-stamp-name stamp)))
    (declare (ignorable stamp-name))
    (gclog "build-mps-scan-for-one-family -> inheritance key[~a]  value[~a]~%" key value)
    (let* ((class-node (gethash key (project-classes (analysis-project anal))))
           (layout (class-layout class-node anal))
           (definition-data (cclass-definition-data class-node))                       )
      (tags-for-templated-layout stamp key layout definition-data anal))))

(defun tags-for-gccontainer (stamp anal)
  (check-type stamp simple-stamp)
  (let* ((alloc (simple-stamp-alloc stamp))
         (decl (containeralloc-ctype alloc))
         (key (alloc-key alloc)))
    (when (cxxrecord-ctype-p decl)
      (error "Should never scan ~a" (cxxrecord-ctype-key decl)))
    (let* ((class-node (gethash key (project-classes (analysis-project anal))))
           (layout (class-layout class-node anal))
           (definition-data (cclass-definition-data class-node)))
      (tags-for-container-layout stamp key layout definition-data anal))))

;; don't need to scan but do need to calculate size
(defun tags-for-gcbitunit (stamp anal)
  (assert (simple-stamp-p stamp))
  (let* ((alloc (simple-stamp-alloc stamp))
         (key (alloc-key alloc)))
    (gclog "build-mps-scan-for-one-family -> inheritance key[~a]  value[~a]~%" key value)
    (let* ((class-node (gethash key (project-classes (analysis-project anal))))
           (layout (class-layout class-node anal))
           (definition-data (cclass-definition-data class-node)))
      (tags-for-bitunit-container-layout stamp key layout definition-data anal))))

(defun string-left-matches (str sub)
  (eql (search str sub) 0))

(defvar *analysis*)

(defun setup-manager ()
  (let* ((manager (make-manager :abstract-species
                                (make-species :name :abstract))))
    (add-species manager (make-species :name :bootstrap
                                       :discriminator (lambda (x)
                                                        (and (lispalloc-p x)
                                                             (gctools::bootstrap-kind-p (alloc-key x))))
                                       :tags 'tags-for-lispallocs))
    (add-species manager (make-species :name :lispalloc
                                       :discriminator (lambda (x)
                                                        (and (lispalloc-p x)
                                                             (not (gctools:bootstrap-kind-p (alloc-key x)))
                                                             (not (alloc-template-specializer-p x *analysis*))))
                                       :tags 'tags-for-lispallocs))
    (add-species manager (make-species :name :templated-lispalloc
                                       :discriminator (lambda (x)
                                                        (and (lispalloc-p x)
                                                             (alloc-template-specializer-p x *analysis*)))
                                       :tags 'tags-for-templated-lispallocs))
    (add-species manager (make-species :name :gcvector
                                       :discriminator (lambda (x)
                                                        (and (containeralloc-p x)
                                                             (search "gctools::GCVector" (alloc-key x))))
                                       :tags 'tags-for-gccontainer))
    (add-species manager (make-species :name :gcarray
                                       :discriminator (lambda (x)
                                                        (and (containeralloc-p x)
                                                             (search "gctools::GCArray" (alloc-key x))))
                                       :tags 'tags-for-gccontainer))
    (add-species manager (make-species :name :classalloc
                                       :discriminator (lambda (x)
                                                        (and (classalloc-p x)
                                                             (not (alloc-template-specializer-p x *analysis*))))
                                       :tags 'tags-for-lispallocs))
    (add-species manager (make-species :name :rootclassalloc
                                       :discriminator (lambda (x)
                                                        (rootclassalloc-p x))
                                       :tags 'tags-for-lispallocs))
    (add-species manager (make-species :name :templated-classalloc
                                       :discriminator (lambda (x)
                                                        (and (classalloc-p x)
                                                             (alloc-template-specializer-p x *analysis*)))
                                       :tags 'tags-for-templated-lispallocs))
    (loop for cur-bitwidth in '(1 2 4)
          for cur-species-name = (intern (format nil "GCBITUNITCONTAINER~a" cur-bitwidth) :keyword)
          do (add-species manager (make-species :name cur-species-name
                                                :discriminator (let ((bitwidth cur-bitwidth)
                                                                     (species-name cur-species-name))
                                                                 (lambda (x)
                                                                   (let* ((match-string (format nil "gctools::GCBitUnitArray_moveable<~a," bitwidth))
                                                                          (containeralloc (containeralloc-p x))
                                                                          (key-match (search match-string (alloc-key x))))
                                                                     (when (and containeralloc key-match)
                                                                       (format t "Matched a match-string: ~s with (alloc-key x): ~s~%" match-string (alloc-key x))
                                                                       (format t "      species-name: ~s~%" species-name)
                                                                       t))))
                                                :bitwidth cur-bitwidth
                                                :tags 'tags-for-gcbitunit)))
    manager))

(defun sort-order (names)
  (sort names #'< :key #'car))

(defun sort-stamps-by-value (analysis)
  (let (names)
    (maphash #'(lambda (k stamp)
                 (declare (ignore k))
                 (when (not (eq (stamp-value% stamp) :no-stamp-value))
                   (push (cons (stamp-value% stamp) stamp) names)))
             (analysis-stamps analysis))
    (setf (analysis-sorted-stamps analysis)
          (mapcar #'(lambda (val-stamp)
                    (cdr val-stamp))
                (sort-order names)))))

(defun analyze-project (project &key types-to-inline-mps-functions)
  (let ((*analysis* (make-analysis :project project
                                 :inline types-to-inline-mps-functions))
        (manager (setup-manager)))
    (setf (analysis-manager *analysis*) manager)
    (organize-allocs-into-species-and-create-stamps *analysis*)
    (generate-forward-declarations *analysis*)
    (analyze-hierarchy *analysis*)
    (sort-stamps-by-value *analysis*)
    *analysis*))

(defun derived-from-cclass* (child ancestor searched project)
  (cond ((gethash child searched)
         ;; Protect ourselves from loops in the inheritance
         ;; There shouldn't be any but something is messed up
         nil)
        ((eq child ancestor)
         t)
        (t
         (setf (gethash child searched) t)
         (dolist (parent-name (cclass-bases child))
           (let ((parent (gethash parent-name (project-classes project))))
             (when (derived-from-cclass* parent ancestor searched project)
               (return-from derived-from-cclass* t))))
         (dolist (parent-name (cclass-vbases child))
           (let ((parent (gethash parent-name (project-classes project))))
             (when (derived-from-cclass* parent ancestor searched project)
               (return-from derived-from-cclass* t))))
         nil)))

(defun derived-from-cclass (child ancestor &optional (project *project*))
  (let ((child-class (if (stringp child) (gethash child (project-classes project)) child))
        (ancestor-class (if (stringp ancestor) (gethash ancestor (project-classes project)) ancestor))
        (searched (make-hash-table)))
    (derived-from-cclass* child-class ancestor-class searched project)))

(defun inherits-from-multiple-classes* (child searched project)
  "Return true if somewhere in the ancestors there is a branch in the inheritance"
  (cond ((gethash child searched)
         ;; Protect ourselves from loops in the inheritance
         ;; There shouldn't be any but something is messed up
         nil)
        (t
         (setf (gethash child searched) t)
         (let ((num-parents (+ (length (cclass-bases child)) (length (cclass-vbases child)))))
           (if (> num-parents 1)
               t
               (let* ((parent (or (car (cclass-bases child)) (car (cclass-vbases child))))
                      (parent-class (gethash parent (project-classes *project*))))
                 (when parent-class
                   (inherits-from-multiple-classes* parent-class searched project))))))))

(defun inherits-from-multiple-classes (child &optional (project *project*))
  (let ((child-class (if (stringp child) (gethash child (project-classes project)) child))
        (searched (make-hash-table)))
    (inherits-from-multiple-classes* child-class searched project)))


;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;;
;; stage: output
;; Generate output
;;
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------

(defun generate-one-gckind-for-stamp (stream stamp)
  (let* ((stamp-name (get-stamp-name stamp)))
    (cond
      ((simple-stamp-p stamp)
;;       (format stream "//GCKind for ~a~%" stamp)
       (format stream "template <> class gctools::GCStamp<~A> {~%" (stamp-key stamp)))
      (t ;; templated-stamp
;;       (format stream "//GCTemplatedKind for ~a~%" stamp)
       (format stream "template <> class gctools::GCStamp<~A> {~%" (stamp-key stamp))))
    (format stream "public:~%")
    (format stream "  static gctools::GCStampEnum const StampWtag = gctools::~a ;~%" stamp-name)
    (format stream "};~%")))

(defun abstract-species-stamp-p (stamp analysis)
  "Return T if the species of the stamp is the abstract-species for the analysis."
  (let* ((manager (analysis-manager analysis))
         (abstract-species (manager-abstract-species manager)))
    (eq (stamp-species stamp) abstract-species)))

(defun generate-gckind-for-stamps (fout anal)
  (maphash (lambda (key stamp)
             (declare (ignore key))
             (when (and (not (eq (stamp-value% stamp) :no-stamp-value))
                        (not (abstract-species-stamp-p stamp anal)))
               (generate-one-gckind-for-stamp fout stamp)))
           (analysis-stamps anal)))

(defun class-layout (x analysis)
  (let* ((fields (ensure-list (linearize-class-layout-impl x x analysis)))
         (fixed (loop :for var :in fields
                   :when (not (typep var 'container-offset))
                   :collect var))
         (variable (loop :for var :in fields
                      :when (typep var 'container-offset)
                      :collect var)))
    (when (> (length variable) 1)
      (analysis-error "Class ~a has more than one GCVector/GCArray part" (cclass-key x)))
    (make-instance 'class-layout
                   :layout-class x
                   :fixed-part fixed
                   :variable-part (first variable))))

(defun field-with-name (cclass name)
  "* Arguments
- cclass :: A cclass.
- name :: A string.
* Description
Search through the fields of cclass and return the one with the matching name.
Otherwise return nil."
  (dolist (field (cclass-fields cclass))
    (let ((the-name (instance-variable-field-name field)))
      (when (string= the-name name)
        (return-from field-with-name field))))
  nil)

(defun fixable-instance-variables (x analysis)
  (let ((fix-vars (fixable-instance-variables-impl x analysis)))
    fix-vars))

(defgeneric fixable-instance-variables-impl (x analysis)
  (:documentation
  "* Arguments 
- x :: An instance variable, a cclass or a ctype.
- analysis :: An analysis.
* Description
Recursively analyze x and return T if x contains fixable pointers."
 ))

(defmethod fixable-instance-variables-impl (x analysis)
  (declare (ignore x analysis))
  nil)

(defmethod fixable-instance-variables-impl ((x instance-variable) analysis)
  (fixable-instance-variables-impl (instance-field-ctype x) analysis))

(defmethod fixable-instance-variables-impl ((x instance-array-element) analysis)
  (fixable-instance-variables-impl (instance-field-ctype x) analysis))

(defmethod fixable-instance-variables-impl ((x cclass) analysis
                                            &aux (project-classes (project-classes (analysis-project analysis))))
  (nconc (loop for base in (cclass-bases x)
               append (fixable-instance-variables-impl (gethash base project-classes) analysis))
         (loop for vbase in (cclass-vbases x)
               append (fixable-instance-variables-impl (gethash vbase project-classes) analysis))
         (loop for field in (cclass-fields x)
               append (fixable-instance-variables-impl (fix-code-for-field field analysis) analysis))))

(defmethod fixable-instance-variables-impl ((x builtin-ctype) analysis)
  (declare (ignore analysis))
  nil)

(defmethod fixable-instance-variables-impl ((x basic-string-ctype) analysis)
  (declare (ignore analysis))
  nil)

(defmethod fixable-instance-variables-impl ((x std-map-ctype) analysis) 
  (declare (ignore analysis))
  nil)

(defmethod fixable-instance-variables-impl ((x shared-mutex-ctype) analysis) 
  (declare (ignore analysis))
  nil)

(defmethod fixable-instance-variables-impl ((x mutex-ctype) analysis) 
  (declare (ignore analysis))
  nil)

(defmethod fixable-instance-variables-impl ((x unique-ptr-ctype) analysis) 
  (declare (ignore analysis))
  nil)

(defmethod fixable-instance-variables-impl ((x atomic-ctype) analysis) 
  (declare (ignore analysis))
  nil)

(defmethod fixable-instance-variables-impl ((x dont-expose-ctype) analysis) 
  (declare (ignore analysis))
  nil)

(defmethod fixable-instance-variables-impl ((x dont-analyze-ctype) analysis) 
  (declare (ignore analysis))
  nil)

(defmethod fixable-instance-variables-impl ((x unclassified-ctype) analysis)
  (declare (ignore analysis))
  (cond
    ((ignorable-ctype-p x) nil)
    (t (warn "ignoring fixable-instance-variables-impl for ~a" x)))
  nil)

(defmethod fixable-instance-variables-impl ((x cxxrecord-ctype) analysis)
  (let ((code (gethash (cxxrecord-ctype-key x) (project-classes (analysis-project analysis)))))
    (unless code
      (error "Could not find ~a in (project-classes (analysis-project analysis))" (cxxrecord-ctype-key x)))
    (fixable-instance-variables-impl code analysis)))

(defmethod fixable-instance-variables-impl ((x class-template-specialization-ctype) analysis)
  (cond
    ((string= (ctype-key x) "unsigned long") nil)
    (t
     (fixable-instance-variables-impl (gethash (ctype-key x) (project-classes (analysis-project analysis))) analysis))))

(defmethod fixable-instance-variables-impl ((x smart-ptr-ctype) analysis)
  (declare (ignore analysis))
  :smart-ptr-fix)

(defmethod fixable-instance-variables-impl ((x tagged-pointer-ctype) analysis)
  (declare (ignore analysis))
  :tagged-pointer-fix)

(defmethod fixable-instance-variables-impl ((x weak-ptr-ctype) analysis)
  (declare (ignore analysis))
  :weak-ptr-fix)
(defmethod fixable-instance-variables-impl ((x ephemeron-ctype) analysis)
  (declare (ignore analysis))
  :ephemeron-fix)

(defmethod fixable-instance-variables-impl ((x constant-array-ctype) analysis)
  (when (contains-fixptr-p x (analysis-project analysis))
    (fixable-instance-variables-impl (constant-array-ctype-element-type x) analysis)))

(defmethod fixable-instance-variables-impl ((x pointer-ctype) analysis )
  (let ((pointee (pointer-ctype-pointee x)))
    (cond
      ((or (gcvector-moveable-ctype-p pointee)
           (gcarray-moveable-ctype-p pointee))
       :raw-tagged-pointer-fix)
      ((is-alloc-p (pointer-ctype-pointee x) (analysis-project analysis))
       :raw-tagged-pointer-fix)
      ((fixable-pointee-p (pointer-ctype-pointee x))
       :raw-tagged-pointer-fix)
      ((ignorable-ctype-p (pointer-ctype-pointee x)) nil)
      (t
       ;;(warn "I'm not sure if I can ignore pointer-ctype ~a  ELIMINATE THESE WARNINGS" x)
       nil))))

(defun fixable-pointee-p (ctype)
  #+(or)(let ((key (ctype-key ctype)))
    (inherits-metadata key :metadata_always_fix_pointers_to_derived_classes)))

(defgeneric ignorable-ctype-p (ctype))
(defmethod ignorable-ctype-p ((ctype t)) nil) ;; by default nothing is ignorable
(defmethod ignorable-ctype-p ((ctype dependent-name-ctype)) nil) ;; Should any of these be nil?
(defmethod ignorable-ctype-p ((ctype unknown-ctype))
  (warn "ignorable-ctype-p called with ~a" ctype)
  nil)

(defun make-table (strings)
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (s strings)
      (setf (gethash s ht) t))
    ht))

(defparameter +not-ignorable-unclassified-ctype+
  (make-table '("double"
                "int"
                "long long"
                "unsigned long")))

(defparameter +ignorable-unclassified-ctype+
  (make-table '("void (void *)"
                "void"
                "_Bool"
                "stamp asttooling::ContextType"
                "stamp asttooling::ErrorType"
                "stamp boost::filesystem::file_type"
                "stamp boost::filesystem::perms"
                "stamp clang::InputKind"
                "stamp clang::ast_matchers::dynamic::VariantValue::ValueType"
                "stamp llvm::APFloat::fltCategory"
                "short"
                "unsigned char"
                "unsigned long long"
                "void (core::Lisp_O *)")))

(defparameter +ignorable-cxxrecord-ctype+
  (make-table '("__sFILE"
                "boost::detail::sp_counted_base"
                "boost::filesystem::detail::dir_itr_imp"
                "boost::filesystem::detail::recur_dir_itr_imp"
                "boost::filesystem::directory_entry"
                "boost::filesystem::directory_iterator"
                "boost::filesystem::recursive_directory_iterator"
                "boost::iostreams::detail::chain_base<boost::iostreams::chain<boost::iostreams::input, char, std::__1::char_traits<char>, std::__1::allocator<char> >, char, std::__1::char_traits<char>, std::__1::allocator<char>, boost::iostreams::input>::chain_impl"
                "boost::iostreams::detail::chain_base<boost::iostreams::chain<boost::iostreams::output, char, std::__1::char_traits<char>, std::__1::allocator<char> >, char, std::__1::char_traits<char>, std::__1::allocator<char>, boost::iostreams::output>::chain_impl"
                "boost::re_detail::named_subexpressions"
                "clang::ASTUnit"
                "clang::CompilerInstance"
                "clang::ast_matchers::dynamic::VariantMatcher"
                "llvm::AttributeImpl"
                "llvm::BasicBlock"
                "llvm::DIBuilder"
                "llvm::DataLayout"
                "llvm::EngineBuilder"
                "llvm::ExecutionEngine"
                "llvm::IRBuilderBase"
                "llvm::Instruction"
                "llvm::LLVMContext"
                "llvm::Linker"
                "llvm::MDNode"
                "llvm::MemoryBuffer"
                "llvm::Module"
                "llvm::NamedMDNode"
                "llvm::Pass"
                "llvm::PassManagerBuilder"
                "llvm::Type"
                "llvm::Value"
                "llvm::fltSemantics"
                "llvm::legacy::PassManagerBase"
                "std::__1::locale::__imp"
                "std::type_info")))

(defparameter +ignorable-class-template-specialization-ctype+
  (make-table '("boost::iostreams::chain<boost::iostreams::input,char,std::__1::char_traits<char>,std::__1::allocator<char>>"
                "boost::iostreams::chain<boost::iostreams::output,char,std::__1::char_traits<char>,std::__1::allocator<char>>"
                "boost::re_detail::basic_regex_implementation<char,boost::regex_traits<char,boost::cpp_regex_traits<char>>>"
                "boost::sub_match<const char *>"
                "std::__1::__tree_node<std::__1::__value_type<std::__1::basic_string<char,std::__1::char_traits<char>,std::__1::allocator<char>>,int>,void *>"
                "std::__1::__tree_node<std::__1::basic_string<char,std::__1::char_traits<char>,std::__1::allocator<char>>,void *>"
                "std::__1::__tree_node_base<void *>"
                "std::__1::basic_ostream<char,std::__1::char_traits<char>>"
                "std::__1::basic_string<char,std::__1::char_traits<char>,std::__1::allocator<char>>"
                "std::__1::codecvt<char,char,(anonymous)>")))

(defun separate-namespace-name (name)
  "Separate a X::Y::Z name into (list X Y Z) - strip any preceeding 'class '"
  (let* ((class-noise "class ")
         (full-name (if (and (> (length name) (length class-noise)) (string= (subseq name 0 (length class-noise)) class-noise))
                        (subseq name (length class-noise) (length name))
                        name))
         (colon-pos (search "::" full-name)))
    (if colon-pos
        (let ((namespace (subseq full-name 0 colon-pos))
              (name (subseq full-name (+ colon-pos 2) (length full-name))))
          (list* namespace (separate-namespace-name name)))
        (list name))))


(defun strip-all-namespaces-from-name (name)
  (let ((parts (separate-namespace-name name)))
    (car (last parts))))

(defun remove-string (name str)
  (let* ((pos (search str name)))
    (if pos
        (let ((news (make-array (- (length name) (length str)) :element-type 'base-char :adjustable nil)))
          (replace news (subseq name 0 pos) :start1 0)
          (replace news (subseq name (+ pos (length str)) (length name)) :start1 pos)) 
        name)))

(defun remove-namespace (namespace name)
  (let* ((prefixed (concatenate 'string namespace "::"))
         (pos (search prefixed name)))
    (if pos
        (let ((removed (concatenate 'string (subseq name 0 pos) (subseq name (+ pos (length prefixed)) (length name)))))
          (remove-namespace namespace removed))
        name)))

(defun remove-class-space (name)
  (remove-string name "class "))

(defun remove-struct-space (name)
  (remove-string name "struct "))

(defun remove-class-struct-noise (name)
  (remove-class-space (remove-struct-space name)))

(defstruct namespace
  (submap (make-hash-table :test #'equal)) ;; map namespaces to names
  names)

(defun namespace-add-name (ns name)
  (if (eql (length name) 1)
      (push (car name) (namespace-names ns))
      (let ((subnamespace (gethash (car name) (namespace-submap ns) (make-namespace))))
        (setf (gethash (car name) (namespace-submap ns)) subnamespace)
        (namespace-add-name subnamespace (cdr name)))))

(defun tag-for-namespace-names (forwards)
  (make-instance 'tags:forwards-tag :forwards% (loop for key being the hash-keys of forwards
                                                     collect key)))

(defun merge-forward-names-by-namespace (analysis)
  (let ((forwards (analysis-forwards analysis))
        (top-namespace (make-namespace)))
    (maphash (lambda (name value)
               (declare (ignorable value))
               (let ((split-name (separate-namespace-name name)))
                 (namespace-add-name top-namespace split-name)))
             forwards)
    top-namespace))

(defun prefix-template-if-needed (name)
  (if (search "<" name)
      "template <>"
      ""))

(defun generate-alloc-stamp (&key (fout t) (fdesc t) (analysis *analysis*))
  (declare (ignorable fdesc))
  (let ((maxstamp 0))
    (format fout "STAMPWTAG_null = ADJUST_STAMP(0), ~%")
    #+(or)(let ((hardwired-kinds (core:hardwired-kinds)))
            (mapc (lambda (kv) (format fout "STAMPWTAG_~a = ADJUST_STAMP(~a), ~%" (car kv) (cdr kv))) hardwired-kinds))
    (mapc (lambda (stamp)
            (format fout "~A = ADJUST_STAMP(~A), // Stamp(~a)  wtag(~a)~%"
                    (get-stamp-name stamp)
                    (stamp-value% stamp)
                    (ash (stamp-value% stamp) (- *wtag-shift*))
                    (logand (stamp-value% stamp) *max-wtag*))
            #+(or)(format fdesc "(define-stampwtag \"~a\" \"~a\" \"~a\" \"~a\" ~a ~a)~%"
                    (get-stamp-name stamp)
                    (cclass-key (stamp-cclass stamp))
                    (first (cclass-bases (stamp-cclass stamp)))
                    (stamp-root-cclass stamp)
                    (stamp-wtag stamp)
                    (stamp-value% stamp))
            (when (> (stamp-value% stamp) maxstamp)
              (setq maxstamp (stamp-value% stamp))))
          (analysis-sorted-stamps analysis))
    (maphash (lambda (stamp-name value)
               (format fout "// Unused ~a = ~a, ~%" stamp-name value))
             (unused-builtin-stamps (analysis-stamp-value-generator analysis)))
    (format fout "  STAMPWTAG_max = ~a,~%" maxstamp)
    (format fout "~%" )))

(defun generate-register-stamp-names (&optional (fout t) (analysis *analysis*))
  (format fout "register_stamp_name(\"STAMPWTAG_null\",0); ~%")
  (mapc (lambda (stamp)
          (format fout "register_stamp_name(\"~A\", ADJUST_STAMP(~A));~%" (get-stamp-name stamp) (stamp-value% stamp)))
        (analysis-sorted-stamps analysis))
  (format fout "~%" ))

(defun is-alloc-p (ctype project)
  "Returns true if the ctype is an object allocated using a template function in gcalloc.
These are objects that are directly managed by the garbage collector.
Pointers to these objects are fixed in obj_scan or they must be roots."
  (or (gethash (ctype-key ctype) (project-rootclassallocs project))
      (gethash (ctype-key ctype) (project-classallocs project))
      (gethash (ctype-key ctype) (project-lispallocs project))
      (gethash (ctype-key ctype) (project-containerallocs project))))

(defun generate-code (analysis &key output-file)
  (format t "About to generate code~%")
  (or output-file (error "You must provide an output-file"))
  (cscrape:write-sif-file (list* (tag-for-namespace-names (analysis-forwards analysis))
                                 (mapcan (lambda (stamp)
                                           (funcall (species-tags (stamp-species stamp)) stamp analysis))
                                         (analysis-sorted-stamps analysis)))
                          output-file)
  (format t "Done generate-code~%"))

(defun build-arguments-adjuster ()
  "Build a function that fixes up compile command arguments to run the static analyzer."
  (lambda (args filename)
    (declare (ignore filename))
    (let ((result (concatenate 'vector args
                               (vector "-v" "-DRUNNING_PRECISEPREP" "-Wno-nullability-completeness"))))
      result)))

(defun setup-tools (compilation-tool-database)
  "* Description
Setup all of the ASTMatcher tools for the clasp-analyzer."
  (let ((tools (clang-tool:make-multitool :arguments-adjuster (build-arguments-adjuster)
                                          :compilation-tool-database compilation-tool-database)))
    (setup-cclass-search tools)         ; search for all classes
    (setup-lispalloc-search tools)
    (setup-classalloc-search tools)
    (setup-rootclassalloc-search tools)
    (setup-containeralloc-search tools)
    tools))

(defun split-list (list pieces)
  (let ((split-size (floor (/ (length list) pieces)))
        result)
    (dotimes (p pieces)
      (let* ((start (* p split-size))
             (end (if (eql p (1- pieces))
                      (length list)
                      (+ start split-size))))
        (push (subseq list start end) result)))
    result))

(defparameter *max-parallel-searches* (core:num-logical-processors))

(defun split-jobs (job-list num-parallel)
  (let ((jobvec (make-array num-parallel)))
    (do* ((i 0 (mod (1+ i) num-parallel))
          (one-job (pop job-list) (pop job-list)))
         ((null one-job) (loop for idx below (length jobvec) collect (elt jobvec idx)))
     (push one-job (elt jobvec i)))))

(defun run-test ()
  (defparameter *test-matcher*
    '(:cxxrecord-decl
      ;;        (:is-definition)
      ;;        (:is-template-instantiation)
      (:matches-name ".*GCInfo.*")))
  (error "match-count is undefined")
  #+(or)(match-count *test-matcher*
               :limit 100
               ;;              :tag :point
               :match-comments '( ".*mytest.*" )
               :match-code #'(lambda (match-info)
                               (let* ((decl (clang-tool:mtag-node match-info :whole))
                                      (args (cast:get-template-args decl))
                                      (arg (cast:template-argument-list-get args 0))
                                      (qtarg (cast:get-as-type arg))
                                      (tsty-new (cast:get-type-ptr-or-null qtarg))
                                      (key (record-key tsty-new))
                                      (classified (classify-ctype tsty-new)))
                                 (format t "MATCH: ------------------~%")
                                 (format t "        Start: ~a~%" (clang-tool:mtag-loc-start match-info :whole))
                                 (format t "         Node: ~a~%" (clang-tool:mtag-node match-info :whole))
                                 (format t "     type-of node: ~a~%" (type-of (clang-tool:mtag-node match-info :whole)))
                                 (format t "         Name: ~a~%" (clang-tool:mtag-name match-info :whole))
                                 (format t "          Arg: ~a~%" classified)
                                 (format t "          key: ~a~%" key)))))

(defun serial-search-all (compilation-tool-database &key (output-file (merge-pathnames #P"project.dat" (clang-tool:main-pathname compilation-tool-database))) (save-project t))
  "* Arguments
- test :: A list of files to run the search on, or NIL for all of them.
- arguments-adjuster :: The arguments adjuster.
* Description
Run searches in *tools* on the source files in the compilation database."
  (format t "serial-search-all --> getcwd: ~a~%" (ext:getcwd))
  (let ((tools (setup-tools compilation-tool-database))
        (all-jobs (clang-tool:source-namestrings compilation-tool-database)))
    (save-data all-jobs (make-pathname :type "lst" :defaults output-file))
    (format t "compilation-tool-database: ~a~%" compilation-tool-database)
    (format t "all-jobs: ~a~%" all-jobs)
    (setf (clang-tool:multitool-results tools) (make-project))
    (clang-tool:batch-run-multitool tools compilation-tool-database)
    (format t "Done batch-run-multitool~%")
    (when save-project
      (let ((project (clang-tool:multitool-results tools)))
        (format t "Saving project data~%")
        (save-data project output-file)
        (format t "Returning project~%")
        (values project output-file)))))

(defstruct parallel-job id filename)
(defstruct parallel-result id filename result)

(defclass job-group ()
  ((jobs :initform nil :accessor jobs)
   (results :initform (make-project) :accessor results)))

(defun one-search-process (compilation-tool-database job-group-index job-group log-path)
  (with-open-file (*standard-output* (merge-pathnames (format nil "process~a-output.log" job-group-index)
                                                      log-path)
                   :direction :output :if-exists :supersede :if-does-not-exist :create)
    (with-open-file (*error-output* (merge-pathnames (format nil "process~a-error.log" job-group-index)
                                                             log-path)
                     :direction :output :if-exists :supersede :if-does-not-exist :create)
      (clang-tool:with-compilation-tool-database compilation-tool-database
        (loop with tools = (setup-tools compilation-tool-database)
              initially (format t "Starting process ~a~%" job-group-index)
                        (setf (clang-tool:multitool-results tools) (results job-group))
              finally (return tools)
              for one-job in (jobs job-group)
              for id = (parallel-job-id one-job)
              for filename = (parallel-job-filename one-job)
              do (format *terminal-io* "Static analysis of ~a ~a~%" id filename)
                 (format t "Static analysis of ~a ~a~%" id filename)
                 (clang-tool:batch-run-multitool tools compilation-tool-database
                                                 :source-namestrings (list filename)))))))

(defparameter *tools* nil)
(defun parallel-search-all-threaded (compilation-tool-database
                                     &key (source-namestrings (clang-tool:source-namestrings compilation-tool-database))
                                       (output-file (merge-pathnames #P"project.dat" (clang-tool:main-pathname compilation-tool-database)))
                                       (save-project t)
                                       (jobs 4))
  (declare (ignorable output-file save-project))
  "* Arguments
- test :: A list of files to run the search on, or NIL for all of them.
- arguments-adjuster :: The arguments adjuster.
* Description
Run searches in *tools* on the source files in the compilation database."
  (format t "serial-search-all --> getcwd: ~a~%" (ext:getcwd))
  (let ((all-jobs source-namestrings)
        (job-groups (loop for idx from 0 below jobs
                          collect (make-instance 'job-group))))
    ;; Distribute the jobs to the job-groups
    (loop for job in all-jobs
          for job-index from 0
          for job-group-index = (mod job-index jobs)
          for job-group = (elt job-groups job-group-index)
          for job-info = (make-parallel-job :id job-index :filename job)
          do (push job-info (jobs job-group)))

    (format t "compilation-tool-database: ~a~%" compilation-tool-database)
    (format t "all-jobs: ~a~%" all-jobs)
    (format t "Starting processes~%")
    (setf *tools* nil)
    (let ((processes (loop for job-group-index from 0 below jobs
                           for job-group = (elt job-groups job-group-index)
                           do (format t "loop for process ~a~%" job-group-index)
                           collect (prog1
                                       (mp:process-run-function
                                        nil
                                        (lambda ()
                                          (let ((tool (one-search-process compilation-tool-database job-group-index job-group *log-path*)))
                                            (mp:atomic-push tool *tools*))))
                                     (sleep 0.5)))))
      (loop for process in processes
            do (mp:process-join process))
      (format t "All processes done merge everything here~%")
      (when save-project
        (format t "Getting results~%")
        (let* ((projects (loop for tool in *tools*
                               collect (clang-tool:multitool-results tool)))
               (merged-project (first projects)))
          (loop for project in (rest projects)
                do (summarize-project project)
                do (merge-projects merged-project project)
                do (format t "After merge merged-project~%")
                do (summarize-project merged-project))
          (save-data merged-project output-file)
          (values merged-project output-file))))))

(defun translate-include (args filename)
  (declare (ignore filename))
  "* Arguments
- args :: A vector of strings (compilation arguments)
* Description
Convert -Iinclude to -I<main-sourcefile-pathname>/include. Uses dynamic variable *main-directory-namestring*."
  (let ((main-directory-namestring (namestring (make-pathname :name nil :type nil :defaults (clang-tool:main-pathname)))))
    (dotimes (i (length args))
      (when (string= (elt args i) "-Iinclude")
        (setf (elt args i) (format nil "-I~a/include" main-directory-namestring))))
    args))

(defun setup-clasp-analyzer-compilation-tool-database (pathname &key selection-pattern source-path-identifier arguments-adjuster)
  "* Arguments
- pathname :: The pathname to the compilation database for clasp analyzer.
- selection-pattern : Used to select a subset of the source files for testing - any files with names that contain this string.
- source-path-identifier :: A string
* Description
Return a compilation database for analyzing the clasp (or some other clasp derived project) source code.
Two files (mps.c and gc_interface.cc) are removed from the list of files that clasp-analyzer will run on.
If the source location of a match contains the string source-path-identifier then that match is processed."
  (let* ((compilation-tool-database (clang-tool:load-compilation-tool-database
                                     pathname
                                     :convert-relative-includes-to-absolute nil
                                     :source-path-identifier source-path-identifier))
         (source-filenames (clang-tool:select-source-namestrings compilation-tool-database selection-pattern))
         (removed-mps-dot-c (remove-if #'(lambda (x) (string= (subseq x (- (length x) 2)) ".c")) source-filenames))
         (final-list removed-mps-dot-c))
    (format t "Searching list of files: ~a~%" final-list)
    (unless final-list
      (error "There are no files selected to search - source-filenames: ~a!!!!" source-filenames))
    (setf (clang-tool:source-namestrings compilation-tool-database) final-list)
    (push #'translate-include (clang-tool:arguments-adjuster-list compilation-tool-database))
    (when arguments-adjuster (push arguments-adjuster (clang-tool:arguments-adjuster-list compilation-tool-database)))
    compilation-tool-database))

(defvar *analysis*)
(defun serial-search/generate-code (compilation-tool-database &key (output-file (merge-pathnames #P"project.dat" (clang-tool:main-pathname compilation-tool-database))))
  (format t "Generating output-file: ~s~%" output-file)
  (clang-tool:with-compilation-tool-database
    compilation-tool-database
    (setf *project* (serial-search-all compilation-tool-database :output-file output-file))
    (format t "About to analyze-project~%")
    (let ((analysis (analyze-project *project*))
          (output-pathname (make-pathname :type "cc" :defaults output-file)))
      (setq *analysis* analysis)
      (setf (analysis-inline analysis) '("core::Cons_O"))
      (format t "About to generate-code~%")
      (generate-code analysis :output-file output-pathname)
      (format t "!~%!~%!   Search done - code generated to ~a~%" output-pathname))
    *project*))

(defun analyze-only (compilation-tool-database &key (output-file (merge-pathnames #P"project.dat" (clang-tool:main-pathname compilation-tool-database))))
  (clang-tool:with-compilation-tool-database compilation-tool-database
    (let ((analysis (analyze-project *project*)))
      (setf (analysis-inline analysis) '("core::Cons_O"))
      (generate-code analysis :output-file (make-pathname :type "cc" :defaults output-file)))
    *project*))

(defun parallel-search/generate-code (compilation-tool-database
                                      &key
                                        source-namestrings
                                        (output-file (merge-pathnames #P"project.dat" (clang-tool:main-pathname compilation-tool-database)))
                                        (pjobs 1))
  (unless source-namestrings
    (setf source-namestrings (clang-tool:source-namestrings compilation-tool-database)))
  (clang-tool:with-compilation-tool-database compilation-tool-database
    (setf *project* (parallel-search-all-threaded compilation-tool-database
                                                  :source-namestrings source-namestrings
                                                  :output-file output-file
                                                  :jobs pjobs)
          *analysis* (analyze-project *project*)
          (analysis-inline *analysis*) '("core::Cons_O"))
    (generate-code *analysis* :output-file (make-pathname :type "cc" :defaults output-file))
    *project*))

(defun search-and-generate-code (output-path database-path
                                 &key log-path parallel
                                      (selection-pattern nil selection-pattern-p))
  (ensure-directories-exist log-path)
  (setf *log-path* log-path)
  (let ((database (if selection-pattern
                      (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database
                        (pathname database-path) :selection-pattern selection-pattern)
                      (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database
                        (pathname database-path)))))
    (if parallel
        (parallel-search/generate-code database :output-file output-path)
        (serial-search/generate-code database :output-file output-path))))

(defun search-source-file (output-path source-path log-path database-path)
  (with-open-file (*standard-output* log-path
                   :direction :output :if-exists :supersede
                   :if-does-not-exist :create)
    (let* ((*error-output* *standard-output*)
           (database (clasp-analyzer:setup-clasp-analyzer-compilation-tool-database
                      database-path))
           (tools (setup-tools database))
           (*project* (make-project)))
      (clang-tool:with-compilation-tool-database database
        (setf (clang-tool:multitool-results tools) *project*)
        (clang-tool:batch-run-multitool tools database
                                        :source-namestrings (list source-path))
        (save-current-project output-path)))))

(defun merge-and-generate-code (output-path project-paths)
  (let ((*project* (make-project))
        *analysis*)
    (loop with count = (length project-paths)
          for project-path in project-paths
          for index from 0
          finally (format t "Project summary:~%  ~a classes~%  ~a lispallocs~
                             ~%  ~a classallocs~%  ~a rootclassallocs~
                             ~%  ~a containerallocs~%"
                          (hash-table-count (project-classes *project*))
                          (hash-table-count (project-lispallocs *project*))
                          (hash-table-count (project-classallocs *project*))
                          (hash-table-count (project-rootclassallocs *project*))
                          (hash-table-count (project-containerallocs *project*)))
          do (format t "Loading ~a [~a/~a]...~%" project-path index count)
             (load-into-current-project project-path))
    (format t "Analyzing...~%")
    (setf *analysis* (analyze-project *project*)
          (analysis-inline *analysis*) '("core::Cons_O"))
    (format t "Generating code...~%")
    (generate-code *analysis* :output-file output-path)))
