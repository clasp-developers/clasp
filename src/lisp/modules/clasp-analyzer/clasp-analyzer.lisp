(provide :clasp-analyzer)

(defpackage #:clasp-analyzer
  (:use #:common-lisp #:core #:ast-tooling #:clang-ast)
  (:export
   #:setup-clasp-analyzer-compilation-tool-database
   #:search/generate-code
   #:load-project
   #:save-project
   #:serial-search-all
   #:search/generate-code
   #:analyze-project
   #:generate-code
   #:build-arguments-adjuster))

(require :clang-tool)

(in-package #:clasp-analyzer)



(defun ensure-list (x)
  (unless (listp x)
    (error "The argument ~a must be a list" x))
  x)

(defparameter *errors* nil
  "Keep track of errors discovered during analysis")

(defmacro analysis-error (fmt &rest body)
  `(push (format nil ,fmt ,@body) *errors*))

;;(require :serialize)
;;(push :use-breaks *features*)
;;(push :gc-warnings *features*)

(defconstant +isystem-dir+ 
  #+target-os-darwin "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/7.0"
  #+target-os-linux "/usr/include/clang/3.6/include"
  "Define the -isystem command line option for Clang compiler runs")

(defconstant +resource-dir+ 
  #+target-os-darwin "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/7.0"
  #+target-os-linux "/usr/lib/llvm-3.6/bin/../lib/clang/3.6.0/include"
  #+(or)"/home/meister/Development/externals-clasp/build/release/lib/clang/3.6.2"
  "Define the -resource-dir command line option for Clang compiler runs")
(defconstant +additional-arguments+
  #+target-os-darwin (vector
                      "-I/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.11.sdk/usr/include"
                      "-I/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include"
                      "-I/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/7.0.0/include"
                      "-I/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.11.sdk/System/Library/Frameworks")
  #-target-os-darwin (vector
                      #+(or)"-I/home/meister/local/gcc-4.8.3/include/c++/4.8.3"
                      #+(or)"-I/home/meister/local/gcc-4.8.3/include/c++/4.8.3/x86_64-redhat-linux"
                      #+(or)"-I/home/meister/local/gcc-4.8.3/include/c++/4.8.3/tr1"
                      #+(or)"-I/home/meister/local/gcc-4.8.3/lib/gcc/x86_64-redhat-linux/4.8.3/include"))


;;; --------------------------------------------------
;;; --------------------------------------------------
;;; Should not need to modify below here
;;; --------------------------------------------------
;;; --------------------------------------------------
(defmacro gclog (fmt &rest args))


;; ----------------------------------------------------------------------
;;
;; Housekeeping structs
;;
;; ----------------------------------------------------------------------


(define-condition unsupported-type (error)
  ((type :initarg :type :accessor unsupported-type)))

(defstruct debug-info
  name
  location
  )



;; ----------------------------------------------------------------------
;;
;; Class structs
;;
;; ----------------------------------------------------------------------

(defstruct cclass
  "Store info on a class "
  key
  template-specializer
  location
  bases
  vbases
  fields
  size
  method-names
  metadata
  )

(defclass instance-field ()
  ((access :initform :public :initarg :access :accessor instance-field-access)
   (ctype :initarg :ctype :accessor instance-field-ctype)))
(defclass instance-variable (instance-field)
  ((field-name :initarg :field-name :accessor instance-variable-field-name)
   (location :initarg :location :accessor instance-variable-location))
  (:documentation "Represent an instance variable, it's name, it's source-location and it's classified type"))

(defmethod print-object ((x instance-variable) stream)
  (format stream "#<~a :field-name ~a>" (class-name (class-of x)) (instance-variable-field-name x)))

(defclass instance-array-element (instance-field)
  ((index :initarg :index :accessor instance-array-element-index)))

(defgeneric instance-field-as-string (x first)
  (:documentation "Return a string that describes this instance-field"))
(defmethod instance-field-as-string ((x instance-variable) first)
  (format nil "~a~a"
          (if first "" ".")
          (instance-variable-field-name x)))

(defmethod instance-field-as-string ((x instance-array-element) first)
  (format nil "[~a]" (instance-array-element-index x)))

(defstruct alloc
  key
  name ;; decl name
  location
  ctype)

(defstruct (lispalloc (:include alloc)))
(defstruct (classalloc (:include alloc)))
(defstruct (rootclassalloc (:include alloc)))
(defstruct (containeralloc (:include alloc)))


(defclass class-layout ()
  ((layout-class :initarg :layout-class :accessor layout-class)
   (fixed-part :initarg :fixed-part :accessor fixed-part)
   (variable-part :initarg :variable-part :accessor variable-part)))

(defstruct enum
  key
  name
  value
  cclass
  species
  children
  in-hierarchy ;; only generate TaggedCast entry for those in hierarchy
  )

(defstruct (simple-enum (:include enum))
  alloc)

(defstruct (templated-enum (:include enum))
  all-allocs)




(defstruct variable
  location
  name
  ctype)

(defstruct (global-variable (:include variable)))
(defstruct (static-local-variable (:include variable)))
(defstruct (local-variable (:include variable)))
  


;; ----------------------------------------------------------------------
;;
;; Project and analysis classes
;;
;; ----------------------------------------------------------------------


(defstruct project
  "Store the results of matching to the entire codebase"
  ;; All class information
  (classes (make-hash-table :test #'equal))
  ;; Different allocs of objects(classes)
  (lispallocs (make-hash-table :test #'equal))   ; exposed to Lisp
  (classallocs (make-hash-table :test #'equal)) ; regular classes
  (rootclassallocs (make-hash-table :test #'equal)) ; regular root classes
  (containerallocs (make-hash-table :test #'equal)) ; containers
  (local-variables (make-hash-table :test #'equal))
  (global-variables (make-hash-table :test #'equal))
  (static-local-variables (make-hash-table :test #'equal))
;;  (new-gcobject-exprs (make-hash-table :test #'equal))
;;  (new-housekeeping-class-exprs (make-hash-table :test #'equal))
  )

(defun merge-projects (union one)
  (maphash (lambda (k v) (setf (gethash k (project-classes union)) v)) (project-classes one))
  (maphash (lambda (k v) (setf (gethash k (project-lispallocs union)) v)) (project-lispallocs one))
  (maphash (lambda (k v) (setf (gethash k (project-classallocs union)) v)) (project-classallocs one))
  (maphash (lambda (k v) (setf (gethash k (project-rootclassallocs union)) v)) (project-rootclassallocs one))
  (maphash (lambda (k v) (setf (gethash k (project-containerallocs union)) v)) (project-containerallocs one))
  (maphash (lambda (k v) (setf (gethash k (project-containerallocs union)) v)) (project-containerallocs one))
  (maphash (lambda (k v) (setf (gethash k (project-local-variables union)) v)) (project-local-variables one))
  (maphash (lambda (k v) (setf (gethash k (project-global-variables union)) v)) (project-global-variables one))
  (maphash (lambda (k v) (setf (gethash k (project-static-local-variables union)) v)) (project-static-local-variables one))
)


(defstruct analysis
  project
  manager
  inline
  (cur-enum-value (multiple-value-bind (hardwired-kinds ignore-classes first-general)
                      (core:hardwired-kinds)
                    first-general))
  (forwards (make-hash-table :test #'equal))
  (enums (make-hash-table :test #'equal))
  sorted-enums
  (scanner-jump-table-indices-for-enum-names (make-hash-table :test #'equal))
  enum-roots
  )

(defmethod print-object ((object analysis) stream)
  (format stream "#<analysis>"))

(defun scanner-jump-table-index-for-enum-name (enum-name anal)
  (let* ((jt (analysis-scanner-jump-table-indices-for-enum-names anal))
         (jt-index (gethash enum-name jt)))
    (if jt-index
        jt-index
        (setf (gethash enum-name jt) (hash-table-count jt)))))

;; Return "inline" if the function should be inlined
;; For now it's really simple, inline if it's in a list and don't if it's not
(defun inline-analysis (func key analysis)
  (if (member key (analysis-inline analysis) :test #'string=)
    "ALWAYS_INLINE"
    "MAYBE_INLINE"))

;; ----------------------------------------------------------------------
;;
;; Class hierarchy
;;
;; ----------------------------------------------------------------------


(defstruct hnode
  parent
  children )

(defun in-enums-p (name analysis)
  (multiple-value-bind (enum enum-p)
      (gethash name (analysis-enums analysis))
    enum-p))


(defun notify-base-names (class-name base-names analysis)
  (when (> (length base-names) 1)
    (format t "Class ~a has multiple bases: ~a - clasp doesn't support this~%" class-name base-names))
  (dolist (class-base-name base-names)
    (multiple-value-bind (parent-enum parent-enum-p)
        (gethash class-base-name (analysis-enums analysis))
      (when parent-enum-p
        (push class-name (enum-children parent-enum))
        #+(or)(format t "Not informing ~a that it has the child ~a because it is outside of the enum hierarchy~%" class-base-name class-name))
      )))
  
(defun notify-parents (class-name analysis)
  (let* ((project (analysis-project analysis))
         (class (gethash class-name (project-classes project)))
         (base-names (append (cclass-bases class) (cclass-vbases class))))
    (if (and base-names (not (string= class-name "core::T_O")))
        (notify-base-names class-name base-names analysis)
        (push class-name (analysis-enum-roots analysis)))))

(defun build-hierarchy (analysis)
  (maphash #'(lambda (node-name enum)
               (notify-parents node-name analysis))
           (analysis-enums analysis)))


(defun traverse (name analysis)
  (let ((enum (gethash name (analysis-enums analysis)))
        (enum-value (analysis-cur-enum-value analysis)))
    (setf (enum-value enum) enum-value)
    (setf (enum-in-hierarchy enum) t)
    (incf (analysis-cur-enum-value analysis))
    (dolist (child (enum-children enum))
      (traverse child analysis))))

(defun assign-enum-values-to-hierarchy (analysis)
  (build-hierarchy analysis)
  (setf (analysis-cur-enum-value analysis)
        (multiple-value-bind (hardwired-kinds ignore-classes first-general)
            (core:hardwired-kinds)
          first-general))
  (dolist (root (analysis-enum-roots analysis))
    (traverse root analysis)))

(defun assign-enum-values-to-those-without (analysis)
  (maphash (lambda (name enum)
             (when (eq (enum-value enum) :unassigned)
               (setf (enum-value enum) (analysis-cur-enum-value analysis))
               (setf (enum-in-hierarchy enum) nil)
               (incf (analysis-cur-enum-value analysis))))
           (analysis-enums analysis)))

(defun analyze-hierarchy (analysis)
  (assign-enum-values-to-hierarchy analysis)
  (assign-enum-values-to-those-without analysis))

(defun hierarchy-end-range (class-name analysis)
  (let ((enum (gethash class-name (analysis-enums analysis))))
    (if (null (enum-children enum))
        (enum-value enum)
      (hierarchy-end-range (car (last (enum-children enum))) analysis))))

        
(defun hierarchy-class-range (class-name analysis)
  (let ((enum (gethash class-name (analysis-enums analysis))))
    (values (enum-value enum) (hierarchy-end-range class-name analysis))))


(defun generate-dynamic-cast-code (fout analysis)
  (maphash (lambda (key enum) 
             (when (enum-in-hierarchy enum)
               (format fout "template <typename FP> struct Cast<~a*,FP> {~%" key )
               (format fout "  inline static bool isA(FP client) {~%" key)
               (format fout "      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));~%")
               (format fout "      int kindVal = header->kind();~%")
               (multiple-value-bind (low high)
                   (hierarchy-class-range key analysis)
                 (format fout "      // low high --> ~a ~a ~%" low high)
                 (if (eql low high)
                     (format fout "      return (kindVal == ~a);~%" low)
                     (format fout "      return ((~a <= kindVal) && (kindVal <= ~a));~%" low high)
                     ))
               (format fout "  };~%")
;;;               (format fout "  static ~a* castOrNULL(FP client) {~%" key)
;;;               (format fout "    if (TaggedCast<~a*,FP>::isA(client)) {~%" key)
;;;               (format fout "      return gctools::tag_general<~a*>(reinterpret_cast<~a*>(gctools::untag_general<FP>(client)));~%" key key)
;;;               (format fout "    }~%")
;;;               (format fout "    return NULL;~%")
;;;               (format fout "  };~%")
               (format fout "};~%")))
           (analysis-enums analysis)))


;; ----------------------------------------------------------------------
;;
;; Clang type classes
;;
;; ----------------------------------------------------------------------



(defstruct gc-template-argument 
  index
  ctype)


;; A ctype is holds the name of a C++ type
(defstruct ctype key)
(defstruct (simple-ctype (:include ctype)))
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

(defstruct (smart-ptr-ctype (:include ctype)) specializer)

(defstruct (tagged-pointer-ctype (:include ctype)) specializer)

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



(defstruct (container (:include class-template-specialization-ctype)))
(defstruct (gcvector-moveable-ctype (:include container)))
(defstruct (gcarray-moveable-ctype (:include container)))
(defstruct (gcstring-moveable-ctype (:include container)))





;; //////////////////////////////////////////////////////////////////////
;;
;; Compile linear layouts of classes
;;
;; //////////////////////////////////////////////////////////////////////

(defun linearize-code-for-field (field base analysis)
  (let ((code (ensure-list (linearize-class-layout-impl field base analysis))))
    (unless (listp code)
      (error "The result of linearize-class-layout-impl MUST be a LIST"))
    (cond
      ((null code) nil)
      ((listp code)
       (mapc (lambda (onecode) (push-prefix-field field onecode)) code)
       code)
      (t
       (error "Feb 2016 I inserted this error to see if the code ever gets here") code))))

(defun fix-code-for-field (field analysis)
  (let ((code (fixable-instance-variables-impl field analysis)))
    (cond
      ((null code) nil)
      ((atom code)
       (list (list field code)))
      ((listp code)
       (mapcar (lambda (f) (cons field f)) code))
      (t
       (error "Feb 2016 I inserted this error to see if the code ever gets here") code))))


(defclass offset ()
  ((offset-type :initarg :offset-type :accessor offset-type)
   (fields :initform nil :initarg :fields :accessor fields)
   (base :initarg :base :accessor base)))

(defmethod print-object ((x offset) stream)
  (format stream "#<~a :fields ~a>" (class-name (class-of x)) (fields x)))

(defclass copyable-offset (offset) ())
(defclass smart-ptr-offset (copyable-offset) ())
(defclass tagged-pointer-offset (copyable-offset) ())
(defclass pointer-offset (copyable-offset) ())
(defclass pod-offset (copyable-offset) ())

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
  ((element-type :initarg :element-type :accessor element-type)
   (elements :initarg :elements :accessor elements)))

(defclass constant-array-offset (array-offset)
  ((constant-array-size :initarg :constant-array-size :accessor constant-array-size)))

(defclass container-offset (offset)
  ((fixed-fields :initarg :fixed-fields :accessor fixed-fields)
   (elements-base :initarg :elements-base :accessor elements-base)
   (elements :initarg :elements :accessor elements)))

(defclass gcarray-offset (container-offset) ())
(defclass gcvector-offset (container-offset) ())
(defclass gcstring-offset (container-offset) ())

(defun c++identifier (str)
  "* Arguments
- str :: A string.
* Description 
Convert the string into a C++ identifier, convert spaces, dashes and colons to underscores"
  (let ((cid (make-array (length str) :element-type 'character)))
    (loop :for i :below (length str)
       :for x = (elt str i)
       :do (if (or (eql x #\space)
                   (eql x #\:)
                   (eql x #\-))
               (setf (elt cid i) #\_)
               (setf (elt cid i) x)))
    cid))

(defmethod offset-type-c++-identifier ((x offset))
  (c++identifier (string (class-name (class-of x)))))

(defmethod offset-ctype ((x offset))
  (ctype-key (offset-type x)))

(defmethod offset-base-ctype ((x offset))
  (cclass-key (base x)))

(defmethod layout-offset-field-names ((off offset))
  "* Arguments
- off :: An offset.
* Description
Generate a list of strings that represent nested field names for the offset."
  (loop :for x :in (fields off)
     :for index :below (length (fields off))
     :collect (instance-field-as-string x (= index 0))))


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
  

(defun codegen-variable-part (stream variable-fields analysis)
    (let* ((array (offset-field-with-name variable-fields "_Data"))
           (capacity (offset-field-with-name variable-fields "_Capacity"))
           (end (or (offset-field-with-name variable-fields "_End") capacity)))
      (unless capacity
        (error "Could not find _Capacity in the variable-fields: ~a with names: ~a of ~a" variable-fields (variable-part-offset-field-names variable-fields) (mapcar (lambda (x) (offset-type-c++-identifier x)) variable-fields)))
      (format stream "{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(~a),~{~a~}), \"~{~a~}\" },~%"
              (offset-base-ctype array)
              (layout-offset-field-names array)
              (layout-offset-field-names array))
      (format stream "{  variable_capacity, sizeof(~a), offsetof(SAFE_TYPE_MACRO(~a),~{~a~}), offsetof(SAFE_TYPE_MACRO(~a),~{~a~}), NULL },~%"
              (maybe-fixup-type (ctype-key (element-type array)) (offset-base-ctype array))
              (offset-base-ctype array)
              (layout-offset-field-names end)
              (offset-base-ctype array)
              (layout-offset-field-names capacity))
      (dolist (one (elements array))
        (let ((field-names (layout-offset-field-names one)))
            (if field-names
                (let* ((fixable (fixable-instance-variables (car (last (fields one))) analysis))
                       (public (mapcar (lambda (iv) (eq (instance-field-access iv) 'ast-tooling:as-public)) (fields one)))
                       (all-public (every #'identity public))
                       (good-name (not (is-bad-special-case-variable-name (layout-offset-field-names one))))
                       (expose-it (and (or all-public fixable) good-name))
                       (*print-pretty* nil))
                  (format stream "~a {    variable_field, ~a, sizeof(~a), offsetof(SAFE_TYPE_MACRO(~a),~{~a~}), \"~{~a~}\" }, // public: ~a fixable: ~a good-name: ~a~%"
                          (if expose-it "" "// not-exposed-yet ")
                          (offset-type-c++-identifier one)
                          (maybe-fixup-type (ctype-key (offset-type one)) (ctype-key (base one)))
                          (ctype-key (base one))
                          (layout-offset-field-names one)
                          (layout-offset-field-names one)
                          public
                          fixable
                          good-name))
                  (format stream "{    variable_field, ~a, sizeof(~a), 0, \"only\" },~%"
                          (offset-type-c++-identifier one)
                          (maybe-fixup-type (ctype-key (element-type array)) (offset-base-ctype array))
                          ;;                      (maybe-fixup-type (ctype-key (offset-type one)) (ctype-key (base one)))
                          (ctype-key (base one))))))))



(defun codegen-full (stream layout analysis)
  (dolist (one (fixed-part layout))
    (let* ((fixable (fixable-instance-variables (car (last (fields one))) analysis))
           (public (mapcar (lambda (iv) (eq (instance-field-access iv) 'ast-tooling:as-public)) (fields one)))
           (all-public (every #'identity public))
           (good-name (not (is-bad-special-case-variable-name (layout-offset-field-names one))))
           (expose-it (and (or all-public fixable) good-name))
           (base (base one)) ;; The outermost class that contains this offset
           (*print-pretty* nil))
      (format stream "~a {  fixed_field, ~a, sizeof(~a), offsetof(SAFE_TYPE_MACRO(~a),~{~a~}), \"~{~a~}\" }, // public: ~a fixable: ~a good-name: ~a~%"
              (if expose-it "" "// not-exposing")
              (offset-type-c++-identifier one)
              (or (offset-ctype one) "UnknownType")
              (offset-base-ctype one)
              (layout-offset-field-names one)
              (layout-offset-field-names one)
              public
              fixable
              good-name)))
  (let* ((variable-part (variable-part layout)))
    (when variable-part
      (codegen-variable-part stream (fixed-fields variable-part) analysis))))


(defun codegen-lisp-layout (stream enum key layout analysis)
  (format stream "{ class_kind, ~a, sizeof(~a), 0, \"~a\" },~%" enum key key )
  (codegen-full stream layout analysis))

(defun codegen-container-layout (stream enum key layout analysis)
  (format stream "{ container_kind, ~a, sizeof(~a), 0, \"~a\" },~%" enum key key )
  (codegen-variable-part stream (fixed-part layout) analysis))

(defun codegen-templated-layout (stream enum key layout analysis)
  (format stream "{ templated_kind, ~a, sizeof(~a), 0, \"~a\" },~%" enum key key )
  (codegen-full stream layout analysis))


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
           (arg0 (loop :for arg :in arguments
                    :when (eq (gc-template-argument-index arg) 0)
                    :return arg))
           (arg0-ctype (gc-template-argument-ctype arg0)))
      #+(or)(fixed (linearize-code-for-field (gethash (gcarray-moveable-ctype-key x) (project-classes (analysis-project analysis))) base analysis))
      (list (make-instance 'gcarray-offset
                           :base base
                           :fixed-fields nodes
                           :offset-type arg0-ctype
                           :elements-base arg0-ctype
                           :elements (ensure-list (linearize-class-layout-impl arg0-ctype arg0-ctype analysis)))))))

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
  (if (ignorable-ctype-p x)
      nil
      (list (make-instance 'pod-offset
                           :base base
                           :offset-type x))))

(defmethod linearize-class-layout-impl ((x lvalue-reference-ctype) base analysis)
  nil)

(defmethod linearize-class-layout-impl ((x enum-ctype) base analysis)
  nil)
(defmethod linearize-class-layout-impl ((x builtin-ctype) base analysis)
  (if (ignorable-ctype-p x)
      nil
      (list (make-instance 'pod-offset
                           :base base
                           :offset-type x))))

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
  (list (make-instance 'smart-ptr-offset :base base :offset-type x)))

(defmethod linearize-class-layout-impl ((x tagged-pointer-ctype) base analysis)
  (list (make-instance 'tagged-pointer-offset :base base :offset-type x)))

(defun linearize-constant-array-contents (array element-type elements base analysis)
  "* Arguments
- array :: A constant-array-ctype.
- element-type :: A ctype.
- elements :: A list of the fields to expose in element-type.
- base :: The ctype that contains the array.
- analysis :: An analysis object.
* Description
Generate offsets for every array element that exposes the fields in elements."
  (let ((number-of-elements (constant-array-ctype-array-size array))
        entries)
    (loop :for index :below number-of-elements
       :append (loop :for element :in elements
                  :collect (let ((element-copy (copy-offset element)))
                             (push-prefix-field (make-instance 'instance-array-element
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
            (linearize-constant-array-contents x element-type elements base analysis)
            nil)
        (list (make-instance 'constant-array-offset
                             :base base
                             :element-type element-type
                             :elements elements
                             :constant-array-size (constant-array-ctype-array-size x)
                             :offset-type x)))))


(defvar *x*)
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
           (gcarray-moveable-ctype-p pointee)
           (gcstring-moveable-ctype-p pointee))
       (list (make-instance 'pointer-offset :base base :offset-type x)))
      ((is-alloc-p (pointer-ctype-pointee x) (analysis-project analysis))
       (list (make-instance 'pointer-offset :base base :offset-type x)))
      ((fixable-pointee-p (pointer-ctype-pointee x))
       (list (make-instance 'pointer-offset :base base :offset-type x)))
      ((ignorable-ctype-p (pointer-ctype-pointee x)) nil)
      (t
       ;;(warn "I'm not sure if I can ignore pointer-ctype ~a  ELIMINATE THESE WARNINGS" x)
       nil))))

;; \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\






#|
(defmethod container-argument ((x gccontainer) idx)
  (dolist (arg (gccontainer-arguments x))
    (when (eql (gc-template-argument-index arg) idx) (return arg))))

(defmethod container-argument ((x container) idx)
  (dolist (arg (container-arguments x))
    (when (eql (gc-template-argument-index arg) idx) (return arg))))
|#






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
      (ast-tooling:type
;;       (cast:get-as-string (cast:get-as-type template-arg))
       (record-key (cast:get-type-ptr-or-null (cast:get-as-type template-arg)))
       )
      (ast-tooling:integral
       (llvm:to-string (cast:get-as-integral template-arg) 10 t))
      (ast-tooling:pack
       (let ((pack-size (cast:pack-size template-arg)))
         (if (eql pack-size 0)
             ""
             (format nil "TEMPLATE_ARG_PACK_SIZE~a" pack-size))))
      (ast-tooling:template
       "TEMPLATE_ARG_AS_STRING::TEMPLATE")
      (ast-tooling:expression
       "TEMPLATE_ARG_AS_STRING::EXPRESSION")
      (ast-tooling:declaration
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
     (let* ((decl-name (safe-get-name-decl decl-node)) 
            (template-args (cast:get-template-args decl-node))
            (template-args-as-list (loop :for i :from 0 :below (cast:size template-args)
                                        :collect (let* ((template-arg (cast:template-argument-list-get template-args i)))
                                                   (template-arg-as-string template-arg)))))
       (values (format nil "~a<~{~a~^,~}>" (decl-name decl-node) template-args-as-list) t)))
    (cast:class-template-partial-specialization-decl
     (let* ((decl-name (decl-name decl-node))
            (template-args (cast:get-template-args decl-node))
            (template-args-as-list (loop :for i :from 0 :below (cast:size template-args)
                                      :for template-arg = (cast:template-argument-list-get template-args i)
                                      :for type-name = (template-arg-as-string template-arg)
                                      :collect type-name)))
       (values (format nil "~a<~{~a~^,~}>" (decl-name decl-node) template-args-as-list) t)))
    (otherwise
     (format t "Add support for record-key for ~a  get-name->~a" decl-node (decl-name decl-node))
     #+use-breaks(break "Check the decl-node")
     )))


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
             classified)
        (case template-arg-kind
          (ast-tooling:type
           (let* ((qtarg (cast:get-as-type arg))
                  (tsty-new (cast:get-type-ptr-or-null qtarg)))
             (gclog "classify-template-arg: ~a~%" (cast:get-as-string qtarg))
             (setq classified (classify-ctype tsty-new))))
          (otherwise
           #+use-breaks(break "Handle template-arg-kind: ~a" template-arg-kind)))
        (gclog "classified2 = ~a~%" classified)
        (push (make-gc-template-argument :index i :ctype classified) args)))
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
           ((string= name "smart_ptr")
            (assert (eql (cast:size args) 1) nil "Must have 1 argument")
            (let* ((arg (cast:template-argument-list-get args 0))
                   (qtarg (cast:get-as-type arg)))
              (gclog "          Found a smart_ptr~%")
              (make-smart-ptr-ctype :key decl-key :specializer (cast:get-as-string qtarg))))
           ((string= name "tagged_pointer")
            (assert (eql (cast:size args) 1) nil "Must have 1 argument")
            (let* ((arg (cast:template-argument-list-get args 0))
                   (qtarg (cast:get-as-type arg)))
              (gclog "          Found a tagged_pointer~%")
              (make-tagged-pointer-ctype :key decl-key :specializer (cast:get-as-string qtarg))))
           ((string= name "GCVector_moveable")
            (let* ((arg (cast:template-argument-list-get args 0))
                   (qtarg (cast:get-as-type arg)))
              (gclog "          Found a smart_ptr~%")
              (make-gcvector-moveable-ctype :key decl-key :name name :arguments (classify-template-args decl))))
           ((string= name "GCArray_moveable")
            (let* ((arg (cast:template-argument-list-get args 0))
                   (qtarg (cast:get-as-type arg)))
              (gclog "          Found a smart_ptr~%")
              (make-gcarray-moveable-ctype :key decl-key :name name :arguments (classify-template-args decl))))
           ((string= name "GCString_moveable")
            (let* ((arg (cast:template-argument-list-get args 0))
                   (qtarg (cast:get-as-type arg)))
              (make-gcstring-moveable-ctype :key decl-key :name name :arguments (classify-template-args decl))))
           (t
            #+gc-warnings(warn "classify-ctype cast:record-type unhandled class-template-specialization-decl  key = ~a  name = ~a~%IGNORE-NAME ~a~%IGNORE-KEY ~a" decl-key name name decl-key)
            (make-class-template-specialization-ctype :key decl-key 
                                                      :name name
                                                      :arguments (classify-template-args decl)
                                                      )))))
      (cast:cxxrecord-decl
       (make-cxxrecord-ctype :key decl-key :name name))
      (otherwise
       (warn "Add support for classify-ctype ~a" decl-key)
       (make-unknown-ctype :key decl-key)
       ))))
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
#||
  (make-typedef-ctype :desugared (classify-ctype (cast:get-type-ptr-or-null (cast:desugar x)))))
||#


(defmethod classify-ctype ((x cast:elaborated-type))
  (let* ((canonical-qual-type (cast:get-canonical-type-internal x))
         (canonical-type (cast:get-type-ptr-or-null canonical-qual-type))
         )
    (classify-ctype canonical-type)))
#||
    (break "Check out canonical-qual-type and canonical-type, compare to x"))
  (let* ((named-qual-type (cast:get-named-type x))
         (real-type (cast:get-type-ptr-or-null named-qual-type)))
    (make-elaborated-ctype :named-type (classify-ctype real-type))))
||#

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
  (make-paren-ctype :inner (classify-ctype (cast:get-type-ptr-or-null (cast:get-inner-type x)))))

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
  (let ((*print-readably* t)
        (*print-array* t)
        (*print-circle* t)
        (*print-width* 140)
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

(defun load-project (db)
  (clang-tool:with-compilation-tool-database db
    (let ((project (load-data (project-pathname "project" "dat"))))
      (setq *project* project)
      project)))
        



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


(defparameter *metadata-submatcher-sexp*
   '(:cxxrecord-decl
     (:for-each ;; -descendant
      (:cxxrecord-decl
       (:matches-name "metadata_.*")
       (:bind :metadata (:cxxrecord-decl))))))
(defparameter *metadata-submatcher*
  (clang-tool:compile-matcher *metadata-submatcher-sexp*))

(defparameter *method-submatcher-sexp*
  '(:cxxrecord-decl
    (:for-each ;; -descendant
     (:cxxmethod-decl
      (:bind :method (:cxxmethod-decl))))))
(defparameter *method-submatcher* 
  (clang-tool:compile-matcher *method-submatcher-sexp*))


(defun setup-cclass-search (mtool)
  (symbol-macrolet ((results (project-classes (clang-tool:multitool-results mtool))))
    (labels ((%%new-class-callback (match-info class-node record-key template-specializer)
               (let ((cname (clang-tool:mtag-name match-info :whole))
                     (record-decl (clang-tool:mtag-node match-info :whole))
                     bases vbases fields method-names metadata )
                 ;;
                 ;; Run a matcher to find the base classes and their namespaces
                 ;;
                 (ext:do-c++-iterator (it (cast:bases-iterator class-node))
                   (let* ((qty (cast:get-type it))
                          (canonical-qty (cast:get-canonical-type-internal (cast:get-type-ptr-or-null qty)))
                          (base-decl (cast:get-as-cxxrecord-decl (cast:get-type-ptr-or-null canonical-qty))))
                     (when base-decl
                       (push (record-key base-decl) bases))))
                 (ext:do-c++-iterator (it (cast:vbases-iterator class-node))
                   (let* ((qty (cast:get-type it))
                          (canonical-qty (cast:get-canonical-type-internal (cast:get-type-ptr-or-null qty)))
                          (base-decl (cast:get-as-cxxrecord-decl (cast:get-type-ptr-or-null canonical-qty))))
                     (when base-decl
                       (push (record-key base-decl) vbases))))
                 ;;
                 ;; Run a matcher to find the GC-scannable fields of this class
                 ;;
                 (clang-tool:sub-match-run
                  *field-submatcher*
                  *field-submatcher-sexp*
                  (clang-tool:mtag-node match-info :whole)
                  (clang-tool:ast-context match-info)
                  (lambda (minfo)
                    (let* ((field-node (clang-tool:mtag-node minfo :field))
                           (type (to-canonical-type (cast:get-type field-node))))
                      (gclog "      >> Field: ~30a~%" field-node)
                      (handler-case
                          (push (make-instance 'instance-variable
                                 :location (clang-tool:mtag-loc-start minfo :field)
                                 :field-name (clang-tool:mtag-name minfo :field)
                                 :access (clang-ast:get-access (clang-tool:mtag-node minfo :field))
                                 :ctype (let ((*debug-info* (make-debug-info :name (clang-tool:mtag-name minfo :field)
                                                                             :location (clang-tool:mtag-loc-start minfo :field))))
                                          (classify-ctype (to-canonical-type type))))
                                fields)
                        (unsupported-type (err)
                          (error "Add support for classifying type: ~a (type-of type): ~a  source: ~a"
                                 type (type-of type) (clang-tool:mtag-source minfo :field)))))))
                 ;;
                 ;; Run a matcher to find the scanGCRoot functions
                 ;;
                 (clang-tool:sub-match-run
                  *method-submatcher*
                  *method-submatcher-sexp*
                  (clang-tool:mtag-node match-info :whole)
                  (clang-tool:ast-context match-info)
                  (lambda (minfo)
                    (let* ((method-node (clang-tool:mtag-node minfo :method))
                           (location (clang-tool:mtag-loc-start minfo :method))
                           (method-name (clang-tool:mtag-name minfo :method)))
                      (gclog "      >> Method: ~30a~%" (clang-tool:mtag-source minfo :method))
                      (push method-name method-names))))
                 (clang-tool:sub-match-run
                  *metadata-submatcher*
                  *metadata-submatcher-sexp*
                  class-node
                  (clang-tool:ast-context match-info)
                  (lambda (minfo)
                    (let* ((metadata-node (clang-tool:mtag-node minfo :metadata))
                           (metadata-name (string-upcase (clang-tool:mtag-name minfo :metadata))))
                      (push (intern metadata-name :keyword) metadata))))
                 ;;                   (when (string= record-key "gctools::StackRootedPointer<class asttooling::BAR>") (break "Check fields"))
                 (when (search "gctools::GCVector_moveable<chem::(anonymous)>" record-key)
                   (break "Check (clang-tool:mtag-node :whole)"))
                 (setf (gethash record-key results)
                       (make-cclass :key record-key
                                    :template-specializer template-specializer
                                    :location (clang-tool:mtag-loc-start match-info :whole)
                                    :bases bases
                                    :vbases vbases
                                    :method-names method-names
                                    :metadata metadata
                                    :fields fields))))
             (%%class-callback (match-info)
               (gclog "MATCH: ------------------~%")
               (gclog "    Start:~%~a~%" (clang-tool:mtag-loc-start match-info :whole))
               (gclog "    Name: ~a~%" (clang-tool:mtag-name match-info :whole))
               (let* ((class-node (clang-tool:mtag-node match-info :whole)))
                 (multiple-value-bind (record-key template-specializer)
                     (record-key class-node)
                   (unless (or (typep class-node 'cast:class-template-partial-specialization-decl) ; ignore partial specializations
                               (and (typep class-node 'cast:class-template-specialization-decl) ; ignore template specializations that have undeclared specialization alloc
                                    (eq (cast:get-specialization-kind class-node) 'ast-tooling:tsk-undeclared))
                               (gethash record-key results)) ; ignore if we've seen it before
                     (%%new-class-callback match-info class-node record-key template-specializer))))))
      (clang-tool:multitool-add-matcher mtool
                             :name :cclasses
                             :matcher-sexp *class-matcher*
                             :initializer (lambda () (setf results (make-hash-table :test #'equal)))
                             :callback (make-instance 'clang-tool:code-match-callback :match-code (function %%class-callback))))))




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
    (:has-name "GCObjectAllocator"))
  )

(defun setup-lispalloc-search (mtool)
  "Setup the TOOL (multitool) to look for instance variables that we want to garbage collect
and the inheritance hierarchy that the garbage collector will need"
  (symbol-macrolet ((class-results (project-lispallocs (clang-tool:multitool-results mtool))))
    (flet ((%%lispalloc-matcher-callback (match-info)
             "This function can only be called as a ASTMatcher callback"
             (let* ((decl (clang-tool:mtag-node match-info :whole))
                    (args (cast:get-template-args decl))
                    (arg (cast:template-argument-list-get args 0))
                    (qtarg (cast:get-as-type arg))
                    (tsty-new (cast:get-type-ptr-or-null qtarg))
                    (class-key (record-key tsty-new))
                    (classified (classify-ctype tsty-new))
                    (class-location (clang-tool:mtag-loc-start match-info :whole))
                    (arg-decl (cast:get-decl tsty-new)) ;; Should I convert to canonical type?????
                    (arg-location (clang-tool:source-loc-as-string match-info (get-loc-start arg-decl)))
                    (arg-name (cast:get-name arg-decl)))
               (unless (gethash class-key class-results)
                 (gclog "Adding class name: ~a~%" class-name)
;;                 (break "Check locations")
                 (let ((lispalloc (make-lispalloc :key class-key
                                                  :name arg-name ;; XXXXXX (clang-tool:mtag-name :whole)
                                                  :location arg-location ;; class-location
                                                  :ctype classified)))
                   (setf (gethash class-key class-results) lispalloc))))))
      ;; Initialize the class search
      (clang-tool:multitool-add-matcher mtool
                             :name :lispallocs
                             :matcher-sexp `(:bind :whole ,*lispalloc-matcher*)
                             :initializer (lambda () (setf class-results (make-hash-table :test #'equal))) ; initializer
                             :callback (make-instance 'clang-tool:code-match-callback :match-code (function %%lispalloc-matcher-callback))))))




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
    (:has-name "ClassAllocator"))
  )

(defun setup-classalloc-search (mtool)
  "Setup the TOOL (multitool) to look for instance variables that we want to garbage collect
and the inheritance hierarchy that the garbage collector will need"
  (symbol-macrolet ((class-results (project-classallocs (clang-tool:multitool-results mtool))))
    (flet ((%%classalloc-matcher-callback (match-info)
             "This function can only be called as a ASTMatcher callback"
             (let* ((decl (clang-tool:mtag-node match-info :whole))
                    (args (cast:get-template-args decl))
                    (arg (cast:template-argument-list-get args 0))
                    (qtarg (cast:get-as-type arg))
                    (tsty-new (cast:get-type-ptr-or-null qtarg))
                    (class-key (record-key tsty-new))
                    (classified (classify-ctype tsty-new))
                    (class-location (clang-tool:mtag-loc-start match-info :whole))
                    (arg-decl (cast:get-decl tsty-new)) ;; Should I convert to canonical type?????
                    (arg-location (clang-tool:source-loc-as-string match-info (get-loc-start arg-decl)))
                    (arg-name (cast:get-name arg-decl)))
               (unless (gethash class-key class-results)
                 (gclog "Adding class name: ~a~%" class-name)
                 (let ((classalloc (make-classalloc :key class-key
                                                  :name arg-name ;;(clang-tool:mtag-name :whole)
                                                  :location arg-location ;;class-location
                                                  :ctype classified)))
                   (setf (gethash class-key class-results) classalloc))))))
      ;; Initialize the class search
      (clang-tool:multitool-add-matcher mtool
                             :name :classallocs
                             :matcher-sexp `(:bind :whole ,*classalloc-matcher*)
                             :initializer (lambda () (setf class-results (make-hash-table :test #'equal))) ; initializer
                             :callback (make-instance 'clang-tool:code-match-callback :match-code (function %%classalloc-matcher-callback))))))





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
    (:has-name "RootClassAllocator"))
  )

(defun setup-rootclassalloc-search (mtool)
  "Setup the TOOL (multitool) to look for instance variables that we want to garbage collect
and the inheritance hierarchy that the garbage collector will need"
  (symbol-macrolet ((class-results (project-rootclassallocs (clang-tool:multitool-results mtool))))
    (flet ((%%rootclassalloc-matcher-callback (match-info)
             "This function can only be called as a ASTMatcher callback"
             (let* ((decl (clang-tool:mtag-node match-info :whole))
                    (args (cast:get-template-args decl))
                    (arg (cast:template-argument-list-get args 0))
                    (qtarg (cast:get-as-type arg))
                    (tsty-new (cast:get-type-ptr-or-null qtarg))
                    (class-key (record-key tsty-new))
                    (classified (classify-ctype tsty-new))
                    (class-location (clang-tool:mtag-loc-start match-info :whole))
                    (arg-decl (cast:get-decl tsty-new)) ;; Should I convert to canonical type?????
                    (arg-location (clang-tool:source-loc-as-string match-info (get-loc-start arg-decl)))
                    (arg-name (cast:get-name arg-decl)))
               (unless (gethash class-key class-results)
                 (gclog "Adding class name: ~a~%" class-name)
                 (let ((rootclassalloc (make-rootclassalloc :key class-key
                                                  :name arg-name ;;(clang-tool:mtag-name :whole)
                                                  :location arg-location ;;class-location
                                                  :ctype classified)))
                   (setf (gethash class-key class-results) rootclassalloc))))))
      ;; Initialize the class search
      (clang-tool:multitool-add-matcher mtool
                             :name :rootclassallocs
                             :matcher-sexp `(:bind :whole ,*rootclassalloc-matcher*)
                             :initializer (lambda () (setf class-results (make-hash-table :test #'equal))) ; initializer
                             :callback (make-instance 'clang-tool:code-match-callback :match-code (function %%rootclassalloc-matcher-callback))))))

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
             "This function can only be called as a ASTMatcher callback"
             (let* ((decl (clang-tool:mtag-node match-info :whole))
                    (args (cast:get-template-args decl))
                    (arg (cast:template-argument-list-get args 0))
                    (qtarg (cast:get-as-type arg))
                    (tsty-new (cast:get-type-ptr-or-null qtarg))
                    (class-key (record-key decl))
                    (classified (classify-decl decl))
                    (class-location (clang-tool:mtag-loc-start match-info :whole)))
               (unless (gethash class-key class-results)
                 (let ((containeralloc (make-containeralloc :key class-key
                                                            :name (clang-tool:mtag-name match-info :whole)
                                                            :location class-location
                                                            :ctype classified)))
                   (setf (gethash class-key class-results) containeralloc))))))
      ;; Initialize the class search
      (clang-tool:multitool-add-matcher mtool
                             :name :containerallocs
                             :matcher-sexp `(:bind :whole ,*containeralloc-matcher*)
                             :initializer (lambda () (setf class-results (make-hash-table :test #'equal))) ; initializer
                             :callback (make-instance 'clang-tool:code-match-callback :match-code (function %%containeralloc-matcher-callback))))))


;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;;
;; Global and static variable matchers
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------


(defparameter *global-variable-matcher*
  '(:bind :whole
    (:var-decl
     (:is-definition)
     (:unless (:has-ancestor (:function-decl)))
     (:unless (:parm-var-decl))
     (:unless
         (:matches-name ".*_gc_safe")))))


(clang-tool:compile-matcher *global-variable-matcher*)


(defun setup-global-variable-search (mtool)
  (symbol-macrolet ((global-variables (project-global-variables (clang-tool:multitool-results mtool))))
    ;; The matcher is going to match static locals and locals as well as globals - so we need to explicitly recognize and ignore them
    (flet ((%%global-variable-callback (match-info)
             (block matcher
               (gclog "VARIABLE MATCH: ------------------~%")
               (gclog "    Start:~%~a~%" (clang-tool:mtag-loc-start match-info :whole))
               (gclog "    Name: ~a~%" (clang-tool:mtag-name match-info :whole))
               (gclog "    namespace: ~a~%" (clang-tool:mtag-name match-info :ns))
               (let* ((var-node (clang-tool:mtag-node match-info :whole))
                      (varname (decl-name var-node))
                      (location (clang-tool:mtag-loc-start match-info :whole))
                      (var-kind (cond
                                  ((and (cast:has-global-storage var-node) (not (cast:is-static-local var-node))) :global)
                                  ((and (cast:has-global-storage var-node) (cast:is-static-local var-node)) :static-local)
                                  (t :local)))
                      (hash-table (ecase var-kind
                                    (:global global-variables)
                                    (:static-local nil)
                                    (:local nil)))
                      (key (format nil "~a@~a" varname location)))
                 (unless (and hash-table (gethash key hash-table))
                   (let* ((qtype (cast:get-type var-node))
                          (type (cast:get-type-ptr-or-null qtype))
                          (classified-type (let ((*debug-info* (make-debug-info :name varname :location location)))
                                             (classify-ctype (to-canonical-type type)))))
                     (when (eq var-kind :global)
                       (setf (gethash key hash-table)
                             (make-global-variable :location location
                                                   :name varname
                                                   :ctype classified-type)))))))))
      (clang-tool:multitool-add-matcher mtool
                             :name :global-variables
                             :matcher-sexp *global-variable-matcher*
                             :initializer (lambda () (setf global-variables (make-hash-table :test #'equal)))
                             :callback (make-instance 'clang-tool:code-match-callback :match-code (function %%global-variable-callback)))))) 

(defparameter *variable-matcher*
  '(:bind :whole
    (:var-decl
     (:is-definition)
     (:has-ancestor
      (:function-decl
       (:bind :function (:function-decl))))
     (:unless (:parm-var-decl))
     (:unless
         (:matches-name ".*_gc_safe"))))) 

(clang-tool:compile-matcher *variable-matcher*)

(defun setup-variable-search (mtool)
  (symbol-macrolet ((static-local-variables (project-static-local-variables (clang-tool:multitool-results mtool)))
                    (local-variables (project-local-variables (clang-tool:multitool-results mtool))))
    ;; The matcher is going to match globals as well as static locals and locals - so we need to explicitly recognize and ignore globals
    (flet ((%%variable-callback (match-info)
             (block matcher
               (gclog "VARIABLE MATCH: ------------------~%")
               (gclog "    Start:~%~a~%" (clang-tool:mtag-loc-start match-info :whole))
               (gclog "    Name: ~a~%" (clang-tool:mtag-name match-info :whole))
               (gclog "    namespace: ~a~%" (clang-tool:mtag-name match-info :ns))
               (let* ((var-node (clang-tool:mtag-node match-info :whole))
                      (varname (decl-name var-node))
                      (location (clang-tool:mtag-loc-start match-info :whole))
                      (var-kind (cond
                                  ((and (cast:has-global-storage var-node) (not (cast:is-static-local var-node))) :global)
                                  ((and (cast:has-global-storage var-node) (cast:is-static-local var-node)) :static-local)
                                  (t :local)))
                      (hash-table (ecase var-kind
                                    (:global nil)
                                    (:static-local static-local-variables)
                                    (:local local-variables)))
                      (key (format nil "~a@~a" varname location)))
                 (unless (and hash-table (gethash key hash-table))
                   (let* ((qtype (cast:get-type var-node))
                          (type (cast:get-type-ptr-or-null qtype))
                          (classified-type (let ((*debug-info* (make-debug-info :name varname :location location)))
                                             (classify-ctype (to-canonical-type type)))))
                     (ecase var-kind
                       (:global nil) ;; not recognized here - see setup-global-variable-search
                       (:static-local
                        (setf (gethash key hash-table)
                              (make-static-local-variable :location location
                                                          :name varname
                                                          :ctype classified-type)))
                       (:local
                        (unless classified-type
                          #+use-breaks(break "classified-type is nil"))
                        (setf (gethash key hash-table)
                              (make-local-variable :location location
                                                   :name varname
                                                   :ctype classified-type))))))))))
      (clang-tool:multitool-add-matcher mtool
                             :name :variables
                             :matcher-sexp *variable-matcher*
                             :initializer (lambda ()
                                            (setf static-local-variables (make-hash-table :test #'equal))
                                            (setf local-variables (make-hash-table :test #'equal)))
                             :callback (make-instance 'clang-tool:code-match-callback :match-code (function %%variable-callback)))))) 

;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;;
;; GCInfo matcher
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------




(defparameter *gcinfo-matcher*
  '(:cxxrecord-decl
    (:is-definition)
    (:is-template-instantiation)
    (:has-name "GCInfo"))
  )

(defun setup-gcinfo-search (mtool)
  "Setup the TOOL (multitool) to look for instance variables that we want to garbage collect
and the inheritance hierarchy that the garbage collector will need"
  (symbol-macrolet ((class-results (project-gcinfos (clang-tool:multitool-results mtool))))
    (flet ((%%gcinfo-matcher-callback (match-info)
             "This function can only be called as a ASTMatcher callback"
             (let* ((decl (clang-tool:mtag-node match-info :whole))
                    (args (cast:get-template-args decl))
                    (arg (cast:template-argument-list-get args 0))
                    (qtarg (cast:get-as-type arg))
                    (tsty-new (cast:get-type-ptr-or-null qtarg))
                    (class-key (record-key tsty-new))
                    (classified (classify-ctype tsty-new))
                    (class-location (clang-tool:mtag-loc-start match-info :whole))
                    (arg-decl (cast:get-decl tsty-new)) ;; Should I convert to canonical type?????
                    (arg-location (clang-tool:source-loc-as-string match-info (get-loc-start arg-decl)))
                    (arg-name (cast:get-name arg-decl)))
               (unless (gethash class-key class-results)
                 (gclog "Adding class name: ~a~%" class-name)
                 (let ((gcinfo (make-gcinfo :key class-key
                                            :name arg-name ;;(clang-tool:mtag-name :whole)
                                            :location arg-location ;;class-location
                                            :ctype classified)))
                   (setf (gethash class-key class-results) gcinfo))))))
      ;; Initialize the class search
      (clang-tool:multitool-add-matcher mtool
                             :name :gcinfos
                             :matcher-sexp `(:bind :whole ,*gcinfo-matcher*)
                             :initializer (lambda () (setf class-results (make-hash-table :test #'equal))) ; initializer
                             :callback (make-instance 'clang-tool:code-match-callback :match-code (function %%gcinfo-matcher-callback)))))) 

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

(defmethod contains-fixptr-impl-p ((x injected-class-name-ctype) project) nil)
(defmethod contains-fixptr-impl-p ((x unclassified-ctype) project) nil)
(defmethod contains-fixptr-impl-p ((x builtin-ctype) project) nil)
(defmethod contains-fixptr-impl-p ((x enum-ctype) project) nil)
(defmethod contains-fixptr-impl-p ((x lvalue-reference-ctype) project) nil)
(defmethod contains-fixptr-impl-p ((x function-proto-ctype) project) nil)
(defmethod contains-fixptr-impl-p ((x unclassified-template-specialization-ctype) project) nil)
(defmethod contains-fixptr-impl-p ((x smart-ptr-ctype) project) t)
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

(defmethod contains-fixptr-impl-p ((x tagged-pointer-ctype) project) t)
(defmethod contains-fixptr-impl-p ((x pointer-ctype) project)
  (cond
    ((container-p (pointer-ctype-pointee x)) t)
    ((string= (ctype-key (pointer-ctype-pointee x)) "gctools::GCString_moveable<char>" ) t)
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
  (warn "expand-forwards-with-template-arguments most general  alloc-ctype--> ~a~%" alloc-ctype))
(defmethod expand-forwards-with-template-arguments (forwards (alloc-ctype cxxrecord-ctype)) nil)
(defmethod expand-forwards-with-template-arguments (forwards (alloc-ctype null)) nil)
(defmethod expand-forwards-with-template-arguments (forwards (alloc-ctype unclassified-ctype)) nil)
(defmethod expand-forwards-with-template-arguments (forwards (alloc-ctype smart-ptr-ctype)) nil)
(defmethod expand-forwards-with-template-arguments (forwards (alloc-ctype tagged-pointer-ctype)) nil)
(defmethod expand-forwards-with-template-arguments (forwards (alloc-ctype cxxrecord-ctype))
  (add-ctype forwards (ctype-key alloc-ctype) alloc-ctype))
(defmethod expand-forwards-with-template-arguments (forwards (alloc-ctype pointer-ctype))
  (expand-forwards-with-template-arguments forwards (pointer-ctype-pointee alloc-ctype)))

(defmethod expand-forwards-with-template-arguments (forwards (alloc-ctype class-template-specialization-ctype))
  (mapc (lambda (template-arg) (expand-forwards-with-template-arguments forwards (gc-template-argument-ctype template-arg)))
        (class-template-specialization-ctype-arguments alloc-ctype)))

(defun fill-forward-declarations (forwards enum analysis)
  (when (simple-enum-p enum)
    (expand-forwards-with-template-arguments forwards (alloc-ctype (simple-enum-alloc enum)))))

(defun generate-forward-declarations (&optional (analysis *analysis*))
  (let* ((forwards (analysis-forwards analysis)))
    (maphash (lambda (key enum) (fill-forward-declarations forwards enum analysis)) (analysis-enums analysis))))

(defun compact-enum-value (species-num alloc-num)
  (logior (ash species-num 16) alloc-num))

#|
(defun categorize-enums (analysis)
  "Return a list of simple-enums and templated-enums gathered from the template classes"
  (let (simple-enums templated-enums template-specializers)
    (mapc (lambda (species)
            (let ((enums (gethash species (analysis-species-to-enum analysis))))
              (when enums
                (mapc (lambda (e)
                        (let ((cclass (gethash (alloc-key (enum-alloc e)) (project-classes (analysis-project analysis)))))
                          (if (and (alloc-key (enum-alloc e)) (cclass-template-specializer cclass))
                               (push e template-specializers)
                               (push e simple-enums)))))
                      enums)))
          (manager-species (analysis-manager analysis)))
    (values (sort simple-enums (lambda (a b) (< (compact-enum-value (enum-species-num a) (enum-alloc-num a))
                                                (compact-enum-value (enum-species-num b) (enum-alloc-num b))))
                  template-specializers))))
|#





(defun class-enum-name (key species &optional (prefix "KIND"))
  (let* ((raw-name (format nil "~a_~a_~a" prefix (symbol-name (species-name species)) (copy-seq key)))
         (name0 (nsubstitute-if #\_ (lambda (c) (member c '(#\SPACE #\, #\< #\> #\: #\- ))) raw-name))
         (name1 (nsubstitute #\P #\* name0))
         (name2 (nsubstitute #\O #\( name1))
         (name3 (nsubstitute #\C #\) name2))
         (name4 (nsubstitute #\A #\& name3))
         (name name4))
    name))

(defstruct species
  name
  preprocessor-guard ;; defines a preprocessor symbol XXX used to wrap #ifdef XXX/#endif around GCKind
  discriminator ;; Function - takes one argument, returns a species index
  scan          ;; Function - generates scanner for species
  skip          ;; Function - generates obj_skip code for species
  finalize      ;; Function - generates obj_finalize code for species
  deallocator   ;; Function - generates obj_deallocate_unmanaged_instance code for species
  dump          ;; Function - fills a stringstream with a dump of the species
  index
  )
  
(defstruct manager
  (species nil) ;; a single argument lambda that returns an integer index
  (next-species-counter 0)
  ignore-discriminator
  )

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


(defun identify-species (manager aclass)
  (let (hits)
    (dolist (species (manager-species manager))
      (when (funcall (species-discriminator species) aclass)
        (push species hits)))
    (cond
      ((> (length hits) 1)
       (error "The class ~a could not be uniquely distinguished between the species: ~a" aclass hits))
      ((eql (length hits) 1)
       (car hits))
      ((and
        (manager-ignore-discriminator manager)
        (funcall (manager-ignore-discriminator manager) aclass))
       nil)
      (t (error " Could not identify species for ~a" aclass)
         nil))))

(defun alloc-template-specializer-p (alloc analysis)
  (let ((alloc-class (gethash (alloc-key alloc) (project-classes (analysis-project analysis)))))
    (if alloc-class
        (cclass-template-specializer alloc-class)
        nil)))

(defun make-enum-for-alloc-if-needed (alloc species analysis)
  (cond
    ((and (not (containeralloc-p alloc))
          (alloc-template-specializer-p alloc analysis))
     ;; We have a templated type - see if we need to construct a templated enum
     (let* ((class (gethash (alloc-key alloc) (project-classes (analysis-project analysis))))
            (single-base-key (let ((bases (cclass-bases class)))
                               (assert (eql (length bases) 1) nil "There can only be one base but class ~a has bases ~a" class bases) ;; there can be only one base
                               (assert (null (cclass-vbases class))) ;; There can be no vbases
                               (car bases)))
            (single-base (gethash single-base-key (project-classes (analysis-project analysis)))))
       (let* ((key (cclass-key single-base))
              (tenum (multiple-value-bind (te present-p)
                         ;; get the templated-enum
                         (gethash key (analysis-enums analysis))
                       (if (and present-p (templated-enum-p te))
                           ;; If its present and a templated-enum then return it
                           te
                           ;; If there wasn't a templated-enum in the hash-table - create one
                           (progn
                             (when (simple-enum-p te)
                               (warn "Since ~a is templated it must be a templated-enum - but there is already a simple-enum defined with this key - this error happened probably because you tried to allocate the TemplateBase of templated classes - don't do that!!" key))
                             (setf (gethash key (analysis-enums analysis))
                                   (make-templated-enum :key key
                                                        :name (class-enum-name key species)
                                                        :value :unassigned ;;(incf (analysis-cur-enum-value analysis))
                                                        :cclass single-base
                                                        :species species)))))))
         ;; save every alloc associated with this templated-enum
         (push alloc (templated-enum-all-allocs tenum)))))
    (t ;; It's a simple-enum
     (let* ((key (alloc-key alloc))
            (class (gethash key (project-classes (analysis-project analysis)))))
       (unless class ;; system allocs don't have classes - make a bogus one
         (setq class (make-cclass :key key)))
       (if (gethash key (analysis-enums analysis))
           (warn "There is already an enum defined with the key: ~a - this may happen if you allocate a TemplateBase of template classes directly" key)
           (setf (gethash key (analysis-enums analysis))
                 (make-simple-enum :key key
                                   :name (class-enum-name key species)
                                   :value :unassigned ;;(incf (analysis-cur-enum-value analysis))
                                   :cclass class
                                   :alloc alloc
                                   :species species)))))))

           


(defun analyze-alloc-and-assign-species-and-enum (alloc anal)
  (let* ((manager (analysis-manager anal))
         (species (identify-species manager alloc)))
    (if species
        (make-enum-for-alloc-if-needed alloc species anal)
        (error "Could not identify a species for ~a - is that ok???" alloc))))

(defun organize-allocs-into-species-and-create-enums (analysis &aux (project (analysis-project analysis)))
  "Every GCObject and GCContainer is assigned to a species and given a GCKind enum value."
  (let ((project (analysis-project analysis)))
    (maphash (lambda (k alloc) (analyze-alloc-and-assign-species-and-enum alloc analysis)) (project-lispallocs project))
    (maphash (lambda (k alloc) (analyze-alloc-and-assign-species-and-enum alloc analysis)) (project-containerallocs project))
    (maphash (lambda (k alloc) (analyze-alloc-and-assign-species-and-enum alloc analysis)) (project-classallocs project))
    (maphash (lambda (k alloc) (analyze-alloc-and-assign-species-and-enum alloc analysis)) (project-rootclassallocs project))))





(defgeneric fixer-macro-name (fixer-head))
(defmethod fixer-macro-name ((x (eql :smart-ptr-fix))) "SMART_PTR_FIX")
(defmethod fixer-macro-name ((x (eql :raw-tagged-pointer-fix))) "RAW_TAGGED_POINTER_FIX")
(defmethod fixer-macro-name ((x (eql :tagged-pointer-fix))) "TAGGED_POINTER_FIX")


(defun scanner-code-for-instance-var (stream ptr-name instance-var)
  (let* ((fixer-macro (fixer-macro-name (car (last instance-var))))
         (variable-chain (butlast instance-var 1))
         (instance-vars (mapcar (lambda (f) (instance-variable-field-name f)) variable-chain)))
    (format stream "    ~a(~a->~{~a~^.~});~%" fixer-macro ptr-name instance-vars)))


(defconstant +ptr-name+ "obj_gc_safe"
  "This variable is used to temporarily hold a pointer to a Wrapper<...> object - we want the GC to ignore it")

(defstruct destination
  stream
  (helper-stream (make-string-output-stream))
  table-name
  label-list
  label-prefix )

(defmacro with-jump-table ((fout jump-table-index dest enum &optional continue) &body body)
  (let ((label-gs (gensym)))
    `(let* ((,fout (destination-stream ,dest))
	    (,label-gs (format nil "~a_~a" (destination-label-prefix ,dest)
                               (enum-name ,enum)))
            (,jump-table-index (length (destination-label-list ,dest))))
       (push (cons (enum-value ,enum) ,label-gs) (destination-label-list ,dest))
       (format ,fout "~a:~%" ,label-gs)
       (format ,fout "{~%")
       ,@body
       (format ,fout "}~%")
       ,(if continue `(format ,fout "~a;~%" ,continue))
       )))



(defun field-data (key instance-var &optional (prefix "fixed_field_fix"))
  (let ((type (car (last instance-var)))
        names)
    (dolist (field (butlast instance-var))
      (push (instance-variable-field-name field) names))
    (let ((reverse-names (nreverse names)))
      (format nil " { ~a, offsetof(MACRO_SAFE_TYPE(~a),~{~a~^.~}), ~s }"
              prefix
              key
              reverse-names
              (format nil "~{~a~^.~}" reverse-names)))))

(defun scanner-for-lispallocs (dest enum anal)
  (assert (simple-enum-p enum))
  (let* ((alloc (simple-enum-alloc enum))
         (key (alloc-key alloc))
         (enum-name (enum-name enum)))
    (gclog "build-mps-scan-for-one-family -> inheritance key[~a]  value[~a]~%" key value)
    (let ((fh (destination-helper-stream dest)))
      (let ((layout
             (class-layout (gethash key (project-classes (analysis-project anal))) anal)))
        (codegen-lisp-layout fh enum-name key layout anal)))))

(defun skipper-for-lispallocs (dest enum anal)
  (assert (simple-enum-p enum))
  (let* ((alloc (simple-enum-alloc enum))
         (key (alloc-key alloc))
         (enum-name (enum-name enum)))
    (gclog "skipper-for-lispallocs -> inheritance classid[~a]  value[~a]~%" key (enum-value enum))))

(defun dumper-for-lispallocs (dest enum anal)
  (assert (simple-enum-p enum))
  (let* ((alloc (simple-enum-alloc enum))
         (key (alloc-key alloc))
         (enum-name (enum-name enum)))
    (with-jump-table (fout jti dest enum "goto BOTTOM")
      ;;    (format fout "    ~A* ~A = reinterpret_cast<~A*>(client);~%" key +ptr-name+ key)
      (format fout "    typedef ~A type_~A;~%" key enum-name)
      (format fout "    sout << \"~a size[\" << (AlignUp(sizeof(type_~a))+global_alignup_sizeof_header) << \"]\" ;~%" enum-name enum-name ))))

(defun finalizer-for-lispallocs (dest enum anal)
  (check-type enum simple-enum)
  (let* ((alloc (simple-enum-alloc enum))
         (key (alloc-key alloc))
         (enum-name (enum-name enum))
         (ns-cn key)
         (cn (strip-all-namespaces-from-name ns-cn)))
    (with-jump-table (fout jti dest enum)
    (gclog "build-mps-finalize-for-one-family -> inheritance key[~a]  value[~a]~%" key value)
    (format fout "    ~A* ~A = reinterpret_cast<~A*>(client);~%" key +ptr-name+ key)
;;    (format fout "    ~A* ~A = BasePtrToMostDerivedPtr<~A>(base);~%" key +ptr-name+ key)
    (format fout "    ~A->~~~A();~%" +ptr-name+ cn)
    (format fout "    return;~%"))))


(defun deallocator-for-lispallocs (dest enum anal)
  (check-type enum simple-enum)
  (let* ((alloc (simple-enum-alloc enum))
         (key (alloc-key alloc))
         (enum-name (enum-name enum))
         (ns-cn key)
         (cn (strip-all-namespaces-from-name ns-cn)))
    (with-jump-table (fout jti dest enum)
    (gclog "build-mps-deallocator-for-one-family -> inheritance key[~a]  value[~a]~%" key value)
    (format fout "    ~A* ~A = reinterpret_cast<~A*>(client);~%" key +ptr-name+ key)
;;    (format fout "    ~A* ~A = BasePtrToMostDerivedPtr<~A>(base);~%" key +ptr-name+ key)
    (format fout "    GC<~A>::deallocate_unmanaged_instance(~A);~%" key +ptr-name+)
    (format fout "    return;~%"))))



(defun scanner-for-templated-lispallocs (dest enum anal)
  (assert (templated-enum-p enum))
  (let* ((key (enum-key enum))
         (enum-name (enum-name enum)))
    (gclog "build-mps-scan-for-one-family -> inheritance key[~a]  value[~a]~%" key value)
;;    (with-jump-table (fout jti dest enum "goto SCAN_ADVANCE")
      (let ((fh (destination-helper-stream dest)))
        (let ((layout (class-layout (gethash key (project-classes (analysis-project anal))) anal)))
          (codegen-templated-layout fh enum-name key layout anal))
        (progn
          #+(or)(format fh "{ templated_class_kind, ~d, ~s },~%" enum-name key)
          #+(or)(format fh "{ templated_class_jump_table_index, ~d, 0, 0, \"\" },~%" jti))
        #+(or)(format fout "    ~A* ~A = reinterpret_cast<~A*>(client);~%" key +ptr-name+ key)
        #+(or)(let ((all-instance-variables (class-layout (gethash key (project-classes (analysis-project anal))) anal)))
                (dolist (instance-var all-instance-variables)
                  (scanner-code-for-instance-var fout +ptr-name+ instance-var)))
        #+(or)(format fout "    size = ~a->templatedSizeof();" +ptr-name+))))


(defun skipper-for-templated-lispallocs (dest enum anal)
  (assert (templated-enum-p enum))
  (let* ((key (enum-key enum))
         (enum-name (enum-name enum)))
    (gclog "build-mps-scan-for-one-family -> inheritance key[~a]  value[~a]~%" key value)
    (with-jump-table (fout jti dest enum)
      (format fout "    ~A* ~A = reinterpret_cast<~A*>(client);~%" key +ptr-name+ key)
      (format fout "    // client = (char*)client + AlignUp(~a->templatedSizeof()) + global_alignup_sizeof_header;~%" +ptr-name+)
      (format fout "    size = ~a->templatedSizeof();~%" +ptr-name+)
      (format fout "    goto DONE; //return client;~%")
      )))

(defun dumper-for-templated-lispallocs (dest enum anal)
  (assert (templated-enum-p enum))
  (let* ((key (enum-key enum))
         (enum-name (enum-name enum)))
    (gclog "build-mps-scan-for-one-family -> inheritance key[~a]  value[~a]~%" key value)
    (with-jump-table (fout jti dest enum "goto BOTTOM")
      (format fout "    ~A* ~A = reinterpret_cast<~A*>(client);~%" key +ptr-name+ key)
      (format fout "    sout << \"~a size[\" << (AlignUp(~a->templatedSizeof()) + global_alignup_sizeof_header) << \"]\" ;~%" enum-name +ptr-name+ )
      )))

(defun finalizer-for-templated-lispallocs (dest enum anal)
  (assert (templated-enum-p enum))
  (let* ((key (enum-key enum))
         (enum-name (enum-name enum))
         (ns-cn key)
         (cn (strip-all-namespaces-from-name ns-cn)))
    (with-jump-table (fout jti dest enum)
    (format fout "    ~A* ~A = reinterpret_cast<~A*>(client);~%" key +ptr-name+ key)
;;    (format fout "    ~A* ~A = BasePtrToMostDerivedPtr<~A>(base);~%" key +ptr-name+ key)
    (format fout "    ~A->~~~A();~%" +ptr-name+ cn)
    )))

(defun deallocator-for-templated-lispallocs (dest enum anal)
  (assert (templated-enum-p enum))
  (let* ((key (enum-key enum))
         (enum-name (enum-name enum))
         (ns-cn key)
         (cn (strip-all-namespaces-from-name ns-cn)))
    (with-jump-table (fout jti dest enum)
    (format fout "    ~A* ~A = reinterpret_cast<~A*>(client);~%" key +ptr-name+ key)
;;    (format fout "    ~A* ~A = BasePtrToMostDerivedPtr<~A>(base);~%" key +ptr-name+ key)
    (format fout "    GC<~A>::deallocate_unmanaged_instance(~A);~%" key +ptr-name+)
    )))

(defun scanner-for-gccontainer (dest enum anal)
  (check-type enum simple-enum)
  (let* ((alloc (simple-enum-alloc enum))
         (decl (containeralloc-ctype alloc))
         (key (alloc-key alloc))
         (enum-name (enum-name enum)))
    ;;    (with-jump-table (fout jti dest enum "goto SCAN_ADVANCE")
    (let ((fh (destination-helper-stream dest)))
      (if (cxxrecord-ctype-p decl)
          (progn
            (format fout "    THROW_HARD_ERROR(BF(\"Should never scan ~a\"));~%" (cxxrecord-ctype-key decl)))
          (let ((layout (class-layout (gethash key (project-classes (analysis-project anal))) anal)))
            (codegen-container-layout fh enum-name key layout anal))))))


(defun skipper-for-gccontainer (dest enum anal)
  (check-type enum simple-enum)
  (let* ((alloc (simple-enum-alloc enum))
         (decl (containeralloc-ctype alloc))
         (key (alloc-key alloc))
         (enum-name (enum-name enum)))
    #+(or)(with-jump-table (fout jti dest enum)
      ;;    (format fout "// processing ~a~%" alloc)
      (if (cxxrecord-ctype-p decl)
          (progn
            (format fout "    THROW_HARD_ERROR(BF(\"Should never scan ~a\"));~%" (cxxrecord-ctype-key decl)))
          (let* ((parms (class-template-specialization-ctype-arguments decl))
                 (parm0 (car parms))
                 (parm0-ctype (gc-template-argument-ctype parm0)))
            ;;          (format fout "// parm0-ctype = ~a~%" parm0-ctype)
            (format fout "  {~%")
            (format fout "    ~A* ~A = reinterpret_cast<~A*>(client);~%" key +ptr-name+ key)
            (format fout "    typedef typename ~A type_~A;~%" key enum-name)
            (format fout "    // size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_~a>(~a->capacity()))+AlignUp(sizeof(gctools::Header_s));~%" enum-name +ptr-name+)
            (format fout "    size = sizeof_container<type_~a>(~a->capacity());~%" enum-name +ptr-name+)
            (format fout "    //client = (char*)client + header_and_gccontainer_size;~%" enum-name)
            (format fout "  }~%")))
      (format fout "    goto DONE; //return client;~%")
      ;;              (format fout "    base = (char*)base + length;~%")
      )))

(defun dumper-for-gccontainer (dest enum anal)
  (check-type enum simple-enum)
  (let* ((alloc (simple-enum-alloc enum))
         (decl (containeralloc-ctype alloc))
         (key (alloc-key alloc))
         (enum-name (enum-name enum)))
    #+(or)(with-jump-table (fout jti dest enum "goto BOTTOM")
            ;;    (format fout "// processing ~a~%" alloc)
            (if (cxxrecord-ctype-p decl)
                (progn
                  (format fout "    THROW_HARD_ERROR(BF(\"Should never dumper ~a\"));~%" (cxxrecord-ctype-key decl)))
                (let* ((parms (class-template-specialization-ctype-arguments decl))
                       (parm0 (car parms))
                       (parm0-ctype (gc-template-argument-ctype parm0)))
                  ;;          (format fout "// parm0-ctype = ~a~%" parm0-ctype)
                  (format fout "    ~A* ~A = reinterpret_cast<~A*>(client);~%" key +ptr-name+ key)
                  (format fout "    sout << \"~a\" << \" size/capacity[\" << ~a->size() << \"/\" << ~a->capacity();~%" key +ptr-name+ +ptr-name+)
                  (format fout "    typedef typename ~A type_~A;~%" key enum-name)
                  (format fout "    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_~a>(~a->capacity()))+AlignUp(sizeof(gctools::Header_s));~%" enum-name +ptr-name+)
                  (format fout "    sout << \"bytes[\" << header_and_gccontainer_size << \"]\";~%" )
                  ))
            ;;              (format fout "    base = (char*)base + length;~%")
            )))



(defun finalizer-for-gccontainer (dest enum anal)
  (check-type enum simple-enum)
  (let* ((alloc (simple-enum-alloc enum))
         (decl (containeralloc-ctype alloc))
         (key (alloc-key alloc))
         (enum-name (enum-name enum)))
    (with-jump-table (fout jti dest enum)
;;    (format fout "// processing ~a~%" alloc)
    (if (cxxrecord-ctype-p decl)
        (progn
          (format fout "    THROW_HARD_ERROR(BF(\"Should never finalize ~a\"));~%" (record-ctype-key decl)))
        (let* ((parms (class-template-specialization-ctype-arguments decl))
               (parm0 (car parms))
               (parm0-ctype (gc-template-argument-ctype parm0)))
;;          (format fout "// parm0-ctype = ~a~%" parm0-ctype)
          (format fout "    THROW_HARD_ERROR(BF(\"Should never finalize containers ~a\"));" (record-ctype-key decl))))
    )))

(defun deallocator-for-gccontainer (dest enum anal)
  (check-type enum simple-enum)
  (let* ((alloc (simple-enum-alloc enum))
         (decl (containeralloc-ctype alloc))
         (key (alloc-key alloc))
         (enum-name (enum-name enum)))
    (with-jump-table (fout jti dest enum)
;;    (format fout "// processing ~a~%" alloc)
    (if (cxxrecord-ctype-p decl)
        (progn
          (format fout "    THROW_HARD_ERROR(BF(\"Should never deallocate ~a\"));~%" (record-ctype-key decl)))
        (let* ((parms (class-template-specialization-ctype-arguments decl))
               (parm0 (car parms))
               (parm0-ctype (gc-template-argument-ctype parm0)))
;;          (format fout "// parm0-ctype = ~a~%" parm0-ctype)
          (format fout "    THROW_HARD_ERROR(BF(\"Should never deallocate containers ~a\"));" (record-ctype-key decl))))
    )))


(defun scanner-for-gcstring (dest enum anal)
  (assert (simple-enum-p enum))
  (let* ((alloc (simple-enum-alloc enum))
         (key (alloc-key alloc))
         (enum-name (enum-name enum)))
    (gclog "build-mps-scan-for-one-family -> inheritance key[~a]  value[~a]~%" key value)
    ;;(with-jump-table (fout jump-table-index dest enum "goto SCAN_ADVANCE")
    (let ((fh (destination-helper-stream dest)))
      (let ((layout (class-layout (gethash key (project-classes (analysis-project anal))) anal)))
        (codegen-container-layout fh enum-name key layout anal)))))



(defun skipper-for-gcstring (dest enum anal)
  (check-type enum simple-enum)
  (let* ((alloc (simple-enum-alloc enum))
         (decl (containeralloc-ctype alloc))
         (key (alloc-key alloc))
         (enum-name (enum-name enum)))
    #+(or)(with-jump-table (fout jti dest enum)
            ;;    (format fout "// processing ~a~%" alloc)
            (if (cxxrecord-ctype-p decl)
                (progn
                  (format fout "    THROW_HARD_ERROR(BF(\"Should never scan ~a\"));~%" (cxxrecord-ctype-key decl)))
                (let* ((parms (class-template-specialization-ctype-arguments decl))
                       (parm0 (car parms))
                       (parm0-ctype (gc-template-argument-ctype parm0)))
                  ;;          (format fout "// parm0-ctype = ~a~%" parm0-ctype)
                  (format fout "  {~%")
                  (format fout "    ~A* ~A = reinterpret_cast<~A*>(client);~%" key +ptr-name+ key)
                  ;;          (format fout "    ~A* ~A = BasePtrToMostDerivedPtr<~A>(base);~%" key +ptr-name+ key)
                  (format fout "    typedef typename ~A type_~A;~%" key enum-name)
                  (format fout "    //size_t header_and_gcstring_size = AlignUp(sizeof_container<type_~a>(~a->capacity()))+AlignUp(sizeof(gctools::Header_s));~%" enum-name +ptr-name+)
                  (format fout "    size = sizeof_container<type_~a>(~a->capacity());~%" enum-name +ptr-name+)
                  (format fout "    //client = (char*)client + Align(header_and_gcstring_size);~%")
                  (format fout "  }~%")
                  ;;          (format fout "    base = (char*)base + length;~%")
                  (format fout "    goto DONE; //return client;~%")

                  ))
            )))

(defun dumper-for-gcstring (dest enum anal)
  (check-type enum simple-enum)
  (let* ((alloc (simple-enum-alloc enum))
         (decl (containeralloc-ctype alloc))
         (key (alloc-key alloc))
         (enum-name (enum-name enum)))
    (with-jump-table (fout jti dest enum "goto BOTTOM")
		      ;;    (format fout "// processing ~a~%" alloc)
		      (if (cxxrecord-ctype-p decl)
			  (progn
			    (format fout "    THROW_HARD_ERROR(BF(\"Should never scan ~a\"));~%" (cxxrecord-ctype-key decl)))
			(let* ((parms (class-template-specialization-ctype-arguments decl))
			       (parm0 (car parms))
			       (parm0-ctype (gc-template-argument-ctype parm0)))
			  ;;          (format fout "// parm0-ctype = ~a~%" parm0-ctype)
			  (format fout "    ~A* ~A = reinterpret_cast<~A*>(client);~%" key +ptr-name+ key)
			  (format fout "    typedef typename ~A type_~A;~%" key enum-name)
			  (format fout "    size_t header_and_gcstring_size = AlignUp(sizeof_container<type_~a>(~a->capacity()))+AlignUp(sizeof(gctools::Header_s));~%" enum-name +ptr-name+)
			  (format fout "    sout << \"~a\" << \"bytes[\" << header_and_gcstring_size << \"]\";~%" enum-name )
			  ))
		      )))



(defun finalizer-for-gcstring (dest enum anal)
  (check-type enum simple-enum)
  (let* ((alloc (simple-enum-alloc enum))
         (decl (containeralloc-ctype alloc))
         (key (alloc-key alloc))
         (enum-name (enum-name enum)))
    (with-jump-table (fout jti dest enum)
;;    (format fout "// processing ~a~%" alloc)
    (if (cxxrecord-ctype-p decl)
        (progn
          (format fout "    THROW_HARD_ERROR(BF(\"Should never finalize ~a\"));~%" (record-ctype-key decl)))
        (let* ((parms (class-template-specialization-ctype-arguments decl))
               (parm0 (car parms))
               (parm0-ctype (gc-template-argument-ctype parm0)))
;;          (format fout "// parm0-ctype = ~a~%" parm0-ctype)
          (format fout "    THROW_HARD_ERROR(BF(\"Should never finalize gcstrings ~a\"));" (record-ctype-key decl))))
    )))

(defun deallocator-for-gcstring (dest enum anal)
  (check-type enum simple-enum)
  (let* ((alloc (simple-enum-alloc enum))
         (decl (containeralloc-ctype alloc))
         (key (alloc-key alloc))
         (enum-name (enum-name enum)))
    (with-jump-table (fout jti dest enum)
;;    (format fout "// processing ~a~%" alloc)
    (if (cxxrecord-ctype-p decl)
        (progn
          (format fout "    THROW_HARD_ERROR(BF(\"Should never deallocate ~a\"));~%" (record-ctype-key decl)))
        (let* ((parms (class-template-specialization-ctype-arguments decl))
               (parm0 (car parms))
               (parm0-ctype (gc-template-argument-ctype parm0)))
;;          (format fout "// parm0-ctype = ~a~%" parm0-ctype)
          (format fout "    THROW_HARD_ERROR(BF(\"Should never deallocate gcstrings ~a\"));" (record-ctype-key decl))))
    )))


(defun string-left-matches (str sub)
  (eql (search str sub) 0))

(defvar *analysis*)
(defun setup-manager ()
  (let* ((manager (make-manager)))
    (add-species manager (make-species :name :bootstrap
                                       :discriminator (lambda (x) (and (lispalloc-p x)
                                                                       (gctools::bootstrap-kind-p (alloc-key x))))
                                       :scan 'scanner-for-lispallocs
                                       :skip 'skipper-for-lispallocs
                                       :dump 'dumper-for-lispallocs
                                       :finalize 'finalizer-for-lispallocs
                                       :deallocator 'deallocator-for-lispallocs
                                       ))
    (add-species manager (make-species :name :lispalloc
                                       :discriminator (lambda (x) (and (lispalloc-p x)
                                                                       (not (gctools:bootstrap-kind-p (alloc-key x)))
                                                                       (not (alloc-template-specializer-p x *analysis*))))
                                       :scan 'scanner-for-lispallocs
                                       :skip 'skipper-for-lispallocs
                                       :dump 'dumper-for-lispallocs
                                       :finalize 'finalizer-for-lispallocs
                                       :deallocator 'deallocator-for-lispallocs
                                       ))
    (add-species manager (make-species :name :templated-lispalloc
                                       :discriminator (lambda (x) (and (lispalloc-p x) (alloc-template-specializer-p x *analysis*)))
                                       :scan 'scanner-for-templated-lispallocs
                                       :skip 'skipper-for-templated-lispallocs
                                       :dump 'dumper-for-templated-lispallocs
                                       :finalize 'finalizer-for-templated-lispallocs
                                       :deallocator 'deallocator-for-templated-lispallocs
                                       ))
    (add-species manager (make-species :name :GCVECTOR
                                       :discriminator (lambda (x) (and (containeralloc-p x) (search "gctools::GCVector" (alloc-key x))))
                                       :scan 'scanner-for-gccontainer
                                       :skip 'skipper-for-gccontainer
                                       :dump 'dumper-for-gccontainer
                                       :finalize 'finalizer-for-gccontainer
                                       :deallocator 'deallocator-for-gccontainer
                                       ))
    (add-species manager (make-species :name :GCARRAY
                                       :discriminator (lambda (x) (and (containeralloc-p x) (search "gctools::GCArray" (alloc-key x))))
                                       :scan 'scanner-for-gccontainer
                                       :skip 'skipper-for-gccontainer
                                       :dump 'dumper-for-gccontainer
                                       :finalize 'finalizer-for-gccontainer
                                       :deallocator 'deallocator-for-gccontainer
                                       ))
    (add-species manager (make-species :name :GCSTRING
                                       :discriminator (lambda (x) (and (containeralloc-p x) (search "gctools::GCString" (alloc-key x))))
                                       :scan 'scanner-for-gcstring ;; don't need to scan but do need to calculate size
                                       :skip 'skipper-for-gcstring
                                       :dump 'dumper-for-gcstring
                                       :finalize 'finalizer-for-gcstring
                                       :deallocator 'deallocator-for-gcstring
                                       ))
    (add-species manager (make-species :name :classalloc
                                       :discriminator (lambda (x) (and (classalloc-p x) (not (alloc-template-specializer-p x *analysis*))))
                                       :scan 'scanner-for-lispallocs
                                       :skip 'skipper-for-lispallocs
                                       :dump 'dumper-for-lispallocs
                                       :finalize 'finalizer-for-lispallocs
                                       :deallocator 'deallocator-for-lispallocs
                                       ))
    (add-species manager (make-species :name :rootclassalloc
                                       :discriminator (lambda (x) (rootclassalloc-p x))
                                       :scan 'scanner-for-lispallocs
                                       :skip 'skipper-for-lispallocs
                                       :dump 'dumper-for-lispallocs
                                       :finalize 'finalizer-for-lispallocs
                                       :deallocator 'deallocator-for-lispallocs
                                       ))
    (add-species manager (make-species :name :templated-classalloc
                                       :discriminator (lambda (x) (and (classalloc-p x) (alloc-template-specializer-p x *analysis*)))
                                       :scan 'scanner-for-templated-lispallocs
                                       :skip 'skipper-for-templated-lispallocs
                                       :dump 'dumper-for-templated-lispallocs
                                       :finalize 'finalizer-for-templated-lispallocs
                                       :deallocator 'deallocator-for-templated-lispallocs
                                       ))
    manager))




(defun sort-enums-by-value (analysis)
  (let (names)
    (maphash #'(lambda (k e)
                 (push (cons (enum-value e) e) names))
             (analysis-enums analysis))
    (setf (analysis-sorted-enums analysis)
          (mapcar #'(lambda (val-enum)
                    (cdr val-enum))
                (sort names #'< :key #'car)))))



(defun analyze-project (project &key types-to-inline-mps-functions )
  (let ((*analysis* (make-analysis :project project
                                 :inline types-to-inline-mps-functions))
        (manager (setup-manager)))
    (setf (analysis-manager *analysis*) manager)
    (organize-allocs-into-species-and-create-enums *analysis*)
    (generate-forward-declarations *analysis*)
    (analyze-hierarchy *analysis*)
    (sort-enums-by-value *analysis*)
    *analysis*))



(defvar *project*)
(defun derived-from-cclass (child ancestor &optional (project *project*))
  (setq child (if (stringp child) (gethash child (project-classes project)) child))
  (setq ancestor (if (stringp ancestor) (gethash ancestor (project-classes project)) ancestor))
  (if (eq child ancestor)
      t
      (progn
        (dolist (parent (cclass-bases child))
          (when (derived-from-cclass parent ancestor)
            (return-from derived-from-cclass t)))
        (dolist (parent (cclass-vbases child))
          (when (derived-from-cclass parent ancestor)
            (return-from derived-from-cclass t)))
        nil)))


(defun inherits-metadata (cclass metadata &optional (project *project*))
  (unless cclass (return-from inherits-metadata nil))
  (setq cclass (if (stringp cclass) (gethash cclass (project-classes project)) cclass))
  (unless cclass (return-from inherits-metadata nil))
  (if (member metadata (cclass-metadata cclass))
      t 
      (progn
        (dolist (parent (cclass-bases cclass))
          (when (inherits-metadata parent metadata project)
            (return-from inherits-metadata t)))
        (dolist (parent (cclass-vbases cclass))
          (when (inherits-metadata parent metadata project)
            (return-from inherits-metadata t)))
        nil)))






;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;;
;; stage: output
;; Generate output
;;
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------




(defun generate-one-gckind-for-enum (stream enum)
  (let* ((enum-name (enum-name enum))
         (species (enum-species enum)))
    (when (species-preprocessor-guard species)
      (format stream "#if defined(~a)~%" (species-preprocessor-guard species)))
    (cond
      ((simple-enum-p enum)
;;       (format stream "//GCKind for ~a~%" enum)
       (format stream "template <> class gctools::GCKind<~A> {~%" (enum-key enum)))
      (t ;; templated-enum
;;       (format stream "//GCTemplatedKind for ~a~%" enum)
       (format stream "template <> class gctools::GCKind<~A> {~%" (enum-key enum))))
    (format stream "public:~%")
    (format stream "  static gctools::GCKindEnum const Kind = gctools::~a ;~%" enum-name)
    (format stream "};~%")
    (when (species-preprocessor-guard species)
      (format stream "#endif // #if defined(~a)~%" (species-preprocessor-guard species)))))


(defun generate-gckind-for-enums (fout anal)
  (maphash (lambda (key enum)
             (generate-one-gckind-for-enum fout enum))
           (analysis-enums anal)))




(defun class-layout (x analysis)
  (let* ((fields (ensure-list (linearize-class-layout-impl x x analysis)))
         (fixed (loop :for var :in fields
                   :when (not (typep var 'container-offset))
                   :collect var))
         (variable (loop :for var :in fields
                      :when (typep var 'container-offset)
                      :collect var)))
    (when (> (length variable) 1)
      (analysis-error "Class ~a has more than one GCVector/GCArray/GCString part" (cclass-key x)))
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

(defun code-for-class-layout (key cclass-layout project &optional (stream t))
  (dolist (instance-var (fixed-part cclass-layout))
    (format stream "~a,~%" (field-data key instance-var "fixed_field_fix")))
  (let ((var-part (variable-part cclass-layout)))
    (when var-part
      (format stream "~a,~%" (field-data key var-part "variable_array0"))
      (let* ((ctype (instance-field-ctype (car var-part)))
             (ctype-key (container-key ctype))
             (container-cclass (gethash ctype-key (project-classes project)))
             (capacity-field (field-with-name container-cclass "_Capacity")))
        (format t "      var-part ctype: ~a~%" ctype)
        (format t "           ctype-key: ~a~%" ctype-key)
        (format t "    container-cclass: ~a~%" container-cclass)
        (format t "      capacity-field: ~a~%" capacity-field)))))

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
(defmethod fixable-instance-variables-impl ((x instance-variable) analysis)
  (fixable-instance-variables-impl (instance-field-ctype x) analysis))

(defmethod fixable-instance-variables-impl ((x instance-array-element) analysis)
  (fixable-instance-variables-impl (instance-field-ctype x) analysis))

(defmethod fixable-instance-variables-impl ((x cclass) analysis)
  (let* ((project (analysis-project analysis))
         (base-code (let (all-fixers)
                       (dolist (base-name (cclass-bases x))
                         (let ((base-fixers (fixable-instance-variables-impl (gethash base-name (project-classes project)) analysis)))
                           (when base-fixers
                             (setq all-fixers (append base-fixers all-fixers)))))
                       all-fixers))
         (vbase-code (let (all-fixers)
                       (dolist (vbase-name (cclass-vbases x))
                         (let ((vbase-fixers (fixable-instance-variables-impl (gethash vbase-name (project-classes project)) analysis)))
                           (when vbase-fixers
                             (setq all-fixers (append vbase-fixers all-fixers)))))
                       all-fixers))
         (field-code (let (all-fixers)
                       (dolist (field (cclass-fields x))
                         (let ((field-fixers (fix-code-for-field field analysis)))
                           (when field-fixers
                             (setq all-fixers (append field-fixers all-fixers)))))
                       all-fixers))
         result)
    (when base-code (setq result (append result base-code)))
    (when vbase-code (setq result (append result vbase-code)))
    (when field-code (setq result (append result field-code)))
    result))
(defmethod fixable-instance-variables-impl ((x builtin-ctype) analysis) nil)


(defmethod fixable-instance-variables-impl ((x unclassified-ctype) analysis)
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

(defmethod fixable-instance-variables-impl ((x smart-ptr-ctype) analysis) :smart-ptr-fix)

(defmethod fixable-instance-variables-impl ((x tagged-pointer-ctype) analysis) :tagged-pointer-fix)

(defmethod fixable-instance-variables-impl ((x constant-array-ctype) analysis)
  (when (contains-fixptr-p x (analysis-project analysis))
    (fixable-instance-variables-impl (constant-array-ctype-element-type x) analysis)))

(defmethod fixable-instance-variables-impl ((x pointer-ctype) analysis )
  (let ((pointee (pointer-ctype-pointee x)))
    (cond
      ((or (gcvector-moveable-ctype-p pointee)
           (gcarray-moveable-ctype-p pointee)
           (gcstring-moveable-ctype-p pointee)) :raw-tagged-pointer-fix)
      ((is-alloc-p (pointer-ctype-pointee x) (analysis-project analysis))
       :raw-tagged-pointer-fix)
      ((fixable-pointee-p (pointer-ctype-pointee x))
       :raw-tagged-pointer-fix)
      ((ignorable-ctype-p (pointer-ctype-pointee x)) nil)
      (t
       ;;(warn "I'm not sure if I can ignore pointer-ctype ~a  ELIMINATE THESE WARNINGS" x)
       nil))))











(defun fixable-pointee-p (ctype)
  (let ((key (ctype-key ctype)))
    (inherits-metadata key :metadata_always_fix_pointers_to_derived_classes)))

(defgeneric ignorable-ctype-p (ctype))

(defmethod ignorable-ctype-p ((ctype builtin-ctype)) nil)
(defmethod ignorable-ctype-p ((ctype function-proto-ctype)) nil)
(defmethod ignorable-ctype-p ((ctype dependent-name-ctype)) t)
(defmethod ignorable-ctype-p ((ctype smart-ptr-ctype)) nil)
(defmethod ignorable-ctype-p ((ctype tagged-pointer-ctype)) nil)

(defun make-table (strings)
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (s strings)
      (setf (gethash s ht) t))
    ht))

(defparameter +not-ignorable-unclassified-ctype+
  (make-table '(
                "double"
                "int"
                "long long"
                "unsigned long")))

(defparameter +ignorable-unclassified-ctype+
  (make-table '(
                "void (void *)"
                "void"
                "_Bool"
                "enum asttooling::ContextType"
                "enum asttooling::ErrorType"
                "enum boost::filesystem::file_type"
                "enum boost::filesystem::perms"
                "enum clang::InputKind"
                "enum clang::ast_matchers::dynamic::VariantValue::ValueType"
                "enum llvm::APFloat::fltCategory"
                "short"
                "unsigned char"
                "unsigned long long"
                "void (core::Lisp_O *)"
                )))


(defmethod ignorable-ctype-p ((ctype unclassified-ctype))
  (let ((key (ctype-key ctype)))
    (if (gethash key +not-ignorable-unclassified-ctype+)
        nil
        (gethash key +ignorable-unclassified-ctype+))))



(defparameter +ignorable-cxxrecord-ctype+
  (make-table '(
                "__sFILE"
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
                "std::type_info"
                )))

(defmethod ignorable-ctype-p ((ctype cxxrecord-ctype))
  (cond
    ((gethash (ctype-key ctype) +ignorable-cxxrecord-ctype+) t)
    ((contains-fixptr-p ctype *project*) nil)
    (t nil)))
     


(defparameter +ignorable-class-template-specialization-ctype+
  (make-table '(
                       "boost::iostreams::chain<boost::iostreams::input,char,std::__1::char_traits<char>,std::__1::allocator<char>>"
                       "boost::iostreams::chain<boost::iostreams::output,char,std::__1::char_traits<char>,std::__1::allocator<char>>"
                       "boost::re_detail::basic_regex_implementation<char,boost::regex_traits<char,boost::cpp_regex_traits<char>>>"
                       "boost::sub_match<const char *>"
                       "std::__1::__tree_node<std::__1::__value_type<std::__1::basic_string<char,std::__1::char_traits<char>,std::__1::allocator<char>>,int>,void *>"
                       "std::__1::__tree_node<std::__1::basic_string<char,std::__1::char_traits<char>,std::__1::allocator<char>>,void *>"
                       "std::__1::__tree_node_base<void *>"
                       "std::__1::basic_ostream<char,std::__1::char_traits<char>>"
                       "std::__1::basic_string<char,std::__1::char_traits<char>,std::__1::allocator<char>>"
                       "std::__1::codecvt<char,char,(anonymous)>"
                       )))

(defmethod ignorable-ctype-p ((ctype class-template-specialization-ctype))
  (gethash (ctype-key ctype) +ignorable-class-template-specialization-ctype+))


(defmethod ignorable-ctype-p ((ctype pointer-ctype))
  (cond
    ((pointer-ctype-p (pointer-ctype-pointee ctype))
     (warn "ignoring pointer-ctype because pointee is another pointer for ~a" ctype)
     t)
    ((ignorable-ctype-p (pointer-ctype-pointee ctype)) t)
    (t
     (warn "Check ignorable-ctype-p ((ctype pointer-ctype)) for ~a" ctype))))



(defun separate-namespace-name (name)
"Separate a X::Y::Z name into (list X Y Z) - strip any preceeding 'class '"
  (let* ((full-name (if (string= (subseq name 0 (length "class ")) "class ")
                        (subseq name (length "class ") (length name))
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
        (let ((removed (concatenate 'string (subseq name 0 pos) (subseq name (+ pos (length str)) (length name)))))
          (remove-string removed str))
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
#|  (when (and (eql (length name) 1) (string= "ddddDiagnostics" (car name)))
    (break "Check name - about to add to namespace"))
|#
  (if (eql (length name) 1)
      (push (car name) (namespace-names ns))
      (let ((subnamespace (gethash (car name) (namespace-submap ns) (make-namespace))))
        (setf (gethash (car name) (namespace-submap ns)) subnamespace)
        (namespace-add-name subnamespace (cdr name)))))

(defun code-for-nested-class-names (stream ns ns-name &optional (indent 0))
  (dolist (name (namespace-names ns))
    (format stream "~vt// NESTED    class ~a::~a; // YOU ARE GOING TO HAVE TO INCLUDE THE DEFINITION OF THIS CLASS!!!~%" (+ indent 4) ns-name name))
  (maphash (lambda (ns-name subnamespace)
             (progn
               (format stream "~vt// nested classes within ~a START~%" indent ns-name)
               (code-for-nested-class-names stream subnamespace ns-name)
               (format stream "~vt// nested classes END~%" indent)))
           (namespace-submap ns)))


(defun code-for-namespace-names (stream ns &optional (indent 0))
  (let ((classes (make-hash-table :test #'equal)))
    (dolist (name (namespace-names ns))
      (setf (gethash name classes) t)
      (format stream "~vtclass ~a;~%" indent name))
    (maphash (lambda (ns-name subnamespace)
               (if (gethash ns-name classes)
                   (progn
                     (format stream "~vt// nested classes within ~a START~%" indent ns-name)
                     (code-for-nested-class-names stream subnamespace ns-name)
                     (format stream "~vt// nested classes END~%" indent))
                   (progn
                     (format stream "~vtnamespace ~a {~%" indent ns-name)
                     (code-for-namespace-names stream subnamespace (+ 4 indent))
                     (format stream "~vt};~%" indent))))
             (namespace-submap ns))))



(defun merge-forward-names-by-namespace (analysis)
  (let ((forwards (analysis-forwards analysis))
        (top-namespace (make-namespace)))
    (maphash (lambda (name value)
               (let ((split-name (separate-namespace-name name)))
                 (namespace-add-name top-namespace split-name)))
             forwards)
    top-namespace))

#|
        current-group
        current-namespace)
    (do* ((cur forward-names (cdr cur))
          (name (car cur) (car cur)))
         ((null cur)
          (push (cons current-namespace (reverse current-group)) merged-groups)
          (reverse merged-groups))
      (multiple-value-bind (ns name)
          (separate-namespace-name (car cur))
        (if (string= ns current-namespace)
            (push name current-group)
            (progn
              (when current-group
                (push (cons current-namespace (reverse current-group)) merged-groups)
                (setq current-group nil)
                (push name current-group)))
            )
        (setq current-namespace ns)
        ))))
|#

(defun prefix-template-if-needed (name)
  (if (search "<" name)
      "template <>"
      ""))


(defun code-enums (stream analysis)
  (format stream "enum { }~%")
)



(defun generate-alloc-enum (&optional (fout t) (anal *analysis*))
  (let ((maxenum 0))
    (format fout "enum { KIND_null = 0, ~%")
    (let ((hardwired-kinds (core:hardwired-kinds)))
      (mapc (lambda (kv) (format fout "KIND_~a = ~a, ~%" (car kv) (cdr kv))) hardwired-kinds))
    (mapc (lambda (enum)
               (format fout "~A = ~A,~%" (enum-name enum) (enum-value enum))
               (when (> (enum-value enum) maxenum)
                 (setq maxenum (enum-value enum))))
             (analysis-sorted-enums anal)
             )
    (format fout "  KIND_max = ~a~%" maxenum)
    (format fout "}~%" )
    ))


#+(or) ;; this info is provided by kind info tables
(defun impl-generate-kind-name-map (dest anal)
  (mapc (lambda (enum)
          (let* ((enum-name (enum-name enum)))
	    (with-jump-table (fout jti dest enum)
			      (format fout "return \"~A\";~%" enum-name))))
	(analysis-sorted-enums anal))
	)




(defun is-alloc-p (ctype project)
  "Returns true if the ctype is an object allocated using a template function in gcalloc.
These are objects that are directly managed by the garbage collector.
Pointers to these objects are fixed in obj_scan or they must be roots."
  (or (gethash (ctype-key ctype) (project-rootclassallocs project))
      (gethash (ctype-key ctype) (project-classallocs project))
      (gethash (ctype-key ctype) (project-lispallocs project))
      (gethash (ctype-key ctype) (project-containerallocs project))))
      

(defgeneric fix-variable-p (var analysis))
(defmethod fix-variable-p ((var global-variable) analysis)
  (fix-variable-p (global-variable-ctype var) analysis))
(defmethod fix-variable-p ((var unclassified-ctype) analysis) nil)
(defmethod fix-variable-p ((var lvalue-reference-ctype) analysis) nil)
(defmethod fix-variable-p ((var builtin-ctype) analysis) nil)
(defmethod fix-variable-p ((var dependent-name-ctype) analysis) nil)
(defmethod fix-variable-p ((var template-type-parm-ctype) analysis) nil)
(defmethod fix-variable-p ((var enum-ctype) analysis) nil)
(defmethod fix-variable-p ((var smart-ptr-ctype) analysis) t)
(defmethod fix-variable-p ((var tagged-pointer-ctype) analysis) t)
(defmethod fix-variable-p ((var class-template-specialization-ctype) analysis) nil)
(defmethod fix-variable-p ((var constant-array-ctype) analysis) nil)
(defmethod fix-variable-p ((var pointer-ctype) analysis)
  (let ((pointee (pointer-ctype-pointee var)))
    (cond
      ((fixable-pointee-p pointee) t)
      ((ignorable-ctype-p pointee) nil)
      ((is-alloc-p (pointer-ctype-pointee var) (analysis-project analysis)) t)
      (t (warn "Handle fix-variable-p for ~a" var)))))
(defmethod fix-variable-p ((var cxxrecord-ctype) analysis) 
  (contains-fixptr-p var (analysis-project analysis)))

(defmethod fix-variable-p ((var injected-class-name-ctype) analysis) nil)

  
(defgeneric fix-macro-name (var))                           
(defmethod fix-macro-name ((var global-variable))
  (fix-macro-name (global-variable-ctype var)))
(defmethod fix-macro-name ((var smart-ptr-ctype)) "SMART_PTR_FIX")
(defmethod fix-macro-name ((var tagged-pointer-ctype)) "TAGGED_POINTER_FIX")
(defmethod fix-macro-name ((var pointer-ctype)) "SIMPLE_POINTER_FIX")
(defmethod fix-macro-name ((var cxxrecord-ctype)) "RECORD_FIX")

(defgeneric validate-macro-name (var))                           
(defmethod validate-macro-name ((var global-variable))
  (validate-macro-name (global-variable-ctype var)))
(defmethod validate-macro-name ((var smart-ptr-ctype)) "SMART_PTR_VALIDATE")
(defmethod validate-macro-name ((var tagged-pointer-ctype)) "TAGGED_POINTER_VALIDATE")
(defmethod validate-macro-name ((var pointer-ctype)) "SIMPLE_POINTER_VALIDATE")
(defmethod validate-macro-name ((var cxxrecord-ctype)) "RECORD_VALIDATE")


(defun generate-code-for-global-non-symbol-variables (stream analysis)
  (maphash (lambda (k v)
             (when (and (fix-variable-p v analysis) (not (search "_sym_" (global-variable-name v))))
               (format stream " ~a(~a);~%" (fix-macro-name v) (global-variable-name v))))
           (project-global-variables (analysis-project analysis)))
  )

(defun generate-code-for-global-symbols (stream analysis)
  (maphash (lambda (k v)
             (when (and (fix-variable-p v analysis) (search "_sym_" (global-variable-name v)))
               (format stream " ~a(~a);~%" (fix-macro-name v) (global-variable-name v))))
           (project-global-variables (analysis-project analysis)))
  )

#|
(maphash (lambda (k v)
             (break "Check v for what type it is")
             (let ((fix (contains-smart-pointers-p v analysis)))
               (when fix (format stream "    ~a(~a);~%" (macro-name v fix) (global-variable-name v) v))))
           (project-global-variables (analysis-project analysis)))
  (format stream "#endif // ifdef GLOBAL_VARIABLES~%")
  )
|#

(defun generate-helper-table (dest)
  (format (destination-stream dest) "~a~%" (get-output-stream-string (destination-helper-stream dest))))


(defun generate-label-table (dest)
  (multiple-value-bind (hardwired-kinds  ignore-classes first-general)
      (core:hardwired-kinds)
    (format (destination-stream dest)
            "static void* ~a_table[] = { ~%"
            (destination-table-name dest))
    (let ((entries (reverse (destination-label-list dest))))
      (dolist (entry entries)
        (format (destination-stream dest) "  /* ~a */ &&~a,~%" (car entry) (cdr entry))))
    (format (destination-stream dest) "   NULL~%" )
    (format (destination-stream dest) "};~%")))

(defmacro do-generator (stream analysis &key table-name function-declaration function-prefix function-table-type generator jump-table-index-function)
  (let ((dest-gs (gensym)))
    `(let ((,dest-gs (make-destination :stream ,stream
				       :table-name ,table-name
				       :label-prefix ,function-prefix)))
       (format ,stream "#if defined(GC_~a)~%" ,table-name)
       (funcall ,generator ,dest-gs ,analysis)
       (format stream "#endif // defined(GC_~a)~%" ,table-name)
       (format stream "#if defined(GC_~a_HELPERS)~%" ,table-name)
       (generate-helper-table ,dest-gs)
       (format stream "#endif // defined(GC_~a_HELPERS)~%" ,table-name)
       (format stream "#if defined(GC_~a_TABLE)~%" ,table-name)
       (generate-label-table ,dest-gs)
       (format stream "#endif // defined(GC_~a_TABLE)~%" ,table-name))))

(defun generate-code (analysis &key test )
  (let ((filename (if test
                      "test_clasp_gc"
                    "clasp_gc")))
    (with-open-file (stream (make-pathname :name filename :type "cc" :defaults (clang-tool:main-pathname)) :direction :output :if-exists :supersede)
                    (format stream "#ifdef DECLARE_FORWARDS~%")
                    (code-for-namespace-names stream (merge-forward-names-by-namespace analysis))
                    (format stream "#endif // DECLARE_FORWARDS~%")
                    (format stream "#if defined(GC_ENUM)~%")
                    (generate-alloc-enum stream analysis)
                    (format stream "#endif // defined(GC_ENUM)~%")
                    (format stream "#if defined(GC_DYNAMIC_CAST)~%")
                    (generate-dynamic-cast-code stream analysis )
                    (format stream "#endif // defined(GC_DYNAMIC_CAST)~%")
                    (format stream "#if defined(GC_KIND_SELECTORS)~%")
                    (generate-gckind-for-enums stream analysis)
                    (format stream "#endif // defined(GC_KIND_SELECTORS)~%")
		    #+(or)(do-generator stream analysis
				  :table-name "OBJ_SKIP"
				  :function-declaration "mps_addr_t ~a(mps_addr_t client)"
				  :function-prefix "obj_skip"
				  :function-table-type "mps_addr_t (*OBJ_SKIP_table[])(mps_addr_t)"
				  :generator (lambda (dest anal)
					       (dolist (enum (analysis-sorted-enums anal))
						 (funcall (species-skip (enum-species enum)) dest enum anal))))
		    (do-generator stream analysis
				  :table-name "OBJ_SCAN"
				  :function-declaration "GC_RESULT ~a(mps_ss_t& ss, mps_addr_t& client, mps_addr_t limit)"
				  :function-prefix "obj_scan"
				  :function-table-type "GC_RESULT (*OBJ_SCAN_table[])(mps_ss_t& ss, mps_addr_t& client, mps_addr_t limit)"
                                  :jump-table-index-function 'scanner-jump-table-index-for-enum-name
				  :generator (lambda (dest anal)
					       (dolist (enum (analysis-sorted-enums anal))
						 (funcall (species-scan (enum-species enum)) dest enum anal))))
		    (do-generator stream analysis
				  :table-name "OBJ_FINALIZE"
				  :function-declaration "void ~a(mps_addr_t client)"
				  :function-prefix "obj_finalize"
				  :function-table-type "void (*OBJ_FINALIZE_table[])(mps_addr_t client)"
				  :generator (lambda (dest anal)
					       (dolist (enum (analysis-sorted-enums anal))
						 (funcall (species-finalize (enum-species enum)) dest enum anal))))
		    (do-generator stream analysis
				  :table-name "OBJ_DEALLOCATOR"
				  :function-declaration "void ~a(mps_addr_t client)"
				  :function-prefix "obj_deallocate_unmanaged_instance"
				  :function-table-type "void (*OBJ_DEALLOCATOR_table[])(mps_addr_t client)"
				  :generator (lambda (dest anal)
					       (dolist (enum (analysis-sorted-enums anal))
						 (funcall (species-deallocator (enum-species enum)) dest enum anal))))
                    (format stream "#if defined(GC_GLOBALS)~%")
                    (generate-code-for-global-non-symbol-variables stream analysis)
                    (format stream "#endif // defined(GC_GLOBALS)~%")
                    (format stream "#if defined(GC_GLOBAL_SYMBOLS)~%")
                    (generate-code-for-global-symbols stream analysis)
                    (format stream "#endif // defined(GC_GLOBAL_SYMBOLS)~%")
                    )))

(defun build-arguments-adjuster ()
  "Build a function that fixes up compile command arguments.
It converts relative -I../... arguments to absolute paths"
  (lambda (args filename) 
    (let ((result (concatenate 'vector #-quiet args #+quiet(remove "-v" args)
                               (vector "-DUSE_MPS"
                                       "-DRUNNING_GC_BUILDER"))))
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
    (setup-global-variable-search tools)
    (setup-variable-search tools)
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

(defparameter *max-parallel-searches* (parse-integer (ext:getenv "PJOBS")))

(defun split-jobs (job-list num-parallel)
  (let ((jobvec (make-array num-parallel)))
    (do* ((i 0 (mod (1+ i) num-parallel))
          (one-job (pop job-list) (pop job-list)))
         ((null one-job) (loop for idx below (length jobvec) collect (elt jobvec idx)))
     (push one-job (elt jobvec i)))))



#+(or)(defun run-job (proc job-list compilation-tool-database)
        (setf (clang-tool:multitool-results *tools*) (make-project))
        (format t "====== Running jobs in fork #~a: ~a~%" proc job-list)
        (clang-tool:batch-run-multitool *tools*
                                        :compilation-tool-database compilation-tool-database)
        (format t "------------ About to save-archive --------------~%")
        (save-data (clang-tool:multitool-results *tools*)
                   (merge-pathnames "project.dat" (clang-tool:main-pathname compilation-tool-database)))
        (core:exit))

#+(or)(defvar *parallel-search-pids* nil)
#+(or)(defun parallel-search-all (&key test one-at-a-time)
        "Run *max-parallel-searches* processes at a time - whenever one finishes, start the next"
        (setq *parallel-search-pids* nil)
        (let ((all-jobs (split-jobs (if test
                                        test
                                        (reverse (lremove (lremove $* ".*mps\.c$") ".*gc_interface\.cc$")))
                                    *max-parallel-searches*
                                    ))
              (spare-processes (if one-at-a-time 1 *max-parallel-searches*)))
          (save-data all-jobs (merge-pathnames "project-all.dat" (clang-tool:main-pathname compilation-tool-database)))
          (format t "all-jobs: ~a~%" all-jobs)
          (dotimes (proc (length all-jobs))
            (setq spare-processes (1- spare-processes))
            (ext:system "sleep 1")
            (let* ((job-list (elt all-jobs proc))
                   (pid (core:fork)))
              (if (eql 0 pid)
                  (run-job proc job-list)
                  (when (eql spare-processes 0)
                    (core:waitpid -1 0)
                    (setq spare-processes (1+ spare-processes)))))
            (format t "Bottom of loop proc: ~a~%" proc))
          (dotimes (proc (1- *max-parallel-searches*))
            (core:waitpid -1 0))
          (format t "~%!~%!  Done ~%!~%")
          (parallel-merge)))

#+(or)(defun parallel-merge (compilation-tool-database &key end (start 0) restart)
        "Merge the analyses generated from the parallel search"
        (let* ((all-jobs (load-data (merge-pathnames "project-all.dat" (clang-tool:main-pathname compilation-tool-database))))
               (merged (if restart
                           (progn
                             (format t "Loading existing project and restarting from ~a~%" start)
                             (load-project))
                           (make-project)))
               (endnum (if (null end)
                           (length all-jobs)
                           end)))
          (setq *project* merged)
          (format t "Starting load/merge loop from ~a up to ~a~%" start endnum)
          (do ((proc start (1+ proc)))
              ((>= proc endnum) nil)
            (format t "Marking memory with ~a~%" (1+ proc))
            (gctools:gc-marker (1+ proc))
            (format t "Loading project for job ~a~%" proc)
            (let* ((project-dat-name (probe-file (project-pathname (format nil "project~a" proc) "dat")))
                   (one (if project-dat-name
                            (load-data (project-pathname (format nil "project~a" proc) "dat"))
                            nil)))
              (if one
                  (progn
                    (format t "     merging...~%")
                    (merge-projects merged one))
                  (format t "File not found.~%"))))
          (save-project)
          merged))


(defun run-test ()
  (defparameter *test-matcher*
    '(:cxxrecord-decl
      ;;        (:is-definition)
      ;;        (:is-template-instantiation)
      (:matches-name ".*GCInfo.*")))
  (match-count *test-matcher*
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



(defun serial-search-only (&key test arguments-adjuster)
    (setup-*tools*)
    (serial-search-all :test test :arguments-adjuster arguments-adjuster))
(export 'serial-search-only)

(defun serial-search-all (compilation-tool-database &key (save-project t))
  "* Arguments
- test :: A list of files to run the search on, or NIL for all of them.
- arguments-adjuster :: The arguments adjuster.
* Description
Run searches in *tools* on the source files in the compilation database."
  (format t "serial-search-all --> getcwd: ~a~%" (ext:getcwd))
  (let ((tools (setup-tools compilation-tool-database))
        (all-jobs (clang-tool:source-namestrings compilation-tool-database)))
    (save-data all-jobs (merge-pathnames #P"project-all.dat" (clang-tool:main-pathname compilation-tool-database)))
    (format t "all-jobs: ~a~%" all-jobs)
    (setf (clang-tool:multitool-results tools) (make-project))
    (clang-tool:batch-run-multitool tools
                         :compilation-tool-database compilation-tool-database)
    (when save-project
      (let ((project (clang-tool:multitool-results tools)))
        (save-data project (merge-pathnames #P"project.dat" (clang-tool:main-pathname compilation-tool-database)))))))

(defun translate-include (args filename)
  "* Arguments
- args :: A vector of strings (compilation arguments)
* Description
Convert -Iinclude to -I<main-sourcefile-pathname>/include. Uses dynamic variable *main-directory-namestring*."
  (let ((main-directory-namestring (namestring (make-pathname :name nil :type nil :defaults (clang-tool:main-pathname)))))
    (dotimes (i (length args))
      (when (string= (elt args i) "-Iinclude")
        (setf (elt args i) (format nil "-I~a/include" main-directory-namestring))))
    args))



   
(defun setup-clasp-analyzer-compilation-tool-database (pathname &key (selection-pattern ".*") source-path-identifier arguments-adjuster)
  "* Arguments
- pathname :: The pathname to the compilation database for clasp analyzer.
- selection-pattern : A regex pattern for selecting a subset of the source files for testing.
- source-path-identifier :: A string
* Description
Return a compilation database for analyzing the clasp (or some other clasp derived project) source code.
Two files (mps.c and gc_interface.cc) are removed from the list of files that clasp-analyzer will run on.
If the source location of a match contains the string source-path-identifier then that match is processed."
  (let* ((compilation-tool-database (clang-tool:load-compilation-tool-database
                                     pathname
                                     :convert-relative-includes-to-absolute t
                                     :source-path-identifier source-path-identifier))
         (source-filenames (clang-tool:select-source-namestrings compilation-tool-database selection-pattern))
         (regex-mps-dot-c (core:make-regex ".*mps\.c$"))
         (removed-mps-dot-c (remove-if #'(lambda (x) (core:regex-matches regex-mps-dot-c x)) source-filenames))
         (regex-gc_interface (core:make-regex ".gc_interface\.cc$"))
         (removed-gc_interface (remove-if (lambda (x) (core:regex-matches regex-gc_interface x)) removed-mps-dot-c)))
    (setf (clang-tool:source-namestrings compilation-tool-database) removed-gc_interface)
    (push #'translate-include (clang-tool:arguments-adjuster-list compilation-tool-database))
    (when arguments-adjuster (push arguments-adjuster (clang-tool:arguments-adjuster-list compilation-tool-database)))
    compilation-tool-database))

(defvar *analysis*)
(defun search/generate-code (compilation-tool-database)
  (clang-tool:with-compilation-tool-database compilation-tool-database
    (setf *project* (serial-search-all compilation-tool-database))
    (let ((analysis (analyze-project *project*)))
      (setq *analysis* analysis)
      (setf (analysis-inline analysis) '("core::Cons_O"))
      (generate-code analysis))
    *project*))

(defun analyze-only (compilation-tool-database)
  (clang-tool:with-compilation-tool-database compilation-tool-database
    (let ((analysis (analyze-project *project*)))
      (setf (analysis-inline analysis) '("core::Cons_O"))
      (generate-code analysis))
    *project*))

