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


(defstruct instance-variable
  "Represent an instance variable, it's name, it's source-location and it's classified type"
  field-name
  location
  ctype
  )



(defstruct alloc
  key
  name ;; decl name
  location
  ctype)

(defstruct (lispalloc (:include alloc)))
(defstruct (classalloc (:include alloc)))
(defstruct (rootclassalloc (:include alloc)))
(defstruct (containeralloc (:include alloc)))



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


(defun notify-parents (class-name analysis)
  (let* ((project (analysis-project analysis))
         (class (gethash class-name (project-classes project)))
         (base-names (append (cclass-bases class) (cclass-vbases class))))
    (if (and base-names (not (string= class-name "core::T_O")))
        (progn
          (when (> (length base-names) 1)
            (format t "Class ~a has multiple bases: ~a~%" class-name base-names))
          (dolist (class-base-name base-names)
            (multiple-value-bind (parent-enum parent-enum-p)
                (gethash class-base-name (analysis-enums analysis))
              (if parent-enum-p
                  (push class-name (enum-children parent-enum))
                (format t "Not informing ~a that it has the child ~a because it is outside of the enum hierarchy~%" class-base-name class-name))
              )))
      (push class-name (analysis-enum-roots analysis)))))

(defun build-hierarchy (analysis)
  (format t "------Analyzing class hierarchy~%")
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



(defstruct ctype key)
(defstruct (cloned-ctype (:include ctype)))
(defstruct (simple-ctype (:include cloned-ctype)))
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
     (format t "Add support for record-key for ~a  get-name->~a~%" decl-node (decl-name decl-node))
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
  (make-constant-array-ctype :element-type (classify-ctype (cast:get-type-ptr-or-null (cast:get-element-type x)))))


(defmethod classify-ctype ((x cast:paren-type))
  (make-paren-ctype :inner (classify-ctype (cast:get-type-ptr-or-null (cast:get-inner-type x)))))


(defmethod classify-ctype ((x t))
  (let ((key (record-key x)))
;;    (warn "Add support for classify-ctype to recognize ~a  key: ~a~%" x key)
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

(defun load-project ()
  (let ((project (load-data (project-pathname "project" "dat"))))
    project))
        



;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;;
;; Search for every regular class - get base classes, fields and methods
;;

(defparameter *class-matcher*
  '(:bind :whole
    (:record-decl
     ;; The class must be a definition
     (:is-definition))))
(clang-tool:compile-matcher *class-matcher*)


(defparameter *base-class-submatcher*
  (clang-tool:compile-matcher '(:record-decl
                     (:is-derived-from
                      (:for-each
                       (:record-decl
                        (:bind :base-name (:record-decl))
                        (:has-ancestor
                         (:namespace-decl
                          (:bind :base-ns (:namespace-decl))))
                        ))))))
(or *base-class-submatcher*
    (error "Problem encountered compiling *base-class-submatcher*"))



(defparameter *field-submatcher*
  (clang-tool:compile-matcher '(:record-decl
                     (:for-each ;; -descendant
                      (:field-decl
                       (:bind :field (:field-decl))
                       )))))

(or *field-submatcher*
    (error "Problem encountered compiling *field-submatcher*"))


(defparameter *metadata-submatcher*
  (clang-tool:compile-matcher
   '(:record-decl
     (:for-each ;; -descendant
      (:record-decl
       (:matches-name "metadata_.*")
       (:bind :metadata (:record-decl)))))))


(defparameter *method-submatcher*
  (clang-tool:compile-matcher '(:record-decl
                     (:for-each ;; -descendant
                      (:method-decl
                       (:bind :method (:method-decl)))))))


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
                  (clang-tool:mtag-node match-info :whole)
                  (clang-tool:ast-context match-info)
                  (lambda (minfo)
                    (let* ((field-node (clang-tool:mtag-node minfo :field))
                           (type (to-canonical-type (cast:get-type field-node))))
                      (gclog "      >> Field: ~30a~%" field-node)
                      (handler-case
                          (push (make-instance-variable
                                 :location (clang-tool:mtag-loc-start minfo :field)
                                 :field-name (clang-tool:mtag-name minfo :field)
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
                             :matcher (clang-tool:compile-matcher *class-matcher*)
                             :initializer (lambda () (setf results (make-hash-table :test #'equal)))
                             :callback (make-instance 'clang-tool:code-match-callback :match-code (function %%class-callback))))))




;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;;
;; Search for lispallocs   as template parameters for GCObjectAllocator
;;


(defparameter *lispalloc-matcher*
  '(:record-decl
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
                             :matcher (clang-tool:compile-matcher `(:bind :whole ,*lispalloc-matcher*))
                             :initializer (lambda () (setf class-results (make-hash-table :test #'equal))) ; initializer
                             :callback (make-instance 'clang-tool:code-match-callback :match-code (function %%lispalloc-matcher-callback))))))




;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;;
;; Search for classallocs - they are template parameters for ClassAllocator
;;


(defparameter *classalloc-matcher*
  '(:record-decl
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
                             :matcher (clang-tool:compile-matcher `(:bind :whole ,*classalloc-matcher*))
                             :initializer (lambda () (setf class-results (make-hash-table :test #'equal))) ; initializer
                             :callback (make-instance 'clang-tool:code-match-callback :match-code (function %%classalloc-matcher-callback))))))





;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;;
;; Search for rootclassallocs - they are template parameters for Rootclassallocator
;;


(defparameter *rootclassalloc-matcher*
  '(:record-decl
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
                             :matcher (clang-tool:compile-matcher `(:bind :whole ,*rootclassalloc-matcher*))
                             :initializer (lambda () (setf class-results (make-hash-table :test #'equal))) ; initializer
                             :callback (make-instance 'clang-tool:code-match-callback :match-code (function %%rootclassalloc-matcher-callback))))))

;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;;
;; Search for containerallocs - they are template parameters for GCContainerAllocator
;;

(defparameter *containeralloc-matcher*
  '(:record-decl
    (:is-definition)
    (:is-template-instantiation)
    (:is-same-or-derived-from
     (:record-decl
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
                             :matcher (clang-tool:compile-matcher `(:bind :whole ,*containeralloc-matcher*))
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
                             :matcher (clang-tool:compile-matcher *global-variable-matcher*)
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
                             :matcher (clang-tool:compile-matcher *variable-matcher*)
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
  '(:record-decl
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
                             :matcher (clang-tool:compile-matcher `(:bind :whole ,*gcinfo-matcher*))
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
             (let* ((field-ctype (instance-variable-ctype field))
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

(defmethod contains-fixptr-impl-p ((x cxxrecord-ctype) project)
  (cond
    ((string= (cxxrecord-ctype-key x) "(my-anonymous-class-name)") nil)
    (t (let ((c (gethash (ctype-key x) (project-classes project))))
         (if c
             (contains-fixptr-impl-p x project)
             nil)))))

(defmethod contains-fixptr-impl-p ((x injected-class-name-ctype) project) nil)
(defmethod contains-fixptr-impl-p ((x unclassified-ctype) project) nil)
(defmethod contains-fixptr-impl-p ((x unclassified-template-specialization-ctype) project) nil)
(defmethod contains-fixptr-impl-p ((x smart-ptr-ctype) project) t)
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

(defun fill-forward-declarations (forwards enum)
  (when (simple-enum-p enum)
    (expand-forwards-with-template-arguments forwards (alloc-ctype (simple-enum-alloc enum)))))
  

(defun generate-forward-declarations (&optional (analysis *analysis*))
  (let* ((forwards (analysis-forwards analysis)))
    (maphash (lambda (key enum) (fill-forward-declarations forwards enum)) (analysis-enums analysis))))





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
         (name name4)
         )
    name))








(defstruct species
  name
  preprocessor-guard ;; defines a preprocessor symbol XXX used to wrap #ifdef XXX/#endif around GCKind
  discriminator ;; Function - takes one argument, returns a species index
  scan          ;; Function - generates scanner for species
  skip          ;; Function - generates obj_skip code for species
  finalize      ;; Function - generates obj_finalize code for species
  deallocator   ;; Function - generates obj_deallocate_unmanaged_instance code for species
  validator      ;; Function - generates validator for species
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
         nil))
    ))

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
    (maphash (lambda (k alloc) (analyze-alloc-and-assign-species-and-enum alloc analysis)) (project-rootclassallocs project))
    ))




(defstruct array-fixer
  element-type)

(defstruct pointer-fixer
  pointee-type)


(defgeneric fixer-macro-name (fixer-head))
(defmethod fixer-macro-name ((x (eql :smart-ptr-fix))) "SMART_PTR_FIX")
(defmethod fixer-macro-name ((x (eql :tagged-pointer-fix))) "TAGGED_POINTER_FIX")

(defgeneric validator-macro-name (validator-head))
(defmethod validator-macro-name ((x (eql :smart-ptr-fix))) "SMART_PTR_VALIDATE")
(defmethod validator-macro-name ((x (eql :tagged-pointer-fix))) "TAGGED_POINTER_VALIDATE")

(defun scanner-code-for-instance-var (stream fh variable-type struct-name ptr-name instance-var)
  (let* ((fixer-macro (fixer-macro-name (car (last instance-var))))
         (variable-chain (butlast instance-var 1))
         (instance-vars (mapcar (lambda (f) (instance-variable-field-name f)) variable-chain)))
    (format stream "    ~a(~a->~{~a~^.~});~%" fixer-macro ptr-name instance-vars)
    (format fh "    { ~a_field_fix, offsetof(MACRO_SAFE_TYPE(~a),~{~a~^.~}),\"~{~a~^.~}\" }, ~%"
            variable-type struct-name instance-vars instance-vars )))

(defun validator-code-for-instance-var (stream ptr-name instance-var)
  (let* ((validator-macro (validator-macro-name (car (last instance-var))))
         (variable-chain (butlast instance-var 1)))
    (format stream "    ~a(~a->~{~a~^.~});~%" validator-macro ptr-name
            (mapcar (lambda (f) (instance-variable-field-name f)) variable-chain))))

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



(defun field-data (key instance-var)
  (let ((type (car (last instance-var)))
        names)
    (dolist (field (butlast instance-var))
      (push (instance-variable-field-name field) names))
    (let ((reverse-names (nreverse names)))
      (format nil " { field_fix, offsetof(MACRO_SAFE_TYPE(~a),~{~a~^.~}), ~s }"
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
      (format fh "{ class_kind, ~d, ~s },~%" enum-name key)
      (format fh "{ class_size, sizeof(~a), \"\" },~%" key )
      (let ((all-instance-variables
             (fix-code (gethash key (project-classes (analysis-project anal))) anal)))
        (dolist (instance-var all-instance-variables)
          (format fh "~a,~%" (field-data key instance-var)))))))

(defun validator-for-lispallocs (dest enum anal)
  ;; Do nothing - uses table
  )

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
    (format fout "    GCObjectAllocator<~A>::deallocate_unmanaged_instance(~A);~%" key +ptr-name+)
    (format fout "    return;~%"))))



(defun scanner-for-templated-lispallocs (dest enum anal)
  (assert (templated-enum-p enum))
  (let* ((key (enum-key enum))
         (enum-name (enum-name enum)))
    (gclog "build-mps-scan-for-one-family -> inheritance key[~a]  value[~a]~%" key value)
    (with-jump-table (fout jti dest enum "goto SCAN_ADVANCE")
      (let ((fh (destination-helper-stream dest)))
        (format fh "{ templated_class_kind, ~d, ~s },~%" enum-name key)
        (format fh "{ templated_class_jump_table_index, ~d, \"\" },~%" jti)
        (format fout "    ~A* ~A = reinterpret_cast<~A*>(client);~%" key +ptr-name+ key)
        (let ((all-instance-variables (fix-code (gethash key (project-classes (analysis-project anal))) anal)))
          (dolist (instance-var all-instance-variables)
            (scanner-code-for-instance-var fout fh "templated_class" key +ptr-name+ instance-var)))
        (format fout "    size = ~a->templatedSizeof();" +ptr-name+)))))

(defun validator-for-templated-lispallocs (dest enum anal)
  (assert (templated-enum-p enum))
  (let* ((key (enum-key enum))
         (enum-name (enum-name enum)))
    (with-jump-table (fout jti dest enum "goto VALIDATE_ADVANCE")
      (format fout "    ~A* ~A = reinterpret_cast<~A*>(client);~%" key +ptr-name+ key)
      (let ((all-instance-variables (fix-code (gethash key (project-classes (analysis-project anal))) anal)))
        (dolist (instance-var all-instance-variables)
          (validator-code-for-instance-var fout +ptr-name+ instance-var))))))

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
    (format fout "    GCObjectAllocator<~A>::deallocate_unmanaged_instance(~A);~%" key +ptr-name+)
    )))




(defun scanner-for-gccontainer (dest enum anal)
  (check-type enum simple-enum)
  (let* ((alloc (simple-enum-alloc enum))
         (decl (containeralloc-ctype alloc))
         (key (alloc-key alloc))
         (enum-name (enum-name enum)))
    (with-jump-table (fout jti dest enum "goto SCAN_ADVANCE")
      (let ((fh (destination-helper-stream dest)))
        (format fh "{ container_kind, ~d, ~s },~%" enum-name key)
        (format fh "{ container_jump_table_index, ~d, \"\" },~%" jti)
        (format fh "{ container_content_size, sizeof(~a::value_type), \"~a\" },~%" key  key)
        (if (cxxrecord-ctype-p decl)
            (progn
              (format fout "    THROW_HARD_ERROR(BF(\"Should never scan ~a\"));~%" (cxxrecord-ctype-key decl)))
            (let* ((parms (class-template-specialization-ctype-arguments decl))
                   (parm0 (car parms))
                   (parm0-ctype (gc-template-argument-ctype parm0)))
              ;;          (format fout "// parm0-ctype = ~a~%" parm0-ctype)
              (format fout "    ~A* ~A = reinterpret_cast<~A*>(client);~%" key +ptr-name+ key)
              (format fout "    for (~a::iterator it = ~a->begin(); it!=~a->end(); ++it) {~%" key +ptr-name+ +ptr-name+)
              ;;          (format fout "        // A scanner for ~a~%" parm0-ctype)
              (cond
                ((smart-ptr-ctype-p parm0-ctype)
                 (format fout "          ~a(*it);~%" (fix-macro-name parm0-ctype))
                 (format fh "{ container_field_fix, 0, \"~a-only\" },~%" (fix-macro-name parm0-ctype)))
                ((pointer-ctype-p parm0-ctype)
                 (format fout "          ~a(*it);~%" (fix-macro-name parm0-ctype))
                 (format fh "{ container_field_fix, 0, \"~a-only\" },~%" (fix-macro-name parm0-ctype)))
                ((tagged-pointer-ctype-p parm0-ctype)
                 (format fout "          ~a(*it);~%" (fix-macro-name parm0-ctype))
                 (format fh "{ container_field_fix, 0, \"~a-only\" },~%" (fix-macro-name parm0-ctype)))
                ((cxxrecord-ctype-p parm0-ctype)
                 (let ((all-instance-variables (fix-code (gethash (ctype-key parm0-ctype) (project-classes (analysis-project anal))) anal)))
                   (dolist (instance-var all-instance-variables)
                     (scanner-code-for-instance-var fout fh "container"
                                                    (format nil "~a::value_type" key)
                                                    "it" instance-var))))
                ((class-template-specialization-ctype-p parm0-ctype)
                 (let ((all-instance-variables (fix-code (gethash (ctype-key parm0-ctype) (project-classes (analysis-project anal))) anal)))
                   (dolist (instance-var all-instance-variables)
                     (scanner-code-for-instance-var fout fh "container"
                                                    (format nil "~a::value_type" key)
                                                    "it" instance-var))))
                (t (error "Write code to scan ~a" parm0-ctype)))

              
              (format fout "    }~%")
              (format fout "    typedef typename ~A type_~A;~%" key enum-name)
              (format fout "    size = sizeof_container<type_~a>(~a->capacity());" enum-name +ptr-name+)))))))

(defun validator-for-gccontainer (dest enum anal)
  (check-type enum simple-enum)
  (let* ((alloc (simple-enum-alloc enum))
         (decl (containeralloc-ctype alloc))
         (key (alloc-key alloc))
         (enum-name (enum-name enum)))
    (with-jump-table (fout jti dest enum "goto VALIDATE_ADVANCE")
      (if (cxxrecord-ctype-p decl)
          (progn
            (format fout "    THROW_HARD_ERROR(BF(\"Should never scan ~a\"));~%" (cxxrecord-ctype-key decl)))
          (let* ((parms (class-template-specialization-ctype-arguments decl))
                 (parm0 (car parms))
                 (parm0-ctype (gc-template-argument-ctype parm0)))
            ;;          (format fout "// parm0-ctype = ~a~%" parm0-ctype)
            (format fout "    ~A* ~A = reinterpret_cast<~A*>(client);~%" key +ptr-name+ key)
            (format fout "    for (~a::iterator it = ~a->begin(); it!=~a->end(); ++it) {~%" key +ptr-name+ +ptr-name+)
            ;;          (format fout "        // A scanner for ~a~%" parm0-ctype)
            (cond
              ((smart-ptr-ctype-p parm0-ctype)
               (format fout "          ~a(*it);~%" (validate-macro-name parm0-ctype)))
              ((tagged-pointer-ctype-p parm0-ctype)
               (format fout "          ~a(*it);~%" (validate-macro-name parm0-ctype)))
              ((cxxrecord-ctype-p parm0-ctype)
               (let ((all-instance-variables (fix-code (gethash (ctype-key parm0-ctype) (project-classes (analysis-project anal))) anal)))
                 (dolist (instance-var all-instance-variables)
                   (validator-code-for-instance-var fout "it" instance-var))))
              ((class-template-specialization-ctype-p parm0-ctype)
               (let ((all-instance-variables (fix-code (gethash (ctype-key parm0-ctype) (project-classes (analysis-project anal))) anal)))
                 (dolist (instance-var all-instance-variables)
                   (validator-code-for-instance-var fout "it" instance-var))))
              ((pointer-ctype-p parm0-ctype)
               (format fout "          ~a(*it);~%" (validate-macro-name parm0-ctype)))
              (t (error "Write code to validate ~a" parm0-ctype)))
            (format fout "    }~%"))))))


(defun skipper-for-gccontainer (dest enum anal)
  (check-type enum simple-enum)
  (let* ((alloc (simple-enum-alloc enum))
         (decl (containeralloc-ctype alloc))
         (key (alloc-key alloc))
         (enum-name (enum-name enum)))
    (with-jump-table (fout jti dest enum)
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
    (with-jump-table (fout jti dest enum "goto BOTTOM")
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
    (with-jump-table (fout jump-table-index dest enum "goto SCAN_ADVANCE")
      (let ((fh (destination-helper-stream dest)))
        (format fh "{ container_kind, ~d, ~s },~%" enum-name key)
        (format fh "{ container_jump_table_index, ~d, \"\" },~%" jump-table-index))
      (format fout "    // Should never be invoked~%"))))


(defun validator-for-gcstring (dest enum anal)
  (assert (simple-enum-p enum))
  (let* ((alloc (simple-enum-alloc enum))
         (key (alloc-key alloc))
         (enum-name (enum-name enum)))
    (with-jump-table (fout jump-table-index dest enum "goto VALIDATE_ADVANCE")
      (format fout "    // Should never be invoked~%"))))


(defun skipper-for-gcstring (dest enum anal)
  (check-type enum simple-enum)
  (let* ((alloc (simple-enum-alloc enum))
         (decl (containeralloc-ctype alloc))
         (key (alloc-key alloc))
         (enum-name (enum-name enum)))
    (with-jump-table (fout jti dest enum)
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

(defparameter *analysis* nil)
(defun setup-manager ()
  (let* ((manager (make-manager)))
    (add-species manager (make-species :name :bootstrap
                                       :discriminator (lambda (x) (and (lispalloc-p x)
                                                                       (gctools::bootstrap-kind-p (alloc-key x))))
                                       :scan 'scanner-for-lispallocs
                                       :skip 'skipper-for-lispallocs
                                       :dump 'dumper-for-lispallocs
                                       :validator 'validator-for-lispallocs
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
                                       :validator 'validator-for-lispallocs
                                       :finalize 'finalizer-for-lispallocs
                                       :deallocator 'deallocator-for-lispallocs
                                       ))
    (add-species manager (make-species :name :templated-lispalloc
                                       :discriminator (lambda (x) (and (lispalloc-p x) (alloc-template-specializer-p x *analysis*)))
                                       :scan 'scanner-for-templated-lispallocs
                                       :skip 'skipper-for-templated-lispallocs
                                       :dump 'dumper-for-templated-lispallocs
                                       :validator 'validator-for-templated-lispallocs
                                       :finalize 'finalizer-for-templated-lispallocs
                                       :deallocator 'deallocator-for-templated-lispallocs
                                       ))
    (add-species manager (make-species :name :GCVECTOR
                                       :discriminator (lambda (x) (and (containeralloc-p x) (search "gctools::GCVector" (alloc-key x))))
                                       :scan 'scanner-for-gccontainer
                                       :skip 'skipper-for-gccontainer
                                       :dump 'dumper-for-gccontainer
                                       :validator 'validator-for-gccontainer
                                       :finalize 'finalizer-for-gccontainer
                                       :deallocator 'deallocator-for-gccontainer
                                       ))
    (add-species manager (make-species :name :GCARRAY
                                       :discriminator (lambda (x) (and (containeralloc-p x) (search "gctools::GCArray" (alloc-key x))))
                                       :scan 'scanner-for-gccontainer
                                       :skip 'skipper-for-gccontainer
                                       :dump 'dumper-for-gccontainer
                                       :validator 'validator-for-gccontainer
                                       :finalize 'finalizer-for-gccontainer
                                       :deallocator 'deallocator-for-gccontainer
                                       ))
    (add-species manager (make-species :name :GCSTRING
                                       :discriminator (lambda (x) (and (containeralloc-p x) (search "gctools::GCString" (alloc-key x))))
                                       :scan 'scanner-for-gcstring ;; don't need to scan but do need to calculate size
                                       :skip 'skipper-for-gcstring
                                       :dump 'dumper-for-gcstring
                                       :validator 'validator-for-gcstring
                                       :finalize 'finalizer-for-gcstring
                                       :deallocator 'deallocator-for-gcstring
                                       ))
    (add-species manager (make-species :name :classalloc
                                       :discriminator (lambda (x) (and (classalloc-p x) (not (alloc-template-specializer-p x *analysis*))))
                                       :scan 'scanner-for-lispallocs
                                       :skip 'skipper-for-lispallocs
                                       :dump 'dumper-for-lispallocs
                                       :validator 'validator-for-lispallocs
                                       :finalize 'finalizer-for-lispallocs
                                       :deallocator 'deallocator-for-lispallocs
                                       ))
    (add-species manager (make-species :name :rootclassalloc
                                       :discriminator (lambda (x) (rootclassalloc-p x))
                                       :scan 'scanner-for-lispallocs
                                       :skip 'skipper-for-lispallocs
                                       :dump 'dumper-for-lispallocs
                                       :validator 'validator-for-lispallocs
                                       :finalize 'finalizer-for-lispallocs
                                       :deallocator 'deallocator-for-lispallocs
                                       ))
    (add-species manager (make-species :name :templated-classalloc
                                       :discriminator (lambda (x) (and (classalloc-p x) (alloc-template-specializer-p x *analysis*)))
                                       :scan 'scanner-for-templated-lispallocs
                                       :skip 'skipper-for-templated-lispallocs
                                       :dump 'dumper-for-templated-lispallocs
                                       :validator 'validator-for-templated-lispallocs
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



(defun fix-code-for-field (field analysis)
  (let ((code (fix-code (instance-variable-ctype field) analysis)))
    (cond
      ((null code) nil)
      ((atom code)
       (list (list field code)))
      ((consp code)
       (mapcar (lambda (f) (cons field f)) code))
      (t
       (error "Feb 2016 I inserted this error to see if the code ever gets here") code))))

(defgeneric fix-code (x analysis))
(defmethod fix-code ((x cclass) analysis)
  (let* ((project (analysis-project analysis))
         (base-code (let (all-fixers)
                       (dolist (base-name (cclass-bases x))
                         (let ((base-fixers (fix-code (gethash base-name (project-classes project)) analysis)))
                           (when base-fixers
                             (setq all-fixers (append base-fixers all-fixers)))))
                       all-fixers))
         (vbase-code (let (all-fixers)
                       (dolist (vbase-name (cclass-vbases x))
                         (let ((vbase-fixers (fix-code (gethash vbase-name (project-classes project)) analysis)))
                           (when vbase-fixers
                             (setq all-fixers (append vbase-fixers all-fixers)))))
                       all-fixers))
         (field-code (let (all-fixers)
                       (dolist (field (cclass-fields x))
                         (let ((field-fixers (fix-code-for-field field analysis)))
                           (when field-fixers
                             (setq all-fixers (append field-fixers all-fixers))
                             )))
                       all-fixers))
         result
         )
    (when base-code (setq result (append result base-code)))
    (when vbase-code (setq result (append result vbase-code)))
    (when field-code (setq result (append result field-code)))
    result))


(defmethod fix-code ((x unclassified-ctype) analysis)
  (cond
    ((ignorable-ctype-p x) nil)
    (t (warn "ignoring fix-code for ~a" x)))
  nil)

(defmethod fix-code ((x cxxrecord-ctype) analysis)
  (let ((code (gethash (cxxrecord-ctype-key x) (project-classes (analysis-project analysis)))))
    (unless code
      (error "Could not find ~a in (project-classes (analysis-project analysis))" (cxxrecord-ctype-key x)))
    (fix-code code analysis)))

(defmethod fix-code ((x class-template-specialization-ctype) analysis)
  (cond
    ((string= (ctype-key x) "unsigned long") nil)
    (t
     (fix-code (gethash (ctype-key x) (project-classes (analysis-project analysis))) analysis))))

(defmethod fix-code ((x smart-ptr-ctype) analysis) :smart-ptr-fix)

(defmethod fix-code ((x tagged-pointer-ctype) analysis) :tagged-pointer-fix)

(defmethod fix-code ((x constant-array-ctype) analysis)
  (if (contains-fixptr-p x (analysis-project analysis))
      (make-array-fixer :element-type (constant-array-ctype-element-type x))))

(defmethod fix-code ((x pointer-ctype) analysis )
  (let ((pointee (pointer-ctype-pointee x)))
    (cond
    ((or (gcvector-moveable-ctype-p pointee)
         (gcarray-moveable-ctype-p pointee)
         (gcstring-moveable-ctype-p pointee))
        (make-pointer-fixer :pointee-type (pointer-ctype-pointee x)))
    ((is-alloc-p (pointer-ctype-pointee x) (analysis-project analysis))
     (make-pointer-fixer :pointee-type (pointer-ctype-pointee x)))
    ((fixable-pointee-p (pointer-ctype-pointee x))
     (make-pointer-fixer :pointee-type (pointer-ctype-pointee x)))
    ((ignorable-ctype-p (pointer-ctype-pointee x)) nil)
    (t
      (warn "I'm not sure if I can ignore pointer-ctype ~a  ELIMINATE THESE WARNINGS" x)
      nil))))


(defun fixable-pointee-p (ctype)
  (let ((key (ctype-key ctype)))
    (inherits-metadata key :metadata_always_fix_pointers_to_derived_classes)))

(defgeneric ignorable-ctype-p (ctype))

(defmethod ignorable-ctype-p ((ctype smart-ptr-ctype)) nil)
(defmethod ignorable-ctype-p ((ctype tagged-pointer-ctype)) nil)

(defun make-ignore-table (strings)
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (s strings)
      (setf (gethash s ht) t))
    ht))

(defparameter +ignorable-unclassified-ctype+
  (make-ignore-table '(
                       "void (void *)"
                       "void"
                       "_Bool"
                       "unsigned int"
                       "unsigned long"
                       "char"
                       "double"
                       "enum asttooling::ContextType"
                       "enum asttooling::ErrorType"
                       "enum boost::filesystem::file_type"
                       "enum boost::filesystem::perms"
                       "enum clang::InputKind"
                       "enum clang::ast_matchers::dynamic::VariantValue::ValueType"
                       "enum llvm::APFloat::fltCategory"
                       "float"
                       "int"
                       "long long"
                       "long"
                       "short"
                       "unsigned char"
                       "unsigned long long"
                       "void (core::Lisp_O *)"
                       )))


(defmethod ignorable-ctype-p ((ctype unclassified-ctype))
  (let ((key (ctype-key ctype)))
    (gethash key +ignorable-unclassified-ctype+)))



(defparameter +ignorable-cxxrecord-ctype+
  (make-ignore-table '(
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
  (make-ignore-table '(
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
(defmethod fix-variable-p ((var unclassified-ctype) analysis)
  nil)
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
    ;;; This info is provided by kind info tables
		    #+(or)(do-generator stream analysis
                                        :table-name "KIND_NAME_MAP"
                                        :function-declaration "const char* ~a()"
                                        :function-prefix "kind_name"
                                        :function-table-type "const char* (*KIND_NAME_MAP_table[])()"
                                        :generator #'impl-generate-kind-name-map)
    ;; This info isn't needed anymore                    
		    #+(or)(do-generator stream analysis
				  :table-name "OBJ_DUMP_MAP"
				  :function-declaration "string ~a(mps_addr_t client)"
				  :function-prefix "obj_dump"
				  :function-table-type "string (*OBJ_DUMP_MAP_table[])(mps_addr_t client)"
				  :generator (lambda (dest anal)
					       (dolist (enum (analysis-sorted-enums anal))
						 (funcall (species-dump (enum-species enum)) dest enum anal))))
		    (do-generator stream analysis
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
				  :table-name "OBJ_VALIDATE"
				  :function-declaration "GC_RESULT ~a(mps_ss_t& ss, mps_addr_t& client, mps_addr_t limit)"
				  :function-prefix "obj_validate"
				  :function-table-type "GC_RESULT (*OBJ_VALIDATE_table[])(mps_ss_t& ss, mps_addr_t& client, mps_addr_t limit)"
                                  :jump-table-index-function 'scanner-jump-table-index-for-enum-name
				  :generator (lambda (dest anal)
					       (dolist (enum (analysis-sorted-enums anal))
						 (funcall (species-validator (enum-species enum)) dest enum anal))))
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
  (lambda (args) 
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
                             (load-project)
                             )
                           (make-project)))
               (endnum (if (null end)
                           (length all-jobs)
                           end))
               )
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
    '(:record-decl
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
  (format t "serial-search-all --> current-dir: ~a~%" (core:current-dir))
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

(defun translate-include (args)
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

(defun search/generate-code (compilation-tool-database)
  (clang-tool:with-compilation-tool-database compilation-tool-database
    (setf *project* (serial-search-all compilation-tool-database))
    (let ((analysis (analyze-project *project*)))
      (setf (analysis-inline analysis) '("core::Cons_O"))
      (generate-code analysis))
    *project*))

(defun analyze-only (compilation-tool-database)
  (clang-tool:with-compilation-tool-database compilation-tool-database
    (let ((analysis (analyze-project *project*)))
      (setf (analysis-inline analysis) '("core::Cons_O"))
      (generate-code analysis))
    *project*))

