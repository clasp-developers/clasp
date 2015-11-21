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

(progn
  (provide 'gcb)
  (require :clang-tool)
  (use-package :ast-tooling)
  )


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
  enum-roots
  )

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
  (make-pathname :name project-name :type project-type :defaults (main-directory-pathname)))

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

(defun save-project ()
  (core::with-print-readably
      (with-open-file (fout (project-pathname "project" "dat") :direction :output)
        (prin1 *project* fout))))

(defun load-project ()
  (setq *project* (load-data (project-pathname "project" "dat")))
  *project*
  )
        



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
(compile-matcher *class-matcher*)


(defparameter *base-class-submatcher*
  (compile-matcher '(:record-decl
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
  (compile-matcher '(:record-decl
                     (:for-each ;; -descendant
                      (:field-decl
                       (:bind :field (:field-decl))
                       )))))

(or *field-submatcher*
    (error "Problem encountered compiling *field-submatcher*"))


(defparameter *metadata-submatcher*
  (compile-matcher
   '(:record-decl
     (:for-each ;; -descendant
      (:record-decl
       (:matches-name "metadata_.*")
       (:bind :metadata (:record-decl)))))))


(defparameter *method-submatcher*
  (compile-matcher '(:record-decl
                     (:for-each ;; -descendant
                      (:method-decl
                       (:bind :method (:method-decl)))))))


(defun setup-cclass-search (mtool)
  (symbol-macrolet ((results (project-classes (multitool-results mtool))))
    (labels ((%%new-class-callback (class-node record-key template-specializer)
               (let ((cname (mtag-name :whole))
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
                 (sub-match-run *field-submatcher*
                                (mtag-node :whole)
                                (lambda ()
                                  (let* ((field-node (mtag-node :field))
                                         (type (to-canonical-type (cast:get-type field-node))))
                                    (gclog "      >> Field: ~30a~%" field-node)
                                    (handler-case
                                        (push (make-instance-variable
                                               :location (mtag-loc-start :field)
                                               :field-name (mtag-name :field)
                                               :ctype (let ((*debug-info* (make-debug-info :name (mtag-name :field)
                                                                                           :location (mtag-loc-start :field))))
                                                        (classify-ctype (to-canonical-type type))))
                                              fields)
                                      (unsupported-type (err)
                                        (error "Add support for classifying type: ~a (type-of type): ~a  source: ~a"
                                               type (type-of type) (mtag-source :field))))
                                    )))
                 ;;
                 ;; Run a matcher to find the scanGCRoot functions
                 ;;
                 (sub-match-run *method-submatcher*
                                (mtag-node :whole)
                                (lambda ()
                                  (let* ((method-node (mtag-node :method))
                                         (location (mtag-loc-start :method))
                                         (method-name (mtag-name :method)))
                                    (gclog "      >> Method: ~30a~%" (mtag-source :method))
                                    (push method-name method-names)
                                    )))
                 (sub-match-run *metadata-submatcher*
                                class-node
                                (lambda ()
                                  (let* ((metadata-node (mtag-node :metadata))
                                         (metadata-name (string-upcase (mtag-name :metadata))))
                                    (push (intern metadata-name :keyword) metadata))))
                 ;;                   (when (string= record-key "gctools::StackRootedPointer<class asttooling::BAR>") (break "Check fields"))
                 (when (search "gctools::GCVector_moveable<chem::(anonymous)>" record-key)
                   (break "Check (mtag-node :whole)"))
                 (setf (gethash record-key results)
                       (make-cclass :key record-key
                                    :template-specializer template-specializer
                                    :location (mtag-loc-start :whole)
                                    :bases bases
                                    :vbases vbases
                                    :method-names method-names
                                    :metadata metadata
                                    :fields fields))
                 ))
             (%%class-callback ()
               (gclog "MATCH: ------------------~%")
               (gclog "    Start:~%~a~%" (mtag-loc-start :whole))
               (gclog "    Name: ~a~%" (mtag-name :whole))
               (let* ((class-node (mtag-node :whole)))
                 (multiple-value-bind (record-key template-specializer)
                     (record-key class-node)
                   (unless (or (typep class-node 'cast:class-template-partial-specialization-decl) ; ignore partial specializations
                               (and (typep class-node 'cast:class-template-specialization-decl) ; ignore template specializations that have undeclared specialization alloc
                                    (eq (cast:get-specialization-kind class-node) 'ast-tooling:tsk-undeclared))
                               (gethash record-key results)) ; ignore if we've seen it before
                     (%%new-class-callback class-node record-key template-specializer))
                   ))))
      (multitool-add-matcher mtool
                             :name :cclasses
                             :matcher (compile-matcher *class-matcher*)
                             :initializer (lambda () (setf results (make-hash-table :test #'equal)))
                             :callback (make-instance 'code-match-callback :match-code (function %%class-callback)))
      )))




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
  (symbol-macrolet ((class-results (project-lispallocs (multitool-results mtool))))
    (flet ((%%lispalloc-matcher-callback ()
             "This function can only be called as a ASTMatcher callback"
             (let* ((decl (mtag-node :whole))
                    (args (cast:get-template-args decl))
                    (arg (cast:template-argument-list-get args 0))
                    (qtarg (cast:get-as-type arg))
                    (tsty-new (cast:get-type-ptr-or-null qtarg))
                    (class-key (record-key tsty-new))
                    (classified (classify-ctype tsty-new))
                    (class-location (mtag-loc-start :whole))
                    (arg-decl (cast:get-decl tsty-new)) ;; Should I convert to canonical type?????
                    (arg-location (source-loc-as-string (get-loc-start arg-decl)))
                    (arg-name (cast:get-name arg-decl)))
               (unless (gethash class-key class-results)
                 (gclog "Adding class name: ~a~%" class-name)
;;                 (break "Check locations")
                 (let ((lispalloc (make-lispalloc :key class-key
                                                :name arg-name ;; XXXXXX (mtag-name :whole)
                                                :location arg-location ;; class-location
                                                :ctype classified)))
                   (setf (gethash class-key class-results) lispalloc)
                   )))))
      ;; Initialize the class search
      (multitool-add-matcher mtool
                             :name :lispallocs
                             :matcher (compile-matcher `(:bind :whole ,*lispalloc-matcher*))
                             :initializer (lambda () (setf class-results (make-hash-table :test #'equal))) ; initializer
                             :callback (make-instance 'code-match-callback :match-code (function %%lispalloc-matcher-callback))))))




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
  (symbol-macrolet ((class-results (project-classallocs (multitool-results mtool))))
    (flet ((%%classalloc-matcher-callback ()
             "This function can only be called as a ASTMatcher callback"
             (let* ((decl (mtag-node :whole))
                    (args (cast:get-template-args decl))
                    (arg (cast:template-argument-list-get args 0))
                    (qtarg (cast:get-as-type arg))
                    (tsty-new (cast:get-type-ptr-or-null qtarg))
                    (class-key (record-key tsty-new))
                    (classified (classify-ctype tsty-new))
                    (class-location (mtag-loc-start :whole))
                    (arg-decl (cast:get-decl tsty-new)) ;; Should I convert to canonical type?????
                    (arg-location (source-loc-as-string (get-loc-start arg-decl)))
                    (arg-name (cast:get-name arg-decl)))
               (unless (gethash class-key class-results)
                 (gclog "Adding class name: ~a~%" class-name)
                 (let ((classalloc (make-classalloc :key class-key
                                                  :name arg-name ;;(mtag-name :whole)
                                                  :location arg-location ;;class-location
                                                  :ctype classified)))
                   (setf (gethash class-key class-results) classalloc)
                   )))))
      ;; Initialize the class search
      (multitool-add-matcher mtool
                             :name :classallocs
                             :matcher (compile-matcher `(:bind :whole ,*classalloc-matcher*))
                             :initializer (lambda () (setf class-results (make-hash-table :test #'equal))) ; initializer
                             :callback (make-instance 'code-match-callback :match-code (function %%classalloc-matcher-callback))))))





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
  (symbol-macrolet ((class-results (project-rootclassallocs (multitool-results mtool))))
    (flet ((%%rootclassalloc-matcher-callback ()
             "This function can only be called as a ASTMatcher callback"
             (let* ((decl (mtag-node :whole))
                    (args (cast:get-template-args decl))
                    (arg (cast:template-argument-list-get args 0))
                    (qtarg (cast:get-as-type arg))
                    (tsty-new (cast:get-type-ptr-or-null qtarg))
                    (class-key (record-key tsty-new))
                    (classified (classify-ctype tsty-new))
                    (class-location (mtag-loc-start :whole))
                    (arg-decl (cast:get-decl tsty-new)) ;; Should I convert to canonical type?????
                    (arg-location (source-loc-as-string (get-loc-start arg-decl)))
                    (arg-name (cast:get-name arg-decl)))
               (unless (gethash class-key class-results)
                 (gclog "Adding class name: ~a~%" class-name)
                 (let ((rootclassalloc (make-rootclassalloc :key class-key
                                                  :name arg-name ;;(mtag-name :whole)
                                                  :location arg-location ;;class-location
                                                  :ctype classified)))
                   (setf (gethash class-key class-results) rootclassalloc)
                   )))))
      ;; Initialize the class search
      (multitool-add-matcher mtool
                             :name :rootclassallocs
                             :matcher (compile-matcher `(:bind :whole ,*rootclassalloc-matcher*))
                             :initializer (lambda () (setf class-results (make-hash-table :test #'equal))) ; initializer
                             :callback (make-instance 'code-match-callback :match-code (function %%rootclassalloc-matcher-callback))))))






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
  (symbol-macrolet ((class-results (project-containerallocs (multitool-results mtool))))
    (flet ((%%containeralloc-matcher-callback ()
             "This function can only be called as a ASTMatcher callback"
             (let* ((decl (mtag-node :whole))
                    (args (cast:get-template-args decl))
                    (arg (cast:template-argument-list-get args 0))
                    (qtarg (cast:get-as-type arg))
                    (tsty-new (cast:get-type-ptr-or-null qtarg))
                    (class-key (record-key decl))
                    (classified (classify-decl decl))
                    (class-location (mtag-loc-start :whole)))
               (unless (gethash class-key class-results)
                 (let ((containeralloc (make-containeralloc :key class-key
                                                          :name (mtag-name :whole)
                                                          :location class-location
                                                          :ctype classified)))
                   (setf (gethash class-key class-results) containeralloc)))
               )))
      ;; Initialize the class search
      (multitool-add-matcher mtool
                             :name :containerallocs
                             :matcher (compile-matcher `(:bind :whole ,*containeralloc-matcher*))
                             :initializer (lambda () (setf class-results (make-hash-table :test #'equal))) ; initializer
                             :callback (make-instance 'code-match-callback :match-code (function %%containeralloc-matcher-callback))))))


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
         (:matches-name ".*_gc_safe")
       )
     )))


(compile-matcher *global-variable-matcher*)


(defun setup-global-variable-search (mtool)
  (symbol-macrolet ((global-variables (project-global-variables (multitool-results mtool))))
    ;; The matcher is going to match static locals and locals as well as globals - so we need to explicitly recognize and ignore them
    (flet ((%%global-variable-callback ()
             (block matcher
               (gclog "VARIABLE MATCH: ------------------~%")
               (gclog "    Start:~%~a~%" (mtag-loc-start :whole))
               (gclog "    Name: ~a~%" (mtag-name :whole))
               (gclog "    namespace: ~a~%" (mtag-name :ns))
               (let* ((var-node (mtag-node :whole))
                      (varname (decl-name var-node))
                      (location (mtag-loc-start :whole))
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
                                             (classify-ctype (to-canonical-type type))))
                          )
                     (when (eq var-kind :global)
                       (setf (gethash key hash-table)
                             (make-global-variable :location location
                                                   :name varname
                                                   :ctype classified-type))))
                   )))))
      (multitool-add-matcher mtool
                             :name :global-variables
                             :matcher (compile-matcher *global-variable-matcher*)
                             :initializer (lambda () (setf global-variables (make-hash-table :test #'equal)))
                             :callback (make-instance 'code-match-callback :match-code (function %%global-variable-callback)))
      )))




(defparameter *variable-matcher*
  '(:bind :whole
    (:var-decl
     (:is-definition)
     (:has-ancestor
      (:function-decl
       (:bind :function (:function-decl))))
     (:unless (:parm-var-decl))
     (:unless
         (:matches-name ".*_gc_safe")
       )
     )))


(compile-matcher *variable-matcher*)




(defun setup-variable-search (mtool)
  (symbol-macrolet ((static-local-variables (project-static-local-variables (multitool-results mtool)))
                    (local-variables (project-local-variables (multitool-results mtool))))
    ;; The matcher is going to match globals as well as static locals and locals - so we need to explicitly recognize and ignore globals
    (flet ((%%variable-callback ()
             (block matcher
               (gclog "VARIABLE MATCH: ------------------~%")
               (gclog "    Start:~%~a~%" (mtag-loc-start :whole))
               (gclog "    Name: ~a~%" (mtag-name :whole))
               (gclog "    namespace: ~a~%" (mtag-name :ns))
               (let* ((var-node (mtag-node :whole))
                      (varname (decl-name var-node))
                      (location (mtag-loc-start :whole))
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
                                             (classify-ctype (to-canonical-type type))))
                          )
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
                                                   :ctype classified-type)))))
                   )))))
      (multitool-add-matcher mtool
                             :name :variables
                             :matcher (compile-matcher *variable-matcher*)
                             :initializer (lambda ()
                                            (setf static-local-variables (make-hash-table :test #'equal))
                                            (setf local-variables (make-hash-table :test #'equal)))
                             :callback (make-instance 'code-match-callback :match-code (function %%variable-callback)))
      )))






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
  (symbol-macrolet ((class-results (project-gcinfos (multitool-results mtool))))
    (flet ((%%gcinfo-matcher-callback ()
             "This function can only be called as a ASTMatcher callback"
             (let* ((decl (mtag-node :whole))
                    (args (cast:get-template-args decl))
                    (arg (cast:template-argument-list-get args 0))
                    (qtarg (cast:get-as-type arg))
                    (tsty-new (cast:get-type-ptr-or-null qtarg))
                    (class-key (record-key tsty-new))
                    (classified (classify-ctype tsty-new))
                    (class-location (mtag-loc-start :whole))
                    (arg-decl (cast:get-decl tsty-new)) ;; Should I convert to canonical type?????
                    (arg-location (source-loc-as-string (get-loc-start arg-decl)))
                    (arg-name (cast:get-name arg-decl)))
               (unless (gethash class-key class-results)
                 (gclog "Adding class name: ~a~%" class-name)
                 (let ((gcinfo (make-gcinfo :key class-key
                                            :name arg-name ;;(mtag-name :whole)
                                            :location arg-location ;;class-location
                                            :ctype classified)))
                   (setf (gethash class-key class-results) gcinfo)
                   )))))
      ;; Initialize the class search
      (multitool-add-matcher mtool
                             :name :gcinfos
                             :matcher (compile-matcher `(:bind :whole ,*gcinfo-matcher*))
                             :initializer (lambda () (setf class-results (make-hash-table :test #'equal))) ; initializer
                             :callback (make-instance 'code-match-callback :match-code (function %%gcinfo-matcher-callback))))))






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


(defgeneric fixer-macro-name (fixer-head fixer))

(defmethod fixer-macro-name ((x (eql :smart-ptr-fix)) fixer) "SMART_PTR_FIX")

(defmethod fixer-macro-name ((x (eql :tagged-pointer-fix)) fixer) "TAGGED_POINTER_FIX")

(defmethod fixer-macro-name ((x pointer-fixer) fixer) "SIMPLE_POINTER_FIX")

(defmethod fixer-macro-name ((x array-fixer) fixer) "ARRAY_FIX")


(defun code-for-instance-var (stream ptr-name instance-var)
  (let* ((fixer-macro (fixer-macro-name (car (last instance-var)) (last instance-var)))
         (variable-chain (butlast instance-var 1)))
    (format stream "    ~a(~a->~{~a~^.~});~%" fixer-macro ptr-name variable-chain)))
#||
           (macro-name (macro fixer))
           (accessor (variable-accessor e)))
      (format stream "~a(~a);~%" (macro fixer) (variable e)))))
||#

(defconstant +ptr-name+ "obj_gc_safe"
  "This variable is used to temporarily hold a pointer to a Wrapper<...> object - we want the GC to ignore it")


(defstruct destination
  stream
  (helper-stream (make-string-output-stream))
  table-name
  label-list
  label-prefix )

(defmacro with-destination ((fout dest enum &optional continue) &body body)
  (let ((label-gs (gensym)))
    `(let* ((,fout (destination-stream ,dest))
	    (,label-gs (format nil "~a_~a" (destination-label-prefix ,dest)
                               (enum-name ,enum))))
       (push (cons (enum-value ,enum) ,label-gs) (destination-label-list ,dest))
       (format ,fout "~a:~%" ,label-gs)
       (format ,fout "{~%")
       ,@body
       (format ,fout "}~%")
       ,(if continue `(format ,fout "~a;~%" ,continue))
       )))


(defun scanner-dummy (dest enum anal)
  (assert (simple-enum-p enum))
  (let* ((alloc (simple-enum-alloc enum))
         (key (alloc-key alloc))
         (enum-name (enum-name enum)))
    (gclog "build-mps-scan-for-one-family -> inheritance key[~a]  value[~a]~%" key value)
    (with-destination (fout dest enum "goto TOP")
		      (format fout "    // Should never be invoked~%")
		      )))


(defun scanner-for-lispallocs (dest enum anal)
  (assert (simple-enum-p enum))
  (let* ((alloc (simple-enum-alloc enum))
         (key (alloc-key alloc))
         (enum-name (enum-name enum)))
    (gclog "build-mps-scan-for-one-family -> inheritance key[~a]  value[~a]~%" key value)
    (with-destination (fout dest enum "goto TOP")
;; Generate a helper function
      (let ((fh (destination-helper-stream dest)))
        (format fh "template <>~%")
        (format fh "~a mps_res_t obj_scan_helper<~a>(mps_ss_t _ss, mps_word_t _mps_zs, mps_word_t _mps_w, mps_word_t &_mps_ufs, mps_word_t _mps_wt, mps_addr_t& client)~%" (inline-analysis :scan key anal) key)
        (format fh "{~%")
        (let ((all-instance-variables (fix-code (gethash key (project-classes (analysis-project anal))) anal)))
          (when all-instance-variables
            (format fh "    ~A* ~A = reinterpret_cast<~A*>(client);~%" key +ptr-name+ key))
          (dolist (instance-var all-instance-variables)
            (code-for-instance-var fh +ptr-name+ instance-var)))
        (format fh "    typedef ~A type_~A;~%" key enum-name)
        (format fh "    client = (char*)client + AlignUp(sizeof(type_~a)) + global_alignup_sizeof_header;~%" enum-name)
        (format fh "    return MPS_RES_OK;~%")
        (format fh "}~%"))
      (format fout "  mps_res_t result = gctools::obj_scan_helper<~a>(_ss,_mps_zs,_mps_w,_mps_ufs,_mps_wt,client);~%" key)
      (format fout "  if ( result != MPS_RES_OK ) return result;~%")
      )))


(defun skipper-for-lispallocs (dest enum anal)
  (assert (simple-enum-p enum))
  (let* ((alloc (simple-enum-alloc enum))
         (key (alloc-key alloc))
         (enum-name (enum-name enum)))
    (gclog "skipper-for-lispallocs -> inheritance classid[~a]  value[~a]~%" key (enum-value enum))
    (with-destination (fout dest enum)
		      (format fout "    typedef ~A type_~A;~%" key enum-name)
		      (format fout "    client = (char*)client + AlignUp(sizeof(type_~a)) + global_alignup_sizeof_header;~%" enum-name)
                      (format fout "    goto DONE; //return client;~%")
		      )))

(defun dumper-for-lispallocs (dest enum anal)
  (assert (simple-enum-p enum))
  (let* ((alloc (simple-enum-alloc enum))
         (key (alloc-key alloc))
         (enum-name (enum-name enum)))
    (with-destination (fout dest enum "goto BOTTOM")
		      ;;    (format fout "    ~A* ~A = reinterpret_cast<~A*>(client);~%" key +ptr-name+ key)
		      (format fout "    typedef ~A type_~A;~%" key enum-name)
		      (format fout "    sout << \"~a size[\" << (AlignUp(sizeof(type_~a))+global_alignup_sizeof_header) << \"]\" ;~%" enum-name enum-name )
		      )))


(defun finalizer-for-lispallocs (dest enum anal)
  (check-type enum simple-enum)
  (let* ((alloc (simple-enum-alloc enum))
         (key (alloc-key alloc))
         (enum-name (enum-name enum))
         (ns-cn key)
         (cn (strip-all-namespaces-from-name ns-cn)))
    (with-destination (fout dest enum)
    (gclog "build-mps-finalize-for-one-family -> inheritance key[~a]  value[~a]~%" key value)
    (format fout "    ~A* ~A = reinterpret_cast<~A*>(client);~%" key +ptr-name+ key)
;;    (format fout "    ~A* ~A = BasePtrToMostDerivedPtr<~A>(base);~%" key +ptr-name+ key)
    (format fout "    ~A->~~~A();~%" +ptr-name+ cn)
    (format fout "    return;~%"))))



(defun scanner-for-templated-lispallocs (dest enum anal)
  (assert (templated-enum-p enum))
  (let* ((key (enum-key enum))
         (enum-name (enum-name enum)))
    (gclog "build-mps-scan-for-one-family -> inheritance key[~a]  value[~a]~%" key value)
    (with-destination (fout dest enum "goto TOP")
		      ;;    (format fout "    typedef ~a MyType;~%" key)
		      ;;    (format fout "    typedef typename gctools::GCHeader<MyType>::HeaderType HeadT;~%")
		      ;;    (format fout "    HeadT* header = reinterpret_cast<HeadT*>(base);~%")
		      ;;    (format fout "    ~A* ~A = BasePtrToMostDerivedPtr<~A>(base);~%" key +ptr-name+ key)
		      (format fout "    ~A* ~A = reinterpret_cast<~A*>(client);~%" key +ptr-name+ key)
		      (let ((all-instance-variables (fix-code (gethash key (project-classes (analysis-project anal))) anal)))
			(dolist (instance-var all-instance-variables)
			  (code-for-instance-var fout +ptr-name+ instance-var)))
		      ;;    (format fout "    base = (char*)base + length;~%")
		      ;;    (format fout "    typedef ~A type_~A;~%" key enum-name)
		      (format fout "    client = (char*)client + AlignUp(~a->templatedSizeof()) + global_alignup_sizeof_header;~%" +ptr-name+)
		      )))

(defun skipper-for-templated-lispallocs (dest enum anal)
  (assert (templated-enum-p enum))
  (let* ((key (enum-key enum))
         (enum-name (enum-name enum)))
    (gclog "build-mps-scan-for-one-family -> inheritance key[~a]  value[~a]~%" key value)
    (with-destination (fout dest enum)
		      (format fout "    ~A* ~A = reinterpret_cast<~A*>(client);~%" key +ptr-name+ key)
		      (format fout "    client = (char*)client + AlignUp(~a->templatedSizeof()) + global_alignup_sizeof_header;~%" +ptr-name+)
                      (format fout "    goto DONE; //return client;~%")
		      )))

(defun dumper-for-templated-lispallocs (dest enum anal)
  (assert (templated-enum-p enum))
  (let* ((key (enum-key enum))
         (enum-name (enum-name enum)))
    (gclog "build-mps-scan-for-one-family -> inheritance key[~a]  value[~a]~%" key value)
    (with-destination (fout dest enum "goto BOTTOM")
		      (format fout "    ~A* ~A = reinterpret_cast<~A*>(client);~%" key +ptr-name+ key)
		      (format fout "    sout << \"~a size[\" << (AlignUp(~a->templatedSizeof()) + global_alignup_sizeof_header) << \"]\" ;~%" enum-name +ptr-name+ )
		      )))

(defun finalizer-for-templated-lispallocs (dest enum anal)
  (assert (templated-enum-p enum))
  (let* ((key (enum-key enum))
         (enum-name (enum-name enum))
         (ns-cn key)
         (cn (strip-all-namespaces-from-name ns-cn)))
    (with-destination (fout dest enum)
    (format fout "    ~A* ~A = reinterpret_cast<~A*>(client);~%" key +ptr-name+ key)
;;    (format fout "    ~A* ~A = BasePtrToMostDerivedPtr<~A>(base);~%" key +ptr-name+ key)
    (format fout "    ~A->~~~A();~%" +ptr-name+ cn)
    )))




(defun scanner-for-gccontainer (dest enum anal)
  (check-type enum simple-enum)
  (let* ((alloc (simple-enum-alloc enum))
         (decl (containeralloc-ctype alloc))
         (key (alloc-key alloc))
         (enum-name (enum-name enum)))
    (with-destination (fout dest enum "goto TOP")
		      ;;    (format fout "// processing ~a~%" alloc)
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
			    (format fout "          ~a(*it);~%" (fix-macro-name parm0-ctype)))
                           ((tagged-pointer-ctype-p parm0-ctype)
                            (format fout "          ~a(*it);~%" (fix-macro-name parm0-ctype)))
			   ((cxxrecord-ctype-p parm0-ctype)
			    (let ((all-instance-variables (fix-code (gethash (ctype-key parm0-ctype) (project-classes (analysis-project anal))) anal)))
			      (dolist (instance-var all-instance-variables)
				(code-for-instance-var fout "it" instance-var))))
			   ((class-template-specialization-ctype-p parm0-ctype)
			    (let ((all-instance-variables (fix-code (gethash (ctype-key parm0-ctype) (project-classes (analysis-project anal))) anal)))
			      (dolist (instance-var all-instance-variables)
				(code-for-instance-var fout "it" instance-var))))
			   ((pointer-ctype-p parm0-ctype)
			    (format fout "          ~a(*it);~%" (fix-macro-name parm0-ctype)))
			   (t (error "Write code to scan ~a" parm0-ctype)))
			  (format fout "    }~%")
			  (format fout "    typedef typename ~A type_~A;~%" key enum-name)
			  (format fout "    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_~a>(~a->capacity()))+AlignUp(sizeof(gctools::Header_s));~%" enum-name +ptr-name+)
			  (format fout "    client = (char*)client + header_and_gccontainer_size;~%" enum-name)))
		      ;;              (format fout "    base = (char*)base + length;~%")
		      )))


(defun skipper-for-gccontainer (dest enum anal)
  (check-type enum simple-enum)
  (let* ((alloc (simple-enum-alloc enum))
         (decl (containeralloc-ctype alloc))
         (key (alloc-key alloc))
         (enum-name (enum-name enum)))
    (with-destination (fout dest enum)
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
            (format fout "    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_~a>(~a->capacity()))+AlignUp(sizeof(gctools::Header_s));~%" enum-name +ptr-name+)
            (format fout "    client = (char*)client + header_and_gccontainer_size;~%" enum-name)
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
    (with-destination (fout dest enum "goto BOTTOM")
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
    (with-destination (fout dest enum)
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




(defun skipper-for-gcstring (dest enum anal)
  (check-type enum simple-enum)
  (let* ((alloc (simple-enum-alloc enum))
         (decl (containeralloc-ctype alloc))
         (key (alloc-key alloc))
         (enum-name (enum-name enum)))
    (with-destination (fout dest enum)
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
            (format fout "    size_t header_and_gcstring_size = AlignUp(sizeof_container<type_~a>(~a->capacity()))+AlignUp(sizeof(gctools::Header_s));~%" enum-name +ptr-name+)
            (format fout "    client = (char*)client + Align(header_and_gcstring_size);~%")
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
    (with-destination (fout dest enum "goto BOTTOM")
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
    (with-destination (fout dest enum)
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


(defun string-left-matches (str sub)
  (eql (search str sub) 0))


(defun setup-manager ()
  (let* ((manager (make-manager)))
    (add-species manager (make-species :name :bootstrap
                                       :discriminator (lambda (x) (and (lispalloc-p x)
                                                                       (gctools::bootstrap-kind-p (alloc-key x))))
                                       :scan 'scanner-for-lispallocs
                                       :skip 'skipper-for-lispallocs
                                       :dump 'dumper-for-lispallocs
                                       :finalize 'finalizer-for-lispallocs
                                       ))
    (add-species manager (make-species :name :lispalloc
                                       :discriminator (lambda (x) (and (lispalloc-p x)
                                                                       (not (gctools:bootstrap-kind-p (alloc-key x)))
                                                                       (not (alloc-template-specializer-p x *analysis*))))
                                       :scan 'scanner-for-lispallocs
                                       :skip 'skipper-for-lispallocs
                                       :dump 'dumper-for-lispallocs
                                       :finalize 'finalizer-for-lispallocs
                                       ))
    (add-species manager (make-species :name :templated-lispalloc
                                       :discriminator (lambda (x) (and (lispalloc-p x) (alloc-template-specializer-p x *analysis*)))
                                       :scan 'scanner-for-templated-lispallocs
                                       :skip 'skipper-for-templated-lispallocs
                                       :dump 'dumper-for-templated-lispallocs
                                       :finalize 'finalizer-for-templated-lispallocs
                                       ))
    (add-species manager (make-species :name :GCVECTOR
                                       :discriminator (lambda (x) (and (containeralloc-p x) (search "gctools::GCVector" (alloc-key x))))
                                       :scan 'scanner-for-gccontainer
                                       :skip 'skipper-for-gccontainer
                                       :dump 'dumper-for-gccontainer
                                       :finalize 'finalizer-for-gccontainer
                                       ))
    (add-species manager (make-species :name :GCARRAY
                                       :discriminator (lambda (x) (and (containeralloc-p x) (search "gctools::GCArray" (alloc-key x))))
                                       :scan 'scanner-for-gccontainer
                                       :skip 'skipper-for-gccontainer
                                       :dump 'dumper-for-gccontainer
                                       :finalize 'finalizer-for-gccontainer
                                       ))
    (add-species manager (make-species :name :GCSTRING
                                       :discriminator (lambda (x) (and (containeralloc-p x) (search "gctools::GCString" (alloc-key x))))
                                       :scan 'scanner-dummy ;; don't need to scan
                                       :skip 'skipper-for-gcstring
                                       :dump 'dumper-for-gcstring
                                       :finalize 'finalizer-for-gcstring
                                       ))
    (add-species manager (make-species :name :classalloc
                                       :discriminator (lambda (x) (and (classalloc-p x) (not (alloc-template-specializer-p x *analysis*))))
                                       :scan 'scanner-for-lispallocs
                                       :skip 'skipper-for-lispallocs
                                       :dump 'dumper-for-lispallocs
                                       :finalize 'finalizer-for-lispallocs
                                       ))
    (add-species manager (make-species :name :rootclassalloc
                                       :discriminator (lambda (x) (rootclassalloc-p x))
                                       :scan 'scanner-for-lispallocs
                                       :skip 'skipper-for-lispallocs
                                       :dump 'dumper-for-lispallocs
                                       :finalize 'finalizer-for-lispallocs
                                       ))
    (add-species manager (make-species :name :templated-classalloc
                                       :discriminator (lambda (x) (and (classalloc-p x) (alloc-template-specializer-p x *analysis*)))
                                       :scan 'scanner-for-templated-lispallocs
                                       :skip 'skipper-for-templated-lispallocs
                                       :dump 'dumper-for-templated-lispallocs
                                       :finalize 'finalizer-for-templated-lispallocs
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



(defparameter *analysis* nil)
(defun analyze-project (&key (project *project*) types-to-inline-mps-functions )
  (setq *analysis* (make-analysis :project project
                                  :manager (setup-manager)
                                  :inline types-to-inline-mps-functions))
  (organize-allocs-into-species-and-create-enums *analysis*)
  (generate-forward-declarations *analysis*)
  (analyze-hierarchy *analysis*)
  (sort-enums-by-value *analysis*)
  t
  )




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
         (species (enum-species enum))
         )
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
      (format stream "#endif // #if defined(~a)~%" (species-preprocessor-guard species)))
    ))


(defun generate-gckind-for-enums (fout anal)
  (maphash (lambda (key enum)
             (generate-one-gckind-for-enum fout enum))
           (analysis-enums anal)))


(defun fix-code-for-field (field analysis)
  (let ((code (fix-code (instance-variable-ctype field) analysis)))
    (cond
      ((null code) nil)
      ((atom code)
       (list (list (instance-variable-field-name field) code)))
      ((consp code)
       (mapcar (lambda (f) (cons (instance-variable-field-name field) f))
               code))
      (t
       (instance-variable-field-name field) code))))

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
  (fix-code (gethash (cxxrecord-ctype-key x) (project-classes (analysis-project analysis))) analysis))

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


(defun impl-generate-kind-name-map (dest anal)
  (mapc (lambda (enum)
          (let* ((enum-name (enum-name enum)))
	    (with-destination (fout dest enum)
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
            "static void* ~a_table[] = { NULL ~%"
            (destination-table-name dest))
    (dotimes (iskip (1- first-general))
      (format (destination-stream dest)
              "       , NULL /* Skip entry for immediate */~%"))
    (let ((entries (reverse (destination-label-list dest))))
      (dolist (entry entries)
        (format (destination-stream dest) "  /* ~a */ , &&~a~%" (car entry) (cdr entry))))
    (format (destination-stream dest) "};~%")))

(defmacro do-generator (stream analysis &key table-name function-declaration function-prefix function-table-type generator)
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





(defun generate-code (&key (analysis *analysis*) test )
  (let ((filename (if test
                      "test_clasp_gc"
                    "clasp_gc")))
    (with-open-file (stream (make-pathname :name filename :type "cc" :defaults (main-directory-pathname)) :direction :output :if-exists :supersede)
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
		    (do-generator stream analysis
				  :table-name "KIND_NAME_MAP"
				  :function-declaration "const char* ~a()"
				  :function-prefix "kind_name"
				  :function-table-type "const char* (*KIND_NAME_MAP_table[])()"
				  :generator #'impl-generate-kind-name-map)
		    (do-generator stream analysis
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
                    (format stream "#if defined(GC_GLOBALS)~%")
                    (generate-code-for-global-non-symbol-variables stream analysis)
                    (format stream "#endif // defined(GC_GLOBALS)~%")
                    (format stream "#if defined(GC_GLOBAL_SYMBOLS)~%")
                    (generate-code-for-global-symbols stream analysis)
                    (format stream "#endif // defined(GC_GLOBAL_SYMBOLS)~%")
                    )))

;; ----------------------------------------------------------------------
;;
;; Setup the *tools* that will run over all of the source code and run
;; several matchers that scrape the C++ AST for info requires to build
;; garbage collection scanners.


;;(load-compilation-database "app-resources:build-databases;clasp_compile_commands.json")

(defun fix-path (root rel)
  (let* ((pnroot (pathname root))
         (pnrel (pathname rel))
         (merged (merge-pathnames pnrel pnroot))
         (abs (uiop/filesystem:native-namestring merged)))
    (format t "Fixed path path: ~a~%" abs)
    (or abs (error "Could not find absolute path for ~a + ~a" root rel))))

(defun ensure-directory (relpath)
  (if (eq (elt relpath (1- (length relpath))) #\/)
      relpath
      (concatenate 'string relpath "/")))

(defun build-arguments-adjuster ()
  "Build a function that fixes up compile command arguments.
It converts relative -I../... arguments to absolute paths"
  (lambda (args) 
    (let ((new-args (copy-seq args))
          (root-directory (make-pathname :name nil :type nil :defaults *main-pathname*)))
      (dotimes (i (length new-args))
        (let ((arg (elt new-args i)))
          (cond
            ((string= arg "-I.." :start1 0 :end1 4)
             (let ((fixed-path (fix-path root-directory (ensure-directory (subseq arg 2)))))
               (elt-set new-args i (concatenate 'string "-I" fixed-path))))
            ((string= arg "../" :start1 0 :end1 3)
             (let ((fixed-path (fix-path root-directory arg)))
               (elt-set new-args i fixed-path)))
            (t #| do nothing |# ))))
      (let ((result (concatenate 'vector #-quiet new-args #+quiet(remove "-v" new-args)
                                 (vector "-DUSE_MPS"
                                         "-DRUNNING_GC_BUILDER"
                                         "-isystem" +isystem-dir+
                                         "-resource-dir" +resource-dir+)
                                 +additional-arguments+)))
        result))))


(defvar *tools* nil)
(defun setup-*tools* ()
  (setf *tools* (make-multitool :arguments-adjuster (build-arguments-adjuster)))
  (setup-cclass-search *tools*) ; search for all classes
  (setup-lispalloc-search *tools*)
  (setup-classalloc-search *tools*)
  (setup-rootclassalloc-search *tools*)
  (setup-containeralloc-search *tools*)
  (setup-global-variable-search *tools*)
  (setup-variable-search *tools*))


;; ----------------------------------------------------------------------
;; Test search
;; ----------------------------------------------------------------------
(progn
  (lnew $test-search)
  (setq $test-search (append
                      (lsel $* ".*mol2\.cc$"))
                      )
  )


(defvar *project* nil)
(defun search-all (&key test tools (save t))
  (multitool-activate-tools *tools* tools)
  (setf (multitool-results *tools*) (make-project))
  (let ((alljobs (if test
                     test
                     (reverse (lremove (lremove $* ".*mps\.c$") ".*gc_interface\.cc$")))))
    (dolist (job alljobs)
;;;      (core:system (format nil "heap ~a" (core:getpid)))
;;;      (core:system (format nil "vmmap -w ~a" (core:getpid)))
      (let ((somejobs (list job)))
        (batch-run-multitool *tools* :filenames somejobs))
      ;; Extract the results for easy access
      (setq *project* (multitool-results *tools*))
      ))
  (when save (save-project))
  )







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



(defun run-job (proc job-list)
  (setf (multitool-results *tools*) (make-project))
  (format t "====== Running jobs in fork #~a: ~a~%" proc job-list)
  (batch-run-multitool *tools* :filenames job-list)
  (format t "------------ About to save-archive --------------~%")
  (save-data (multitool-results *tools*) (project-pathname (format nil "project~a" proc) "dat"))
  (core:exit))

(defvar *parallel-search-pids* nil)
(defun parallel-search-all (&key test one-at-a-time)
  "Run *max-parallel-searches* processes at a time - whenever one finishes, start the next"
  (setq *parallel-search-pids* nil)
  (let ((all-jobs (split-jobs (if test
                                  test
                                  (reverse (lremove (lremove $* ".*mps\.c$") ".*gc_interface\.cc$")))
                              *max-parallel-searches*
                              ))
        (spare-processes (if one-at-a-time 1 *max-parallel-searches*)))
    (save-data all-jobs (project-pathname "project-all" "dat"))
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


(defun serial-search-all (&key start-directory test one-at-a-time)
  "Run *max-parallel-searches* processes at a time - whenever one finishes, start the next"
  (format t "serial-search-all --> current-dir: ~a~%" (core:current-dir))
  (setq *parallel-search-pids* nil)
  (let ((all-jobs (if test
                      test
                      (reverse (lremove (lremove $* ".*mps\.c$") ".*gc_interface\.cc$")))))
    (save-data all-jobs (project-pathname "project-all" "dat"))
    (format t "all-jobs: ~a~%" all-jobs)
    (setf (multitool-results *tools*) (make-project))
    (batch-run-multitool *tools* :filenames all-jobs)
    (setq *project* (multitool-results *tools*))
    (save-data *project* (project-pathname "project" "dat"))))



(defun parallel-merge (&key end (start 0) restart)
  "Merge the analyses generated from the parallel search"
  (let* ((all-jobs (load-data (project-pathname "project-all" "dat")))
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





(progn
  (lnew $tiny-test-search)
  (setq $tiny-test-search (append
                      (lsel $* ".*/lambdaListHandler\.cc")
                      ))
  )





(defun load-test-ast ()
  (lnew $tiny-test-search)
  (setq $tiny-test-search (lsel $* ".*/cons\.cc"))
  (load-asts $tiny-test-search
             :arguments-adjuster-code *arguments-adjuster*))

(defun run-test ()
  (defparameter *test-matcher*
    '(:record-decl
      ;;        (:is-definition)
      ;;        (:is-template-instantiation)
      (:matches-name ".*GCInfo.*"))
    )
  (match-count *test-matcher*
               :limit 100
               ;;              :tag :point
               :match-comments '( ".*mytest.*" )
               :match-code #'(lambda ()
                         (let* ((decl (mtag-node :whole))
                                (args (cast:get-template-args decl))
                                (arg (cast:template-argument-list-get args 0))
                                (qtarg (cast:get-as-type arg))
                                (tsty-new (cast:get-type-ptr-or-null qtarg))
                                (key (record-key tsty-new))
                                (classified (classify-ctype tsty-new))
                                )
                           (format t "MATCH: ------------------~%")
                           (format t "        Start: ~a~%" (mtag-loc-start :whole))
                           (format t "         Node: ~a~%" (mtag-node :whole))
                           (format t "     type-of node: ~a~%" (type-of (mtag-node :whole)))
                           (format t "         Name: ~a~%" (mtag-name :whole))
                           (format t "          Arg: ~a~%" classified)
                           (format t "          key: ~a~%" key)
                           )
                         )
               )
  )


(defun run-serial-search ()
  (clasp-analyzer:load-compilation-database "app-resources:build-databases;clasp_compile_commands.json")
  (clasp-analyzer::serial-search-all))


(defun serial-search-only (&key test)
    (setup-*tools*)
    (serial-search-all :test test))
(export 'serial-search-only)
(export '$*)

(defun serial-search-all-then-generate-code-and-quit (&key test)
    (setup-*tools*)
    (serial-search-all :test test)
    (analyze-project)
    (setf (analysis-inline *analysis*) '("core::Cons_O"))
    (generate-code)
    (quit))

(defun parallel-search-all-then-generate-code-and-quit ()
    (setup-*tools*)
    (parallel-search-all)
    (analyze-project)
    (setf (analysis-inline *analysis*) '("core::Cons_O"))
    (generate-code)
    (quit))

