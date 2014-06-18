;;
;; I'm tring to get (search :test t) to work
;; This should build the project for one file
;;

;;(push :use-breaks *features*)
;;(push :gc-warnings *features*)


(defmacro gclog (fmt &rest args))

(progn
  (provide 'gcb)
  (load "sys:clang-tool.lsp") ; (require 'clang-tool)
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
  (cur-enum-value 0)
  manager
  (forwards (make-hash-table :test #'equal))
  (enums (make-hash-table :test #'equal))
  classes-with-fixptrs
;;  (species-to-enum (make-hash-table :test #'eq))
;;  (alloc-to-enum (make-hash-table :test #'eq))
;;  list-of-all-enums
;;  (contains-fixptr (make-hash-table :test #'eq))
  )





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
  (cast:get-qualified-name-as-string decl)
;;  (cast:get-name-as-string decl)
  )


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


(defun save-project ()
  (serialize:save-archive *project* (project-pathname "project" "dat")))

(defun load-project ()
  (setq *project* (serialize:load-archive (project-pathname "project" "dat")))
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
                             :callback (make-instance 'code-match-callback :code (function %%class-callback)))
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
                             :callback (make-instance 'code-match-callback :code (function %%lispalloc-matcher-callback))))))




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
                             :callback (make-instance 'code-match-callback :code (function %%classalloc-matcher-callback))))))





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
                             :callback (make-instance 'code-match-callback :code (function %%rootclassalloc-matcher-callback))))))






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
                             :callback (make-instance 'code-match-callback :code (function %%containeralloc-matcher-callback))))))


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
                             :callback (make-instance 'code-match-callback :code (function %%global-variable-callback)))
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
                        (if (string= key "_staticObj@/Users/meister/Development/cando/clasp/src/asttooling/testAST.cc:16:13")
                            (break "Caught static var"))
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
                             :callback (make-instance 'code-match-callback :code (function %%variable-callback)))
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
                             :callback (make-instance 'code-match-callback :code (function %%gcinfo-matcher-callback))))))






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
    ((string= (cxxrecord-ctype-key x) "(anonymous)") nil)
    (t (let ((c (gethash (ctype-key x) (project-classes project))))
         (if c
             (contains-fixptr-impl-p x project)
             nil)))))

(defmethod contains-fixptr-impl-p ((x injected-class-name-ctype) project) nil)
(defmethod contains-fixptr-impl-p ((x unclassified-ctype) project) nil)
(defmethod contains-fixptr-impl-p ((x unclassified-template-specialization-ctype) project) nil)
(defmethod contains-fixptr-impl-p ((x smart-ptr-ctype) project) t)
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
                               (assert (eql (length bases) 1)) ;; there can be only one base
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
                                                        :value (incf (analysis-cur-enum-value analysis))
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
                                   :value (incf (analysis-cur-enum-value analysis))
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



(defun sort-enums-by-index (enums)
  (sort (copy-list enums) #'(lambda (a b) (< (enum-value a) (enum-value b)))))


(defun scanner-for-system (fout enum anal)
  (let ((cclass (enum-cclass enum)))
    (format fout "case ~a: {~%" (enum-name enum))
    (cond
      ((string= "fwd2" (cclass-key cclass))
       (format fout "THROW_HARD_ERROR(BF(\"~a should never be scanned\"));~%" (cclass-key cclass)))
      ((string= "fwd" (cclass-key cclass))
       (format fout "base = (char*)base + length;~%"))
      ((string= "pad1" (cclass-key cclass))
       (format fout "base = (char*)base + length;~%"))
      ((string= "pad" (cclass-key cclass))
       (format fout "base = (char*)base + length;~%"))
      (t (error "Illegal scanner-for-system species ~a" enum)))
    (format fout "break;~%}~%")
    ))



(defun skipper-for-system (fout enum anal)
  (let ((cclass (enum-cclass enum)))
    (format fout "case ~a: {~%" (enum-name enum))
;;    (format fout "Header_s<void>* header = reinterpret_cast<Header_s<void>*>(base);~%")
    (cond
      ((string= "fwd2" (cclass-key cclass))
       (format fout "THROW_HARD_ERROR(BF(\"~a should never be skipped\"));~%" (cclass-key cclass)))
      ((string= "fwd" (cclass-key cclass))
       (format fout "base = (char*)base + length;~%"))
      ((string= "pad1" (cclass-key cclass))
       (format fout "base = (char*)base + length;~%"))
      ((string= "pad" (cclass-key cclass))
       (format fout "base = (char*)base + length;~%"))
      (t (error "Illegal skipper-for-system species ~a" species)))
    (format fout "break;~%}~%")
    ))

(defun finalizer-for-system (fout enum anal)
  (check-type enum simple-enum)
  (let ((cclass (enum-cclass enum)))
    (format fout "case ~a: {~%" (enum-name enum))
    (format fout "    THROW_HARD_ERROR(BF(\"~a should never be finalized\"));~%" (cclass-key cclass)))
  (format fout "    break;~%}~%"))
        


(defstruct array-fixer
  element-type)

(defstruct pointer-fixer
  pointee-type)


(defgeneric fixer-macro-name (fixer-head fixer))

(defmethod fixer-macro-name ((x (eql :smart-ptr-fix)) fixer) "SMART_PTR_FIX")

(defmethod fixer-macro-name ((x pointer-fixer) fixer) "POINTER_FIX")



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


(defun scanner-for-lispallocs (fout enum anal)
  (assert (simple-enum-p enum))
  (let* ((alloc (simple-enum-alloc enum))
         (key (alloc-key alloc))
         (enum-name (enum-name enum)))
    (gclog "build-mps-scan-for-one-family -> inheritance key[~a]  value[~a]~%" key value)
    (format fout "case ~a: {~%" enum-name)
;;    (format fout "Header_s* header = reinterpret_cast<Header_s*>(base);~%")
    (format fout "    ~A* ~A = BasePtrToMostDerivedPtr<~A>(base);~%" key +ptr-name+ key)
    (let ((all-instance-variables (fix-code (gethash key (project-classes (analysis-project anal))) anal)))
      (dolist (instance-var all-instance-variables)
        (code-for-instance-var fout +ptr-name+ instance-var)))
    (format fout "    typedef ~A type_~A;~%" key enum-name)
    (format fout "    base = (char*)base + length;~%")
    (format fout "} break;~%")
    ))


(defun skipper-for-lispallocs (fout enum anal)
  (assert (simple-enum-p enum))
  (let* ((alloc (simple-enum-alloc enum))
         (key (alloc-key alloc))
         (enum-name (enum-name enum)))
    (gclog "skipper-for-lispallocs -> inheritance classid[~a]  value[~a]~%" key (enum-value enum))
    (format fout "case ~a: {~%" enum-name)
    (format fout "    typedef ~A type_~A;~%" key enum-name)
    (format fout "    base = (char*)base + length;~%")
    (format fout "} break;~%")))


(defun finalizer-for-lispallocs (fout enum anal)
  (check-type enum simple-enum)
  (let* ((alloc (simple-enum-alloc enum))
         (key (alloc-key alloc))
         (enum-name (enum-name enum))
         (ns-cn key)
         (cn (strip-all-namespaces-from-name ns-cn)))
    (gclog "build-mps-finalize-for-one-family -> inheritance key[~a]  value[~a]~%" key value)
    (format fout "case ~a: {~%" enum-name)
    (format fout "    ~A* ~A = BasePtrToMostDerivedPtr<~A>(base);~%" key +ptr-name+ key)
    (format fout "    ~A->~~~A();~%" +ptr-name+ cn)
    (format fout "    return;~%")
    (format fout "} break;~%")))


(defun scanner-for-templated-lispallocs (fout enum anal)
  (assert (templated-enum-p enum))
  (let* ((key (enum-key enum))
         (enum-name (enum-name enum)))
    (gclog "build-mps-scan-for-one-family -> inheritance key[~a]  value[~a]~%" key value)
    (format fout "case ~a: {~%" enum-name)
    (format fout "    typedef ~a MyType;~%" key)
    (format fout "    typedef typename gctools::GCHeader<MyType>::HeaderType HeadT;~%")
    (format fout "    HeadT* header = reinterpret_cast<HeadT*>(base);~%")
    (format fout "    ~A* ~A = BasePtrToMostDerivedPtr<~A>(base);~%" key +ptr-name+ key)
    (let ((all-instance-variables (fix-code (gethash key (project-classes (analysis-project anal))) anal)))
      (dolist (instance-var all-instance-variables)
        (code-for-instance-var fout +ptr-name+ instance-var)))
    (format fout "    base = (char*)base + length;~%")
    (format fout "} break;~%")
    ))
(defun skipper-for-templated-lispallocs (fout enum anal)
  (assert (templated-enum-p enum))
  (let* ((key (enum-key enum))
         (enum-name (enum-name enum)))
    (gclog "build-mps-scan-for-one-family -> inheritance key[~a]  value[~a]~%" key value)
    (format fout "case ~a: {~%" enum-name)
    (format fout "    base = (char*)base + length;~%")
    (format fout "} break;~%")
    ))
(defun finalizer-for-templated-lispallocs (fout enum anal)
  (assert (templated-enum-p enum))
  (let* ((key (enum-key enum))
         (enum-name (enum-name enum))
         (ns-cn key)
         (cn (strip-all-namespaces-from-name ns-cn)))
    (gclog "build-mps-scan-for-one-family -> inheritance key[~a]  value[~a]~%" key value)
    (format fout "case ~a: {~%" enum-name)
    (format fout "    ~A* ~A = BasePtrToMostDerivedPtr<~A>(base);~%" key +ptr-name+ key)
    (format fout "    ~A->~~~A();~%" +ptr-name+ cn)
    (format fout "} break;~%")
    ))




(defun scanner-for-gccontainer (fout enum anal)
  (check-type enum simple-enum)
  (let* ((alloc (simple-enum-alloc enum))
         (decl (containeralloc-ctype alloc))
         (key (alloc-key alloc))
         (enum-name (enum-name enum)))
    (format fout "case ~a: {~%" enum-name)
    (format fout "// processing ~a~%" alloc)
    (if (cxxrecord-ctype-p decl)
        (progn
          (format fout "    THROW_HARD_ERROR(BF(\"Should never scan ~a\"));~%" (cxxrecord-ctype-key decl)))
        (let* ((parms (class-template-specialization-ctype-arguments decl))
               (parm0 (car parms))
               (parm0-ctype (gc-template-argument-ctype parm0)))
          (format fout "// parm0-ctype = ~a~%" parm0-ctype)
          (format fout "    ~A* ~A = BasePtrToMostDerivedPtr<~A>(base);~%" key +ptr-name+ key)
          (format fout "    for (~a::iterator it = ~a->begin(); it!=~a->end(); ++it) {~%" key +ptr-name+ +ptr-name+)
          (format fout "        // A scanner for ~a~%" parm0-ctype)
          (cond
            ((smart-ptr-ctype-p parm0-ctype)
             (format fout "          SMART_PTR_FIX(*it);~%"))
            ((cxxrecord-ctype-p parm0-ctype)
             (let ((all-instance-variables (fix-code (gethash (ctype-key parm0-ctype) (project-classes (analysis-project anal))) anal)))
               (dolist (instance-var all-instance-variables)
                 (code-for-instance-var fout "it" instance-var))))
            ((pointer-ctype-p parm0-ctype)
             (format fout "          POINTER_FIX(*it);~%" ))
            (t (error "Write code to scan ~a" parm0-ctype)))
          (format fout "    }~%")
;;              (format fout "    typedef typename ~A type_~A;~%" key enum-name)
;;              (format fout "    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_~a>(~a->capacity()))+AlignUp(sizeof(gctools::Header_s));~%" enum-name +ptr-name+)
;;              (format fout "    base = (char*)base + templatedHeader->size;~%" enum-name)))
              (format fout "    base = (char*)base + length;~%")
        (format fout "} break;~%")
        ))))


(defun skipper-for-gccontainer (fout enum anal)
  (check-type enum simple-enum)
  (let* ((alloc (simple-enum-alloc enum))
         (decl (containeralloc-ctype alloc))
         (key (alloc-key alloc))
         (enum-name (enum-name enum)))
    (format fout "case ~a: {~%" enum-name)
    (format fout "// processing ~a~%" alloc)
    (if (cxxrecord-ctype-p decl)
        (progn
          (format fout "    THROW_HARD_ERROR(BF(\"Should never scan ~a\"));~%" (cxxrecord-ctype-key decl)))
        (let* ((parms (class-template-specialization-ctype-arguments decl))
               (parm0 (car parms))
               (parm0-ctype (gc-template-argument-ctype parm0)))
          (format fout "// parm0-ctype = ~a~%" parm0-ctype)
;;          (format fout "    ~A* ~A = BasePtrToMostDerivedPtr<~A>(base);~%" key +ptr-name+ key)
;;          (format fout "    typedef typename ~A type_~A;~%" key enum-name)
;;          (format fout "    size_t header_and_gccontainer_size = AlignUp(sizeof_container<type_~a>(~a->capacity()))+AlignUp(sizeof(gctools::Header_s));~%" enum-name +ptr-name+)
;;          (format fout "    base = (char*)base + Align(header_and_gccontainer_size);~%")
              (format fout "    base = (char*)base + length;~%")
          ))
    (format fout "} break;~%")
    ))



(defun finalizer-for-gccontainer (fout enum anal)
  (check-type enum simple-enum)
  (let* ((alloc (simple-enum-alloc enum))
         (decl (containeralloc-ctype alloc))
         (key (alloc-key alloc))
         (enum-name (enum-name enum)))
    (format fout "case ~a: {~%" enum-name)
    (format fout "// processing ~a~%" alloc)
    (if (cxxrecord-ctype-p decl)
        (progn
          (format fout "    THROW_HARD_ERROR(BF(\"Should never finalize ~a\"));~%" (record-ctype-key decl)))
        (let* ((parms (class-template-specialization-ctype-arguments decl))
               (parm0 (car parms))
               (parm0-ctype (gc-template-argument-ctype parm0)))
          (format fout "// parm0-ctype = ~a~%" parm0-ctype)
          (format fout "    THROW_HARD_ERROR(BF(\"Should never finalize containers ~a\"));" (record-ctype-key decl))))
    (format fout "} break;~%")
    ))






(defun skipper-for-gcstring (fout enum anal)
  (check-type enum simple-enum)
  (let* ((alloc (simple-enum-alloc enum))
         (decl (containeralloc-ctype alloc))
         (key (alloc-key alloc))
         (enum-name (enum-name enum)))
    (format fout "case ~a: {~%" enum-name)
    (format fout "// processing ~a~%" alloc)
    (if (cxxrecord-ctype-p decl)
        (progn
          (format fout "    THROW_HARD_ERROR(BF(\"Should never scan ~a\"));~%" (cxxrecord-ctype-key decl)))
        (let* ((parms (class-template-specialization-ctype-arguments decl))
               (parm0 (car parms))
               (parm0-ctype (gc-template-argument-ctype parm0)))
          (format fout "// parm0-ctype = ~a~%" parm0-ctype)
;;          (format fout "    ~A* ~A = BasePtrToMostDerivedPtr<~A>(base);~%" key +ptr-name+ key)
;;          (format fout "    typedef typename ~A type_~A;~%" key enum-name)
;;          (format fout "    size_t header_and_gcstring_size = AlignUp(sizeof_container<type_~a>(~a->capacity()))+AlignUp(sizeof(gctools::Header_s));~%" enum-name +ptr-name+)
;;          (format fout "    base = (char*)base + Align(header_and_gcstring_size);~%")
          (format fout "    base = (char*)base + length;~%")
          ))
    (format fout "} break;~%")
    ))



(defun finalizer-for-gcstring (fout enum anal)
  (check-type enum simple-enum)
  (let* ((alloc (simple-enum-alloc enum))
         (decl (containeralloc-ctype alloc))
         (key (alloc-key alloc))
         (enum-name (enum-name enum)))
    (format fout "case ~a: {~%" enum-name)
    (format fout "// processing ~a~%" alloc)
    (if (cxxrecord-ctype-p decl)
        (progn
          (format fout "    THROW_HARD_ERROR(BF(\"Should never finalize ~a\"));~%" (record-ctype-key decl)))
        (let* ((parms (class-template-specialization-ctype-arguments decl))
               (parm0 (car parms))
               (parm0-ctype (gc-template-argument-ctype parm0)))
          (format fout "// parm0-ctype = ~a~%" parm0-ctype)
          (format fout "    THROW_HARD_ERROR(BF(\"Should never finalize gcstrings ~a\"));" (record-ctype-key decl))))
    (format fout "} break;~%")
    ))


(defun string-left-matches (str sub)
  (eql (search str sub) 0))


(defun setup-manager ()
  (let* ((manager (make-manager)))
    (add-species manager (make-species :name :system
                                       :discriminator (lambda (x) (and (lispalloc-p x) (stringp x) (search "system_" x)))
                                       :preprocessor-guard "SYSTEM_GUARD"
                                       :scan 'scanner-for-system
                                       :skip 'skipper-for-system
                                       :finalize 'finalizer-for-system
                                       ))
    (add-species manager (make-species :name :bootstrap
                                       :discriminator (lambda (x) (and (lispalloc-p x)
                                                                       (gctools::bootstrap-kind-p (alloc-key x))))
                                       :scan 'scanner-for-lispallocs
                                       :skip 'skipper-for-lispallocs
                                       :finalize 'finalizer-for-lispallocs
                                       ))
    (add-species manager (make-species :name :lispalloc
                                       :discriminator (lambda (x) (and (lispalloc-p x)
                                                                       (not (gctools:bootstrap-kind-p (alloc-key x)))
                                                                       (not (alloc-template-specializer-p x *analysis*))))
                                       :scan 'scanner-for-lispallocs
                                       :skip 'skipper-for-lispallocs
                                       :finalize 'finalizer-for-lispallocs
                                       ))
    (add-species manager (make-species :name :templated-lispalloc
                                       :discriminator (lambda (x) (and (lispalloc-p x) (alloc-template-specializer-p x *analysis*)))
                                       :scan 'scanner-for-templated-lispallocs
                                       :skip 'skipper-for-templated-lispallocs
                                       :finalize 'finalizer-for-templated-lispallocs
                                       ))
    (add-species manager (make-species :name :GCVECTOR
                                       :discriminator (lambda (x) (and (containeralloc-p x) (search "gctools::GCVector" (alloc-key x))))
                                       :scan 'scanner-for-gccontainer
                                       :skip 'skipper-for-gccontainer
                                       :finalize 'finalizer-for-gccontainer
                                       ))
    (add-species manager (make-species :name :GCARRAY
                                       :discriminator (lambda (x) (and (containeralloc-p x) (search "gctools::GCArray" (alloc-key x))))
                                       :scan 'scanner-for-gccontainer
                                       :skip 'skipper-for-gccontainer
                                       :finalize 'finalizer-for-gccontainer
                                       ))
    (add-species manager (make-species :name :GCSTRING
                                       :discriminator (lambda (x) (and (containeralloc-p x) (search "gctools::GCString" (alloc-key x))))
                                       :scan 'skipper-for-gcstring ;; don't need to scan
                                       :skip 'skipper-for-gcstring
                                       :finalize 'finalizer-for-gcstring
                                       ))
    (add-species manager (make-species :name :classalloc
                                       :discriminator (lambda (x) (and (classalloc-p x) (not (alloc-template-specializer-p x *analysis*))))
                                       :scan 'scanner-for-lispallocs
                                       :skip 'skipper-for-lispallocs
                                       :finalize 'finalizer-for-lispallocs
                                       ))
    (add-species manager (make-species :name :rootclassalloc
                                       :discriminator (lambda (x) (rootclassalloc-p x))
                                       :scan 'scanner-for-lispallocs
                                       :skip 'skipper-for-lispallocs
                                       :finalize 'finalizer-for-lispallocs
                                       ))
    (add-species manager (make-species :name :templated-classalloc
                                       :discriminator (lambda (x) (and (classalloc-p x) (alloc-template-specializer-p x *analysis*)))
                                       :scan 'scanner-for-templated-lispallocs
                                       :skip 'skipper-for-templated-lispallocs
                                       :finalize 'finalizer-for-templated-lispallocs
                                       ))
    manager))



(defun setup-system-species (analysis)
  "The MPS system requires a few enum's to function - put them 
in the system-species and assign them a Kind value.
Species are groups of enums that share the same obj_scan, obj_skip and obj_finalize code"
  (let ((system-species (lookup-species (analysis-manager analysis) :system))
        (bootstrap-species (lookup-species (analysis-manager analysis) :bootstrap)))
    (make-enum-for-alloc-if-needed (make-alloc :key "fwd") system-species analysis)
    (make-enum-for-alloc-if-needed (make-alloc :key "fwd2") system-species analysis)
    (make-enum-for-alloc-if-needed (make-alloc :key "pad1") system-species analysis)
    (make-enum-for-alloc-if-needed (make-alloc :key "pad") system-species analysis)
    ))




(defparameter *analysis* nil)
(defun analyze-project (&optional (project *project*))
  (setq *analysis* (make-analysis :project project
                                  :manager (setup-manager)))
  (setup-system-species *analysis*)
  (organize-allocs-into-species-and-create-enums *analysis*)
  (generate-forward-declarations *analysis*)
  (setf (analysis-classes-with-fixptrs *analysis*) (classes-that-contain-fixptrs project))
  (check-globals-for-fixptrs *analysis*)
  #|
;;  (multiple-value-bind (simple-enums templated-enums) ; ; ; ; ;
;;      (categorize-enums *analysis*)   ; ; ; ; ;
;;      (break "Do something with the enums") ; ; ; ; ;
;;      (setf (analysis-list-of-all-enums *analysis*) list-of-all-enums) ; ; ; ; ;
    ))
|#
  t
  )








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
       (format stream "//GCKind for ~a~%" enum)
       (format stream "template <> class gctools::GCKind<~A> {~%" (enum-key enum)))
      (t ;; templated-enum
       (format stream "//GCTemplatedKind for ~a~%" enum)
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
    ((string= (ctype-key x) "unsigned int") nil)
    ((string= (ctype-key x) "unsigned long") nil)
    ((string= (ctype-key x) "_Bool") nil)
    (t
     (warn "ignoring fix-code for ~a" x)))
  nil)

(defmethod fix-code ((x cxxrecord-ctype) analysis)
  (fix-code (gethash (cxxrecord-ctype-key x) (project-classes (analysis-project analysis))) analysis))

(defmethod fix-code ((x class-template-specialization-ctype) analysis)
  (cond
    ((string= (ctype-key x) "unsigned long") nil)
    (t
     (fix-code (gethash (ctype-key x) (project-classes (analysis-project analysis))) analysis))))

(defmethod fix-code ((x pointer-ctype) analysis)
  (warn "ignoring fix-code for ~a" x)
  nil)

(defmethod fix-code ((x smart-ptr-ctype) analysis) :smart-ptr-fix)

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
    ((isalloc-p (pointer-ctype-pointee x) (analysis-project analysis))
     (make-pointer-fixer :pointee-type (pointer-ctype-pointee x)))
    (t
      (warn "ignoring pointer-ctype ~a" x)
      nil))))
    

(defun sorted-enums (analysis)
  (let (enums)
    (maphash (lambda (key enum) (push enum enums)) (analysis-enums analysis))
    (sort enums #'(lambda (a b) (< (enum-value a) (enum-value b))))))




(defun build-mps-scan (fout anal)
  (let* ((sorted-enums (sorted-enums anal)))
    (dolist (enum sorted-enums)
      (funcall (species-scan (enum-species enum)) fout enum anal))))


(defun build-mps-skip (fout anal)
  (let* ((sorted-enums (sorted-enums anal)))
    (dolist (enum sorted-enums)
      (funcall (species-skip (enum-species enum)) fout enum anal))))


(defun build-mps-finalize (fout anal)
  (let* ((sorted-enums (sorted-enums anal)))
    (dolist (enum sorted-enums)
      (funcall (species-finalize (enum-species enum)) fout enum anal))))



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
  (let ((all-enums (analysis-enums anal)))
    (format fout "enum { KIND_null = 0, ~%")
    (maphash (lambda (key enum)
               (format fout "~A = ~A,~%" (enum-name enum) (enum-value enum)))
             (analysis-enums anal)
             )
    (format fout "}~%" )
    ))


(defun generate-kind-name-map (fout anal)
    (maphash (lambda (key enum)
      (let* ((enum-name (enum-name enum)))
        (format fout "   case ~A: return \"~A\";~%" enum-name enum-name)))
             (analysis-enums anal))
  )




(defun isalloc-p (ctype project)
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
(defmethod fix-variable-p ((var class-template-specialization-ctype) analysis) nil)
(defmethod fix-variable-p ((var constant-array-ctype) analysis) nil)
(defmethod fix-variable-p ((var pointer-ctype) analysis)
  (let ((pointee (pointer-ctype-pointee var)))
  (cond
    ((string= "core::Creator" (ctype-key pointee)) t) ;; some pointers to core::Creator are for derived classes of core::Creator
    ((string= "gctools::StackRoot" (ctype-key pointee)) nil) ;; I'll handle this explicitly
    ((unclassified-ctype-p pointee) nil)
    ((isalloc-p (pointer-ctype-pointee var) (analysis-project analysis)) t)
    (t nil))))
(defmethod fix-variable-p ((var cxxrecord-ctype) analysis) 
  (contains-fixptrs-p var (analysis-project analysis)))

(defmethod fix-variable-p ((var injected-class-name-ctype) analysis) nil)

  
(defgeneric fix-macro-name (var))                           
(defmethod fix-macro-name ((var global-variable))
  (fix-macro-name (global-variable-ctype var)))
(defmethod fix-macro-name ((var smart-ptr-ctype)) "SMART_PTR_FIX")
(defmethod fix-macro-name ((var pointer-ctype)) "POINTER_FIX")
(defmethod fix-macro-name ((var cxxrecord-ctype)) "RECORD_FIX")


(defun generate-code-for-global-variables (stream analysis)
  (maphash (lambda (k v)
             (when (fix-variable-p v analysis)
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

(defun generate-code (&optional (analysis *analysis*))
  (with-open-file (stream (make-pathname :name "clasp_gc" :type "cc" :defaults (main-directory-pathname)) :direction :output :if-exists :supersede)
    (format stream "#ifdef DECLARE_FORWARDS~%")
    (code-for-namespace-names stream (merge-forward-names-by-namespace analysis))
    (format stream "#endif // DECLARE_FORWARDS~%")
    (format stream "#if defined(GC_ENUM)~%")
    (generate-alloc-enum stream analysis)
    (format stream "#endif // defined(GC_ENUM)~%")
    (format stream "#if defined(GC_KIND_NAME_MAP)~%")
    (generate-kind-name-map stream analysis)
    (format stream "#endif // defined(GC_KIND_NAME_MAP)~%")
    (format stream "#if defined(GC_KIND_SELECTORS)~%")
    (generate-gckind-for-enums stream analysis)
    (format stream "#endif // defined(GC_KIND_SELECTORS)~%")
    (format stream "#if defined(GC_OBJ_SCAN)~%")
    (build-mps-scan stream analysis)
    (format stream "#endif // defined(GC_OBJ_SCAN)~%")
    (format stream "#if defined(GC_OBJ_SKIP)~%")
    (build-mps-skip stream analysis)
    (format stream "#endif // defined(GC_OBJ_SKIP)~%")
    (format stream "#if defined(GC_OBJ_FINALIZE)~%")
    (build-mps-finalize stream analysis)
    (format stream "#endif // defined(GC_OBJ_FINALIZE)~%")
    (format stream "#if defined(GC_GLOBALS)~%")
    (generate-code-for-global-variables stream analysis)
    (format stream "#endif // defined(GC_GLOBALS)~%")
    ))

;; ----------------------------------------------------------------------
;;
;; Setup the *tools* that will run over all of the source code and run
;; several matchers that scrape the C++ AST for info requires to build
;; garbage collection scanners.


(load-compilation-database "app:Contents;Resources;buildDatabases;clasp_compile_commands.json")

(defvar *tools* nil)
(defvar *arguments-adjuster* (lambda (args) (concatenate 'vector #-quiet args #+quiet(remove "-v" args)
                                                         #("-DUSE_MPS"
                                                           "-DRUNNING_GC_BUILDER"
                                                           "-resource-dir"
                                                           "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr//lib/clang/5.1"))))

(progn
  (setf *tools* (make-multitool :arguments-adjuster *arguments-adjuster*))
  (setup-cclass-search *tools*) ; search for all classes
  (setup-lispalloc-search *tools*)
  (setup-classalloc-search *tools*)
  (setup-rootclassalloc-search *tools*)
  (setup-containeralloc-search *tools*)
  (setup-global-variable-search *tools*)
  (setup-variable-search *tools*)
)


;; ----------------------------------------------------------------------
;; Test search
;; ----------------------------------------------------------------------
(progn
  (lnew $test-search)
  (setq $test-search (append
                      (lsel $* ".*cons\.cc$"))
                      )
  )


(defvar *project* nil)
(defun search-all (&key test tools (save t))
  (multitool-activate-tools *tools* tools)
  (setf (multitool-results *tools*) (make-project))
  (let ((alljobs (if test
                     $test-search
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





(defparameter *max-parallel-searches* 6)

(defun fork-jobs (proc job-list)
  (setf (multitool-results *tools*) (make-project))
  (batch-run-multitool *tools* :filenames job-list)
  (serialize:save-archive (multitool-results *tools*) (project-pathname (format nil "project~a" proc) "dat"))
  (core:exit))

(defvar *parallel-search-pids* nil)
(defun parallel-search-all (&key test)
  "Run *max-parallel-searches* processes at a time - whenever one finishes, start the next"
  (setq *parallel-search-pids* nil)
  (let ((all-jobs (if test
                      $test-search
                      (reverse (lremove (lremove $* ".*mps\.c$") ".*gc_interface\.cc$"))))
        (spare-processes *max-parallel-searches*))
    (serialize:save-archive all-jobs (project-pathname "project-all" "dat"))
    (format t "all-jobs: ~a~%" all-jobs)
    (dotimes (proc (length all-jobs))
      (setq spare-processes (1- spare-processes))
      (core:system "sleep 1")
      (let* ((job-list (list (elt all-jobs proc)))
             (pid (core:fork)))
        (if (eql 0 pid)
            (fork-jobs proc job-list)
            (when (eql spare-processes 0)
              (core:waitpid -1 0)
              (setq spare-processes (1+ spare-processes)))))
      (format t "Bottom of loop proc: ~a~%" proc)
      )
    (dotimes (proc (1- *max-parallel-searches*))
      (core:waitpid -1 0))
    (format t "~%!~%!  Done ~%!~%")
    (parallel-merge)
    ))




(defun parallel-merge (&key end (start 0) restart)
  "Merge the analyses generated from the parallel search"
  (let* ((all-jobs (serialize:load-archive (project-pathname "project-all" "dat")))
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
                      (serialize:load-archive (project-pathname (format nil "project~a" proc) "dat"))
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
             :arguments-adjuster-code (lambda (args) (concatenate 'vector #-quiet args #+quiet(remove "-v" args)
                                                                  #("-DUSE_MPS"
                                                                    "-DRUNNING_GC_BUILDER"
                                                                    "-resource-dir"
                                                                    "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr//lib/clang/5.1")))))



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
               :code #'(lambda ()
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

