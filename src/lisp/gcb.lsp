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
  )


(defstruct instance-variable
  "Represent an instance variable, it's name, it's source-location and it's classified type"
  field-name
  location
  ctype
  )



(defstruct kind
  key
  name ;; decl name
  location
  ctype)

(defstruct (lispkind (:include kind)))
(defstruct (classkind (:include kind)))
(defstruct (containerkind (:include kind)))



(defstruct enum
  gckind
  species
  enum-name
  species-num
  kind-num
  )



;; ----------------------------------------------------------------------
;;
;; Project and analysis classes
;;
;; ----------------------------------------------------------------------


(defstruct project
  "Store the results of matching to the entire codebase"
  ;; All class information
  (classes (make-hash-table :test #'equal))
  ;; Different kinds of objects(classes)
  (lispkinds (make-hash-table :test #'equal))   ; exposed to Lisp
  (classkinds (make-hash-table :test #'equal)) ; regular classes
  (containerkinds (make-hash-table :test #'equal)) ; containers
;;; Other
;;  (local-variables (make-hash-table :test #'equal))
;;  (global-variables (make-hash-table :test #'equal))
;;  (static-local-variables (make-hash-table :test #'equal))
;;  (new-gcobject-exprs (make-hash-table :test #'equal))
;;  (new-housekeeping-class-exprs (make-hash-table :test #'equal))
  )



(defstruct analysis
  project
  manager
  (forwards (make-hash-table :test #'equal))
  (kind-to-species (make-hash-table :test #'eq))
  (species-to-enum (make-hash-table :test #'eq))
  list-of-all-enums
  (contains-fixable-pointer (make-hash-table :test #'eq))
  )

(defmethod print-object ((object analysis) stream)
  (format stream "#S(ANALYSIS ...)"))




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
       "TEMPLATE-ARG-AS-STRING::PACK")
      (ast-tooling:template
       "TEMPLATE-ARG-AS-STRING::TEMPLATE")
      (ast-tooling:expression
       "TEMPLATE-ARG-AS-STRING::EXPRESSION")
      (ast-tooling:declaration
       "TEMPLATE-ARG-AS-STRING::DECLARATION")
      (otherwise
       (error "Add support for template-arg-as-string of kind: ~a" template-arg-kind)))))




(defgeneric record-key (node))

(defmethod record-key ((node cast:type))
  (cast:get-as-string (cast:make-qual-type node))) ;; get-as-string (cast:desugar node)))

(defmethod record-key ((node cast:record-type))
  "Names of RecordType(s) are calculated using recursive calls to record-key.
This avoids the prefixing of 'class ' and 'struct ' to the names of classes and structs"
  (record-key (cast:get-decl node)))

(defmethod record-key ((decl-node cast:named-decl))
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
       (warn "Add support for classify-ctype ~a" decl)
       (make-maybe-interesting-ctype :key (record-key decl-key))
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


(defparameter *method-submatcher*
  (compile-matcher '(:record-decl
                     (:for-each ;; -descendant
                      (:method-decl
                       (:bind :method (:method-decl)))))))


(defun setup-cclass-search (mtool)
  (symbol-macrolet ((results (project-classes (multitool-results mtool))))
    (labels ((%%new-class-callback (class-node record-key template-specializer)
               (let ((cname (mtag-name :whole))
                     bases vbases fields method-names )
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
                 ;;                   (when (string= record-key "gctools::StackRootedPointer<class asttooling::BAR>") (break "Check fields"))
                 (setf (gethash record-key results)
                       (make-cclass :key record-key
                                    :template-specializer template-specializer
                                    :location (mtag-loc-start :whole)
                                    :bases bases
                                    :vbases vbases
                                    :method-names method-names
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
                               (and (typep class-node 'cast:class-template-specialization-decl) ; ignore template specializations that have undeclared specialization kind
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
;; Search for lispkinds   as template parameters for GCObjectAllocator
;;


(defparameter *lispkind-matcher*
  '(:record-decl
    (:is-definition)
    (:is-template-instantiation)
    (:has-name "GCObjectAllocator"))
  )

(defun setup-lispkind-search (mtool)
  "Setup the TOOL (multitool) to look for instance variables that we want to garbage collect
and the inheritance hierarchy that the garbage collector will need"
  (symbol-macrolet ((class-results (project-lispkinds (multitool-results mtool))))
    (flet ((%%lispkind-matcher-callback ()
             "This function can only be called as a ASTMatcher callback"
             (let* ((decl (mtag-node :whole))
                    (args (cast:get-template-args decl))
                    (arg (cast:template-argument-list-get args 0))
                    (qtarg (cast:get-as-type arg))
                    (tsty-new (cast:get-type-ptr-or-null qtarg))
                    (class-key (record-key tsty-new))
                    (classified (classify-ctype tsty-new))
                    (class-location (mtag-loc-start :whole)))
               (unless (gethash class-key class-results)
                 (gclog "Adding class name: ~a~%" class-name)
                 (let ((lispkind (make-lispkind :key class-key
                                                :name (mtag-name :whole)
                                                :location class-location
                                                :ctype classified)))
                   (setf (gethash class-key class-results) lispkind)
                   )))))
      ;; Initialize the class search
      (multitool-add-matcher mtool
                             :name :lispkinds
                             :matcher (compile-matcher `(:bind :whole ,*lispkind-matcher*))
                             :initializer (lambda () (setf class-results (make-hash-table :test #'equal))) ; initializer
                             :callback (make-instance 'code-match-callback :code (function %%lispkind-matcher-callback))))))




;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;;
;; Search for classkinds - they are template parameters for ClassAllocator
;;


(defparameter *classkind-matcher*
  '(:record-decl
    (:is-definition)
    (:is-template-instantiation)
    (:has-name "ClassAllocator"))
  )

(defun setup-classkind-search (mtool)
  "Setup the TOOL (multitool) to look for instance variables that we want to garbage collect
and the inheritance hierarchy that the garbage collector will need"
  (symbol-macrolet ((class-results (project-classkinds (multitool-results mtool))))
    (flet ((%%classkind-matcher-callback ()
             "This function can only be called as a ASTMatcher callback"
             (let* ((decl (mtag-node :whole))
                    (args (cast:get-template-args decl))
                    (arg (cast:template-argument-list-get args 0))
                    (qtarg (cast:get-as-type arg))
                    (tsty-new (cast:get-type-ptr-or-null qtarg))
                    (class-key (record-key tsty-new))
                    (classified (classify-ctype tsty-new))
                    (class-location (mtag-loc-start :whole)))
               (unless (gethash class-key class-results)
                 (gclog "Adding class name: ~a~%" class-name)
                 (let ((classkind (make-classkind :key class-key
                                                  :name (mtag-name :whole)
                                                  :location class-location
                                                  :ctype classified)))
                   (setf (gethash class-key class-results) classkind)
                   )))))
      ;; Initialize the class search
      (multitool-add-matcher mtool
                             :name :classkinds
                             :matcher (compile-matcher `(:bind :whole ,*classkind-matcher*))
                             :initializer (lambda () (setf class-results (make-hash-table :test #'equal))) ; initializer
                             :callback (make-instance 'code-match-callback :code (function %%classkind-matcher-callback))))))






;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;;
;; Search for containerkinds - they are template parameters for GCContainerAllocator
;;


(defparameter *containerkind-matcher*
  '(:record-decl
    (:is-definition)
    (:is-template-instantiation)
    (:is-same-or-derived-from
     (:record-decl
      (:has-name "GCContainer")))))

(defun setup-containerkind-search (mtool)
  "Setup the TOOL (multitool) to look for instance variables that we want to garbage collect
and the inheritance hierarchy that the garbage collector will need"
  (symbol-macrolet ((class-results (project-containerkinds (multitool-results mtool))))
    (flet ((%%containerkind-matcher-callback ()
             "This function can only be called as a ASTMatcher callback"
             (let* ((decl (mtag-node :whole))
                    (class-key (record-key decl))
                    (classified (classify-decl decl))
                    (class-location (mtag-loc-start :whole)))
;;               (break "Check containerkind")
               (unless (gethash class-key class-results)
                 (gclog "Adding class name: ~a~%" class-name)
                 (let ((containerkind (make-containerkind :key class-key
                                                          :name (mtag-name :whole)
                                                          :location class-location
                                                          :ctype classified)))
                   (setf (gethash class-key class-results) containerkind)
                   )))))
      ;; Initialize the class search
      (multitool-add-matcher mtool
                             :name :containerkinds
                             :matcher (compile-matcher `(:bind :whole ,*containerkind-matcher*))
                             :initializer (lambda () (setf class-results (make-hash-table :test #'equal))) ; initializer
                             :callback (make-instance 'code-match-callback :code (function %%containerkind-matcher-callback))))))


;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;;
;; stage: analysis
;; Carry out analysis on the project
;;
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------




(defparameter *contains-fixable-pointer-impl-ht* nil)
(defun contains-fixable-pointer-p (x analysis)
  (let ((*contains-fixable-pointer-impl-ht* (make-hash-table :test #'eq)))
    (contains-fixable-pointer-impl-p x analysis)))

(defgeneric contains-fixable-pointer-impl-p (x analysis))

(defmethod contains-fixable-pointer-impl-p :around (x analysis)
  (multiple-value-bind (precalc precalc-p)
      (gethash x *contains-fixable-pointer-impl-ht*)
    (if precalc-p
        precalc
        (progn
          (setf (gethash x *contains-fixable-pointer-impl-ht*) nil)
          (let ((res (call-next-method x analysis)))
            (setf (gethash x *contains-fixable-pointer-impl-ht*) res)
            res)))))


(defmethod contains-fixable-pointer-impl-p ((x cclass) analysis)
  (let* (result
         (project (analysis-project analysis))
         (all-classes (project-classes project)))
    (loop :for base-name :in (cclass-bases x)
       :do (when base-name
             (let ((base (gethash base-name all-classes)))
               (when base 
                 (let ((one-result (contains-fixable-pointer-impl-p base analysis)))
                   (setq result (or result one-result)))))))
    (loop :for base-name :in (cclass-vbases x)
       :do (when base-name
             (let ((base (gethash base-name all-classes)))
               (when base 
                 (let ((one-result (contains-fixable-pointer-impl-p base analysis)))
                   (setq result (or result one-result)))))))
    (loop :for field :in (cclass-fields x)
       :do (when field
             (let* ((field-ctype (instance-variable-ctype field))
                    (one-result (contains-fixable-pointer-impl-p field-ctype analysis)))
               (setq result (or result one-result)))))
    result))


(defmethod contains-fixable-pointer-impl-p ((x unclassified-ctype) analysis) nil)
(defmethod contains-fixable-pointer-impl-p ((x smart-ptr-ctype) analysis) t)
(defmethod contains-fixable-pointer-impl-p ((x pointer-ctype) analysis)
  (cond
    ((container-p (pointer-ctype-pointee x)) t)
    ((string= (ctype-key (pointer-ctype-pointee x)) "gctools::GCString_moveable<char>" ) t)
    ((contains-fixable-pointer-impl-p (pointer-ctype-pointee x) analysis) 
     (break "Caught contains-fixable-pointer-impl-p for pointer to something that contains fixable-pointers")
     t
     )
    ((null (pointer-ctype-key x)) :invalid)
    (t
     (warn "Handle contains-fixable-pointer-impl-p for ~a" x)
     nil)))
           


(defun add-ctype (forwards key kind-ctype)
  (when (string= key "class core::SequenceStepper *")
    (break "trap"))
  (setf (gethash key forwards) kind-ctype))

(defgeneric expand-forwards-with-template-arguments (forwards project kind-ctype))
; don't need anything for cxxrecord-ctype
(defmethod expand-forwards-with-template-arguments (forwards project (kind-ctype cxxrecord-ctype)) nil)
(defmethod expand-forwards-with-template-arguments (forwards project (kind-ctype null)) nil)
(defmethod expand-forwards-with-template-arguments (forwards project (kind-ctype unclassified-ctype)) nil)
(defmethod expand-forwards-with-template-arguments (forwards project (kind-ctype smart-ptr-ctype)) nil)
(defmethod expand-forwards-with-template-arguments (forwards project (kind-ctype cxxrecord-ctype))
  (add-ctype forwards (ctype-key kind-ctype) kind-ctype))
(defmethod expand-forwards-with-template-arguments (forwards project (kind-ctype pointer-ctype))
  (expand-forwards-with-template-arguments forwards project (pointer-ctype-pointee kind-ctype)))

                                                    
(defmethod expand-forwards-with-template-arguments (forwards project (kind-ctype class-template-specialization-ctype))
  (mapc (lambda (template-arg) (expand-forwards-with-template-arguments forwards project (gc-template-argument-ctype template-arg)))
        (class-template-specialization-ctype-arguments kind-ctype)))

(defun fill-forward-declarations (forwards project kind)
  (expand-forwards-with-template-arguments forwards project (kind-ctype kind)))
  

(defun generate-forward-declarations (&optional (analysis *analysis*))
  (let* ((project (analysis-project analysis))
         (forwards (analysis-forwards analysis))
         (forward-names nil))
    (maphash (lambda (k v) (fill-forward-declarations forwards project v)) (project-lispkinds project))
    (maphash (lambda (k v) (fill-forward-declarations forwards project v)) (project-classkinds project))
    (maphash (lambda (k v) (fill-forward-declarations forwards project v)) (project-containerkinds project))
    ))





(defun compact-enum-value (species-num kind-num)
  (logior (ash species-num 16) kind-num))

(defun list-of-all-enums (analysis)
  (let (all)
    (mapc (lambda (species)
            (let ((enums (gethash species (analysis-species-to-enum analysis))))
              (when enums
                (mapc (lambda (e)
                        (push e all))
                      enums))
              )) (manager-species (analysis-manager analysis)))
    (sort all (lambda (a b) (< (compact-enum-value (enum-species-num a) (enum-kind-num a))
                               (compact-enum-value (enum-species-num b) (enum-kind-num b)))))))






(defun ctype-name (classid) classid)


(defun class-enum-name (classid species &optional (prefix "KIND"))
  (let* ((raw-name (copy-seq (ctype-name classid)))
         (name0 (nsubstitute-if #\_ (lambda (c) (member c '(#\SPACE #\, #\< #\> #\: ))) raw-name))
         (name (nsubstitute #\P #\* name0))
         )
    (format nil "~a_~a_~a" prefix (symbol-name (species-name species)) name)))








(defstruct species
  name
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
         
        

(defun assign-kind-species (gco species anal &optional kind-num)
  (unless kind-num (setq kind-num (length (gethash species (analysis-species-to-enum anal) nil))))
  (let* ((manager (analysis-manager anal))
         (species-num (species-index species))
         (key (kind-key gco))
         (gco-info (make-enum :gckind gco
                                :species species
                                :enum-name (class-enum-name key species)
                                :species-num species-num
                                :kind-num kind-num ))
         )
    (setf (gethash gco (analysis-kind-to-species anal)) species) ; what species does a gco belong to
    (push gco-info (gethash species (analysis-species-to-enum anal)))
         ))



(defun analyze-kind-and-assign-species (gco anal)
  (let* ((manager (analysis-manager anal))
         (species (identify-species manager gco)))
    (when species
        (assign-kind-species gco species anal))))

(defun organize-kinds-into-species (analysis &aux (project (analysis-project analysis)))
  "Every GCObject and GCContainer is assigned to a species and given a GCKind enum value."
  (let ((project (analysis-project analysis)))
    (maphash (lambda (k v) (analyze-kind-and-assign-species v analysis)) (project-lispkinds project))
    (maphash (lambda (k v) (analyze-kind-and-assign-species v analysis)) (project-containerkinds project))
    (maphash (lambda (k v) (analyze-kind-and-assign-species v analysis)) (project-classkinds project))
    ))

(defun sort-enums-by-index (enums)
  (sort (copy-list enums) #'(lambda (a b) (< (enum-kind-num a) (enum-kind-num b)))))


(defun scanner-for-system (fout species anal)
  (let ((enums (sort-enums-by-index (gethash species (analysis-species-to-enum anal)))))
    (dolist (gce enums)
      (let ((gckind (enum-gckind gce)))
        (format fout "case ~a: {~%" (enum-enum-name gce))
        (cond
          ((string= "fwd2" (gckind-key gckind))
           (format fout "THROW_HARD_ERROR(BF(\"~a should never be scanned\"));~%" (gckind-key gckind)))
          ((string= "fwd" (gckind-key gckind))
           (format fout "base = (char*)base + ALIGN_UP(header->fwd._Size);~%"))
          ((string= "pad1" (gckind-key gckind))
           (format fout "base = (char*)base + ALIGN_UP(sizeof_with_header<Pad1_s>());~%"))
          ((string= "pad" (gckind-key gckind))
           (format fout "base = (char*)base + ALIGN_UP(header->pad._Size);~%"))
          (t (error "Illegal scanner-for-system species ~a" species)))
        (format fout "break;~%}~%")
        ))
    ))



(defun skipper-for-system (fout species anal)
  (let ((enums (sort-enums-by-index (gethash species (analysis-species-to-enum anal)))))
    (dolist (gce enums)
      (let ((gckind (enum-gckind gce)))
        (format fout "case ~a: {~%" (enum-enum-name gce))
        (cond
          ((string= "fwd2" (gckind-key gckind))
           (format fout "THROW_HARD_ERROR(BF(\"~a should never be skipped\"));~%" (gckind-key gckind)))
          ((string= "fwd" (gckind-key gckind))
           (format fout "base = (char*)base + ALIGN_UP(header->fwd._Size);~%"))
          ((string= "pad1" (gckind-key gckind))
           (format fout "base = (char*)base + ALIGN_UP(sizeof_with_header<Pad1_s>());~%"))
          ((string= "pad" (gckind-key gckind))
           (format fout "base = (char*)base + ALIGN_UP(header->pad._Size);~%"))
          (t (error "Illegal skipper-for-system species ~a" species)))
        (format fout "break;~%}~%")
        ))
    ))

(defun finalizer-for-system (fout species anal)
  (let ((enums (sort-enums-by-index (gethash species (analysis-species-to-enum anal)))))
    (dolist (gce enums)
      (let ((gckind (enum-gckind gce)))
        (format fout "case ~a: {~%" (enum-enum-name gce))
        (format fout "    THROW_HARD_ERROR(BF(\"~a should never be finalized\"));~%" (gckind-key gckind)))
      (format fout "    break;~%}~%"))))
        


(defun scanner-for-one-gcobject (fout classid species anal)
  (let* ((enum-name (class-enum-name classid species))
         (inheritance (project-lispkinds (analysis-project anal)))
         (all-instance-variables (gather-instance-variables classid inheritance)))
    (gclog "build-mps-scan-for-one-family -> inheritance classid[~a]  value[~a]~%" (ctype-name classid) value)
    (format fout "case ~a: {~%" enum-name)
    (format fout "    ~A* ~A = reinterpret_cast<~A*>(obj_ptr);~%" (ctype-name classid) +ptr-name+ (ctype-name classid))
    (maphash #'(lambda (k v &aux (csp (contains-fixable-pointer-p (instance-variable-ctype v) anal)))
                 (cond
                   ((null csp))
                   ((eq csp :maybe) (code-for-instance-var fout +ptr-name+ k (instance-variable-ctype v) :maybe))
                   ((search "gc_ignore" k) (format fout "// Ignoring instance variable ~a // ~a~%" k (instance-variable-ctype v)))
                   (t (code-for-instance-var fout +ptr-name+ k (instance-variable-ctype v) t))))
             all-instance-variables)
    (format fout "    typedef ~A type_~A;~%" (ctype-name classid) enum-name)
    (format fout "    base = (char*)base + ALIGN(SIZEOF_WITH_HEADER(type_~A));~%" enum-name)
    (format fout "} break;~%")
    ))


(defun scanner-for-lispkinds (fout species anal)
  (let ((enums (sort-enums-by-index (gethash species (analysis-species-to-enum anal)))))
    (dolist (enum enums)
      (let* ((entry (enum-gckind enum))
             (classid (gckind-key entry)))
        (scanner-for-one-gcobject fout classid species anal)))))


(defun skipper-for-lispkinds (fout species anal)
  (let ((enums (sort-enums-by-index (gethash species (analysis-species-to-enum anal)))))
    (dolist (enum enums)
      (let* ((entry (enum-gckind enum))
             (classid (gckind-key entry))
             (enum-name (class-enum-name classid species)))
        (gclog "skipper-for-lispkinds -> inheritance classid[~a]  value[~a]~%" (ctype-name classid) value)
        (format fout "case ~a: {~%" enum-name)
        (format fout "    typedef ~A type_~A;~%" (ctype-name classid) enum-name)
        (format fout "    base = (char*)base + ALIGN(SIZEOF_WITH_HEADER(type_~A));~%" enum-name)
        (format fout "} break;~%")))
    ))


(defun finalizer-for-lispkinds (fout species anal)
  (let ((enums (sort-enums-by-index (gethash species (analysis-species-to-enum anal)))))
    (dolist (enum enums)
      (let* ((entry (enum-gckind enum))
             (classid (gckind-key entry))
             (enum-name (class-enum-name classid species))
             (ns-cn (ctype-name classid))
             (cn (let ((ns-divider (search "::" ns-cn)))
                   (if ns-divider
                       (subseq ns-cn (+ ns-divider 2) (length ns-cn))
                       ns-cn))))
        (gclog "build-mps-finalize-for-one-family -> inheritance classid[~a]  value[~a]~%" (ctype-name classid) value)
        (format fout "case ~a: {~%" enum-name)
        (format fout "    ~A* ~A = reinterpret_cast<~A*>(obj_ptr);~%" ns-cn +ptr-name+ ns-cn)
        (format fout "    ~A->~~~A();~%" +ptr-name+ cn)
        (format fout "    return;~%")
        (format fout "} break;~%")))
    ))




(defun test-tcode-gcvector (&optional (anal *analysis*))
  (with-open-file (fout (make-pathname :name "test-gcvector" :type "cc" :defaults (main-directory-pathname)) :direction :output :if-exists :supersede)
    (let ((gcvector-species (lookup-species (analysis-manager anal) :gcvector)))
      (format t "Processing ~a entries~%" (length (gethash gcvector-species (analysis-species-to-enum anal))))
;;      (scanner-for-gcvector fout gcvector-species anal)
      )))


(defun scanner-for-gccontainer (fout species anal)
  (let ((enums (sort-enums-by-index (gethash species (analysis-species-to-enum anal)))))
    (dolist (enum enums)
      (let* ((entry (enum-gckind enum))
             (decl (gccontainer-decl-type entry))
             (classid (gckind-key entry))
             (enum-name (class-enum-name classid species)))
        (format fout "case ~a: {~%" enum-name)
        (format fout "// processing ~a~%" entry)
        (if (cxxrecord-ctype-p decl)
            (progn
              (format fout "    THROW_HARD_ERROR(BF(\"Should never scan ~a\"));~%" (cxxrecord-ctype-key decl)))
            (let* ((parms (class-template-specialization-ctype-arguments decl))
                   (parm0 (car parms))
                   (parm0-ctype (gc-template-argument-ctype parm0)))
              (format fout "// parm0-ctype = ~a~%" parm0-ctype)
              (format fout "    ~A* ~A = reinterpret_cast<~A*>(obj_ptr);~%" (ctype-name classid) +ptr-name+ (ctype-name classid))
              (format fout "    for (~a::iterator it = ~a->begin(); it!=~a->end(); ++it) {~%" (ctype-name classid) +ptr-name+ +ptr-name+)
              (format fout "        // A scanner for ~a~%" parm0-ctype)
              (cond
                ((smart-ptr-ctype-p parm0-ctype)
                 (format fout "          SMART_PTR_FIX(*it);~%"))
                ((cxxrecord-ctype-p parm0-ctype)
                 (format fout "          it->onHeapScanGCRoots(GC_SCAN_ARGS_PASS);~%"))
                (t (error "Write code to scan ~a" parm0-ctype)))
                 
              #|        (maphash #'(lambda (k v &aux (csp (contains-fixable-pointer-p (instance-variable-ctype v) anal)))
              (cond
              ((null csp))
              ((eq csp :maybe) (code-for-instance-var fout +ptr-name+ k (instance-variable-ctype v) :maybe))
              ((search "gc_ignore" k) (format fout "// Ignoring instance variable ~a // ~a~%" k (instance-variable-ctype v)))
              (t (code-for-instance-var fout +ptr-name+ k (instance-variable-ctype v) t))))
              all-instance-variables)
                |#
              (format fout "    }~%")
              (format fout "    typedef typename ~A type_~A;~%" (ctype-name classid) enum-name)
              (format fout "    size_t header_and_gccontainer_size = ALIGN_UP(gc_sizeof<type_~a>(~a->capacity()))+ALIGN_UP(sizeof(gctools::Header_s));~%" enum-name +ptr-name+)
              (format fout "    base = (char*)base + ALIGN(header_and_gccontainer_size);~%"))))
        (format fout "} break;~%")
        )))


(defun skipper-for-gccontainer (fout species anal)
  (let ((enums (sort-enums-by-index (gethash species (analysis-species-to-enum anal)))))
    (dolist (enum enums)
      (let* ((entry (enum-gckind enum))
             (decl (gccontainer-decl-type entry))
             (classid (gckind-key entry))
             (enum-name (class-enum-name classid species)))
        (format fout "case ~a: {~%" enum-name)
        (format fout "// processing ~a~%" entry)
        (if (cxxrecord-ctype-p decl)
            (progn
              (format fout "    THROW_HARD_ERROR(BF(\"Should never scan ~a\"));~%" (cxxrecord-ctype-key decl)))
            (let* ((parms (class-template-specialization-ctype-arguments decl))
                   (parm0 (car parms))
                   (parm0-ctype (gc-template-argument-ctype parm0)))
              (format fout "// parm0-ctype = ~a~%" parm0-ctype)
              (format fout "    ~A* ~A = reinterpret_cast<~A*>(base);~%" (ctype-name classid) +ptr-name+ (ctype-name classid))
              (format fout "    typedef typename ~A type_~A;~%" (ctype-name classid) enum-name)
              (format fout "    size_t header_and_gccontainer_size = ALIGN_UP(gc_sizeof<type_~a>(~a->capacity()))+ALIGN_UP(sizeof(gctools::Header_s));~%" enum-name +ptr-name+)
              (format fout "    base = (char*)base + ALIGN(header_and_gccontainer_size);~%")
              )))
      (format fout "} break;~%")
      )))



(defun finalizer-for-gccontainer (fout species anal)
  (let ((enums (sort-enums-by-index (gethash species (analysis-species-to-enum anal)))))
    (dolist (enum enums)
      (let* ((entry (enum-gckind enum))
             (decl (gccontainer-decl-type entry))
             (classid (gckind-key entry))
             (enum-name (class-enum-name classid species)))
        (format fout "case ~a: {~%" enum-name)
        (format fout "// processing ~a~%" entry)
        (if (cxxrecord-ctype-p decl)
            (progn
              (format fout "    THROW_HARD_ERROR(BF(\"Should never finalize ~a\"));~%" (record-ctype-key decl)))
            (let* ((parms (class-template-specialization-ctype-arguments decl))
                   (parm0 (car parms))
                   (parm0-ctype (gc-template-argument-ctype parm0)))
              (format fout "// parm0-ctype = ~a~%" parm0-ctype)
              (format fout "    THROW_HARD_ERROR(BF(\"Should never finalize ~a\"));" (record-ctype-key decl)))))
      (format fout "} break;~%")
      )))




(defun setup-manager ()
  (let* ((manager (make-manager)))
    (add-species manager (make-species :name :system
                                       :discriminator (lambda (x) (and (lispkind-p x) (stringp x) (search "system_" x)))
                                       :scan 'scanner-for-system
                                       :skip 'skipper-for-system
                                       :finalize 'finalizer-for-system
                                       ))
    (add-species manager (make-species :name :bootstrap
                                       :discriminator (lambda (x) (and (lispkind-p x)
                                                                       (gctools::bootstrap-kind-p (kind-key x))))
                                       :scan 'scanner-for-lispkinds
                                       :skip 'skipper-for-lispkinds
                                       :finalize 'finalizer-for-lispkinds
                                       ))
    (add-species manager (make-species :name :gcobject
                                       :discriminator (lambda (x) (and (lispkind-p x)
                                                                       (not (gctools:bootstrap-kind-p (kind-key x)))
                                                                       (not (string= "clbind::Wrapper" (kind-name x)))
                                                                       (not (string= "clbind::Iterator" (kind-name x)))
                                                                       ))
                                       :scan 'scanner-for-lispkinds
                                       :skip 'skipper-for-lispkinds
                                       :finalize 'finalizer-for-lispkinds
                                       ))
    (add-species manager (make-species :name :WRAPPER
                                       :discriminator (lambda (x) (progn
                                                                    (and (lispkind-p x)
                                                                         (string= "clbind::Wrapper" (kind-name x)))))
                                       :scan 'scanner-for-lispkinds
                                       :skip 'skipper-for-lispkinds
                                       :finalize 'finalizer-for-lispkinds
                                       ))
    (add-species manager (make-species :name :ITERATOR
                                       :discriminator (lambda (x) (and (lispkind-p x)
                                                                       (string= "clbind::Iterator" (kind-name x))))
                                       :scan 'scanner-for-lispkinds
                                       :skip 'skipper-for-lispkinds
                                       :finalize 'finalizer-for-lispkinds
                                       ))
    (add-species manager (make-species :name :GCVECTOR
                                       :discriminator (lambda (x) (and (containerkind-p x)
                                                                       (search "gctools::GCVector" (kind-key x))))
                                       :scan 'scanner-for-gccontainer
                                       :skip 'skipper-for-gccontainer
                                       :finalize 'finalizer-for-gccontainer
                                       ))
    (add-species manager (make-species :name :GCARRAY
                                       :discriminator (lambda (x) (and (containerkind-p x)
                                                                       (search "gctools::GCArray" (kind-key x))))
                                       :scan 'scanner-for-gccontainer
                                       :skip 'skipper-for-gccontainer
                                       :finalize 'finalizer-for-gccontainer
                                       ))
    (add-species manager (make-species :name :GCSTRING
                                       :discriminator (lambda (x) (and (containerkind-p x)
                                                                       (search "gctools::GCString" (kind-key x))))
                                       :scan 'scanner-for-gccontainer
                                       :skip 'skipper-for-gccontainer
                                       :finalize 'finalizer-for-gccontainer
                                       ))
    (add-species manager (make-species :name :class
                                       :discriminator (lambda (x) (and (classkind-p x)))
                                       :scan 'scanner-for-gccontainer
                                       :skip 'skipper-for-gccontainer
                                       :finalize 'finalizer-for-gccontainer
                                       ))
    manager))



(defun setup-system-species (analysis)
  "The MPS system requires a few GCKind enum's to function - put them 
in the system-species and assign them a GCKind value"
  (let ((system-species (lookup-species (analysis-manager analysis) :system))
        (bootstrap-species (lookup-species (analysis-manager analysis) :bootstrap)))
    (assign-kind-species (make-kind :key "fwd" :name "fwd") system-species analysis 1)
    (assign-kind-species (make-kind :key "fwd2" :name "fwd2") system-species analysis 2)
    (assign-kind-species (make-kind :key "pad1" :name "pad1") system-species analysis 3)
    (assign-kind-species (make-kind :key "pad" :name "pad") system-species analysis 4)
    ))




(defparameter *analysis* nil)
(defun analyze-project (&optional (project *project*))
  (setq *analysis* (make-analysis :project project
                                  :manager (setup-manager)
                                  ))
  (setup-system-species *analysis*)
  (generate-forward-declarations *analysis*)
  (organize-kinds-into-species *analysis*)
  (let ((*track-reachability t)
        (list-of-all-enums (list-of-all-enums *analysis*)))
    (setf (analysis-list-of-all-enums *analysis*) list-of-all-enums)
    )
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


(defstruct array-fixer
  element-type)

(defstruct pointer-fixer
  pointee-type)

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
  (warn "ignoring fix-code for ~a" x)
  nil)

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
  (make-array-fixer :element-type (constant-array-ctype-element-type x)))


(defmethod fix-code ((x pointer-ctype) analysis )
  (let ((pointee (pointer-ctype-pointee x)))
    (if (or (gcvector-moveable-ctype-p pointee)
              (gcarray-moveable-ctype-p pointee)
              (gcstring-moveable-ctype-p pointee))
        (make-pointer-fixer :pointee-type (pointer-ctype-pointee x))
        (progn
          (warn "ignoring pointer-ctype ~a" x)
          nil))))
    


(defun generate-fix-code (analysis)
  (let ((project (analysis-project analysis)))
    (maphash (lambda (k v)
               (format t "Fix for ~a~%" k)
               (format t "   ~a~%" (fix-code (gethash k (project-classes project)) analysis)))
             (project-lispkinds project))
    (maphash (lambda (k v)
               (format t "Fix for ~a~%" k)
               (format t "   ~a~%" (fix-code (gethash k (project-classes project)) analysis)))
             (project-containerkinds project))
    (maphash (lambda (k v)
               (format t "Fix for ~a~%" k)
               (format t "   ~a~%" (fix-code (gethash k (project-classes project)) analysis)))
             (project-classkinds project))
    ))

(defun separate-namespace-name (name)
"Separate a X::Y name into (values X Y) - strip any preceeding 'class '"
  (let* ((full-name (if (string= (subseq name 0 (length "class ")) "class ")
                        (subseq name (length "class ") (length name))
                        name))
         (colon-pos (search "::" full-name))
         (namespace (subseq full-name 0 colon-pos))
         (name (subseq full-name (+ colon-pos 2) (length full-name))))
    (values namespace name)))

        
(defun sort-forward-declarations (analysis)
  "Sort the forward declaration names by their length - that will sort them
in order they need to be forward declared because longer ones need shorter ones"
  (let ((forwards (analysis-forwards analysis))
        forward-names)
    (maphash (lambda (k v) (push k forward-names)) forwards)
    (sort forward-names #'(lambda (x y) (< (length x) (length y))))))


(defun remove-namespace (namespace name)
  (let* ((prefixed (concatenate 'string namespace "::"))
         (pos (search prefixed name)))
    (if pos
        (let ((removed (concatenate 'string (subseq name 0 pos) (subseq name (+ pos (length prefixed)) (length name)))))
          (remove-namespace namespace removed))
        name)))

(defun remove-class-space (name)
  (let* ((class-space "class ")
         (pos (search class-space name)))
    (if pos
        (let ((removed (concatenate 'string (subseq name 0 pos) (subseq name (+ pos (length class-space)) (length name)))))
          (remove-class-space removed))
        name)))

(defun merge-forward-names-by-namespace (analysis)
  (let ((forward-names (sort-forward-declarations analysis))
        merged-groups
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

(defun prefix-template-if-needed (name)
  (if (search "<" name)
      "template <>"
      ""))

(defun code-forwards (stream &optional (analysis *analysis*))
  (let ((forwards (merge-forward-names-by-namespace analysis)))
    (dolist (l forwards)
      (let ((namespace (car l)))
        (format stream "namespace ~a {~%" namespace)
        (dolist (one (cdr l))
          (let ((name (remove-class-space (remove-namespace namespace one))))
            (format stream "    ~a class ~a;~%" (prefix-template-if-needed name) name)))
        (format stream "};~%")
        ))
    ))
          

(defun code-enums (stream analysis)
  (format stream "enum { }~%")
)



(defun generate-kind-enum (fout anal)
  (let ((all-enums (analysis-list-of-all-enums anal)))
    (format fout "enum { KIND_null = 0, ~%")
    (dolist (entry all-enums)
      (let ((enum-name (enum-enum-name entry))
            (enum-val (compact-enum-value (enum-species-num entry) (enum-kind-num entry))))
        (format fout "~A = 0x~x,~%" enum-name enum-val)))
    (format fout "}~%" )
    ))




(defun generate-code (&optional (analysis *analysis*))
  (with-open-file (stream (make-pathname :name "clasp_gc" :type "cc" :defaults (main-directory-pathname)) :direction :output :if-exists :supersede)
    (format stream "#ifdef DECLARE_FORWARDS~%")
    (code-forwards stream analysis)
    (format stream "#endif // DECLARE_FORWARDS~%")
    (format stream "#if defined(GC_ENUM)~%")
    (code-enums stream analysis)
    (generate-kind-enum stream analysis)
    (format stream "#endif // defined(GC_ENUM)~%")
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
  (setup-lispkind-search *tools*)
  (setup-classkind-search *tools*)
  (setup-containerkind-search *tools*)
)


;; ----------------------------------------------------------------------
;; Single search
;; ----------------------------------------------------------------------
(progn
  (lnew $test-search)
  (setq $test-search (subseq $* 0 10))
  )
;; ----------------------------------------------------------------------
;; Single search
;; ----------------------------------------------------------------------
(progn
  (lnew $test-search)
  (setq $test-search (append
                      (lsel $* ".*/metaClass\.cc")
                      ))
  )


(defvar *project* nil)
(defun search-all (&key test tools)
  (multitool-activate-tools *tools* tools)
  (setf (multitool-results *tools*) (make-project))
  (batch-run-multitool *tools* :filenames (if test
                                              $test-search
                                              (reverse (lremove (lremove $* ".*mps\.c$") ".*gc_interface\.cc$"))))
  ;; Extract the results for easy access
  (setq *project* (multitool-results *tools*))
  (core:system (format nil "vmmap -w ~a" (core:getpid)))
  (save-project)
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

(defvar *parallel-search-pids* nil)
(defun parallel-search-all (&key test)
;;  (error "fork doesn't work with the Boehm garbage collector")
  (setq *parallel-search-pids* nil)
  (let ((all-jobs (if test
                      (subseq $* 0 test)
                      (reverse (lremove (lremove $* ".*mps\.c$") ".*gc_interface\.cc$")))))
    (format t "all-jobs: ~a~%" all-jobs)
    (let ((split-jobs (split-list all-jobs *max-parallel-searches*)))
      (dotimes (proc *max-parallel-searches*)
        (core:system "sleep 1")
        (let* ((job-list (elt split-jobs proc))
               (pid (core:fork)));; 0 )) ;;(core:fork))
          (if (eql 0 pid)
              (with-open-file (*standard-output* (project-pathname (format nil "project~a" proc) "log")
                                                 :direction :output :if-exists :supersede)
                (format t "Running search on: ~a~%" 
                        (setf (multitool-results *tools*) (make-project))
                        (batch-run-multitool *tools* :filenames job-list)
                        (serialize:save-archive (multitool-results *tools*) (project-pathname (format nil "project~a" proc) "dat"))
                        (core:exit)))
              (push pid *parallel-search-pids*)
              )))))
  (format t "Started parallel-search-all   pids: ~a~%" *parallel-search-pids*)
  )




(defun fork-jobs (proc job-list)
  (setf (multitool-results *tools*) (make-project))
  (batch-run-multitool *tools* :filenames job-list)
  (serialize:save-archive (multitool-results *tools*) (project-pathname (format nil "project~a" proc) "dat"))
  (core:exit))

(defvar *parallel-search-pids* nil)
(defun sequential-fork-search-all (&key test)
  "Run *max-parallel-searches* processes at a time - whenever one finishes, start the next"
  (setq *parallel-search-pids* nil)
  (let ((all-jobs (if test
                      (subseq $* 0 test)
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
    ))


(defun merge-projects (union one)
  (maphash (lambda (k v) (setf (gethash k (project-classes union)) v)) (project-classes one))
  (maphash (lambda (k v) (setf (gethash k (project-lispkinds union)) v)) (project-lispkinds one))
  (maphash (lambda (k v) (setf (gethash k (project-classkinds union)) v)) (project-classkinds one))
  (maphash (lambda (k v) (setf (gethash k (project-containerkinds union)) v)) (project-containerkinds one))
)


(defun parallel-merge (&optional (num-jobs *max-parallel-searches*))
  "Merge the analyses generated from the parallel search"
  (let ((all-jobs (serialize:load-archive (project-pathname "project-all" "dat")))
        (merged (make-project)))
    (dotimes (proc (length all-jobs))
      (format t "Marking memory with ~a~%" (1+ proc))
      (gctools:gc-marker (1+ proc))
      (format t "Loading project for job ~a~%" proc)
      (let* ((project-dat-name (truename (project-pathname (format nil "project~a" proc) "dat")))
             (one (if project-dat-name
                      (serialize:load-archive (project-pathname (format nil "project~a" proc) "dat"))
                      nil)))
        (if one
            (progn
              (format t "     merging...~%")
              (merge-projects merged one))
            (format t "File not found.~%"))))
    (setq *project* merged)
    (save-project)
    merged))





(progn
  (lnew $test-search)
  (setq $test-search (append
                      (lsel $* ".*/lambdaListHandler\.cc")
                      ))
  )






(defun load-test-ast ()
  (lnew $test-search)
  (setq $test-search (lsel $* ".*/lambdaListHandler\.cc"))
  (load-asts $test-search
             :arguments-adjuster-code (lambda (args) (concatenate 'vector #-quiet args #+quiet(remove "-v" args)
                                                                  #("-DUSE_MPS"
                                                                    "-DRUNNING_GC_BUILDER"
                                                                    "-resource-dir"
                                                                    "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr//lib/clang/5.1")))))


(defun run-test ()
  (defparameter *test-matcher*
    '(:record-decl
      (:is-definition)
      (:is-template-instantiation)
      (:has-name "ClassAllocator")))
  (match-run *test-matcher*
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

