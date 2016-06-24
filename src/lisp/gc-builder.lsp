


;;(push :use-breaks *features*)
;;(push :gc-warnings *features*)


(progn
  (provide 'gc-builder)
  (load "sys:clang-tool.lsp") ; (require 'clang-tool)
  (use-package :ast-tooling)
  )

;;(defmacro gclog (fmt &rest args) `(format t ,fmt ,@args))
(defmacro gclog (fmt &rest args) nil)


(defparameter *track-reachability* nil
  "Set this to T when you want contains-smart-pointers-p and
on-stack-has-smart-pointers-on-heap-p to keep track of the reachability
of housekeeping-classes ")

(defun safe-get-name-decl (decl)
  (if (cast:get-identifier decl)
      (cast:get-name decl)
      "NO-NAME-SAFE"))


(defun decl-name (decl)
  (cast:get-qualified-name-as-string decl)
;;  (cast:get-name-as-string decl)
  )


(defstruct variable
  location
  name
  ctype)

(defstruct (global-variable (:include variable)))
(defstruct (static-local-variable (:include variable)))
(defstruct (local-variable (:include variable)))

(defstruct new-gcobject-expr
  ctype
  num-placement-args
  location)

(defstruct new-housekeeping-class-expr
  ctype
  num-placement-args
  location)
  

(defstruct gckind
  key
  name
  location
  )

(defstruct (gcclass (:include gckind))
  bases
  vbases
  has-destructor
  metadata
)

(defstruct (gcobject-subclass (:include gcclass))
  "Represent an inheritance relationship between classes for C++/Lisp classes that inherit from GCObject"
  instance-variables
)


(defstruct (gccontainer (:include gckind))
  "Represent an inheritance relationship between GCContainer classes for C++/Lisp classes that inherit from GCObject"
  arguments
  decl-type
)

(defstruct (gcframe0 (:include gccontainer)))
(defstruct (gcarray0 (:include gccontainer)))
(defstruct (gcvec0 (:include gccontainer)))
  





(defun gcobject-subclass-wrapper-p (x)
  (search "clbind::Wrapper" (gcobject-subclass-name x)))



;; Use closed over environment to keep track of counters for kind-id
(let ((builtin-id-counter (1- (gctools:max-bootstrap-kinds)))
      (wrapper-id-counter 0)
      (iterator-id-counter 0))
  (defun gcobject-kind-id-reset ()
    (setq builtin-id-counter (1- (gctools:max-bootstrap-kinds))
          wrapper-id-counter 0
          iterator-id-counter 0))
  (defun gcobject-kind-id-builtins ()
    (incf builtin-id-counter))
  (defun gcobject-kind-id (x)
    (cond
      ((gctools:bootstrap-kind-p (gcobject-subclass-name x)) (values :builtin (gctools:bootstrap-kind-p (gcobject-subclass-name x))))
      ((search "clbind::Wrapper" (gcobject-subclass-name x))  (values :wrapper (+ #16r010000 (incf wrapper-id-counter))))
      ((search "clbind::Iterator" (gcobject-subclass-name x)) (values :iterator (+ #16r020000 (incf iterator-id-counter))))
      (t (values :builtin (incf builtin-id-counter)))))
  (defun gcobject-kind-id-status ()
    (format t "builtin-id-counter ~a~%" builtin-id-counter)
    (format t "wrapper-id-counter ~a~%" wrapper-id-counter)
    (format t "iterator-id-counter ~a~%" iterator-id-counter))
  )





;; ------------------------------------------------------------
;; ------------------------------------------------------------
;; ------------------------------------------------------------
;; ------------------------------------------------------------
;;
;; Keep track of kind enum
;;
;;
  
;; Use closed over environment to keep track of counters for kind-id
(let ((gcvector-kind-counter 0)
      (gcdummycontainer-kind-counter 0) ;; Add other containers here
      )
  (defun gccontainer-kind-reset ()
    (setq gcvector-kind-counter 0
          gcdummycontainer-kind-counter 0))
  (defun gccontainer-kind (x)
    (cond
      #|((search "GCVector" (gccontainer-name x)) (values :gcvector (gctools:bootstrap-kind-p (gccontainer-name x))))|#
      #| add more kinds here |#
      (t (values :gcvector (incf gcvector-kind-counter)))))
  (defun gccontainer-kind-status ()
    (format t "gcvector-kind-counter ~a~%" gcvector-kind-counter))
  )






(defstruct project
  "Store the results of an project"
  (gcobjects (make-hash-table :test #'equal))
  (gccontainers (make-hash-table :test #'equal))
  (housekeeping-classes (make-hash-table :test #'equal))
  (local-variables (make-hash-table :test #'equal))
  (global-variables (make-hash-table :test #'equal))
  (static-local-variables (make-hash-table :test #'equal))
  (new-gcobject-exprs (make-hash-table :test #'equal))
  (new-housekeeping-class-exprs (make-hash-table :test #'equal))
  )





    
(defun analysis-housekeeping-class-contains-smart-pointers-p (aclass analysis)
  (let ((key (housekeeping-class-key aclass)))
    (gethash key (analysis-housekeeping-class-contains-smart-pointers analysis))))

(defun (setf analysis-housekeeping-class-contains-smart-pointers-p) (new-value aclass analysis)
  (let ((key (housekeeping-class-key aclass)))
    (setf (gethash key (analysis-housekeeping-class-contains-smart-pointers analysis)) new-value )))


(defun analysis-housekeeping-class-on-stack-has-smart-pointers-on-heap-p (aclass analysis)
  (let ((key (housekeeping-class-key aclass)))
    (gethash key (analysis-housekeeping-class-on-stack-has-smart-pointers-on-heap analysis))))

(defun (setf analysis-housekeeping-class-on-stack-has-smart-pointers-on-heap-p) (new-value aclass analysis)
  (let ((key (housekeeping-class-key aclass)))
    (setf (gethash key (analysis-housekeeping-class-on-stack-has-smart-pointers-on-heap analysis)) new-value )))


#|
(defun organize-gcobjects (analysis)
  "Classify the GCObject families as :builtin, :wrapper, and :iterator.
Put them in three different slots of the organized-gcobjects object depending on their family"
  (gcobject-kind-id-reset)
  (with-accessors ((builtins organized-gcobjects-builtins)
                   (wrappers organized-gcobjects-wrappers)
                   (iterators organized-gcobjects-iterators))
      (analysis-organized-gcobjects analysis)
    (setq builtins nil
          wrappers nil
          iterators nil)
    (maphash (lambda (k v)
               (unless (or (string= (gcobject-subclass-key v) "clbind::Wrapper")
                           (string= (gcobject-subclass-key v) "clbind::Iterator"))
                 (multiple-value-bind (family kind-id)
                     (gcobject-kind-id v)
                   (setf (gcobject-subclass-family v) family)
                   (setf (gcobject-subclass-kind-id v) kind-id)
                   (case family
                     (:builtin (push v builtins))
                     (:wrapper (push v wrappers))
                     (:iterator (push v iterators))
                     (otherwise (error "Add support for gcobject family: ~a" family))))))
             (project-gcobjects (analysis-project analysis)))
    (setf builtins (nreverse builtins)
          wrappers (nreverse wrappers)
          iterators (nreverse iterators))
    ))
|#                                   
  





(defun merge-projects (union one)
  (maphash (lambda (k v) (setf (gethash k (project-gcobjects union)) v)) (project-gcobjects one))
  (maphash (lambda (k v) (setf (gethash k (project-gccontainers union)) v)) (project-gccontainers one))
  (maphash (lambda (k v) (setf (gethash k (project-housekeeping-classes union)) v)) (project-housekeeping-classes one))
  (maphash (lambda (k v) (setf (gethash k (project-local-variables union)) v)) (project-local-variables one))
  (maphash (lambda (k v) (setf (gethash k (project-global-variables union)) v)) (project-global-variables one))
  (maphash (lambda (k v) (setf (gethash k (project-static-local-variables union)) v)) (project-static-local-variables one))
  (maphash (lambda (k v) (setf (gethash k (project-new-gcobject-exprs union)) v)) (project-new-gcobject-exprs one))
  (maphash (lambda (k v) (setf (gethash k (project-new-housekeeping-class-exprs union)) v)) (project-new-housekeeping-class-exprs one))
)




(defun after-first-space (str)
  (let ((p (position #\space str)))
    (subseq str (1+ p) (length str))))


(defmacro with-all-housekeeping-classes ((all-classes) &body body)
  `(let ((*all-housekeeping-classes* ,all-classes))
     ,@body))



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

;; Build a string key that represents the node
(defgeneric record-key (node))

(defmethod record-key ((node cast:type))
  (cast:make-qual-type node)) ;; get-as-string (cast:desugar node)))

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


(defstruct debug-info
  name
  location
  )







    
  


(defstruct instance-variable
  "Represent an instance variable, it's name, it's source-location and it's classified type"
  field-name
  location
  ctype
  )



;;;
;;;   
(defstruct ctype)

(defstruct cloned-ctype
  description)


(defmacro make-defstruct.cloned-ctype (name-prefixes)
  (let (defstructs)
    (dolist (name-prefix name-prefixes)
      (push `(defstruct (,(intern (format nil "~a-CTYPE" (string name-prefix))) (:include cloned-ctype)))
            defstructs))
    `(progn ,@defstructs)))


(make-defstruct.cloned-ctype (#:dependent-sized-array
                              #:constant-array
                              #:incomplete-array
                              #:rvalue-reference
                              #:lvalue-reference
                              #:subst-template-type-parm
                              #:template-type-parm
                              #:builtin
                              #:enum
                              #:member-pointer
                              #:function-proto
                              #:template-type-parm
                              #:dependent-name
                              #:auto
                              #:vector
                              #:dependent-template-specialization
                              ))




(defstruct (template-type-parm-ctype (:include ctype))
  description)


(defstruct (typedef-ctype (:include ctype))
  desugared)



(defstruct (pointer-ctype (:include ctype))
  pointee)

(defstruct (paren-ctype (:include ctype))
  inner)

(defstruct (elaborated-ctype (:include ctype))
  named-type)

(defstruct (simple-ctype (:include cloned-ctype)))
(defstruct (unclassified-ctype (:include simple-ctype)))

(defstruct (template-specialization-ctype (:include simple-ctype)))

(defstruct (unclassified-template-specialization-ctype (:include template-specialization-ctype))
  template-name)

(defstruct (uninteresting-ctype (:include simple-ctype)))

(defstruct (maybe-interesting-ctype (:include simple-ctype))
  )


(defstruct (rest-argument (:include simple-ctype)))




(defstruct (record-ctype (:include ctype))
  key)

(defstruct (cxxrecord-ctype (:include record-ctype)))


(defstruct (class-template-specialization-ctype (:include record-ctype))
  arguments)


(defstruct (container (:include class-template-specialization-ctype)))

(defmethod container-argument ((x gccontainer) idx)
  (dolist (arg (gccontainer-arguments x))
    (when (eql (gc-template-argument-index arg) idx) (return arg))))

(defmethod container-argument ((x container) idx)
  (dolist (arg (container-arguments x))
    (when (eql (gc-template-argument-index arg) idx) (return arg))))


(defstruct (smart-ptr-ctype (:include record-ctype))
  specializer
  )


(defstruct (weak-smart-ptr-ctype (:include smart-ptr-ctype))
  )

(defstruct (injected-class-name-ctype (:include record-ctype)))

(defstruct (from-object-ctype (:include record-ctype)))

(defstruct (lisp-object-allocator-functor-ctype (:include record-ctype)))

(defstruct (static-scope (:include record-ctype)))



(defstruct (pointer-to-record-ctype (:include ctype))
  description)


(defstruct (stl-container (:include container)))

(defstruct (stl-pair (:include stl-container)))
(defstruct (stl-list (:include stl-container)))
(defstruct (stl-vector (:include stl-container)))
(defstruct (stl-set (:include stl-container)))
(defstruct (stl-map (:include stl-container)))
(defstruct (stl-queue (:include stl-container)))
(defstruct (stl-stack (:include stl-container)))
(defstruct (stl-multimap (:include stl-container)))

(defstruct (gcholder (:include container))
  name)

(defstruct (rooted-gcholder (:include container))
  name)

(defstruct (gcobject-derived (:include ctype))
  description)

(defstruct gc-template-argument 
  index
  ctype)


(defstruct scanner-method
  has-body
  is-this-declaration-a-definition
  does-this-declaration-have-a-body
  location)

(defstruct housekeeping-class
  "Store info on a housekeeping class and a flag for whether it directly or transitively 
contains smart-pointers"
  key
  template-specializer
  location
  bases
  fields
  has-on-stack-scan-gc-roots
  has-on-heap-scan-gc-roots
  )





(defun build-ignore-table (list)
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (key list) (setf (gethash key ht) t))
    ht))

  





(defmethod is-smart-p ((x ctype))
  "Anything but the cases below is not a smart-ptr"
  nil)

(defmethod is-smart-p ((x elaborated-ctype))
  (is-smart-p (elaborated-ctype-named-type x)))

(defmethod is-smart-p ((x typedef-ctype))
  (is-smart-p (typedef-ctype-desugared x)))


(defmethod is-smart-p ((x smart-ptr-ctype)) t)
(defmethod is-smart-p ((x cloned-ctype)) nil)

(defmethod is-smart-p ((x gc-template-argument))
  (is-smart-p (gc-template-argument-ctype x)))



  

(defparameter *contains-smart-pointers-impl-ht* nil)
(defun contains-smart-pointers-p (x analysis)
  (let ((*contains-smart-pointers-impl-ht* (make-hash-table :test #'eq)))
    (contains-smart-pointers-impl-p x analysis)))

(defgeneric contains-smart-pointers-impl-p (x analysis))

(defmethod contains-smart-pointers-impl-p :around (x analysis)
  (multiple-value-bind (precalc precalc-p)
      (gethash x *contains-smart-pointers-impl-ht*)
    (if precalc-p
        precalc
        (progn
          (setf (gethash x *contains-smart-pointers-impl-ht*) nil)
          (let ((res (call-next-method x analysis)))
            (setf (gethash x *contains-smart-pointers-impl-ht*) res)
            res)))))

(defmethod contains-smart-pointers-impl-p ((x t) analysis)
  (when (not x)
    #+use-breaks(break "contains-smart-pointers-impl-p on nil"))
  (when (eq x t)
    #+use-breaks(break "contains-smart-pointers-impl-p on t"))
  (when (eq x :maybe)
    #+use-breaks(break "contains-smart-pointers-impl-p on :maybe"))
  (warn "Add support for contains-smart-pointers-impl-p for ~a" x)
  #+use-breaks(break "Check x")
  t)


(defmethod contains-smart-pointers-impl-p ((x gccontainer) anal)
  "This should cover every gccontainer"
  (let (sp)
    (dotimes (i (length (gccontainer-arguments x)))
      (let ((one-res (contains-smart-pointers-impl-p (elt (gccontainer-arguments x) i) anal)))
        (setq sp (or sp one-res))))
    sp))
  
(defmethod contains-smart-pointers-impl-p ((x unclassified-ctype) anal)
  nil)



(defmethod contains-smart-pointers-impl-p ((x global-variable) analysis)
  (contains-smart-pointers-impl-p (global-variable-ctype x) analysis))

(defmethod contains-smart-pointers-impl-p ((x instance-variable) analysis)
  (contains-smart-pointers-impl-p (instance-variable-ctype x) analysis))


(defmethod contains-smart-pointers-impl-p ((x function-proto-ctype) analysis)
  nil)

(defmethod contains-smart-pointers-impl-p ((x dependent-template-specialization-ctype) analysis)
  nil)

(defmethod contains-smart-pointers-impl-p ((x rvalue-reference-ctype) analysis)
  nil)


(defmethod contains-smart-pointers-impl-p ((x typedef-ctype) analysis)
  (contains-smart-pointers-impl-p (typedef-ctype-desugared x) analysis))

(defmethod contains-smart-pointers-impl-p ((x elaborated-ctype) analysis)
  (contains-smart-pointers-impl-p (elaborated-ctype-named-type x) analysis))

(defmethod contains-smart-pointers-impl-p ((x dependent-name-ctype) analysis) nil)
(defmethod contains-smart-pointers-impl-p ((x constant-array-ctype) analysis) nil)
(defmethod contains-smart-pointers-impl-p ((x incomplete-array-ctype) analysis ) nil)
(defmethod contains-smart-pointers-impl-p ((x template-type-parm-ctype) anal) nil)
(defmethod contains-smart-pointers-impl-p ((x subst-template-type-parm-ctype) anal) nil)
(defmethod contains-smart-pointers-impl-p ((x lvalue-reference-ctype) anal) nil)
(defmethod contains-smart-pointers-impl-p ((x enum-ctype) anal) nil)
(defmethod contains-smart-pointers-impl-p ((x member-pointer-ctype) anal) nil)
(defmethod contains-smart-pointers-impl-p ((x dependent-sized-array-ctype) anal) nil)

(defmethod contains-smart-pointers-impl-p ((x smart-ptr-ctype) anal) t)
(defmethod contains-smart-pointers-impl-p ((x weak-smart-ptr-ctype) anal) t)
(defmethod contains-smart-pointers-impl-p ((x uninteresting-ctype) anal) nil)
(defmethod contains-smart-pointers-impl-p ((x paren-ctype) anal) nil)
(defmethod contains-smart-pointers-impl-p ((x housekeeping-class) anal)
  (let* (result
         (project (analysis-project anal))
         (all-classes (project-housekeeping-classes project)))
    (loop :for base-name :in (housekeeping-class-bases x)
       :do (when base-name
             (let ((base (gethash base-name all-classes)))
               (when base 
                 (let ((one-result (contains-smart-pointers-impl-p base anal)))
                   (setq result (or result one-result)))))))
    (loop :for field :in (housekeeping-class-fields x)
       :do (when field
             (let* ((field-ctype (instance-variable-ctype field))
                    (one-result (contains-smart-pointers-impl-p field-ctype anal)))
               (setq result (or result one-result)))))
    (when (and *track-reachability* result)
      (setf (gethash (housekeeping-class-key x) (analysis-housekeeping-class-stored-on-heap anal)) x))
    result))


(defmethod contains-smart-pointers-impl-p ((x stl-container) anal)
  "This should cover every stl-xxxx container"
  (let (sp)
    (dotimes (i (length (stl-container-arguments x)))
      (let ((one-res (contains-smart-pointers-impl-p (elt (stl-container-arguments x) i) anal)))
        (setq sp (or sp one-res))))
    sp))


(defconstant +vector-ctype-ignores+
  (build-ignore-table '(
"__attribute__((__vector_size__(1 * sizeof(long long)))) long long"
"__attribute__((__vector_size__(2 * sizeof(double)))) double"
"__attribute__((__vector_size__(2 * sizeof(float)))) float"
"__attribute__((__vector_size__(4 * sizeof(float)))) float"
"__attribute__((__vector_size__(4 * sizeof(int)))) int"
"__attribute__((__vector_size__(4 * sizeof(short)))) short"
"__attribute__((__vector_size__(8 * sizeof(short)))) short")))

(defun check-if-vector-ctype-ignore (key)
  (if (gethash key +vector-ctype-ignores+)
      nil
      (progn
        (warn "Check if the following vector-ctype class is contains-smart-pointers-impl-p - if not, make it an ignore~%vector-ctype-ignore ~a" key)
        #+use-breaks(break "About to return :maybe")
        :maybe)))

(defmethod contains-smart-pointers-impl-p ((x vector-ctype) anal)
  (check-if-vector-ctype-ignore (vector-ctype-description x)))

(defmethod contains-smart-pointers-impl-p ((x auto-ctype) anal)
  (check-if-auto-ctype-ignore (auto-ctype-description x)))



(defconstant +template-specialization-ignores+
  (build-ignore-table '(
                        "SmallVector<struct clang::RecursiveASTVisitor::EnqueueJob, 16>"
                        "__bit_iterator<__bitset<_N_words, _Size>, true, 0>"
                        "aligned<sizeof(Derived)>"
                        "assign_nodes<table<Types> >"
                        "bitset<_Size>"
                        "move_assign_nodes<table<Types> >"
                        "smart_ptr<Derivable<Alien> >"
                        "smart_ptr<Iterator<IT, Policy> >"
                        "smart_ptr<Wrapper<OT, HolderType> >"
                        "value_to_type<is_numeric<BlockInputIterator>::value>"
                        )))


(defun check-if-template-specialization-ignore (key)
  (unless (or (search "type-parameter-" key)  ;; All ignorable template-specializations contain type-parameter- template parameters
              (gethash key +template-specialization-ignores+))
      (warn "Check if the following template-specialization class is contains-smart-pointers-impl-p - if not, make it an ignore~%T-S-IGNORE ~a" key)
      :maybe
      ))

(defmethod contains-smart-pointers-impl-p ((x unclassified-template-specialization-ctype) anal)
  (let ((desc (unclassified-template-specialization-ctype-description x)))
    (check-if-template-specialization-ignore desc)))



(defmethod contains-smart-pointers-impl-p ((x gc-template-argument) anal) (contains-smart-pointers-impl-p (gc-template-argument-ctype x) anal))
(defmethod contains-smart-pointers-impl-p ((x builtin-ctype) anal) nil)
(defmethod contains-smart-pointers-impl-p ((x gcholder) anal) t)
(defmethod contains-smart-pointers-impl-p ((x rooted-gcholder) anal) t)
(defmethod contains-smart-pointers-impl-p ((x pointer-ctype) anal) (contains-smart-pointers-impl-p (pointer-ctype-pointee x) anal))
    


(defparameter *all-housekeeping-classes* nil)


(defconstant +housekeeping-class-ignores+
  (build-ignore-table '("<anonymous>"
                        "mps_message_s"
                        "clang::CodeCompletionHandler"
                        "clang::ExternalPreprocessorSource"
                        "clang::HeaderMap"
                        "clang::ScratchBuffer"
                        "llvm::StringMapEntry<class llvm::Value *>"
                        "__sFILEX"
                        "boost::shared_ptr<class boost::program_options::option_description>"
                        "boost::shared_ptr<class boost::program_options::options_description>"
                        "clang::ASTMutationListener"
                        "clang::ASTReader"
                        "clang::ASTRecordLayout"
                        "clang::ASTUnit::ASTWriterData"
                        "clang::CXXABI"
                        "clang::CXXOperatorIdName"
                        "clang::FileSystemStatCache"
                        "clang::HeaderSearch"
                        "clang::LineTableInfo"
                        "clang::MacroArgs"
                        "clang::MangleNumberingContext"
                        "clang::Sema"
                        "clang::TargetInfo"
                        "clang::diag::CustomDiagInfo"
                        "clang::tooling::FileMatchTrieNode"
                        "llvm::AttributeImpl"
                        "llvm::AttributeSetImpl"
                        "llvm::DenseMap<const void *,class llvm::SmallVector<class clang::ast_type_traits::DynTypedNode, 1>,struct llvm::DenseMapInfo<const void *>>"
                        "llvm::GVMaterializer"
                        "llvm::LLVMContextImpl"
                        "llvm::PointerUnion<class clang::VarTemplateDecl *,class clang::MemberSpecializationInfo *>"
                        "llvm::SmallVector<struct std::__1::pair<unsigned int, class clang::Decl *>,64>"
                        "llvm::TargetLibraryInfo"
                        "llvm::Timer"
                        "llvm::ValueMapCallbackVH<const class llvm::GlobalValue *,void *,struct llvm::ExecutionEngineState::AddressMapConfig>"
                        "llvm::ValueSymbolTable"
                        "llvm::fltSemantics"
                        "llvm::yaml::Scanner"
                        "llvm_regex"

                        "llvm::AssertingVH<const class llvm::GlobalValue>"
                        "boost::program_options::basic_option<char>"
                        "std::__1::map"
                        "boost::mpl::apply1"
                        "__double2"
                        "__float2"
                        "__gmp_alloc_cstring"
                        "__nl_cat_d"
                        "__sFILE"
                        "_opaque_pthread_t"
                        "boost::_bi::list0"
                        "boost::adjacency_list"
                        "boost::any"
                        "boost::archive::detail::basic_pointer_iserializer"
                        "boost::archive::detail::basic_pointer_oserializer"
                        "boost::archive::library_version_type"
                        "boost::detail::adj_list_edge_iterator"
                        "boost::detail::esft2_deleter_wrapper"
                        "boost::detail::function::function_buffer"
                        "boost::detail::lightweight_mutex::scoped_lock"
                        "boost::detail::sp_counted_base"
                        "boost::detail::spinlock_pool<2>::scoped_lock"
                        "boost::detail::val_out_edge_iterator"
                        "boost::dynamic_bitset"
                        "boost::escaped_list_separator::char_eq"
                        "boost::exception_detail::error_info_container"
                        "boost::filesystem::directory_entry"
                        "boost::filesystem::directory_iterator"
                        "boost::filesystem::file_status"
                        "boost::filesystem::path::iterator"
                        "boost::filesystem::recursive_directory_iterator"
                        "boost::function0"
                        "boost::gregorian::date"
                        "boost::io::ios_flags_saver"
                        "boost::iostreams::detail::chainbuf::sentry"
                        "boost::iostreams::gzip_params"
                        "boost::iostreams::zlib_params"
                        "boost::multi_index::detail::index_matcher::entry"
                        "boost::offset_separator"
                        "boost::posix_time::ptime"
                        "boost::posix_time::time_duration"
                        "boost::random::binomial_distribution::param_type"
                        "boost::random::chi_squared_distribution::param_type"
                        "boost::random::detail::div_t"
                        "boost::random::discrete_distribution::param_type"
                        "boost::random::extreme_value_distribution::param_type"
                        "boost::random::fisher_f_distribution::param_type"
                        "boost::random::gamma_distribution::param_type"
                        "boost::random::geometric_distribution::param_type"
                        "boost::random::negative_binomial_distribution::param_type"
                        "boost::random::piecewise_constant_distribution::param_type"
                        "boost::random::piecewise_linear_distribution::param_type"
                        "boost::random::poisson_distribution::param_type"
                        "boost::random::student_t_distribution::param_type"
                        "boost::random::triangle_distribution::param_type"
                        "boost::random::uniform_int_distribution::param_type"
                        "boost::random::uniform_real_distribution::param_type"
                        "boost::random::weibull_distribution::param_type"
                        "boost::re_detail::named_subexpressions::name"
                        "boost::re_detail::re_alt"
                        "boost::re_detail::re_brace"
                        "boost::re_detail::re_jump"
                        "boost::re_detail::re_literal"
                        "boost::re_detail::re_recurse"
                        "boost::re_detail::re_repeat"
                        "boost::re_detail::re_set"
                        "boost::re_detail::re_syntax_base"
                        "boost::re_detail::repeater_count"
                        "boost::re_detail::save_state_init"
                        "boost::re_detail::saved_extra_block"
                        "boost::re_detail::saved_state"
                        "boost::regex_iterator"
                        "boost::regex_token_iterator"
                        "boost::scoped_static_mutex_lock"
                        "boost::serialization::collection_size_type"
                        "boost::serialization::extended_type_info"
                        "boost::serialization::item_version_type"
                        "boost::serialization::version_type"
                        "boost::unordered::iterator_detail::c_iterator"
                        "boost::unordered::iterator_detail::cl_iterator"
                        "boost::unordered::iterator_detail::iterator"
                        "boost::unordered::iterator_detail::l_iterator"
                        "boost_132::detail::sp_counted_base"
                        "clang::ASTContext"
                        "clang::ASTContext::import_iterator"
                        "clang::ASTTemplateArgumentListInfo"
                        "clang::ASTUnit"
                        "clang::ArrayType"
                        "clang::AutoType"
                        "clang::BinaryOperator"
                        "clang::BuiltinType"
                        "clang::CXXBaseSpecifier"
                        "clang::CXXConstructorDecl"
                        "clang::CXXCtorInitializer"
                        "clang::CXXMemberCallExpr"
                        "clang::CXXMethodDecl"
                        "clang::CXXRecordDecl"
                        "clang::CXXRecordDecl::friend_iterator"
                        "clang::CallExpr"
                        "clang::CanTypeIterator"
                        "clang::CaseStmt"
                        "clang::CharSourceRange"
                        "clang::ClassTemplatePartialSpecializationDecl"
                        "clang::ClassTemplateSpecializationDecl"
                        "clang::ClassTemplateSpecializationDecl::SpecializedPartialSpecialization"
                        "clang::ConditionalOperator"
                        "clang::ConstStmtRange"
                        "clang::Decl"
                        "clang::Decl::redecl_iterator"
                        "clang::DeclAccessPair"
                        "clang::DeclContext"
                        "clang::DeclContext::decl_iterator"
                        "clang::DeclContext::filtered_decl_iterator"
                        "clang::DeclContext::specific_decl_iterator"
                        "clang::DeclGroupRef"
                        "clang::DeclStmt"
                        "clang::DeclarationName"
                        "clang::DeclarationNameInfo"
                        "clang::DeclaratorDecl"
                        "clang::DefMacroDirective"
                        "clang::DependentTemplateName"
                        "clang::DiagnosticBuilder"
                        "clang::DiagnosticMappingInfo"
                        "clang::EnumDecl"
                        "clang::EnumType"
                        "clang::EvaluatedStmt"
                        "clang::Expr"
                        "clang::ExtQuals"
                        "clang::ExternalASTSource::MemoryBufferSizes"
                        "clang::FieldDecl"
                        "clang::FileEntry"
                        "clang::FileID"
                        "clang::FixItHint"
                        "clang::FullSourceLoc"
                        "clang::FunctionDecl"
                        "clang::FunctionProtoType"
                        "clang::FunctionProtoType::ExtProtoInfo"
                        "clang::FunctionProtoTypeLoc"
                        "clang::FunctionTemplateDecl"
                        "clang::FunctionTemplateSpecializationInfo"
                        "clang::FunctionType"
                        "clang::IdentifierInfo"
                        "clang::ImplicitCastExpr"
                        "clang::InitListExpr"
                        "clang::LambdaExpr::Capture"
                        "clang::LazyVector::iterator"
                        "clang::MacroDefinition"
                        "clang::MacroDirective"
                        "clang::MacroDirective::DefInfo"
                        "clang::MemberPointerType"
                        "clang::Module"
                        "clang::NamedDecl"
                        "clang::NamespaceAliasDecl"
                        "clang::NestedNameSpecifier"
                        "clang::NestedNameSpecifierLoc"
                        "clang::ObjCDictionaryElement"
                        "clang::ObjCInterfaceDecl"
                        "clang::ObjCInterfaceDecl::filtered_category_iterator"
                        "clang::ObjCInterfaceType"
                        "clang::ObjCMethodDecl"
                        "clang::ObjCObjectPointerType"
                        "clang::ObjCPropertyDecl"
                        "clang::OpaqueValueExpr"
                        "clang::OverloadExpr"
                        "clang::OverloadExpr::FindResult"
                        "clang::PPConditionalInfo"
                        "clang::ParmVarDecl"
                        "clang::ParsedSourceLocation"
                        "clang::PartialDiagnostic::Storage"
                        "clang::PointerType"
                        "clang::PreprocessingRecord::PPEntityID"
                        "clang::PreprocessingRecord::iterator"
                        "clang::QualType"
                        "clang::QualifiedTemplateName"
                        "clang::QualifiedTypeLoc"
                        "clang::QualifierCollector"
                        "clang::Qualifiers"
                        "clang::RecordDecl"
                        "clang::RecordType"
                        "clang::Redeclarable::redecl_iterator"
                        "clang::RedeclarableTemplateDecl::SpecIterator"
                        "clang::ReferenceType"
                        "clang::RopePieceBTreeIterator"
                        "clang::Selector"
                        "clang::SourceLocation"
                        "clang::SourceManager"
                        "clang::SourceRange"
                        "clang::SplitQualType"
                        "clang::SrcMgr::ContentCache"
                        "clang::SrcMgr::ExpansionInfo"
                        "clang::SrcMgr::FileInfo"
                        "clang::SrcMgr::SLocEntry"
                        "clang::Stmt"
                        "clang::StmtRange"
                        "clang::SubstTemplateTemplateParmStorage"
                        "clang::SwitchCase"
                        "clang::TagDecl"
                        "clang::TemplateArgument"
                        "clang::TemplateParameterList"
                        "clang::TemplateSpecializationType"
                        "clang::Type"
                        "clang::TypeLoc"
                        "clang::TypeSourceInfo"
                        "clang::UnaryOperator"
                        "clang::UncommonTemplateNameStorage"
                        "clang::UsingDecl::shadow_iterator"
                        "clang::UsingShadowDecl"
                        "clang::VarDecl"
                        "clang::VarTemplatePartialSpecializationDecl"
                        "clang::VarTemplateSpecializationDecl"
                        "clang::VarTemplateSpecializationDecl::SpecializedPartialSpecialization"
                        "clang::ast_matchers::BoundNodes"
                        "clang::ast_matchers::MatchFinder"
                        "clang::ast_matchers::dynamic::VariantMatcher"
                        "clang::ast_matchers::dynamic::VariantValue"
                        "clang::ast_matchers::internal::BoundNodesTreeBuilder"
                        "clang::ast_matchers::internal::CollectMatchesCallback"
                        "clang::ast_matchers::internal::DynTypedMatcher"
                        "clang::ast_matchers::internal::NotEqualsBoundNodePredicate"
                        "clang::ast_type_traits::ASTNodeKind"
                        "clang::ast_type_traits::DynTypedNode"
                        "clang::comments::CommandInfo"
                        "clang::specific_attr_iterator"
                        "clang::tooling::ClangTool"
                        "clang::tooling::CompilationDatabase"
                        "clang::tooling::Range"
                        "clang::tooling::RefactoringTool"
                        "clang::tooling::Replacement"
                        "dconv"
                        "dl_info"
                        "fconv"
                        "hostent"
                        "linger"
                        "llvm::APFloat"
                        "llvm::APInt"
                        "llvm::APSInt"
                        "llvm::ArrayType"
                        "llvm::AssemblyAnnotationWriter"
                        "llvm::Attribute"
                        "llvm::BasicBlock"
                        "llvm::BinaryOperator"
                        "llvm::BlockAddress"
                        "llvm::CompositeType"
                        "llvm::Constant"
                        "llvm::ConstantExpr"
                        "llvm::ConstantInt"
                        "llvm::DIArray"
                        "llvm::DIBuilder"
                        "llvm::DIDescriptor"
                        "llvm::DebugLoc"
                        "llvm::DenseMapIterator"
                        "llvm::EngineBuilder"
                        "llvm::ExecutionEngine"
                        "llvm::FoldingSetBucketIterator"
                        "llvm::FoldingSetImpl::Node"
                        "llvm::FoldingSetIterator"
                        "llvm::Function"
                        "llvm::FunctionType"
                        "llvm::GenericValue"
                        "llvm::GetElementPtrInst"
                        "llvm::GlobalVariable"
                        "llvm::IRBuilderBase"
                        "llvm::IRBuilderBase::InsertPoint"
                        "llvm::Instruction"
                        "llvm::LLVMContext"
                        "llvm::Linker"
                        "llvm::LoadInst"
                        "llvm::MDNode"
                        "llvm::MDString"
                        "llvm::MallocAllocator"
                        "llvm::MemSlab"
                        "llvm::Module"
                        "llvm::NamedMDNode"
                        "llvm::PHINode"
                        "llvm::Pass"
                        "llvm::PassManagerBuilder"
                        "llvm::PointerIntPair"
                        "llvm::PointerType"
                        "llvm::PointerUnion"
                        "llvm::PointerUnion3"
                        "llvm::PointerUnion4"
                        "llvm::Regex"
                        "llvm::SMLoc"
                        "llvm::SmallDenseMap::LargeRep"
                        "llvm::SmallPtrSetIterator"
                        "llvm::SourceMgr::SrcBuffer"
                        "llvm::StoreInst"
                        "llvm::StringMapConstIterator"
                        "llvm::StringMapEntry"
                        "llvm::StringRef"
                        "llvm::StructLayout"
                        "llvm::StructType"
                        "llvm::SwitchInst::CaseIt"
                        "llvm::TargetOptions"
                        "llvm::Twine"
                        "llvm::Twine::Child"
                        "llvm::Type"
                        "llvm::Use"
                        "llvm::User::value_op_iterator"
                        "llvm::Value"
                        "llvm::ValueMapCallbackVH"
                        "llvm::ValueMapConstIterator"
                        "llvm::ValueMapConstIterator::ValueTypeProxy"
                        "llvm::ValueMapIterator"
                        "llvm::ValueMapIterator::ValueTypeProxy"
                        "llvm::VectorType"
                        "llvm::error_code"
                        "llvm::generic_gep_type_iterator"
                        "llvm::ilist_iterator"
                        "llvm::legacy::PassManagerBase"
                        "llvm::raw_fd_ostream"
                        "llvm::raw_string_ostream"
                        "llvm::sys::TimeValue"
                        "llvm::sys::fs::directory_iterator"
                        "llvm::sys::fs::file_status"
                        "llvmo::IRBuilder_O"
                        "mach_timespec"
                        "passwd"
                        "rusage"
                        "size_t_pair"
                        "sockaddr_in"
                        "sockaddr_un"
                        "stat"
                        "std::__1::__bit_iterator"
                        "std::__1::__deque_iterator"
                        "std::__1::__destruct_n"
                        "std::__1::__hash_const_iterator"
                        "std::__1::__hash_const_local_iterator"
                        "std::__1::__hash_iterator"
                        "std::__1::__hash_local_iterator"
                        "std::__1::__list_const_iterator"
                        "std::__1::__list_iterator"
                        "std::__1::__map_const_iterator"
                        "std::__1::__map_iterator"
                        "std::__1::__rs_default"
                        "std::__1::__split_buffer"
                        "std::__1::__tree_const_iterator"
                        "std::__1::__tree_iterator"
                        "std::__1::__wrap_iter"
                        "std::__1::basic_istream::sentry"
                        "std::__1::basic_istream<char, std::__1::char_traits<char> >::sentry"
                        "std::__1::basic_ostream::sentry"
                        "std::__1::basic_ostream<char, std::__1::char_traits<char> >::sentry"
                        "std::__1::bitset"
                        "std::__1::fpos"
                        "std::__1::istream_iterator"
                        "std::__1::locale"
                        "std::__1::money_base::pattern"
                        "std::__1::move_iterator"
                        "std::__1::raw_storage_iterator"
                        "std::__1::reverse_iterator"
                        "std::__1::vector"
                        "std::__1::wbuffer_convert"
                        "std::invalid_argument"
                        "std::logic_error"
                        "std::nested_exception"
                        "std::out_of_range"
                        "std::overflow_error"
                        "std::runtime_error"
                        "timespec"
                        "timeval"
                        "tm"
                        "boost::regex_error"
                        "<anonymous namespace>::ForceCodegenLinking"
                        "<anonymous namespace>::ForceInterpreterLinking"
                        "<anonymous namespace>::ForceJITLinking"
                        "<anonymous namespace>::ForceMCJITLinking"
                        "boost::container::ordered_range_t"
                        "boost::container::ordered_unique_range_t"
                        "boost::filesystem::path"
                        "boost::math::constants::detail::constant_initializer2::initializer"
                        "boost::math::constants::detail::constant_initializer::initializer"
                        "boost::math::detail::bessel_i0_initializer::init"
                        "boost::math::detail::bessel_i1_initializer::init"
                        "boost::math::detail::bessel_j0_initializer::init"
                        "boost::math::detail::bessel_j1_initializer::init"
                        "boost::math::detail::bessel_k0_initializer::init"
                        "boost::math::detail::bessel_k1_initializer::init"
                        "boost::math::detail::bessel_y0_initializer::init"
                        "boost::math::detail::bessel_y1_initializer::init"
                        "boost::math::detail::digamma_initializer::init"
                        "boost::math::detail::erf_initializer::init"
                        "boost::math::detail::erf_inv_initializer::init"
                        "boost::math::detail::expint_1_initializer::init"
                        "boost::math::detail::expint_i_initializer::init"
                        "boost::math::detail::expm1_initializer::init"
                        "boost::math::detail::igamma_initializer::init"
                        "boost::math::detail::lgamma_initializer::init"
                        "boost::math::detail::log1p_initializer::init"
                        "boost::math::detail::min_shift_initializer::init"
                        "boost::math::detail::owens_t_initializer::init"
                        "boost::math::detail::zeta_initializer::init"
                        "boost::math::lanczos::lanczos_initializer::init"
                        "boost::math::lanczos::lanczos_initializer<boost::math::lanczos::lanczos17m64, long double>::init"
                        "boost::no_property"
                        "boost::object_cache::data"
                        "boost::parameter::void_"
                        "boost::static_mutex"
                        "boost::unordered::piecewise_construct_t"
                        "clang::ast_matchers::internal::TypeTraversePolymorphicMatcher<clang::QualType, internal::TypeMatcherhasDeducedTypeGetter, internal::TypeTraverseMatcher, void (clang::ast_matchers::internal::TypeList<clang::AutoType, void, void, void>)>::Func"
                        "clang::ast_matchers::internal::TypeTraversePolymorphicMatcher<clang::QualType, internal::TypeMatcherhasElementTypeGetter, internal::TypeTraverseMatcher, void (clang::ast_matchers::internal::TypeList<clang::ArrayType, clang::ComplexType, void, void>)>::Func"
                        "clang::ast_matchers::internal::TypeTraversePolymorphicMatcher<clang::QualType, internal::TypeMatcherhasValueTypeGetter, internal::TypeTraverseMatcher, void (clang::ast_matchers::internal::TypeList<clang::AtomicType, void, void, void>)>::Func"
                        "clang::ast_matchers::internal::TypeTraversePolymorphicMatcher<clang::QualType, internal::TypeMatcherinnerTypeGetter, internal::TypeTraverseMatcher, void (clang::ast_matchers::internal::TypeList<clang::ParenType, void, void, void>)>::Func"
                        "clang::ast_matchers::internal::TypeTraversePolymorphicMatcher<clang::QualType, internal::TypeMatcherpointeeGetter, internal::TypeTraverseMatcher, void (clang::ast_matchers::internal::TypeList<clang::BlockPointerType, clang::MemberPointerType, clang::PointerType, clang::ReferenceType>)>::Func"
                        "clang::ast_matchers::internal::TypeTraversePolymorphicMatcher<clang::TypeLoc, internal::TypeLocMatcherhasElementTypeGetter, internal::TypeLocTraverseMatcher, void (clang::ast_matchers::internal::TypeList<clang::ArrayType, clang::ComplexType, void, void>)>::Func"
                        "clang::ast_matchers::internal::TypeTraversePolymorphicMatcher<clang::TypeLoc, internal::TypeLocMatcherhasValueTypeGetter, internal::TypeLocTraverseMatcher, void (clang::ast_matchers::internal::TypeList<clang::AtomicType, void, void, void>)>::Func"
                        "clang::ast_matchers::internal::TypeTraversePolymorphicMatcher<clang::TypeLoc, internal::TypeLocMatcherpointeeGetter, internal::TypeLocTraverseMatcher, void (clang::ast_matchers::internal::TypeList<clang::BlockPointerType, clang::MemberPointerType, clang::PointerType, clang::ReferenceType>)>::Func"
                        "reg::type_id"
                        "std::__1::adopt_lock_t"
                        "std::__1::allocator_arg_t"
                        "std::__1::defer_lock_t"
                        "std::__1::locale::id"
                        "std::__1::piecewise_construct_t"
                        "std::__1::try_to_lock_t"
                        "std::type_info"
                        "mps_root_s"
                        "mps_thr_s"
                        "mps_ss_s"
                        "mps_pool_s"
                        "mps_fmt_s"
                        "mps_chain_s"
                        "mps_ap_s"
                        "mps_arena_s"
                        "mps_fmt_auto_header_s"                        
                        "__gmp_expr<__mpf_struct [1],__mpf_struct [1]>"
                        "__gmp_expr<__mpq_struct [1],__mpq_struct [1]>"
                        "__gmp_expr<__mpz_struct [1],__mpz_struct [1]>"
                        "boost::arg<1>"
                        "boost::arg<2>"
                        "boost::arg<3>"
                        "boost::arg<4>"
                        "boost::arg<5>"
                        "boost::arg<6>"
                        "boost::arg<7>"
                        "boost::arg<8>"
                        "boost::arg<9>"
                        "boost::array<float,35>"
                        "boost::array<long double,171>"
                        "boost::basic_format<char,struct std::__1::char_traits<char>,class std::__1::allocator<char>>"
                        "boost::basic_regex<char,struct boost::regex_traits<char, class boost::cpp_regex_traits<char> >>"
                        "boost::char_separator<char,struct std::__1::char_traits<char>>"
                        "boost::date_time::month_functor<class boost::gregorian::date>"
                        "boost::date_time::wrapping_int2<short,1,12>"
                        "boost::date_time::year_month_day_base<class boost::gregorian::greg_year,class boost::gregorian::greg_month,class boost::gregorian::greg_day>"
                        "boost::detail::make_property_map_from_arg_pack_gen<struct boost::graph::keywords::tag::color_map,enum boost::default_color_type>"
                        "boost::dynamic_bitset<unsigned long,class std::__1::allocator<unsigned long>>"
                        "boost::exception_detail::refcount_ptr<struct boost::exception_detail::error_info_container>"
                        "boost::gregorian::greg_year"
                        "boost::hash<char>"
                        "boost::iostreams::back_insert_device<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >>"
                        "boost::iostreams::detail::basic_buffer<char,class std::__1::allocator<char>>"
                        "boost::iostreams::detail::linked_streambuf<char,struct std::__1::char_traits<char>>"
                        "boost::iostreams::filtering_stream<struct boost::iostreams::input,char,struct std::__1::char_traits<char>,class std::__1::allocator<char>,struct boost::iostreams::public_>"
                        "boost::iostreams::filtering_stream<struct boost::iostreams::output,char,struct std::__1::char_traits<char>,class std::__1::allocator<char>,struct boost::iostreams::public_>"
                        "boost::iostreams::stream_buffer<class boost::iostreams::basic_null_device<char, struct boost::iostreams::input>,struct std::__1::char_traits<char>,class std::__1::allocator<char>,struct boost::iostreams::input>"
                        "boost::match_results<const char *,class std::__1::allocator<struct boost::sub_match<const char *> >>"
                        "boost::move_iterator"
                        "boost::parameter::keyword"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::attractive_force>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::buffer>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::capacity_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::centrality_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::color_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::cooling>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::diameter_range>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::discover_time_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::displacement_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::distance_combine>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::distance_compare>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::distance_inf>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::distance_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::distance_zero>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::edge_centrality_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::edge_color_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::edge_copy>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::edges_equivalent>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::force_pairs>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::in_parallel>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::index_in_heap_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::isomorphism_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::iterations>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::learning_constant_range>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::lookahead>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::lowpoint_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::max_priority_queue>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::orig_to_copy>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::parity_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::polling>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::predecessor_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::rank_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::repulsive_force>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::residual_capacity_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::reverse_edge_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::root_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::root_vertex>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::vertex_assignment_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::vertex_copy>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::vertex_index1_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::vertex_index2_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::vertex_index_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::vertex_invariant1>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::vertex_invariant2>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::vertex_invariant>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::vertex_max_invariant>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::vertices_equivalent>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::visitor>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::weight_map2>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::weight_map>"
                        "boost::program_options::basic_parsed_options<char>"
                        "boost::random::detail::generator_seed_seq<class boost::random::linear_congruential_engine<unsigned int, 16807, 0, 2147483647>>"
                        "boost::random::detail::generator_seed_seq<class boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>>"
                        "boost::random::linear_congruential_engine<unsigned int,16807,0,2147483647>"
                        "boost::random::linear_congruential_engine<unsigned int,40014,0,2147483563>"
                        "boost::random::mersenne_twister_engine<unsigned int,32,351,175,19,-861171993,11,-1,7,834054912,15,-1769472,17,1812433253>"
                        "boost::random::normal_distribution<double>"
                        "boost::random::variate_generator<class boost::random::mersenne_twister_engine<unsigned int, 32, 351, 175, 19, 3433795303, 11, 4294967295, 7, 834054912, 15, 4293197824, 17, 1812433253> &,class boost::random::normal_distribution<double>>"
                        "boost::random::variate_generator<class boost::random::mersenne_twister_engine<unsigned int, 32, 351, 175, 19, 3433795303, 11, 4294967295, 7, 834054912, 15, 4293197824, 17, 1812433253> &,class boost::uniform_real<double>>"
                        "boost::re_detail::basic_regex_implementation"
                        "boost::scoped_ptr<class clbind::detail::cast_graph::impl>"
                        "boost::shared_ptr<const void>"
                        "boost::shared_ptr<void>"
                        "boost::token_iterator<class boost::char_separator<char, struct std::__1::char_traits<char> >,class std::__1::__wrap_iter<const char *>,class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >>"
                        "boost::token_iterator<class boost::offset_separator,class std::__1::__wrap_iter<const char *>,class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >>"
                        "boost::tokenizer<class boost::char_separator<char, struct std::__1::char_traits<char> >,class std::__1::__wrap_iter<const char *>,class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >>"
                        "boost::tokenizer<class boost::offset_separator,class std::__1::__wrap_iter<const char *>,class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >>"
                        "boost::uniform_real<double>"
                        "clang::ASTConsumer"
                        "clang::AccessSpecDecl"
                        "clang::AddrLabelExpr"
                        "clang::AdjustedType"
                        "clang::ArraySubscriptExpr"
                        "clang::ArrayTypeTraitExpr"
                        "clang::ArrayType_cannot_be_used_with_getAs<class clang::FunctionNoProtoType,0>"
                        "clang::ArrayType_cannot_be_used_with_getAs<class clang::ReferenceType,0>"
                        "clang::AsTypeExpr"
                        "clang::AtomicExpr"
                        "clang::AtomicType"
                        "clang::Attr"
                        "clang::AttributedStmt"
                        "clang::AttributedType"
                        "clang::BinaryConditionalOperator"
                        "clang::BlockDecl"
                        "clang::BlockExpr"
                        "clang::BlockPointerType"
                        "clang::BreakStmt"
                        "clang::CStyleCastExpr"
                        "clang::CUDAKernelCallExpr"
                        "clang::CXXBindTemporaryExpr"
                        "clang::CXXBoolLiteralExpr"
                        "clang::CXXCatchStmt"
                        "clang::CXXConstCastExpr"
                        "clang::CXXConstructExpr"
                        "clang::CXXConversionDecl"
                        "clang::CXXDefaultArgExpr"
                        "clang::CXXDefaultInitExpr"
                        "clang::CXXDeleteExpr"
                        "clang::CXXDependentScopeMemberExpr"
                        "clang::CXXDestructorDecl"
                        "clang::CXXDynamicCastExpr"
                        "clang::CXXForRangeStmt"
                        "clang::CXXFunctionalCastExpr"
                        "clang::CXXNewExpr"
                        "clang::CXXNoexceptExpr"
                        "clang::CXXNullPtrLiteralExpr"
                        "clang::CXXOperatorCallExpr"
                        "clang::CXXPseudoDestructorExpr"
                        "clang::CXXReinterpretCastExpr"
                        "clang::CXXScalarValueInitExpr"
                        "clang::CXXStaticCastExpr"
                        "clang::CXXStdInitializerListExpr"
                        "clang::CXXTemporaryObjectExpr"
                        "clang::CXXThisExpr"
                        "clang::CXXThrowExpr"
                        "clang::CXXTryStmt"
                        "clang::CXXTypeidExpr"
                        "clang::CXXUnresolvedConstructExpr"
                        "clang::CXXUuidofExpr"
                        "clang::CanQual"
                        "clang::CanQual<class clang::ReferenceType>"
                        "clang::CanQual<class clang::Type>"
                        "clang::CapturedDecl"
                        "clang::CapturedStmt"
                        "clang::CharacterLiteral"
                        "clang::ChooseExpr"
                        "clang::ClassScopeFunctionSpecializationDecl"
                        "clang::ClassTemplateDecl"
                        "clang::CompilerInstance"
                        "clang::ComplexType"
                        "clang::CompoundAssignOperator"
                        "clang::CompoundLiteralExpr"
                        "clang::CompoundStmt"
                        "clang::ConstantArrayType"
                        "clang::ContinueStmt"
                        "clang::ConvertVectorExpr"
                        "clang::DecayedType"
                        "clang::DeclRefExpr"
                        "clang::DecltypeType"
                        "clang::DefaultStmt"
                        "clang::DependentNameType"
                        "clang::DependentScopeDeclRefExpr"
                        "clang::DependentSizedArrayType"
                        "clang::DependentSizedExtVectorType"
                        "clang::DependentTemplateSpecializationType"
                        "clang::DesignatedInitExpr"
                        "clang::DoStmt"
                        "clang::ElaboratedType"
                        "clang::EmptyDecl"
                        "clang::EnumConstantDecl"
                        "clang::ExprWithCleanups"
                        "clang::ExpressionTraitExpr"
                        "clang::ExtVectorElementExpr"
                        "clang::ExtVectorType"
                        "clang::FileScopeAsmDecl"
                        "clang::FloatingLiteral"
                        "clang::ForStmt"
                        "clang::FriendDecl"
                        "clang::FriendTemplateDecl"
                        "clang::FrontendAction"
                        "clang::FunctionNoProtoType"
                        "clang::FunctionParmPackExpr"
                        "clang::GCCAsmStmt"
                        "clang::GNUNullExpr"
                        "clang::GenericSelectionExpr"
                        "clang::GotoStmt"
                        "clang::IfStmt"
                        "clang::ImaginaryLiteral"
                        "clang::ImplicitParamDecl"
                        "clang::ImplicitValueInitExpr"
                        "clang::ImportDecl"
                        "clang::IncompleteArrayType"
                        "clang::IndirectFieldDecl"
                        "clang::IndirectGotoStmt"
                        "clang::InjectedClassNameType"
                        "clang::IntegerLiteral"
                        "clang::LValueReferenceType"
                        "clang::LabelDecl"
                        "clang::LabelStmt"
                        "clang::LambdaExpr"
                        "clang::LangOptions"
                        "clang::Lexer"
                        "clang::LinkageSpecDecl"
                        "clang::MSAsmStmt"
                        "clang::MSDependentExistsStmt"
                        "clang::MSPropertyDecl"
                        "clang::MSPropertyRefExpr"
                        "clang::MaterializeTemporaryExpr"
                        "clang::MemberExpr"
                        "clang::NamespaceDecl"
                        "clang::NonTypeTemplateParmDecl"
                        "clang::NullStmt"
                        "clang::OMPClause"
                        "clang::OMPParallelDirective"
                        "clang::OMPThreadPrivateDecl"
                        "clang::ObjCArrayLiteral"
                        "clang::ObjCAtCatchStmt"
                        "clang::ObjCAtDefsFieldDecl"
                        "clang::ObjCAtFinallyStmt"
                        "clang::ObjCAtSynchronizedStmt"
                        "clang::ObjCAtThrowStmt"
                        "clang::ObjCAtTryStmt"
                        "clang::ObjCAutoreleasePoolStmt"
                        "clang::ObjCBoolLiteralExpr"
                        "clang::ObjCBoxedExpr"
                        "clang::ObjCBridgedCastExpr"
                        "clang::ObjCCategoryDecl"
                        "clang::ObjCCategoryImplDecl"
                        "clang::ObjCCompatibleAliasDecl"
                        "clang::ObjCContainerDecl"
                        "clang::ObjCDictionaryLiteral"
                        "clang::ObjCEncodeExpr"
                        "clang::ObjCForCollectionStmt"
                        "clang::ObjCImplDecl"
                        "clang::ObjCImplementationDecl"
                        "clang::ObjCIndirectCopyRestoreExpr"
                        "clang::ObjCIsaExpr"
                        "clang::ObjCIvarDecl"
                        "clang::ObjCIvarRefExpr"
                        "clang::ObjCMessageExpr"
                        "clang::ObjCObjectType"
                        "clang::ObjCPropertyImplDecl"
                        "clang::ObjCPropertyRefExpr"
                        "clang::ObjCProtocolDecl"
                        "clang::ObjCProtocolExpr"
                        "clang::ObjCSelectorExpr"
                        "clang::ObjCStringLiteral"
                        "clang::ObjCSubscriptRefExpr"
                        "clang::OffsetOfExpr"
                        "clang::PackExpansionExpr"
                        "clang::PackExpansionType"
                        "clang::ParenExpr"
                        "clang::ParenListExpr"
                        "clang::ParenType"
                        "clang::PredefinedExpr"
                        "clang::PresumedLoc"
                        "clang::PseudoObjectExpr"
                        "clang::RValueReferenceType"
                        "clang::RedeclarableTemplateDecl"
                        "clang::RedeclarableTemplateDecl::SpecIterator<class clang::ClassTemplateSpecializationDecl,struct clang::RedeclarableTemplateDecl::SpecEntryTraits<class clang::ClassTemplateSpecializationDecl>,class clang::ClassTemplateSpecializationDecl>"
                        "clang::RedeclarableTemplateDecl::SpecIterator<class clang::FunctionTemplateSpecializationInfo,struct clang::RedeclarableTemplateDecl::SpecEntryTraits<class clang::FunctionTemplateSpecializationInfo>,class clang::FunctionDecl>"
                        "clang::RedeclarableTemplateDecl::SpecIterator<class clang::VarTemplateSpecializationDecl,struct clang::RedeclarableTemplateDecl::SpecEntryTraits<class clang::VarTemplateSpecializationDecl>,class clang::VarTemplateSpecializationDecl>"
                        "clang::ReturnStmt"
                        "clang::Rewriter"
                        "clang::SEHExceptStmt"
                        "clang::SEHFinallyStmt"
                        "clang::SEHTryStmt"
                        "clang::ShuffleVectorExpr"
                        "clang::SizeOfPackExpr"
                        "clang::StaticAssertDecl"
                        "clang::StmtExpr"
                        "clang::StringLiteral"
                        "clang::SubstNonTypeTemplateParmExpr"
                        "clang::SubstNonTypeTemplateParmPackExpr"
                        "clang::SubstTemplateTypeParmPackType"
                        "clang::SubstTemplateTypeParmType"
                        "clang::SwitchStmt"
                        "clang::TagType"
                        "clang::TemplateArgumentList"
                        "clang::TemplateDecl"
                        "clang::TemplateName"
                        "clang::TemplateTemplateParmDecl"
                        "clang::TemplateTypeParmDecl"
                        "clang::TemplateTypeParmType"
                        "clang::TranslationUnitDecl"
                        "clang::TypeAliasDecl"
                        "clang::TypeAliasTemplateDecl"
                        "clang::TypeDecl"
                        "clang::TypeOfExprType"
                        "clang::TypeOfType"
                        "clang::TypeTraitExpr"
                        "clang::TypedefDecl"
                        "clang::TypedefNameDecl"
                        "clang::TypedefType"
                        "clang::UnaryExprOrTypeTraitExpr"
                        "clang::UnaryTransformType"
                        "clang::UnresolvedLookupExpr"
                        "clang::UnresolvedMemberExpr"
                        "clang::UnresolvedUsingType"
                        "clang::UnresolvedUsingTypenameDecl"
                        "clang::UnresolvedUsingValueDecl"
                        "clang::UserDefinedLiteral"
                        "clang::UsingDecl"
                        "clang::UsingDirectiveDecl"
                        "clang::VAArgExpr"
                        "clang::ValueDecl"
                        "clang::VarTemplateDecl"
                        "clang::VariableArrayType"
                        "clang::VectorType"
                        "clang::WhileStmt"
                        "clang::ast_matchers::MatchFinder::MatchCallback"
                        "clang::ast_matchers::MatchFinder::MatchResult"
                        "clang::ast_matchers::internal::ArgumentAdaptingMatcherFunc<TEMPLATE,struct clang::ast_matchers::internal::TypeList<class clang::Decl, class clang::Stmt, void, void>,struct clang::ast_matchers::internal::TypeList<class clang::Decl, class clang::Stmt, void, void>>"
                        "clang::ast_matchers::internal::ArgumentAdaptingMatcherFunc<TEMPLATE,struct clang::ast_matchers::internal::TypeList<struct clang::ast_matchers::internal::TypeList<class clang::Decl, class clang::Stmt, class clang::NestedNameSpecifier, class clang::NestedNameSpecifierLoc>, struct clang::ast_matchers::internal::TypeList<class clang::QualType, class clang::Type, class clang::TypeLoc, class clang::CXXCtorInitializer>, void, void>,struct clang::ast_matchers::internal::TypeList<struct clang::ast_matchers::internal::TypeList<class clang::Decl, class clang::Stmt, class clang::NestedNameSpecifier, void>, struct clang::ast_matchers::internal::TypeList<class clang::NestedNameSpecifierLoc, class clang::TypeLoc, class clang::QualType, void>, void, void>>"
                        "clang::ast_matchers::internal::BoundNodesMap"
                        "clang::ast_matchers::internal::VariadicAllOfMatcher<class clang::CXXCtorInitializer>"
                        "clang::ast_matchers::internal::VariadicAllOfMatcher<class clang::Decl>"
                        "clang::ast_matchers::internal::VariadicAllOfMatcher<class clang::NestedNameSpecifier>"
                        "clang::ast_matchers::internal::VariadicAllOfMatcher<class clang::NestedNameSpecifierLoc>"
                        "clang::ast_matchers::internal::VariadicAllOfMatcher<class clang::QualType>"
                        "clang::ast_matchers::internal::VariadicAllOfMatcher<class clang::Stmt>"
                        "clang::ast_matchers::internal::VariadicAllOfMatcher<class clang::Type>"
                        "clang::ast_matchers::internal::VariadicAllOfMatcher<class clang::TypeLoc>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Decl,class clang::AccessSpecDecl>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Decl,class clang::CXXConstructorDecl>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Decl,class clang::CXXDestructorDecl>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Decl,class clang::CXXMethodDecl>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Decl,class clang::CXXRecordDecl>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Decl,class clang::ClassTemplateDecl>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Decl,class clang::ClassTemplateSpecializationDecl>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Decl,class clang::DeclaratorDecl>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Decl,class clang::EnumConstantDecl>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Decl,class clang::EnumDecl>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Decl,class clang::FieldDecl>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Decl,class clang::FriendDecl>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Decl,class clang::FunctionDecl>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Decl,class clang::FunctionTemplateDecl>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Decl,class clang::NamedDecl>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Decl,class clang::NamespaceDecl>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Decl,class clang::ParmVarDecl>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Decl,class clang::UnresolvedUsingValueDecl>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Decl,class clang::UsingDecl>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Decl,class clang::VarDecl>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::ArraySubscriptExpr>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::AsmStmt>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::BinaryOperator>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::BreakStmt>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::CStyleCastExpr>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::CXXBindTemporaryExpr>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::CXXBoolLiteralExpr>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::CXXCatchStmt>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::CXXConstCastExpr>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::CXXConstructExpr>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::CXXDefaultArgExpr>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::CXXDeleteExpr>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::CXXDynamicCastExpr>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::CXXForRangeStmt>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::CXXFunctionalCastExpr>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::CXXMemberCallExpr>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::CXXNewExpr>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::CXXNullPtrLiteralExpr>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::CXXOperatorCallExpr>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::CXXReinterpretCastExpr>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::CXXStaticCastExpr>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::CXXTemporaryObjectExpr>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::CXXThisExpr>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::CXXThrowExpr>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::CXXTryStmt>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::CXXUnresolvedConstructExpr>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::CallExpr>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::CaseStmt>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::CastExpr>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::CharacterLiteral>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::CompoundLiteralExpr>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::CompoundStmt>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::ConditionalOperator>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::ContinueStmt>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::DeclRefExpr>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::DeclStmt>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::DefaultStmt>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::DoStmt>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::ExplicitCastExpr>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::Expr>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::FloatingLiteral>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::ForStmt>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::GotoStmt>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::IfStmt>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::ImplicitCastExpr>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::InitListExpr>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::IntegerLiteral>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::LabelStmt>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::LambdaExpr>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::MaterializeTemporaryExpr>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::MemberExpr>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::NullStmt>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::ReturnStmt>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::StringLiteral>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::SwitchCase>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::SwitchStmt>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::UnaryExprOrTypeTraitExpr>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::UnaryOperator>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::UserDefinedLiteral>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Stmt,class clang::WhileStmt>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Type,class clang::ArrayType>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Type,class clang::AtomicType>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Type,class clang::AutoType>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Type,class clang::BlockPointerType>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Type,class clang::BuiltinType>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Type,class clang::ComplexType>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Type,class clang::ConstantArrayType>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Type,class clang::DependentSizedArrayType>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Type,class clang::ElaboratedType>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Type,class clang::FunctionType>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Type,class clang::IncompleteArrayType>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Type,class clang::LValueReferenceType>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Type,class clang::MemberPointerType>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Type,class clang::ParenType>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Type,class clang::PointerType>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Type,class clang::RValueReferenceType>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Type,class clang::RecordType>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Type,class clang::ReferenceType>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Type,class clang::TemplateSpecializationType>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Type,class clang::TypedefType>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Type,class clang::UnaryTransformType>"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher<class clang::Type,class clang::VariableArrayType>"
                        "clang::ast_matchers::internal::VariadicOperatorMatcherFunc<1,1>"
                        "clang::ast_matchers::internal::VariadicOperatorMatcherFunc<2,-1>"
                        "clang::comments::Comment"
                        "clang::comments::FullComment"
                        "clang::tooling::ArgumentsAdjuster"
                        "clang::tooling::ClangStripOutputAdjuster"
                        "clang::tooling::ClangSyntaxOnlyAdjuster"
                        "clang::tooling::CompileCommand"
                        "clang::tooling::FrontendActionFactory"
                        "clang::tooling::JSONCompilationDatabase"
                        "llvm::ArrayRef<class clang::OMPClause *>"
                        "llvm::ArrayRef<class clang::TemplateArgument>"
                        "llvm::ArrayRef<class llvm::Constant *>"
                        "llvm::ArrayRef<class llvm::Type *>"
                        "llvm::ArrayRef<class llvm::Value *>"
                        "llvm::ArrayRef<unsigned int>"
                        "llvm::BranchInst"
                        "llvm::CallInst"
                        "llvm::DenseMapIterator<const class llvm::MDString *,class llvm::MDNode *,struct llvm::DenseMapInfo<const class llvm::MDString *>,1>"
                        "llvm::DenseMapIterator<unsigned int,struct llvm::PointerAlignElem,struct llvm::DenseMapInfo<unsigned int>,1>"
                        "llvm::FoldingSetVectorIterator"
                        "llvm::ManagedStatic<class asttooling::<anonymous>::RegistryMaps>"
                        "llvm::Optional<class clang::ast_matchers::internal::DynTypedMatcher>"
                        "llvm::Optional<enum clang::ARMInterruptAttr::InterruptType>"
                        "llvm::Optional<enum clang::BlocksAttr::BlockType>"
                        "llvm::Optional<enum clang::CallableWhenAttr::ConsumedState>"
                        "llvm::Optional<enum clang::ConsumableAttr::ConsumedState>"
                        "llvm::Optional<enum clang::ObjCMethodFamilyAttr::FamilyKind>"
                        "llvm::Optional<enum clang::ParamTypestateAttr::ConsumedState>"
                        "llvm::Optional<enum clang::PcsAttr::PCSType>"
                        "llvm::Optional<enum clang::ReturnTypestateAttr::ConsumedState>"
                        "llvm::Optional<enum clang::SetTypestateAttr::ConsumedState>"
                        "llvm::Optional<enum clang::TestTypestateAttr::ConsumedState>"
                        "llvm::Optional<enum clang::TypeVisibilityAttr::VisibilityType>"
                        "llvm::Optional<enum clang::VisibilityAttr::VisibilityType>"
                        "llvm::OwningPtr<class llvm::MemoryBuffer>"
                        "llvm::PointerIntPair<class llvm::User *,1,unsigned int,class llvm::PointerLikeTypeTraits<class llvm::User *>>"
                        "llvm::SmallString<128>"
                        "llvm::SmallVector<char,100>"
                        "llvm::SmallVector<int,16>"
                        "llvm::SmallVector<struct clang::RecursiveASTVisitor<class asttooling::ASTVisitor_Adapter>::EnqueueJob,16>"
                        "llvm::SmallVector<struct std::__1::pair<const char *, struct std::__1::pair<int, const char *> >,4>"
                        "llvm::StringMapEntry<class clang::IdentifierInfo *>"
                        "llvm::SwitchInst::CaseIteratorT"
                        "llvm::SwitchInst::CaseIteratorT<const class llvm::SwitchInst,const class llvm::ConstantInt,const class llvm::BasicBlock>"
                        "llvm::generic_gep_type_iterator<const class llvm::Use *>"
                        "llvm::ilist_iterator<class llvm::Argument>"
                        "llvm::ilist_iterator<class llvm::GlobalVariable>"
                        "llvm::ilist_iterator<const class llvm::Function>"
                        "llvm::ilist_iterator<const class llvm::NamedMDNode>"
                        "llvm::mapped_iterator"
                        "llvm::sys::SmartMutex<0>"
                        "llvm::sys::fs::detail::DirIterState"
                        "llvm::value_use_iterator"
                        "llvm::value_use_iterator<const class llvm::User>"
                        "reg::null_type"
                        "std::__1::__bit_iterator<class std::__1::vector<_Bool, class std::__1::allocator<_Bool> >,0,0>"
                        "std::__1::__ignore_t<unsigned char>"
                        "std::__1::__list_const_iterator<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >,void *>"
                        "std::__1::__list_iterator<class boost::iostreams::detail::linked_streambuf<char, struct std::__1::char_traits<char> > *,void *>"
                        "std::__1::__map_const_iterator<class std::__1::__tree_const_iterator<union std::__1::__value_type<class boost::tuples::tuple<unsigned long, unsigned long, unsigned long, long, struct boost::tuples::null_type, struct boost::tuples::null_type, struct boost::tuples::null_type, struct boost::tuples::null_type, struct boost::tuples::null_type, struct boost::tuples::null_type>, struct std::__1::pair<long, int> >, class std::__1::__tree_node<union std::__1::__value_type<class boost::tuples::tuple<unsigned long, unsigned long, unsigned long, long, struct boost::tuples::null_type, struct boost::tuples::null_type, struct boost::tuples::null_type, struct boost::tuples::null_type, struct boost::tuples::null_type, struct boost::tuples::null_type>, struct std::__1::pair<long, int> >, void *> *, long>>"
                        "std::__1::__map_const_iterator<class std::__1::__tree_const_iterator<union std::__1::__value_type<class clang::FileID, class clang::RewriteBuffer>, class std::__1::__tree_node<union std::__1::__value_type<class clang::FileID, class clang::RewriteBuffer>, void *> *, long>>"
                        "std::__1::__map_const_iterator<class std::__1::__tree_const_iterator<union std::__1::__value_type<class clbind::type_id, class mem::smart_ptr<class clbind::ClassRep_O> >, class std::__1::__tree_node<union std::__1::__value_type<class clbind::type_id, class mem::smart_ptr<class clbind::ClassRep_O> >, void *> *, long>>"
                        "std::__1::__map_const_iterator<class std::__1::__tree_const_iterator<union std::__1::__value_type<class clbind::type_id, unsigned long>, class std::__1::__tree_node<union std::__1::__value_type<class clbind::type_id, unsigned long>, void *> *, long>>"
                        "std::__1::__map_const_iterator<class std::__1::__tree_const_iterator<union std::__1::__value_type<class mem::smart_ptr<class core::Class_O>, int>, class std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Class_O>, int>, void *> *, long>>"
                        "std::__1::__map_const_iterator<class std::__1::__tree_const_iterator<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::Class_O> >, class std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::Class_O> >, void *> *, long>>"
                        "std::__1::__map_const_iterator<class std::__1::__tree_const_iterator<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::Function_O> >, class std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::Function_O> >, void *> *, long>>"
                        "std::__1::__map_const_iterator<class std::__1::__tree_const_iterator<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::SingleDispatchGenericFunction_O> >, class std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::SingleDispatchGenericFunction_O> >, void *> *, long>>"
                        "std::__1::__map_const_iterator<class std::__1::__tree_const_iterator<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::T_O> >, class std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::T_O> >, void *> *, long>>"
                        "std::__1::__map_const_iterator<class std::__1::__tree_const_iterator<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, const class asttooling::internal::MatcherDescriptor *>, class std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, const class asttooling::internal::MatcherDescriptor *>, void *> *, long>>"
                        "std::__1::__map_const_iterator<class std::__1::__tree_const_iterator<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, int>, class std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, int>, void *> *, long>>"
                        "std::__1::__map_const_iterator<class std::__1::__tree_const_iterator<union std::__1::__value_type<class mem::weak_smart_ptr<class core::T_O>, struct core::SourcePosInfo>, class std::__1::__tree_node<union std::__1::__value_type<class mem::weak_smart_ptr<class core::T_O>, struct core::SourcePosInfo>, void *> *, long>>"
                        "std::__1::__map_const_iterator<class std::__1::__tree_const_iterator<union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, class clang::ast_type_traits::DynTypedNode>, class std::__1::__tree_node<union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, class clang::ast_type_traits::DynTypedNode>, void *> *, long>>"
                        "std::__1::__map_const_iterator<class std::__1::__tree_const_iterator<union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, class mem::smart_ptr<class core::Character_O> >, class std::__1::__tree_node<union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, class mem::smart_ptr<class core::Character_O> >, void *> *, long>>"
                        "std::__1::__map_const_iterator<class std::__1::__tree_const_iterator<union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, class mem::smart_ptr<class core::LoadTimeValues_O> >, class std::__1::__tree_node<union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, class mem::smart_ptr<class core::LoadTimeValues_O> >, void *> *, long>>"
                        "std::__1::__map_const_iterator<class std::__1::__tree_const_iterator<union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, class mem::smart_ptr<class core::SourceFileInfo_O> >, class std::__1::__tree_node<union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, class mem::smart_ptr<class core::SourceFileInfo_O> >, void *> *, long>>"
                        "std::__1::__map_const_iterator<class std::__1::__tree_const_iterator<union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, class mem::smart_ptr<class core::Symbol_O> >, class std::__1::__tree_node<union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, class mem::smart_ptr<class core::Symbol_O> >, void *> *, long>>"
                        "std::__1::__map_const_iterator<class std::__1::__tree_const_iterator<union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, class mem::smart_ptr<class llvmo::Module_O> >, class std::__1::__tree_node<union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, class mem::smart_ptr<class llvmo::Module_O> >, void *> *, long>>"
                        "std::__1::__map_const_iterator<class std::__1::__tree_const_iterator<union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, int>, class std::__1::__tree_node<union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, int>, void *> *, long>>"
                        "std::__1::__map_const_iterator<class std::__1::__tree_const_iterator<union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, struct core::SymbolStorage>, class std::__1::__tree_node<union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, struct core::SymbolStorage>, void *> *, long>>"
                        "std::__1::__map_const_iterator<class std::__1::__tree_const_iterator<union std::__1::__value_type<int, class mem::smart_ptr<class core::Symbol_O> >, class std::__1::__tree_node<union std::__1::__value_type<int, class mem::smart_ptr<class core::Symbol_O> >, void *> *, long>>"
                        "std::__1::__map_const_iterator<class std::__1::__tree_const_iterator<union std::__1::__value_type<int, class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> > >, class std::__1::__tree_node<union std::__1::__value_type<int, class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> > >, void *> *, long>>"
                        "std::__1::__map_iterator<class std::__1::__tree_iterator<union std::__1::__value_type<class mem::smart_ptr<class core::SymbolVector_O>, class mem::smart_ptr<class core::T_O> >, class std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::SymbolVector_O>, class mem::smart_ptr<class core::T_O> >, void *> *, long>>"
                        "std::__1::__map_iterator<class std::__1::__tree_iterator<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::Class_O> >, class std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::Class_O> >, void *> *, long>>"
                        "std::__1::__map_iterator<class std::__1::__tree_iterator<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::Function_O> >, class std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::Function_O> >, void *> *, long>>"
                        "std::__1::__map_iterator<class std::__1::__tree_iterator<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::SpecialForm_O> >, class std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::SpecialForm_O> >, void *> *, long>>"
                        "std::__1::__map_iterator<class std::__1::__tree_iterator<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::weak_smart_ptr<class core::T_O> >, class std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::weak_smart_ptr<class core::T_O> >, void *> *, long>>"
                        "std::__1::__map_iterator<class std::__1::__tree_iterator<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, const class asttooling::internal::MatcherDescriptor *>, class std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, const class asttooling::internal::MatcherDescriptor *>, void *> *, long>>"
                        "std::__1::__map_iterator<class std::__1::__tree_iterator<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, int>, class std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, int>, void *> *, long>>"
                        "std::__1::__map_iterator<class std::__1::__tree_iterator<union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, class mem::smart_ptr<class core::LoadTimeValues_O> >, class std::__1::__tree_node<union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, class mem::smart_ptr<class core::LoadTimeValues_O> >, void *> *, long>>"
                        "std::__1::__map_iterator<class std::__1::__tree_iterator<union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, class mem::smart_ptr<class llvmo::Module_O> >, class std::__1::__tree_node<union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, class mem::smart_ptr<class llvmo::Module_O> >, void *> *, long>>"
                        "std::__1::__map_iterator<class std::__1::__tree_iterator<union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, int>, class std::__1::__tree_node<union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, int>, void *> *, long>>"
                        "std::__1::__map_iterator<class std::__1::__tree_iterator<union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, struct core::SymbolStorage>, class std::__1::__tree_node<union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, struct core::SymbolStorage>, void *> *, long>>"
                        "std::__1::__map_iterator<class std::__1::__tree_iterator<union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, struct llvmo::GlobalVariableStringHolder>, class std::__1::__tree_node<union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, struct llvmo::GlobalVariableStringHolder>, void *> *, long>>"
                        "std::__1::__map_iterator<class std::__1::__tree_iterator<union std::__1::__value_type<const class std::type_info *, class mem::smart_ptr<class core::Symbol_O> >, class std::__1::__tree_node<union std::__1::__value_type<const class std::type_info *, class mem::smart_ptr<class core::Symbol_O> >, void *> *, long>>"
                        "std::__1::__map_iterator<class std::__1::__tree_iterator<union std::__1::__value_type<int, class mem::smart_ptr<class core::Symbol_O> >, class std::__1::__tree_node<union std::__1::__value_type<int, class mem::smart_ptr<class core::Symbol_O> >, void *> *, long>>"
                        "std::__1::__split_buffer<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >,class std::__1::allocator<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> > > &>"
                        "std::__1::__split_buffer<struct boost::io::detail::format_item<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >,class std::__1::allocator<struct boost::io::detail::format_item<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> > > &>"
                        "std::__1::__split_buffer<struct core::DynamicBinding,class std::__1::allocator<struct core::DynamicBinding> &>"
                        "std::__1::__tree_const_iterator<class mem::smart_ptr<class core::Fixnum_O>,class std::__1::__tree_node<class mem::smart_ptr<class core::Fixnum_O>, void *> *,long>"
                        "std::__1::__tree_const_iterator<class mem::smart_ptr<class core::Function_O>,class std::__1::__tree_node<class mem::smart_ptr<class core::Function_O>, void *> *,long>"
                        "std::__1::__tree_const_iterator<class mem::smart_ptr<class core::SNode_O>,class std::__1::__tree_node<class mem::smart_ptr<class core::SNode_O>, void *> *,long>"
                        "std::__1::__tree_const_iterator<class mem::smart_ptr<class core::Symbol_O>,class std::__1::__tree_node<class mem::smart_ptr<class core::Symbol_O>, void *> *,long>"
                        "std::__1::__tree_const_iterator<class mem::smart_ptr<class core::T_O>,class std::__1::__tree_node<class mem::smart_ptr<class core::T_O>, void *> *,long>"
                        "std::__1::__tree_const_iterator<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >,class std::__1::__tree_node<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, void *> *,long>"
                        "std::__1::__tree_node<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >,void *>"
                        "std::__1::__tree_node_base<void *>"
                        "std::__1::__wrap_iter<char *>"
                        "std::__1::__wrap_iter<class clang::tooling::Range *>"
                        "std::__1::__wrap_iter<class clang::tooling::Replacement *>"
                        "std::__1::__wrap_iter<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> > *>"
                        "std::__1::__wrap_iter<const char *>"
                        "std::__1::__wrap_iter<const class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> > *>"
                        "std::__1::__wrap_iter<const int *>"
                        "std::__1::__wrap_iter<const struct boost::re_detail::named_subexpressions::name *>"
                        "std::__1::__wrap_iter<const struct clbind::value *>"
                        "std::__1::__wrap_iter<struct std::__1::pair<class clbind::type_id, void *(*)(void *)> *>"
                        "std::__1::__wrap_iter<unsigned char *>"
                        "std::__1::__wrap_iter<unsigned long *>"
                        "std::__1::__wrap_iter<void **>"
                        "std::__1::__wrap_iter<wchar_t *>"
                        "std::__1::auto_ptr<struct clbind::detail::registration>"
                        "std::__1::basic_filebuf"
                        "std::__1::basic_ifstream<char,struct std::__1::char_traits<char>>"
                        "std::__1::basic_istringstream<char,struct std::__1::char_traits<char>,class std::__1::allocator<char>>"
                        "std::__1::basic_ofstream<char,struct std::__1::char_traits<char>>"
                        "std::__1::basic_ostringstream<char,struct std::__1::char_traits<char>,class std::__1::allocator<char>>"
                        "std::__1::basic_streambuf<char,struct std::__1::char_traits<char>>"
                        "std::__1::basic_string<char,struct std::__1::char_traits<char>,class std::__1::allocator<char>>"
                        "std::__1::basic_string<wchar_t,struct std::__1::char_traits<wchar_t>,class std::__1::allocator<wchar_t>>"
                        "std::__1::basic_stringstream<char,struct std::__1::char_traits<char>,class std::__1::allocator<char>>"
                        "std::__1::chrono::time_point<class std::__1::chrono::steady_clock,class std::__1::chrono::duration<long long, class std::__1::ratio<1, 1000000000> >>"
                        "std::__1::chrono::time_point<class std::__1::chrono::system_clock,class std::__1::chrono::duration<long double, class std::__1::ratio<1, 1000000000> >>"
                        "std::__1::chrono::time_point<class std::__1::chrono::system_clock,class std::__1::chrono::duration<long long, class std::__1::ratio<1, 1000000> >>"
                        "std::__1::deque<int,class std::__1::allocator<int>>"
                        "std::__1::fpos<__mbstate_t>"
                        "std::__1::istreambuf_iterator<char,struct std::__1::char_traits<char>>"
                        "std::__1::list<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >,class std::__1::allocator<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> > >>"
                        "std::__1::pair<class llvm::StringRef,class llvm::StringRef>"
                        "std::__1::pair<class std::__1::__map_iterator<class std::__1::__tree_iterator<union std::__1::__value_type<class clbind::type_id, unsigned long>, class std::__1::__tree_node<union std::__1::__value_type<class clbind::type_id, unsigned long>, void *> *, long> >,_Bool>"
                        "std::__1::pair<class std::__1::__map_iterator<class std::__1::__tree_iterator<union std::__1::__value_type<class reg::type_id, unsigned long>, class std::__1::__tree_node<union std::__1::__value_type<class reg::type_id, unsigned long>, void *> *, long> >,_Bool>"
                        "std::__1::pair<class std::__1::__tree_const_iterator<class clang::tooling::Replacement, class std::__1::__tree_node<class clang::tooling::Replacement, void *> *, long>,_Bool>"
                        "std::__1::pair<class std::__1::__wrap_iter<const struct boost::re_detail::named_subexpressions::name *>,class std::__1::__wrap_iter<const struct boost::re_detail::named_subexpressions::name *>>"
                        "std::__1::pair<const void *,class llvm::Pass *>"
                        "std::__1::pair<long,int>"
                        "std::__1::pair<void *,int>"
                        "std::__1::queue<struct clbind::detail::<anonymous>::queue_entry,class std::__1::deque<struct clbind::detail::<anonymous>::queue_entry, class std::__1::allocator<struct clbind::detail::<anonymous>::queue_entry> >>"
                        "std::__1::reverse_iterator<class std::__1::__deque_iterator<int, const int *, const int &, const int *const *, long, 1024>>"
                        "std::__1::reverse_iterator<class std::__1::__wrap_iter<const int *>>"
                        "std::__1::reverse_iterator<class std::__1::__wrap_iter<int *>>"
                        "std::__1::set<class clang::tooling::Replacement,struct std::__1::less<class clang::tooling::Replacement>,class std::__1::allocator<class clang::tooling::Replacement>>"
                        "std::__1::set<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >,struct std::__1::less<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> > >,class std::__1::allocator<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> > >>"
                        "std::__1::shared_ptr"
                        "std::__1::uniform_int_distribution<long>"
                        "std::__1::unique_lock<class std::__1::mutex>"
                        "std::__1::unique_ptr<char,void (*)(void *)>"
                        "std::__1::unique_ptr<class clang::CharSourceRange,struct std::__1::default_delete<class clang::CharSourceRange>>"
                        "std::__1::unique_ptr<class clang::PresumedLoc,struct std::__1::default_delete<class clang::PresumedLoc>>"
                        "std::__1::unique_ptr<class clang::QualType,struct std::__1::default_delete<class clang::QualType>>"
                        "std::__1::unique_ptr<class clang::SourceLocation,struct std::__1::default_delete<class clang::SourceLocation>>"
                        "std::__1::unique_ptr<class clang::SourceRange,struct std::__1::default_delete<class clang::SourceRange>>"
                        "std::__1::unique_ptr<class clang::TemplateName,struct std::__1::default_delete<class clang::TemplateName>>"
                        "std::__1::unique_ptr<class clang::TypeLoc,struct std::__1::default_delete<class clang::TypeLoc>>"
                        "std::__1::unique_ptr<class clang::ast_matchers::BoundNodes,struct std::__1::default_delete<class clang::ast_matchers::BoundNodes>>"
                        "std::__1::unique_ptr<class clang::ast_matchers::dynamic::VariantMatcher,struct std::__1::default_delete<class clang::ast_matchers::dynamic::VariantMatcher>>"
                        "std::__1::unique_ptr<class clang::ast_matchers::dynamic::VariantValue,struct std::__1::default_delete<class clang::ast_matchers::dynamic::VariantValue>>"
                        "std::__1::unique_ptr<class clang::tooling::Range,struct std::__1::default_delete<class clang::tooling::Range>>"
                        "std::__1::unique_ptr<class clang::tooling::Replacement,struct std::__1::default_delete<class clang::tooling::Replacement>>"
                        "std::__1::unique_ptr<class llvm::APInt,struct std::__1::default_delete<class llvm::APInt>>"
                        "std::__1::unique_ptr<class llvm::APSInt,struct std::__1::default_delete<class llvm::APSInt>>"
                        "std::__1::unique_ptr<class llvm::StringRef,struct std::__1::default_delete<class llvm::StringRef>>"
                        "std::__1::unique_ptr<class std::__1::__tree_node<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, void *>,class std::__1::__tree_node_destructor<class std::__1::allocator<class std::__1::__tree_node<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, void *> > >>"
                        "std::__1::unique_ptr<struct __sFILE,int (*)(struct __sFILE *)>"
                        "std::__1::unique_ptr<struct clang::tooling::CompileCommand,struct std::__1::default_delete<struct clang::tooling::CompileCommand>>"
                        "std::__1::unique_ptr<unsigned char,void (*)(void *)>"
                        "std::__1::unique_ptr<unsigned int,void (*)(void *)>"
                        "std::__1::vector<_Bool,class std::__1::allocator<_Bool>>"
                        "std::__1::vector<_Bool,type-parameter-0-0>"
                        "std::__1::vector<class asttooling::internal::MatcherDescriptor *,class std::__1::allocator<class asttooling::internal::MatcherDescriptor *>>"
                        "std::__1::vector<class clang::ASTUnit *,class std::__1::allocator<class clang::ASTUnit *>>"
                        "std::__1::vector<class clang::ast_matchers::dynamic::VariantMatcher,class std::__1::allocator<class clang::ast_matchers::dynamic::VariantMatcher>>"
                        "std::__1::vector<class clang::ast_matchers::internal::DynTypedMatcher,class std::__1::allocator<class clang::ast_matchers::internal::DynTypedMatcher>>"
                        "std::__1::vector<class clang::tooling::Range,class std::__1::allocator<class clang::tooling::Range>>"
                        "std::__1::vector<class clang::tooling::Replacement,class std::__1::allocator<class clang::tooling::Replacement>>"
                        "std::__1::vector<class llvm::Constant *,class std::__1::allocator<class llvm::Constant *>>"
                        "std::__1::vector<class llvm::Type *,class std::__1::allocator<class llvm::Type *>>"
                        "std::__1::vector<class llvm::Value *,class std::__1::allocator<class llvm::Value *>>"
                        "std::__1::vector<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >,class std::__1::allocator<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> > >>"
                        "std::__1::vector<class std::__1::list<int, class std::__1::allocator<int> >,class std::__1::allocator<class std::__1::list<int, class std::__1::allocator<int> > >>"
                        "std::__1::vector<class std::__1::vector<_Bool, class std::__1::allocator<_Bool> >,class std::__1::allocator<class std::__1::vector<_Bool, class std::__1::allocator<_Bool> > >>"
                        "std::__1::vector<int,class std::__1::allocator<int>>"
                        "std::__1::vector<struct asttooling::Diagnostics::ErrorContent,class std::__1::allocator<struct asttooling::Diagnostics::ErrorContent>>"
                        "std::__1::vector<struct asttooling::Diagnostics::ErrorContent::Message,class std::__1::allocator<struct asttooling::Diagnostics::ErrorContent::Message>>"
                        "std::__1::vector<struct asttooling::ParserValue,class std::__1::allocator<struct asttooling::ParserValue>>"
                        "std::__1::vector<struct clbind::detail::<anonymous>::cast_entry,class std::__1::allocator<struct clbind::detail::<anonymous>::cast_entry>>"
                        "std::__1::vector<struct clbind::detail::<anonymous>::edge,class std::__1::allocator<struct clbind::detail::<anonymous>::edge>>"
                        "std::__1::vector<struct llvm::GenericValue,class std::__1::allocator<struct llvm::GenericValue>>"
                        "std::__1::vector<struct std::__1::pair<_Bool, struct boost::re_detail::re_syntax_base *>,class std::__1::allocator<struct std::__1::pair<_Bool, struct boost::re_detail::re_syntax_base *> >>"
                        "std::__1::vector<unsigned char,class std::__1::allocator<unsigned char>>"
                        "std::__1::vector<unsigned int,class std::__1::allocator<unsigned int>>"
                        "std::__1::vector<void *,class std::__1::allocator<void *>>"
                        "std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::T_O> >,void *>"
                        "std::__1::__wrap_iter<class core::MicroHeapBlock **>"
                        "std::__1::__wrap_iter<class core::MultiStringBlock **>"
                        "std::__1::__wrap_iter<class core::SequenceStepper *const *>"
                        "std::__1::__wrap_iter<class mem::smart_ptr<class core::Package_O> *>"
                        "std::__1::__wrap_iter<class mem::smart_ptr<class core::Symbol_O> *>"
                        "std::__1::__wrap_iter<class std::__1::__map_iterator<class std::__1::__tree_iterator<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::weak_smart_ptr<class core::T_O> >, class std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::weak_smart_ptr<class core::T_O> >, void *> *, long> > *>"
                        "std::__1::__wrap_iter<const class mem::smart_ptr<class core::Package_O> *>"
                        "std::__1::__wrap_iter<const class mem::smart_ptr<class core::Symbol_O> *>"
                        "std::__1::__wrap_iter<const class mem::smart_ptr<class core::T_O> *>"
                        "std::__1::__wrap_iter<const struct core::AuxArgument *>"
                        "std::__1::__wrap_iter<const struct core::KeywordArgument *>"
                        "std::__1::__wrap_iter<const struct core::OptionalArgument *>"
                        "std::__1::__wrap_iter<const struct core::RequiredArgument *>"
                        "std::__1::__wrap_iter<struct clbind::detail::<anonymous>::edge *>"
                        "std::__1::__wrap_iter<struct core::AuxArgument *>"
                        "std::__1::__wrap_iter<struct core::CacheRecord *>"
                        "std::__1::__wrap_iter<struct core::KeywordArgument *>"
                        "std::__1::__wrap_iter<struct core::OptionalArgument *>"
                        "std::__1::__wrap_iter<struct core::ProfilerFunctionTimer *>"
                        "std::__1::__wrap_iter<struct core::RequiredArgument *>"
                        "std::__1::__wrap_iter<void (**)(class core::Lisp_O *)>"
                        "std::__1::map<class boost::tuples::tuple<unsigned long, unsigned long, unsigned long, long, struct boost::tuples::null_type, struct boost::tuples::null_type, struct boost::tuples::null_type, struct boost::tuples::null_type, struct boost::tuples::null_type, struct boost::tuples::null_type>,struct std::__1::pair<long, int>,struct std::__1::less<class boost::tuples::tuple<unsigned long, unsigned long, unsigned long, long, struct boost::tuples::null_type, struct boost::tuples::null_type, struct boost::tuples::null_type, struct boost::tuples::null_type, struct boost::tuples::null_type, struct boost::tuples::null_type> >,class std::__1::allocator<struct std::__1::pair<const class boost::tuples::tuple<unsigned long, unsigned long, unsigned long, long, struct boost::tuples::null_type, struct boost::tuples::null_type, struct boost::tuples::null_type, struct boost::tuples::null_type, struct boost::tuples::null_type, struct boost::tuples::null_type>, struct std::__1::pair<long, int> > >>"
                        "std::__1::map<class clbind::type_id,class mem::smart_ptr<class clbind::ClassRep_O>,struct std::__1::less<class clbind::type_id>,class std::__1::allocator<struct std::__1::pair<const class clbind::type_id, class mem::smart_ptr<class clbind::ClassRep_O> > >>"
                        "std::__1::map<class clbind::type_id,unsigned long,struct std::__1::less<class clbind::type_id>,class std::__1::allocator<struct std::__1::pair<const class clbind::type_id, unsigned long> >>"
                        "std::__1::map<class reg::type_id,unsigned long,struct std::__1::less<class reg::type_id>,class std::__1::allocator<struct std::__1::pair<const class reg::type_id, unsigned long> >>"
                        "std::__1::map<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >,int,struct std::__1::less<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> > >,class std::__1::allocator<struct std::__1::pair<const class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, int> >>"
                        "std::__1::map<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >,struct llvmo::GlobalVariableStringHolder,struct std::__1::less<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> > >,class std::__1::allocator<struct std::__1::pair<const class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, struct llvmo::GlobalVariableStringHolder> >>"

                        ;; Insert many more strings here
                        )))
                                
(defun check-if-housekeeping-class-ignore (key)
  "Return nil if the key is in the +housekeeping-class-ignores+ otherwise throw a warning and return :maybe"
  (when (string= key "core::T_O")
    #+use-breaks(break "T_O"))
  (if (gethash key +housekeeping-class-ignores+)
      nil
      (progn
        (warn "Check if the following class is rootable and if it is - why isn't it in the housekeeping classes??~%H-C-IGNORE ~a~%NOTE: If this is a Wrapper or Adapter type then we need to deal with it!!!!!!!!" key)
        (when (string= key "gctools::GCVector<class mem::smart_ptr<class core::T_O>,class gctools::GCAlloc_malloc<class gctools::GCVector_impl<class mem::smart_ptr<class core::T_O> > >>")
          (break "Why is this class being checked here?????"))
        #+use-breaks(break "About to return :maybe")
        :maybe)))
                                

(defun rootable-housekeeping-class-p (key &optional (analysis *analysis*))
  (when (string= key "AllocatorFunctor")
    #+use-breaks(break "rootable-housekeeping-class-p"))
  (if (string= key "")
      nil
      (multiple-value-bind (value found)
          (gethash key (project-housekeeping-classes (analysis-project analysis)))
        (if found
            value
            (check-if-housekeeping-class-ignore key)))
      ))


(defmethod contains-smart-pointers-impl-p ((x record-ctype) anal)
  (let ((hc (rootable-housekeeping-class-p (record-ctype-key x) anal)))
    (cond
      ((null hc) nil)
      ((eq hc :maybe) :maybe)
      (t (contains-smart-pointers-impl-p hc anal)))))






(defgeneric on-stack-has-smart-pointers-on-heap-p (x anal))
(defmethod on-stack-has-smart-pointers-on-heap-p ((x t) anal)
  (declare (special *debug-info*))
  (if (eq x :maybe)
      #+use-breaks(break ":maybe in on-stack-has-smart-pointers-on-heap-p"))
  (warn "Add support for on-stack-has-smart-pointers-on-heap-p for ~a of type: ~a~%  ~a~%" x (type-of x) *debug-info*)
  #+use-breaks(break "breaking - ")
  :maybe)


(defmethod on-stack-has-smart-pointers-on-heap-p ((x instance-variable) anal)
  (on-stack-has-smart-pointers-on-heap-p (instance-variable-ctype x) anal))

(defmethod on-stack-has-smart-pointers-on-heap-p ((x rvalue-reference-ctype) anal)
  nil)

(defmethod on-stack-has-smart-pointers-on-heap-p ((x typedef-ctype) anal)
  (on-stack-has-smart-pointers-on-heap-p (typedef-ctype-desugared x) anal))

(defmethod on-stack-has-smart-pointers-on-heap-p ((x elaborated-ctype) anal)
  (on-stack-has-smart-pointers-on-heap-p (elaborated-ctype-named-type x) anal))

(defmethod on-stack-has-smart-pointers-on-heap-p ((x dependent-name-ctype) anal) nil)
(defmethod on-stack-has-smart-pointers-on-heap-p ((x constant-array-ctype) anal) nil)
(defmethod on-stack-has-smart-pointers-on-heap-p ((x incomplete-array-ctype) anal) nil)
(defmethod on-stack-has-smart-pointers-on-heap-p ((x template-type-parm-ctype) anal) nil)
(defmethod on-stack-has-smart-pointers-on-heap-p ((x subst-template-type-parm-ctype) anal) nil)
(defmethod on-stack-has-smart-pointers-on-heap-p ((x lvalue-reference-ctype) anal) nil)
(defmethod on-stack-has-smart-pointers-on-heap-p ((x enum-ctype) anal) nil)
(defmethod on-stack-has-smart-pointers-on-heap-p ((x member-pointer-ctype) anal) nil)
(defmethod on-stack-has-smart-pointers-on-heap-p ((x dependent-sized-array-ctype) anal) nil)

(defmethod on-stack-has-smart-pointers-on-heap-p ((x smart-ptr-ctype) anal) nil)
(defmethod on-stack-has-smart-pointers-on-heap-p ((x weak-smart-ptr-ctype) anal) nil)
(defmethod on-stack-has-smart-pointers-on-heap-p ((x uninteresting-ctype) anal) nil)
(defmethod on-stack-has-smart-pointers-on-heap-p ((x enum-ctype) anal) nil)
(defmethod on-stack-has-smart-pointers-on-heap-p ((x vector-ctype) anal) nil)

(defmethod on-stack-has-smart-pointers-on-heap-p ((x gccontainer) anal) t)


(defconstant +auto-ctype-ignores+
  (build-ignore-table '(
                        "auto"
                        "class clang::tooling::Range"
                        "class clang::tooling::Replacement"
                        "class std::__1::__map_iterator<class std::__1::__tree_iterator<union std::__1::__value_type<const class std::type_info *, class mem::smart_ptr<class core::Symbol_O> >, class std::__1::__tree_node<union std::__1::__value_type<const class std::type_info *, class mem::smart_ptr<class core::Symbol_O> >, void *> *, long> >"
                        "class std::__1::__tree_const_iterator<class mem::smart_ptr<class core::Fixnum_O>, class std::__1::__tree_node<class mem::smart_ptr<class core::Fixnum_O>, void *> *, long>"
                        "class std::__1::__tree_const_iterator<class mem::smart_ptr<class core::Function_O>, class std::__1::__tree_node<class mem::smart_ptr<class core::Function_O>, void *> *, long>"
                        "class std::__1::__tree_const_iterator<class mem::smart_ptr<class core::SNode_O>, class std::__1::__tree_node<class mem::smart_ptr<class core::SNode_O>, void *> *, long>"
                        "class std::__1::__wrap_iter<class clang::tooling::Range *>"
                        "class std::__1::__wrap_iter<class clang::tooling::Replacement *>"
                        "class std::__1::__wrap_iter<class mem::smart_ptr<class core::Symbol_O> *>"
                        "class std::__1::__wrap_iter<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> > *>"


                        )))

(defun check-if-auto-ctype-ignore (key)
  (if (gethash key +auto-ctype-ignores+)
      nil
      (warn "Check if the following auto-ctype class is contains-smart-pointers-p - if not, make it an ignore~%auto-ctype-ignore ~a" key))
  #+use-breaks(break "About to return :maybe")
  :maybe)

(defmethod on-stack-has-smart-pointers-on-heap-p ((x auto-ctype) anal)
  (check-if-auto-ctype-ignore (auto-ctype-description x)))

(defmethod on-stack-has-smart-pointers-on-heap-p ((x pointer-ctype) anal)
  (contains-smart-pointers-p x anal))

(defmethod on-stack-has-smart-pointers-on-heap-p ((x unclassified-ctype) anal)
  (warn "Add support for on-stack-has-smart-pointers-on-heap-p (x unclassified-ctype) ~a" x)
  #+use-breaks(break "About to return :maybe")
  :maybe ;; we will have to follow up on this
  )


(defmethod on-stack-has-smart-pointers-on-heap-p ((x record-ctype) anal)
  (let ((housekeeping-class (rootable-housekeeping-class-p (record-ctype-key x))))
    (cond
      ((null housekeeping-class) nil)
      ((eq housekeeping-class :maybe)
       (warn "Add support for on-stack-has-smart-pointers-on-heap-p (x record-ctype) ~a~% Variable: ~a" x *debug-info*)
       :maybe)
      (t (on-stack-has-smart-pointers-on-heap-p housekeeping-class anal)))))

(defmethod on-stack-has-smart-pointers-on-heap-p ((x housekeeping-class) anal)
  (let* (result
         (project (analysis-project anal))
         (all-classes (project-housekeeping-classes project)))
    (loop :for base-name :in (housekeeping-class-bases x)
       :do (when base-name
             (let ((base (gethash base-name all-classes)))
               (when base
                 (let ((one-res (on-stack-has-smart-pointers-on-heap-p base anal)))
                   (setq result (or result one-res)))))))
    (loop :for field :in (housekeeping-class-fields x)
       :do (let ((one-res (on-stack-has-smart-pointers-on-heap-p field anal)))
             (setq result (or result one-res))))
    (when (and result *track-reachability*)
      (setf (gethash (housekeeping-class-key x) (analysis-housekeeping-class-stored-on-stack anal)) x))
    result))





(defmethod on-stack-has-smart-pointers-on-heap-p ((x unclassified-template-specialization-ctype) anal)
  (let ((desc (unclassified-template-specialization-ctype-description x)))
    (check-if-template-specialization-ignore desc)))


(defmethod on-stack-has-smart-pointers-on-heap-p ((x maybe-interesting-ctype) anal)
  (let ((desc (maybe-interesting-ctype-description x)))
    (cond
      ((search "struct clbind::policies<>" desc) nil)
      (t
       (format t "Determine if maybe-interesting-ctype for ~a has on-stack-has-smart-pointers-on-heap-p~%" x)
       (format t "*debug-info* -> ~a~%" *debug-info*)
       t ;; for now return t
       ))))




(defmethod on-stack-has-smart-pointers-on-heap-p ((x gcholder) anal) t) ;; Yes - always
(defmethod on-stack-has-smart-pointers-on-heap-p ((x rooted-gcholder) anal) t) ;; Yes - always

(defmethod on-stack-has-smart-pointers-on-heap-p ((x stl-container) anal)
  "This should cover every stl-xxxx container"
  (let (sp)
    (dotimes (i (length (stl-container-arguments x)))
      (let ((one-res (contains-smart-pointers-p (elt (stl-container-arguments x) i) anal)))
        (setq sp (or sp one-res))))
    sp))


(defmethod on-stack-has-smart-pointers-on-heap-p ((x builtin-ctype) anal) nil)

(defmethod on-stack-has-smart-pointers-on-heap-p ((x dependent-template-specialization-ctype) anal)
  nil)



(defun maybe-macro-name (str fix)
  (format nil "~a~a" (if (eq fix :maybe) "MAYBE_" "") str))

(defgeneric macro-name (type-info fix))

(defmethod macro-name ((x global-variable) fix)
  (macro-name (global-variable-ctype x) fix))


(defmethod macro-name ((x elaborated-ctype) fix)
  (macro-name (elaborated-ctype-named-type x) fix))

(defmethod macro-name ((x typedef-ctype) fix)
  (macro-name (typedef-ctype-desugared x) fix))

(defmethod macro-name ((x gcholder) fix)
  (format nil "~a~a_FIX" (if (eq fix :maybe) "MAYBE_" "") (gcholder-name x)))

(defmethod macro-name ((x stl-map) fix)
  (handler-case
      (let* ((first-arg (container-argument x 0))
             (second-arg (container-argument x 1))
             (first-smart (is-smart-p first-arg))
             (second-smart (is-smart-p second-arg))
             )
        (cond
          ((and first-smart second-smart) (maybe-macro-name "STLMAP_SMART_FIRST_SECOND_FIX" fix))
          (first-smart (maybe-macro-name "STLMAP_SMART_FIRST_FIX" fix))
          (second-smart (maybe-macro-name "STLMAP_SMART_SECOND_FIX" fix))
          (t "STLMAP_TEMPLATE_SCANNER_FIX")))
    (simple-type-error (err)
      "ERROR_UNHANDLED_STLMAP_TYPE")
    ))



(defmethod macro-name ((x stl-multimap) fix)
  (handler-case
      (let ((first-arg (container-argument x 0))
            (second-arg (container-argument x 1)))
        (cond
          ((and (is-smart-p first-arg) (is-smart-p second-arg)) (maybe-macro-name "STLMULTIMAP_SMART_FIRST_SECOND_FIX" fix))
          ((is-smart-p first-arg) (maybe-macro-name "STLMULTIMAP_SMART_FIRST_FIX" fix))
          ((is-smart-p second-arg) (maybe-macro-name "STLMULTIMAP_SMART_SECOND_FIX" fix))
          (t "STLMULTIMAP_UNKNOWN_TYPE")))
    (simple-type-error (err)
      "ERROR_UNHANDLED_STLMAP_TYPE")
    ))




(defmethod macro-name ((x gcarray0) fix)
  (handler-case
      (let* ((arg (container-argument x 0))
             (type-info (gc-template-argument-ctype arg)))
        (cond
          ((is-smart-p arg) (maybe-macro-name "GCARRAY0_FIX" fix))
          (t (maybe-macro-name "GCARRAY0_FIX" fix))))
    (simple-type-error (err)
      "ERROR_UNHANDLED_GCARRAY0_TYPE")
    ))

(defmethod macro-name ((x gcframe0) fix)
  (handler-case
      (let* ((arg (container-argument x 0))
             (type-info (gc-template-argument-ctype arg)))
        (cond
          ((is-smart-p arg) (maybe-macro-name "GCFRAME0_FIX" fix))
          (t (maybe-macro-name "GCFRAME0_FIX" fix))))
    (simple-type-error (err)
      "ERROR_UNHANDLED_GCFRAME0_TYPE")
    ))

(defmethod macro-name ((x gcvec0) fix)
  (handler-case
      (let* ((arg (container-argument x 0))
             (type-info (gc-template-argument-ctype arg)))
        (maybe-macro-name "GCVEC0_FIX" fix))
    (simple-type-error (err)
      "ERROR_UNHANDLED_GCVEC0_TYPE")
    ))


(defmethod macro-name ((x stl-vector) fix)
  (handler-case
      (let* ((arg (container-argument x 0))
             (type-info (gc-template-argument-ctype arg)))
        (cond
          ((is-smart-p arg) (maybe-macro-name "STLVECTOR_FIX" fix))
          (t (maybe-macro-name "STLVECTOR_HANDLE_FIX" fix))))
    (simple-type-error (err)
      "ERROR_UNHANDLED_STLVECTOR_TYPE")
    ))


(defmethod macro-name ((x stl-set) fix)
  (handler-case
      (let* ((arg (container-argument x 0))
             (type-info (gc-template-argument-ctype arg)))
        (cond
          ((is-smart-p arg) (maybe-macro-name "STLSET_FIX" fix))
          (t (maybe-macro-name "STLSET_HANDLE_CTYPE" fix))))
    (simple-type-error (err)
      "ERROR_UNHANDLED_STLSET_TYPE")
    ))

(defun macro-ctype-name (x)
  (cond
    ((and (typep x 'pointer-ctype)
          (typep (pointer-ctype-pointee x) 'smart-ptr-ctype))
     "POINTER_TO_SMART_PTR")
    (t (type-of x))))

(defmethod macro-name ((x smart-ptr-ctype) fix) (maybe-macro-name "SMART_PTR_FIX" fix))
(defmethod macro-name ((x weak-smart-ptr-ctype) fix) (maybe-macro-name "WEAK_SMART_PTR_FIX" fix))
(defmethod macro-name ((x unclassified-ctype) fix) "IGNORE")
(defmethod macro-name ((x cloned-ctype) fix) "IGNORE")
(defmethod macro-name ((x ctype) fix)
  (substitute #\_ #\-
              (maybe-macro-name (format nil "HANDLE_~a" (macro-ctype-name x)) fix)))






(defun one-class-instance-variables (result-ht class top-class-name)
  (let ((vars (gcobject-subclass-instance-variables class)))
    (mapc #'(lambda (x)
	      (when (gethash (instance-variable-field-name x) result-ht)
                (warn "A duplicate instance variable ~a was seen in top class ~a"
                      (instance-variable-field-name x) top-class-name ))
	      (setf (gethash (instance-variable-field-name x) result-ht) x))
	  vars)))

(defun ancestor-instance-variables-recursive (result-ht classid classes top-class-name)
  "Add the instance variables for classid and then do the same for its base classes"
  (let ((class (gethash classid classes)))
    (when class 
      (gclog "Looked up classid[~a] -> ~a~%" classid class)
      (let* ((bases (if class
                        (gcobject-subclass-bases class)
                        nil)))
        (one-class-instance-variables result-ht class top-class-name)
        (mapc #'(lambda (x)
                  (gclog "Looking at base: ~a~%" x)
                  (ancestor-instance-variables-recursive result-ht x classes top-class-name)) bases)))))


(defun gather-instance-variables (classid classes)
  "Gather all of the instance variables for a class (including the instance variables of base classes)"
  (let ((gathered (make-hash-table :test #'equal)))
    (ancestor-instance-variables-recursive gathered classid classes classid)
    gathered))



(defun has-or-inherits-destructor (class all-classes)
  (if (gcobject-subclass-has-destructor class)
      t
      (loop :for base-class-name :in (gcobject-subclass-bases class)
         :for base-class = (gethash base-class-name all-classes)
         :for inherits = (has-or-inherits-destructor base-class all-classes)
         :until inherits
         :finally (return inherits))
  ))



(defun inherited-metadata (class all-classes memoized-metadata)
  "Recursively accumulate the metadata associated with this class and all of its base classes"
  (or (gethash class memoized-metadata)
      (setf (gethash class memoized-metadata)
            (if (string= (gckind-name class) "core::T_O")
                (gcclass-metadata class)
                (append (gcclass-metadata class)
                        (loop :for base-class-name :in (gcobject-subclass-bases class)
                           :for base-class = (gethash base-class-name all-classes)
                           :append (when base-class
                                       (inherited-metadata base-class all-classes memoized-metadata))
                           )
                        (loop :for base-class-name :in (gcobject-subclass-vbases class)
                           :for base-class = (gethash base-class-name all-classes)
                           :append (when base-class (inherited-metadata base-class all-classes memoized-metadata)) 
                           )
                        )))))


(defun create-metadata-to-class-map (analysis)
  "Builds a hash-table that maps metadata flags to lists of classes that have that metadata"
  (let ((all-classes (project-gcobjects (analysis-project analysis)))
        (results (analysis-metadata-to-gcclass analysis))
        (memoized-metadata (make-hash-table :test #'eq)))
    (maphash (lambda (name class) (inherited-metadata class all-classes memoized-metadata))
             all-classes)
    (clrhash results)
    (maphash (lambda (class metadata-list)
               (dolist (one-metadata metadata-list)
                 (push class (gethash one-metadata results))))
             memoized-metadata)))


(defun is-or-inherits-from-heap-root (class all-classes)
  (if (string= (housekeeping-class-key class) "gctools::HeapRoot")
      t
      (loop :for base-class-name :in (housekeeping-class-bases class)
         :for base-class = (gethash base-class-name all-classes)
         :for inherits = (if base-class
                             (is-or-inherits-from-heap-root base-class all-classes)
                             (check-if-housekeeping-class-ignore base-class-name))
         :until inherits
         :finally (return inherits))
  ))


(defgeneric is-or-inherits-from-stack-root (class all-classes))

(defmethod is-or-inherits-from-stack-root ((class t) all-classes) nil)

(defmethod is-or-inherits-from-stack-root ((class housekeeping-class) all-classes)
  (or (string= (housekeeping-class-key class) "gctools::StackRoot")
      (loop :for base-class-name :in (housekeeping-class-bases class)
         :for base-class = (gethash base-class-name all-classes)
         :for inherits = (if base-class
                             (is-or-inherits-from-stack-root base-class all-classes)
                             (check-if-housekeeping-class-ignore base-class-name))
         :until inherits
         :finally (return inherits))
  ))









;; --------------------------------------------------


(defun housekeeping-one-class-instance-variables (result-ht class top-class-name)
  (let ((vars (housekeeping-class-fields class)))
    (mapc #'(lambda (x)
	      (when (gethash (instance-variable-field-name x) result-ht)
                (warn "A duplicate instance variable ~a was seen in top class ~a"
                      (instance-variable-field-name x) top-class-name ))
	      (setf (gethash (instance-variable-field-name x) result-ht) x))
	  vars)))

(defun housekeeping-ancestor-instance-variables-recursive (result-ht classid classes top-class-name)
  "Add the instance variables for classid and then do the same for its base classes"
  (let ((class (gethash classid classes)))
    (when class 
      (gclog "Looked up classid[~a] -> ~a~%" classid class)
      (let* ((bases (if class
                        (housekeeping-class-bases class)
                        nil)))
        (housekeeping-one-class-instance-variables result-ht class top-class-name)
        (mapc #'(lambda (x)
                  (gclog "Looking at base: ~a~%" x)
                  (housekeeping-ancestor-instance-variables-recursive result-ht x classes top-class-name)) bases)))))


(defun housekeeping-gather-instance-variables (classid classes)
  "Gather all of the instance variables for a class (including the instance variables of base classes)"
  (let ((gathered (make-hash-table :test #'equal)))
    (housekeeping-ancestor-instance-variables-recursive gathered classid classes classid)
    gathered))

;; --------------------------------------------------






(defun ctype-name (classid) classid)


(defun class-enum-name (classid species &optional (prefix "KIND"))
  (let* ((raw-name (copy-seq (ctype-name classid)))
         (name0 (nsubstitute-if #\_ (lambda (c) (member c '(#\SPACE #\, #\< #\> #\: ))) raw-name))
         (name (nsubstitute #\P #\* name0))
         )
    (format nil "~a_~a_~a" prefix (symbol-name (species-name species)) name)))



(defun code-for-instance-var (output-stream ptr-name instance-var type-info &optional fix)
  (let ((macro-name (macro-name type-info fix)))
    (if macro-name
      (format output-stream "    ~A(~A->~A); /* ~a */~%" macro-name ptr-name instance-var type-info)
      (format output-stream "    NO_MACRO_AVAILABLE(~a->~a); /* ~a */~%" ptr-name instance-var type-info)
      )))


(defun code-for-global-var (stream type-info global-var location fix)
  (format stream "    ~a(~a); /* ~a ~a */~%" (macro-name type-info fix) global-var location type-info)
)



(defun inherits-from-gcobject (base-name child-name &optional (project *project*))
  (when (string= base-name child-name) (return-from inherits-from-gcobject t))
  (when (string= "core::T_O" child-name) (return-from inherits-from-gcobject nil))
  (let ((child (gethash child-name (project-gcobjects project))))
    (unless child (return-from inherits-from-gcobject nil))
    (dolist (child-base-name (gcobject-subclass-bases child))
      (when (inherits-from-gcobject base-name child-base-name project)
        (return-from inherits-from-gcobject t)))
    (dolist (child-vbase-name (gcobject-subclass-vbases child))
      (when (inherits-from-gcobject base-name child-vbase-name project)
        (return-from inherits-from-gcobject t)))
    nil))


(defun identify-non-moving-classes (non-moving-list &optional (project *project*))
  "Identifies the long lived classes, SYMBOL and CLASS and those that inherit from them
that we do not want the GC to move - they will be allocated in an AMS pool"
  (let (non-moving)
    (dolist (nm-base non-moving-list)
      (maphash (lambda (k v)
              (when (inherits-from-gcobject nm-base k project)
                (push k non-moving)))
            (project-gcobjects project)))
    non-moving))


(defun generate-code-for-non-moving-classes (stream non-moving-list &optional (project *project*))
  (let ((non-moving-classes non-moving-list)
        ;;(non-moving-classes (identify-non-moving-classes non-moving-list project))
        (separated-by-namespace (make-hash-table :test #'equal)))
    (format stream "#ifdef NON_MOVING_CLASSES~%")
    (mapc (lambda (class &aux (cn (gcobject-subclass-key class)))
            (let ((namespace (subseq cn 0 (search "::" cn)))
                  (name (subseq cn (+ (search "::" cn) 2) (length cn))))
              (push name (gethash namespace separated-by-namespace nil))))
          non-moving-classes)
    (maphash (lambda (namespace names)
               (format stream "namespace ~a {~%" namespace)
               (mapc (lambda (cn)
                       (format stream "    class ~a;~%" cn))
                     names)
               (format stream "};~%"))
             separated-by-namespace)
    (format stream "namespace gctools {~%")
    (maphash (lambda (namespace names)
               (mapc (lambda (cn)
                       (format stream "template <> struct allocation_point<~a::~a> {~%"
                               namespace cn)
                       (format stream "    static mps_ap_t get() {~%")
                       (format stream "        return NON_MOVING_POOL_ALLOCATION_POINT;~%")
                       (format stream "    };~%")
                       (format stream "};~%")
                       ) names)
               ) separated-by-namespace)
    (format stream "#endif // NON_MOVING_CLASSES~%")
    ))
      

(defun generate-code-initialize-kind-for-species (stream species-name-list analysis)
  (let* ((manager (analysis-manager analysis)))
    (dolist (species-name species-name-list)
      (let* ((species (lookup-species manager species-name))
             (enums (gethash species (analysis-species-to-gcenum analysis))))
        (mapc (lambda (enum &aux (kind (gcenum-gckind enum)) (key (gckind-key kind)))
                (unless (or (string= "_RootDummyClass" key)
                            (string= "gctools::GCObject" key)
                            (string= "asttooling::Foo<int>" key))
                  (format stream "    initializeKind<~a>();~%" key)))
              enums)
        ))))
             



(defun split-by-one-space (string)
  "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
  (loop for i = 0 then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))


;; ------------------------------------------------------------
;; ------------------------------------------------------------
;; ------------------------------------------------------------
;; ------------------------------------------------------------
;; ------------------------------------------------------------
;; ------------------------------------------------------------



(defun generate-kind-enum (fout anal)
  (let ((all-enums (analysis-list-of-all-gcenums anal)))
    (format fout "enum { KIND_null = 0, ~%")
    (dolist (entry all-enums)
      (let ((enum-name (gcenum-enum-name entry))
            (enum-val (compact-enum-value (gcenum-species-num entry) (gcenum-kind-num entry))))
        (format fout "~A = 0x~x,~%" enum-name enum-val)))
    (format fout "}~%" )
    ))


(defun identify-namespace-define (classid)
  (let ((substrings (split-by-one-space classid)))
    (if (eql (length substrings) 1)
        (subseq (car substrings) 0 (search "::" (car substrings)))
        (let ((first-namespace (subseq (first substrings) 0 (search "::" (first substrings))))
              (second-namespace (let ((next-qualified (loop for x in (cdr substrings)
                                                           until (search "::" x)
                                                           finally (return x))))
                                  (subseq next-qualified 0 (search "::" next-qualified)))))
          (format nil "~a_~a" first-namespace second-namespace)))))


(defun generate-one-gc-info (output-stream entry)
  (let* ((classid (gckind-key (gcenum-gckind entry)))
         (species (gcenum-species entry))
         (enum-name (class-enum-name classid species))
         (namespace (identify-namespace-define classid))
         )
        (format output-stream "#if  defined(NAMESPACE_~a) || defined(GCINFO_~a)~%" namespace enum-name)
        (format output-stream "//GCInfo for ~a~%" entry)
        (format output-stream "template <> class gctools::GCInfo<~A> {~%" (ctype-name classid))
        (format output-stream "public:~%")
        (format output-stream "  static gctools::GCKindEnum const Kind = gctools::~a ;~%" enum-name)
        (format output-stream "};~%")
        (format output-stream "#endif~%")
        ))


(defun generate-gc-info (fout anal)
  (let ((all-enums (analysis-list-of-all-gcenums anal)))
    (dolist (entry all-enums)
      (generate-one-gc-info fout entry))
    ))



(defun generate-kind-name-map (fout anal)
  (let ((all-enums (analysis-list-of-all-gcenums anal)))
    (dolist (gcenum all-enums)
      (let* ((entry (gcenum-gckind gcenum))
             (species (gcenum-species gcenum))
             (classid (gckind-key entry))
             (enum-name (class-enum-name classid species)))
        (format fout "   case ~A: return \"~A\";~%" enum-name enum-name)))
  ))


(defconstant +ptr-name+ "obj_gc_safe"
  "This variable is used to temporarily hold a pointer to a Wrapper<...> object - we want the GC to ignore it")













;; ------------------------------------------------------------
;; ------------------------------------------------------------
;; ------------------------------------------------------------
;; ------------------------------------------------------------
















(defun find-globals-locals-parameters (&key testing)
  (let ((results (ast-tooling:ast-search '(ast-tooling:global-variables ast-tooling:local-variables)
                                         *compile-commands-path*
                                         (if testing *testing-files* nil))))
    (let ((globals (cdr (assoc 'ast-tooling:global-variables results)))
	  (locals (cdr (assoc 'ast-tooling:local-variables results)))
	  (parameters (cdr (assoc 'ast-tooling:parameters results))))
      (values globals locals parameters))))


(defun match-right (str right-sub-str)
  (let ((len-str (length str))
	(len-right-sub-str (length right-sub-str)))
    (if (>= len-str len-right-sub-str)
	(string= (substr str (- len-str len-right-sub-str) len-right-sub-str) right-sub-str)
	nil)))

(defun filter-out-redundant-globals (globals)
  (let ((necessary-globals (make-hash-table :test #'equal))
	(unclassified-globals (make-hash-table :test #'equal))
	(ignore-globals (make-hash-table :test #'equal)))
    (format t "Number of globals: ~a~%" (hash-table-count globals))
    (maphash #'(lambda (var-name v)
		 (format t "Var: ~a    v-> ~a~%" var-name v)
		 (let ((source-location (car v))
		       (type-info (caddr v)))
		   (format t "type-info --> ~A~%" type-info)
		   (cond
		     ((or (and
			   (smart-ptr-ctype-p type-info)
			   (string= "core::Symbol_O" (smart-ptr-ctype-specializer type-info)))
			  (match-right var-name "::___staticDereferencedUnboundInstance")
			  (match-right var-name "::___staticDereferencedNilInstance")
			  (match-right var-name "::___staticClass")
			  (match-right var-name "::_unbound")
			  (match-right var-name "::_nil"))
		      (setf (gethash var-name ignore-globals) v))
		     ((unclassified-ctype-p type-info)
		      (setf (gethash var-name unclassified-globals) v))
		     (t
		      (setf (gethash var-name necessary-globals) v)))))
	     globals)
    (format t "Done scanning globals~%")
    (values necessary-globals unclassified-globals ignore-globals)))


(defun has-smart-ptr-args (args)
  (let ((has-smart nil))
    (mapc #'(lambda (v)
	      (when (eq (car v) 'ast-tooling:smart-ptr)
		(setq has-smart t)))
	  args)))

(defun filter-unrooted-locals (locals)
  (let ((unrooted nil))
    (maphash #'(lambda (k v)
		 (let* ((type-info (caddr v)))
		   (let* ((type (car type-info)))
		     (when (or 
			    (eq type 'ast-tooling:stl-vector)
			    (eq type 'ast-tooling:stl-set)
			    (eq type 'ast-tooling:stl-map))
		       (let ((args (cddr (cadr type-info))))
			 (print (list k v))
                         #|			 (when (has-smart-args args)
                         (setq unrooted (cons (cons k v) unrooted)))
                |#
			 )
		       )
		     )))
	     locals)
    unrooted))














;;
;; Extract all header files into one .cc file
;;






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



(defun inherits-from-gcholder (decl)
  (gclog "inherits-from-gcholder ~a~%" decl)
  (unless (cast:has-definition decl) (return-from inherits-from-gcholder nil))
  (ext:do-c++-iterator (it (cast:bases-iterator decl))
    (let ((qty (cast:get-type it)))
      (when qty
        (let ((base-decl (cast:get-as-cxxrecord-decl (cast:get-type-ptr-or-null qty))))
          (when base-decl
            (let ((base-name (cast:get-name-as-string base-decl)))
              (when (string= base-name "GCHolder")
                (return-from inherits-from-gcholder t))))))))
  nil)


(defun inherits-from-gccontainer (decl)
  (gclog "inherits-from-gccontainer ~a~%" decl)
  (unless (cast:has-definition decl) (return-from inherits-from-gccontainer nil))
  (ext:do-c++-iterator (it (cast:bases-iterator decl))
    (let ((qty (cast:get-type it)))
      (when qty
        (let ((base-decl (cast:get-as-cxxrecord-decl (cast:get-type-ptr-or-null qty))))
          (when base-decl
            (let ((base-name (cast:get-name-as-string base-decl)))
              (when (string= base-name "GCContainer")
                (return-from inherits-from-gccontainer t))))))))
  nil)




(defun inherits-from-rooted-gcholder (decl)
  (gclog "inherits-from-rooted-gcholder ~a~%" decl)
  (unless (cast:has-definition decl) (return-from inherits-from-rooted-gcholder nil))
  (ext:do-c++-iterator (it (cast:bases-iterator decl))
    (let ((qty (cast:get-type it)))
      (when qty
        (let ((base-decl (cast:get-as-cxxrecord-decl (cast:get-type-ptr-or-null qty))))
          (when base-decl
            (let ((base-name (cast:get-name-as-string base-decl)))
              (when (string= base-name "RootedGCHolder")
                (return-from inherits-from-rooted-gcholder t))))))))
  nil)


(defun stl-name (suffix)
  (intern (string-upcase (format nil "STL~a" suffix)) :cl-user))








(define-condition unsupported-type (error)
  ((type :initarg :type :accessor unsupported-type)))

(defvar *debug-info* nil)

(defgeneric classify-ctype (type))

(defun describe-type (type)
  (cast:get-as-string (cast:desugar type)))

(defmacro make-defmethods.classify-ctype (name-prefixes)
  (let (methods)
    (dolist (name-prefix name-prefixes)
      (push `(defmethod classify-ctype
                 ((type ,(intern (format nil "~a-TYPE" (string name-prefix)) :cast)))
               (,(intern (format nil "MAKE-~a-CTYPE" (string name-prefix))) :description (describe-type type)))
            methods))
    `(progn ,@methods)))



(make-defmethods.classify-ctype (#:dependent-sized-array
                                 #:constant-array
                                 #:incomplete-array
                                 #:rvalue-reference
                                 #:lvalue-reference
                                 #:subst-template-type-parm
                                 #:template-type-parm
                                 #:builtin
                                 #:enum
                                 #:member-pointer
                                 #:function-proto
                                 #:template-type-parm
                                 #:dependent-name
                                 #:vector
                                 #:auto
                                 #:dependent-template-specialization
                                 ))



(defconstant +record-ctype-ignores+
  (build-ignore-table '(
                        "<anonymous>"
                        "__darwin_fp_control"
                        "__darwin_fp_status"
                        "__darwin_i386_avx_state"
                        "__darwin_i386_exception_state"
                        "__darwin_i386_float_state"
                        "__darwin_i386_thread_state"
                        "__darwin_mmst_reg"
                        "__darwin_sigaltstack"
                        "__darwin_x86_avx_state64"
                        "__darwin_x86_exception_state64"
                        "__darwin_x86_float_state64"
                        "__darwin_x86_thread_state64"
                        "__darwin_xmm_reg"
                        "__double2"
                        "__float2"
                        "__gmp_alloc_cstring"
                        "__gmp_binary_expr<class __gmp_expr<mpf_t, mpf_t>,double,struct __gmp_binary_divides>"
                        "__gmp_binary_expr<class __gmp_expr<mpf_t, mpf_t>,double,struct __gmp_binary_minus>"
                        "__gmp_binary_expr<class __gmp_expr<mpf_t, mpf_t>,double,struct __gmp_binary_multiplies>"
                        "__gmp_binary_expr<class __gmp_expr<mpf_t, mpf_t>,double,struct __gmp_binary_plus>"
                        "__gmp_binary_expr<class __gmp_expr<mpf_t, mpf_t>,long,struct __gmp_binary_divides>"
                        "__gmp_binary_expr<class __gmp_expr<mpf_t, mpf_t>,long,struct __gmp_binary_minus>"
                        "__gmp_binary_expr<class __gmp_expr<mpf_t, mpf_t>,long,struct __gmp_binary_multiplies>"
                        "__gmp_binary_expr<class __gmp_expr<mpf_t, mpf_t>,long,struct __gmp_binary_plus>"
                        "__gmp_binary_expr<class __gmp_expr<mpf_t, mpf_t>,unsigned long,struct __gmp_binary_divides>"
                        "__gmp_binary_expr<class __gmp_expr<mpf_t, mpf_t>,unsigned long,struct __gmp_binary_lshift>"
                        "__gmp_binary_expr<class __gmp_expr<mpf_t, mpf_t>,unsigned long,struct __gmp_binary_minus>"
                        "__gmp_binary_expr<class __gmp_expr<mpf_t, mpf_t>,unsigned long,struct __gmp_binary_multiplies>"
                        "__gmp_binary_expr<class __gmp_expr<mpf_t, mpf_t>,unsigned long,struct __gmp_binary_plus>"
                        "__gmp_binary_expr<class __gmp_expr<mpf_t, mpf_t>,unsigned long,struct __gmp_binary_rshift>"
                        "__gmp_binary_expr<class __gmp_expr<mpq_t, mpq_t>,class __gmp_expr<mpz_t, mpz_t>,struct __gmp_binary_minus>"
                        "__gmp_binary_expr<class __gmp_expr<mpq_t, mpq_t>,class __gmp_expr<mpz_t, mpz_t>,struct __gmp_binary_plus>"
                        "__gmp_binary_expr<class __gmp_expr<mpq_t, mpq_t>,double,struct __gmp_binary_divides>"
                        "__gmp_binary_expr<class __gmp_expr<mpq_t, mpq_t>,double,struct __gmp_binary_minus>"
                        "__gmp_binary_expr<class __gmp_expr<mpq_t, mpq_t>,double,struct __gmp_binary_multiplies>"
                        "__gmp_binary_expr<class __gmp_expr<mpq_t, mpq_t>,double,struct __gmp_binary_plus>"
                        "__gmp_binary_expr<class __gmp_expr<mpq_t, mpq_t>,long,struct __gmp_binary_divides>"
                        "__gmp_binary_expr<class __gmp_expr<mpq_t, mpq_t>,long,struct __gmp_binary_minus>"
                        "__gmp_binary_expr<class __gmp_expr<mpq_t, mpq_t>,long,struct __gmp_binary_multiplies>"
                        "__gmp_binary_expr<class __gmp_expr<mpq_t, mpq_t>,long,struct __gmp_binary_plus>"
                        "__gmp_binary_expr<class __gmp_expr<mpq_t, mpq_t>,unsigned long,struct __gmp_binary_divides>"
                        "__gmp_binary_expr<class __gmp_expr<mpq_t, mpq_t>,unsigned long,struct __gmp_binary_lshift>"
                        "__gmp_binary_expr<class __gmp_expr<mpq_t, mpq_t>,unsigned long,struct __gmp_binary_minus>"
                        "__gmp_binary_expr<class __gmp_expr<mpq_t, mpq_t>,unsigned long,struct __gmp_binary_multiplies>"
                        "__gmp_binary_expr<class __gmp_expr<mpq_t, mpq_t>,unsigned long,struct __gmp_binary_plus>"
                        "__gmp_binary_expr<class __gmp_expr<mpq_t, mpq_t>,unsigned long,struct __gmp_binary_rshift>"
                        "__gmp_binary_expr<class __gmp_expr<mpz_t, mpz_t>,class __gmp_expr<mpq_t, mpq_t>,struct __gmp_binary_minus>"
                        "__gmp_binary_expr<class __gmp_expr<mpz_t, mpz_t>,class __gmp_expr<mpq_t, mpq_t>,struct __gmp_binary_plus>"
                        "__gmp_binary_expr<class __gmp_expr<mpz_t, mpz_t>,double,struct __gmp_binary_and>"
                        "__gmp_binary_expr<class __gmp_expr<mpz_t, mpz_t>,double,struct __gmp_binary_divides>"
                        "__gmp_binary_expr<class __gmp_expr<mpz_t, mpz_t>,double,struct __gmp_binary_ior>"
                        "__gmp_binary_expr<class __gmp_expr<mpz_t, mpz_t>,double,struct __gmp_binary_minus>"
                        "__gmp_binary_expr<class __gmp_expr<mpz_t, mpz_t>,double,struct __gmp_binary_modulus>"
                        "__gmp_binary_expr<class __gmp_expr<mpz_t, mpz_t>,double,struct __gmp_binary_multiplies>"
                        "__gmp_binary_expr<class __gmp_expr<mpz_t, mpz_t>,double,struct __gmp_binary_plus>"
                        "__gmp_binary_expr<class __gmp_expr<mpz_t, mpz_t>,double,struct __gmp_binary_xor>"
                        "__gmp_binary_expr<class __gmp_expr<mpz_t, mpz_t>,long,struct __gmp_binary_and>"
                        "__gmp_binary_expr<class __gmp_expr<mpz_t, mpz_t>,long,struct __gmp_binary_divides>"
                        "__gmp_binary_expr<class __gmp_expr<mpz_t, mpz_t>,long,struct __gmp_binary_ior>"
                        "__gmp_binary_expr<class __gmp_expr<mpz_t, mpz_t>,long,struct __gmp_binary_minus>"
                        "__gmp_binary_expr<class __gmp_expr<mpz_t, mpz_t>,long,struct __gmp_binary_modulus>"
                        "__gmp_binary_expr<class __gmp_expr<mpz_t, mpz_t>,long,struct __gmp_binary_multiplies>"
                        "__gmp_binary_expr<class __gmp_expr<mpz_t, mpz_t>,long,struct __gmp_binary_plus>"
                        "__gmp_binary_expr<class __gmp_expr<mpz_t, mpz_t>,long,struct __gmp_binary_xor>"
                        "__gmp_binary_expr<class __gmp_expr<mpz_t, mpz_t>,unsigned long,struct __gmp_binary_and>"
                        "__gmp_binary_expr<class __gmp_expr<mpz_t, mpz_t>,unsigned long,struct __gmp_binary_divides>"
                        "__gmp_binary_expr<class __gmp_expr<mpz_t, mpz_t>,unsigned long,struct __gmp_binary_ior>"
                        "__gmp_binary_expr<class __gmp_expr<mpz_t, mpz_t>,unsigned long,struct __gmp_binary_lshift>"
                        "__gmp_binary_expr<class __gmp_expr<mpz_t, mpz_t>,unsigned long,struct __gmp_binary_minus>"
                        "__gmp_binary_expr<class __gmp_expr<mpz_t, mpz_t>,unsigned long,struct __gmp_binary_modulus>"
                        "__gmp_binary_expr<class __gmp_expr<mpz_t, mpz_t>,unsigned long,struct __gmp_binary_multiplies>"
                        "__gmp_binary_expr<class __gmp_expr<mpz_t, mpz_t>,unsigned long,struct __gmp_binary_plus>"
                        "__gmp_binary_expr<class __gmp_expr<mpz_t, mpz_t>,unsigned long,struct __gmp_binary_rshift>"
                        "__gmp_binary_expr<class __gmp_expr<mpz_t, mpz_t>,unsigned long,struct __gmp_binary_xor>"
                        "__gmp_expr<__mpf_struct [1],__mpf_struct [1]>"
                        "__gmp_expr<__mpq_struct [1],__mpq_struct [1]>"
                        "__gmp_expr<__mpz_struct [1],__mpz_struct [1]>"
                        "__nl_cat_d"
                        "__sbuf"
                        "__sigaction_u"
                        "_opaque_pthread_cond_t"
                        "_opaque_pthread_mutex_t"
                        "_opaque_pthread_t"
                        "boost::align::a8"
                        "boost::archive::basic_streambuf_locale_saver<char,struct std::__1::char_traits<char>>"
                        "boost::archive::detail::basic_pointer_iserializer"
                        "boost::archive::detail::basic_pointer_oserializer"
                        "boost::archive::library_version_type"
                        "boost::array<float,35>"
                        "boost::array<long double,171>"
                        "boost::container::ordered_range_t"
                        "boost::container::ordered_unique_range_t"
                        "boost::detail::aligned_storage::aligned_storage_imp::data_t"
                        "boost::detail::esft2_deleter_wrapper"
                        "boost::detail::has_one_T<char>"
                        "boost::detail::has_one_T<double>"
                        "boost::detail::has_one_T<float>"
                        "boost::detail::has_one_T<int (class boost::detail::alignment_dummy::*)(void)>"
                        "boost::detail::has_one_T<int class boost::detail::alignment_dummy::*>"
                        "boost::detail::has_one_T<int>"
                        "boost::detail::has_one_T<long double>"
                        "boost::detail::has_one_T<long long>"
                        "boost::detail::has_one_T<long>"
                        "boost::detail::has_one_T<short>"
                        "boost::detail::has_one_T<void (*)(void)>"
                        "boost::detail::has_one_T<void *>"
                        "boost::detail::lightweight_mutex"
                        "boost::detail::lightweight_mutex::scoped_lock"
                        "boost::detail::make_property_map_from_arg_pack_gen<struct boost::graph::keywords::tag::color_map,enum boost::default_color_type>"
                        "boost::detail::shared_count"
                        "boost::detail::sp_counted_base"
                        "boost::detail::spinlock_pool<2>::scoped_lock"
                        "boost::exception_detail::error_info_container"
                        "boost::exception_detail::refcount_ptr<struct boost::exception_detail::error_info_container>"
                        "boost::filesystem::directory_entry"
                        "boost::filesystem::file_status"
                        "boost::filesystem::path"
                        "boost::hash<char>"
                        "boost::io::basic_altstringbuf<char,struct std::__1::char_traits<char>,class std::__1::allocator<char>>"
                        "boost::io::basic_oaltstringstream<char, std::__1::char_traits<char>, std::__1::allocator<char> >::No_Op"
                        "boost::io::detail::group0"
                        "boost::io::detail::group2<class std::__1::__iom_t5,const double &>"
                        "boost::io::detail::group2<class std::__1::__iom_t5,const long double &>"
                        "boost::io::detail::stream_format_state<char,struct std::__1::char_traits<char>>"
                        "boost::io::ios_flags_saver"
                        "boost::io::ios_precision_saver"
                        "boost::math::constants::detail::constant_initializer2::initializer"
                        "boost::math::constants::detail::constant_initializer::initializer"
                        "boost::math::detail::bessel_i0_initializer::init"
                        "boost::math::detail::bessel_i1_initializer::init"
                        "boost::math::detail::bessel_j0_initializer::init"
                        "boost::math::detail::bessel_j1_initializer::init"
                        "boost::math::detail::bessel_k0_initializer::init"
                        "boost::math::detail::bessel_k1_initializer::init"
                        "boost::math::detail::bessel_y0_initializer::init"
                        "boost::math::detail::bessel_y1_initializer::init"
                        "boost::math::detail::digamma_initializer::init"
                        "boost::math::detail::erf_initializer::init"
                        "boost::math::detail::erf_inv_initializer::init"
                        "boost::math::detail::expint_1_initializer::init"
                        "boost::math::detail::expint_i_initializer::init"
                        "boost::math::detail::expm1_initializer::init"
                        "boost::math::detail::igamma_initializer::init"
                        "boost::math::detail::lgamma_initializer::init"
                        "boost::math::detail::log1p_initializer::init"
                        "boost::math::detail::min_shift_initializer::init"
                        "boost::math::detail::owens_t_initializer::init"
                        "boost::math::detail::zeta_initializer::init"
                        "boost::math::lanczos::lanczos_initializer::init"
                        "boost::math::lanczos::lanczos_initializer<boost::math::lanczos::lanczos17m64, long double>::init"
                        "boost::multi_index::detail::index_matcher::entry"
                        "boost::no_property"
                        "boost::null_visitor"
                        "boost::optional<class std::__1::locale>"
                        "boost::optional_detail::aligned_storage::dummy_u"
                        "boost::optional_detail::aligned_storage<class std::__1::locale>"
                        "boost::optional_detail::aligned_storage<std::__1::locale>::dummy_u"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::attractive_force>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::buffer>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::capacity_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::centrality_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::color_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::cooling>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::diameter_range>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::discover_time_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::displacement_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::distance_combine>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::distance_compare>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::distance_inf>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::distance_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::distance_zero>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::edge_centrality_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::edge_color_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::edge_copy>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::edges_equivalent>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::force_pairs>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::in_parallel>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::index_in_heap_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::isomorphism_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::iterations>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::learning_constant_range>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::lookahead>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::lowpoint_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::max_priority_queue>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::orig_to_copy>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::parity_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::polling>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::predecessor_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::rank_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::repulsive_force>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::residual_capacity_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::reverse_edge_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::root_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::root_vertex>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::vertex_assignment_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::vertex_copy>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::vertex_index1_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::vertex_index2_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::vertex_index_map>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::vertex_invariant1>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::vertex_invariant2>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::vertex_invariant>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::vertex_max_invariant>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::vertices_equivalent>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::visitor>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::weight_map2>"
                        "boost::parameter::keyword<struct boost::graph::keywords::tag::weight_map>"
                        "boost::parameter::void_"
                        "boost::random::binomial_distribution::<anonymous union>::<anonymous>"
                        "boost::random::binomial_distribution::<anonymous>"
                        "boost::random::binomial_distribution::param_type"
                        "boost::random::chi_squared_distribution::param_type"
                        "boost::random::detail::div_t"
                        "boost::random::detail::generator_seed_seq<class boost::random::linear_congruential_engine<unsigned int, 16807, 0, 2147483647>>"
                        "boost::random::detail::generator_seed_seq<class boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>>"
                        "boost::random::discrete_distribution::param_type"
                        "boost::random::extreme_value_distribution::param_type"
                        "boost::random::fisher_f_distribution::param_type"
                        "boost::random::gamma_distribution::param_type"
                        "boost::random::geometric_distribution::param_type"
                        "boost::random::linear_congruential_engine<unsigned int,16807,0,2147483647>"
                        "boost::random::linear_congruential_engine<unsigned int,40014,0,2147483563>"
                        "boost::random::linear_congruential_engine<unsigned long long,25214903917,11,281474976710656>"
                        "boost::random::negative_binomial_distribution::param_type"
                        "boost::random::piecewise_constant_distribution::param_type"
                        "boost::random::piecewise_linear_distribution::param_type"
                        "boost::random::poisson_distribution::<anonymous union>::<anonymous>"
                        "boost::random::poisson_distribution::<anonymous>"
                        "boost::random::poisson_distribution::param_type"
                        "boost::random::student_t_distribution::param_type"
                        "boost::random::triangle_distribution::param_type"
                        "boost::random::uniform_int_distribution::param_type"
                        "boost::random::uniform_real_distribution::param_type"
                        "boost::random::weibull_distribution::param_type"
                        "boost::scoped_ptr<class std::__1::locale>"
                        "boost::serialization::collection_size_type"
                        "boost::serialization::extended_type_info"
                        "boost::serialization::item_version_type"
                        "boost::serialization::version_type"
                        "boost::shared_ptr<class boost::io::basic_altstringbuf<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >>"
                        "boost::shared_ptr<const void>"
                        "boost::shared_ptr<struct boost::filesystem::detail::dir_itr_imp>"
                        "boost::shared_ptr<struct boost::filesystem::detail::recur_dir_itr_imp>"
                        "boost::shared_ptr<struct boost::filesystem::filesystem_error::m_imp>"
                        "boost::shared_ptr<void>"
                        "boost::system::error_code"
                        "boost::unordered::piecewise_construct_t"
                        "boost_132::detail::shared_count"
                        "boost_132::detail::sp_counted_base"
                        "gctools::Fwd2_s"
                        "gctools::Fwd_s"
                        "gctools::Header_u"
                        "gctools::Kind_s"
                        "gctools::Pad1_s"
                        "gctools::Pad_s"
                        "sigval"
                        "std::__1::__allocator_destructor<class std::__1::allocator<class std::__1::__function::__func<struct core::MapLambda, class std::__1::allocator<struct core::MapLambda>, void (class mem::smart_ptr<class core::SNode_O>)> >>"
                        "std::__1::__bit_iterator<class std::__1::vector<_Bool, class std::__1::allocator<_Bool> >,0,0>"
                        "std::__1::__compressed_pair<char *,class std::__1::allocator<char>>"
                        "std::__1::__compressed_pair<char *,void (*)(void *)>"
                        "std::__1::__compressed_pair<class boost::filesystem::directory_iterator *,class std::__1::allocator<class boost::filesystem::directory_iterator> &>"
                        "std::__1::__compressed_pair<class boost::filesystem::directory_iterator *,class std::__1::allocator<class boost::filesystem::directory_iterator>>"
                        "std::__1::__compressed_pair<class std::__1::__function::__func<struct core::MapLambda, class std::__1::allocator<struct core::MapLambda>, void (class mem::smart_ptr<class core::SNode_O>)> *,class std::__1::__allocator_destructor<class std::__1::allocator<class std::__1::__function::__func<struct core::MapLambda, class std::__1::allocator<struct core::MapLambda>, void (class mem::smart_ptr<class core::SNode_O>)> > >>"
                        "std::__1::__compressed_pair<class std::__1::__tree_end_node<class std::__1::__tree_node_base<void *> *>,class std::__1::allocator<class std::__1::__tree_node<class boost::shared_ptr<const void>, void *> >>"
                        "std::__1::__compressed_pair<class std::__1::__tree_end_node<class std::__1::__tree_node_base<void *> *>,class std::__1::allocator<class std::__1::__tree_node<class mem::smart_ptr<class core::Function_O>, void *> >>"
                        "std::__1::__compressed_pair<class std::__1::__tree_end_node<class std::__1::__tree_node_base<void *> *>,class std::__1::allocator<class std::__1::__tree_node<class mem::smart_ptr<class core::SNode_O>, void *> >>"
                        "std::__1::__compressed_pair<class std::__1::__tree_end_node<class std::__1::__tree_node_base<void *> *>,class std::__1::allocator<class std::__1::__tree_node<class mem::smart_ptr<class core::Symbol_O>, void *> >>"
                        "std::__1::__compressed_pair<class std::__1::__tree_end_node<class std::__1::__tree_node_base<void *> *>,class std::__1::allocator<class std::__1::__tree_node<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, void *> >>"
                        "std::__1::__compressed_pair<class std::__1::__tree_end_node<class std::__1::__tree_node_base<void *> *>,class std::__1::allocator<class std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::Class_O> >, void *> >>"
                        "std::__1::__compressed_pair<class std::__1::__tree_end_node<class std::__1::__tree_node_base<void *> *>,class std::__1::allocator<class std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::Function_O> >, void *> >>"
                        "std::__1::__compressed_pair<class std::__1::__tree_end_node<class std::__1::__tree_node_base<void *> *>,class std::__1::allocator<class std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::SingleDispatchGenericFunction_O> >, void *> >>"
                        "std::__1::__compressed_pair<class std::__1::__tree_end_node<class std::__1::__tree_node_base<void *> *>,class std::__1::allocator<class std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::SpecialForm_O> >, void *> >>"
                        "std::__1::__compressed_pair<class std::__1::__tree_end_node<class std::__1::__tree_node_base<void *> *>,class std::__1::allocator<class std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::T_O> >, void *> >>"
                        "std::__1::__compressed_pair<class std::__1::__tree_end_node<class std::__1::__tree_node_base<void *> *>,class std::__1::allocator<class std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::weak_smart_ptr<class core::T_O> >, void *> >>"
                        "std::__1::__compressed_pair<class std::__1::__tree_end_node<class std::__1::__tree_node_base<void *> *>,class std::__1::allocator<class std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, int>, void *> >>"
                        "std::__1::__compressed_pair<class std::__1::__tree_end_node<class std::__1::__tree_node_base<void *> *>,class std::__1::allocator<class std::__1::__tree_node<union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, class mem::smart_ptr<class core::LoadTimeValues_O> >, void *> >>"
                        "std::__1::__compressed_pair<class std::__1::__tree_end_node<class std::__1::__tree_node_base<void *> *>,class std::__1::allocator<class std::__1::__tree_node<union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, class mem::smart_ptr<class core::SourceFileInfo_O> >, void *> >>"
                        "std::__1::__compressed_pair<class std::__1::__tree_end_node<class std::__1::__tree_node_base<void *> *>,class std::__1::allocator<class std::__1::__tree_node<union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, int>, void *> >>"
                        "std::__1::__compressed_pair<class std::__1::__tree_end_node<class std::__1::__tree_node_base<void *> *>,class std::__1::allocator<class std::__1::__tree_node<union std::__1::__value_type<int, class mem::smart_ptr<class core::Symbol_O> >, void *> >>"
                        "std::__1::__compressed_pair<class std::__1::__tree_node<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, void *> *,class std::__1::__tree_node_destructor<class std::__1::allocator<class std::__1::__tree_node<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, void *> > >>"
                        "std::__1::__compressed_pair<class std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::T_O> >, void *> *,class std::__1::__map_node_destructor<class std::__1::allocator<class std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::T_O> >, void *> > >>"
                        "std::__1::__compressed_pair<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> > *,class std::__1::allocator<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> > > &>"
                        "std::__1::__compressed_pair<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> > *,class std::__1::allocator<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> > >>"
                        "std::__1::__compressed_pair<class std::__1::vector<_Bool, class std::__1::allocator<_Bool> > *,class std::__1::allocator<class std::__1::vector<_Bool, class std::__1::allocator<_Bool> > >>"
                        "std::__1::__compressed_pair<struct __sFILE *,int (*)(struct __sFILE *)>"
                        "std::__1::__compressed_pair<struct boost::io::detail::format_item<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> > *,class std::__1::allocator<struct boost::io::detail::format_item<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> > > &>"
                        "std::__1::__compressed_pair<struct boost::io::detail::format_item<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> > *,class std::__1::allocator<struct boost::io::detail::format_item<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> > >>"
                        "std::__1::__compressed_pair<struct core::MapLambda,class std::__1::allocator<struct core::MapLambda>>"
                        "std::__1::__compressed_pair<struct std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >::__rep,class std::__1::allocator<char>>"
                        "std::__1::__compressed_pair<struct std::__1::basic_string<wchar_t, struct std::__1::char_traits<wchar_t>, class std::__1::allocator<wchar_t> >::__rep,class std::__1::allocator<wchar_t>>"
                        "std::__1::__compressed_pair<unsigned char *,void (*)(void *)>"
                        "std::__1::__compressed_pair<unsigned int *,class std::__1::allocator<unsigned int>>"
                        "std::__1::__compressed_pair<unsigned int *,void (*)(void *)>"
                        "std::__1::__compressed_pair<unsigned long,class std::__1::__map_value_compare<class mem::smart_ptr<class core::Symbol_O>, union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::Class_O> >, struct std::__1::less<class mem::smart_ptr<class core::Symbol_O> >, true>>"
                        "std::__1::__compressed_pair<unsigned long,class std::__1::__map_value_compare<class mem::smart_ptr<class core::Symbol_O>, union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::Function_O> >, struct std::__1::less<class mem::smart_ptr<class core::Symbol_O> >, true>>"
                        "std::__1::__compressed_pair<unsigned long,class std::__1::__map_value_compare<class mem::smart_ptr<class core::Symbol_O>, union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::SingleDispatchGenericFunction_O> >, struct std::__1::less<class mem::smart_ptr<class core::Symbol_O> >, true>>"
                        "std::__1::__compressed_pair<unsigned long,class std::__1::__map_value_compare<class mem::smart_ptr<class core::Symbol_O>, union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::SpecialForm_O> >, struct std::__1::less<class mem::smart_ptr<class core::Symbol_O> >, true>>"
                        "std::__1::__compressed_pair<unsigned long,class std::__1::__map_value_compare<class mem::smart_ptr<class core::Symbol_O>, union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::T_O> >, struct std::__1::less<class mem::smart_ptr<class core::Symbol_O> >, true>>"
                        "std::__1::__compressed_pair<unsigned long,class std::__1::__map_value_compare<class mem::smart_ptr<class core::Symbol_O>, union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::weak_smart_ptr<class core::T_O> >, struct std::__1::less<class mem::smart_ptr<class core::Symbol_O> >, true>>"
                        "std::__1::__compressed_pair<unsigned long,class std::__1::__map_value_compare<class mem::smart_ptr<class core::Symbol_O>, union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, int>, struct std::__1::less<class mem::smart_ptr<class core::Symbol_O> >, true>>"
                        "std::__1::__compressed_pair<unsigned long,class std::__1::__map_value_compare<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, class mem::smart_ptr<class core::LoadTimeValues_O> >, struct std::__1::less<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> > >, true>>"
                        "std::__1::__compressed_pair<unsigned long,class std::__1::__map_value_compare<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, class mem::smart_ptr<class core::SourceFileInfo_O> >, struct std::__1::less<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> > >, true>>"
                        "std::__1::__compressed_pair<unsigned long,class std::__1::__map_value_compare<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, int>, struct std::__1::less<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> > >, true>>"
                        "std::__1::__compressed_pair<unsigned long,class std::__1::__map_value_compare<int, union std::__1::__value_type<int, class mem::smart_ptr<class core::Symbol_O> >, struct std::__1::less<int>, true>>"
                        "std::__1::__compressed_pair<unsigned long,class std::__1::allocator<unsigned long>>"
                        "std::__1::__compressed_pair<unsigned long,struct boost::archive::detail::shared_ptr_helper::collection_type_compare>"
                        "std::__1::__compressed_pair<unsigned long,struct std::__1::less<class mem::smart_ptr<class core::Function_O> >>"
                        "std::__1::__compressed_pair<unsigned long,struct std::__1::less<class mem::smart_ptr<class core::SNode_O> >>"
                        "std::__1::__compressed_pair<unsigned long,struct std::__1::less<class mem::smart_ptr<class core::Symbol_O> >>"
                        "std::__1::__compressed_pair<unsigned long,struct std::__1::less<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> > >>"
                        "std::__1::__compressed_pair<void (**)(class core::Lisp_O *),class std::__1::allocator<void (*)(class core::Lisp_O *)>>"
                        "std::__1::__compressed_pair<wchar_t *,class std::__1::allocator<wchar_t>>"
                        "std::__1::__destruct_n"
                        "std::__1::__ignore_t<unsigned char>"
                        "std::__1::__iom_t5"
                        "std::__1::__map_const_iterator<class std::__1::__tree_const_iterator<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, int>, class std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, int>, void *> *, long>>"
                        "std::__1::__map_iterator<class std::__1::__tree_iterator<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, int>, class std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, int>, void *> *, long>>"
                        "std::__1::__map_node_destructor<class std::__1::allocator<class std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::T_O> >, void *> >>"
                        "std::__1::__rs_default"
                        "std::__1::__split_buffer<class mem::smart_ptr<class core::T_O>,class std::__1::allocator<class mem::smart_ptr<class core::T_O> > &>"
                        "std::__1::__split_buffer<struct boost::io::detail::format_item<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >,class std::__1::allocator<struct boost::io::detail::format_item<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> > > &>"
                        "std::__1::__split_buffer<struct core::DynamicBinding,class std::__1::allocator<struct core::DynamicBinding> &>"
                        "std::__1::__tree<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >,struct std::__1::less<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> > >,class std::__1::allocator<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> > >>"
                        "std::__1::__tree<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::Class_O> >,class std::__1::__map_value_compare<class mem::smart_ptr<class core::Symbol_O>, union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::Class_O> >, struct std::__1::less<class mem::smart_ptr<class core::Symbol_O> >, true>,class std::__1::allocator<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::Class_O> > >>"
                        "std::__1::__tree<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::Function_O> >,class std::__1::__map_value_compare<class mem::smart_ptr<class core::Symbol_O>, union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::Function_O> >, struct std::__1::less<class mem::smart_ptr<class core::Symbol_O> >, true>,class std::__1::allocator<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::Function_O> > >>"
                        "std::__1::__tree<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::SingleDispatchGenericFunction_O> >,class std::__1::__map_value_compare<class mem::smart_ptr<class core::Symbol_O>, union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::SingleDispatchGenericFunction_O> >, struct std::__1::less<class mem::smart_ptr<class core::Symbol_O> >, true>,class std::__1::allocator<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::SingleDispatchGenericFunction_O> > >>"
                        "std::__1::__tree<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::SpecialForm_O> >,class std::__1::__map_value_compare<class mem::smart_ptr<class core::Symbol_O>, union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::SpecialForm_O> >, struct std::__1::less<class mem::smart_ptr<class core::Symbol_O> >, true>,class std::__1::allocator<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::SpecialForm_O> > >>"
                        "std::__1::__tree<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::T_O> >,class std::__1::__map_value_compare<class mem::smart_ptr<class core::Symbol_O>, union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::T_O> >, struct std::__1::less<class mem::smart_ptr<class core::Symbol_O> >, true>,class std::__1::allocator<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::T_O> > >>"
                        "std::__1::__tree<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::weak_smart_ptr<class core::T_O> >,class std::__1::__map_value_compare<class mem::smart_ptr<class core::Symbol_O>, union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::weak_smart_ptr<class core::T_O> >, struct std::__1::less<class mem::smart_ptr<class core::Symbol_O> >, true>,class std::__1::allocator<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::weak_smart_ptr<class core::T_O> > >>"
                        "std::__1::__tree<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, int>,class std::__1::__map_value_compare<class mem::smart_ptr<class core::Symbol_O>, union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, int>, struct std::__1::less<class mem::smart_ptr<class core::Symbol_O> >, true>,class std::__1::allocator<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, int> >>"
                        "std::__1::__tree<union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, class mem::smart_ptr<class core::LoadTimeValues_O> >,class std::__1::__map_value_compare<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, class mem::smart_ptr<class core::LoadTimeValues_O> >, struct std::__1::less<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> > >, true>,class std::__1::allocator<union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, class mem::smart_ptr<class core::LoadTimeValues_O> > >>"
                        "std::__1::__tree<union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, class mem::smart_ptr<class core::SourceFileInfo_O> >,class std::__1::__map_value_compare<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, class mem::smart_ptr<class core::SourceFileInfo_O> >, struct std::__1::less<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> > >, true>,class std::__1::allocator<union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, class mem::smart_ptr<class core::SourceFileInfo_O> > >>"
                        "std::__1::__tree<union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, int>,class std::__1::__map_value_compare<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, int>, struct std::__1::less<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> > >, true>,class std::__1::allocator<union std::__1::__value_type<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, int> >>"
                        "std::__1::__tree<union std::__1::__value_type<int, class mem::smart_ptr<class core::Symbol_O> >,class std::__1::__map_value_compare<int, union std::__1::__value_type<int, class mem::smart_ptr<class core::Symbol_O> >, struct std::__1::less<int>, true>,class std::__1::allocator<union std::__1::__value_type<int, class mem::smart_ptr<class core::Symbol_O> > >>"
                        "std::__1::__tree_const_iterator<class mem::smart_ptr<class core::SNode_O>,class std::__1::__tree_node<class mem::smart_ptr<class core::SNode_O>, void *> *,long>"
                        "std::__1::__tree_const_iterator<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >,class std::__1::__tree_node<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, void *> *,long>"
                        "std::__1::__tree_const_iterator<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::Function_O> >,class std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::Function_O> >, void *> *,long>"
                        "std::__1::__tree_const_iterator<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, int>,class std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, int>, void *> *,long>"
                        "std::__1::__tree_end_node<class std::__1::__tree_node_base<void *> *>"
                        "std::__1::__tree_iterator<class mem::smart_ptr<class core::SNode_O>,class std::__1::__tree_node<class mem::smart_ptr<class core::SNode_O>, void *> *,long>"
                        "std::__1::__tree_iterator<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >,class std::__1::__tree_node<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, void *> *,long>"
                        "std::__1::__tree_iterator<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::Function_O> >,class std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::Function_O> >, void *> *,long>"
                        "std::__1::__tree_iterator<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, int>,class std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, int>, void *> *,long>"
                        "std::__1::__tree_node<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >,void *>"
                        "std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::T_O> >,void *>"
                        "std::__1::__tree_node_base<void *>"
                        "std::__1::__tree_node_destructor<class std::__1::allocator<class std::__1::__tree_node<class mem::smart_ptr<class core::SNode_O>, void *> >>"
                        "std::__1::__tree_node_destructor<class std::__1::allocator<class std::__1::__tree_node<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, void *> >>"
                        "std::__1::__tuple_impl<struct std::__1::__tuple_indices<0>,TEMPLATE-ARG-AS-STRING::PACK>"
                        "std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>,class mem::smart_ptr<class core::Function_O>>"
                        "std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>,class mem::smart_ptr<class core::T_O>>"
                        "std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>,class mem::weak_smart_ptr<class core::T_O>>"
                        "std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>,int>"
                        "std::__1::__value_type<int,class mem::smart_ptr<class core::Symbol_O>>"
                        "std::__1::__wrap_iter<const char *>"
                        "std::__1::__wrap_iter<const class mem::smart_ptr<class core::T_O> *>"
                        "std::__1::adopt_lock_t"
                        "std::__1::aligned_storage<24, 16>::type"
                        "std::__1::allocator<char>"
                        "std::__1::allocator<class std::__1::__function::__func<struct core::MapLambda, class std::__1::allocator<struct core::MapLambda>, void (class mem::smart_ptr<class core::SNode_O>)>>"
                        "std::__1::allocator_arg_t"
                        "std::__1::basic_filebuf<char,struct std::__1::char_traits<char>>"
                        "std::__1::basic_istream::sentry"
                        "std::__1::basic_istream<char, std::__1::char_traits<char> >::sentry"
                        "std::__1::basic_ofstream<char,struct std::__1::char_traits<char>>"
                        "std::__1::basic_ostream::sentry"
                        "std::__1::basic_ostream<char, std::__1::char_traits<char> >::sentry"
                        "std::__1::basic_streambuf<char,struct std::__1::char_traits<char>>"
                        "std::__1::basic_string::__long"
                        "std::__1::basic_string::__raw"
                        "std::__1::basic_string::__rep::<anonymous>"
                        "std::__1::basic_string::__short"
                        "std::__1::basic_string::__short::<anonymous>"
                        "std::__1::basic_string<char, std::__1::char_traits<char>, std::__1::allocator<char> >::__long"
                        "std::__1::basic_string<char, std::__1::char_traits<char>, std::__1::allocator<char> >::__raw"
                        "std::__1::basic_string<char, std::__1::char_traits<char>, std::__1::allocator<char> >::__rep"
                        "std::__1::basic_string<char, std::__1::char_traits<char>, std::__1::allocator<char> >::__rep::<anonymous>"
                        "std::__1::basic_string<char, std::__1::char_traits<char>, std::__1::allocator<char> >::__short"
                        "std::__1::basic_string<char, std::__1::char_traits<char>, std::__1::allocator<char> >::__short::<anonymous>"
                        "std::__1::basic_string<char,struct std::__1::char_traits<char>,class std::__1::allocator<char>>"
                        "std::__1::basic_string<wchar_t, std::__1::char_traits<wchar_t>, std::__1::allocator<wchar_t> >::__long"
                        "std::__1::basic_string<wchar_t, std::__1::char_traits<wchar_t>, std::__1::allocator<wchar_t> >::__raw"
                        "std::__1::basic_string<wchar_t, std::__1::char_traits<wchar_t>, std::__1::allocator<wchar_t> >::__rep"
                        "std::__1::basic_string<wchar_t, std::__1::char_traits<wchar_t>, std::__1::allocator<wchar_t> >::__rep::<anonymous>"
                        "std::__1::basic_string<wchar_t, std::__1::char_traits<wchar_t>, std::__1::allocator<wchar_t> >::__short"
                        "std::__1::basic_string<wchar_t, std::__1::char_traits<wchar_t>, std::__1::allocator<wchar_t> >::__short::<anonymous>"
                        "std::__1::basic_string<wchar_t,struct std::__1::char_traits<wchar_t>,class std::__1::allocator<wchar_t>>"
                        "std::__1::basic_stringbuf<char,struct std::__1::char_traits<char>,class std::__1::allocator<char>>"
                        "std::__1::basic_stringstream<char,struct std::__1::char_traits<char>,class std::__1::allocator<char>>"
                        "std::__1::chrono::duration<long double,class std::__1::ratio<1, 1000000000>>"
                        "std::__1::chrono::duration<long long,class std::__1::ratio<1, 1000000000>>"
                        "std::__1::chrono::duration<long long,class std::__1::ratio<1, 1000000>>"
                        "std::__1::chrono::time_point<class std::__1::chrono::steady_clock,class std::__1::chrono::duration<long long, class std::__1::ratio<1, 1000000000> >>"
                        "std::__1::chrono::time_point<class std::__1::chrono::system_clock,class std::__1::chrono::duration<long double, class std::__1::ratio<1, 1000000000> >>"
                        "std::__1::chrono::time_point<class std::__1::chrono::system_clock,class std::__1::chrono::duration<long long, class std::__1::ratio<1, 1000000> >>"
                        "std::__1::condition_variable"
                        "std::__1::defer_lock_t"
                        "std::__1::error_code"
                        "std::__1::istreambuf_iterator<char,struct std::__1::char_traits<char>>"
                        "std::__1::locale"
                        "std::__1::locale::id"
                        "std::__1::map<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >,class mem::smart_ptr<class core::LoadTimeValues_O>,struct std::__1::less<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> > >,class std::__1::allocator<struct std::__1::pair<const class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, class mem::smart_ptr<class core::LoadTimeValues_O> > >>"
                        "std::__1::map<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >,class mem::smart_ptr<class core::SourceFileInfo_O>,struct std::__1::less<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> > >,class std::__1::allocator<struct std::__1::pair<const class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, class mem::smart_ptr<class core::SourceFileInfo_O> > >>"
                        "std::__1::map<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >,int,struct std::__1::less<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> > >,class std::__1::allocator<struct std::__1::pair<const class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, int> >>"
                        "std::__1::money_base::pattern"
                        "std::__1::mutex"
                        "std::__1::once_flag"
                        "std::__1::piecewise_construct_t"
                        "std::__1::stack<class boost::filesystem::directory_iterator,class std::__1::vector<class boost::filesystem::directory_iterator, class std::__1::allocator<class boost::filesystem::directory_iterator> >>"
                        "std::__1::try_to_lock_t"
                        "std::__1::uniform_int_distribution::param_type"
                        "std::__1::uniform_int_distribution<long>"
                        "std::__1::uniform_int_distribution<long>::param_type"
                        "std::__1::unique_lock<class std::__1::mutex>"
                        "std::__1::unique_ptr<char,void (*)(void *)>"
                        "std::__1::unique_ptr<class std::__1::__function::__base<void (class mem::smart_ptr<class core::SNode_O>)>,class std::__1::__allocator_destructor<class std::__1::allocator<class std::__1::__function::__func<struct core::MapLambda, class std::__1::allocator<struct core::MapLambda>, void (class mem::smart_ptr<class core::SNode_O>)> > >>"
                        "std::__1::unique_ptr<class std::__1::__function::__func<struct core::MapLambda, class std::__1::allocator<struct core::MapLambda>, void (class mem::smart_ptr<class core::SNode_O>)>,class std::__1::__allocator_destructor<class std::__1::allocator<class std::__1::__function::__func<struct core::MapLambda, class std::__1::allocator<struct core::MapLambda>, void (class mem::smart_ptr<class core::SNode_O>)> > >>"
                        "std::__1::unique_ptr<class std::__1::__tree_node<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, void *>,class std::__1::__tree_node_destructor<class std::__1::allocator<class std::__1::__tree_node<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, void *> > >>"
                        "std::__1::unique_ptr<class std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::T_O> >, void *>,class std::__1::__map_node_destructor<class std::__1::allocator<class std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::T_O> >, void *> > >>"
                        "std::__1::unique_ptr<struct __sFILE,int (*)(struct __sFILE *)>"
                        "std::__1::unique_ptr<unsigned char,void (*)(void *)>"
                        "std::__1::unique_ptr<unsigned int,void (*)(void *)>"
                        "std::__1::vector<_Bool,class std::__1::allocator<_Bool>>"
                        "std::__1::vector<class boost::filesystem::directory_iterator,class std::__1::allocator<class boost::filesystem::directory_iterator>>"
                        "std::exception_ptr"
                        "std::nested_exception"
                        "std::out_of_range"
                        "timespec"
                        "timeval"
                        "wait::<anonymous>"
                        )))



(defun check-uninteresting-class-template-specialization-decl-name (name)
  (or
   (string= name "ArrayRef")
   (string= name "Iterator")
   (string= name "ManagedStatic")
   (string= name "Matcher")
   (string= name "OptionDiffPrinter")
   (string= name "OptionValue")
   (string= name "Optional")
   (string= name "ValuesClass")
   (string= name "__allocator_destructor")
   (string= name "__bit_iterator")
   (string= name "__compressed_pair")
   (string= name "__gmp_binary_expr")
   (string= name "__gmp_expr")
   (string= name "__ignore_t")
   (string= name "__list_const_iterator")
   (string= name "__map_const_iterator")
   (string= name "__map_iterator")
   (string= name "__map_node_destructor")
   (string= name "__ph")
   (string= name "__split_buffer")
   (string= name "__tree")
   (string= name "__tree_const_iterator")
   (string= name "__tree_end_node")
   (string= name "__tree_iterator")
   (string= name "__tree_node")
   (string= name "__tree_node_base")
   (string= name "__tree_node_destructor")
   (string= name "__tuple_impl")
   (string= name "__value_type")
   (string= name "__wrap_iter")
   (string= name "aligned")
   (string= name "aligned_storage")
   (string= name "allocator")
   (string= name "auto_ptr")
   (string= name "basic_altstringbuf")
   (string= name "basic_filebuf")
   (string= name "basic_format")
   (string= name "basic_ifstream")
   (string= name "basic_istringstream")
   (string= name "basic_ofstream")
   (string= name "basic_regex")
   (string= name "basic_streambuf")
   (string= name "basic_streambuf_locale_saver")
   (string= name "basic_string")
   (string= name "basic_stringbuf")
   (string= name "basic_stringstream")
   (string= name "bool_")
   (string= name "class_")
   (string= name "constructor")
   (string= name "deque")
   (string= name "duration")
   (string= name "externalClass_")
   (string= name "filtering_stream")
   (string= name "format_item")
   (string= name "generator_seed_seq")
   (string= name "group2")
   (string= name "has_one_T")
   (string= name "hash")
   (string= name "ilist_iterator")
   (string= name "initializer_list")
   (string= name "integral_constant")
   (string= name "istreambuf_iterator")
   (string= name "keyword")
   (string= name "less")
   (string= name "linear_congruential_engine")
   (string= name "make_property_map_from_arg_pack_gen")
   (string= name "mersenne_twister_engine")
   (string= name "multiple_values")
   (string= name "normal_distribution")
   (string= name "optional")
   (string= name "refcount_ptr")
   (string= name "reverse_iterator")
   (string= name "scoped_ptr")
   (string= name "specific_decl_iterator")
   (string= name "stream_format_state")
   (string= name "time_point")
   (string= name "uniform_int_distribution")
   (string= name "uniform_real")
   (string= name "unique_lock")
   (string= name "unique_ptr")
   (string= name "variate_generator")))


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
           ((string= name "weak_smart_ptr")
            (assert (eql (cast:size args) 1) nil "Must have 1 argument")
            (let* ((arg (cast:template-argument-list-get args 0))
                   (qtarg (cast:get-as-type arg)))
              (make-weak-smart-ptr-ctype :key decl-key :specializer (cast:get-as-string qtarg))))
           ((string= name "Frame0") (make-gcframe0 :key decl-key :arguments (classify-template-args decl)))
           ((string= name "Array0") (make-gcarray0 :key decl-key :arguments (classify-template-args decl)))
           ((string= name "Vec0") (make-gcvec0 :key decl-key :arguments (classify-template-args decl)))
           ((string= name "pair") (make-stl-pair :key decl-key :arguments (classify-template-args decl)))
           ((string= name "list") (make-stl-list :key decl-key :arguments (classify-template-args decl)))
           ((string= name "vector") (make-stl-vector :key decl-key :arguments (classify-template-args decl)))
           ((string= name "set") (make-stl-set  :key decl-key :arguments (classify-template-args decl)))
           ((string= name "map") (make-stl-map :key decl-key :arguments (classify-template-args decl)))
           ((string= name "queue") (make-stl-queue :key decl-key :arguments (classify-template-args decl)))
           ((string= name "stack") (make-stl-stack  :key decl-key :arguments (classify-template-args decl)))
           ((string= name "multimap") (make-stl-multimap  :key decl-key :arguments (classify-template-args decl)))
           ((string= name "from_object") (make-from-object-ctype :key decl-key))
           ((string= name "LispObjectAllocatorFunctor") (make-lisp-object-allocator-functor-ctype :key decl-key))
           ((string= name "static_scope") (make-static-scope :key decl-key))
           ((inherits-from-gcholder decl)
            (assert (eql (cast:size args) 2) nil
                    "~a requires 2 arg only but was given ~a args"
                    name
                    (cast:size args))
            (make-gcholder :key decl-key
                           :name (intern (string-upcase (format nil "GCHOLDER_~a" name)))
                           :arguments (classify-template-args decl)))
           ((inherits-from-rooted-gcholder decl)
            (assert (eql (cast:size args) 2) nil
                    "~a requires 2 arg only but was given ~a args"
                    name
                    (cast:size args))
            (make-rooted-gcholder :key decl-key
                                  :name (intern (string-upcase (format nil "ROOTED_GCHOLDER_~a" name)))
                                  :arguments (classify-template-args decl)))
           ((check-uninteresting-class-template-specialization-decl-name name)
            (make-uninteresting-ctype :description decl-key))
           (t
            #+gc-warnings(warn "classify-ctype cast:record-type unhandled class-template-specialization-decl  key = ~a  name = ~a~%IGNORE-NAME ~a~%IGNORE-KEY ~a" decl-key name name decl-key)
            (make-class-template-specialization-ctype :key decl-key 
                                                      :arguments (classify-template-args decl)
                                                      )))))
      (cast:cxxrecord-decl
       (if (gethash decl-key +record-ctype-ignores+)
           (make-uninteresting-ctype :description decl-key)
           (progn
             (make-cxxrecord-ctype :key decl-key)
             )
           ))
      (otherwise
       (warn "Add support for classify-ctype ~a" decl)
       (make-maybe-interesting-ctype :description decl-key)
       ))))







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
         :description (cast:get-as-string (cast:desugar tsty))
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


(defmethod classify-ctype ((x cast:paren-type))
  (make-paren-ctype :inner (classify-ctype (cast:get-type-ptr-or-null (cast:get-inner-type x)))))


(defmethod classify-ctype ((x t))
#+gc-warnings(warn "Add support for classify-ctype to recognize ~a~%" x)
  (make-unclassified-ctype :description (format nil "~a" x)))

;;; Convert a clang::Type or clang::QualType to the canonical clang::Type
(defgeneric to-canonical-type (x))
(defmethod to-canonical-type ((x cast:qual-type))
  (to-canonical-type (cast:get-type-ptr-or-null x)))

(defmethod to-canonical-type ((x cast:type))
  (let* ((canonical-qual-type (cast:get-canonical-type-internal x))
         (canonical-type (cast:get-type-ptr-or-null canonical-qual-type)))
    canonical-type))


(defun annotated-class-name (class-node)
  (format nil "~a" (clang-ast:get-qualified-name-as-string class-node)))



(defun check-instance-var-result (instance-vars)
  (let ((v (gethash "core::Ratio_O" instance-vars)))
    (print v)))

(defun gc-builder-c-version (&key test)
  (let* ((*testing* test)
         (json-database-pathname "app:Contents;Resources;buildDatabases;clasp_compile_commands.json" )
         (db (ast-tooling:jsoncompilation-database-load-from-file
              (namestring (probe-file json-database-pathname))))
         (filename-vector (if *testing*
                              (subseq (ast-tooling:get-all-files db) 0 *testing*)
                              (ast-tooling:get-all-files db)))
         (filenames (map 'list #'identity filename-vector)))
    (format t "Running gc-builder-c-version on: ~a~%" filenames)
    (format t "Starting ast-search~%")
    (let ((results (ast-tooling:ast-search '(ast-tooling:garbage-collector-scanner) json-database-pathname filenames)))
      (destructuring-bind (inheritance instance-variables)
          (cdr (assoc 'ast-tooling:garbage-collector-scanner results))
        (check-instance-var-result instance-variables)
        ))))






;;
;; This should match on regular classes or injected template-instantiations of template classes
;; that inherit from GCObject
;;
#|
                Ignore Derivable<...> classes because they are instantiated in
                clbind_wrappers.h:
                template <typename T>
                struct from_object<T*> { ... }    In a dynamic_cast test.
                No instances of Derivable<...> should ever be allocated.
                |#
(defparameter *class-gcobject-derived-matcher*
  '(:record-decl
    (:is-same-or-derived-from
     (:record-decl
      (:has-name "GCObject")))
    (:any-of
     (:record-decl
      (:is-template-instantiation)
      (:has-parent
       (:class-template-decl)))
     (:record-decl
      (:unless (:any-of
                (:has-parent
                 (:class-template-decl))
                (:has-parent
                 (:record-decl))))
      (:is-definition)
      ))))


(defparameter *gcobject-method-submatcher*
  (compile-matcher
   '(:record-decl
     (:for-each ;; -descendant
      (:destructor-decl
;;       (:unless (:is-implicit))
;;       (:isOverride)  ; (:unless (:is-implicit))
       (:bind :method (:method-decl)))))))



(defparameter *gcobject-metadata-submatcher*
  (compile-matcher
   '(:record-decl
     (:for-each ;; -descendant
      (:record-decl
       (:matches-name "metadata_.*")
       (:bind :metadata (:record-decl)))))))



(defparameter *instance-var-submatcher*
  (compile-matcher
   '(:record-decl
     (:is-definition)
     (:for-each ;; -descendant
      (:field-decl
       (:bind :field (:field-decl)))))))



(defparameter *class-gccontainer-derived-matcher*
  '(:record-decl
    (:is-definition)
    (:is-template-instantiation)
    (:is-same-or-derived-from
     (:record-decl
      (:matches-name "GCContainer_moveable")))
    ))






(defun setup-finder-gcobject-info (mtool)
  "Setup the TOOL (multitool) to look for instance variables that we want to garbage collect
and the inheritance hierarchy that the garbage collector will need"
  (symbol-macrolet ((class-results (project-gcobjects (multitool-results mtool))))
    (flet ((%%class-gcobject-derived-matcher-callback ()
             "This function can only be called as a ASTMatcher callback"
             (let* ((class-node (mtag-node :whole))
                    (class-location (mtag-loc-start :whole))
                    (class-name (annotated-class-name class-node))
                    (class-key (record-key class-node)))
               (unless (gethash class-key class-results)
                 (gclog "Adding class name: ~a~%" class-name)
                 (let* ((bases (let (temp-bases)
                                 (ext:map-c++-iterator
                                  (lambda (x)
                                    (let* ((qualtype (cast:get-type x))
                                           (type (cast:get-type-ptr-or-null qualtype))
                                           (base (cast:get-as-cxxrecord-decl type))
                                           (base-name (decl-name base))
                                           )
                                      (gclog "   Base name: ~a~%" base-name)
                                      (push base-name temp-bases)))
                                  (clang-ast:bases-iterator (mtag-node :whole)))
                                 temp-bases
                                 ))
                        (vbases (let (temp-vbases)
                                  (ext:map-c++-iterator
                                   (lambda (x)
                                     (let* ((qualtype (cast:get-type x))
                                            (type (cast:get-type-ptr-or-null qualtype))
                                            (vbase (cast:get-as-cxxrecord-decl type))
                                            (vbase-name (decl-name vbase))
                                            )
                                       (gclog "   VBase name: ~a~%" vbase-name)
                                       (push vbase-name temp-vbases)))
                                   (clang-ast:vbases-iterator (mtag-node :whole)))
                                  temp-vbases
                                  ))
                        (gcobject-subclass (progn
                                             (make-gcobject-subclass :key class-key
                                                                     :name class-name
                                                                     :location class-location
                                                                     :bases bases
                                                                     :vbases vbases))))
                   ;; (format t "Adding class-results for ~a~%" class-name) ;
                   (sub-match-run
                    *instance-var-submatcher*
                    class-node
                    (lambda ()
                      (let* ((whole-node (mtag-node :field))
                             (qualty (clang-ast:get-type whole-node))
                             (type (cast:get-type-ptr-or-null qualty))
                             (classified (progn
                                           (gclog "  Looking at field: ~a type: ~a   typeof ~a~%"
                                                  (cast:get-name whole-node)
                                                  (clang-ast:get-as-string qualty)
                                                  (type-of (clang-ast:get-type-ptr-or-null qualty)))
                                           (let* ((*debug-info* (make-debug-info :name (cast:get-name whole-node)
                                                                                 :location (mtag-loc-start :field)))
                                                  (canonical-type (to-canonical-type type))
                                                  (classified-ctype (classify-ctype canonical-type)))
                                             classified-ctype
                                             )))
                             (instance-var (make-instance-variable
                                            :location (mtag-loc-start :field)
                                            :field-name (cast:get-name whole-node)
                                            :ctype classified)))
                        (pushnew instance-var (gcobject-subclass-instance-variables gcobject-subclass)
                                 :key #'instance-variable-field-name
                                 :test #'(lambda (x y) (string= x y) ))
                        )))
                   (sub-match-run
                    *gcobject-method-submatcher*
                    class-node
                    (lambda ()
                      (let* ((method-node (mtag-node :method))
                             (method-name (mtag-name :method)))
                        (when (not (cast:is-implicit method-node))
                          (setf (gcobject-subclass-has-destructor gcobject-subclass) t))
                        )))
                   (sub-match-run
                    *gcobject-metadata-submatcher*
                    class-node
                    (lambda ()
                      (let* ((metadata-node (mtag-node :metadata))
                             (metadata-name (string-upcase (mtag-name :metadata))))
                        (push (intern metadata-name :keyword) (gcobject-subclass-metadata gcobject-subclass)))))
                   (setf (gethash class-key class-results) gcobject-subclass)
                   )))))
      ;; Initialize the class search
      (multitool-add-matcher mtool
                             :name :gcobjects
                             :matcher (compile-matcher `(:bind :whole ,*class-gcobject-derived-matcher*))
                             :initializer (lambda () (setf class-results (make-hash-table :test #'equal))) ; initializer
                             :callback (make-instance 'code-match-callback :code (function %%class-gcobject-derived-matcher-callback))
                             ))))





(defun setup-finder-gccontainer-info (mtool)
  "Setup the TOOL (multitool) to look for instance variables that we want to garbage collect
and the inheritance hierarchy that the garbage collector will need"
  (symbol-macrolet ((class-results (project-gccontainers (multitool-results mtool))))
    (flet ((%%class-gccontainer-derived-matcher-callback ()
             "This function can only be called as a ASTMatcher callback"
             (let* ((class-node (mtag-node :whole))
                    (class-location (mtag-loc-start :whole))
                    (class-name (annotated-class-name class-node))
                    (class-key (record-key class-node)))
               (unless (gethash class-key class-results)
                 (gclog "Adding class name: ~a~%" class-name)
                 (let* ((bases (let (temp-bases)
                                 (ext:map-c++-iterator
                                  (lambda (x)
                                    (let* ((qualtype (cast:get-type x))
                                           (type (cast:get-type-ptr-or-null qualtype))
                                           (base (cast:get-as-cxxrecord-decl type))
                                           (base-name (decl-name base))
                                           )
                                      (gclog "   Base name: ~a~%" base-name)
                                      (push base-name temp-bases)))
                                  (clang-ast:bases-iterator (mtag-node :whole)))
                                 temp-bases
                                 ))
                        (vbases (let (temp-vbases)
                                  (ext:map-c++-iterator
                                   (lambda (x)
                                     (let* ((qualtype (cast:get-type x))
                                            (type (cast:get-type-ptr-or-null qualtype))
                                            (vbase (cast:get-as-cxxrecord-decl type))
                                            (vbase-name (decl-name vbase))
                                            )
                                       (gclog "   VBase name: ~a~%" vbase-name)
                                       (push vbase-name temp-vbases)))
                                   (clang-ast:vbases-iterator (mtag-node :whole)))
                                  temp-vbases
                                  ))
                        (decl-type (progn
                                     ;;(break "About to classify-decl class-node")
                                 (classify-decl class-node)))
                        (gccontainer (make-gccontainer :key class-key
                                                       :name class-name
                                                       :location class-location
                                                       :decl-type decl-type))
                        )
                   ;; (format t "Adding class-results for ~a~%" class-name) ;
                   (setf (gethash class-key class-results) gccontainer)
                   )))))
      ;; Initialize the class search
      (multitool-add-matcher mtool
                             :name :gccontainers
                             :matcher (compile-matcher `(:bind :whole ,*class-gccontainer-derived-matcher*))
                             :initializer (lambda () (setf class-results (make-hash-table :test #'equal))) ; initializer
                             :callback (make-instance 'code-match-callback :code (function %%class-gccontainer-derived-matcher-callback))
                             ))))



;; ----------------------------------------------------------------------
;;
;; Search for house-keeping classes that contain smart-pointers and
;; set up garbage collector root-scanning functions for them








(defparameter *class-housekeeping-matcher*
  '(:bind :whole
    (:record-decl
     ;; The class must be a definition
     (:is-definition)
     ;; We don't want any classes that inherit from GCObject or GCContainer
     (:unless
         (:is-same-or-derived-from
          (:record-decl
           (:any-of
            (:has-name "GCObject")
            (:has-name "GCContainer")
            ))))
     )))
(compile-matcher *class-housekeeping-matcher*)


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

#||
(defparameter *field-submatcher*
  (compile-matcher '(:record-decl
                     (:for-each ;; -descendant
                      (:field-decl
                       (:bind :field (:field-decl))
                       (:has-type
                        (:record-decl
                         (:bind :type (:record-decl))
                         )))))))
||#
(or *field-submatcher*
    (error "Problem encountered compiling *field-submatcher*"))


(defparameter *method-submatcher*
  (compile-matcher '(:record-decl
                     (:for-each ;; -descendant
                      (:method-decl
                       (:bind :method (:method-decl))
                       (:any-of
                        (:has-name "onHeapScanGCRoots")
                        (:has-name "onStackScanGCRoots")))))))


(defun setup-housekeeping-class-search (mtool)
  (symbol-macrolet ((results (project-housekeeping-classes (multitool-results mtool))))
    (labels ((%%new-housekeeping-class-callback (class-node record-key template-specializer)
             (let ((cname (mtag-name :whole))
                   bases fields
                   on-stack-scan-gc-roots
                   on-heap-scan-gc-roots)
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
                     (push (record-key base-decl) bases))))
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
                                  (cond
                                    ((string= method-name "onHeapScanGCRoots")
                                     (setq on-heap-scan-gc-roots (make-scanner-method
                                                                  :has-body (cast:has-body method-node)
                                                                  :is-this-declaration-a-definition (cast:is-this-declaration-adefinition method-node)
                                                                  :does-this-declaration-have-a-body (cast:does-this-declaration-have-abody method-node)
                                                                  :location location)))
                                    ((string= method-name "onStackScanGCRoots")
                                     (setq on-stack-scan-gc-roots (make-scanner-method
                                                                   :has-body (cast:has-body method-node)
                                                                   :is-this-declaration-a-definition (cast:is-this-declaration-adefinition method-node)
                                                                   :does-this-declaration-have-a-body (cast:does-this-declaration-have-abody method-node)
                                                                   :location location)))
                                    (t (error "I matched a method I shouldn't have - only onStackScanGCRoots or onHeapScanGCRoots should have been matched, but got: ~a" method-name)))
                                  )))
               ;;                   (when (string= record-key "gctools::StackRootedPointer<class asttooling::BAR>") (break "Check fields"))
               (setf (gethash record-key results)
                     (make-housekeeping-class :key record-key
                                              :template-specializer template-specializer
                                              :location (mtag-loc-start :whole)
                                              :has-on-stack-scan-gc-roots on-stack-scan-gc-roots
                                              :has-on-heap-scan-gc-roots on-heap-scan-gc-roots
                                              :bases bases
                                              :fields fields))
               ))
           (%%housekeeping-class-callback ()
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
                   (%%new-housekeeping-class-callback class-node record-key template-specializer))
                 ))))
      (multitool-add-matcher mtool
                             :name :housekeeping-classes
                             :matcher (compile-matcher *class-housekeeping-matcher*)
                             :initializer (lambda () (setf results (make-hash-table :test #'equal)))
                             :callback (make-instance 'code-match-callback :code (function %%housekeeping-class-callback)))
      )))



(defun check-contains-smart-pointers-updated-impl (analysis aclass)
  "If we already have the contains-smart-pointers flag set then return nil - no change needs to be made.
If we don't then check if we contain smart-pointers directly or indirectly and 
set contains-smart-pointers to T if we do and return T"
  (block check
    (dolist (base (housekeeping-class-bases aclass))
      (let* ((base-class (gethash base (project-housekeeping-classes (analysis-project analysis))))
             (contains (when base-class (contains-smart-pointers-p base-class analysis)))
             )
        (when (and base-class contains)
          (setf (analysis-housekeeping-class-contains-smart-pointers-p aclass analysis) t)
          (return-from check contains))))
    (dolist (field (housekeeping-class-fields aclass))
      (let* ((field-type (instance-variable-ctype field))
             (contains (when field-type (contains-smart-pointers-p field-type analysis))))
        (when contains
          (setf (analysis-housekeeping-class-contains-smart-pointers-p aclass analysis) t)
          (return-from check contains))))
    nil))



(defun check-contains-smart-pointers-updated (analysis aclass)
  "If we already have the contains-smart-pointers flag set then return nil - no change needs to be made.
If we don't then check if we contain smart-pointers directly or indirectly and 
set contains-smart-pointers to T if we do and return T"
  (if (analysis-housekeeping-class-contains-smart-pointers-p aclass analysis)
      (return-from check-contains-smart-pointers-updated nil))
  (check-contains-smart-pointers-updated-impl analysis aclass))


     
(defun once-identify-housekeeping-classes-that-contain-smart-pointers (analysis)
  (let #|symbol-macrolet|# ((all-classes (project-housekeeping-classes (analysis-project analysis))))
    (with-all-housekeeping-classes (all-classes)
      (let (updates-seen
            (updates-this-round 0))
        (maphash (lambda (k v)
                   (let ((one-update (check-contains-smart-pointers-updated analysis v)))
                     (when one-update
                       (incf updates-this-round)
                       ;;                     (format t "Class contains smart-pointers: ~a~%" (housekeeping-class-key v))
                       )
                     (setq updates-seen (or updates-seen one-update))
                     ))
                 all-classes)
        (if updates-seen
            updates-this-round
            nil)))))


(defun identify-housekeeping-classes-that-contain-smart-pointers (analysis)
  (loop
     :while (once-identify-housekeeping-classes-that-contain-smart-pointers analysis)))



(defun check-if-on-stack-has-smart-pointers-on-heap-impl (analysis aclass)
  (let #|symbol-macrolet|# ((all-classes (project-housekeeping-classes (analysis-project analysis))))
    (block check
      (dolist (base (housekeeping-class-bases aclass))
        (let ((base-class (gethash base all-classes)))
          (when base-class
            (when (analysis-housekeeping-class-on-stack-has-smart-pointers-on-heap-p base-class analysis)
              (setf (analysis-housekeeping-class-on-stack-has-smart-pointers-on-heap-p aclass analysis) t)
              (return-from check t)))))
      (dolist (field (housekeeping-class-fields aclass))
        (declare (special *debug-info*))
        (let ((*debug-info* field)
              (field-type (instance-variable-ctype field)))
          (when (on-stack-has-smart-pointers-on-heap-p field-type analysis)
            (setf (analysis-housekeeping-class-on-stack-has-smart-pointers-on-heap-p aclass analysis) t)
            (return-from check t))))))
  nil)

(defun check-if-on-stack-has-smart-pointers-on-heap (analysis aclass)
  (block check
    (if (analysis-housekeeping-class-on-stack-has-smart-pointers-on-heap-p aclass analysis)
        (return-from check nil))
    (check-if-on-stack-has-smart-pointers-on-heap-impl analysis aclass)))

(defun once-identify-housekeeping-classes-that-when-on-stack-have-smart-pointers-on-heap (analysis)
  (let #|symbol-macrolet|# ((all-classes (project-housekeeping-classes (analysis-project analysis))))
    (with-all-housekeeping-classes (all-classes)
      (let (updates-seen
            (updates-this-round 0))
        (maphash (lambda (k v)
                   (let ((one-update (check-if-on-stack-has-smart-pointers-on-heap analysis v)))
                     (when one-update
                       (incf updates-this-round)
                       (format t "Class on-stack-has-smart-pointers-on-heap: ~a~%" (housekeeping-class-key v))
                       )
                     (setq updates-seen (or updates-seen one-update))
                     ))
                 all-classes)
        (if updates-seen
            updates-this-round
            nil)))))

(defun identify-housekeeping-classes-that-when-on-stack-have-smart-pointers-on-heap (analysis)
  (loop
     :while (once-identify-housekeeping-classes-that-when-on-stack-have-smart-pointers-on-heap analysis)))


(defun identify-smart-pointer-info (&optional (analysis *analysis*))
  "I'm not sure that I should do this anymore!!!!!!!
I think this is handled by the classify-XXXXX functions for gcobjects, globals, static-locals, locals"
  (warn "identify-smart-pointer-info depreciated!!!!")
  (let ((housekeeping-classes (project-housekeeping-classes (analysis-project analysis))))
    (reset-class-smart-pointer-info analysis)
    (identify-housekeeping-classes-that-contain-smart-pointers analysis)
    (identify-housekeeping-classes-that-when-on-stack-have-smart-pointers-on-heap analysis)))




(defun generate-root-fixer-code (stream aclass analysis all-classes full-name allocation-type template-prototype-only)
  (flet ((%%needs-rooting-p (one-type)
           "Return true if the given ONE-TYPE needs to be rooted"
           (if (eq allocation-type :heap)
               (contains-smart-pointers-p one-type analysis) 
               (on-stack-has-smart-pointers-on-heap-p one-type analysis))))
    (let ((template-specializer (housekeeping-class-template-specializer aclass))
          (bases (housekeeping-class-bases aclass))
          (fields (housekeeping-class-fields aclass)))
      (if template-prototype-only
          (when template-specializer
            (format stream "~a GC_RESULT ~a::~aScanGCRoots(GC_SCAN_ARGS_PROTOTYPE);~%"
                    (if template-specializer "template<>" "")
                    full-name
                    (if (eq allocation-type :stack) "onStack" "onHeap")))
          (progn
            (format stream "~a GC_RESULT ~a::~aScanGCRoots(GC_SCAN_ARGS_PROTOTYPE)"
                    (if template-specializer "template<>" "")
                    full-name
                    (if (eq allocation-type :stack) "onStack" "onHeap"))
            (fresh-line stream)
            (format stream "{~%")
            (format stream "    // Location: ~a~%" (housekeeping-class-location aclass))
            (dolist (base bases)
              (let ((base-class (gethash base all-classes)))
                (if (null base-class)
                    (warn "root-fixer-for-class could not find base ~a" base)
                    (if (%%needs-rooting-p base-class)
                        (progn
                          (fresh-line stream)
                          (format stream "    this->~a::onHeapScanGCRoots(GC_SCAN_ARGS_PASS);" base)
                          (setq final-fresh-line t))
                        (progn
                          (fresh-line stream)
                          (format stream "// No rooting for base: ~a~%" base)
                          )))))
            (fresh-line stream)
            (format stream "#pragma clang diagnostic push~%")
            (format stream "#pragma clang diagnostic ignored \"-Wunused-variable\"~%")
            (when fields
              (format stream "   MPS_SCAN_BEGIN(GC_SCAN_STATE) {~%")
              (dolist (field fields)
                (let ((field-ctype (instance-variable-ctype field)))
                  (if (%%needs-rooting-p field-ctype)
                      (progn
                        (fresh-line stream)
                        (code-for-instance-var stream "this" (instance-variable-field-name field) (instance-variable-ctype field))
                        (setq final-fresh-line t))
                      (progn
                        (fresh-line stream)
                        (format stream "// No rooting for ~a~%" (instance-variable-field-name field)))
                      )))
              (format stream "    } MPS_SCAN_END(GC_SCAN_STATE);~%")
              )
            (format stream "#pragma clang diagnostic pop~%")
            (format stream " return GC_RES_OK;~%")
            ;;      (format stream "/* FF = ~a */" final-fresh-line )
            (when final-fresh-line (fresh-line))
            (format stream "};~%~%")
            )))))

(defun root-fixer-for-class (stream aclass analysis allocation-type &key template-prototype)
  "Construct code to root this class depending on its allocation-type"
  (let ((all-classes (project-housekeeping-classes (analysis-project analysis))))
    (with-all-housekeeping-classes (all-classes)
      (flet ((%%scanner-method-defined-p ()
               (if (eq allocation-type :heap)
                   (if (housekeeping-class-has-on-heap-scan-gc-roots aclass)
                       (values (scanner-method-does-this-declaration-have-a-body (housekeeping-class-has-on-heap-scan-gc-roots aclass))
                               (scanner-method-location (housekeeping-class-has-on-heap-scan-gc-roots aclass)))
                       nil)
                   (if (housekeeping-class-has-on-stack-scan-gc-roots aclass)
                       (values (scanner-method-does-this-declaration-have-a-body (housekeeping-class-has-on-stack-scan-gc-roots aclass))
                               (scanner-method-location (housekeeping-class-has-on-stack-scan-gc-roots aclass))
                               )
                       nil))))
        (let ((full-name (housekeeping-class-key aclass)))
          (multiple-value-bind (defined-p location)
              (%%scanner-method-defined-p)
            (if defined-p
                (format stream "//~%// scanner already defined for ~a::~aScanGCRoots~%// at: ~a~%//~%~%" full-name (if (eq allocation-type :stack) "onStack" "onHeap") location)
                (generate-root-fixer-code stream aclass analysis all-classes full-name allocation-type template-prototype))
            ))))))


(defun generate-code-for-housekeeping-classes (stream analysis)
  (format stream "#if defined(HOUSEKEEPING_SCANNERS)~%")
  (let #|symbol-macrolet|# ((classes (project-housekeeping-classes (analysis-project analysis))))
    ;; First generate the prototypes
    (maphash (lambda (key class)
               (when (gethash key (analysis-housekeeping-class-stored-on-heap analysis))
                 (root-fixer-for-class stream class analysis :heap :template-prototype t))
               (when (gethash key (analysis-housekeeping-class-stored-on-stack analysis))
                 (root-fixer-for-class stream class analysis :stack :template-prototype t)))
             classes)
    ;; Then the bodies
    (maphash (lambda (key class)
               (when (gethash key (analysis-housekeeping-class-stored-on-heap analysis))
                 (root-fixer-for-class stream class analysis :heap ))
               (when (gethash key (analysis-housekeeping-class-stored-on-stack analysis))
                 (root-fixer-for-class stream class analysis :stack )))
             classes)
    )
  (format stream "#endif // HOUSEKEEPING_SCANNERS~%"))




(defun generate-code-for-global-variables (stream analysis)
  (with-all-housekeeping-classes ((project-housekeeping-classes (analysis-project analysis)))
    (format stream "#if defined(CODE_FOR_GLOBAL_VARIABLES)~%")
    (maphash (lambda (k v)
               (let ((fix (contains-smart-pointers-p v analysis)))
                 (when fix (format stream "    ~a(~a); // ~a ~%" (macro-name v fix) (global-variable-name v) v))))
             (project-global-variables (analysis-project analysis)))
    (format stream "#endif // ifdef GLOBAL_VARIABLES~%")
    ))








;; -----------------------------------------------------------------
;;
;; Search for globals and locals





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







(defparameter *new-gcobject-matcher*
  '(:bind :whole
    (:new-expr
     (:has-type
      (:points-to
       (:record-decl
        (:is-same-or-derived-from
         (:record-decl
          (:matches-name "GCObject"))))))
     )))

(compile-matcher *new-gcobject-matcher*)






(defun setup-new-gcobject-search (mtool)
  (symbol-macrolet ((new-gcobject-exprs (project-new-gcobject-exprs (multitool-results mtool))))
    (flet ((%%new-callback ()
             (block matcher
               (gclog "VARIABLE MATCH: ------------------~%")
               (gclog "    Start:~%~a~%" (mtag-loc-start :whole))
               (let* ((new-expr-node (mtag-node :whole))
                      (location (mtag-loc-start :whole))
                      (qtype (cast:get-type new-expr-node))
                      (type (cast:get-type-ptr-or-null qtype))
                      (classified-type (classify-ctype (to-canonical-type type)))
                      (key (format nil "new ~a()@~a"
                                   (cast:get-as-string qtype)
                                   location))
                                   
                      )
                 (setf (gethash key new-gcobject-exprs)
                       (make-new-gcobject-expr :ctype classified-type
                                               :num-placement-args (cast:get-num-placement-args new-expr-node)
                                               :location location))
                 ))))
      (multitool-add-matcher mtool
                             :name :new-gcobjects
                             :matcher (compile-matcher *new-gcobject-matcher*)
                             :initializer (lambda ()
                                            (setf new-gcobject-exprs (make-hash-table :test #'equal)))
                             :callback (make-instance 'code-match-callback
                                                      :code (function %%new-callback)))
      )))




(defparameter *new-housekeeping-class-matcher*
  '(:bind :whole
    (:new-expr
     (:has-type
      (:points-to
       (:record-decl
        (:is-same-or-derived-from
         (:record-decl
          (:unless
              (:matches-name "GCObject"))))))))))

(compile-matcher *new-housekeeping-class-matcher*)


(defun setup-new-housekeeping-class-search (mtool)
  (symbol-macrolet ((new-housekeeping-class-exprs (project-new-housekeeping-class-exprs (multitool-results mtool))))
    (flet ((%%new-callback ()
             (block matcher
               (gclog "    Start:~%~a~%" (mtag-loc-start :whole))
               (let* ((new-expr-node (mtag-node :whole))
                      (location (mtag-loc-start :whole))
                      (qtype (cast:get-type new-expr-node))
                      (type (cast:get-type-ptr-or-null qtype))
                      (classified-type (classify-ctype (to-canonical-type type)))
                      (key (format nil "new ~a()@~a"
                                   (cast:get-as-string qtype)
                                   location))
                                   
                      )
                 (setf (gethash key new-housekeeping-class-exprs)
                       (make-new-housekeeping-class-expr :ctype classified-type
                                                         :num-placement-args (cast:get-num-placement-args new-expr-node)
                                                         :location location))
                 ))))
      (multitool-add-matcher mtool
                             :name :new-housekeeping-classes
                             :matcher (compile-matcher *new-housekeeping-class-matcher*)
                             :initializer (lambda ()
                                            (setf new-housekeeping-class-exprs (make-hash-table :test #'equal)))
                             :callback (make-instance 'code-match-callback
                                                      :code (function %%new-callback)))
      )))






(defun generate-code-for-static-variables (stream analysis)
  (let #|symbol-macrolet|# ((project (analysis-project analysis)))
    (with-all-housekeeping-classes ((project-housekeeping-classes project))
      (format stream "#if defined(GC_STATIC_LOCAL_VARIABLES)~%")
      (maphash (lambda (k v)
                 (unless (builtin-ctype-p (variable-ctype v))
                   (format stream "STATIC_LOCAL_VARIABLE(~a); // ~a ~a~%"
                           (variable-name v)
                           (variable-location v)
                           (variable-ctype v))))
               (project-static-local-variables project))
      (format stream "#endif // ifdef GC_STATIC_LOCAL_VARIABLES~%")
      )))


(defun problematic-local-variable-ctype-p (vt analysis)
  (on-stack-has-smart-pointers-on-heap-p vt analysis))

(defgeneric rooted-local-variable-ctype-p (type analysis))
(defmethod rooted-local-variable-ctype-p ((type t) analysis)
  nil)

(defmethod rooted-local-variable-ctype-p ((type record-ctype) analysis)
  (rooted-local-variable-ctype-p (gethash (record-ctype-key type) (project-housekeeping-classes (analysis-project analysis))) analysis))

(defmethod rooted-local-variable-ctype-p ((type housekeeping-class) analysis)
  (or (is-or-inherits-from-stack-root type (project-housekeeping-classes (analysis-project analysis)))
      nil))

(defmethod rooted-local-variable-ctype-p ((type rooted-gcholder) analysis) t)


(defun gcsafe-local-variable-p (var analysis)
  (or (rooted-local-variable-ctype-p (variable-ctype var) analysis)
      (search "_gc_safe" (variable-name var))))

(defun generate-code-for-local-variables (stream analysis)
  (let #|symbol-macrolet|# ((project (analysis-project analysis)))
    (with-all-housekeeping-classes ((project-housekeeping-classes project))
      (format stream "#if defined(GC_LOCAL_VARIABLES_DANGEROUS_UNROOTED)~%")
      (maphash (lambda (k v &aux (vt (variable-ctype v)))
                 (let ((problem (problematic-local-variable-ctype-p vt analysis))
                       (safe (gcsafe-local-variable-p v analysis) ))
                   (when (and problem (not safe))
                     (format stream "#error \"Dangerous local variable: ~a\"  // safe(~a) ~a ~a ~a~%"
                             k ;; (variable-name v)
                             safe
                             problem
                             vt
                             (variable-location v)
                             ))))
               (project-local-variables project))
      (format stream "#endif // ifdef GC_LOCAL_VARIABLES_DANGEROUS_UNROOTED~%")
      (format stream "#if defined(GC_LOCAL_VARIABLES_SAFE_ROOTED)~%")
      (maphash (lambda (k v &aux (vt (variable-ctype v)))
                 (let ((problem (problematic-local-variable-ctype-p vt analysis))
                       (safe (gcsafe-local-variable-p v analysis) ))
                   (when (and problem safe)
                     (format stream "GC_SAFE_LOCAL_VARIABLE(~a); // ~a ~a ~a~%"
                             (variable-name v)
                             problem
                             vt
                             (variable-location v)
                             ))))
               (project-local-variables project))
      (format stream "#endif // ifdef GC_LOCAL_VARIABLES_SAFE_ROOTED~%")
#||
      (format stream "#if defined(GC_LOCAL_VARIABLES_SHOULD_BE_SAFE)~%")
      (maphash (lambda (k v &aux (var (variable-ctype v)))
                 (when (not (problematic-local-variable-ctype-p var analysis))
                   (format stream "GC_SAFE_LOCAL_VARIABLE(~a); // ~a ~a~%"
                           (variable-name v)
                           var
                           (variable-location v)
                           )))
               (project-local-variables project))
      (format stream "#endif // ifdef GC_LOCAL_VARIABLES_IMPORTANT~%")
||#
      )))





(defun generate-code-for-new-gcobject-exprs (stream analysis)
  (let* ((project (analysis-project analysis))
         (new-gcobject-exprs (project-new-gcobject-exprs project))
         )
    (format stream "#if defined(GC_DANGEROUS_NEW_GCOBJECT_EXPRS)~%")
    (maphash (lambda (k v )
               (when (eql (new-gcobject-expr-num-placement-args v) 0)
                 (format stream "#error \"DANGEROUS INVOCATION OF NEW ON GCOBJECT: ~a\" // ~a~%" k v)))
             new-gcobject-exprs)
    (format stream "#endif // ifdef GC_DANGEROUS_NEW_GCOBJECT_EXPRS~%")
    (format stream "#if defined(GC_SAFE_NEW_GCOBJECT_EXPRS)~%")
    (maphash (lambda (k v )
               (unless (eql (new-gcobject-expr-num-placement-args v) 0)
                 (format stream "GC_SAFE_PLACEMENT_NEW_GCOBJECT(~a); // ~a~%" k v)))
             new-gcobject-exprs)
    (format stream "#endif // ifdef GC_SAFE_NEW_GCOBJECT_EXPRS~%")))


(defun generate-code-for-new-housekeeping-class-exprs (stream analysis)
  (let* ((project (analysis-project analysis))
         (new-housekeeping-class-exprs (project-new-housekeeping-class-exprs project))
         )
    (format stream "#if defined(GC_DANGEROUS_NEW_GCOBJECT_EXPRS)~%")
    (maphash (lambda (k v )
               (when (eql (new-housekeeping-class-expr-num-placement-args v) 0)
                 (format stream "#error \"DANGEROUS INVOCATION OF NEW ON GCOBJECT: ~a\" // ~a~%" k v)))
             new-housekeeping-class-exprs)
    (format stream "#endif // ifdef GC_DANGEROUS_NEW_GCOBJECT_EXPRS~%")
    (format stream "#if defined(GC_SAFE_NEW_GCOBJECT_EXPRS)~%")
    (maphash (lambda (k v )
               (unless (eql (new-housekeeping-class-expr-num-placement-args v) 0)
                 (format stream "GC_SAFE_PLACEMENT_NEW_GCOBJECT(~a); // ~a~%" k v)))
             new-housekeeping-class-exprs)
    (format stream "#endif // ifdef GC_SAFE_NEW_GCOBJECT_EXPRS~%")))

      





;; ----------------------------------------------------------------------
;;
;; Setup the *tools* that will run over all of the source code and run
;; several matchers that scrape the C++ AST for info requires to build
;; garbage collection scanners.


(load-compilation-database "app:Contents;Resources;buildDatabases;clasp_compile_commands.json")

(defvar *tools* nil)
(progn
  (setf *tools* (make-multitool))
  (setup-finder-gcobject-info *tools*)
  (setup-finder-gccontainer-info *tools*)
  (setup-housekeeping-class-search *tools*)
  (setup-global-variable-search *tools*)
  (setup-variable-search *tools*)
  (setup-new-gcobject-search *tools*)
  (setup-new-housekeeping-class-search *tools*))




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
                                              (reverse (lremove (lremove $* ".*mps\.c$") ".*gc_interface\.cc$")))
                       :arguments-adjuster-code (lambda (args) (concatenate 'vector #-quiet args #+quiet(remove "-v" args)
                                                                            #("-DUSE_MPS"
                                                                              "-DRUNNING_GC_BUILDER"
                                                                              "-resource-dir"
                                                                              "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr//lib/clang/5.1"))))
  ;; Extract the results for easy access
  (setq *project* (multitool-results *tools*))
  (save-project)
  )



(defparameter *max-parallel-searches* 4)

(defun project-pathname (project-name project-type)
  (make-pathname :name project-name :type project-type :defaults (main-directory-pathname)))

(defun *parallel-search-pids* nil)
(defun parallel-search-all (&key test)
  (setq *parallel-search-pids* nil)
  (let ((all-jobs (if test
                      (subseq $* 0 test)
                      (reverse (lremove (lremove $* ".*mps\.c$") ".*gc_interface\.cc$")))))
    (format t "all-jobs: ~a~%" all-jobs)
    (let ((split-jobs (split-list all-jobs *max-parallel-searches*)))
      (dotimes (proc *max-parallel-searches*)
        (core:system "sleep 1")
        (let* ((job-list (elt split-jobs proc))
               (pid (core:fork)))
          (if (eql 0 pid)
              (with-open-file (*standard-output* (project-pathname (format nil "project~a" proc) "log")
                                                 :direction :output :if-exists :supersede)
                (format t "Running search on: ~a~%" 
                        (setf (multitool-results *tools*) (make-project))
                        (batch-run-multitool *tools* :filenames job-list
                                             :arguments-adjuster-code (lambda (args)
                                                                        (concatenate 'vector #-quiet args #+quiet(remove "-v" args)
                                                                                     #("-DUSE_MPS"
                                                                                       "-DRUNNING_GC_BUILDER"
                                                                                       "-resource-dir"
                                                                                       "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr//lib/clang/5.1"))))
                        (serialize:save-archive (multitool-results *tools*) (project-pathname (format nil "project~a" proc) "dat"))
                        (core:exit)))
              (push pid *parallel-search-pids*)
              )))))
  (format t "Started parallel-search-all   pids: ~a~%" *parallel-search-pids*)
  )


(defun parallel-merge (&optional (num-jobs *max-parallel-searches*))
  "Merge the analyses generated from the parallel search"
  (let ((merged (make-project)))
    (dotimes (proc num-jobs)
      (format t "Loading project for job ~a~%" proc)
      (let ((one (serialize:load-archive (project-pathname (format nil "project~a" proc) "dat"))))
        (format t "     merging...~%")
        (merge-projects merged one)))
    (setq *project* merged)
    (save-project)
    merged))

(defun save-project ()
  (serialize:save-archive *project* (project-pathname "project" "dat")))

(defun load-project ()
  (setq *project* (serialize:load-archive (project-pathname "project" "dat")))
  )
        

(defun find-gcobject-subclass-matches-name (name &optional (project *project*))
  (maphash (lambda (k v) (when (search name k) (print k) (print v))) (project-gcobjects project)))

(defun find-housekeeping-class-matches-name (name &optional (project *project*))
  (maphash (lambda (k v) (when (search name k) (print k) (print v))) (project-housekeeping-classes project)))


(defun classify-gcobjects (&optional (analysis *analysis*))
  (maphash (lambda (key gcclass)
             (gclog "classify-gcobjects gcclass: ~a~%" gcclass)
             (mapc (lambda (v)
                     (let ((contains (contains-smart-pointers-p (instance-variable-ctype v) analysis)))
                       (gclog "    ---> contains-smart-pointers-p: ~a  ~a ~a~%" contains v (instance-variable-ctype v))))
                   (gcobject-subclass-instance-variables gcclass))
             )
           (project-gcobjects (analysis-project analysis))))

(defun classify-globals (&optional (analysis *analysis*))
  (let #|symbol-macrolet|# ((table (analysis-global-variables-with-smart-pointers analysis)))
    (clrhash table)
    (maphash (lambda (k v)
               (let ((status (contains-smart-pointers-p (variable-ctype v) analysis)))
                 (cond
                   ;;                 (((eq status :maybe))
                   (status
                    (setf (gethash k table) v))
                   (t nil))))
             (project-global-variables (analysis-project analysis)))))

(defun classify-static-locals (&optional (analysis *analysis*))
  (let #|symbol-macrolet|# ((table (analysis-static-local-variables-with-smart-pointers analysis)))
    (clrhash table)
    (maphash (lambda (k v)
               (let ((status (contains-smart-pointers-p (variable-ctype v) analysis)))
                 (cond
                   ;;                 (((eq status :maybe))
                   (status
                    (setf (gethash k table) v))
                   (t nil))))
             (project-static-local-variables (analysis-project analysis)))))

  
(defun classify-locals (&optional (analysis *analysis*))
  (let ((table (analysis-local-variables-with-smart-pointers-on-heap analysis)))
    (clrhash table)
    (maphash (lambda (k v)
               (let ((status (on-stack-has-smart-pointers-on-heap-p (variable-ctype v) analysis)))
                 (cond
                   ;;                 (((eq status :maybe))
                   (status
                    (setf (gethash k table) v))
                   (t nil))))
             (project-local-variables (analysis-project analysis)))))


#||
(defun classify-housekeeping-class-inheritance (&optional (analysis *analysis*))
  (let ((inherits-from-heap-root (analysis-housekeeping-class-inherits-from-heap-root analysis))
        (inherits-from-stack-root (analysis-housekeeping-class-inherits-from-stack-root analysis)))
    (clrhash inherits-from-heap-root)
    (clrhash inherits-from-stack-root)
    (maphash (lambda (key class)
               (when (is-or-inherits-from-heap-root class)
                 (setf (gethash key inherits-from-heap-root) class))
               (when (is-or-inherits-from-stack-root class)
                 (setf (gethash key inherits-from-stack-root) class)))
             (project-housekeeping-classes (analysis-project analysis)))))
||#




(defun describe-problematic-housekeeping-classes (&optional (stream t) (analysis *analysis*))
  (let ((on-stack-ht (analysis-housekeeping-class-stored-on-stack analysis))
        (on-heap-ht (analysis-housekeeping-class-stored-on-heap analysis))
        (all-classes (project-housekeeping-classes (analysis-project analysis))))
    (maphash (lambda (k class)
               (when (and (gethash k on-stack-ht) (analysis-housekeeping-class-on-stack-has-smart-pointers-on-heap-p class analysis)
                          #| Check if it's already StackRooted |# )
                 (if (is-or-inherits-from-stack-root class all-classes)
                     (format stream "Allocated on stack ~a has smart pointers on heap ---- Inherits from StackRoot~%" k)
                     (format stream "Allocated on stack ~a has smart pointers on heap~%" k)))
               (when (and (gethash k on-heap-ht) (analysis-housekeeping-class-contains-smart-pointers-p class analysis))
                 (if (is-or-inherits-from-heap-root class all-classes)
                     (format stream "Allocated on heap ~a has smart pointers ---- Inherits from HeapRoot~%" k)
                     (format stream "Allocated on heap ~a has smart pointers~%" k)))
               )
             (project-housekeeping-classes (analysis-project analysis)))))


;; ------------------------------------------------------------
;; ------------------------------------------------------------
;; ------------------------------------------------------------
;;
;; Analyze information
;;
;; ------------------------------------------------------------
;; ------------------------------------------------------------
;; ------------------------------------------------------------

(defstruct analysis
  project
  manager  ;; gckind-manager
  list-of-all-gcenums
  (metadata-to-gcclass (make-hash-table :test #'eq)) ;; (map metadata (list of gcclasses with metadata))
  (gckind-to-species (make-hash-table :test #'eq)) ;; get species for each gc-object
  (species-to-gcenum (make-hash-table :test #'eq)) ;; Each species has a list of gcobjects
  (housekeeping-class-on-stack-has-smart-pointers-on-heap (make-hash-table :test #'equal))
  (housekeeping-class-contains-smart-pointers (make-hash-table :test #'equal))
  (housekeeping-class-stored-on-heap (make-hash-table :test #'equal))
  (housekeeping-class-stored-on-stack (make-hash-table :test #'equal))
  (global-variables-with-smart-pointers (make-hash-table :test #'equal))
  (static-local-variables-with-smart-pointers (make-hash-table :test #'equal))
  (local-variables-with-smart-pointers-on-heap (make-hash-table :test #'equal))
  )

(defmethod print-object ((object analysis) stream)
  (format stream "#S(ANALYSIS ...)"))

(defun compact-enum-value (species-num kind-num)
  (logior (ash species-num 16) kind-num))

(defun list-of-all-gcenums (analysis)
  (let (all)
    (mapc (lambda (species)
            (let ((gcenums (gethash species (analysis-species-to-gcenum analysis))))
              (when gcenums
                (mapc (lambda (e)
                        (push e all))
                      gcenums))
              )) (gckind-manager-species (analysis-manager analysis)))
    (sort all (lambda (a b) (< (compact-enum-value (gcenum-species-num a) (gcenum-kind-num a))
                               (compact-enum-value (gcenum-species-num b) (gcenum-kind-num b)))))))

(defstruct gcenum
  gckind
  species
  enum-name
  species-num
  kind-num
  )

  (defstruct species
    name
    discriminator ;; Function - takes one argument, returns a species index
    scan ;; Function - generates scanner for species
    skip ;; Function - generates obj_skip code for species
    finalize ;; Function - generates obj_finalize code for species
    index
    )
  
  (defstruct gckind-manager
    (species nil) ;; a single argument lambda that returns an integer index
    (next-species-counter 0)
    ignore-discriminator
    )

(defun lookup-species (manager name)
  (dolist (sp (gckind-manager-species manager))
    (when (eq name (species-name sp))
      (return-from lookup-species sp)))
  (error "Could not find species ~a in manager" name))

(defun add-species (manager species)
  (setf (species-index species) (gckind-manager-next-species-counter manager))
  (incf (gckind-manager-next-species-counter manager))
  (push species (gckind-manager-species manager))
  species)


(defun identify-species (manager aclass)
  (let (hits)
    (dolist (species (gckind-manager-species manager))
      (when (funcall (species-discriminator species) aclass)
        (push species hits)))
    (cond
      ((> (length hits) 1)
       (error "The class ~a could not be uniquely distinguished between the species: ~a" aclass hits))
      ((eql (length hits) 1)
       (car hits))
      ((and
        (gckind-manager-ignore-discriminator manager)
        (funcall (gckind-manager-ignore-discriminator manager) aclass))
       nil)
      (t (error " Could not identify species for ~a - and it's not recognized by the gckind-managers ignore-discriminator" aclass)
         nil))
    ))
         
        

(defun assign-gcobject-species (gco species anal &optional kind-num)
  (unless kind-num (setq kind-num (length (gethash species (analysis-species-to-gcenum anal) nil))))
  (let* ((manager (analysis-manager anal))
         (species-num (species-index species))
         (key (gckind-key gco))
         (gco-info (make-gcenum :gckind gco
                                :species species
                                :enum-name (class-enum-name key species)
                                :species-num species-num
                                :kind-num kind-num ))
         )
    (setf (gethash gco (analysis-gckind-to-species anal)) species) ; what species does a gco belong to
    (push gco-info (gethash species (analysis-species-to-gcenum anal)))
         ))



(defun analyze-gcobject-and-assign-species (gco anal)
  (let* ((manager (analysis-manager anal))
         (species (identify-species manager gco)))
    (when species
        (assign-gcobject-species gco species anal))))

(defun sort-gcenums-by-index (gcenums)
  (sort (copy-list gcenums) #'(lambda (a b) (< (gcenum-kind-num a) (gcenum-kind-num b)))))


(defun scanner-for-system (fout species anal)
  (let ((gcenums (sort-gcenums-by-index (gethash species (analysis-species-to-gcenum anal)))))
    (dolist (gce gcenums)
      (let ((gckind (gcenum-gckind gce)))
        (format fout "case ~a: {~%" (gcenum-enum-name gce))
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
  (let ((gcenums (sort-gcenums-by-index (gethash species (analysis-species-to-gcenum anal)))))
    (dolist (gce gcenums)
      (let ((gckind (gcenum-gckind gce)))
        (format fout "case ~a: {~%" (gcenum-enum-name gce))
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
  (let ((gcenums (sort-gcenums-by-index (gethash species (analysis-species-to-gcenum anal)))))
    (dolist (gce gcenums)
      (let ((gckind (gcenum-gckind gce)))
        (format fout "case ~a: {~%" (gcenum-enum-name gce))
        (format fout "    THROW_HARD_ERROR(BF(\"~a should never be finalized\"));~%" (gckind-key gckind)))
      (format fout "    break;~%}~%"))))
        


(defun scanner-for-one-gcobject (fout classid species anal)
  (let* ((enum-name (class-enum-name classid species))
         (inheritance (project-gcobjects (analysis-project anal)))
         (all-instance-variables (gather-instance-variables classid inheritance)))
    (gclog "build-mps-scan-for-one-family -> inheritance classid[~a]  value[~a]~%" (ctype-name classid) value)
    (format fout "case ~a: {~%" enum-name)
    (format fout "    ~A* ~A = reinterpret_cast<~A*>(obj_ptr);~%" (ctype-name classid) +ptr-name+ (ctype-name classid))
    (maphash #'(lambda (k v &aux (csp (contains-smart-pointers-p (instance-variable-ctype v) anal)))
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


(defun scanner-for-gcobjects (fout species anal)
  (let ((gcenums (sort-gcenums-by-index (gethash species (analysis-species-to-gcenum anal)))))
    (dolist (gcenum gcenums)
      (let* ((entry (gcenum-gckind gcenum))
             (classid (gckind-key entry)))
        (scanner-for-one-gcobject fout classid species anal)))))


(defun skipper-for-gcobjects (fout species anal)
  (let ((gcenums (sort-gcenums-by-index (gethash species (analysis-species-to-gcenum anal)))))
    (dolist (gcenum gcenums)
      (let* ((entry (gcenum-gckind gcenum))
             (classid (gckind-key entry))
             (enum-name (class-enum-name classid species)))
        (gclog "skipper-for-gcobjects -> inheritance classid[~a]  value[~a]~%" (ctype-name classid) value)
        (format fout "case ~a: {~%" enum-name)
        (format fout "    typedef ~A type_~A;~%" (ctype-name classid) enum-name)
        (format fout "    base = (char*)base + ALIGN(SIZEOF_WITH_HEADER(type_~A));~%" enum-name)
        (format fout "} break;~%")))
    ))


(defun finalizer-for-gcobjects (fout species anal)
  (let ((gcenums (sort-gcenums-by-index (gethash species (analysis-species-to-gcenum anal)))))
    (dolist (gcenum gcenums)
      (let* ((entry (gcenum-gckind gcenum))
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
      (format t "Processing ~a entries~%" (length (gethash gcvector-species (analysis-species-to-gcenum anal))))
;;      (scanner-for-gcvector fout gcvector-species anal)
      )))


(defun scanner-for-gccontainer (fout species anal)
  (let ((gcenums (sort-gcenums-by-index (gethash species (analysis-species-to-gcenum anal)))))
    (dolist (gcenum gcenums)
      (let* ((entry (gcenum-gckind gcenum))
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
                 
              #|        (maphash #'(lambda (k v &aux (csp (contains-smart-pointers-p (instance-variable-ctype v) anal)))
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
  (let ((gcenums (sort-gcenums-by-index (gethash species (analysis-species-to-gcenum anal)))))
    (dolist (gcenum gcenums)
      (let* ((entry (gcenum-gckind gcenum))
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
  (let ((gcenums (sort-gcenums-by-index (gethash species (analysis-species-to-gcenum anal)))))
    (dolist (gcenum gcenums)
      (let* ((entry (gcenum-gckind gcenum))
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
  (let* ((manager (make-gckind-manager)))
    (add-species manager (make-species :name :system
                                       :discriminator (lambda (x) (and (stringp x) (search "system_" x)))
                                       :scan 'scanner-for-system
                                       :skip 'skipper-for-system
                                       :finalize 'finalizer-for-system
                                       ))
    (add-species manager (make-species :name :bootstrap
                                       :discriminator (lambda (x) (and (gcobject-subclass-p x)
                                                                       (gctools::bootstrap-kind-p (gcclass-key x))))
                                       :scan 'scanner-for-gcobjects
                                       :skip 'skipper-for-gcobjects
                                       :finalize 'finalizer-for-gcobjects
                                       ))
    (add-species manager (make-species :name :gcobject
                                       :discriminator (lambda (x) (and (gcobject-subclass-p x)
                                                                       (not (gctools:bootstrap-kind-p (gcclass-key x)))
                                                                       (not (string= "clbind::Wrapper" (gcobject-subclass-name x)))
                                                                       (not (string= "clbind::Iterator" (gcobject-subclass-name x)))
                                                                       ))
                                       :scan 'scanner-for-gcobjects
                                       :skip 'skipper-for-gcobjects
                                       :finalize 'finalizer-for-gcobjects
                                       ))
    (add-species manager (make-species :name :WRAPPER
                                       :discriminator (lambda (x) (progn
                                                                    (and (gcobject-subclass-p x)
                                                                         (string= "clbind::Wrapper" (gcobject-subclass-name x)))))
                                       :scan 'scanner-for-gcobjects
                                       :skip 'skipper-for-gcobjects
                                       :finalize 'finalizer-for-gcobjects
                                       ))
    (add-species manager (make-species :name :ITERATOR
                                       :discriminator (lambda (x) (and (gcobject-subclass-p x)
                                                                       (string= "clbind::Iterator" (gcobject-subclass-name x))))
                                       :scan 'scanner-for-gcobjects
                                       :skip 'skipper-for-gcobjects
                                       :finalize 'finalizer-for-gcobjects
                                       ))
    (add-species manager (make-species :name :GCVECTOR
                                       :discriminator (lambda (x) (and (gccontainer-p x)
                                                                       (search "gctools::GCVector" (gccontainer-key x))))
                                       :scan 'scanner-for-gccontainer
                                       :skip 'skipper-for-gccontainer
                                       :finalize 'finalizer-for-gccontainer
                                       ))
    (add-species manager (make-species :name :GCARRAY
                                       :discriminator (lambda (x) (and (gccontainer-p x)
                                                                       (search "gctools::GCArray" (gccontainer-key x))))
                                       :scan 'scanner-for-gccontainer
                                       :skip 'skipper-for-gccontainer
                                       :finalize 'finalizer-for-gccontainer
                                       ))
    (setf (gckind-manager-ignore-discriminator manager) #'(lambda (x)
                                                            (or
                                                             (and (gccontainer-p x)
                                                                 (or
                                                                  (string= (gccontainer-key x) "gctools::GCContainer")
                                                                  ))
                                                             (and (gcobject-subclass-p x)
                                                                 (or
                                                                  (string= (gcobject-subclass-name x) "clbind::Wrapper")
                                                                  (string= (gcobject-subclass-name x) "clbind::Iterator")
                                                                  ))
                                                             )))
    manager))




(defun sorted-species (analysis)
  (let (species)
    (maphash (lambda (spec gcenums) (push spec species)) (analysis-species-to-gcenum analysis))
    (sort species #'(lambda (a b) (< (species-index a) (species-index b))))))




(defun build-mps-scan (fout anal)
  (let* ((sorted-species (sorted-species anal)))
    (dolist (species sorted-species)
      (funcall (species-scan species) fout species anal))))


(defun build-mps-skip (fout anal)
  (let* ((sorted-species (sorted-species anal)))
    (dolist (species sorted-species)
      (funcall (species-skip species) fout species anal))))


(defun build-mps-finalize (fout anal)
  (let* ((sorted-species (sorted-species anal)))
    (dolist (species sorted-species)
      (funcall (species-finalize species) fout species anal))))














(defun setup-system-species (analysis)
  "The MPS system requires a few GCKind enum's to function - put them 
in the system-species and assign them a GCKind value"
  (let ((system-species (lookup-species (analysis-manager analysis) :system))
        (bootstrap-species (lookup-species (analysis-manager analysis) :bootstrap)))
    (assign-gcobject-species (make-gckind :key "fwd" :name "fwd") system-species analysis 1)
    (assign-gcobject-species (make-gckind :key "fwd2" :name "fwd2") system-species analysis 2)
    (assign-gcobject-species (make-gckind :key "pad1" :name "pad1") system-species analysis 3)
    (assign-gcobject-species (make-gckind :key "pad" :name "pad") system-species analysis 4)
    ))


(defun organize-gcclasses-into-species-and-assign-kinds (analysis &aux (project (analysis-project analysis)))
  "Every GCObject and GCContainer is assigned to a species and given a GCKind enum value."
  (let ((project (analysis-project analysis)))
    (maphash (lambda (k v) (analyze-gcobject-and-assign-species v analysis)) (project-gcobjects project))
    (maphash (lambda (k v) (analyze-gcobject-and-assign-species v analysis)) (project-gccontainers project))
    ))
  





(defparameter *analysis* nil)
(defun analyze-project (&optional (project *project*))
  (setq *analysis* (make-analysis :project project
                                  :manager (setup-manager)))
  (setup-system-species *analysis*)
  (organize-gcclasses-into-species-and-assign-kinds *analysis*)
  (create-metadata-to-class-map *analysis*)
;;  (identify-smart-pointer-info *analysis*)
;;  (classify-housekeeping-class-inheritance *analysis*)
  (let ((*track-reachability* t)
        (list-of-all-gcenums (list-of-all-gcenums *analysis*)))
    (setf (analysis-list-of-all-gcenums *analysis*) list-of-all-gcenums)
    (clrhash (analysis-housekeeping-class-stored-on-heap *analysis*))
    (clrhash (analysis-housekeeping-class-stored-on-stack *analysis*))
    (classify-gcobjects *analysis*)
    (classify-globals *analysis*)
    (classify-static-locals *analysis*)
    (classify-locals *analysis*)
    )
;;  (describe-problematic-housekeeping-classes)
;;  (describe-problematic-local-variables)
)















(defun generate-code (&optional (analysis *analysis*))
  (unless analysis (error "Run analyze-project first"))
  ;;    (identify-smart-pointer-info analysis)
  (with-open-file (fout (make-pathname :name "clasp_gc" :type "cc" :defaults (main-directory-pathname)) :direction :output :if-exists :supersede)
    (generate-code-for-non-moving-classes fout (gethash :METADATA_GC_DO_NOT_MOVE (analysis-metadata-to-gcclass analysis)))
    (format fout "#if defined(GC_ENUM)~%")
    (generate-kind-enum fout analysis)
    (format fout "#endif // defined(GC_ENUM)~%")
    (format fout "#if defined(SETUP_KIND)~%")
    (generate-code-initialize-kind-for-species fout '(:bootstrap :gcobject)  analysis)
    (format fout "#endif // SETUP_KIND~%")
    (format fout "#if defined(GC_KIND_SELECTORS)~%")
    (generate-gc-info fout analysis)
    (format fout "#endif // defined(GC_KIND_SELECTORS)~%")
    (format fout "#if defined(GC_KIND_NAME_MAP)~%")
    (generate-kind-name-map fout analysis)
    (format fout "#endif // defined(GC_KIND_NAME_MAP)~%")
    (format fout "#if defined(GC_SCAN_METHOD)~%")
    (build-mps-scan fout analysis)
    (format fout "#endif // defined(GC_SCAN_METHOD)~%")
    (format fout "#if defined(GC_SKIP_METHOD)~%")
    (build-mps-skip fout analysis)
    (format fout "#endif // defined(GC_SKIP_METHOD)~%")
    (format fout "#if defined(GC_FINALIZE_METHOD)~%")
    (build-mps-finalize fout analysis)
    (format fout "#endif // defined(GC_FINALIZE_METHOD)~%")
    (generate-code-for-housekeeping-classes fout analysis)
    (generate-code-for-global-variables fout analysis)
    (generate-code-for-local-variables fout analysis)
    (generate-code-for-new-gcobject-exprs fout analysis)
    (generate-code-for-new-housekeeping-class-exprs fout analysis) 
   ))

(defun boolean (x) (if x t nil))
(defun describe-all-classes (name &key (analysis *analysis*) detail)
  (format t "There housekeeping classes: ~a~%" (project-housekeeping-classes (analysis-project analysis)))
  (format t "~8a ~8a ~8a ~8a ~a~%" "onstack" "onheap" "oshspoh" "csp" "Name")
  (maphash (lambda (k v)
             (when (search name (format nil "~a" v))
               (format t "~8a ~8a ~8a ~8a ~30a         ~a~%~a~%"
                       (boolean (gethash k (analysis-housekeeping-class-stored-on-stack analysis)))
                       (boolean (gethash k (analysis-housekeeping-class-stored-on-heap analysis)))
                       (on-stack-has-smart-pointers-on-heap-p v analysis)
                       (contains-smart-pointers-p v analysis)
                       k
                       (housekeeping-class-location v)
                       v)
               (when detail
                 (format t "~a~%" v)
                 (format t "     contains-smart-pointers-p -> ~a~%" (contains-smart-pointers-p v analysis))
                 (format t "     on-stack-has-smart-pointers-on-heap-p -> ~a~%" (on-stack-has-smart-pointers-on-heap-p v analysis))
                 )
               ))
           (project-housekeeping-classes (analysis-project analysis))))

(defun describe-all-interesting-classes (name &key (analysis *analysis*) detail)
  "Describe classes either stored on stack or on heap that contain smart-pointers"
  (format t "There housekeeping classes: ~a~%" (project-housekeeping-classes (analysis-project analysis)))
  (format t "~8a ~8a ~8a ~8a ~a~%" "onstack" "onheap" "oshspoh" "csp" "Name")
  (maphash (lambda (k v)
             (when (and (search name (format nil "~a" v))
                        (or (gethash k (analysis-housekeeping-class-stored-on-stack analysis))
                            (gethash k (analysis-housekeeping-class-stored-on-heap analysis))))
               (format t "~8a ~8a ~8a ~8a ~30a         ~a~%~a~%"
                       (boolean (gethash k (analysis-housekeeping-class-stored-on-stack analysis)))
                       (boolean (gethash k (analysis-housekeeping-class-stored-on-heap analysis)))
                       (on-stack-has-smart-pointers-on-heap-p v analysis)
                       (contains-smart-pointers-p v analysis)
                       k
                       (housekeeping-class-location v)
                       v)
               (when detail
                 (format t "~a~%" v)
                 (format t "     contains-smart-pointers-p -> ~a~%" (contains-smart-pointers-p v analysis))
                 (format t "     on-stack-has-smart-pointers-on-heap-p -> ~a~%" (on-stack-has-smart-pointers-on-heap-p v analysis))
                 )
               ))
           (project-housekeeping-classes (analysis-project analysis))))


(defun reset-class-smart-pointer-info (&optional (analysis *analysis*))
  (maphash (lambda (k v)
             (remhash k (analysis-housekeeping-class-on-stack-has-smart-pointers-on-heap analysis))
             (remhash k (analysis-housekeeping-class-contains-smart-pointers analysis))
             )
           (project-housekeeping-classes (analysis-project analysis))))


(defun describe-global-variables (&optional (name "") (analysis *analysis*))
  (with-all-housekeeping-classes ((project-housekeeping-classes (analysis-project analysis)))
    (maphash (lambda (k v)
               (when (search name (format nil "~a" v))
                 (format t "~10a ~a~%" (contains-smart-pointers-p (global-variable-ctype v) analysis) k)
                 )
               )
             (project-global-variables (analysis-project analysis)))))




(defun find-global-variable-roots (&optional (analysis *analysis*))
  (let #|symbol-macrolet|# ((project (analysis-project analysis)))
    (with-all-housekeeping-classes ((project-housekeeping-classes project))
      (maphash (lambda (k v)
                 (when (contains-smart-pointers-p (global-variable-ctype v) analysis)
                   (format t "~10a ~a~%" (contains-smart-pointers-p (global-variable-ctype v) analysis) k)
                   )
                 )
               (project-global-variables project)))))
  

(defun describe-static-local-variables (&optional (name "") (analysis *analysis*))
  (let #|symbol-macrolet|# ((project (analysis-project analysis)))
    (with-all-housekeeping-classes ((project-housekeeping-classes project))
      (maphash (lambda (k v)
                 (when (search name (format nil "~a" v))
                   (format t "~10a ~a~%" (contains-smart-pointers-p (static-local-variable-ctype v) analysis) k)
                   )
                 )
               (project-static-local-variables project)))))

(defun describe-local-variables (&key (name "") (analysis *analysis*) details all)
  (let ((project (analysis-project analysis)))
    (format t "Project local-variables: ~a~%" (project-local-variables project))
    (format t "~10a ~10a ~a~%" "csp" "oshspoh" "Name")
    (with-all-housekeeping-classes ((project-housekeeping-classes project))
      (maphash (lambda (k v)
                 (let ((csp (contains-smart-pointers-p (local-variable-ctype v) analysis))
                       (oshspohp (on-stack-has-smart-pointers-on-heap-p (local-variable-ctype v) analysis)))
                   (when (search name (format nil "~a" v))
                     (when (or all csp oshspohp)
                       (format t "~10a ~10a ~a~%" csp oshspohp k)
                       (when details (format t "~a~%" v))))
                   ))
               (project-local-variables project)))))




;;
;; Test matcher for template classes
;;



#+testing 
(progn
  (defvar $test-search nil)
  (setq $test-search (lsel $* ".*/lisp\.cc"))
  (load-asts $test-search
             :arguments-adjuster-code (lambda (args) (concatenate 'vector #-quiet args #+quiet(remove "-v" args)
                                                                  #("-DUSE_MPS"
                                                                    "-DRUNNING_GC_BUILDER"
                                                                    "-resource-dir"
                                                                    "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr//lib/clang/5.1")))))


#|
                Ignore Derivable<...> classes because they are instantiated in
                clbind_wrappers.h:
                template <typename T>
                struct from_object<T*> { ... }    In a dynamic_cast test.
                No instances of Derivable<...> should ever be allocated.
                |#
#+testing
(progn
  (defparameter *test-matcher*
    '(:record-decl
      (:is-definition)
      (:unless
          (:is-same-or-derived-from
           (:record-decl
            (:any-of
             (:has-name "GCObject")
             (:has-name "GCContainer")
             ))))
      )
    )
  (match-run *test-matcher*
             ;;:limit 100
               ;;              :tag :point
               :match-comments '( ".*mytest.*" )
               :code #'(lambda ()
                         (let* ((class-node (mtag-node :whole))
                                (key (record-key class-node))
                                )
                           (when (and (typep class-node 'cast:class-template-specialization-decl)
                                      (not (eq (cast:get-specialization-kind class-node) 'ast-tooling:tsk-undeclared)))
                             (format t "CXXRecordDecl ~a  specialization-kind: ~a ~a ~a~%" (typep class-node 'cast:cxxrecord-decl) (cast:get-specialization-kind class-node) class-node key))
                           )
                         #+(or)(progn
                           (format t "MATCH: ------------------~%")
                           (format t "        Start: ~a~%" (mtag-loc-start :whole))
                           (format t "         Node: ~a~%" (mtag-node :whole))
                           (format t "         Name: ~a~%" (mtag-name :whole))
                           (format t "   record-key: ~a~%" (record-key (mtag-node :whole)))
                           )
;;                         (cast:dump (mtag-node :grand-parent))
                         #|                       (format t "         diagnostic name: ~a~%"
                         (cast:get-name (cast:get-specialized-template (mtag-node :whole))))
                (when (typep (mtag-node :whole) 'cast:class-template-specialization-decl)
                         (format t "   +++++ First instantiation location  ~a    ~%"
                         (source-loc-as-string (cast:get-point-of-instantiation (mtag-node :whole)))))
                |#
                       )
             )
  )






(defun run-all ()
  (load-project)
  (analyze-project)
  (describe-problematic-housekeeping-classes))
