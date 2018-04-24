

(push :use-breaks *features*)

(progn
  (provide 'gc-builder)
  (load "sys:clang-tool.lsp") ; (require 'clang-tool)
  (use-package :ast-tooling)
  )

;;(defmacro gclog (fmt &rest args) `(format t ,fmt ,@args))
(defmacro gclog (fmt &rest args) nil)


(defun safe-get-name-decl (decl)
  (if (cast:get-identifier decl)
      (cast:get-name decl)
      "NO-NAME-SAFE"))



(defstruct variable
  location
  name
  ctype)

(defstruct (global-variable (:include variable)))
(defstruct (static-local-variable (:include variable)))
(defstruct (local-variable (:include variable)))



;;;
;;; Generate a unique ID for every class derived from GCObject
;;; Wrappers and Iterators have their own ID number space
;;; Wrappers  #16r01xxxx
;;; Iterators #16r02xxxx
;;; System
;;; regular   #16r00xxxx
(defstruct (organized-gcobjects
             (:print-object))
  builtins
  wrappers
  iterators )

(defmethod print-object ((object organized-gcobjects) stream)
  (format stream "#<organized-gcobjects>"))

(defstruct gcobject-subclass
  "Represent an inheritance relationship between classes for C++/Lisp classes that inherit from GCObject"
  key
  name
  bases
  vbases
  instance-variables
  has-destructor
;;  inherits-destructor
  family ;; (or :builtin :wrapper :iterator)
  kind-id
)

(defun gcobject-subclass-wrapper-p (x)
  (search "clbind::Wrapper" (gcobject-subclass-name x)))



;; Use closed over environment to keep track of counters for kind-id
(let ((builtin-id-counter 0)
      (wrapper-id-counter 0)
      (iterator-id-counter 0))
  (defun gcobject-kind-id-reset ()
    (setq builtin-id-counter 0
          wrapper-id-counter 0
          iterator-id-counter 0))
  (defun gcobject-kind-id-builtins ()
    (incf builtin-id-counter))
  (defun gcobject-kind-id (x)
    (cond
      ((search "clbind::Wrapper" (gcobject-subclass-name x))  (values :wrapper (+ #16r010000 (incf wrapper-id-counter))))
      ((search "clbind::Iterator" (gcobject-subclass-name x)) (values :iterator (+ #16r020000 (incf iterator-id-counter))))
      (t (values :builtin (incf builtin-id-counter)))))
  (defun gcobject-kind-id-status ()
    (format t "builtin-id-counter ~a~%" builtin-id-counter)
    (format t "wrapper-id-counter ~a~%" wrapper-id-counter)
    (format t "iterator-id-counter ~a~%" iterator-id-counter))
  )

(defstruct project
  "Store the results of an project"
  (gcobjects (make-hash-table :test #'equal))
  (housekeeping-classes (make-hash-table :test #'equal))
  (local-variables (make-hash-table :test #'equal))
  (global-variables (make-hash-table :test #'equal))
  (static-local-variables (make-hash-table :test #'equal))
  )



(defstruct analysis
  project
  (organized-gcobjects (make-organized-gcobjects))
  (housekeeping-class-on-stack-has-smart-pointers-on-heap (make-hash-table :test #'equal))
  (housekeeping-class-contains-smart-pointers (make-hash-table :test #'equal))
  (housekeeping-class-stored-on-heap (make-hash-table :test #'equal))
  (housekeeping-class-stored-on-stack (make-hash-table :test #'equal))
  (global-variables-with-smart-pointers (make-hash-table :test #'equal))
  (static-local-variables-with-smart-pointers (make-hash-table :test #'equal))
  (local-variables-with-smart-pointers-on-heap (make-hash-table :test #'equal))
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
                                   
  





(defun merge-projects (union one)
  (maphash (lambda (k v) (setf (gethash k (project-gcobjects union)) v)) (project-gcobjects one))
  (maphash (lambda (k v) (setf (gethash k (project-housekeeping-classes union)) v)) (project-housekeeping-classes one))
  (maphash (lambda (k v) (setf (gethash k (project-local-variables union)) v)) (project-local-variables one))
  (maphash (lambda (k v) (setf (gethash k (project-global-variables union)) v)) (project-global-variables one))
  (maphash (lambda (k v) (setf (gethash k (project-static-local-variables union)) v)) (project-static-local-variables one))
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
       (cast:get-as-string (cast:get-as-type template-arg)))
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



(defun record-key (decl-node)
  "Return the name of the class/struct and whether it is a template specializer or not"
  (or decl-node (error "There is a problem, the decl-node is nil"))
  (case (type-of decl-node)
    ((cast:cxxrecord-decl
      cast:record-decl)
     (values (cast:get-qualified-name-as-string decl-node) nil))
    (cast:enum-decl
     (values (cast:get-qualified-name-as-string decl-node) nil))
    (cast:class-template-specialization-decl
     (let* ((decl-name (safe-get-name-decl decl-node))if (cast:get-identifier decl-node)
            (template-args (cast:get-template-args decl-node))
            (template-args-as-list (loop :for i :from 0 :below (cast:size template-args)
                                        :collect (let* ((template-arg (cast:template-argument-list-get template-args i)))
                                                   (template-arg-as-string template-arg)))))
       (values (format nil "~a<~{~a~^,~}>" (cast:get-qualified-name-as-string decl-node) template-args-as-list) t)))
    (cast:class-template-partial-specialization-decl
     (let* ((decl-name (cast:get-qualified-name-as-string decl-node))
            (template-args (cast:get-template-args decl-node))
            (template-args-as-list (loop :for i :from 0 :below (cast:size template-args)
                                      :for template-arg = (cast:template-argument-list-get template-args i)
                                      :for type-name = (template-arg-as-string template-arg)
                                      :collect type-name)))
       (values (format nil "~a<~{~a~^,~}>" (cast:get-qualified-name-as-string decl-node) template-args-as-list) t)))
    (otherwise
     (format t "Add support for record-key for ~a  get-name->~a~%" decl-node (cast:get-qualified-name-as-string decl-node))
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

(defstruct (unclassified-class-template-specialization-ctype (:include record-ctype)))


(defstruct (container (:include class-template-specialization-ctype)))

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



  

  

(defgeneric contains-smart-pointers-p (x analysis))
(defmethod contains-smart-pointers-p ((x t) analysis)
  (when (not x)
    #+use-breaks(break "contains-smart-pointers-p on nil"))
  (when (eq x t)
    #+use-breaks(break "contains-smart-pointers-p on t"))
  (when (eq x :maybe)
    #+use-breaks(break "contains-smart-pointers-p on :maybe"))
  (warn "Add support for contains-smart-pointers-p for ~a" x)
  #+use-breaks(break "Check x")
  t)


(defmethod contains-smart-pointers-p ((x global-variable) analysis)
  (contains-smart-pointers-p (global-variable-ctype x) analysis))

(defmethod contains-smart-pointers-p ((x instance-variable) analysis)
  (contains-smart-pointers-p (instance-variable-ctype x) analysis))


(defmethod contains-smart-pointers-p ((x function-proto-ctype) analysis)
  nil)

(defmethod contains-smart-pointers-p ((x dependent-template-specialization-ctype) analysis)
  nil)

(defmethod contains-smart-pointers-p ((x rvalue-reference-ctype) analysis)
  nil)

#||
(defmethod contains-smart-pointers-p ((x unclassified-template-specialization-ctype))
  (assert *all-housekeeping-classes* nil "Make sure *all-housekeeping-classes* is defined")
  (let ((key (unclassified-template-specialization-ctype-description x)))
    (multiple-value-bind (entry found)
        (gethash key *all-housekeeping-classes*)
      (if found
          (contains-smart-pointers-p entry)
          (missing-record key)))))
||#

(defmethod contains-smart-pointers-p ((x typedef-ctype) analysis)
  (contains-smart-pointers-p (typedef-ctype-desugared x) analysis))

(defmethod contains-smart-pointers-p ((x elaborated-ctype) analysis)
  (contains-smart-pointers-p (elaborated-ctype-named-type x) analysis))

(defmethod contains-smart-pointers-p ((x dependent-name-ctype) analysis) nil)
(defmethod contains-smart-pointers-p ((x constant-array-ctype) analysis) nil)
(defmethod contains-smart-pointers-p ((x incomplete-array-ctype) analysis ) nil)
(defmethod contains-smart-pointers-p ((x template-type-parm-ctype) anal) nil)
(defmethod contains-smart-pointers-p ((x subst-template-type-parm-ctype) anal) nil)
(defmethod contains-smart-pointers-p ((x lvalue-reference-ctype) anal) nil)
(defmethod contains-smart-pointers-p ((x enum-ctype) anal) nil)
(defmethod contains-smart-pointers-p ((x member-pointer-ctype) anal) nil)
(defmethod contains-smart-pointers-p ((x dependent-sized-array-ctype) anal) nil)

(defmethod contains-smart-pointers-p ((x smart-ptr-ctype) anal) t)
(defmethod contains-smart-pointers-p ((x weak-smart-ptr-ctype) anal) t)
(defmethod contains-smart-pointers-p ((x uninteresting-ctype) anal) nil)
(defmethod contains-smart-pointers-p ((x paren-ctype) anal) nil)
(defmethod contains-smart-pointers-p ((x housekeeping-class) anal)
  (setf (gethash (housekeeping-class-key x) (analysis-housekeeping-class-stored-on-heap anal)) x)
  #||
  ;; Old way - we cached the results - but I don't think it worked properly ; ;
  ;;                                    ; ;
  (analysis-housekeeping-class-contains-smart-pointers-p x anal)
  ||#
  (let* (result
         (project (analysis-project anal))
         (all-classes (project-housekeeping-classes project)))
    (loop :for base-name :in (housekeeping-class-bases x)
       :do (when base-name
             (let ((base (gethash base-name all-classes)))
               (when base 
                 (setq result (or result (contains-smart-pointers-p base anal)))))))
    (loop :for field :in (housekeeping-class-fields x)
       :do (when field
             (let ((field-ctype (instance-variable-ctype field)))
             (setq result (or result (contains-smart-pointers-p field-ctype anal))))))
    result))


(defmethod contains-smart-pointers-p ((x stl-container) anal)
  "This should cover every stl-xxxx container"
  (let (sp)
    (dotimes (i (length (stl-container-arguments x)))
      (setq sp (or sp (contains-smart-pointers-p (elt (stl-container-arguments x) i) anal))))
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
      (warn "Check if the following vector-ctype class is contains-smart-pointers-p - if not, make it an ignore~%vector-ctype-ignore ~a" key))
  :maybe)

(defmethod contains-smart-pointers-p ((x vector-ctype) anal)
  (check-if-vector-ctype-ignore (vector-ctype-description x)))

(defmethod contains-smart-pointers-p ((x auto-ctype) anal)
  (check-if-auto-ctype-ignore (auto-ctype-description x)))




(defconstant +template-specialization-ignores+
  (build-ignore-table '(


                        "asttooling::FOO<class asttooling::A,int>"
                        "asttooling::FOO<int,int>"
                        "boost::char_separator<char,struct std::__1::char_traits<char>>"
                        "boost::date_time::month_functor<class boost::gregorian::date>"
                        "boost::date_time::wrapping_int2<short,1,12>"
                        "boost::dynamic_bitset<unsigned long,class std::__1::allocator<unsigned long>>"
                        "boost::exception_detail::refcount_ptr<struct boost::exception_detail::error_info_container>"
                        "boost::hash<char>"
                        "boost::iostreams::back_insert_device<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >>"
                        "boost::iostreams::detail::basic_buffer<char,class std::__1::allocator<char>>"
                        "boost::iostreams::detail::linked_streambuf<char,struct std::__1::char_traits<char>>"
                        "boost::iostreams::stream_buffer<class boost::iostreams::basic_null_device<char, struct boost::iostreams::input>,struct std::__1::char_traits<char>,class std::__1::allocator<char>,struct boost::iostreams::input>"
                        "boost::program_options::basic_parsed_options<char>"
                        "boost::random::detail::generator_seed_seq<class boost::random::linear_congruential_engine<unsigned int, 16807, 0, 2147483647>>"
                        "boost::random::detail::generator_seed_seq<class boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>>"
                        "boost::random::linear_congruential_engine<unsigned int,16807,0,2147483647>"
                        "boost::random::linear_congruential_engine<unsigned int,40014,0,2147483563>"
                        "boost::scoped_ptr<class clbind::detail::cast_graph::impl>"
                        "boost::shared_ptr<const void>"
                        "boost::shared_ptr<void>"
                        "boost::token_iterator<class boost::char_separator<char, struct std::__1::char_traits<char> >,class std::__1::__wrap_iter<const char *>,class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >>"
                        "boost::token_iterator<class boost::offset_separator,class std::__1::__wrap_iter<const char *>,class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >>"
                        "boost::tokenizer<class boost::char_separator<char, struct std::__1::char_traits<char> >,class std::__1::__wrap_iter<const char *>,class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >>"
                        "boost::tokenizer<class boost::offset_separator,class std::__1::__wrap_iter<const char *>,class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >>"
                        "boost::tuples::tuple<unsigned long,unsigned long,unsigned long,long,struct boost::tuples::null_type,struct boost::tuples::null_type,struct boost::tuples::null_type,struct boost::tuples::null_type,struct boost::tuples::null_type,struct boost::tuples::null_type>"
                        "clang::ArrayType_cannot_be_used_with_getAs<class clang::FunctionNoProtoType,0>"
                        "clang::ArrayType_cannot_be_used_with_getAs<class clang::ReferenceType,0>"
                        "clang::CanQual<class clang::ReferenceType>"
                        "clang::CanQual<class clang::Type>"
                        "clang::RedeclarableTemplateDecl::SpecIterator<class clang::ClassTemplateSpecializationDecl,struct clang::RedeclarableTemplateDecl::SpecEntryTraits<class clang::ClassTemplateSpecializationDecl>,class clang::ClassTemplateSpecializationDecl>"
                        "clang::RedeclarableTemplateDecl::SpecIterator<class clang::FunctionTemplateSpecializationInfo,struct clang::RedeclarableTemplateDecl::SpecEntryTraits<class clang::FunctionTemplateSpecializationInfo>,class clang::FunctionDecl>"
                        "clang::RedeclarableTemplateDecl::SpecIterator<class clang::VarTemplateSpecializationDecl,struct clang::RedeclarableTemplateDecl::SpecEntryTraits<class clang::VarTemplateSpecializationDecl>,class clang::VarTemplateSpecializationDecl>"



                        "llvm::DenseMapIterator<const class llvm::MDString *,class llvm::MDNode *,struct llvm::DenseMapInfo<const class llvm::MDString *>,1>"
                        "llvm::DenseMapIterator<unsigned int,struct llvm::PointerAlignElem,struct llvm::DenseMapInfo<unsigned int>,1>"
                        "llvm::OwningPtr<class llvm::MemoryBuffer>"
                        "llvm::PointerIntPair<class llvm::User *,1,unsigned int,class llvm::PointerLikeTypeTraits<class llvm::User *>>"
                        "llvm::SmallString<128>"
                        "llvm::SmallVector<char,100>"
                        "llvm::SmallVector<int,16>"
                        "llvm::SmallVector<struct clang::RecursiveASTVisitor<class asttooling::ASTVisitor_Adapter>::EnqueueJob,16>"
                        "llvm::SmallVector<struct std::__1::pair<const char *, struct std::__1::pair<int, const char *> >,4>"
                        "llvm::SwitchInst::CaseIteratorT<const class llvm::SwitchInst,const class llvm::ConstantInt,const class llvm::BasicBlock>"
                        "llvm::generic_gep_type_iterator<const class llvm::Use *>"
                        "llvm::sys::SmartMutex<0>"
                        "llvm::value_use_iterator<const class llvm::User>"
                        "std::__1::__list_iterator<class boost::iostreams::detail::linked_streambuf<char, struct std::__1::char_traits<char> > *,void *>"
                        "std::__1::__split_buffer<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >,class std::__1::allocator<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> > > &>"
                        "std::__1::__split_buffer<struct boost::io::detail::format_item<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >,class std::__1::allocator<struct boost::io::detail::format_item<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> > > &>"
                        "std::__1::basic_ostringstream<char,struct std::__1::char_traits<char>,class std::__1::allocator<char>>"
                        "std::__1::basic_streambuf<char,struct std::__1::char_traits<char>>"
                        "std::__1::fpos<__mbstate_t>"
                        "std::__1::istreambuf_iterator<char,struct std::__1::char_traits<char>>"
                        "std::__1::uniform_int_distribution<long>"
                        "std::__1::unique_lock<class std::__1::mutex>"
                        "std::__1::unique_ptr<char,void (*)(void *)>"
                        "std::__1::unique_ptr<class std::__1::__tree_node<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, void *>,class std::__1::__tree_node_destructor<class std::__1::allocator<class std::__1::__tree_node<class std::__1::basic_string<char, struct std::__1::char_traits<char>, class std::__1::allocator<char> >, void *> > >>"
                        "std::__1::unique_ptr<struct __sFILE,int (*)(struct __sFILE *)>"
                        "std::__1::unique_ptr<unsigned char,void (*)(void *)>"
                        "std::__1::unique_ptr<unsigned int,void (*)(void *)>"




                        "LispObjectAllocatorFunctor<type-parameter-0-0>"
                        "LispObjectAllocatorFunctor"
                        "clang::ast_matchers::internal::VariadicDynCastAllOfMatcher"
                        "clang::ast_matchers::internal::VariadicAllOfMatcher"
                        "boost::arg"
                        "boost::parameter::keyword"
                        "clang::ast_matchers::internal::ArgumentAdaptingMatcherFunc"
                        "clang::ast_matchers::internal::VariadicOperatorMatcherFunc"
                        "std::__1::__ignore_t"
                        "boost::detail::make_property_map_from_arg_pack_gen"
                        "basic_vtable0<type-parameter-0-0>"
                        "character_pointer_range<type-parameter-0-0>"
                        "array<type-parameter-0-0, 101>"
                        "boost::array"
                        "basic_regex<type-parameter-0-0, regex_traits<type-parameter-0-0, cpp_regex_traits<type-parameter-0-0> > >"
                        "singleton_wrapper<type-parameter-0-0>"
                        "basic_string<type-parameter-0-0, char_traits<type-parameter-0-0>, allocator<type-parameter-0-0> >"
                        "boost::scoped_ptr"
                        "date_input_facet<class boost::gregorian::date, type-parameter-0-0, istreambuf_iterator<type-parameter-0-0, char_traits<type-parameter-0-0> > >"
                        "Iterator<type-parameter-0-0, struct reg::null_type>"
                        "SmallVector<type-parameter-0-0, 4>"
                        "StringMapEntry<type-parameter-0-0>"
                        "__base<type-parameter-0-0 (type-parameter-0-1...)>"
                        "__call_once_param<type-parameter-0-0>"
                        "basic_ostream<type-parameter-0-0, type-parameter-0-1>"
                        "basic_streambuf<type-parameter-0-0, type-parameter-0-1>"
                        "boost::iostreams::detail::linked_streambuf"
                        "boost::match_results"
                        "boost::re_detail::basic_regex_implementation"
                        "boost::tuples::tuple"
                        "date_facet<class boost::gregorian::date, type-parameter-0-0, ostreambuf_iterator<type-parameter-0-0, char_traits<type-parameter-0-0> > >"
                        "functor_wrapper<type-parameter-0-0, type-parameter-0-1>"
                        "functor_wrapper<type-parameter-1-0, type-parameter-1-1>"
                        "hashed_index_node<typename type-parameter-0-3::type::node_type>"
                        "iplist<type-parameter-0-0, ilist_traits<type-parameter-0-0> >"
                        "linked_streambuf<type-parameter-0-1, char_traits<type-parameter-0-1> >"
                        "linked_streambuf<typename char_type_of<type-parameter-0-0>::type, type-parameter-0-1>"
                        "llvm::PointerIntPair"
                        "llvm::SmallVector"
                        "llvm::sys::SmartMutex"
                        "num_get<type-parameter-0-1, type-parameter-0-2>"
                        "num_put<type-parameter-0-1, type-parameter-0-2>"
                        "pair<type-parameter-0-1, type-parameter-0-2>"
                        "re_set_long<typename type-parameter-0-1::char_class_type>"
                        "re_set_long<typename type-parameter-0-2::char_class_type>"
                        "regex_data<type-parameter-0-0, type-parameter-0-1>"
                        "saved_assertion<type-parameter-0-0>"
                        "saved_matched_paren<type-parameter-0-0>"
                        "saved_position<type-parameter-0-0>"
                        "saved_recursion<match_results<type-parameter-0-0, type-parameter-0-1> >"
                        "saved_repeater<type-parameter-0-0>"
                        "saved_single_repeat<type-parameter-0-0>"
                        "std::__1::basic_filebuf"
                        "std::__1::basic_streambuf"
                        "stl_buf_unlocker<basic_stringbuf<type-parameter-0-0, type-parameter-0-1, allocator<type-parameter-0-0> >, type-parameter-0-0>"
                        "time_facet<class boost::posix_time::ptime, type-parameter-0-0, ostreambuf_iterator<type-parameter-0-0, char_traits<type-parameter-0-0> > >"
                        "time_input_facet<class boost::posix_time::ptime, type-parameter-0-0, istreambuf_iterator<type-parameter-0-0, char_traits<type-parameter-0-0> > >"
                        "typed_value<type-parameter-0-0, char>"
                        "typed_value<type-parameter-0-0, wchar_t>"
                        "vector<type-parameter-0-0, allocator<type-parameter-0-0> >"




                        "<type-parameter-0-0>"
                        "AlignedCharArrayUnion<typename DenseMapBase<SmallDenseMap<KeyT, ValueT, InlineBuckets, KeyInfoT>, type-parameter-0-0, type-parameter-0-1, type-parameter-0-3>::BucketT [InlineBuckets], char, char, char, char, char, char>"
                        "ArrayType_cannot_be_used_with_getAs<type-parameter-1-0, (llvm::is_same<type-parameter-1-0, class ArrayType>::value || llvm::is_base_of<class ArrayType, type-parameter-1-0>::value)>"
                        "OptionDiffPrinter<typename type-parameter-0-0::parser_data_type, type-parameter-0-1>"
                        "OptionValue<type-parameter-0-1>"
                        "StringMapConstIterator<type-parameter-0-0>"
                        "StringMapIterator<type-parameter-0-0>"
                        "TypedMatcherOps<type-parameter-0-0>"
                        "ValuesClass<type-parameter-0-0>"
                        "__bit_array<type-parameter-0-0>"
                        "__bit_iterator<type-parameter-0-0, false, 0>"
                        "__bit_iterator<vector<_Bool, type-parameter-0-0>, false, 0>"
                        "__bit_iterator<vector<_Bool, type-parameter-0-0>, true, 0>"
                        "__call_once_param<tuple<typename decay<type-parameter-0-0>::type, typename decay<type-parameter-0-1>::type...> >"
                        "__deque_iterator<type-parameter-0-0, typename allocator_traits<type-parameter-0-1>::pointer, type-parameter-0-0 &, typename allocator_traits<typename allocator_traits<type-parameter-0-1>::rebind_alloc<typename allocator_traits<type-parameter-0-1>::pointer> >::pointer, typename allocator_traits<type-parameter-0-1>::difference_type, __block_size>"
                        "__deque_iterator<type-parameter-0-1, type-parameter-0-2, type-parameter-0-3, type-parameter-0-4, type-parameter-0-5, _B2>"
                        "__gmp_expr<type-parameter-0-0, type-parameter-0-0>"
                        "__hash_const_iterator<typename allocator_traits<typename allocator_traits<type-parameter-0-3>::rebind_alloc<__hash_node<type-parameter-0-0, typename allocator_traits<type-parameter-0-3>::void_pointer> > >::pointer>"
                        "__hash_iterator<typename allocator_traits<typename allocator_traits<type-parameter-0-3>::rebind_alloc<__hash_node<type-parameter-0-0, typename allocator_traits<type-parameter-0-3>::void_pointer> > >::pointer>"
                        "__independent_bits_engine<type-parameter-1-0, typename conditional<sizeof(result_type) <= sizeof(uint32_t), unsigned int, unsigned long long>::type>"
                        "__map_const_iterator<typename __tree<__value_type<type-parameter-0-0, type-parameter-0-1>, __map_value_compare<type-parameter-0-0, __value_type<type-parameter-0-0, type-parameter-0-1>, type-parameter-0-2, is_empty<type-parameter-0-2>::value && !__is_final(type-parameter-0-2)>, typename allocator_traits<type-parameter-0-3>::rebind_alloc<__value_type<type-parameter-0-0, type-parameter-0-1> > >::const_iterator>"
                        "__map_iterator<typename __tree<__value_type<type-parameter-0-0, type-parameter-0-1>, __map_value_compare<type-parameter-0-0, __value_type<type-parameter-0-0, type-parameter-0-1>, type-parameter-0-2, is_empty<type-parameter-0-2>::value && !__is_final(type-parameter-0-2)>, typename allocator_traits<type-parameter-0-3>::rebind_alloc<__value_type<type-parameter-0-0, type-parameter-0-1> > >::iterator>"
                        "__split_buffer<type-parameter-0-0, type-parameter-0-1 &>"
                        "__split_buffer<type-parameter-0-0, typename remove_reference<type-parameter-0-1>::type &>"
                        "__split_buffer<typename __deque_base<type-parameter-0-0, type-parameter-0-1>::pointer, typename __deque_base<type-parameter-0-0, type-parameter-0-1>::__pointer_allocator &>"
                        "__tree_const_iterator<type-parameter-0-0, typename allocator_traits<typename allocator_traits<type-parameter-0-2>::rebind_alloc<__tree_node<type-parameter-0-0, typename allocator_traits<type-parameter-0-2>::void_pointer> > >::pointer, typename allocator_traits<type-parameter-0-2>::difference_type>"
                        "__tree_iterator<type-parameter-0-0, typename allocator_traits<typename allocator_traits<type-parameter-0-2>::rebind_alloc<__tree_node<type-parameter-0-0, typename allocator_traits<type-parameter-0-2>::void_pointer> > >::pointer, typename allocator_traits<type-parameter-0-2>::difference_type>"
                        "__wrap_iter<typename __vector_base<type-parameter-0-0, type-parameter-0-1>::pointer>"
                        "__wrap_iter<typename allocator_traits<type-parameter-0-2>::pointer>"
                        "allocator<__func<type-parameter-1-0, allocator<type-parameter-1-0>, type-parameter-0-0 (type-parameter-0-1...)> >"
                        "allocator<__shared_ptr_emplace<type-parameter-0-0, allocator<type-parameter-0-0> > >"
                        "archive_constructed<type-parameter-0-0>"
                        "array_constructor<typename rebind_wrap<typename type-parameter-0-0::allocator, typename type-parameter-0-0::bucket>::type>"
                        "auto_ptr<stream_buffer<typename unwrap_ios<type-parameter-1-0>::type, char_traits<type-parameter-0-1>, type-parameter-0-3, type-parameter-0-4> >"
                        "auto_ptr_ref<type-parameter-1-0>"
                        "auto_ptr_with_deleter<type-parameter-0-1>"
                        "auto_space<unsigned long, typename type-parameter-0-3::type::final_allocator_type>"
                        "basic_buffer<typename char_type_of<type-parameter-0-0>::type, allocator<typename char_type_of<type-parameter-0-0>::type> >"
                        "basic_char_set<type-parameter-0-0, type-parameter-0-1>"
                        "basic_istream<type-parameter-0-0, char_traits<type-parameter-0-0> >"
                        "basic_istringstream<type-parameter-0-0, type-parameter-0-1, allocator<type-parameter-0-0> >"
                        "basic_istringstream<type-parameter-0-1, char_traits<type-parameter-0-1>, allocator<type-parameter-0-1> >"
                        "basic_oaltstringstream<type-parameter-0-0, type-parameter-0-1, allocator<type-parameter-0-0> >"
                        "basic_oaltstringstream<type-parameter-0-0, type-parameter-0-1, type-parameter-0-2>"
                        "basic_ostringstream<type-parameter-0-0, char_traits<type-parameter-0-0>, allocator<type-parameter-0-0> >"
                        "basic_ostringstream<type-parameter-0-1, type-parameter-0-2, allocator<type-parameter-0-1> >"
                        "basic_ostringstream<type-parameter-0-2, char_traits<type-parameter-0-2>, allocator<type-parameter-0-2> >"
                        "basic_ostringstream<typename date_facet<typename type-parameter-0-0::date_type, type-parameter-0-1, type-parameter-0-2>::char_type, char_traits<typename date_facet<typename type-parameter-0-0::date_type, type-parameter-0-1, type-parameter-0-2>::char_type>, allocator<typename date_facet<typename type-parameter-0-0::date_type, type-parameter-0-1, type-parameter-0-2>::char_type> >"
                        "basic_regex_formatter<type-parameter-0-0, match_results<type-parameter-0-1, type-parameter-0-2>, type-parameter-0-4, type-parameter-0-3>"
                        "basic_regex_parser<type-parameter-0-0, type-parameter-0-1>"
                        "basic_string<char, struct std::__1::char_traits<char>, type-parameter-0-3>"
                        "basic_string<type-parameter-0-0, type-parameter-0-1, allocator<type-parameter-0-0> >"
                        "basic_string<type-parameter-0-0, type-parameter-0-1, type-parameter-0-2>"
                        "basic_string<type-parameter-0-1, char_traits<type-parameter-0-1>, allocator<type-parameter-0-1> >"
                        "basic_string<type-parameter-0-1, char_traits<type-parameter-0-1>, type-parameter-0-2>"
                        "basic_string<type-parameter-1-0, type-parameter-1-1, type-parameter-1-2>"
                        "basic_string<typename iterator_traits<type-parameter-0-0>::value_type, char_traits<typename iterator_traits<type-parameter-0-0>::value_type>, allocator<typename iterator_traits<type-parameter-0-0>::value_type> >"
                        "basic_string<typename regex_iterator_traits<type-parameter-0-0>::value_type, char_traits<typename regex_iterator_traits<type-parameter-0-0>::value_type>, allocator<typename regex_iterator_traits<type-parameter-0-0>::value_type> >"
                        "basic_string<typename regex_iterator_traits<type-parameter-0-0>::value_type, type-parameter-0-1, type-parameter-0-2>"
                        "basic_string<typename type-parameter-0-0::value_type, char_traits<typename type-parameter-0-0::value_type>, allocator<typename type-parameter-0-0::value_type> >"
                        "bessel_j_small_z_series_term<type-parameter-0-0, type-parameter-0-1>"
                        "bessel_y_small_z_series_term_a<type-parameter-0-0, type-parameter-0-1>"
                        "bessel_y_small_z_series_term_b<type-parameter-0-0, type-parameter-0-1>"
                        "beta_inv_ab_t<type-parameter-0-0, type-parameter-0-1>"
                        "bucket_array<typename type-parameter-0-3::type::final_allocator_type>"
                        "call_frexp<type-parameter-0-0>"
                        "call_ldexp<type-parameter-0-0>"
                        "char_separator<type-parameter-0-1, typename basic_string<type-parameter-0-1, char_traits<type-parameter-0-1>, allocator<type-parameter-0-1> >::traits_type>"
                        "complex<type-parameter-0-0>"
                        "copy_nodes<typename rebind_wrap<typename type-parameter-0-0::allocator, typename type-parameter-0-0::node>::type>"
                        "cpp_regex_traits_base<type-parameter-0-0>"
                        "cyl_bessel_i_small_z<type-parameter-0-0, type-parameter-0-1>"
                        "deque<typename range_value<type-parameter-0-0>::type, allocator<typename range_value<type-parameter-0-0>::type> >"
                        "digraph<type-parameter-0-0>"
                        "dynamic_bitset<type-parameter-0-0, type-parameter-0-1>"
                        "eps_tolerance<type-parameter-0-0>"
                        "erf_asympt_series_t<type-parameter-0-0>"
                        "expint_fraction<type-parameter-0-0>"
                        "expint_i_series<type-parameter-0-0>"
                        "expint_series<type-parameter-0-0>"
                        "expm1_series<type-parameter-0-0>"
                        "find_format_store<typename range_const_iterator<type-parameter-0-0>::type, type-parameter-0-1, type-parameter-0-3>"
                        "find_format_store<typename range_const_iterator<type-parameter-0-0>::type, type-parameter-0-2, type-parameter-0-4>"
                        "find_format_store<typename range_const_iterator<type-parameter-0-1>::type, type-parameter-0-2, type-parameter-0-4>"
                        "find_format_store<typename range_const_iterator<type-parameter-0-1>::type, type-parameter-0-3, type-parameter-0-5>"
                        "find_format_store<typename range_iterator<type-parameter-0-0>::type, type-parameter-0-1, type-parameter-0-3>"
                        "find_format_store<typename range_iterator<type-parameter-0-0>::type, type-parameter-0-2, type-parameter-0-4>"
                        "first_finderF<type-parameter-0-0, type-parameter-0-1>"
                        "first_kday_of_month<type-parameter-0-0>"
                        "from_object<type-parameter-0-0, struct std::__1::integral_constant<_Bool, true> >"
                        "from_object<type-parameter-0-1, struct std::__1::integral_constant<_Bool, true> >"
                        "from_object<type-parameter-0-1, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<1> >::type>"
                        "from_object<type-parameter-0-10, struct std::__1::integral_constant<_Bool, true> >"
                        "from_object<type-parameter-0-10, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<10> >::type>"
                        "from_object<type-parameter-0-10, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<7> >::type>"
                        "from_object<type-parameter-0-10, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<8> >::type>"
                        "from_object<type-parameter-0-10, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<9> >::type>"
                        "from_object<type-parameter-0-10, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue_<7> >::type>"
                        "from_object<type-parameter-0-10, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue_<8> >::type>"
                        "from_object<type-parameter-0-11, struct std::__1::integral_constant<_Bool, true> >"
                        "from_object<type-parameter-0-11, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<10> >::type>"
                        "from_object<type-parameter-0-11, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<11> >::type>"
                        "from_object<type-parameter-0-11, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue_<8> >::type>"
                        "from_object<type-parameter-0-11, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue_<9> >::type>"
                        "from_object<type-parameter-0-12, struct std::__1::integral_constant<_Bool, true> >"
                        "from_object<type-parameter-0-12, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<11> >::type>"
                        "from_object<type-parameter-0-12, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<12> >::type>"
                        "from_object<type-parameter-0-12, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue_<10> >::type>"
                        "from_object<type-parameter-0-12, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue_<9> >::type>"
                        "from_object<type-parameter-0-13, struct std::__1::integral_constant<_Bool, true> >"
                        "from_object<type-parameter-0-13, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<12> >::type>"
                        "from_object<type-parameter-0-13, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<13> >::type>"
                        "from_object<type-parameter-0-13, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue_<10> >::type>"
                        "from_object<type-parameter-0-13, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue_<11> >::type>"
                        "from_object<type-parameter-0-14, struct std::__1::integral_constant<_Bool, true> >"
                        "from_object<type-parameter-0-14, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<13> >::type>"
                        "from_object<type-parameter-0-14, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<14> >::type>"
                        "from_object<type-parameter-0-14, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue_<11> >::type>"
                        "from_object<type-parameter-0-14, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue_<12> >::type>"
                        "from_object<type-parameter-0-15, struct std::__1::integral_constant<_Bool, true> >"
                        "from_object<type-parameter-0-15, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<14> >::type>"
                        "from_object<type-parameter-0-15, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<15> >::type>"
                        "from_object<type-parameter-0-15, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue_<12> >::type>"
                        "from_object<type-parameter-0-15, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue_<13> >::type>"
                        "from_object<type-parameter-0-16, struct std::__1::integral_constant<_Bool, true> >"
                        "from_object<type-parameter-0-16, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<15> >::type>"
                        "from_object<type-parameter-0-16, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<16> >::type>"
                        "from_object<type-parameter-0-16, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue_<13> >::type>"
                        "from_object<type-parameter-0-16, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue_<14> >::type>"
                        "from_object<type-parameter-0-17, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<16> >::type>"
                        "from_object<type-parameter-0-17, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue_<14> >::type>"
                        "from_object<type-parameter-0-17, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue_<15> >::type>"
                        "from_object<type-parameter-0-18, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue_<15> >::type>"
                        "from_object<type-parameter-0-2, struct std::__1::integral_constant<_Bool, true> >"
                        "from_object<type-parameter-0-2, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<0> >::type>"
                        "from_object<type-parameter-0-2, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<1> >::type>"
                        "from_object<type-parameter-0-2, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<2> >::type>"
                        "from_object<type-parameter-0-2, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue_<0> >::type>"
                        "from_object<type-parameter-0-3, struct std::__1::integral_constant<_Bool, true> >"
                        "from_object<type-parameter-0-3, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<0> >::type>"
                        "from_object<type-parameter-0-3, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<1> >::type>"
                        "from_object<type-parameter-0-3, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<2> >::type>"
                        "from_object<type-parameter-0-3, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<3> >::type>"
                        "from_object<type-parameter-0-3, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue_<0> >::type>"
                        "from_object<type-parameter-0-3, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue_<1> >::type>"
                        "from_object<type-parameter-0-4, struct std::__1::integral_constant<_Bool, true> >"
                        "from_object<type-parameter-0-4, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<1> >::type>"
                        "from_object<type-parameter-0-4, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<2> >::type>"
                        "from_object<type-parameter-0-4, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<3> >::type>"
                        "from_object<type-parameter-0-4, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<4> >::type>"
                        "from_object<type-parameter-0-4, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue_<1> >::type>"
                        "from_object<type-parameter-0-4, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue_<2> >::type>"
                        "from_object<type-parameter-0-5, struct std::__1::integral_constant<_Bool, true> >"
                        "from_object<type-parameter-0-5, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<2> >::type>"
                        "from_object<type-parameter-0-5, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<3> >::type>"
                        "from_object<type-parameter-0-5, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<4> >::type>"
                        "from_object<type-parameter-0-5, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<5> >::type>"
                        "from_object<type-parameter-0-5, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue_<2> >::type>"
                        "from_object<type-parameter-0-5, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue_<3> >::type>"
                        "from_object<type-parameter-0-6, struct std::__1::integral_constant<_Bool, true> >"
                        "from_object<type-parameter-0-6, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<3> >::type>"
                        "from_object<type-parameter-0-6, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<4> >::type>"
                        "from_object<type-parameter-0-6, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<5> >::type>"
                        "from_object<type-parameter-0-6, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<6> >::type>"
                        "from_object<type-parameter-0-6, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue_<3> >::type>"
                        "from_object<type-parameter-0-6, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue_<4> >::type>"
                        "from_object<type-parameter-0-7, struct std::__1::integral_constant<_Bool, true> >"
                        "from_object<type-parameter-0-7, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<4> >::type>"
                        "from_object<type-parameter-0-7, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<5> >::type>"
                        "from_object<type-parameter-0-7, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<6> >::type>"
                        "from_object<type-parameter-0-7, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<7> >::type>"
                        "from_object<type-parameter-0-7, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue_<4> >::type>"
                        "from_object<type-parameter-0-7, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue_<5> >::type>"
                        "from_object<type-parameter-0-8, struct std::__1::integral_constant<_Bool, true> >"
                        "from_object<type-parameter-0-8, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<5> >::type>"
                        "from_object<type-parameter-0-8, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<6> >::type>"
                        "from_object<type-parameter-0-8, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<7> >::type>"
                        "from_object<type-parameter-0-8, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<8> >::type>"
                        "from_object<type-parameter-0-8, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue_<5> >::type>"
                        "from_object<type-parameter-0-8, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue_<6> >::type>"
                        "from_object<type-parameter-0-9, struct std::__1::integral_constant<_Bool, true> >"
                        "from_object<type-parameter-0-9, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<6> >::type>"
                        "from_object<type-parameter-0-9, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<7> >::type>"
                        "from_object<type-parameter-0-9, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<8> >::type>"
                        "from_object<type-parameter-0-9, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue<9> >::type>"
                        "from_object<type-parameter-0-9, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue_<6> >::type>"
                        "from_object<type-parameter-0-9, typename DoesNotContain_<type-parameter-0-0, struct pureOutValue_<7> >::type>"
                        "gamma_distribution<type-parameter-0-1>"
                        "gamma_inva_t<type-parameter-0-0, type-parameter-0-1>"
                        "generator_seed_seq<type-parameter-1-0>"
                        "hash<type-parameter-0-0>"
                        "hashed_index_iterator<hashed_index_node<typename type-parameter-0-3::type::node_type>, bucket_array<typename type-parameter-0-3::type::final_allocator_type> >"
                        "ibeta_fraction2_t<type-parameter-0-0>"
                        "ibeta_series_t<type-parameter-0-0>"
                        "ilist_iterator<type-parameter-0-0>"
                        "istreambuf_iterator<type-parameter-0-0, type-parameter-0-1>"
                        "istreambuf_iterator<type-parameter-0-1, char_traits<type-parameter-0-1> >"
                        "iterator<typename type-parameter-0-0::node>"
                        "iterator_range<type-parameter-0-0>"
                        "iterator_range<type-parameter-1-0>"
                        "iterator_range<typename range_const_iterator<type-parameter-0-0>::type>"
                        "iterator_range<typename range_const_iterator<type-parameter-0-1>::type>"
                        "iterator_range<typename range_iterator<type-parameter-0-0>::type>"
                        "iterator_range<typename range_iterator<type-parameter-0-1>::type>"
                        "iterator_range<typename range_iterator<type-parameter-1-1>::type>"
                        "last_finderF<type-parameter-0-0, type-parameter-0-1>"
                        "last_kday_of_month<type-parameter-0-0>"
                        "less_equal<typename hashed_index_node<typename type-parameter-0-3::type::node_type>::impl_type::pointer>"
                        "less_equal<typename prevent_eti<type-parameter-0-0, typename rebind_to<type-parameter-0-0, hashed_index_node_impl<Allocator> >::type>::type::pointer>"
                        "lexical_stream_limited_src<typename lexical_cast_stream_traits<type-parameter-0-1, type-parameter-0-0>::char_type, typename lexical_cast_stream_traits<type-parameter-0-1, type-parameter-0-0>::traits, stream_trait::requires_stringbuf>"
                        "list1<const type-parameter-1-0 &>"
                        "list1<type-parameter-1-0 &>"
                        "list2<const type-parameter-1-0 &, const type-parameter-1-1 &>"
                        "list2<const type-parameter-1-0 &, type-parameter-1-1 &>"
                        "list2<type-parameter-1-0 &, const type-parameter-1-1 &>"
                        "list2<type-parameter-1-0 &, type-parameter-1-1 &>"
                        "list3<const type-parameter-1-0 &, const type-parameter-1-1 &, const type-parameter-1-2 &>"
                        "list3<type-parameter-1-0 &, type-parameter-1-1 &, type-parameter-1-2 &>"
                        "list4<const type-parameter-1-0 &, const type-parameter-1-1 &, const type-parameter-1-2 &, const type-parameter-1-3 &>"
                        "list4<type-parameter-1-0 &, type-parameter-1-1 &, type-parameter-1-2 &, type-parameter-1-3 &>"
                        "list5<const type-parameter-1-0 &, const type-parameter-1-1 &, const type-parameter-1-2 &, const type-parameter-1-3 &, const type-parameter-1-4 &>"
                        "list5<type-parameter-1-0 &, type-parameter-1-1 &, type-parameter-1-2 &, type-parameter-1-3 &, type-parameter-1-4 &>"
                        "list6<const type-parameter-1-0 &, const type-parameter-1-1 &, const type-parameter-1-2 &, const type-parameter-1-3 &, const type-parameter-1-4 &, const type-parameter-1-5 &>"
                        "list6<type-parameter-1-0 &, type-parameter-1-1 &, type-parameter-1-2 &, type-parameter-1-3 &, type-parameter-1-4 &, type-parameter-1-5 &>"
                        "list7<const type-parameter-1-0 &, const type-parameter-1-1 &, const type-parameter-1-2 &, const type-parameter-1-3 &, const type-parameter-1-4 &, const type-parameter-1-5 &, const type-parameter-1-6 &>"
                        "list7<type-parameter-1-0 &, type-parameter-1-1 &, type-parameter-1-2 &, type-parameter-1-3 &, type-parameter-1-4 &, type-parameter-1-5 &, type-parameter-1-6 &>"
                        "list8<const type-parameter-1-0 &, const type-parameter-1-1 &, const type-parameter-1-2 &, const type-parameter-1-3 &, const type-parameter-1-4 &, const type-parameter-1-5 &, const type-parameter-1-6 &, const type-parameter-1-7 &>"
                        "list8<type-parameter-1-0 &, type-parameter-1-1 &, type-parameter-1-2 &, type-parameter-1-3 &, type-parameter-1-4 &, type-parameter-1-5 &, type-parameter-1-6 &, type-parameter-1-7 &>"
                        "list9<const type-parameter-1-0 &, const type-parameter-1-1 &, const type-parameter-1-2 &, const type-parameter-1-3 &, const type-parameter-1-4 &, const type-parameter-1-5 &, const type-parameter-1-6 &, const type-parameter-1-7 &, const type-parameter-1-8 &>"
                        "list9<type-parameter-1-0 &, type-parameter-1-1 &, type-parameter-1-2 &, type-parameter-1-3 &, type-parameter-1-4 &, type-parameter-1-5 &, type-parameter-1-6 &, type-parameter-1-7 &, type-parameter-1-8 &>"
                        "list<pair<shared_ptr<const type-parameter-0-1>, const type-parameter-0-0 *>, allocator<pair<shared_ptr<const type-parameter-0-1>, const type-parameter-0-0 *> > >"
                        "log1p_series<type-parameter-0-0>"
                        "log1p_series<typename promote_args<type-parameter-0-0, float, float, float, float, float>::type>"
                        "lower_incomplete_gamma_series<type-parameter-0-0>"
                        "map<typename type-parameter-0-1::stored_vertex *, typename type-parameter-0-1::stored_vertex *, less<typename type-parameter-0-1::stored_vertex *>, allocator<pair<typename type-parameter-0-1::stored_vertex *const, typename type-parameter-0-1::stored_vertex *> > >"
                        "match_results<const type-parameter-0-0 *, typename vector<sub_match<const type-parameter-0-0 *>, allocator<sub_match<const type-parameter-0-0 *> > >::allocator_type>"
                        "match_results<type-parameter-0-0, typename vector<sub_match<type-parameter-0-0>, allocator<sub_match<type-parameter-0-0> > >::allocator_type>"
                        "match_results<type-parameter-0-1, typename match_results<type-parameter-0-1, typename vector<sub_match<type-parameter-0-1>, allocator<sub_match<type-parameter-0-1> > >::allocator_type>::allocator_type>"
                        "match_results<type-parameter-0-1, typename vector<sub_match<type-parameter-0-1>, allocator<sub_match<type-parameter-0-1> > >::allocator_type>"
                        "match_results<typename basic_string<type-parameter-0-2, type-parameter-0-0, type-parameter-0-1>::const_iterator, typename vector<sub_match<typename basic_string<type-parameter-0-2, type-parameter-0-0, type-parameter-0-1>::const_iterator>, allocator<sub_match<typename basic_string<type-parameter-0-2, type-parameter-0-0, type-parameter-0-1>::const_iterator> > >::allocator_type>"
                        "move_nodes<typename rebind_wrap<typename type-parameter-0-0::allocator, typename type-parameter-0-0::node>::type>"
                        "multiple_values<type-parameter-0-0>"
                        "multiple_values<type-parameter-0-1>"
                        "node_constructor<typename rebind_wrap<typename type-parameter-0-0::allocator, typename type-parameter-0-0::node>::type>"
                        "node_holder<typename rebind_wrap<typename type-parameter-0-0::allocator, typename type-parameter-0-0::node>::type>"
                        "non_blocking_adapter<type-parameter-0-1>"
                        "nth_kday_of_month<type-parameter-0-0>"
                        "optional<type-parameter-0-1>"
                        "optional<typename type-parameter-0-1::vertex_descriptor>"
                        "ostreambuf_iterator<type-parameter-0-0, char_traits<type-parameter-0-0> >"
                        "ostreambuf_iterator<type-parameter-0-0, type-parameter-0-1>"
                        "ostreambuf_iterator<type-parameter-0-1, char_traits<type-parameter-0-1> >"
                        "ostreambuf_iterator<type-parameter-0-2, char_traits<type-parameter-0-2> >"
                        "pair<__hash_iterator<typename allocator_traits<typename allocator_traits<type-parameter-0-3>::rebind_alloc<__hash_node<type-parameter-0-0, typename allocator_traits<type-parameter-0-3>::void_pointer> > >::pointer>, _Bool>"
                        "pair<__map_iterator<typename __tree<__value_type<type-parameter-0-0, type-parameter-0-1>, __map_value_compare<type-parameter-0-0, __value_type<type-parameter-0-0, type-parameter-0-1>, type-parameter-0-2, is_empty<type-parameter-0-2>::value && !__is_final(type-parameter-0-2)>, typename allocator_traits<type-parameter-0-3>::rebind_alloc<__value_type<type-parameter-0-0, type-parameter-0-1> > >::iterator>, _Bool>"
                        "pair<__tree_iterator<type-parameter-0-0, typename allocator_traits<typename allocator_traits<type-parameter-0-2>::rebind_alloc<__tree_node<type-parameter-0-0, typename allocator_traits<type-parameter-0-2>::void_pointer> > >::pointer, typename allocator_traits<type-parameter-0-2>::difference_type>, _Bool>"
                        "pair<__tree_iterator<type-parameter-0-0, typename allocator_traits<typename allocator_traits<type-parameter-0-2>::rebind_alloc<__tree_node<type-parameter-0-0, typename allocator_traits<type-parameter-0-2>::void_pointer> > >::pointer, typename allocator_traits<type-parameter-0-2>::difference_type>, __tree_iterator<type-parameter-0-0, typename allocator_traits<typename allocator_traits<type-parameter-0-2>::rebind_alloc<__tree_node<type-parameter-0-0, typename allocator_traits<type-parameter-0-2>::void_pointer> > >::pointer, typename allocator_traits<type-parameter-0-2>::difference_type> >"
                        "pair<const type-parameter-0-0 *, const type-parameter-0-0 *>"
                        "pair<type-parameter-0-0 *, long>"
                        "pair<type-parameter-0-0, type-parameter-0-0>"
                        "pair<type-parameter-0-1, type-parameter-0-0>"
                        "pair<type-parameter-0-1, type-parameter-0-1>"
                        "pair<typename DenseMap<ValueMapCallbackVH<type-parameter-0-0, type-parameter-0-1, type-parameter-0-2>, type-parameter-0-1, DenseMapInfo<ValueMapCallbackVH<type-parameter-0-0, type-parameter-0-1, type-parameter-0-2> > >::iterator, _Bool>"
                        "pair<typename char_type_of<type-parameter-0-0>::type *, typename char_type_of<type-parameter-0-0>::type *>"
                        "pair<typename graph_traits<type-parameter-0-0>::vertex_iterator, typename graph_traits<type-parameter-0-0>::vertex_iterator>"
                        "pair<typename iterator_traits<type-parameter-0-0>::value_type *, long>"
                        "pair<typename iterator_traits<type-parameter-0-1>::value_type *, long>"
                        "pair<typename multi_index_base_type<type-parameter-0-0, type-parameter-0-1, type-parameter-0-2>::type::node_type *, _Bool>"
                        "pair<typename type-parameter-0-0::out_edge_iterator, typename type-parameter-0-0::out_edge_iterator>"
                        "pair<typename type-parameter-0-3::type::final_node_type *, _Bool>"
                        "pair<typename unordered_multiset<type-parameter-0-0, type-parameter-0-1, type-parameter-0-2, type-parameter-0-3>::const_iterator, typename unordered_multiset<type-parameter-0-0, type-parameter-0-1, type-parameter-0-2, type-parameter-0-3>::const_iterator>"
                        "parse_match_result<type-parameter-0-0>"
                        "parser_buf<type-parameter-0-0, char_traits<type-parameter-0-0> >"
                        "peekable_source<type-parameter-1-0>"
                        "perl_matcher<type-parameter-0-0, type-parameter-0-1, type-parameter-0-3>"
                        "perl_matcher<type-parameter-0-0, typename match_results<type-parameter-0-0, typename vector<sub_match<type-parameter-0-0>, allocator<sub_match<type-parameter-0-0> > >::allocator_type>::allocator_type, type-parameter-0-2>"
                        "perl_matcher<type-parameter-0-1, typename match_results<type-parameter-0-1, typename vector<sub_match<type-parameter-0-1>, allocator<sub_match<type-parameter-0-1> > >::allocator_type>::allocator_type, type-parameter-0-3>"
                        "poisson_distribution<type-parameter-0-0, type-parameter-0-1>"
                        "regex_iterator<type-parameter-0-1, type-parameter-0-3, type-parameter-0-2>"
                        "scoped_ptr<type-parameter-0-5>"
                        "serialization_version<typename multi_index_base_type<type-parameter-0-0, type-parameter-0-1, type-parameter-0-2>::type::value_type>"
                        "set_hash_functions<typename type-parameter-0-0::hasher, typename type-parameter-0-0::key_equal>"
                        "shared_ptr<basic_regex_implementation<type-parameter-0-0, type-parameter-0-1> >"
                        "shared_ptr<const type-parameter-0-1>"
                        "shared_ptr<type-parameter-0-0>"
                        "small_gamma2_series<type-parameter-0-0>"
                        "smart_ptr<Iterator<typename BeginReturnType<type-parameter-0-1, type-parameter-0-2>::type, type-parameter-0-0> >"
                        "smart_ptr<Wrapper<const type-parameter-0-0, const type-parameter-0-0 *> >"
                        "smart_ptr<Wrapper<const type-parameter-0-0, unique_ptr<const type-parameter-0-0, default_delete<const type-parameter-0-0> > > >"
                        "smart_ptr<Wrapper<type-parameter-0-0, type-parameter-0-0 *> >"
                        "smart_ptr<Wrapper<type-parameter-0-0, type-parameter-0-0 *const> >"
                        "smart_ptr<Wrapper<type-parameter-0-0, type-parameter-0-1> >"
                        "smart_ptr<Wrapper<type-parameter-0-0, unique_ptr<type-parameter-0-0, default_delete<type-parameter-0-0> > > >"
                        "smart_ptr<Wrapper<type-parameter-0-2, type-parameter-0-1> >"
                        "smart_ptr<const type-parameter-1-0>"
                        "smart_ptr<type-parameter-0-0>"
                        "smart_ptr<type-parameter-0-1>"
                        "smart_ptr<type-parameter-0-2>"
                        "smart_ptr<type-parameter-1-0>"
                        "specific_attr_iterator<type-parameter-0-0, type-parameter-0-1>"
                        "sph_bessel_j_small_z_series_term<type-parameter-0-0, type-parameter-0-1>"
                        "split_pred<type-parameter-0-0, type-parameter-0-1, type-parameter-0-2, type-parameter-0-3>"
                        "stack_construct<type-parameter-0-0, typename type-parameter-0-1::value_type>"
                        "string_out_iterator<basic_string<type-parameter-0-1, char_traits<type-parameter-0-1>, allocator<type-parameter-0-1> > >"
                        "string_out_iterator<basic_string<typename regex_iterator_traits<type-parameter-0-0>::value_type, char_traits<typename regex_iterator_traits<type-parameter-0-0>::value_type>, allocator<typename regex_iterator_traits<type-parameter-0-0>::value_type> > >"
                        "sub_match<type-parameter-0-0>"
                        "tokenizer<char_separator<type-parameter-0-1, typename basic_string<type-parameter-0-1, char_traits<type-parameter-0-1>, allocator<type-parameter-0-1> >::traits_type>, typename basic_string<type-parameter-0-1, char_traits<type-parameter-0-1>, allocator<type-parameter-0-1> >::const_iterator, basic_string<type-parameter-0-1, char_traits<type-parameter-0-1>, allocator<type-parameter-0-1> > >"
                        "transform_iterator<copy_iterator_rangeF<typename range_value<type-parameter-0-0>::type, typename range_iterator<type-parameter-0-1>::type>, find_iterator<typename range_iterator<type-parameter-0-1>::type>, struct boost::use_default, struct boost::use_default>"
                        "transform_iterator<copy_iterator_rangeF<typename range_value<type-parameter-0-0>::type, typename range_iterator<type-parameter-0-1>::type>, split_iterator<typename range_iterator<type-parameter-0-1>::type>, struct boost::use_default, struct boost::use_default>"
                        "uniform_int_float<type-parameter-0-0>"
                        "uniform_real<type-parameter-0-0>"
                        "unique_lock<type-parameter-0-0>"
                        "unique_lock<type-parameter-0-1>"
                        "unique_ptr<__base<type-parameter-0-0 (type-parameter-0-1...)>, __allocator_destructor<allocator<__func<type-parameter-1-0, allocator<type-parameter-1-0>, type-parameter-0-0 (type-parameter-0-1...)> > > >"
                        "unique_ptr<__base<type-parameter-0-0 (type-parameter-0-1...)>, __allocator_destructor<typename allocator_traits<type-parameter-1-1>::rebind_alloc<__func<type-parameter-1-0, type-parameter-1-1, type-parameter-0-0 (type-parameter-0-1...)> > > >"
                        "unique_ptr<__func<type-parameter-0-0, type-parameter-0-1, type-parameter-0-2 (type-parameter-0-3...)>, __allocator_destructor<typename typename type-parameter-0-1::rebind<__func<type-parameter-0-0, type-parameter-0-1, type-parameter-0-2 (type-parameter-0-3...)> >::other> >"
                        "unique_ptr<__hash_node<type-parameter-0-0, typename allocator_traits<type-parameter-0-3>::void_pointer>, __hash_node_destructor<typename allocator_traits<type-parameter-0-3>::rebind_alloc<__hash_node<type-parameter-0-0, typename allocator_traits<type-parameter-0-3>::void_pointer> > > >"
                        "unique_ptr<__shared_ptr_emplace<type-parameter-0-0, allocator<type-parameter-0-0> >, __allocator_destructor<allocator<__shared_ptr_emplace<type-parameter-0-0, allocator<type-parameter-0-0> > > > >"
                        "unique_ptr<__shared_ptr_emplace<type-parameter-0-0, type-parameter-1-0>, __allocator_destructor<typename typename type-parameter-1-0::rebind<__shared_ptr_emplace<type-parameter-0-0, type-parameter-1-0> >::other> >"
                        "unique_ptr<__shared_ptr_pointer<nullptr_t, type-parameter-1-0, type-parameter-1-1>, __allocator_destructor<typename typename type-parameter-1-1::rebind<__shared_ptr_pointer<nullptr_t, type-parameter-1-0, type-parameter-1-1> >::other> >"
                        "unique_ptr<__shared_ptr_pointer<type-parameter-1-0 *, type-parameter-1-1, type-parameter-1-2>, __allocator_destructor<typename typename type-parameter-1-2::rebind<__shared_ptr_pointer<type-parameter-1-0 *, type-parameter-1-1, type-parameter-1-2> >::other> >"
                        "unique_ptr<__tree_node<type-parameter-0-0, typename allocator_traits<type-parameter-0-2>::void_pointer>, __tree_node_destructor<typename allocator_traits<type-parameter-0-2>::rebind_alloc<__tree_node<type-parameter-0-0, typename allocator_traits<type-parameter-0-2>::void_pointer> > > >"
                        "unique_ptr<type-parameter-0-0, void (*)(void *)>"
                        "unique_ptr<type-parameter-1-0, default_delete<type-parameter-1-0> >"
                        "unique_ptr<typename __list_imp<type-parameter-0-0, type-parameter-0-1>::__node, __allocator_destructor<typename __list_imp<type-parameter-0-0, type-parameter-0-1>::__node_allocator> >"
                        "unique_ptr<typename __tree<__value_type<type-parameter-0-0, type-parameter-0-1>, __map_value_compare<type-parameter-0-0, __value_type<type-parameter-0-0, type-parameter-0-1>, type-parameter-0-2, is_empty<type-parameter-0-2>::value && !__is_final(type-parameter-0-2)>, typename allocator_traits<type-parameter-0-3>::rebind_alloc<__value_type<type-parameter-0-0, type-parameter-0-1> > >::__node, __map_node_destructor<typename __tree<__value_type<type-parameter-0-0, type-parameter-0-1>, __map_value_compare<type-parameter-0-0, __value_type<type-parameter-0-0, type-parameter-0-1>, type-parameter-0-2, is_empty<type-parameter-0-2>::value && !__is_final(type-parameter-0-2)>, typename allocator_traits<type-parameter-0-3>::rebind_alloc<__value_type<type-parameter-0-0, type-parameter-0-1> > >::__node_allocator> >"
                        "unique_ptr<typename iterator_traits<type-parameter-0-0>::value_type, struct std::__1::__return_temporary_buffer>"
                        "unique_ptr<typename iterator_traits<type-parameter-0-1>::value_type, struct std::__1::__destruct_n &>"
                        "unique_ptr<typename iterator_traits<type-parameter-0-1>::value_type, struct std::__1::__return_temporary_buffer>"
                        "upper_incomplete_gamma_fract<type-parameter-0-0>"
                        "vector<basic_string<type-parameter-0-0, char_traits<type-parameter-0-0>, allocator<type-parameter-0-0> >, allocator<basic_string<type-parameter-0-0, char_traits<type-parameter-0-0>, allocator<type-parameter-0-0> > > >"
                        "vector<basic_string<type-parameter-0-1, char_traits<type-parameter-0-1>, allocator<type-parameter-0-1> >, allocator<basic_string<type-parameter-0-1, char_traits<type-parameter-0-1>, allocator<type-parameter-0-1> > > >"
                        "vector<pair<type-parameter-0-1, type-parameter-0-0>, allocator<pair<type-parameter-0-1, type-parameter-0-0> > >"
                        "vector<pair<typename graph_traits<type-parameter-0-0>::vertex_descriptor, pair<typename graph_traits<type-parameter-0-0>::out_edge_iterator, typename graph_traits<type-parameter-0-0>::out_edge_iterator> >, allocator<pair<typename graph_traits<type-parameter-0-0>::vertex_descriptor, pair<typename graph_traits<type-parameter-0-0>::out_edge_iterator, typename graph_traits<type-parameter-0-0>::out_edge_iterator> > > >"
                        "vector<type-parameter-0-0, type-parameter-0-1>"
                        "vector<type-parameter-0-1, allocator<type-parameter-0-1> >"
                        "vector<typename regex_iterator_traits<type-parameter-0-0>::value_type, allocator<typename regex_iterator_traits<type-parameter-0-0>::value_type> >"
                        "vector<typename type-parameter-0-0::EdgeIter, allocator<typename type-parameter-0-0::EdgeIter> >"
                        "vector<typename type-parameter-0-2::char_type, allocator<typename type-parameter-0-2::char_type> >"
                        "wrapping_int<typename type-parameter-0-0::int_type, ticks_per_day>"

                        "__bit_iterator<__bitset<_N_words, _Size>, true, 0>"
                        "aligned<sizeof(Derived)>"
                        "assign_nodes<table<Types> >"
                        "asttooling::FOO"
                        "bitset<_Size>"
                        "boost::char_separator"
                        "boost::date_time::month_functor"
                        "boost::date_time::wrapping_int2"
                        "boost::dynamic_bitset"
                        "boost::exception_detail::refcount_ptr"
                        "boost::hash"
                        "boost::iostreams::back_insert_device"
                        "boost::iostreams::detail::basic_buffer"
                        "boost::iostreams::stream_buffer"
                        "boost::move_iterator"
                        "boost::program_options::basic_parsed_options"
                        "boost::random::detail::generator_seed_seq"
                        "boost::random::linear_congruential_engine"
                        "boost::shared_ptr"
                        "boost::token_iterator"
                        "boost::tokenizer"
                        "clang::ArrayType_cannot_be_used_with_getAs"
                        "clang::CanQual"
                        "clang::RedeclarableTemplateDecl::SpecIterator"
                        "class std::__1::__tree_node_base<void *> *"
                        "llvm::DenseMapIterator"
                        "llvm::FoldingSetVectorIterator"
                        "llvm::OwningPtr"
                        "llvm::SmallString"
                        "llvm::SwitchInst::CaseIteratorT"
                        "llvm::generic_gep_type_iterator"
                        "llvm::mapped_iterator"
                        "llvm::value_use_iterator"
                        "move_assign_nodes<table<Types> >"
                        "smart_ptr<Iterator<IT, Policy> >"
                        "smart_ptr<Wrapper<OT, HolderType> >"
                        "std::__1::__list_iterator"
                        "std::__1::__split_buffer"
                        "std::__1::basic_ostringstream"
                        "std::__1::fpos"
                        "std::__1::istreambuf_iterator"
                        "std::__1::shared_ptr"
                        "std::__1::uniform_int_distribution"
                        "std::__1::unique_lock"
                        "std::__1::unique_ptr"
                        "value_to_type<is_numeric<BlockInputIterator>::value>"



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
                        "boost::detail::make_property_map_from_arg_pack_gen<struct boost::graph::keywords::tag::color_map,enum boost::default_color_type>"
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
                        "clang::ast_matchers::internal::ArgumentAdaptingMatcherFunc<TEMPLATE,struct clang::ast_matchers::internal::TypeList<class clang::Decl, class clang::Stmt, void, void>,struct clang::ast_matchers::internal::TypeList<class clang::Decl, class clang::Stmt, void, void>>"
                        "clang::ast_matchers::internal::ArgumentAdaptingMatcherFunc<TEMPLATE,struct clang::ast_matchers::internal::TypeList<struct clang::ast_matchers::internal::TypeList<class clang::Decl, class clang::Stmt, class clang::NestedNameSpecifier, class clang::NestedNameSpecifierLoc>, struct clang::ast_matchers::internal::TypeList<class clang::QualType, class clang::Type, class clang::TypeLoc, class clang::CXXCtorInitializer>, void, void>,struct clang::ast_matchers::internal::TypeList<struct clang::ast_matchers::internal::TypeList<class clang::Decl, class clang::Stmt, class clang::NestedNameSpecifier, void>, struct clang::ast_matchers::internal::TypeList<class clang::NestedNameSpecifierLoc, class clang::TypeLoc, class clang::QualType, void>, void, void>>"
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
                        "std::__1::__ignore_t<unsigned char>"


                        "clbind::detail::static_scope" ;; This might need to be handled differently

                        )))


(defun check-if-template-specialization-ignore (key)
  (if (gethash key +template-specialization-ignores+)
      nil
      (progn
        (if (search "type-parameter-" key)
            (warn "Check if the following template-specialization class is contains-smart-pointers-p - if not, make it an ignore~%OBVIOUS-T-S-EXCEPTION ~a" key)
            (warn "Check if the following template-specialization class is contains-smart-pointers-p - if not, make it an ignore~%T-S-IGNORE ~a" key))
        :maybe)))

(defmethod contains-smart-pointers-p ((x unclassified-template-specialization-ctype) anal)
  (let ((desc (unclassified-template-specialization-ctype-description x)))
    (check-if-template-specialization-ignore desc)))



(defmethod contains-smart-pointers-p ((x gc-template-argument) anal) (contains-smart-pointers-p (gc-template-argument-ctype x) anal))
(defmethod contains-smart-pointers-p ((x builtin-ctype) anal) nil)
(defmethod contains-smart-pointers-p ((x gcholder) anal) t)
(defmethod contains-smart-pointers-p ((x rooted-gcholder) anal) t)
(defmethod contains-smart-pointers-p ((x pointer-ctype) anal) (contains-smart-pointers-p (pointer-ctype-pointee x) anal))
    


(defparameter *all-housekeeping-classes* nil)


(defconstant +housekeeping-class-ignores+
  (build-ignore-table '("<anonymous>"
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
                        "std::__1::__map_const_iterator<class std::__1::__tree_const_iterator<union std::__1::__value_type<class mem::smart_ptr<class core::Instance_O>, int>, class std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Instance_O>, int>, void *> *, long>>"
                        "std::__1::__map_const_iterator<class std::__1::__tree_const_iterator<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::Instance_O> >, class std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::Instance_O> >, void *> *, long>>"
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
                        "std::__1::__map_iterator<class std::__1::__tree_iterator<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::Instance_O> >, class std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::Instance_O> >, void *> *, long>>"
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
        (warn "Check if the following class is rootable and if it is - why isn't it in the housekeeping classes??~%H-C-IGNORE ~a" key)
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


(defmethod contains-smart-pointers-p ((x record-ctype) anal)
  (let ((hc (rootable-housekeeping-class-p (record-ctype-key x) anal)))
    (cond
      ((null hc) nil)
      ((eq hc :maybe) :maybe)
      (t (contains-smart-pointers-p hc anal)))))



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
  :maybe)

(defmethod on-stack-has-smart-pointers-on-heap-p ((x auto-ctype) anal)
  (check-if-auto-ctype-ignore (auto-ctype-description x)))

(defmethod on-stack-has-smart-pointers-on-heap-p ((x pointer-ctype) anal)
  (contains-smart-pointers-p x anal))

(defmethod on-stack-has-smart-pointers-on-heap-p ((x unclassified-ctype) anal)
  (warn "Add support for on-stack-has-smart-pointers-on-heap-p (x unclassified-ctype) ~a" x)
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
  (setf (gethash (housekeeping-class-key x) (analysis-housekeeping-class-stored-on-stack anal)) x)
#||
  ;; - old way - we cached the result - but I don't think it worked properly
  (analysis-housekeeping-class-on-stack-has-smart-pointers-on-heap-p x anal)
||#
  (let* (result
         (project (analysis-project anal))
         (all-classes (project-housekeeping-classes project)))
    (loop :for base-name :in (housekeeping-class-bases x)
       :do (when base-name
             (let ((base (gethash base-name all-classes)))
               (when base
                 (setq result (or result (on-stack-has-smart-pointers-on-heap-p base anal)))))))
    (loop :for field :in (housekeeping-class-fields x)
       :do (setq result (or result (on-stack-has-smart-pointers-on-heap-p field anal))))
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
      (setq sp (or sp (contains-smart-pointers-p (elt (stl-container-arguments x) i) anal))))
    sp))


(defmethod on-stack-has-smart-pointers-on-heap-p ((x builtin-ctype) anal) nil)

(defmethod on-stack-has-smart-pointers-on-heap-p ((x dependent-template-specialization-ctype) anal)
  (let ((key (dependent-template-specialization-ctype-description x)))
    (cond
      ((string= key "typename __alloc_traits::rebind_alloc<_FF>") nil)
      (t (warn "Check if on-stack-has-smart-pointers-on-heap-p for ~a" key)))))



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
          (t "STLMAP_UNKNOWN_TYPE")))
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


(defmethod macro-name ((x stl-vector) fix)
  (handler-case
      (let* ((arg (container-argument x 0))
             (type-info (gc-template-argument-ctype arg)))
        (cond
          ((is-smart-p arg) (maybe-macro-name "STLVECTOR_FIX" fix))
          (t (maybe-macro-name "STLVECTOR_HANDLE_CTYPE" fix))))
    (simple-type-error (err)
      "ERROR_UNHANDLED_STLMAP_TYPE")
    ))


(defmethod macro-name ((x smart-ptr-ctype) fix) (maybe-macro-name "SMART_PTR_FIX" fix))
(defmethod macro-name ((x weak-smart-ptr-ctype) fix) (maybe-macro-name "WEAK_SMART_PTR_FIX" fix))
(defmethod macro-name ((x unclassified-ctype) fix) "IGNORE")
(defmethod macro-name ((x cloned-ctype) fix) "IGNORE")
(defmethod macro-name ((x ctype) fix) (substitute #\_ #\- (maybe-macro-name (format nil "HANDLE_~a" (type-of x)) fix)))






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


(defun is-or-inherits-from-stack-root (class all-classes)
  (if (string= (housekeeping-class-key class) "gctools::StackRoot")
      t
      (loop :for base-class-name :in (housekeeping-class-bases class)
         :for base-class = (gethash base-class-name all-classes)
         :for inherits = (if base-class
                             (is-or-inherits-from-stack-root base-class all-classes)
                             (check-if-housekeeping-class-ignore base-class-name))
         :until inherits
         :finally (return inherits))
  ))








(defun ctype-name (classid) classid)


(defun class-enum-name (classid)
  (let* ((raw-name (copy-seq (ctype-name classid)))
         (name0 (nsubstitute-if #\_ (lambda (c) (member c '(#\SPACE #\, #\< #\> #\: ))) raw-name))
         (name (nsubstitute #\P #\* name0))
         )
    (if (match-right name "::Wrapper")
        (progn
          (format nil "KIND_~a" name)
          #+use-breaks(break "Check name"))
        (format nil "KIND_~a" name))))



(defun code-for-instance-var (output-stream ptr-name instance-var type-info &optional fix)
  (let ((macro-name (macro-name type-info fix)))
    (if macro-name
      (format output-stream "    ~A(~A->~A); /* ~a */~%" macro-name ptr-name instance-var type-info)
      (format output-stream "    NO_MACRO_AVAILABLE(~a->~a); /* ~a */~%" ptr-name instance-var type-info)
      )))


(defun code-for-global-var (stream type-info global-var location fix)
  (format stream "    ~a(~a); /* ~a ~a */~%" (macro-name type-info fix) global-var location type-info)
)

(defun generate-kind-enum (anal &key (output-stream t))
  (with-accessors ((builtins organized-gcobjects-builtins)
                   (wrappers organized-gcobjects-wrappers)
                   (iterators organized-gcobjects-iterators))
      (analysis-organized-gcobjects anal)
    (format output-stream "enum { KIND_null = 0, ~%")
    (dolist (builtin-entry builtins)
      (let ((enum-name (class-enum-name (gcobject-subclass-key builtin-entry))))
        (format output-stream "~A = ~a,~%" enum-name (gcobject-subclass-kind-id builtin-entry))))
    (format output-stream "KIND_max = ~a,~%" (gcobject-kind-id-builtins))
    (format output-stream "KIND_fwd = ~a,~%" (gcobject-kind-id-builtins))
    (format output-stream "KIND_fwd2 = ~a,~%" (gcobject-kind-id-builtins))
    (format output-stream "KIND_pad1 = ~a,~%" (gcobject-kind-id-builtins))
    (format output-stream "KIND_pad = ~a,~%" (gcobject-kind-id-builtins))
    (dolist (entry wrappers)
      (let ((enum-name (class-enum-name (gcobject-subclass-key entry))))
        (format output-stream "~A = ~a,~%" enum-name (gcobject-subclass-kind-id entry))))
    (dolist (entry iterators)
      (let ((enum-name (class-enum-name (gcobject-subclass-key entry))))
        (format output-stream "~A = ~a,~%" enum-name (gcobject-subclass-kind-id entry))))
    (format output-stream "}~%" )
    ))


(defun generate-gc-info (anal &key (output-stream t))
  (with-accessors ((builtins organized-gcobjects-builtins)
                   (wrappers organized-gcobjects-wrappers)
                   (iterators organized-gcobjects-iterators))
      (analysis-organized-gcobjects anal)
    (dolist (entry builtins)
      (let* ((classid (gcobject-subclass-key entry))
             (enum-name (class-enum-name classid)))
        (format output-stream "template <> class gctools::GcInfo<~A> {~%" (ctype-name classid))
        (format output-stream "public:~%")
        (format output-stream "  static gctools::GcKindEnum const Kind = gctools::~a ;~%" enum-name)
        (format output-stream "};~%") ))
    (dolist (entry wrappers)
      (let* ((classid (gcobject-subclass-key entry))
             (enum-name (class-enum-name classid)))
        (format output-stream "template <> class gctools::GcInfo<~A> {~%" (ctype-name classid))
        (format output-stream "public:~%")
        (format output-stream "  static gctools::GcKindEnum const Kind = gctools::~a ;~%" enum-name)
        (format output-stream "};~%") ))
    (dolist (entry iterators)
      (let* ((classid (gcobject-subclass-key entry))
             (enum-name (class-enum-name classid)))
        (format output-stream "template <> class gctools::GcInfo<~A> {~%" (ctype-name classid))
        (format output-stream "public:~%")
        (format output-stream "  static gctools::GcKindEnum const Kind = gctools::~a ;~%" enum-name)
        (format output-stream "};~%") ))
    ))



(defun generate-kind-name-map (anal &key (output-stream t))
  (with-accessors ((builtins organized-gcobjects-builtins)
                   (wrappers organized-gcobjects-wrappers)
                   (iterators organized-gcobjects-iterators))
      (analysis-organized-gcobjects anal)
    (dolist (entry builtins)
      (let* ((classid (gcobject-subclass-key entry))
             (enum-name (class-enum-name classid)))
        (format output-stream "   case ~A: return \"~A\";~%" enum-name enum-name)))
    (dolist (entry wrappers)
      (let* ((classid (gcobject-subclass-key entry))
             (enum-name (class-enum-name classid)))
        (format output-stream "   case ~A: return \"~A\";~%" enum-name enum-name)))
    (dolist (entry iterators)
      (let* ((classid (gcobject-subclass-key entry))
             (enum-name (class-enum-name classid)))
        (format output-stream "   case ~A: return \"~A\";~%" enum-name enum-name)))
  ))


(defconstant +ptr-name+ "obj_gc_safe"
  "This variable is used to temporarily hold a pointer to a Wrapper<...> object - we want the GC to ignore it")


(defun build-mps-scan-for-one-family (family-name kinds anal &optional (fout t))
  (format fout "#if defined(GC_SCAN_METHOD_~a)~%" family-name)
  (dolist (entry kinds)
    (let* ((classid (gcobject-subclass-key entry))
           (enum-name (class-enum-name classid))
           (inheritance (project-gcobjects (analysis-project anal)))
           (all-instance-variables (gather-instance-variables classid inheritance)))
      (gclog "build-mps-scan-for-one-family -> inheritance classid[~a]  value[~a]~%" (ctype-name classid) value)
      (format fout "case ~a: {~%" enum-name)
      (format fout "    ~A* ~A = reinterpret_cast<~A*>(obj_ptr);~%" (ctype-name classid) +ptr-name+ (ctype-name classid))
      (format fout "    DEBUG_SCAN_OBJECT(~A);~%" +ptr-name+)
      (maphash #'(lambda (k v &aux (csp (contains-smart-pointers-p (instance-variable-ctype v) anal)))
                   (cond
                     ((null csp))
                     ((eq csp :maybe) (code-for-instance-var fout +ptr-name+ k (instance-variable-ctype v) :maybe))
                     (t (code-for-instance-var fout +ptr-name+ k (instance-variable-ctype v) t))))
               all-instance-variables)
      (format fout "    typedef ~A type_~A;~%" (ctype-name classid) enum-name)
      (format fout "    base = (char*)base + ALIGN(SIZEOF_WITH_HEADER(type_~A));~%" enum-name)
      (format fout "} break;~%")
      ))
  (format fout "#endif // defined(GC_SCAN_METHOD_~a)~%" family-name)
  )


(defun build-mps-scan (anal &key ((:housekeeping-classes *all-housekeeping-classes*) (project-housekeeping-classes (analysis-project anal))) (output-stream t))
  (with-accessors ((builtins organized-gcobjects-builtins)
                   (wrappers organized-gcobjects-wrappers)
                   (iterators organized-gcobjects-iterators))
      (analysis-organized-gcobjects anal)
    (build-mps-scan-for-one-family "BUILTINS" builtins anal output-stream)
    (build-mps-scan-for-one-family "WRAPPERS" wrappers anal output-stream)
    (build-mps-scan-for-one-family "ITERATORS" iterators anal output-stream)
    ))

(defun build-mps-skip-for-one-family (family-name kinds &optional (fout t))
  (format fout "#if defined(GC_SKIP_METHOD_~a)~%" family-name)
  (dolist (entry kinds)
    (let* ((classid (gcobject-subclass-key entry))
           (enum-name (class-enum-name classid)))
      (gclog "build-mps-scip-for-one-family -> inheritance classid[~a]  value[~a]~%" (ctype-name classid) value)
      (format fout "case ~a: {~%" enum-name)
      (format fout "    typedef ~A type_~A;~%" (ctype-name classid) enum-name)
      (format fout "    base = (char*)base + ALIGN(SIZEOF_WITH_HEADER(type_~A));~%" enum-name)
      (format fout "} break;~%")))
  (format fout "#endif // defined(GC_SKIP_METHOD_~a)~%" family-name)
  )






(defun build-mps-skip (anal &key (output-stream t))
  (with-accessors ((builtins organized-gcobjects-builtins)
                   (wrappers organized-gcobjects-wrappers)
                   (iterators organized-gcobjects-iterators))
      (analysis-organized-gcobjects anal)
    (build-mps-skip-for-one-family "BUILTINS" builtins output-stream)
    (build-mps-skip-for-one-family "WRAPPERS" wrappers output-stream)
    (build-mps-skip-for-one-family "ITERATORS" iterators output-stream)
    ))





(defun build-mps-finalize-for-one-family (family-name kinds &optional (fout t))
  (format fout "#if defined(GC_FINALIZE_METHOD_~a)~%" family-name)
  (dolist (entry kinds)
    (let* ((classid (gcobject-subclass-key entry))
           (enum-name (class-enum-name classid)))
      (gclog "build-mps-finalize-for-one-family -> inheritance classid[~a]  value[~a]~%" (ctype-name classid) value)
      (format fout "case ~a: {~%" enum-name)
      (format fout "    ~A* ~A = reinterpret_cast<~A*>(obj_ptr);~%" (ctype-name classid) +ptr-name+ (ctype-name classid))
      (format fout "    ~A->~~~A();~%" +ptr-name+ (ctype-name classid) )
      (format fout "} break;~%")))
  (format fout "#endif // defined(GC_FINALIZE_METHOD_~a)~%" family-name)
  )


(defun build-mps-finalize (anal &key (output-stream t))
  (with-accessors ((builtins organized-gcobjects-builtins)
                   (wrappers organized-gcobjects-wrappers)
                   (iterators organized-gcobjects-iterators))
      (analysis-organized-gcobjects anal)
    (build-mps-finalize-for-one-family "BUILTINS" builtins output-stream)
    (build-mps-finalize-for-one-family "WRAPPERS" wrappers output-stream)
    (build-mps-finalize-for-one-family "ITERATORS" iterators output-stream)
    ))




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





(defun generate-code-mps-functions (fout analysis all-housekeeping-classes) 
  (with-all-housekeeping-classes (all-housekeeping-classes)
    (let* ((inheritance (project-gcobjects (analysis-project analysis))))
      (format fout "#if defined(GC_ENUM)~%")
      (generate-kind-enum analysis :output-stream fout)
      (format fout "#endif // defined(GC_ENUM)~%")
      (format fout "#if defined(GC_KIND_SELECTORS)~%")
      (generate-gc-info analysis :output-stream fout)
      (format fout "#endif // defined(GC_KIND_SELECTORS)~%")
      (format fout "#if defined(GC_KIND_NAME_MAP)~%")
      (generate-kind-name-map analysis :output-stream fout)
      (format fout "#endif // defined(GC_KIND_NAME_MAP)~%")
      (build-mps-scan analysis :output-stream fout)
      (format fout "#if defined(GC_SKIP_METHOD)~%")
      (build-mps-skip analysis  :output-stream fout)
      (format fout "#endif // defined(GC_SKIP_METHOD)~%")
      (format fout "#if defined(GC_FINALIZE_METHOD)~%")
      (build-mps-finalize analysis  :output-stream fout)
      (format fout "#endif // defined(GC_FINALIZE_METHOD)~%")
      )))












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
           (break "Handle template-arg-kind: ~a" template-arg-kind)))
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
                        "std::__1::__compressed_pair<class std::__1::__tree_end_node<class std::__1::__tree_node_base<void *> *>,class std::__1::allocator<class std::__1::__tree_node<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::Instance_O> >, void *> >>"
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
                        "std::__1::__compressed_pair<unsigned long,class std::__1::__map_value_compare<class mem::smart_ptr<class core::Symbol_O>, union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::Instance_O> >, struct std::__1::less<class mem::smart_ptr<class core::Symbol_O> >, true>>"
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
                        "std::__1::__tree<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::Instance_O> >,class std::__1::__map_value_compare<class mem::smart_ptr<class core::Symbol_O>, union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::Instance_O> >, struct std::__1::less<class mem::smart_ptr<class core::Symbol_O> >, true>,class std::__1::allocator<union std::__1::__value_type<class mem::smart_ptr<class core::Symbol_O>, class mem::smart_ptr<class core::Instance_O> > >>"
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


(defmethod classify-ctype ((x cast:record-type))
  (let* ((decl (cast:get-decl x))
         (name (cast:get-name decl))
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
           ((string= name "pair")
            (make-stl-pair :key decl-key :arguments (classify-template-args decl)))
           ((string= name "list")
            (make-stl-list :key decl-key :arguments (classify-template-args decl)))
           ((string= name "vector")
            (make-stl-vector :key decl-key :arguments (classify-template-args decl)))
           ((string= name "set")
            (make-stl-set  :key decl-key :arguments (classify-template-args decl)))
           ((string= name "map")
            (make-stl-map :key decl-key :arguments (classify-template-args decl)))
           ((string= name "queue")
            (make-stl-queue :key decl-key :arguments (classify-template-args decl)))
           ((string= name "stack")
            (make-stl-stack  :key decl-key :arguments (classify-template-args decl)))
           ((string= name "multimap")
            (make-stl-multimap  :key decl-key :arguments (classify-template-args decl)))
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
           ((or
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
             (string= name "variate_generator"))
            (make-uninteresting-ctype :description decl-key))
           (t
            (warn "classify-ctype cast:record-type unhandled class-template-specialization-decl  key = ~a  name = ~a~%IGNORE-NAME ~a~%IGNORE-KEY ~a" decl-key name name decl-key)
            (make-unclassified-class-template-specialization-ctype :key decl-key
                                                      )))))
      (cast:cxxrecord-decl
       (if (gethash decl-key +record-ctype-ignores+)
           (make-uninteresting-ctype :description decl-key)
           (progn
             (warn "Add support for classify-ctype (x cast:record-type) or ... ~%RECORD-TYPE-IGNORE ~a" decl-key)
             (make-cxxrecord-ctype :key decl-key)
             )
           ))
      (otherwise
       (warn "Add support for classify-ctype ~a" x)
       (make-maybe-interesting-ctype :description decl-key)
       ))))

#||
  ((string= name "from_object") (make-from-object-ctype :key (record-key cxxrecord-decl)))
  ((string= name "LispObjectAllocatorFunctor") (make-lisp-object-allocator-functor-ctype :key (record-key cxxrecord-decl)))
  ((string= name "static_scope") (make-static-scope :key (record-key cxxrecord-decl)))

  ((and cxxrecord-decl
      (inherits-from-gcholder cxxrecord-decl))
      (assert (eql (cast:get-num-args decl) 1) nil
      "~a requires 1 arg only but was given ~a args"
      name
      (cast-get-num-args decl))
      (make-gcholder :name (intern (string-upcase (format nil "GCHOLDER_~a" name)))
      :arguments (classify-template-args decl)))
  ((and cxxrecord-decl
      (inherits-from-rooted-gcholder cxxrecord-decl))
      (assert (eql (cast:get-num-args decl) 1) nil
      "~a requires 1 arg only but was given ~a args"
      name
      (cast-get-num-args decl))
      (make-rooted-gcholder :name (intern (string-upcase (format nil "ROOTED_GCHOLDER_~a" name)))
      :arguments (classify-template-args decl)))
  ||#


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
  (warn "Add support for classify-ctype to recognize ~a~%" x)
  (make-unclassified-ctype :description (format nil "~a" x)))


(defun canonical-type (x)
  (let* ((canonical-qual-type (cast:get-canonical-type-internal x))
         (canonical-type (cast:get-type-ptr-or-null canonical-qual-type))
         )
    canonical-type))


(defun annotated-class-name (class-node)
  (format nil "~a" (clang-ast:get-qualified-name-as-string class-node)))



(defun check-instance-var-result (instance-vars)
  (let ((v (gethash "core::Ratio_O" instance-vars)))
    (print v)))

(defun gc-builder-c-version (&key test)
  (let* ((*testing* test)
         (json-database-pathname "app:Contents;Resources;buildDatabases;brcl_compile_commands.json" )
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







(defparameter *class-gcobject-derived-matcher*
  '(:record-decl
    (:is-definition)
    (:is-same-or-derived-from
     (:record-decl
      (:matches-name "GCObject")))))


(defparameter *gcobject-method-matcher*
  (compile-matcher
   '(:record-decl
     (:for-each ;; -descendant
      (:destructor-decl
;;       (:unless (:is-implicit))
;;       (:isOverride)  ; (:unless (:is-implicit))
       (:bind :method (:method-decl)))))))


(defparameter *instance-var-matcher*
  (compile-matcher
   '(:record-decl
     (:is-definition)
     (:for-each ;; -descendant
      (:field-decl
       (:bind :field (:field-decl)))))))







(defun setup-finder-gc-info (mtool)
  "Setup the TOOL (multitool) to look for instance variables that we want to garbage collect
and the inheritance hierarchy that the garbage collector will need"
  (symbol-macrolet ((class-results (project-gcobjects (multitool-results mtool))))
    (flet ((%%class-gcobject-derived-matcher-callback ()
             "This function can only be called as a ASTMatcher callback"
             (let* ((class-node (mtag-node :whole))
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
                                           (base-name (cast:get-qualified-name-as-string base))
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
                                            (vbase-name (cast:get-qualified-name-as-string vbase))
                                            )
                                       (gclog "   VBase name: ~a~%" vbase-name)
                                       (push vbase-name temp-vbases)))
                                   (clang-ast:vbases-iterator (mtag-node :whole)))
                                  temp-vbases
                                  ))
                        (gcobject-subclass (make-gcobject-subclass :key class-key
                                                                   :name class-name
                                                                   :bases bases
                                                                   :vbases vbases)))
                   ;; (format t "Adding class-results for ~a~%" class-name) ;
                   (sub-match-run
                    *instance-var-matcher*
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
                                           (let ((*debug-info* (make-debug-info :name (cast:get-name whole-node)
                                                                                :location (mtag-loc-start :field))))
                                             (classify-ctype (canonical-type type)))))
                             (instance-var (make-instance-variable
                                            :location (mtag-loc-start :field)
                                            :field-name (cast:get-name whole-node)
                                            :ctype classified)))
                        (pushnew instance-var (gcobject-subclass-instance-variables gcobject-subclass)
                                 :key #'instance-variable-field-name
                                 :test #'(lambda (x y) (string= x y) ))
                        )))
                   (sub-match-run
                    *gcobject-method-matcher*
                    class-node
                    (lambda ()
                      (let* ((method-node (mtag-node :method))
                             (method-name (mtag-name :method)))
                        (when (not (cast:is-implicit method-node))
                          (setf (gcobject-subclass-has-destructor gcobject-subclass) t))
                        )))
                   (setf (gethash class-key class-results) gcobject-subclass)
                   )))))
      ;; Initialize the class search
      (multitool-add-matcher mtool
                             :matcher (compile-matcher `(:bind :whole ,*class-gcobject-derived-matcher*))
                             :initializer (lambda () (setf class-results (make-hash-table :test #'equal))) ; initializer
                             :callback (make-instance 'code-match-callback :code (function %%class-gcobject-derived-matcher-callback))
                             ))))



;; ----------------------------------------------------------------------
;;
;; Search for house-keeping classes that contain smart-pointers and
;; set up garbage collector root-scanning functions for them








(defparameter *class-housekeeping-matcher*
  '(:bind :whole
    (:record-decl
     #+housekeeping-namespace(:has-ancestor
      (:namespace-decl
       (:any-of
        (:has-name "translate")
        (:has-name "gctools") (:has-name "mem") (:has-name "core") (:has-name "cl")
        (:has-name "cluser") (:has-name "ext") (:has-name "kw") (:has-name "mpi")
        (:has-name "gray") (:has-name "llvmo") (:has-name "cffi") (:has-name "clbind")
        (:has-name "reg") (:has-name "asttooling") (:has-name "ql") (:has-name "comp")
        (:has-name "sockets") (:has-name "serveEvent") (:has-name "clangSys"))
       (:bind :ns (:namespace-decl))
       )
      )
     (:is-definition)
     (:unless
         (:is-same-or-derived-from
          (:record-decl
           (:any-of
            (:has-name "GCObject")
            (:has-name "GCIgnoreClass")
            (:has-name "VariadicMethoid")
;;            (:has-name "StackBoundClass")
;;            (:has-name "GC_Automated")
;;            (:has-name "GC_Manual")
;;            (:has-name "smart_ptr")
;;            (:has-name "weak_smart_ptr")
;;            (:has-name "VariadicFunctoid")
;;            (:has-name "UnorderedSet")
;;            (:has-name "Vector0")
;;            (:has-name "WeakMultiStringMap")
;;            (:has-name "WeakMap")
;;            (:has-name "SymbolMap")
;;            (:has-name "guid_impl")
;;            (:has-name "WeakSymbolMultiMap")
;;            (:has-name "StringMap")
;;            (:has-name "IndexedSymbolMap")
;;            (:has-name "OrderedSet")
;;            (:has-name "LexicalGo")
;;            (:has-name "DynamicGo")
;;            (:has-name "ReturnFrom")
            )))))))

(defparameter *base-class-sub-matcher*
  (compile-matcher '(:record-decl
                     (:is-derived-from
                      (:record-decl
                       (:bind :base-name (:record-decl))
                       (:has-ancestor
                        (:namespace-decl
                         (:bind :base-ns (:namespace-decl))))
                       )))))
(or *base-class-sub-matcher*
    (error "Problem encountered compiling *base-class-sub-matcher*"))


(defparameter *field-sub-matcher*
  (compile-matcher '(:record-decl
                     (:for-each ;; -descendant
                      (:field-decl
                       (:bind :field (:field-decl))
                       (:has-type
                        (:record-decl
                         (:bind :type (:record-decl))
                         (:unless
                             (:any-of
                              (:has-name "basic_string")
                              (:has-name "string"))))))))))
(or *field-sub-matcher*
    (error "Problem encountered compiling *field-sub-matcher*"))


(defparameter *method-sub-matcher*
  (compile-matcher '(:record-decl
                     (:for-each ;; -descendant
                      (:method-decl
                       (:bind :method (:method-decl))
                       (:any-of
                        (:has-name "on_heap_scanGCRoots")
                        (:has-name "on_stack_scanGCRoots")))))))


(defun setup-housekeeping-class-search (mtool)
  (symbol-macrolet ((results (project-housekeeping-classes (multitool-results mtool))))
    (flet ((%%housekeeping-class-callback ()
             (block matcher
               (gclog "MATCH: ------------------~%")
               (gclog "    Start:~%~a~%" (mtag-loc-start :whole))
               (gclog "    Name: ~a~%" (mtag-name :whole))
               (let* ((class-node (mtag-node :whole))
;;                      (namespace (mtag-name :ns))
                      (cname (mtag-name :whole))
                      bases fields
                      has-on-stack-scan-gc-roots
                      has-on-heap-scan-gc-roots)
                 (multiple-value-bind (record-key template-specializer)
                     (record-key class-node)
                   ;;
                   ;; If we have seen this class then return
                   ;;
                   (when (gethash record-key results)
                     (gclog "+++++++++++   This class has already been matched~%")
                     (return-from matcher nil))
                   ;;
                   ;; Run a matcher to find the base classes and their namespaces
                   ;;
                   (sub-match-run *base-class-sub-matcher*
                                  (mtag-node :whole)
                                  (lambda ()
                                    (let ((base-node (mtag-node :base-name))
                                          (base-ns (mtag-name :base-ns))
                                          (base-name (mtag-name :base-name)))
                                      (gclog "    ++ Base: ~a::~a~%" base-ns base-name)
                                      (push (cast:get-qualified-name-as-string base-node) ;; (make-cname :namespace base-ns :name base-name)
                                            bases))))
                   ;;
                   ;; Run a matcher to find the GC-scannable fields of this class
                   ;;
                   (sub-match-run *field-sub-matcher*
                                  (mtag-node :whole)
                                  (lambda ()
                                    (let* ((field-node (mtag-node :field))
                                           (qualty (cast:get-type field-node))
                                           (type (cast:get-type-ptr-or-null qualty)))
                                      (gclog "      >> Field: ~30a~%" (mtag-source :field))
                                      (handler-case
                                          (push (make-instance-variable
                                                 :location (mtag-loc-start :field)
                                                 :field-name (mtag-name :field)
                                                 :ctype (let ((*debug-info* (make-debug-info :name (mtag-name :field)
                                                                                             :location (mtag-loc-start :field))))
                                                          (classify-ctype (canonical-type type))))
                                                fields)
                                        (unsupported-type (err)
                                          (error "Add support for classifying type: ~a (type-of type): ~a  source: ~a"
                                                 type (type-of type) (mtag-source :field))))
                                      )))
                   ;;
                   ;; Run a matcher to find the scanGCRoot functions
                   ;;
                   (sub-match-run *method-sub-matcher*
                                  (mtag-node :whole)
                                  (lambda ()
                                    (let* ((method-node (mtag-node :method))
                                           (method-name (mtag-name :method)))
                                      (gclog "      >> Method: ~30a~%" (mtag-source :method))
                                      (cond
                                        ((string= method-name "on_heap_scanGCRoots")
                                         (setq has-on-heap-scan-gc-roots t))
                                        ((string= method-name "on_stack_scanGCRoots")
                                         (setq has-on-stack-scan-gc-roots t))
                                        (t (error "I matched a method I shouldn't have - only on_stack_scanGCRoots or on_heap_scanGCRoots should have been matched, but got: ~a" method-name)))
                                      )))
                   (setf (gethash record-key results)
                         (make-housekeeping-class :key record-key
                                                  :template-specializer template-specializer
                                                  :location (mtag-loc-start :whole)
                                                  :has-on-stack-scan-gc-roots has-on-stack-scan-gc-roots
                                                  :has-on-heap-scan-gc-roots has-on-heap-scan-gc-roots
                                                  :bases bases
                                                  :fields fields))
                   )))))
      (multitool-add-matcher mtool
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
  (symbol-macrolet ((all-classes (project-housekeeping-classes (analysis-project analysis))))
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
  (symbol-macrolet ((all-classes (project-housekeeping-classes (analysis-project analysis))))
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
  (symbol-macrolet ((all-classes (project-housekeeping-classes (analysis-project analysis))))
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




(defun root-fixer-for-class (stream aclass analysis fix allocation-type)
  "Construct code to root this class depending on its allocation-type"
  (symbol-macrolet ((all-classes (analysis-housekeeping-classes analysis)))
    (with-all-housekeeping-classes (all-classes)
      (flet ((%%needs-rooting-p (one-type)
               "Return true if the given ONE-TYPE needs to be rooted"
               (if (eq allocation-type :heap)
                   (contains-smart-pointers-p one-type analysis)
                   (on-stack-has-smart-pointers-on-heap-p one-type analysis))))
        (let ((full-name (housekeeping-class-key aclass))
              (template-specializer (housekeeping-class-template-specializer aclass))
              (bases (housekeeping-class-bases aclass))
              (fields (housekeeping-class-fields aclass))
              final-fresh-line
              )
          (format stream "GC_RESULT ~a ~a::~ascanGCRoots(GC_SCAN_ARGS_PROTOTYPE) {"
                  (if template-specializer "template<>" "")
                  full-name
                  (if (eq allocation-type :stack) "on_stack_" "on_heap_"))
          (fresh-line stream)
          (format stream "    // Location: ~a~%" (housekeeping-class-location aclass))
          (dolist (base bases)
            (let ((base-class (gethash base all-classes)))
              (if (null base-class)
                  (warn "root-fixer-for-class could not find base ~a" base)
                  (when (%%needs-rooting-p base-class)
                    (fresh-line stream)
                    (format stream "    this->~a::scanGCRoots(GC_SCAN_ARGS_PASS);" base) ;
                    (setq final-fresh-line t)))))
          (dolist (field fields)
            (let ((field-ctype (instance-variable-ctype field)))
              (when (%%needs-rooting-p field-ctype)
                (fresh-line stream)
                (code-for-instance-var stream "this" (instance-variable-field-name field) (instance-variable-ctype field))
                (setq final-fresh-line t))))
          ;;      (format stream "/* FF = ~a */" final-fresh-line )
          (when final-fresh-line (fresh-line))
          (format stream "};~%~%")
          )))))



(defun generate-code-for-housekeeping-classes (stream analysis)
  (format stream "#if defined(ON_HEAP_HOUSEKEEPING_SCANNERS)~%")
  (symbol-macrolet ((classes (project-housekeeping-classes (analysis-project analysis))))
    (maphash (lambda (key class)
               (let ((fix (contains-smart-pointers-p class analysis)))
                 (when (and fix (housekeeping-class-has-on-heap-scan-gc-roots class))
                   (root-fixer-for-class stream class analysis fix :heap))))
             classes)
    (format stream "#endif // ON_HEAP_HOUSEKEEPING_SCANNERS~%")
    (format stream "#if defined(ON_STACK_HOUSEKEEPING_SCANNERS)~%")
    (maphash (lambda (key class)
               (let ((fix (on-stack-has-smart-pointers-on-heap-p class analysis)))
                 (when (and fix (housekeeping-class-has-on-stack-scan-gc-roots class))
                   (root-fixer-for-class stream class analysis fix :stack))))
             classes))
  (format stream "#endif // ON_STACK_HOUSEKEEPING_SCANNERS~%"))




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



(defparameter *variable-matcher*
  '(:bind :whole
    (:var-decl
     (:is-definition)
     (:unless (:parm-var-decl))
     (:unless
         (:any-of
          (:matches-name "_sym_.*")
          (:has-name "___staticClass")
          (:has-name "___staticClassSymbol")
          (:has-name "___static_newNil_callback")
          (:matches-name ".*_gc_safe")))
     )))


(compile-matcher *variable-matcher*)




(defun setup-variable-search (mtool)
  (symbol-macrolet ((global-variables (project-global-variables (multitool-results mtool)))
                    (static-local-variables (project-static-local-variables (multitool-results mtool)))
                    (local-variables (project-local-variables (multitool-results mtool))))
    (flet ((%%variable-callback ()
             (block matcher
               (gclog "VARIABLE MATCH: ------------------~%")
               (gclog "    Start:~%~a~%" (mtag-loc-start :whole))
               (gclog "    Name: ~a~%" (mtag-name :whole))
               (gclog "    namespace: ~a~%" (mtag-name :ns))
               (let* ((var-node (mtag-node :whole))
                      (varname (cast:get-qualified-name-as-string var-node))
                      (location (mtag-loc-start :whole))
                      (var-kind (cond
                                  ((and (cast:has-global-storage var-node) (not (cast:is-static-local var-node))) :global)
                                  ((and (cast:has-global-storage var-node) (cast:is-static-local var-node)) :static-local)
                                  (t :local)))
                      (hash-table (ecase var-kind
                                    (:global global-variables)
                                    (:static-local static-local-variables)
                                    (:local local-variables)))
                      (key (format nil "~a@~a" varname location)))
                 (unless (gethash key hash-table)
                   (let* ((qtype (cast:get-type var-node))
                          (type (cast:get-type-ptr-or-null qtype))
                          (classified-type (let ((*debug-info* (make-debug-info :name varname :location location)))
                                             (classify-ctype (canonical-type type))))
                          )
                     (ecase var-kind
                       (:global
                        (setf (gethash key hash-table)
                              (make-global-variable :location location
                                                    :name varname
                                                    :ctype classified-type)))
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
                             :matcher (compile-matcher *variable-matcher*)
                             :initializer (lambda ()
                                            (setf global-variables (make-hash-table :test #'equal))
                                            (setf static-local-variables (make-hash-table :test #'equal))
                                            (setf local-variables (make-hash-table :test #'equal)))
                             :callback (make-instance 'code-match-callback :code (function %%variable-callback)))
      )))



(defun problematic-local-variable-ctype-p (vt analysis)
  (on-stack-has-smart-pointers-on-heap-p vt analysis))



(defun generate-code-for-variables (stream analysis)
  (symbol-macrolet ((project (analysis-project analysis)))
    (with-all-housekeeping-classes ((project-housekeeping-classes project))
      (format stream "#if defined(GC_GLOBAL_VARIABLES)~%")
      (maphash (lambda (k v)
                 (unless (or
                          (builtin-ctype-p (variable-ctype v))
                          (uninteresting-ctype-p (variable-ctype v)))
                   (code-for-global-var stream (variable-ctype v)
                                        (variable-name v)
                                        (variable-location v)
                                        (on-stack-has-smart-pointers-on-heap-p (variable-ctype v) analysis))))
               (project-global-variables project))
      (format stream "#endif // ifdef GC_GLOBAL_VARIABLES~%")
      (format stream "#if defined(GC_STATIC_LOCAL_VARIABLES)~%")
      (maphash (lambda (k v)
                 (unless (builtin-ctype-p (variable-ctype v))
                   (format stream "STATIC_LOCAL_VARIABLE(~a); // ~a ~a~%"
                           (variable-name v)
                           (variable-location v)
                           (variable-ctype v))))
               (project-static-local-variables project))
      (format stream "#endif // ifdef GC_STATIC_LOCAL_VARIABLES~%")
      (format stream "#if defined(GC_LOCAL_VARIABLES_DANGEROUS)~%")
      (maphash (lambda (k v &aux (var (variable-ctype v)))
                 (let ((problem (problematic-local-variable-ctype-p var analysis)))
                   (when problem
                     (format stream "GC_PROBLEMATIC_LOCAL_VARIABLE(~a); // ~a ~a ~a~%"
                             (variable-name v)
                             problem
                             var
                             (variable-location v)
                             ))))
               (project-local-variables project))
      (format stream "#endif // ifdef GC_LOCAL_VARIABLES_IMPORTANT~%")
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
      )))





;; ----------------------------------------------------------------------
;;
;; Setup the *tools* that will run over all of the source code and run
;; several matchers that scrape the C++ AST for info requires to build
;; garbage collection scanners.


(load-compilation-database "app:Contents;Resources;buildDatabases;brcl_compile_commands.json")

(defparameter *tools* (make-multitool))

(setup-finder-gc-info *tools*)
(setup-housekeeping-class-search *tools*)
(setup-variable-search *tools*)


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


(lnew $test-search)
(setq $test-search (append
                    (lsel $* ".*/serialize\.cc")
                           ))



#+ignore(progn
          (defparameter *l* (split-list $* 5))
          (setq $test-search (loop
                                :for v :in (first *l*)
                                :for w :in (second *l*)
                                :for x :in (third *l*)
                                :for y :in (fourth *l*)
                                :for z :in (fifth *l*)
                                :collect v
                                :collect w
                                :collect x
                                :collect y
                                :collect z)))

(defparameter *project* nil)
(defun search-all (&key test)
  (setf (multitool-results *tools*) (make-project))
  (batch-run-multitool *tools* :filenames (if test
                                              $test-search
                                              (reverse $*))
                       :arguments-adjuster-code (lambda (args) (concatenate 'vector #-quiet args #+quiet(remove "-v" args) #("-DUSE_MPS"))))
  ;; Extract the results for easy access
  (setq *project* (multitool-results *tools*))
  )



(defparameter *max-parallel-searches* 5)


(defun *parallel-search-pids* nil)
(defun parallel-search-all (&key test)
  (setq *parallel-search-pids* nil)
  (let ((all-jobs (if test
                      (subseq $* 0 test)
                      (reverse $*))))
    (let ((split-jobs (split-list all-jobs *max-parallel-searches*)))
      (dotimes (proc *max-parallel-searches*)
        (core:system "sleep 1")
        (let* ((job-list (elt split-jobs proc))
               (pid (core:fork)))
          (if (eql 0 pid)
              (with-open-file (*standard-output* (format nil "sys:project~a.log" proc)
                                                 :direction :output :if-exists :supersede)
                (format t "Running search on: ~a~%" 
                        (setf (multitool-results *tools*) (make-project))
                        (batch-run-multitool *tools* :filenames job-list
                                             :arguments-adjuster-code (lambda (args)
                                                                        (concatenate 'vector #-quiet args #+quiet(remove "-v" args) #("-DUSE_MPS"))))
                        (serialize:save-archive (multitool-results *tools*) (format nil "sys:project~a.dat" proc))
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
      (let ((one (serialize:load-archive (format nil "sys:project~a.dat" proc))))
        (format t "     merging...~%")
        (merge-projects merged one)))
    (setq *project* merged)
    (save-project)
    merged))

(defun save-project ()
  (serialize:save-archive *project* "sys:project.dat"))

(defun load-project ()
  (setq *project* (serialize:load-archive "sys:project.dat"))
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
  (symbol-macrolet ((table (analysis-global-variables-with-smart-pointers analysis)))
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
  (symbol-macrolet ((table (analysis-static-local-variables-with-smart-pointers analysis)))
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



#|
;; What do I do here?
(defun describe-problematic-local-variables (&optional (stream t) (analysis *analysis*))
  (maphash (lambda (key var)
             (when (on-stack-has-smart-pointers-on-heap (variable-ctype var))
           (project-housekeeping-classes (analysis-project analysis)))))
|#

(defparameter *analysis* nil)
(defun analyze-project (&optional (project *project*))
  (setq *analysis* (make-analysis :project project))
  (organize-gcobjects *analysis*)
;;  (identify-smart-pointer-info *analysis*)
;;  (classify-housekeeping-class-inheritance *analysis*)
  (progn
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
  (let ((housekeeping-classes (project-housekeeping-classes (analysis-project analysis))))
;;    (identify-smart-pointer-info analysis)
    (with-open-file (fout (make-pathname :name "clasp_gc" :type "cc" :defaults (main-directory-pathname)) :direction :output :if-exists :supersede)
      (generate-code-mps-functions fout analysis housekeeping-classes)
      (generate-code-for-housekeeping-classes fout analysis)
      (generate-code-for-global-variables fout analysis)
      
;;      (generate-code-for-local-variables fout analysis)
      ))
  )

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
  (symbol-macrolet ((project (analysis-project analysis)))
    (with-all-housekeeping-classes ((project-housekeeping-classes project))
      (maphash (lambda (k v)
                 (when (contains-smart-pointers-p (global-variable-ctype v) analysis)
                   (format t "~10a ~a~%" (contains-smart-pointers-p (global-variable-ctype v) analysis) k)
                   )
                 )
               (project-global-variables project)))))
  

(defun describe-static-local-variables (&optional (name "") (analysis *analysis*))
  (symbol-macrolet ((project (analysis-project analysis)))
    (with-all-housekeeping-classes ((project-housekeeping-classes project))
      (maphash (lambda (k v)
                 (when (search name (format nil "~a" v))
                   (format t "~10a ~a~%" (contains-smart-pointers-p (static-local-variable-ctype v) analysis) k)
                   )
                 )
               (project-static-local-variables project)))))


(defun describe-local-variables (&optional (name "") (analysis *analysis*) details)
  (symbol-macrolet ((project (analysis-project analysis)))
    (format t "Project local-variables: ~a~%" (project-local-variables project))
    (format t "~10a ~10a ~a~%" "csp" "oshspoh" "Name")
    (with-all-housekeeping-classes ((project-housekeeping-classes project))
      (maphash (lambda (k v)
                 (when (search name (format nil "~a" v))
                   (format t "~10a ~10a ~a~%" (contains-smart-pointers-p (local-variable-ctype v) analysis) (on-stack-has-smart-pointers-on-heap-p (local-variable-ctype v) analysis) k)
                   (when details
                     (format t "~a~%" v))
                   )
                 )
               (project-local-variables project)))))
;;
;; Test matcher for template classes
;;



#+testing 
(progn
  (lnew $test-search)
  (setq $test-search (lsel $* ".*/testAST\.cc"))
  (load-asts $test-search))


#+testing
(progn
  (defparameter *test-matcher*
    '(:record-decl
      (:is-definition)
      (:for-each
       (:field-decl
        (:has-type
         (:decl
          (:bind :decl (:decl))))))))
  (match-run *test-matcher*
             ;;             :limit 10
             ;;              :tag :point
             :match-comments '( ".*mytest.*" )
             :code #'(lambda ()
                       (format t "MATCH: ------------------~%")
                       (format t "        Start: ~a~%" (mtag-loc-start :whole))
                       (format t "         Node: ~a~%" (mtag-node :whole))
                       (format t "         Name: ~a~%" (mtag-name :whole))
                       (format t "         Decl: ~a~%" (mtag-node :decl))
                       (format t "   record-key: ~a~%" (record-key (mtag-node :decl)))
                       #|                       (format t "         diagnostic name: ~a~%"
                       (cast:get-name (cast:get-specialized-template (mtag-node :whole))))
     (when (typep (mtag-node :whole) 'cast:class-template-specialization-decl)
                       (format t "   +++++ First instantiation location  ~a    ~%"
                       (source-loc-as-string (cast:get-point-of-instantiation (mtag-node :whole)))))
     |#
                       )
             )
  )




#+testing
(progn
  (defparameter *test-matcher*
    '(:var-decl
;;      (:has-name "_lisp")
      (:is-definition)
      (:unless
          (:has-ancestor
           (:namespace-decl)))
      #|(:has-ancestor
        (:namespace-decl
         (:any-of
          (:has-name "::") (:has-name "translate")
          (:has-name "gctools") (:has-name "mem") (:has-name "core") (:has-name "cl")
          (:has-name "cluser") (:has-name "ext") (:has-name "kw") (:has-name "mpi")
          (:has-name "gray") (:has-name "llvmo") (:has-name "cffi") (:has-name "clbind")
          (:has-name "reg") (:has-name "asttooling") (:has-name "ql") (:has-name "comp")
          (:has-name "sockets") (:has-name "serveEvent") (:has-name "clangSys"))
         (:bind :ns (:namespace-decl))))|#
      (:unless (:parm-var-decl)) ;
      (:unless
          (:any-of
           (:matches-name "_sym_.*")
           (:has-name "___staticClass")
           (:has-name "___staticClassSymbol")
           (:has-name "___static_newNil_callback")
           (:matches-name ".*_gc_safe")))
      ))
  (match-count *test-matcher*
               ;;:limit 100
             ;;              :tag :point
             :match-comments '( ".*mytest.*" )
             :code #'(lambda ()
                       (format t "MATCH: ------------------~%")
                       (format t "        Start: ~a~%" (mtag-loc-start :whole))
                       (format t "         Node: ~a~%" (mtag-node :whole))
                       (format t "         Name: ~a~%" (mtag-name :whole))
;;                       (format t "         Decl: ~a~%" (mtag-node :decl))
;;                       (format t "   record-key: ~a~%" (record-key (mtag-node :decl)))
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
