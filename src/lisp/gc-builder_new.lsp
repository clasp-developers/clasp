(progn
  (provide 'gc-builder)
  (load "sys:clang-tool.lsp") ; (require 'clang-tool)
  (use-package :ast-tooling)
  )

;; (defmacro gclog (fmt &rest args) `(format t ,fmt ,@args))
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
       "PACK")
      (ast-tooling:template
       "TEMPLATE")
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
     (break "Check the decl-node"))))


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

(defstruct (smart-ptr-ctype (:include ctype))
  specializer
  )


(defstruct (weak-smart-ptr-ctype (:include smart-ptr-ctype))
  )

(defstruct (rest-argument (:include simple-ctype)))


(defstruct (container (:include ctype))
  arguments
  )

(defmethod container-argument ((x container) idx)
  (dolist (arg (container-arguments x))
    (when (eql (gc-template-argument-index arg) idx) (return arg))))


(defstruct (record-ctype (:include ctype))
  key)

(defstruct (injected-class-name-ctype (:include record-ctype)))

(defstruct (from-object (:include record-ctype)))



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
    (break "contains-smart-pointers-p on nil"))
  (when (eq x t)
    (break "contains-smart-pointers-p on t"))
  (when (eq x :maybe)
    (break "contains-smart-pointers-p on :maybe"))
  (warn "Add support for contains-smart-pointers-p for ~a" x)
  t)


(defmethod contains-smart-pointers-p ((x global-variable) analysis)
  (contains-smart-pointers-p (global-variable-ctype x) analysis))




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
    (analysis-housekeeping-class-contains-smart-pointers-p x anal)
    (setf (gethash x (analysis-housekeeping-class-stored-on-heap anal)) t)
    )


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
      ((string= name "from_object") (make-from-object :key (record-key cxxrecord-decl)))
      ((and cxxrecord-decl
            (inherits-from-gcholder cxxrecord-decl))
       (assert (eql (cast:get-num-args tsty) 1) nil
               "~a requires 1 arg only but was given ~a args"
               name
               (cast-get-num-args tsty))
       (make-gcholder :name (intern (string-upcase (format nil "GCHOLDER_~a" name)))
                      :arguments (classify-template-args tsty)))
      (t
       ;;       (warn "Add support for classify-template-specialization-type to recognize ~a  ~a~%" name (cast:get-as-string (cast:desugar tsty)))
       (make-unclassified-template-specialization-ctype
        :description (record-key cxxrecord-decl) ;; (cast:get-as-string (cast::desugar tsty))
        :template-name name)))))







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

(defmethod classify-ctype ((x cast:record-type))
  (make-record-ctype :key (record-key (cast:get-decl x))))

(defmethod classify-ctype ((x cast:injected-class-name-type))
  (make-injected-class-name-ctype :key (record-key (cast:get-decl x))))


(defmethod classify-ctype ((x cast:template-specialization-type))
  (classify-template-specialization-type x))


(defmethod classify-ctype ((x cast:typedef-type))
  (make-typedef-ctype :desugared (classify-ctype (cast:get-type-ptr-or-null (cast:desugar x)))))


(defmethod classify-ctype ((x cast:elaborated-type))
  (let* ((named-qual-type (cast:get-named-type x))
         (real-type (cast:get-type-ptr-or-null named-qual-type)))
    (make-elaborated-ctype :named-type (classify-ctype real-type))))


(defmethod classify-ctype ((x cast:pointer-type))
  (make-pointer-ctype :pointee (classify-ctype (cast:get-type-ptr-or-null (cast:get-pointee-type x)))))


(defmethod classify-ctype ((x cast:paren-type))
  (make-paren-ctype :inner (classify-ctype (cast:get-type-ptr-or-null (cast:get-inner-type x)))))


(defmethod classify-ctype ((x t))
  (warn "Add support for classify-ctype to recognize ~a~%" x)
  (make-unclassified-ctype :description (format nil "~a" x)))


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
                                             (classify-ctype type))))
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
     (:has-ancestor
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
               (gclog "    namespace: ~a~%" (mtag-name :ns))
               (let* ((class-node (mtag-node :whole))
                      (namespace (mtag-name :ns))
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
                                                          (classify-ctype type)))
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
                                             (classify-ctype type)))
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
                          (break "classified-type is nil"))
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


(lnew $test-search)
(setq $test-search (lsel $* ".*/testAST\.cc"))


(defparameter *project* nil)
(defun search-all (&key test)
  (setf (multitool-results *tools*) (make-project))
  (batch-run-multitool *tools* :filenames (if test
                                              $test-search
                                              $*)
                       :arguments-adjuster-code (lambda (args) (concatenate 'vector (remove "-v" args) #("-DUSE_MPS"))))
  ;; Extract the results for easy access
  (setq *project* (multitool-results *tools*))
  )



(defparameter *max-parallel-searches* 5)

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


(defun *parallel-search-pids* nil)
(defun parallel-search-all (&key test)
  (setq *parallel-search-pids* nil)
  (let ((all-jobs (if test
                      (subseq $* 0 test)
                      $*)))
    (let ((split-jobs (split-list all-jobs *max-parallel-searches*)))
      (dotimes (proc *max-parallel-searches*)
        (let* ((job-list (elt split-jobs proc))
               (pid (core:fork)))
          (if (eql 0 pid)
              (with-open-file (*standard-output* (format nil "sys:project~a.log" proc)
                                                 :direction :output :if-exists :supersede)
                (format t "Running search on: ~a~%" 
                        (setf (multitool-results *tools*) (make-project))
                        (batch-run-multitool *tools* :filenames job-list
                                             :arguments-adjuster-code (lambda (args)
                                                                        (concatenate 'vector (remove "-v" args) #("-DUSE_MPS"))))
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
             (project-local-variables (analysis-project analysis)))))

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
             (project-local-variables (analysis-project analysis)))))

  
(defun classify-locals (&optional (analysis *analysis*))
  (symbol-macrolet ((table (analysis-local-variables-with-smart-pointers-on-heap analysis)))
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
  (let ((all-classes (project-housekeeping-classes (analysis-project analysis))))
    (maphash (lambda (key class)
               (when (analysis-housekeeping-class-contains-smart-pointers-p class analysis)
                 (unless (is-or-inherits-from-heap-root class all-classes)
                   (format stream "ERROR: Housekeeping class contains smart pointers but does not inherit from HeapRoot - This must be changed!~%    ~a ~a~%" key  (housekeeping-class-location class))))
               (when (analysis-housekeeping-class-on-stack-has-smart-pointers-on-heap-p class analysis)
                 (unless (is-or-inherits-from-stack-root class all-classes)
                   (format stream "ERROR: Housekeeping class eon stack has smart pointers on heap but does not inherit from StackRoot - This must be changed!~%    ~a ~a~%" key (housekeeping-class-location class))))
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
  (identify-smart-pointer-info *analysis*)
;;  (classify-housekeeping-class-inheritance *analysis*)
  (progn
    (clrhash (analysis-housekeeping-class-stored-on-heap *analysis*))
    (clrhash (analysis-housekeeping-class-stored-on-stack *analysis*))
    (classify-globals *analysis*)
    (classify-static-locals *analysis*)
    (classify-locals *analysis*)
    )
  (describe-problematic-housekeeping-classes)
;;  (describe-problematic-local-variables)
)


(defun generate-code (&optional (analysis *analysis*))
  (organize-gcobjects analysis)
  (let ((housekeeping-classes (project-housekeeping-classes (analysis-project analysis))))
    (identify-smart-pointer-info analysis)
    (with-open-file (fout (make-pathname :name "clasp_gc" :type "cc" :defaults (main-directory-pathname)) :direction :output :if-exists :supersede)
      (generate-mps-functions fout analysis housekeeping-classes)
      (generate-code-for-housekeeping-classes fout analysis)
      (generate-code-for-global-variables fout analysis)
      (generate-code-for-local-variables fout analysis)
      ))
  )

(defun describe-all-classes (name &key (analysis *analysis*) detail)
  (format t "There housekeeping classes: ~a~%" (project-housekeeping-classes (analysis-project analysis)))
  (format t "~8a ~8a ~8a ~8a ~a~%" "onstack" "onheap" "oshspoh" "csp" "Name")
  (maphash (lambda (k v)
             (when (search name k)
               (format t "~8a ~8a ~8a ~8a ~30a         ~a~%"
                       (gethash k (analysis-housekeeping-class-stored-on-stack analysis))
                       (gethash k (analysis-housekeeping-class-stored-on-heap analysis))
                       (on-stack-has-smart-pointers-on-heap-p v analysis)
                       (contains-smart-pointers-p v analysis)
                       k
                       (housekeeping-class-location v))
               (when detail
                 (format t "~a~%" v)
                 (format t "     contains-smart-pointers-p -> ~a~%" (contains-smart-pointers-p v analysis))
                 (format t "     on-stack-has-smart-pointers-on-heap-p -> ~a~%" (on-stack-has-smart-pointers-on-heap-p v analysis))
                 )
               ))
           (project-housekeeping-classes (analysis-project analysis))))


(defun reset-class-smart-pointer-info (&optional (analysis *analysis*))
  (maphash (lambda (k v)
             (setf (gethash k (analysis-housekeeping-class-on-stack-has-smart-pointers-on-heap analysis)) nil)
             (setf (gethash k (analysis-housekeeping-class-contains-smart-pointers analysis)) nil)
             )
           (project-housekeeping-classes (analysis-project analysis))))


(defun describe-global-variables (&optional (name "") (analysis *analysis*))
  (with-all-housekeeping-classes ((project-housekeeping-classes (analysis-project analysis)))
    (maphash (lambda (k v)
               (when (search name k)
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
                 (when (search name k)
                   (format t "~10a ~a~%" (contains-smart-pointers-p (static-local-variable-ctype v) analysis) k)
                   )
                 )
               (project-static-local-variables project)))))


(defun describe-local-variables (&optional (name "") (analysis *analysis*) details)
  (symbol-macrolet ((project (analysis-project analysis)))
    (format t "Project local-variables: ~a~%" (project-local-variables project))
    (with-all-housekeeping-classes ((project-housekeeping-classes project))
      (maphash (lambda (k v)
                 (when (search name k)
                   (format t "~10a ~a~%" (on-stack-has-smart-pointers-on-heap-p (local-variable-ctype v) analysis) k)
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



