
(declaim (optimize (debug 2)))


(defun flatten (structure)
  (cond ((null structure) nil)
        ((atom structure) `(,structure))
        (t (mapcan #'flatten structure))))


(defparameter *current-package* () )
(defparameter *current-namespace* () )
(defparameter *current-wrapped-namespace* ())
(defmacro set-package (pkg ns external-ns)
  (setf *current-package* pkg)
  (setf *current-namespace* ns)
  (setf *current-wrapped-namespace* external-ns)
  )

(defparameter *all-wrapped-ptr-classes* ())
(defparameter *current-wrapped-ptr-class* ())
(defparameter *prev-class* ())

(defun clear-wrapped-ptr-classes ()
  (setq *all-wrapped-ptr-classes* ()))




(defstruct external-class
  lisp-name
  class-name
  classo-name
  base-classo-name
  ctor-code
  dtor-code
  init ;; plist containing keys :lisp-args :c-args and c-code
  other-function-prototypes
  from-object-translators
  to-object-translators
  exposed-static-methods #| exposed static class methods of the class_O type |#
  static-methods         #| unexposed static class methods of the class_O type |#
  wrapped-methods        #| wrapped methods of the PTR-TYPE |#
  wrapped-static-methods #| wrapped static class methods of the PTR-TYPE |#
)


;;
;; Throw an error if all three keys aren't present
;; in the init plist
(defun init-validate (init)
  (assert (getf init :lisp-args))
;;  (assert (getf init :lisp-decl))
;;  (assert (getf init :lisp-docs))
  (assert (getf init :c-args))
  (assert (getf init :c-code))
  )

(defun external-class-init-lisp-args (cl)
  (getf (external-class-init cl) :lisp-args))

(defun external-class-init-lisp-decl (cl)
  (getf (external-class-init cl) :lisp-decl "" ))

(defun external-class-init-lisp-docs (cl)
  (getf (external-class-init cl) :lisp-docs "" ))


(defun external-class-init-c-args (cl)
  (getf (external-class-init cl) :c-args))

(defun external-class-init-c-code (cl)
  (getf (external-class-init cl) :c-code))



(defstruct (wrapped-ptr-class (:include external-class))
  polymorphic-ptr   ;; returns whether the wrapped class is polymorphic or not
  ptr-name
  ptr-type
)

(defstruct static-method
  c++-name
  returns
  arguments
  body )


(defstruct (exposed-static-method (:include static-method))
  lisp-name
  )

(defun static-method-header (static-method)
  (format nil "static ~a ~a~a;~%"
	  (static-method-returns static-method)
	  (static-method-c++-name static-method)
	  (static-method-arguments static-method)
))

(defun static-method-implementation (meth)
  (format nil "~a <_CLASSO-NAME_>::~a~a ~a~%"
	  (static-method-returns meth)
	  (static-method-c++-name meth)
	  (static-method-arguments meth)
	  (static-method-body meth)))


(defun exposed-static-method-expose (meth)
  (format nil "af_def(<_PACKAGE_>,\"~a\",&<_CLASSO-NAME_>::~a);~%"
	  (exposed-static-method-lisp-name meth)
	  (static-method-c++-name meth)
	  ))


(defgeneric expose-wrapped-static-method (meth))

(defstruct <wrapped-static-method>
  name
)

(defmethod expose-wrapped-static-method ( (meth <wrapped-static-method>))
    (format nil "core::af_def(<_PACKAGE_>,\"<_CLASS-NAME_>-~a\",&<_WRAPPED-NAMESPACE_>::<_CLASS-NAME_>::~a);~%"
     (<wrapped-static-method>-name meth)
     (<wrapped-static-method>-name meth)))

(defstruct <overloaded-wrapped-static-method>
  lisp-name
  c++-returns
  c++-name
  c++-args)

;;(defparameter *a* (make-<overloaded-wrapped-static-method> :lisp-name "lisp" :c++-returns "bool" :c++-name "c++-name" :c++-args "args"))
;;(<overloaded-wrapped-static-method>-c++-returns *a*)

(defparameter *unique-fn-counter* 1)

(defmethod expose-wrapped-static-method ( (meth <overloaded-wrapped-static-method>))
  (let ((sym (format nil "fx~a" *unique-fn-counter*)))
    (setq *unique-fn-counter* (1+ *unique-fn-counter*))
    (list
     (format nil "    ~a (*~a)~a = &<_PTR-TYPE_>::~a;~%" 
	     (<overloaded-wrapped-static-method>-c++-returns meth)
	     sym
	     (<overloaded-wrapped-static-method>-c++-args meth)
	     (<overloaded-wrapped-static-method>-c++-name meth)
	     )
     (format nil "    core::af_def(<_PACKAGE_>,\"~a\",~a);~%"
	     (<overloaded-wrapped-static-method>-lisp-name meth)
	     sym)
     )
    )
  )
	    
	    
  
	   







(defstruct wrapped-method
  name
  )

(defun wrapped-method-expose (meth)
  (format nil ".def(\"~a\", &<_PTR-TYPE_>::~a)~%"
	  (wrapped-method-name meth)
	  (wrapped-method-name meth)
	  ))




(defstruct object-method 
  name
  define
  code
  expose
)

;;
;; object-raw-methods are methods that have the form
;; T_sp <fn>(Executable_sp exec, Cons_sp args, Environment_sp env, Lisp_sp lisp)
(defstruct object-raw-method
  name
  code
)

(defun object-raw-method-define (meth)
  (format nil "core::T_sp ~a(core::Executable_sp exec, core::Cons_sp args, core::Environment_sp env, core::Lisp_sp lisp);~%" (object-raw-name meth)))












(defstruct from-object-translator
  to-type
  return-type
  code
  )

(defun expand-from-object-translator-code (trans)
  (let* ((to-type (from-object-translator-to-type trans))
	 (raw-return-type (from-object-translator-return-type trans))
	 (return-type (if raw-return-type raw-return-type to-type))
	 (code (from-object-translator-code trans))
	 )
  (format nil "
namespace translate
{
    template <>
    struct from_object<~a>
    {
        typedef ~a ExpectedType;
        typedef ~a DeclareType;
        static DeclareType convert(core::T_sp object)
        ~a
    };
};
"  to-type to-type return-type code )))





(defstruct to-object-translator
  from-type
  code
  )

(defun expand-to-object-translator-code (trans)
  (let* ((from-type (to-object-translator-from-type trans))
	 (code (to-object-translator-code trans))
	 )
  (format nil "
namespace translate
{
    template <>
    struct to_object<~a>
    {
        static core::T_sp convert(~a ptr)
        ~a
    };
};
" from-type from-type code)))



(defun macro-alist (cl)
  `(
    ( "<_PACKAGE_>" . ,*current-package*)
    ( "<_WRAPPED-NAMESPACE_>" . ,*current-wrapped-namespace* )
    ( "<_NAMESPACE_>" . ,*current-namespace*)
    ( "<_CLASS_sp-NAME_>" . ,(format nil "~a_sp" (wrapped-ptr-class-class-name cl)))
    ( "<_CLASS-NAME_>" . ,(wrapped-ptr-class-class-name cl))
    ( "<_CLASSO-NAME_>" . ,(wrapped-ptr-class-classo-name cl))
    ( "<_BASE-CLASSO-NAME_>" . ,(wrapped-ptr-class-base-classo-name cl))
    ( "<_LISP-NAME_>" . ,(wrapped-ptr-class-lisp-name cl))
    ( "<_PTR-NAME_>" . ,(wrapped-ptr-class-ptr-name cl))
    ( "<_PTR-TYPE_>" . ,(wrapped-ptr-class-ptr-type cl))
    ( "<_SELF_>" . "self")
   ))






(defparameter *all-symbols* ())

(defun e-symbol (sym)
  (setf *all-symbols* (cons sym *all-symbols*)))

(defun replace-one (src match-str replace-str)
  (let ((match-begin (search match-str src)))
    (if match-begin
	(let* ((match-end (+ match-begin (length match-str)))
	       (front (subseq src 0 match-begin))
	       (back (subseq src match-end (length src))))
	  (values (concatenate 'string front replace-str back) t))
	(values src nil))))

(defun replace-all (src match-str replace-str)
  (if (search match-str src)
      (let ((rep (replace-one src match-str replace-str)))
	(replace-all rep match-str replace-str))
      src))

;;
;; return the string src after all substitutions in 
;; the alist are made eg:
;; (e-replace  '( ( "<=ZZ=>" . "AAA") ("<=YY=>" . "TEST"))) "ab<=ZZ=>efg<=ZZ=>hi<=YY=>jk"
(defun e-replace (alist src)
  (if alist
      (let* ((match-str (caar alist))
	     (replace-str (cdar alist))
	     (rest-alist (cdr alist))
	     (replaced (replace-all src match-str replace-str)))
	(e-replace rest-alist replaced))
      src))

(defun e-replace-all (alist &rest src-tree)
  (let* ((src-list (flatten src-tree))
	 (all-src (apply #'concatenate 'string src-list)))
    (e-replace alist all-src)))




	


;;
;; Define a new wrapped class and define its components
(defun e-ptr-class (&key
		      (polymorphic t)
		      lisp-name
		      class-name
		      (base-classo-name "core::ExternalObject_O")
		      exposed-static-methods
		      static-methods
		      other-function-prototypes
		      wrapped-method-names
		      wrapped-static-methods
		      ctor-code
		      dtor-code
		      ptr-type

		      (ptr-name "_ptr")
		      init
		      from-object-translators
		      to-object-translators
		      )
  (when init
    (init-validate init))
;;  (break "About to make-wrapped-ptr-class")
  (let ((cl (make-wrapped-ptr-class
	     :polymorphic-ptr polymorphic
	     :lisp-name lisp-name
	     :class-name class-name
	     :classo-name (concatenate 'string class-name "_O")
	     :base-classo-name base-classo-name
	     :ptr-type ptr-type
	     :ptr-name ptr-name
	     :init init
	     :other-function-prototypes other-function-prototypes
	     :static-methods static-methods
	     :exposed-static-methods exposed-static-methods
	     :wrapped-methods (mapcar #'(lambda (x) (make-wrapped-method :name x)) wrapped-method-names)
	     :wrapped-static-methods wrapped-static-methods
	     :from-object-translators from-object-translators
	     :to-object-translators to-object-translators
	     )))
    (when (not ctor-code)
      (setf (wrapped-ptr-class-ctor-code cl) (format nil "<_CLASSO-NAME_>(core::MetaClass_sp const& mc) : Base(mc)~a {};~%"
						  (if ptr-name
						      (format nil ", ~a(NULL) " ptr-name)
						      ""))))
    (when (not dtor-code)
      (setf (wrapped-ptr-class-dtor-code cl) (format nil "~~<_CLASSO-NAME_>() {~a}~%"
						  (if ptr-name
						      (format nil "if (~a != NULL ) {/* delete ~a;*/ ~a = NULL;};" ptr-name ptr-name ptr-name)
						      ""))))
    (setf *prev-class* cl)
    (setf *all-wrapped-ptr-classes* (acons lisp-name cl *all-wrapped-ptr-classes*))
    )
  )


(defun e-derived-class (&rest args)
  (apply #'e-ptr-class :ptr-name () args))



;; Define the pointer name and class that is wrapped by this wrapped-ptr-class
;;(defun e-wrapped (wrapped-ptr-type wrapped-ptr-name)
;;  (setf (wrapped-ptr-class-ptr-name *current-wrapped-ptr-class*) wrapped-ptr-name)
;;  (setf (wrapped-ptr-class-ptr-type *current-wrapped-ptr-class*) wrapped-ptr-type))


#|
(defun o-method (&key name define code (expose t))
  (let ((the-method (make-object-method :name name
					:define define
					:code code
					:expose expose)))
    (setf (wrapped-ptr-class-object-methods *current-wrapped-ptr-class*)
	  (cons the-method (wrapped-ptr-class-object-methods *current-wrapped-ptr-class*)))))
|#







(defun e-write-tree (sout text-tree)
;;  (when (consp text-tree) (break "In e-write-tree with: ~a" (subseq (car text-tree) 0 100)))
  (let ((text-list (flatten text-tree)))
    (dolist (one-text text-list)
      (format sout "~a~%" one-text))))
      


(defun e-expose-wrapped-method (meth)
  (format nil "        .def(\"~a\",&<_PTR-TYPE_>::~a)~%" (wrapped-method-name meth) (wrapped-method-name meth)))

(defun e-expose-object-method (meth)
  (format nil "        .def(\"~a\",&<_CLASSO-NAME_>::~a)~%"
	  (object-method-name meth)
	  (object-method-name meth)))

(defun e-expose-object-raw-method (meth)
  (format nil "        .defNoWrap(\"~a\",&<_CLASSO-NAME_>::~a,\"~a\")~%"
	  (object-raw-method-name meth)
	  (object-raw-method-name meth)
	  (object-raw-method-args meth)))



(defun expand-header (cl)
  (apply #'concatenate 'string
	 (flatten
	  (list
	   "
namespace <_NAMESPACE_>
{
FORWARD(<_CLASS-NAME_>);
class <_CLASSO-NAME_> : public <_BASE-CLASSO-NAME_>
{
    LISP_EXTERNAL_CLASS(<_PACKAGE_>,<_PTR-TYPE_>,<_CLASSO-NAME_>,\"<_LISP-NAME_>\",<_BASE-CLASSO-NAME_>);
    typedef <_PTR-TYPE_> WrappedType;
    typedef <_PTR-TYPE_>* PointerToWrappedType;
    typedef PointerToWrappedType ExternalType;
"
	   (when (wrapped-ptr-class-ptr-name cl)
	     "
protected:
    PointerToWrappedType <_PTR-NAME_>;
public:
    virtual void* externalObject() const
    {
        return this-><_PTR-NAME_>;
    };
    PointerToWrappedType wrappedPtr() const
    {
        return this-><_PTR-NAME_>;
    }
")
	   "
public:"
	   (unless (wrapped-ptr-class-ptr-name cl)
	     (list
	      "    PointerToWrappedType wrappedPtr() const { return "
	      (if (wrapped-ptr-class-polymorphic-ptr cl) "dynamic_cast" "static_cast")
	      "<PointerToWrappedType>(this->_ptr);};"
	      ))
"
    void set_wrapped(PointerToWrappedType ptr)
    {
/*        if (this->_ptr != NULL ) delete this->_ptr; */
        this->_ptr = ptr;
    }
"
	   (mapcar #'(lambda (x) (format nil "~a;~%" (static-method-header x))) (wrapped-ptr-class-static-methods cl))
	   (mapcar #'(lambda (x) (format nil "~a;~%" (static-method-header x))) (wrapped-ptr-class-exposed-static-methods cl))
	   (wrapped-ptr-class-ctor-code cl)
	   (wrapped-ptr-class-dtor-code cl)
	   (when (wrapped-ptr-class-init cl)
	     (format nil "    static <_CLASS-NAME_>_sp make~a;~%" (external-class-init-c-args cl)  ) )
	   (wrapped-ptr-class-other-function-prototypes cl)
	   (format nil "~%")
	   ;; Now you can include the header-stream stuff
	   (format nil " }; // ~a~%" (wrapped-ptr-class-classo-name cl))
	   (format nil " }; // ~a~%" *current-namespace* )
	   (format nil "TRANSLATE(<_NAMESPACE_>::<_CLASSO-NAME_>);~%")
	   "/* from_object translators */
"
	   (mapcar #'(lambda (x) (format nil "~a;~%" (expand-from-object-translator-code x))) (wrapped-ptr-class-from-object-translators cl))	
	   "/* to_object translators */
"
	   (mapcar #'(lambda (x) (format nil "~a;~%" (expand-to-object-translator-code x))) (wrapped-ptr-class-to-object-translators cl))
	   )
	  )
	 ) )





(defun expand-exposers (cl)
  (apply #'concatenate 'string
	 (flatten
	  (list
	   "
namespace <_NAMESPACE_>
{
EXPOSE_CLASS(<_NAMESPACE_>,<_CLASSO-NAME_>);

void <_CLASSO-NAME_>::exposeCando(core::Lisp_sp lisp)
{_G();
    core::externalClass_<<_CLASSO-NAME_>>(_lisp,core::no_init)
"
	   (mapcar #'(lambda (x) (format nil "~a~%" (wrapped-method-expose x))) (wrapped-ptr-class-wrapped-methods cl))
	   ";
"
	   (mapcar #'(lambda (x) (format nil "~a~%" (exposed-static-method-expose x))) (wrapped-ptr-class-exposed-static-methods cl))
	   #| expose the class_O maker |#
	   (when (wrapped-ptr-class-init cl)
	     (format nil "        core::af_def(<_PACKAGE_>,\"make-<_CLASS-NAME_>\",&<_CLASSO-NAME_>::make,ARGS_<_CLASSO-NAME_>_make,DECL_<_CLASSO-NAME_>_make,DOCS_<_CLASSO-NAME_>_make);~%"))
	   #| EXPOSE wrapped static class member functions here |#
	   (mapcar #'(lambda (x) (expose-wrapped-static-method x)) (external-class-wrapped-static-methods cl))


	   "};
"
"
	   void <_CLASSO-NAME_>::exposePython(core::Lisp_sp lisp)
	   {_G();
	       IMPLEMENT_ME();
           };
}; // <_NAMESPACE_>" ) ) ) )



(defun expand-code (cl)
  (apply #'concatenate 'string
	 (flatten
	  (list
	   (format nil "namespace <_NAMESPACE_>~%")
	   (format nil "{~%")
	   
	   (when (wrapped-ptr-class-init cl)
	     (list
	      (format nil "#define ARGS_<_CLASSO-NAME_>_make \"~a\"~%" (external-class-init-lisp-args cl))
	      (format nil "#define DECL_<_CLASSO-NAME_>_make \"~a\"~%" (external-class-init-lisp-decl cl))
	      (format nil "#define DOCS_<_CLASSO-NAME_>_make \"~a\"~%" (external-class-init-lisp-docs cl))

	      (format nil "<_CLASS-NAME_>_sp <_CLASSO-NAME_>::make~a~%" (external-class-init-c-args cl))
	      (format nil "{_G();~%")
	      (format nil "    <_CLASS-NAME_>_sp <_SELF_> = RP_Create<<_CLASSO-NAME_>>(_lisp);~%")
	      (format nil "~a~%" (external-class-init-c-code cl))
	      (format nil "    return <_SELF_>;~%")
	      (format nil "};~%")
	      ))
	   (mapcar #'(lambda (x) (format nil "~a;~%" (static-method-implementation x))) (wrapped-ptr-class-static-methods cl))
	   (mapcar #'(lambda (x) (format nil "~a;~%" (static-method-implementation x))) (wrapped-ptr-class-exposed-static-methods cl))
	   (format nil "}~%")
	   ) ) ) )








(defun e-write-one-class-header (sout cl)
  (let ((replace-dict (macro-alist cl)))
    (e-write-tree sout (e-replace-all replace-dict (expand-header cl)))))


(defun e-write-one-class-exposer (sout cl)
  (let ((replace-dict (macro-alist cl)))
;;    (break "replace-dict --> ~a" replace-dict)
    (e-write-tree sout (e-replace-all replace-dict (expand-exposers cl)))))

(defun e-write-one-class-code (sout cl)
  (let ((replace-dict (macro-alist cl)))
    (e-write-tree sout (e-replace-all replace-dict (expand-code cl)))))



(defun e-write-all-headers (sout)
  (mapcar #'(lambda (pair) (e-write-one-class-header sout (cdr pair))) (reverse *all-wrapped-ptr-classes*))
)



(defun e-write-all-codes-and-exposers (sout)
  (mapcar #'(lambda (pair)
	      (e-write-one-class-code sout (cdr pair))
	      (e-write-one-class-exposer sout (cdr pair))
	      ) (reverse *all-wrapped-ptr-classes*))
)



(defun e-write-all-classes (file-prefix &key (header-prefix "") (code-prefix "") )
  (with-open-file (sout (format nil "~a_gen.h" file-prefix) :direction :output :if-exists :supersede)
    (format sout "#ifndef _~a_~a_H~%" *current-package* file-prefix)
    (format sout "#define _~a_~a_H~%" *current-package* file-prefix)
    (format sout "~a~%" header-prefix)
    (e-write-all-headers sout)
    (format sout "#endif // _~a_~a_H~%" *current-package* file-prefix)
    )


  (with-open-file (sout (format nil "~a_gen.cc" file-prefix) :direction :output :if-exists :supersede)
    (format sout "~a~%" code-prefix)
    (e-write-all-codes-and-exposers sout)
    )
  )





