(declaim (optimize (debug 3)))


(defun typename_P_args_void (num)
  (eval `(concatenate 'string ,@(loop for i from 1 to num collect (format nil ",typename P~a=void" i)))))

(defun typename_P_args (num)
  (eval `(concatenate 'string ,@(loop for i from 1 to num collect (format nil ",typename P~a" i)))))

(defun typename_P_args_no_RT (num)
  (eval `(concatenate 'string ,@(loop for i from 1 to num collect (format nil ",typename P~a" i)))))



(defun P_args_noVoid (num &optional (max 0))
  (declare (ignore max))
  (eval `(concatenate 'string ,@(loop for i from 1 to num collect (format nil "~[~:;,~]P~a" (1- i) i)))))

(defun P_args_noVoid_start (start num &optional (max 0))
  (declare (ignore max))
  (eval `(concatenate 'string ,@(loop for i from start to num collect (format nil "~[~:;,~]P~a" (1- i) i)))))


(defun P_args_noVoid-first-comma (num &optional (max 0))
  (declare (ignore max))
  (eval `(concatenate 'string ,@(loop for i from 1 to num collect (format nil ",P~a" i)))))

(defun P_args (num max)
  (eval `(concatenate 'string ,@(loop for i from 1 to max collect (format nil (if (<= i num) ",P~a" ",void") i)))))



(defun flatten (structure)
  (cond ((null structure) nil)
        ((atom structure) `(,structure))
        (t (mapcan #'flatten structure))))




(defun from_object (spaces idx offset)
  (concatenate 'string
	       spaces
	       "typename translate::from_object<P"
	       (format nil "~a" idx)
	       "> a"
	       (format nil "~a(af->entry(singleDispatchArgumentIndex));~%" idx)
#||	       " = translate::from_object<P"
	       (format nil "~a" idx)
	       ">::convert(af->entry("
	       (format nil "~a" (1- idx))
	       "+"
	       (format nil "~a" offset)
	       "));~%"
	       ||#
	       ))


(defun from_object_with_single_dispatch (spaces iidx offset singleDispatchArgumentIndex )
  (concatenate 'string
	       (if (eql iidx 1)
		   (format nil "~aint idx = 0;~%" spaces )
		   (format nil "~a++idx; // advance to next arg~%" spaces ))
	       spaces
	       (format nil "if ( idx == ~a ) ++idx; // skip the single dispatch argument index ~%" singleDispatchArgumentIndex )
	       spaces
	       (format nil "typename translate::from_object<P~a> a~a(af->entry(idx));~%" iidx iidx)))


(defun all_from_objects (spaces arity &optional (offset 1))
  (format nil (eval `(concatenate
	  'string ,@(loop for i from 1 to arity
		       collect (from_object spaces  i offset))))))


(defun all_from_objects_with_single_dispatch (spaces arity singleDispatchArgumentIndex &optional (offset 1) )
  (format nil (eval `(concatenate
	  'string ,@(loop for i from 1 to arity
		       collect (from_object_with_single_dispatch spaces  i offset singleDispatchArgumentIndex ))))))


(defun method_args (num)
  (format nil (eval `(concatenate
	  'string ,@(loop for i from 1 to num
		       collect (concatenate 'string (if (/= i 1) ",") (format nil "a~a._v" i)))))))


;;




(defun fix-external (external)
  "EXTERNAL is only allowed to have the values '' or 'ExternalIndirect' or 'ExternalObject' "
  (cond
    ((string-equal external "externalindirect") "ExternalIndirect")
    ((string-equal external "externalobject") "ExternalObject")
    ((string= external "") "")
    (t
     (error "Illegal value for EXTERNAL - it must be empty string, ExternalIndirect or ExternalObject"))))


(defun create-methodPtrT-class (sout return-type arity max-arity &key const master (external ""))
  (setq external (fix-external external))
  (let* ((templateTree
	  (list
	   "~%~%~%"
	   "// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: " (if master "general_case" "partially_specialized") " returns(" return-type ") arity(" (format nil "~d" arity)  ")~%"
	   "template < typename OT" (if (equal return-type "RT") ", typename RT ") 
	   (if master (typename_P_args_void arity) (if (equal return-type "RT") (typename_P_args arity) (typename_P_args_no_RT arity) )) ">~%"
	   "class " const "ActivationFrame" external "MethodPtrT " (if master "" (list "<OT," return-type (#|P_args_noVoid|# P_args arity max-arity) ">"))  " {~%"
	   "   public:~%"
	   "      enum { NumParams = " (format nil "~a" arity) " };~%"
	   "      typedef " return-type " (OT" (when (string= external "ExternalIndirect") "::WrappedClass") "::*" const "Type)(" (P_args_noVoid arity) ") " const " ;~%"
	   "      static T_mv activate( " const "Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )~%"
	   "    {_G();~%"
	   "	if ( af->length() != " (format nil "~a" (1+ arity)) " )~%"
	   "	{~%"
	   "           // note: number of ActivationFrame entries passed is one more than the ~%"
	   "           // number of arguments for the method - one entry is for the receiver object~%"
	   "	    stringstream ss;~%"
	   "	    ss << \"Method expected \"<< " (format nil "~a" arity) " << \" argument(s) but was passed \" << (af->length()-1) << \" argument(s).\";~%"
	   "	    SIMPLE_ERROR_BF(ss.str());~%"
	   "	}~%"
	   "        typename translate::from_object<OT > ot(af->entry(singleDispatchArgumentIndex));~%"
	   (all_from_objects_with_single_dispatch "        " arity "singleDispatchArgumentIndex")
	   (if (not (string= external "ExternalIndirect"))
	       (list
		"        OT* receiver = ot._v.get();~%")
	       (list
		"        OT* otget = ot._v.get();~%"
		"        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();~%"
	       ))
	   "        ASSERTF(receiver!=NULL,BF(\"The receiver pointer is NULL and this should never happen\"));~%"
	   (if (equal return-type "RT")
	       "        RT retval = "
	       ""
	       )
	   "(receiver->*method)(" (method_args arity) ");~%"
	   (if (equal return-type "RT")
	       "        return translate::to_object<RT>::convert(retval);~%"
	       "        return Values0<core::T_O>();~%")
	   "   }~%"
	   "};~%"))
	 (template (flatten templateTree)))
    template
    (format sout (eval `(concatenate 'string ,@template)))
    ))
  

#|
(_methodPtrT t "void" 0 10 "" "")
(format t (eval `(concatenate 'string ,@res)))

(defparameter res (_methodPtrT_create "void" 0 10 "" ""))
(mapcar (lambda (x) (print (class-of x))) res)
(format t (eval `(concatenate 'string ,@res)))
|#





(defun create-methodptr-functoid (sout maxarity &key (const "") (external ""))
  (setq external (fix-external external))
  (let* ((template-tree
	  (list
	   "~%~%~%"
	   "// Wrapper for " const "ActivationFrame" external "MethodPtrT~%"
	   "template<typename OT, typename RT" (typename_P_args_void maxarity) " >~%"
	   "class " const "ActivationFrame" external "MethodWrapPtr : public SingleDispatchMethoid {~%"
	   "private:~%"
	   "    typedef typename " const "ActivationFrame" external "MethodPtrT<OT,RT" (P_args_noVoid-first-comma maxarity)  ">::" const "Type " const "MethPtr;~%"
	   "    " const "ActivationFrame" external "MethodPtrT<OT,RT" (P_args_noVoid-first-comma maxarity) ">		invoker;~%"
	   "    " const "MethPtr 				mptr;~%"
	   "public:~%"
	   "    string describe() const { return \"ActivationFrame" external "MethodWrapPtr\";}~%"
	   "    enum { NumParams = " const "ActivationFrame" external "MethodPtrT<OT,RT" (P_args_noVoid-first-comma maxarity) ">::NumParams };~%"
	   "    typedef OT      ReceiverT;~%"
	   "    typedef RT      ReturnT;~%"
           (loop for i from 1 to maxarity collect (format nil "    typedef P~a      Param~aT;~%" i i))
	   "// constructor~%"
	   "    " const "ActivationFrame" external "MethodWrapPtr(const string& name," const "MethPtr ptr) : SingleDispatchMethoid(name), mptr(ptr) {}~%"
	   "~%"
	   "    T_mv activate(const_ActivationFrame_spREF af)~%"
	   "    {_G();~%"
	   "        return this->invoker.activate(this->mptr,af,this->_SingleDispatchArgumentIndex);~%"
	   "    };~%"
	   "};~%"
	  ))
    (template (flatten template-tree)))
  (format sout (eval `(concatenate 'string ,@template)))))







(defun create-def (sout arity &key (const "") (external ""))
  (setq external (fix-external external))
  (let* ((tempTree
	  (list
	   "~%~%~%"
	   "template <typename RT" (typename_P_args arity) ">~%"
	   "    " (if (string= external "") "class_" "externalClass_") "& def( const string& name, RT (OT"
	   (if (string= external "ExternalIndirect")
	    "::WrappedClass"
	    ""
	    )
	   "::*mp)(" (P_args_noVoid arity) ") " const " , const string& lambda_list=\"\", const string& declares=\"\", const string& docstring=\"\", bool autoExport=true )~%"
	   "    {_G();~%"
	   "        SingleDispatchMethoid* m = new " const "ActivationFrame" external "MethodWrapPtr<OT,RT" (if (= arity 0) "" ",") (P_args_noVoid arity) ">(name,mp);~%"
	   "        lisp_defineSingleDispatchMethod(name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport," (format nil "~a" (1+ arity)) ");~%"
	   "        return *this;~%"
	   "    }~%"
	   ))
	(template (flatten tempTree)))
    template
    (format sout (eval `(concatenate 'string ,@template)))))




(defun create-def-accessor (sout arity &key external )
  (let* ((tempTree
	  (list
	   "~%~%~%"
	   "template <typename DATA, typename RT" (typename_P_args arity) ">~%"
	   "    class_& def_accessor( const string& name, DATA ptr_to_member_data,  RT (OT::*mp)(" (P_args_noVoid arity) ")  , const string& lambda_list=\"\", const string& declares=\"\", const string& docstring=\"\", bool autoExport=true )~%"
	   "    {_G();~%"
	   "        SingleDispatchMethoid* reader = new MemberDataReader<OT,DATA>(\"reader\"+name,ptr_to_member_data);~%"
	   "        lisp_defineSingleDispatchMethod(name,this->_ClassSymbol,reader,\"\",\"\",docstring,autoExport);~%"
	   "        SingleDispatchMethoid* writer = new ActivationFrameMethodWrapPtr<OT,RT" (if (= arity 0) "" ",") (P_args_noVoid arity) ">(\"accessor->\"+name,mp);~%"
	   "        string setf_name = \"setf_\" + name;~%"
	   "        lisp_defineSingleDispatchMethod(setf_name,this->_ClassSymbol,writer,lambda_list,declares,docstring,autoExport);~%"
	   "        return *this;~%"
	   "    }~%"
	   ))
	 (template (flatten tempTree)))
    template
    (format sout (eval `(concatenate 'string ,@template)))))













