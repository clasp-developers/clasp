  


(defun typename_P_args_void (num)
  (eval `(concatenate 'string ,@(loop for i from 1 to num collect (format nil ",typename P~a=void" i)))))

(defun typename_P_args (num)
  (eval `(concatenate 'string ,@(loop for i from 1 to num collect (format nil ",typename P~a" i)))))

(defun typename_P_args_no_RT (num)
  (eval `(concatenate 'string ,@(loop for i from 1 to num collect (format nil "~[~:;,~]typename P~a" (1- i) i)))))



(defun P_args_noVoid (num &optional (max 0))
  (declare (ignore max))
  (eval `(concatenate 'string ,@(loop for i from 1 to num collect (format nil "~[~:;,~]P~a" (1- i) i)))))

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
	       (format nil "~a(af->entry(~a+~a));~%" idx (1- idx) offset)
       
))


(defun all_from_objects (spaces arity &optional (offset 0))
  (format nil (eval `(concatenate
	  'string ,@(loop for i from 1 to arity
		       collect (from_object spaces  i offset))))))




(defun function_args (num)
  (format nil (eval `(concatenate
	  'string ,@(loop for i from 1 to num
		       collect (concatenate 'string (if (/= i 1) ",") (format nil "a~a._v" i)))))))


;;



(defun _functionPtrT_fill (return-type arity max-arity &key const master)
  (let* ((s2 (format nil "~a" arity))
	 (templateTree (list
			"~%~%~%"
			"// COMMENT: " (if master "general_case" "partially_specialized") " returns(" return-type ") arity(" (format nil "~d" arity)  ")~%"
			"template < typename Policies, " (if (equal return-type "RT") "typename RT ") 
			(if master (typename_P_args_void arity) (if (equal return-type "RT") (typename_P_args arity) (typename_P_args_no_RT arity))) ">~%"
			"class " (if const "Const" "") "ActivationFrameFunctionPtrT " (if master "" (list "<" return-type (#|P_args_noVoid|# P_args arity max-arity) ">"))  " {~%"
			"   public:~%"
			"      enum { NumParams = " s2 " };~%"
			"      typedef " return-type " (*" (if const "Const" "") "Type)(" (P_args_noVoid arity) ") " (if const "const" "")  " ;~%"
			"      static T_mv activate( Type fn, const_ActivationFrame_spREF af)~%"
			"    {_G();~%"
			"	if ( af->length() != " s2 " )~%"
			"	{~%"
			"	    stringstream ss;~%"
			"	    ss << \"Function expected \"<< (" s2 ") << \" argument(s) but was passed \" << af->length() << \" argument(s).\";~%"
			"	    SIMPLE_ERROR_BF(ss.str());~%"
			"	}~%"
			(all_from_objects "        " arity)
			(if (equal return-type "RT")
			    (list "        RT retval = fn(" (function_args arity) ");~%"
				  "        return(translate::to_object<RT>::convert(retval));~%")
			    (list "        fn(" (function_args arity) ");~%"
				  "        return Values0<core::T_O>();~%"))
			"   }~%"




			"};~%"))
	 (template (flatten templateTree)))
    template))
  
(defun functionPtrT_class (sout return-type arity max-arity &key const master)
  (let ((template (_functionPtrT_fill return-type arity max-arity :const const :master master)))
    (format sout (eval `(concatenate 'string ,@template)))
    ))

;;(functionPtrT_class t "RT" 2 10 )




(defun def_fill_worker (arity)
  (let* ((tempTree (list
		    "~%~%~%"
		    "template <typename RT" (typename_P_args arity) ">~%"
		    "    void af_def(const string& packageName, const string& name, RT (*fp)(" (P_args_noVoid arity) ") , const string& arguments=\"\", const string& declares=\"\", const string& docstring=\"\", int locked=1 )~%"
		    "    {_G();~%"
		    "        Functoid* f = new ActivationFrameFunctionWrapPtr<RT" (if (= arity 0) "" ",") (P_args_noVoid arity) ">(packageName+\"::\"+name,fp);~%"
		    "        lisp_defun_lispify_name(packageName,name,f,arguments,declares,docstring,locked,true," (format nil "~d" arity) ");~%"
		    "    }~%"
		    ))
	(template (flatten tempTree)))
    template))

(defun fill_def (sout num)
  (format sout (eval `(concatenate 'string ,@(def_fill_worker num)))))






(defun fill_wrap_ptr (sout maxarity)
  (let* ((template-tree
	  (list
	   "~%~%~%"
	   "// Wrapper for ActivationFrameFunctionPtrT~%"
	   "template<typename RT" (typename_P_args_void maxarity) " >~%"
	   "class ActivationFrameFunctionWrapPtr : public AFFunctoid {~%"
	   "private:~%"
	   "    typedef typename ActivationFrameFunctionPtrT<RT" (P_args_noVoid-first-comma maxarity)  ">::Type FuncPtr;~%"
	   "    ActivationFrameFunctionPtrT<RT" (P_args_noVoid-first-comma maxarity) ">		invoker;~%"
	   "    FuncPtr 				fptr;~%"
	   "public:~%"
	   "    string describe() const { return \"ActivationFrameFunctionWrapPtr\";}~%"
	   "    enum { NumParams = ActivationFrameFunctionPtrT<RT" (P_args_noVoid-first-comma maxarity) ">::NumParams };~%"
	   "    typedef RT      ReturnT;~%"
           (loop for i from 1 to maxarity collect (format nil "    typedef P~a      Param~aT;~%" i i))
	   "// constructor~%"
	   "    ActivationFrameFunctionWrapPtr(const string& name, FuncPtr ptr) : AFFunctoid(name), fptr(ptr) {}~%"
	   "~%"
	   "    T_mv activate(const_ActivationFrame_spREF af)~%"
	   "    {_G();~%"
	   "        return(this->invoker.activate(this->fptr,af));~%"
	   "    };~%"
	   "};~%"
	  ))
    (template (flatten template-tree)))
  (format sout (eval `(concatenate 'string ,@template)))))




(defparameter *maxarity* 16) ;; Need 16 for WRITE and that's the highest you need

(with-open-file (sout (si:argv 3) :direction :output :if-exists :supersede)
  (format sout "#ifndef af_functionptrt_H~%")
  (format sout "#define af_functionptrt_H~%")
  (do
   ((i *maxarity* (1- i)))
   ((< i 0))
    (functionPtrT_class sout "RT" i *maxarity* :master (eql i *maxarity*)))
  (do
   ((i *maxarity* (1- i)))
   ((< i 0))
    (functionPtrT_class sout "void" i *maxarity* ))
  (fill_wrap_ptr sout *maxarity*)
  (dotimes (i (1+ *maxarity*)) (fill_def sout i))
  (format sout "#endif // af_functionptrt_H~%")
)
