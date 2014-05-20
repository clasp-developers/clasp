(provide 'gc-builder)


(defun print-gc-info (&optional (inheritance *inheritance*) (instance-variables *instance-variables*))
  (format t "(progn~%")
  (format t "(export '(gctools::instance-variable gctools::inheritance gctools::not-yet-classified-template-specialization-type gctools::not-yet-classified gctools::smart-ptr gctools::weak-smart-ptr gctools::bases gctools::virtual-bases) :gctools)~%")
  (format t "(defparameter *inheritance* (make-hash-table :test #'equal))~%")
  (format t "(defparameter *instance-variables* (make-hash-table :test #'equal))~%")
  (maphash #'(lambda (k v) (format t "(setf (gethash \"~a\" *inheritance*) '~a)~%" k v)) inheritance)
  (maphash #'(lambda (k v) (format t "(setf (gethash \"~a\" *instance-variables*) '~a)~%" k v)) instance-variables)
  (format t ")~%")
  )


(defstruct inheritance-relationship
  myclass
  bases
  vbases )


(defstruct instance-variable
  field-name
  class-name
  ctype
)



(defstruct ctype )

(defstruct (simple-ctype (:include ctype))
  description)

(defstruct (unclassified-ctype (:include simple-ctype)))

(defstruct (smart-ptr-ctype (:include ctype))
  specializer
  )


(defstruct (weak-smart-ptr-ctype (:include smart-ptr-ctype))
)

(defstruct (rest-argument (:include simple-ctype)))


(defstruct (container (:include ctype))
  name
  arguments
  )

(defmethod container-argument ((x container) idx)
    (dolist (arg (container-arguments x))
      (when (eql (template-argument-index arg) idx) (return arg))))


(defstruct (record-ctype (:include ctype))
  description)

(defstruct (pointer-to-record-ctype (:include ctype))
  description)

(defstruct (stl-container (:include container)))

(defstruct (gcholder (:include container)))

(defstruct (gcobject-derived (:include ctype))
  description)

(defstruct template-argument 
  index
  ctype)

(defmethod is-smart-p ((x ctype)) nil)
(defmethod is-smart-p ((x smart-ptr-ctype)) t)

(defmethod is-smart-p ((x template-argument))
  (is-smart-p (template-argument-ctype x)))


(defmethod macro-name ((x gcholder))
  (format nil "~a_FIX" (container-name x)))

(defmethod macro-name ((x stl-container))
  (case (stl-container-name x)
    ('stlmap
     (let ((first-arg (container-argument x 0))
	   (second-arg (container-argument x 1)))
       (cond
	 ((and (is-smart-p first-arg) (is-smart-p second-arg)) "STLMAP_SMART_FIRST_SECOND_FIX")
	 ((is-smart-p first-arg) "STLMAP_SMART_FIRST_FIX")
	 ((is-smart-p second-arg) "STLMAP_SMART_SECOND_FIX")
	 (t "IGNORE"))))
    ('stlmultimap
     (let ((first-arg (container-argument x 0))
	   (second-arg (container-argument x 1)))
       (cond
	 ((and (is-smart-p first-arg) (is-smart-p second-arg)) "STLMULTIMAP_SMART_FIRST_SECOND_FIX")
	 ((is-smart-p first-arg) "STLMULTIMAP_SMART_FIRST_FIX")
	 ((is-smart-p second-arg) "STLMULTIMAP_SMART_SECOND_FIX")
	 (t "IGNORE"))))
    ('stlvector
     (let* ((arg (container-argument x 0))
	    (type-info (template-argument-ctype arg)))
       (format t "IN macro-name ::: type-info --> ~a    (unclassified-ctype-description type-info) --> ~a~%" type-info (unclassified-ctype-description type-info) )
       (cond
	 ((is-smart-p arg) "STLVECTOR_SMART_FIX")
	 ((equal (unclassified-ctype-description type-info) "struct core::RequiredArgument") "STL_VECTOR_REQUIRED_ARGUMENT_FIX")
	 ((equal (unclassified-ctype-description type-info) "struct core::OptionalArgument") "STL_VECTOR_OPTIONAL_ARGUMENT_FIX")
	 ((equal (unclassified-ctype-description type-info) "struct core::KeywordArgument")  "STL_VECTOR_KEYWORD_ARGUMENT_FIX")
	 ((equal (unclassified-ctype-description type-info) "struct core::AuxArgument")      "STL_VECTOR_AUX_ARGUMENT_FIX")
	 (t "IGNORE"))))))

(defmethod macro-name ((x smart-ptr-ctype))
  "SMART_PTR_FIX")

(defmethod macro-name ((x weak-smart-ptr-ctype))
  "WEAK_SMART_PTR_FIX")

(defmethod macro-name ((x unclassified-ctype))
  "IGNORE")

(defmethod macro-name ((x ctype))
  (format nil "HANDLE_CTYPE-~a" (type-of x)))

(defmethod macro-name ((x rest-argument))
  "REST_ARGUMENT_FIX")

#||case type
		       ('smart-ptr-ctype "SMART_PTR_FIX")
		       ('weak-smart-ptr-ctype "WEAK_SMART_PTR_FIX")
		       ('vector0 "VECTOR0_FIX")
		       ('gctools:set "SET_FIX")
		       ('gctools:indexed-symbol-map "INDEXED_SYMBOL_MAP_FIX")
		       ('gctools:string-map "STRING_MAP_FIX")
		       ('gctools:symbol-map "SYMBOL_MAP_FIX")
		       ('gctools:stl-map
			(cond
			  ((and (is-smart-p (cadr type-info)) (is-smart-p (caddr type-info))) "STL_MAP_SMART_FIRST_SECOND_FIX")
			  ((is-smart-p (cadr type-info)) "STL_MAP_SMART_FIRST_FIX")
			  ((is-smart-p (caddr type-info)) "STL_MAP_SMART_SECOND_FIX")
			  (t "IGNORE")))
		       ('gctools:stl-multimap
			(cond
			  ((and (is-smart-p (cadr type-info)) (is-smart-p (caddr type-info))) "STL_MULTIMAP_SMART_FIRST_SECOND_FIX")
			  ((is-smart-p (cadr type-info)) "STL_MULTIMAP_SMART_FIRST_FIX")
			  ((is-smart-p (caddr type-info)) "STL_MULTIMAP_SMART_SECOND_FIX")
			  (t "IGNORE")))
		       ('gctools:stl-vector
			(cond
			  ((is-smart-p (cadr type-info)) "STL_VECTOR_SMART_FIX")
			  ((equal (caddr (cadr type-info)) "struct core::RequiredArgument") "STL_VECTOR_REQUIRED_ARGUMENT_FIX")
			  ((equal (caddr (cadr type-info)) "struct core::OptionalArgument") "STL_VECTOR_OPTIONAL_ARGUMENT_FIX")
			  ((equal (caddr (cadr type-info)) "struct core::KeywordArgument")  "STL_VECTOR_KEYWORD_ARGUMENT_FIX")
			  ((equal (caddr (cadr type-info)) "struct core::AuxArgument")      "STL_VECTOR_AUX_ARGUMENT_FIX")
			  (t "IGNORE")))
		       ('gctools:rest-argument "REST_ARGUMENT_FIX")
		       ('gctools:stl-set
			(if (is-smart-p (cadr type-info))
			    "STL_SET_FIX"
			    "IGNORE"))
		       (t "IGNORE"))))
||#





(defun one-class-instance-variables (result-ht classid instance-variables)
  (let ((vars (gethash classid instance-variables)))
    (mapc #'(lambda (x)
	      (when (gethash (instance-variable-field-name x) result-ht)
		  (warn (format nil "Instance variable already exists[~a] type[~a]" (instance-variable-field-name x) (instance-variable-ctype x))))
	      (setf (gethash (instance-variable-field-name x) result-ht) x))
	  vars)))

(defun ancestor-instance-variables-recursive (result-ht classid instance-variables inheritance)
  "Add the instance variables for classid and then do the same for its base classes"
  (one-class-instance-variables result-ht classid instance-variables)
  (let* ((inheritance-info (gethash classid inheritance))
	(bases (if inheritance-info
		   (inheritance-relationship-bases inheritance-info)
		   nil)))
    (mapc #'(lambda (x)
	      (ancestor-instance-variables-recursive result-ht x instance-variables inheritance)) bases)))

(defun gather-instance-variables (classid instance-variables inheritance)
  "Gather all of the instance variables for a class (including the instance variables of base classes)"
  (let ((gathered (make-hash-table :test #'equal)))
    (ancestor-instance-variables-recursive gathered classid instance-variables inheritance)
    gathered))



(defun split-by-one-space (string)
  "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
  (loop for i = 0 then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))


(defun ctype-name (classid)
  (cadr (split-by-one-space classid)))


(defun class-enum-name (classid)
  (let* ((ctype-name (ctype-name classid))
	 (enum-name (substitute #\_ #\: ctype-name)))
    (format nil "KIND_~a" enum-name)))



(defun code-for-instance-var (output-stream ptr-name instance-var type-record)
  (let* ((type-info (instance-variable-ctype type-record))
	 (type (type-of type-info))
	 (macro-name (macro-name type-info)))
    (when macro-name
      (format output-stream "    ~A(~A->~A); /* ~a */~%" macro-name ptr-name instance-var type-info))))



(defun generate-kind-enum (inheritance &key (output-stream t))
  (format output-stream "enum { KIND_null, ~%")
  (let ((num 0))
    (maphash #'(lambda (classid value)
		 (setq num (1+ num))
		 (let ((enum-name (class-enum-name classid)))
		   (format output-stream "~A, " enum-name)))
	     inheritance)
    (format output-stream "KIND_max /* idx=~A */, KIND_fwd, KIND_fwd2, KIND_pad1, KIND_pad }~%" num)))


(defun generate-kind-selectors (inheritance &key (output-stream t))
  (let ((num 0))
    (maphash #'(lambda (classid value)
		 (setq num (1+ num))
		 (let ((enum-name (class-enum-name classid)))
		   (format output-stream "template <> GcKindEnum GcKindSelector<~A>() { return ~A; };~%" (ctype-name classid) enum-name)))
	     inheritance)
    ))


(defun generate-kind-name-map (inheritance &key (output-stream t))
  (let ((num 0))
    (maphash #'(lambda (classid value)
		 (setq num (1+ num))
		 (let ((enum-name (class-enum-name classid)))
		   (format output-stream "   case ~A: return \"~A\";~%" enum-name enum-name)))
	     inheritance)
    ))


(defconstant +ptr-name+ "obj")
(defun build-mps-scan (inheritance instance-variables &key (output-stream t))
  (format t "Entered build-mps-scan~%")
  (maphash #'(lambda (classid value)
	       (format t "build-mps-scan -> inheritance classid[~a]  value[~a]~%" classid value)
	       (let ((all-instance-variables (gather-instance-variables classid instance-variables inheritance)))
		 (format output-stream "case ~a: {~%" (class-enum-name classid))
		 (format output-stream "    ~A* ~A = reinterpret_cast<~A*>(obj_ptr);~%" (ctype-name classid) +ptr-name+ (ctype-name classid))
		 (format output-stream "    DEBUG_SCAN_OBJECT(~A);~%" +ptr-name+)
		 (maphash #'(lambda (k v) (code-for-instance-var output-stream +ptr-name+ k v)) all-instance-variables)
		 (format output-stream "    base = (char*)base + ALIGN(SIZEOF_WITH_HEADER(~A));~%" (ctype-name classid))
		 (format output-stream "} break;~%")
		 ))
	   inheritance ))

(defun build-mps-skip (inheritance instance-variables &key (output-stream t))
  (maphash #'(lambda (classid value)
	       (format output-stream "case ~a: {~%" (class-enum-name classid))
	       (format output-stream "    base = (char*)base + ALIGN(SIZEOF_WITH_HEADER(~A));~%" (ctype-name classid))
	       (format output-stream "} break;~%")
	       )
	   inheritance
	   ))

;;(print-gc-info *inheritance* *instance-variables*)


(defun generate-gc-info ()
  (format t "Starting ast-search~%")
  (let ((results (gctools:ast-search '(gctools:garbage-collector-scanner) *compile-commands-path* (list *all-headers*))))
    (destructuring-bind (inheritance instance-variables)
	(cdr (assoc 'gctools:garbage-collector-scanner results))
      (values inheritance instance-variables))))


(defun find-globals-locals-parameters (&key testing)
  (let ((results (gctools:ast-search '(gctools:global-variables gctools:local-variables)
				     *compile-commands-path*
			    (if testing *testing-files* nil))))
    (let ((globals (cdr (assoc 'gctools:global-variables results)))
	  (locals (cdr (assoc 'gctools:local-variables results)))
	  (parameters (cdr (assoc 'gctools:parameters results))))
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
    (maphash #'(lambda (var-name v)
		 (format t "Var: ~a    v-> ~a~%" var-name v)
		 (let ((source-location (car v))
		       (type-info (caddr v)))
		   (format t "type-info --> ~A~%" type-info)
		   (cond
		     ((or (and (smart-ptr-ctype-p type-info) (string= "class core::Symbol_O" (smart-ptr-ctype-specializer type-info)))
			  (match-right var-name "::___staticDereferencedUnboundInstance")
			  (match-right var-name "::___staticDereferencedNilInstance")
			  (match-right var-name "::___staticClass")
			  (match-right var-name "::_unbound")
			  (match-right var-name "::_nil"))
		      (setf (gethash var-name ignore-globals) v))
		     ((unclassified-ctype-p type-info)
		      (setf (gethash var-name unclassified-globals) v))
		     (t (setf (gethash var-name necessary-globals) v)))))
	     globals)
    (values necessary-globals unclassified-globals ignore-globals)))


(defun has-smart-ptr-args (args)
  (let ((has-smart nil))
    (mapc #'(lambda (v)
	      (when (eq (car v) 'gctools:smart-ptr)
		(setq has-smart t)))
	  args)))

(defun filter-unrooted-locals (locals)
  (let ((unrooted nil))
    (maphash #'(lambda (k v)
		 (let* ((type-info (caddr v)))
		   (let* ((type (car type-info)))
		     (when (or 
			    (eq type 'gctools:stl-vector)
			    (eq type 'gctools:stl-set)
			    (eq type 'gctools:stl-map))
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



(defun run-rewrite-repr (&optional save)
  (gctools:ast-rewrite-repr *compile-commands-path*
			    nil
			    :save save)
)


(defun run-rewrite-nullary-methods (&optional save (start 0) end)
  (let ((ht (make-hash-table :test 'equal)))
    (format t "Starting run-rewrite-nullary-methods~%")
    (mapc #'(lambda (kv &aux (k (car kv)) (v (cadr kv)))
	      (format t "adding nullary function transformation from XXX->~A()  ==>  ~A(XXX)~%" k v)
	      (setf (gethash k ht) v))
	  '(
	    ( "genericFunctionP" "af_genericFunctionP" )
	    ( "endp" "af_endp" )
	    ( "llvm_sys_value_p" "af_llvm_sys_value_p")
	    ( "interpretedFunctionP" "af_interpretedFunctionP")
	    ( "metaClassP" "af_metaClassP")
	    ( "classP" "af_classP")
	    ( "integerP" "af_integerP")
	    ( "realP" "af_realP")
	    ( "streamP" "af_streamP")
	    ( "multipleValuesP" "af_multipleValuesP")
	    ( "consP" "af_consP")
	    ( "sourceCodeConsP" "af_sourceCodeConsP")
	    ( "vectorP" "af_vectorP")
	    ( "vectorObjectsP" "af_vectorObjectsP")
	    ( "lambda_list_handler_p" "af_lambda_list_handler_p")
	    ( "compiled_bodyP" "af_compiled_bodyP")
	    ( "symbolP" "af_symbolp")
	    ( "keywordP" "af_keywordP")
	    ( "standardObjectP" "af_standardObjectP")
	    ( "structureObjectP" "af_structureObjectP")
	    ( "executableP" "af_executableP")
	    ( "functionP" "af_functionP")
	    ( "compiledFunctionP" "af_compiledFunctionP")
	    ( "arrayP" "af_arrayP")
	    ( "arrayObjectsP" "af_arrayObjectsP")
	    ( "numberP" "af_numberP")
	    ( "floatP" "af_floatP")
	    ( "shortFloatP" "af_shortFloatP")
	    ( "singleFloatP" "af_singleFloatP")
	    ( "doubleFloatP" "af_doubleFloatP")
	    ( "longFloatP" "af_longFloatP")
	    ( "complexP" "af_complexP")
	    ( "rationalP" "af_rationalP")
	    ( "ratioP" "af_ratioP")
	    ( "pointerP" "af_pointerP")
	    ( "bitVectorP" "af_bitVectorP")
	    ( "fixnumP" "af_fixnumP")
	    ( "bignumP" "af_bignumP")
	    ( "stringP" "af_stringP")
	    ( "simpleStringP" "af_simpleStringP")
	    ( "strP" "af_strP")
	    ( "packageP" "af_packageP")
	    ( "booleanP" "af_booleanP")
	    ( "specialFormP" "af_specialFormP")
	    ( "hashTableP" "af_hashTableP")
	    ( "readtableP" "af_readtableP")
	    ( "characterP" "af_characterP")
	    ( "pathnameP" "af_pathnameP")
	    ( "simple_bit_vector_p" "af_simple_bit_vector_p")
	    ( "activation_frame_p" "af_activation_frame_p")
	    ( "single_dispatch_activation_frame_p" "af_single_dispatch_activation_frame_p")
	    ( "singleDispatchGenericFunctionP" "af_singleDispatchGenericFunctionP")
	    ( "externalObjectP" "af_externalObjectP")
	    ( "cdr"    "cCdr")
	    ( "cddr"   "cCddr")
	    ( "cdddr"  "cCdddr")
	    ( "cddddr" "cCddddr")
	    ( "ocar" "oCar")
	    ( "ocdr" "oCdr")
	    ( "ccar" "cCar")
	    ( "ccdr" "cCdr")
	    ( "ocaar" "oCaar")
	    ( "ocadr" "oCadr")
	    ( "ocdar" "oCdar")
	    ( "ocddr" "oCddr")
	    ( "ocaaar" "oCaaar")
	    ( "ocaadr" "oCaadr")
	    ( "ocadar" "oCadar")
	    ( "ocaddr" "oCaddr")
	    ( "ocdaar" "oCdaar")
	    ( "ocdadr" "oCdadr")
	    ( "ocddar" "oCddar")
	    ( "ocdddr" "oCdddr")
	    ( "ocaaaar" "oCaaaar")
	    ( "ocaaadr" "oCaaadr")
	    ( "ocaadar" "oCaadar")
	    ( "ocaaddr" "oCaaddr")
	    ( "ocadaar" "oCadaar")
	    ( "ocadadr" "oCadadr")
	    ( "ocaddar" "oCaddar")
	    ( "ocadddr" "oCadddr")
	    ( "ocdaaar" "oCdaaar")
	    ( "ocdaadr" "oCdaadr")
	    ( "ocdadar" "oCdadar")
	    ( "ocdaddr" "oCdaddr")
	    ( "ocddaar" "oCddaar")
	    ( "ocddadr" "oCddadr")
	    ( "ocdddar" "oCdddar")
	    ( "ocddddr" "oCddddr")
	    ( "ofirst"   "oFirst")
	    ( "osecond"  "oSecond")
	    ( "othird"   "oThird")
	    ( "ofourth"  "oFourth")
	    ( "ofifth"   "oFifth")
	    ( "osixth"   "oSixth")
	    ( "oseventh" "oSeventh")
	    ( "oeighth"  "oEighth")
	    ( "oninth"   "oNinth")
	    ( "otenth"   "oTenth")
	    ))
    (gctools:ast-rewrite-nullary-member-functions
     ht
     *compile-commands-path*
     nil
     :save save
     :start start :end end
    )))









;; -----------------------------------------------------------------
;;
;; Search for globals and locals

(defvar *results* nil)
(defvar *globals* nil)
(defvar *locals* nil)
(defvar *parameters* nil)
(defvar *necessary-globals* nil)
(defvar *unclassified-globals* nil)
(defvar *ignore-globals* nil)
(defun get-globals-locals (&key testing)
  (format t "Starting search of AST~%")
  (multiple-value-setq (*globals* *locals* *parameters*)
    (find-globals-locals-parameters :testing testing))
  (format t "Filtering out symbols and _nil/_unbound class variables~%")
  (multiple-value-setq (*necessary-globals* *unclassified-globals* *ignore-globals*)
    (filter-out-redundant-globals *globals*))
  (format t "Results in *necessary-globals*, *unclassified-globals*, *ignore-globals* and *locals*~%"))


;; Search for inheritance and class field info
;; Generate a header file that contains the body of the
;; MPS-scan and MPS-skip functions
;;
(defvar *output-filename* "../gctools/gcinfo.h")

(defvar *inheritance*)
(defvar *instance-variables*)

(defun identify-instance-variables ()
  "Searches the Clang AST and sets up class inheritance and instance variables of all lisp classes"
  (multiple-value-setq (*inheritance* *instance-variables*)
    (generate-gc-info)))

(defun generate-mps-functions () 
  (identify-instance-variables)
  (with-open-file (fout "../gctools/gcinfo.h" :direction :output :if-exists :supersede)
    (format fout "#if defined(GC_ENUM)~%")
    (generate-kind-enum *inheritance* :output-stream fout)
    (format fout "#endif // defined(GC_ENUM)~%")
    (format fout "#if defined(GC_KIND_SELECTORS)~%")
    (generate-kind-selectors *inheritance* :output-stream fout)
    (format fout "#endif // defined(GC_KIND_SELECTORS)~%")
    (format fout "#if defined(GC_KIND_NAME_MAP)~%")
    (generate-kind-name-map *inheritance* :output-stream fout)
    (format fout "#endif // defined(GC_KIND_NAME_MAP)~%")
    (format fout "#if defined(GC_SCAN_METHOD)~%")
    (build-mps-scan *inheritance* *instance-variables* :output-stream fout)
    (format fout "#endif // defined(GC_SCAN_METHOD)~%")
    (format fout "#if defined(GC_SKIP_METHOD)~%")
    (build-mps-skip *inheritance* *instance-variables* :output-stream fout)
    (format fout "#endif // defined(GC_SKIP_METHOD)~%")
    ))


(defvar *tooling-base-dir*
  #+target-os-linux "/home/meister/Development/cando/brcl/src/main"
  #+target-os-darwin "/Users/meister/Development/cando/brcl/src/main"
  "Directory where compile-commands database lives")
(defvar *compile-commands-path*
  (format nil "~a/compile_commands.json" *tooling-base-dir*)
  "Name of compile commands database eg: compile_commands.json")
(defvar *all-headers* (format nil "~a/allHeaders.cc" *tooling-base-dir*)
  "Name of the source cc file that includes all headers eg: allHeaders.cc")
(defvar *testing-files* (mapcar #'(lambda (x) (format nil "~a/~a" *tooling-base-dir* x))
				'("../core/lambdaListHandler.cc"))
				"List of file names for testing tooling")
(defun setup-tooling (&key dir compile-commands all-headers testing-files)
  (when dir (setq *tooling-base-dir* dir))
  (when compile-commands (setq *compile-commands* compile-commands))
  (when all-headers (setq *all-headers* all-headers))
  (when testing-files (setq *testing-files* testing-files)))

(defun info ()
  (format t "*tooling-base-dir* --> ~a~%" *tooling-base-dir*)
  (format t "*compile-commands-path* --> ~a~%" *compile-commands-path*)
  (format t "*all-headers* --> ~a~%" *all-headers*)
  (format t "*testing-files* --> ~a~%" *testing-files*)
)

;; setup the tooling with defaults

(info)
(format t "Available functions:~%")
(format t "    (setup-tooling &key (dir \"/Users/meister/Development/cando/brcl\") (compilation-database \"compile_commands.json\") (all-headers \"allHeaders.cc\") - Setup the tooling directories and files~%")
(format t "    (identify-instance-variables)~%")
(format t "    (generate-mps-functions)~%")
(format t "    (get-globals-locals)          - Identifies all global and local variables~%")
(format t "    (run-rewrite-repr)            - Converts x->__repr__() to _rep_(x)~%")
(format t "    (run-rewrite-nullary-methods) - Converts method functions x->YYY() to af_YYY(x)~%")
(format t "                                  - the YYY method/function names are specified within~%")
(format t "                                  - the run-rewrite-nullary-methods function~%")


