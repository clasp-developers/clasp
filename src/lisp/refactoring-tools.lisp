(progn
  (provide 'gc-builder)
  (require 'clang-tool)
  (use-package :ast-tooling)
  )

#|
(defmacro gclog (fmt &rest args)
  `(format t ,fmt ,@args))
|#

(defmacro gclog (fmt &rest args)
  nil)

(defparameter *testing* t)


(defun print-gc-info (&optional (inheritance *inheritance*) (instance-variables *instance-variables*))
  (format t "(progn~%")
  (format t "(export '(gctools::instance-variable gctools::inheritance gctools::not-yet-classified-template-specialization-type gctools::not-yet-classified gctools::smart-ptr gctools::weak-smart-ptr gctools::bases gctools::virtual-bases) :gctools)~%")
  (format t "(defparameter *inheritance* (make-hash-table :test #'equal))~%")
  (format t "(defparameter *instance-variables* (make-hash-table :test #'equal))~%")
  (maphash #'(lambda (k v) (format t "(setf (gethash \"~a\" *inheritance*) '~a)~%" k v)) inheritance)
  (maphash #'(lambda (k v) (format t "(setf (gethash \"~a\" *instance-variables*) '~a)~%" k v)) instance-variables)
  (format t ")~%")
  )


(defstruct gc-tool
  database-pathname
  initializers ;; thunks that initialize the named-results objects
  matchers-and-callbacks  ;; keep track of matchers and callbacks
  (named-results (make-hash-table :test #'eq))
  )

(defun gc-tool-add-matcher (tool matcher callback)
  "Keep track of matchers and callbacks so they don't go out of scope while the tool is alive"
  (push (cons matcher callback) (gc-tool-matchers-and-callbacks tool)))


(defun gc-tool-run (gc-tool &key test)
  (let* ((*testing* test)
         (dbfilename (let* ((json-database-pathname (gc-tool-database-pathname gc-tool))
                            (file (probe-file (gc-tool-database-pathname gc-tool))))
                       (unless file (error "No file with name ~A" json-database-pathname))
                       (namestring file)))
         (db (ast-tooling:jsoncompilation-database-load-from-file dbfilename))
         (filenames (if *testing*
                        (subseq (ast-tooling:get-all-files db) 0 *testing*)
                        (ast-tooling:get-all-files db)))
         (tool (let ((temp-tool (ast-tooling:new-clang-tool db filenames))
                     (syntax-only-adjuster (ast-tooling:make-clang-syntax-only-adjuster))
                     (strip-output-adjuster (ast-tooling:make-clang-strip-output-adjuster)))
                 (ast-tooling:clear-arguments-adjusters temp-tool)
                 (ast-tooling:append-arguments-adjuster temp-tool syntax-only-adjuster)
                 (ast-tooling:append-arguments-adjuster temp-tool strip-output-adjuster)
                 temp-tool))
         (match-finder (let ((mf (ast-tooling:new-match-finder)))
                         (dolist (mc (gc-tool-matchers-and-callbacks gc-tool))
                           (add-dynamic-matcher mf (car mc) (cdr mc)))
                         mf))
         (factory (new-frontend-action-factory match-finder)))
    (dolist (init (gc-tool-initializers gc-tool))
      (funcall init))
    (ast-tooling:clang-tool-run tool factory))
  gc-tool)





(defstruct gcobject-subclass
  "Represent an inheritance relationship between classes for Lisp objects"
  myclass
  bases
  vbases
  instance-variables
)


(defstruct instance-variable
  field-name
  ctype
  )



(defstruct ctype )

(defstruct (simple-ctype (:include ctype))
  description)

(defstruct (builtin-ctype (:include ctype))
  description)

(defstruct (pointer-ctype (:include ctype))
  pointee)

(defstruct (paren-ctype (:include ctype))
  inner)

(defstruct (function-proto-ctype (:include ctype))
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
    (when (eql (gc-template-argument-index arg) idx) (return arg))))


(defstruct (record-ctype (:include ctype))
  description)

(defstruct (pointer-to-record-ctype (:include ctype))
  description)

(defstruct (stl-container (:include container)))

(defstruct (gcholder (:include container)))

(defstruct (gcobject-derived (:include ctype))
  description)

(defstruct gc-template-argument 
  index
  ctype)









(defmethod is-smart-p ((x ctype)) nil)
(defmethod is-smart-p ((x smart-ptr-ctype)) t)

(defmethod is-smart-p ((x gc-template-argument))
  (is-smart-p (gc-template-argument-ctype x)))


(defmethod macro-name ((x gcholder))
  (format nil "~a_FIX" (container-name x)))

(defmethod macro-name ((x stl-container))
  (handler-case
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
                (type-info (gc-template-argument-ctype arg)))
           (format t "IN macro-name ::: type-info --> ~a    (unclassified-ctype-description type-info) --> ~a~%" type-info (unclassified-ctype-description type-info) )
           (cond
             ((is-smart-p arg) "STLVECTOR_SMART_FIX")
             ((equal (unclassified-ctype-description type-info) "struct core::RequiredArgument") "STL_VECTOR_REQUIRED_ARGUMENT_FIX")
             ((equal (unclassified-ctype-description type-info) "struct core::OptionalArgument") "STL_VECTOR_OPTIONAL_ARGUMENT_FIX")
             ((equal (unclassified-ctype-description type-info) "struct core::KeywordArgument")  "STL_VECTOR_KEYWORD_ARGUMENT_FIX")
             ((equal (unclassified-ctype-description type-info) "struct core::AuxArgument")      "STL_VECTOR_AUX_ARGUMENT_FIX")
             (t "IGNORE")))))
    (simple-type-error (err)
      (format t "UNHANDLED_TYPE_ERROR(~a)~%" x)))
  )

(defmethod macro-name ((x smart-ptr-ctype))
  "SMART_PTR_FIX")

(defmethod macro-name ((x weak-smart-ptr-ctype))
  "WEAK_SMART_PTR_FIX")

(defmethod macro-name ((x unclassified-ctype))
  "IGNORE")

(defmethod macro-name ((x ctype))
  (substitute #\_ #\- (format nil "HANDLE_CTYPE_~a" (type-of x))))

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





(defun one-class-instance-variables (result-ht class)
  (let ((vars (gcobject-subclass-instance-variables class)))
    (mapc #'(lambda (x)
	      (when (gethash (instance-variable-field-name x) result-ht)
                (warn (format nil "Instance variable already exists[~a] type[~a]" (instance-variable-field-name x) (instance-variable-ctype x))))
	      (setf (gethash (instance-variable-field-name x) result-ht) x))
	  vars)))

(defun ancestor-instance-variables-recursive (result-ht classid classes)
  "Add the instance variables for classid and then do the same for its base classes"
  (let ((class (gethash classid classes)))
    (gclog "Looked up classid[~a] -> ~a~%" classid class)
    (let* ((bases (if class
                      (gcobject-subclass-bases class)
                      nil)))
      (one-class-instance-variables result-ht class)
      (mapc #'(lambda (x)
                (gclog "Looking at base: ~a~%" x)
                (ancestor-instance-variables-recursive result-ht x classes)) bases))))


(defun gather-instance-variables (classid classes)
  "Gather all of the instance variables for a class (including the instance variables of base classes)"
  (let ((gathered (make-hash-table :test #'equal)))
    (ancestor-instance-variables-recursive gathered classid classes)
    gathered))




(defun ctype-name (classid)
  classid)


(defun class-enum-name (classid)
  (let* ((ctype-name (ctype-name classid)))
    (if (match-right classid "::Wrapper")
        (format nil "reg::registered_class< typename ~a >::id" classid)
        (format nil "reg::registered_class< ~a >::id" classid))))



(defun code-for-instance-var (output-stream ptr-name instance-var type-info)
  (let* ((type (type-of type-info))
	 (macro-name (macro-name type-info)))
    (when macro-name
      (format output-stream "    ~A(~A->~A); /* ~a */~%" macro-name ptr-name instance-var type-info)
      )))


#|
(defun generate-kind-enum (inheritance &key (output-stream t))
  (format output-stream "enum { KIND_null, ~%")
  (let ((num 0))
    (maphash #'(lambda (classid value)
		 (setq num (1+ num))
		 (let ((enum-name (class-enum-name classid)))
		   (format output-stream "~A, " enum-name)))
	     inheritance)
    (format output-stream "KIND_max /* idx=~A */, KIND_fwd, KIND_fwd2, KIND_pad1, KIND_pad }~%" num)))
|#

#|
(defun generate-kind-selectors (inheritance &key (output-stream t))
  (let ((num 0))
    (maphash #'(lambda (classid value)
		 (setq num (1+ num))
		 (let ((enum-name (class-enum-name classid)))
		   (format output-stream "template <> GcKindEnum GcKindSelector<~A>() { return ~A; };~%" (ctype-name classid) enum-name)))
	     inheritance)
    ))
|#



(defun generate-kind-name-map (inheritance &key (output-stream t))
  (let ((num 0))
    (maphash #'(lambda (classid value)
		 (setq num (1+ num))
		 (let ((enum-name (class-enum-name classid)))
		   (format output-stream "   case ~A: return \"~A\";~%" enum-name enum-name)))
	     inheritance)
    ))


(defconstant +ptr-name+ "obj")
(defun build-mps-scan (inheritance &key (output-stream t))
  (format t "Entered build-mps-scan~%")
  (maphash #'(lambda (classid value)
	       (gclog "build-mps-scan -> inheritance classid[~a]  value[~a]~%" classid value)
	       (let ((all-instance-variables (gather-instance-variables classid  inheritance)))
		 (format output-stream "case ~a: {~%" (class-enum-name classid))
		 (format output-stream "    ~A* ~A = reinterpret_cast<~A*>(obj_ptr);~%" (ctype-name classid) +ptr-name+ (ctype-name classid))
		 (format output-stream "    DEBUG_SCAN_OBJECT(~A);~%" +ptr-name+)
		 (maphash #'(lambda (k v) (code-for-instance-var output-stream +ptr-name+ k (instance-variable-ctype v))) all-instance-variables)
		 (format output-stream "    base = (char*)base + ALIGN(SIZEOF_WITH_HEADER(~A));~%" (ctype-name classid))
		 (format output-stream "} break;~%")
		 ))
	   inheritance ))

(defun build-mps-skip (inheritance &key (output-stream t))
  (maphash #'(lambda (classid value)
	       (format output-stream "case ~a: {~%" (class-enum-name classid))
	       (format output-stream "    base = (char*)base + ALIGN(SIZEOF_WITH_HEADER(~A));~%" (ctype-name classid))
	       (format output-stream "} break;~%")
	       )
	   inheritance
	   ))

;;(print-gc-info *inheritance* *instance-variables*)

#|
(defun generate-gc-info ()
  (format t "Starting ast-search~%")
  (let ((results (ast-tooling:ast-search '(ast-tooling:garbage-collector-scanner) *compile-commands-path* (list *all-headers*))))
    (destructuring-bind (inheritance instance-variables)
	(cdr (assoc 'ast-tooling:garbage-collector-scanner results))
      (values inheritance instance-variables))))
|#



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


(defun generate-mps-functions (tool-pack) 
  (let* ((results (gc-tool-named-results tool-pack))
         (inheritance (gethash :gcobject-subclasses results)))
    (with-open-file (fout "app:Contents;Resources;buildDatabases;brcl_gc.cc" :direction :output :if-exists :supersede)
      #|
      (format fout "#if defined(GC_ENUM)~%")
      (generate-kind-enum inheritance :output-stream fout)
      (format fout "#endif // defined(GC_ENUM)~%")
      (format fout "#if defined(GC_KIND_SELECTORS)~%")
      (generate-kind-selectors inheritance :output-stream fout)
      (format fout "#endif // defined(GC_KIND_SELECTORS)~%")
      |#
      (format fout "#if defined(GC_KIND_NAME_MAP)~%")
      (generate-kind-name-map inheritance :output-stream fout)
      (format fout "#endif // defined(GC_KIND_NAME_MAP)~%")
      (format fout "#if defined(GC_SCAN_METHOD)~%")
      (build-mps-scan inheritance :output-stream fout)
      (format fout "#endif // defined(GC_SCAN_METHOD)~%")
      (format fout "#if defined(GC_SKIP_METHOD)~%")
      (build-mps-skip inheritance  :output-stream fout)
      (format fout "#endif // defined(GC_SKIP_METHOD)~%")
      ))
  )










 

;;
;; Extract all header files into one .cc file
;;




(defun syntax-only-arguments (args-vec)
  (let* ((largs (map 'list #'identity args-vec))
         (new-args (list "-fsyntax-only" (car largs)))
         (clang-resource-pathname (make-pathname :host "app"
                                                 :directory (list :absolute

                                                                  "Contents"
                                                                  "Resources"
                                                                  "externals"
                                                                  "release"
                                                                  "lib"
                                                                  "clang"
                                                                  (ast-tooling:clang-version-string))))
         (resource-dir-pathname (probe-file clang-resource-pathname))
         )
    (assert resource-dir-pathname nil
            "The resource-dir-pathname ~S for clang does not resolve to a directory"
            clang-resource-pathname)
    ;;    (push "-Xclang" new-args) ;; pass the previous (next arg when reversed)
    (push (format nil "-resource-dir=~A" (namestring resource-dir-pathname)) new-args)
    (push "-Wno-unused-function" new-args)
    (pop largs)   ; pop the clang++
    (do () ((null largs) (nreverse new-args))
      (cond
        ((string= (car largs) "-o")
         (pop largs) (pop largs)) ;; pop the -o and arg
        ((string= (substr (car largs) 0 3) "-o")
         (pop largs)) ;; pop just the -o
        ((string= (car largs) "-fcolor-diagnostics")
         (pop largs))
        ((string= (car largs) "-fdiagnostics-color")
         (pop largs))
        (t (push (pop largs) new-args)))))
  )






(defun classify-template-args (in-tsty)
  (let (args)
    (do* ((i (1- (cast:get-num-args in-tsty)) (1- i)))
         ((< i 0))
      (let* ((arg (cast:get-arg in-tsty i))
             (qtarg (cast:get-as-type arg))
             (tsty-new (cast:get-type-ptr-or-null qtarg))
             classified)
        (gclog "classify-template-arg: ~a~%" (cast:get-as-string qtarg))
        (when (eq (type-of tsty-new) 'cast:template-specialization-type)
          (setf classified (classify-template-specialization-type #|qtarg|# tsty-new))
          (unless classified
            (when (is-derived-from-gcobject qtarg)
              (setf classified (make-gcobject-derived :description (cast:get-as-string qtarg))))))
        (gclog "classified = ~a  (type-of tsty-new)-> ~a~%" classified (type-of tsty-new))
        (unless classified
          (setf classified (classify-ctype tsty-new)))
        #|          (if (eq (type-of tsty-new) 'cast:record-type)
        (setf classified (make-record-ctype :description (cast:get-as-string qtarg)))
        (setf classified (make-unclassified-ctype :description (cast:get-as-string qtarg)))))
    |#
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



(defun classify-template-specialization-type (tsty)
  (let* ((decl (clang-ast:get-as-cxxrecord-decl tsty)))
    (if decl ;; (sys:validp decl)
        (let ((decl-name (clang-ast:get-name decl)))
          (gclog "            Checking out what's in decl: ~a~%" decl-name)
          (cond
            ((null decl) nil)
            ((string= decl-name "smart_ptr")
             (assert (eql (cast:get-num-args tsty) 1) nil "Must have 1 argument")
             (let* ((arg (cast:get-arg tsty 0))
                    (qtarg (cast:get-as-type arg)))
               (gclog "          Found a smart_ptr~%")
               (make-smart-ptr-ctype :specializer (cast:get-as-string qtarg))))
            ((string= decl-name "weak_smart_ptr")
             (assert (eql (cast:get-num-args tsty) 1) nil "Must have 1 argument")
             (let* ((arg (cast:get-arg tsty 0))
                    (qtarg (cast:get-as-type arg)))
               (make-weak-smart-ptr-ctype :specializer (cast:get-as-string qtarg))))
            ((or (string= decl-name "vector")
                 (string= decl-name "set")
                 (string= decl-name "map")
                 (string= decl-name "queue")
                 (string= decl-name "stack")
                 (string= decl-name "multimap"))
             (make-stl-container :name (intern (string-upcase (format nil "STL~a" decl-name)) :cl-user)
                                 :arguments (classify-template-args tsty)))
            ((inherits-from-gcholder decl)
             (assert (eql (cast:get-num-args tsty) 1) nil "~a requires 1 arg only but was given ~a args" decl-name (cast-get-num-args tsty))
             (make-gcholder :name (intern (string-upcase (format nil "GCHOLDER_~a" decl-name)))
                            :arguments (classify-template-args tsty)))
            (t
             (make-unclassified-ctype :description (cast:get-as-string (cast::desugar tsty)))))
          )
        nil)))


(define-condition unsupported-type (error)
  ((type :initarg :type :accessor unsupported-type)))


(defun classify-ctype (type)
  (gclog "classify-ctype   type: ~a~%" type)
  (cond
    ((eq (type-of type) 'cast:record-type)
     (make-record-ctype :description (cast:get-name (cast:get-decl type))))
    ((eq (type-of type) 'cast:template-specialization-type)
     (classify-template-specialization-type type))
    ((eq (type-of type) 'cast:typedef-type)
     (classify-ctype (cast:get-type-ptr-or-null (cast:desugar type))))
;;     (classify-template-specialization-type (cast:get-as-template-specialization-type type)))
    ((eq (type-of type) 'cast:elaborated-type)
     (let* ((named-qual-type (cast:get-named-type type))
            (real-type (cast:get-type-ptr-or-null named-qual-type)))
       (classify-ctype real-type)))
    ((eq (type-of type) 'cast:builtin-type)
     (make-builtin-ctype :description (cast:get-as-string (cast::desugar type))))
    ((eq (type-of type) 'cast:pointer-type)
     (make-pointer-ctype :pointee (classify-ctype (cast:get-type-ptr-or-null (cast:get-pointee-type type)))))
    ((eq (type-of type) 'cast:paren-type)
     (make-paren-ctype :inner (classify-ctype (cast:get-type-ptr-or-null (cast:get-inner-type type)))))
    ((eq (type-of type) 'cast:function-proto-type)
     (make-function-proto-ctype :description "FunctionProtoType - fill in more info if necessary"))
    (t (make-unclassified-ctype :description (type-of type)))))


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







(defparameter *inheritance-matcher*
  '(:record-decl
    (:is-definition)
    (:is-same-or-derived-from
     (:record-decl
      (:matches-name "GCObject")
      )
     )
    ))


(defparameter *instance-var-matcher*
  (compile-matcher
   '(:record-decl
     (:is-definition)
     (:for-each-descendant
      (:field-decl
       (:bind :field (:field-decl))
       )
      )
     )
   )
  )




(defun setup-finder-gc-info (tool)
  "Setup the TOOL (gc-tool) to look for instance variables that we want to garbage collect
and the inheritance hierarchy that the garbage collector will need"
  (symbol-macrolet ((class-results (gethash :gcobject-subclasses (gc-tool-named-results tool)))
                    )
    ;;    (push #'(lambda () (setf instance-var-results (make-hash-table :test #'equal))) (gc-tool-initializers tool))
    #||    (gc-tool-add-matcher
    tool 
    (compile-matcher `(:bind :whole ,*instance-var-matcher*))
    (make-instance
    'code-match-callback
    :code #'(lambda
    (&aux
    (whole-node (mtag-node :whole))
    (qualty (clang-ast:get-type whole-node))
    (class-node (mtag-node :class))
    (class-name (annotated-class-name class-node))
    (qualty-as-template-specialization-type
    (clang-ast:get-as-template-specialization-type (cast:get-type-ptr-or-null qualty)))
    (classified
    (progn
    (gclog "Looking at: ~a   typeof ~a~%" (clang-ast:get-as-string qualty)  (type-of (clang-ast:get-type-ptr-or-null qualty)))
    (cond
    (qualty-as-template-specialization-type
    (gclog "    It's a TemplateSpecializationType~%")
    (classify-template-specialization-type #|qualty|# qualty-as-template-specialization-type))
    ((string= (clang-ast:get-as-string qualty) "struct core::RestArgument")
    (gclog "     It's a RestArgument~%")
    (make-rest-argument :description (clang-ast:get-as-string qualty)))
    (t
    (gclog "     It's Unclassified~%")
    (make-unclassified-ctype :description (clang-ast:get-as-string qualty)))))
    )
    )
    (let ((instance-var (make-instance-variable :field-name (cast:get-name whole-node)
    :class-name class-name
    :ctype classified)))
    (pushnew instance-var (gethash class-name instance-var-results)
    :key #'instance-variable-field-name
    :test #'(lambda (x y) (string= x y) ))))))
  ||#
    ;; Initialize the inheritance 
  (push #'(lambda () (setf class-results (make-hash-table :test #'equal))) (gc-tool-initializers tool))
  (gc-tool-add-matcher
   tool
   (compile-matcher `(:bind :whole ,*inheritance-matcher*))
   (make-instance
    'code-match-callback
    :code #'(lambda ()
              (let* ((class-node (mtag-node :whole))
                     (class-name (annotated-class-name class-node)))
                (unless (gethash class-name class-results)
                  (gclog "Adding class name: ~a~%" class-name)
                  (let* ((bases (let (temp-bases)
                                  (ext:map-c++-iterator #'(lambda (x)
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
                                   (ext:map-c++-iterator #'(lambda (x)
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
                         (gcobject-subclass (make-gcobject-subclass :myclass class-name
                                                                    :bases bases
                                                                    :vbases vbases)))
                    ;; (format t "Adding class-results for ~a~%" class-name) ;
                    (setf (gethash class-name class-results) gcobject-subclass)
                    (sub-match-run
                     *instance-var-matcher*
                     class-node
                     (lambda ()
                       (let* ((whole-node (mtag-node :field))
                              (qualty (clang-ast:get-type whole-node))
                              (qualty-as-template-specialization-type
                               (clang-ast:get-as-template-specialization-type (cast:get-type-ptr-or-null qualty)))
                              (classified (progn
                                            (gclog "  Looking at field: ~a type: ~a   typeof ~a~%"
                                                   (cast:get-name whole-node)
                                                   (clang-ast:get-as-string qualty)
                                                   (type-of (clang-ast:get-type-ptr-or-null qualty)))
                                            (cond
                                              (qualty-as-template-specialization-type
                                               (gclog "      It's a TemplateSpecializationType~%")
                                               (classify-template-specialization-type #|qualty|# qualty-as-template-specialization-type))
                                              ((string= (clang-ast:get-as-string qualty) "struct core::RestArgument")
                                               (gclog "       It's a RestArgument~%")
                                               (make-rest-argument :description (clang-ast:get-as-string qualty)))
                                              (t
                                               (gclog "       It's Unclassified~%")
                                               (make-unclassified-ctype :description (clang-ast:get-as-string qualty))))))
                              (instance-var (make-instance-variable :field-name (cast:get-name whole-node)
                                                                    :ctype classified)))
                         (pushnew instance-var (gcobject-subclass-instance-variables gcobject-subclass)
                                  :key #'instance-variable-field-name
                                  :test #'(lambda (x y) (string= x y) )))))
                    ))))))))














(defvar *tooling-base-dir*
  #+target-os-linux "/home/meister/Development/cando/brcl/src/main"
  #+target-os-freebsd "/home/meister/Development/cando/brcl/src/main"
  #+target-os-darwin "/Users/meister/Development/cando/brcl/src/main"
  "Directory where compile-commands database lives")
(defvar *compile-commands-path*
  (format nil "~a/compile_commands.json" *tooling-base-dir*)
  "Name of compile commands database eg: compile_commands.json")
(defvar *testing-files* (mapcar #'(lambda (x) (format nil "~a/~a" *tooling-base-dir* x))
				'("../core/lambdaListHandler.cc"))
  "List of file names for testing tooling")
(defun setup-tooling (&key dir compile-commands all-headers testing-files)
  (when dir (setq *tooling-base-dir* dir))
  (when compile-commands (setq *compile-commands* compile-commands))
  (when testing-files (setq *testing-files* testing-files)))

(defun info ()
  (format t "*tooling-base-dir* --> ~a~%" *tooling-base-dir*)
  (format t "*compile-commands-path* --> ~a~%" *compile-commands-path*)
  (format t "*testing-files* --> ~a~%" *testing-files*)
  )

;; setup the tooling with defaults      ;


(defun help-gc-builder () 
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
  )





;; ----------------------------------------------------------------------
;;
;; Search for house-keeping classes that contain smart-pointers and
;; set up garbage collector root-scanning functions for them


























;; ----------------------------------------------------------------------
;;
;; Setup the *gc-tool* that will run over all of the source code and run
;; several matchers that scrape the C++ AST for info requires to build
;; garbage collection scanners.



(defparameter *gc-tool* (make-gc-tool :database-pathname #P"app:Contents;Resources;buildDatabases;brcl_compile_commands.json"))
(setup-finder-gc-info *gc-tool*)

;;(setup-houskeeping-class-matchers *gc-tool*)




(defun do-all (&key test)
  (gc-tool-run *gc-tool* :test test)
  (generate-mps-functions *gc-tool*)
  )
