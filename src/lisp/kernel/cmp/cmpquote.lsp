

(in-package :cmp)

(defvar *load-time-value-initialization-function*)
#||(defvar *load-time-value-invocation-history-frame*)||#

(defvar *load-time-value-result* nil
  "Temporary storage for results of evaluating top-level-forms")

(defvar *load-time-value-coalesce* nil)
(defvar *load-time-initializer-environment* nil)


(defvar *run-time-literal-holder-name* "<default>")
(defvar *run-time-literal-holder* (load-time-value-array *run-time-literal-holder-name* 1024)
  "Stores the literal values for the default module that COMPILE compiles functions into")
(core::set-run-time-values-vector *run-time-literal-holder-name*)


(defvar *run-time-value-nil-index* (data-vector-push-extend *run-time-literal-holder* nil 16))
(defvar *run-time-value-t-index* (data-vector-push-extend *run-time-literal-holder* t 16))
(defvar *run-time-literals-external-name* "globalRunTimeValues")


(defvar +run-and-load-time-value-holder-global-var-type+ +ltv*+) ;; Was +ltvsp*+

(defvar *load-time-value-holder-global-var* nil
  "Store the current load-time-value data structure for COMPILE-FILE")

(defvar *run-time-value-holder-global-var* nil
  "All load-time-values and quoted values are stored in this array accessed with an integer index"
  )


(defvar *next-load-time-value-index*  nil
  "Each load-time-value and quoted value get's assigned a unique integer index
and the next one is here")


(defvar *generate-compile-file-load-time-values* nil
  "This variable controls whether literals are compiled into the
load-time-value manager (true - in COMPILE-FILE) or not (false - in COMPILE)."
)





(defun get-next-available-ltv-entry (&optional value)
  "Return the next ltv-index. If this is being invoked from COMPILE then
the value is put into *default-load-time-value-vector* and its index is returned"
  (let ((ltv-idx *next-load-time-value-index*))
    (incf *next-load-time-value-index*)
    ltv-idx))
	



(defmacro with-next-load-time-value ((ltv-ref obj env) &rest creator)
  "An ltv-index for _obj_ will be created and the maker is codegen'd into the load-time-value function.
Return the ltv index of the value."
  (let ((index-gs (gensym "index")))
    ;; Always create a new load-time-value
    `(let ((,index-gs (get-next-available-ltv-entry)))
       (with-irbuilder (*load-time-initializer-environment* *irbuilder-ltv-function-body*)
	 ;;	 (core::hash-table-setf-gethash ,coalesce-hash-table ,key-gs ,index-gs)
	 (irc-low-level-trace)
	 (let ((,ltv-ref (irc-intrinsic "loadTimeValueReference" *load-time-value-holder-global-var* (jit-constant-i32 ,index-gs))))
	   (with-landing-pad (irc-get-cleanup-landing-pad-block ,env)
	     ,@creator))
	 )
       ,index-gs)))






(defmacro with-coalesce-load-time-value ((ltv-ref result obj env)
					 &key coalesce-hash-table maker
					   (push-and-get-reference-fn-name "loadTimeValueReference")
					   (copy-value-fn-name "copyLoadTimeValue")
					   (get-next-available-entry-index-fn-name 'get-next-available-ltv-entry))
  "An index for _obj_ will be looked up in the coalesce-hash-table
- if not found one is created and the maker is codegen'd into the load-time-value function.
Finally, if _result_ is not nil then the load-time-value index and *load-time-value-holder-global-var* will be used to
codegen a lookup for the value at runtime.
The objument _result_ can be passed as nil - this is for setting up special load-time-values like nil and t.
Return the ltv index of the value."
  (let ((index-gs (gensym "index"))
	(key-gs (gensym "key")))
    `(let ((,index-gs nil)
	   (,key-gs ,obj))
       (setq ,index-gs (gethash ,key-gs ,coalesce-hash-table))
       ;; If there is no index then create one
       (cmp-log "with-coalesce-load-time-value index within coalesce-hash-table: %s\n" ,index-gs)
       (if (null ,index-gs)
	   (with-irbuilder (*load-time-initializer-environment* *irbuilder-ltv-function-body*)
	     (setq ,index-gs (,get-next-available-entry-index-fn-name))
	     (cmp-log "new index: %s\n" ,index-gs)
	     (core::hash-table-setf-gethash ,coalesce-hash-table ,key-gs ,index-gs)
	     (irc-low-level-trace)
	     (let ((,ltv-ref (irc-intrinsic ,push-and-get-reference-fn-name  *load-time-value-holder-global-var* (jit-constant-i32 ,index-gs))))
	       (with-landing-pad (irc-get-cleanup-landing-pad-block *load-time-initializer-environment*) ;(irc-get-cleanup-landing-pad-block ,env)
		 (cmp-log "About to generate code for load-time-value maker: %s\n" ',maker) 
;;		 (break "Where will codegeneration go?")
		 ,maker))
	     ;; insert block may have changed in maker
	     ))
       (when ,result
	 (cmp-log "with-coalesce-load-time-value - setting up copy-value with function: %s\n" ,copy-value-fn-name)
	 (irc-intrinsic ,copy-value-fn-name ,result *load-time-value-holder-global-var* (jit-constant-i32 ,index-gs)))
       ,index-gs
       )))
	     


(defmacro with-initialize-load-time-value ((ltv-ref ltv-idx env) &rest initializer)
  `(with-irbuilder (*load-time-initializer-environment* *irbuilder-ltv-function-body*)
     ;;	 (core::hash-table-setf-gethash ,coalesce-hash-table ,key-gs ,index-gs)
     (let ((,ltv-ref (irc-intrinsic "loadTimeValueReference" *load-time-value-holder-global-var* (jit-constant-i32 ,ltv-idx))))
       (with-landing-pad (irc-get-cleanup-landing-pad-block ,env)
	 ,@initializer))
     ;; insert block may have changed in initializer
     ))
	     

(defmacro with-walk-structure ((obj env) &key maker walker)
  (let ((idx-gs (gensym "idx"))
	(foundp-gs (gensym "foundp")))
    `(multiple-value-bind (,idx-gs ,foundp-gs)
	 (gethash ,obj *node-table*)
       (if ,foundp-gs
	   ,idx-gs
	   (progn
	     (setq ,idx-gs ,maker)
	     (core::hash-table-setf-gethash *node-table* ,obj ,idx-gs)
	     ,walker
	     ,idx-gs)))))
	     






(defparameter *node-table* (make-hash-table :test #'eq))


(defun walk-structure-simple (cur env)
  (with-walk-structure (cur env)
    :maker (codegen-literal nil cur env)))

			   

(defun walk-structure-cons (cur env)
  "If the CONS cur is not in the *node-table* then create
an ltv for it, put it in the *node-table*
and walk the car and cdr"
  (with-walk-structure (cur env)
    :maker (make-ltv-cons cur env)
    :walker (progn
	      (walk-structure (car cur) env)
	      (walk-structure (cdr cur) env))))

#||
;; Old way allowed source-code-cons
(defun make-ltv-cons (val env)
  (if (source-code-cons-p val)
      (with-next-load-time-value (ltv-ref val env)
	(let* ((source-file-info (source-file-info val))
	       (source-path-name (if source-file-info
				     (source-file-info-path-name source-file-info)
				     "-no-file-")))
	  (irc-intrinsic "ltv_makeSourceCodeCons"
		    ltv-ref
		    (jit-constant-unique-string-ptr source-path-name)
		    (jit-constant-i32 (source-file-info-lineno val))
		    (jit-constant-i32 (source-file-info-column val)))))
      (with-next-load-time-value (ltv-ref val env)
	(irc-intrinsic "ltv_makeCons" ltv-ref))))
||#


(defun make-ltv-cons (val env)
  (with-next-load-time-value (ltv-ref val env)
    (irc-intrinsic "ltv_makeCons" ltv-ref)))


(defun initialize-ltv-cons (obj ltv-idx env)
  (with-initialize-load-time-value (ltv-ref ltv-idx env)
    (let ((car-ref (irc-intrinsic "loadTimeValueReference" *load-time-value-holder-global-var*
			     (jit-constant-i32 (gethash (car obj) *node-table*))))
	  (cdr-ref (irc-intrinsic "loadTimeValueReference" *load-time-value-holder-global-var*
			     (jit-constant-i32 (gethash (cdr obj) *node-table*)))))
      (irc-intrinsic "rplaca" ltv-ref car-ref)
      (irc-intrinsic "rplacd" ltv-ref cdr-ref))))





(defun walk-structure-array-objects (obj env)
  (with-walk-structure (obj env)
    :maker (make-ltv-array-objects obj env)
    :walker (let ((total-size (if (array-has-fill-pointer-p obj)
				  (length obj)
				  (array-total-size obj))))
	      (dotimes (idx total-size)
		(walk-structure (row-major-aref obj idx) env)))))

(defun make-ltv-array-objects (val env)
  (let ((array-element-type (irc-alloca-tsp *load-time-initializer-environment*
					    :irbuilder *irbuilder-ltv-function-alloca*
					    :label "array-element-type")))
    (with-irbuilder (*load-time-initializer-environment* *irbuilder-ltv-function-body*)
      (codegen-literal array-element-type (array-element-type val) *load-time-initializer-environment*))
    (with-next-load-time-value (ltv-ref val env)
      (irc-intrinsic "ltv_makeArrayObjects" ltv-ref
		array-element-type
		(jit-constant-i32 (array-rank val))
		(jit-constant-i32-vector-ptr (array-dimensions val))))))

(defun initialize-ltv-array-objects (obj ltv-idx env)
  (let* ((total-size (if (array-has-fill-pointer-p obj)
			 (length obj)
			 (array-total-size obj)))
	 (ltv-indices (core::make-vector t total-size nil nil nil nil nil nil)))
    (dotimes (i total-size)
      (setf-svref ltv-indices i (gethash (row-major-aref obj i) *node-table*)))
    (with-initialize-load-time-value (ltv-ref ltv-idx env)
      (irc-intrinsic "ltv_initializeArrayObjectsRowMajorArefOrder"
		ltv-ref
		*load-time-value-holder-global-var*
		(jit-constant-i32-vector-ptr ltv-indices)))))





(defun walk-structure-hash-table (obj env)
  (with-walk-structure (obj env)
    :maker (make-ltv-hash-table obj env)
    :walker (maphash #'(lambda (key val)
			 (walk-structure key env)
			 (walk-structure val env))
		     obj)))

(defun make-ltv-hash-table (val env)
  (let ((ht-test (irc-alloca-tsp *load-time-initializer-environment*
				 :irbuilder *irbuilder-ltv-function-alloca*
				 :label "hash-table-test")))
    (with-irbuilder (*load-time-initializer-environment* *irbuilder-ltv-function-body*)
      (codegen-literal ht-test (hash-table-test val) *load-time-initializer-environment*))
    (with-next-load-time-value (ltv-ref val env)
      (irc-intrinsic "ltv_makeHashTable" ltv-ref ht-test))))

(defun initialize-ltv-hash-table (obj ltv-idx env)
  (let* ((num-entries (hash-table-count obj))
	 (ltv-key-value-indices (core::make-vector t (* 2 num-entries) nil nil nil nil nil nil)))
    ;; Key/value indices are stored as adjacent pairs in ltv-key-value-indices
    (let ((key-idx 0))
      (maphash #'(lambda (key val)
		   (setf-svref ltv-key-value-indices key-idx (gethash key *node-table*))
		   (setf-svref ltv-key-value-indices (1+ key-idx) (gethash val *node-table*))
		   (setq key-idx (+ 2 key-idx)))
	       obj))
    (with-initialize-load-time-value (ltv-ref ltv-idx env)
      (irc-intrinsic "ltv_initializeHashTable" ltv-ref
		(jit-constant-i32 num-entries)
		*load-time-value-holder-global-var*
		(jit-constant-i32-vector-ptr ltv-key-value-indices)))))



(defun walk-structure (cur env)
  (cond
    ((consp cur)   (walk-structure-cons cur env))
    ((stringp cur) (walk-structure-simple cur env))
    ((vectorp cur) (walk-structure-array-objects cur env))
    ((arrayp cur)  (walk-structure-array-objects cur env))
    ((hash-table-p cur) (walk-structure-hash-table cur env))
    (t (walk-structure-simple cur env))))









(defun initialize-ltv-nodes (env)
  (maphash #'(lambda (key val)
	       (cond
		 ((consp key)   (initialize-ltv-cons key val env))
		 ((stringp key) nil)
		 ((vectorp key)  (initialize-ltv-array-objects key val env))
		 ((arrayp key)  (initialize-ltv-array-objects key val env))
		 ((hash-table-p key) (initialize-ltv-hash-table key val env))
		 (t nil)))
	   *node-table*))


(defun codegen-ltv/container (result obj env)
  (setq *node-table* (make-hash-table :test #'eq))
  (let ((ltv-idx (walk-structure obj env)))
    (initialize-ltv-nodes env)
    (when result
      (irc-intrinsic "getLoadTimeValue"
		result
		*load-time-value-holder-global-var*
		(jit-constant-i32 ltv-idx)))
    ltv-idx))




  


(defvar *fixnum-coalesce* nil 
  "Store a hash-table-eql of fixnums to indices")

(defun codegen-ltv-fixnum (result obj env)
  (with-coalesce-load-time-value (ltv-ref result obj env)
    :coalesce-hash-table *fixnum-coalesce*
    :maker (irc-intrinsic "makeFixnum" ltv-ref (jit-constant-i32 obj))))




(defvar *bignum-coalesce* nil)
(defun codegen-ltv-bignum (result obj env)
  (with-coalesce-load-time-value (ltv-temp result obj env)
    :coalesce-hash-table *bignum-coalesce*
    :maker (let ((string-arg (jit-make-global-string-ptr (bformat nil "%d" obj))))
	     (irc-intrinsic "makeBignum" ltv-temp string-arg))))


(defvar *symbol-coalesce* nil)
(defun codegen-ltv/symbol (result symbol env)
  (with-coalesce-load-time-value (ltv-temp result symbol env)
    :coalesce-hash-table *symbol-coalesce*
    :maker (let* ((sn (symbol-name symbol))
		  (sym-pkg (symbol-package symbol))
		  (sn-gv (llvm-sys:get-or-create-uniqued-string-global-variable
			  *the-module* sn
			  (bformat nil ":::symbol-name-%s" sn)))
		  (sn-value-ptr (llvm-sys:create-in-bounds-gep
				 *irbuilder* sn-gv (list (jit-constant-i32 0) (jit-constant-i32 0)) "sn")))
	     (if sym-pkg
		 (let* ((pn (package-name (symbol-package symbol)))
			(pn-gv (llvm-sys:get-or-create-uniqued-string-global-variable
				*the-module* pn (bformat nil ":::package-name-%s" pn)))
			(pn-value-ptr (llvm-sys:create-in-bounds-gep
				       *irbuilder*
				       pn-gv (list (jit-constant-i32 0) (jit-constant-i32 0)) "pn")))
		   (irc-intrinsic "internSymbol_tsp" ltv-temp sn-value-ptr pn-value-ptr))
		 (irc-intrinsic "makeSymbol_tsp" ltv-temp sn-value-ptr)
		 ))))



(defun symbol-indices-in-current-*load-time-value-vector* (symbols env)
  (let (result)
    (dolist (sym symbols)
      ;; Coalesce the symbol into the *symbol-coalesce* hash-table
      (codegen-ltv/symbol nil sym env)
      ;; Get its index
      (setq result (cons (gethash sym *symbol-coalesce*) result)))
    (nreverse result)))





(defvar *character-coalesce* nil)
(defun codegen-ltv/character (result obj env)
  "Return IR code that generates a Character_sp object"
  (with-coalesce-load-time-value (ltv-ref result obj env)
    :coalesce-hash-table *character-coalesce*
    :maker (let ((constant-ap-arg (jit-constant-i32 (char-code obj))))
	     (irc-intrinsic "makeCharacter" ltv-ref constant-ap-arg))))






(defun codegen-ltv/integer (result obj env)
  (cond
    ((fixnump obj) (codegen-ltv-fixnum result obj env))
    ((bignump obj) (codegen-ltv-bignum result obj env))
    (t (error "Illegal argument ~a for codegen-ltv/integer" obj))))



(defvar *string-coalesce* nil)
(defun codegen-ltv/string (result str env)
  "Return IR code that generates a string"
  (with-coalesce-load-time-value (ltv-ref result str env)
    :coalesce-hash-table *string-coalesce*
    :maker (let* ((constant (llvm-sys:make-string-global *the-module* str))
		  (ptr (llvm-sys:create-in-bounds-gep *irbuilder* constant
						      (list (jit-constant-i32 0) (jit-constant-i32 0)) "ptr")))
	     (irc-intrinsic "makeString" ltv-ref ptr))))






#+short-float(defvar *short-float-coalesce* nil)
#+short-float(defun codegen-ltv-short-float (result dbl env)
  (with-coalesce-load-time-value (ltv-ref result dbl env)
    :coalesce-hash-table *short-float-coalesce*
    :maker (let* ((constant (llvm-sys:make-apfloat dbl))
		  (constant-ap-arg (llvm-sys:constant-fp-get *llvm-context* constant)))
	     (irc-intrinsic "makeShortFloat" ltv-ref constant-ap-arg))))

(defvar *single-float-coalesce* nil)
(defun codegen-ltv-single-float (result flt env)
  (with-coalesce-load-time-value (ltv-ref result flt env)
    :coalesce-hash-table *single-float-coalesce*
    :maker (let* ((constant (llvm-sys:make-apfloat-float flt))
		  (constant-ap-arg (llvm-sys:constant-fp-get *llvm-context* constant)))
	     (irc-intrinsic "makeSingleFloat" ltv-ref constant-ap-arg))))

(defvar *double-float-coalesce* nil)
(defun codegen-ltv-double-float (result dbl env)
  "Return IR code that generates a double float"
  (with-coalesce-load-time-value (ltv-ref result dbl env)
    :coalesce-hash-table *double-float-coalesce*
    :maker (let* ((constant (llvm-sys:make-apfloat-double dbl))
		  (constant-ap-arg (llvm-sys:constant-fp-get *llvm-context* constant)))
	     (irc-intrinsic "makeDoubleFloat" ltv-ref constant-ap-arg))))


#+long-float(defvar *long-float-coalesce* nil)
#+long-float(defun codegen-ltv-long-float (result val env)
  "Return IR code that generates a long float"
  (with-coalesce-load-time-value (ltv-ref result val env)
    :coalesce-hash-table *long-float-coalesce*
    :maker (let* ((constant (llvm-sys:make-apfloat-long-float val))
		  (constant-ap-arg (llvm-sys:constant-fp-get *llvm-context* constant)))
	     (irc-intrinsic "makeLongFloat" ltv-ref constant-ap-arg))))


(defun codegen-ltv/float (result arg env)
  (cond
    #+short-float((short-float-p arg)  (codegen-ltv-short-float result arg env))
    ((single-float-p arg) (codegen-ltv-single-float result arg env))
    ((double-float-p arg) (codegen-ltv-double-float result arg env))
    #+long-float((long-float-p arg)   (codegen-ltv-long-float result arg env))
    (t (error "Illegal argument ~a for codegen-float" arg))))





(defvar *nil-coalesce* nil)
(defun codegen-ltv/nil (result env)
  (with-coalesce-load-time-value (ltv-ref result nil env)
    :coalesce-hash-table *nil-coalesce*
    :maker (irc-intrinsic "makeNil" ltv-ref)))

(defvar *t-coalesce* nil)
(defun codegen-ltv-t (result env)
  (with-coalesce-load-time-value (ltv-ref result nil env)
    :coalesce-hash-table *t-coalesce*
    :maker (irc-intrinsic "makeT" ltv-ref)))




(defun codegen-ltv/array (result obj env)
  (cond
    ((stringp obj) (error "Strings should not get here"))
    ((vectorp obj) (codegen-ltv/container result obj env))
    ((arrayp obj) (codegen-ltv/container result obj env))
    (t (error "Add support to codegen array of type ~a" (class-name obj)))))


;; ----------------------------------------------------------
;; ----------------------------------------------------------
;; ----------------------------------------------------------
;;
;; The following are invoked only when within a COMPILE
;;
;; ----------------------------------------------------------
;; ----------------------------------------------------------



(defvar *run-time-literal-holder* nil
  "This dynamic variable is set to a LoadTimeValues object every time
COMPILE is invoked and it stores the literals for the function being compiled")

(defvar *run-time-value-nil-index* nil)
(defvar *run-time-value-t-index* nil)





(defun codegen-rtv/nil (result env)
  (if result
    (irc-intrinsic "copyLoadTimeValue" result *run-time-value-holder-global-var* (jit-constant-i32 *run-time-value-nil-index*))
    *run-time-value-nil-index*))

(defun codegen-rtv/t (result env)
  (if result
    (irc-intrinsic "copyLoadTimeValue" result *run-time-value-holder-global-var* (jit-constant-i32 *run-time-value-t-index*))
    *run-time-value-t-index*))


(defvar *run-time-symbol-coalesce* (make-hash-table :test #'eq)
  "Coalesce literal symbols")
(defun codegen-rtv/symbol (result sym env)
  (let ((idx (gethash sym *run-time-symbol-coalesce*)))
    (unless idx
      (setq idx (data-vector-push-extend *run-time-literal-holder* sym 16))
      (hash-table-setf-gethash *run-time-symbol-coalesce* sym idx))
    (if result
      (irc-intrinsic "copyLoadTimeValue" result
		*run-time-value-holder-global-var*
		(jit-constant-i32 idx))
      idx
      )))



(defun codegen-rtv/all (result val env)
  "If result is defined then copy run-time-value into it otherwise return idx of run-time-value"
  (let ((idx (data-vector-push-extend *run-time-literal-holder* val 16)))
    (if result
	(irc-intrinsic "copyLoadTimeValue" result
		  *run-time-value-holder-global-var*
		  (jit-constant-i32 idx))
	idx
	)))


(defun codegen-rtv (result obj env)
  (when *generate-compile-file-load-time-values*
    (error "You cannot generate run-time-values from COMPILE-FILE"))
  (cond
    ((null obj) (codegen-rtv/nil result env))
    ((eq t obj) (codegen-rtv/t result env))
    ((symbolp obj) (codegen-rtv/symbol result obj env))
    (t (codegen-rtv/all result obj env))))




(defvar *run-time-symbol-type-symbol-coalesce* (make-hash-table :test #'eq)
  "Coalesce literal symbols")
(defun codegen-rts/symbol (result sym env)
  (let ((idx (gethash sym *run-time-symbol-type-symbol-coalesce*)))
    (unless idx
      (setq idx (symbols-vector-push-extend *run-time-literal-holder* sym 16))
      (hash-table-setf-gethash *run-time-symbol-type-symbol-coalesce* sym idx))
    (if result
      (irc-intrinsic "copyLoadTimeSymbol" result
		*run-time-value-holder-global-var*
		(jit-constant-i32 idx))
      idx
      )))



;; --------------------------------------------------
;; --------------------------------------------------
;;
;; Save load-time-symbols
;;
;; --------------------------------------------------
;; --------------------------------------------------


(defvar *next-load-time-symbol-index*  nil
  "Each load-time-symbol gets assigned a unique integer index
and the next one is stored here")


(defun get-next-available-lts-entry (&optional value)
  "Return the next load-time-symbol-index. If this is being invoked from COMPILE then
the value is put into *default-load-time-value-vector* and its index is returned"
  (let ((load-time-symbol-idx *next-load-time-symbol-index*))
    (incf *next-load-time-symbol-index*)
    load-time-symbol-idx))
	



(defvar *symbol-type-symbol-coalesce* nil)
(defun codegen-lts/symbol (result symbol env)
  (with-coalesce-load-time-value (lts-temp result symbol env)
    :coalesce-hash-table *symbol-type-symbol-coalesce*
    :maker (let* ((sn (symbol-name symbol))
		  (sym-pkg (symbol-package symbol))
		  (sn-gv (llvm-sys:get-or-create-uniqued-string-global-variable
			  *the-module* sn
			  (bformat nil ":::symbol-name-%s" sn)))
		  (sn-value-ptr (llvm-sys:create-in-bounds-gep
				 *irbuilder* sn-gv (list (jit-constant-i32 0) (jit-constant-i32 0)) "sn")))
	     (if sym-pkg
		 (let* ((pn (package-name (symbol-package symbol)))
			(pn-gv (llvm-sys:get-or-create-uniqued-string-global-variable
				*the-module* pn (bformat nil ":::package-name-%s" pn)))
			(pn-value-ptr (llvm-sys:create-in-bounds-gep
				       *irbuilder*
				       pn-gv (list (jit-constant-i32 0) (jit-constant-i32 0)) "pn")))
		   (irc-intrinsic "internSymbol_symsp" lts-temp sn-value-ptr pn-value-ptr))
		 (irc-intrinsic "makeSymbol_symsp" lts-temp sn-value-ptr)
		 ))
    :push-and-get-reference-fn-name "loadTimeSymbolReference"
    :copy-value-fn-name "copyLoadTimeSymbol"
    :get-next-available-entry-index-fn-name get-next-available-lts-entry
    ))











;; ----------------------------------------------------------
;; ----------------------------------------------------------
;; ----------------------------------------------------------
;;
;; The following are only invoked from COMPILE-FILE
;;
;; ----------------------------------------------------------
;; ----------------------------------------------------------



(defmacro with-load-time-value-counters ((ltv-value-counter ltv-symbol-counter &key postscript ) &rest body)
  "Wrap code that modifies *next-load-time-value-index* and *next-load-time-symbol-index* and then 
evaluate POSTSCRIPT with their final values in ltv-value-counter and ltv-symbol-counter respectively"
  `(let (,ltv-value-counter
	 ,ltv-symbol-counter
	 (*next-load-time-value-index* 0)
	 (*next-load-time-symbol-index* 0)
	 )
     ,@body
     (setf ,ltv-value-counter *next-load-time-value-index*
	   ,ltv-symbol-counter *next-load-time-symbol-index*)
     ,postscript))



(defun initialize-special-load-time-values (env)
  "Initialize special load-time-values like nil and t"
  (codegen-ltv/nil nil env)
  (codegen-ltv-t   nil env))



(defmacro with-load-time-value-unit ((ltv-init-fn) &rest body)
  "Wraps generation of load-time-values. This is only invoked from COMPILE-FILE and it creates
the ___loadTimeDataInitializer function and sets up everything for the coalescence and
marshaling of compiled quoted data"
  (let ((cleanup-block-gs (gensym "ltv-cleanup-block"))
	(irbuilder-alloca (gensym "ltv-irbuilder-alloca"))
	(irbuilder-body (gensym "ltv-irbuilder-body"))
	(traceid-gs (gensym "traceid"))
	(fn-env-gs (gensym "ltv-fn-env")))
    #||	(ltv-invocation-history-frame (gensym "ltv-ihf")))||#
    `(multiple-value-bind (,ltv-init-fn ,fn-env-gs ,cleanup-block-gs
					,irbuilder-alloca ,
					irbuilder-body #||,ltv-invocation-history-frame||#)
	 (irc-function-create "___loadTimeDataInitializer" nil nil
			      :function-type +fn-void+
			      :argument-names nil)
       (let ((*load-time-value-initialization-function* ,ltv-init-fn)
	     (*current-function* ,ltv-init-fn)
	     (*generate-compile-file-load-time-values* t)
	     (*load-time-initializer-environment* ,fn-env-gs)
	     (*irbuilder-ltv-function-alloca* ,irbuilder-alloca)
	     (*irbuilder-ltv-function-body* ,irbuilder-body)
	     (*load-time-value-holder-global-var*
	      (llvm-sys:make-global-variable *the-module*
					     +run-and-load-time-value-holder-global-var-type+
					     nil
					     'llvm-sys:internal-linkage
					     (llvm-sys:constant-pointer-null-get +run-and-load-time-value-holder-global-var-type+)
					     *load-time-value-holder-name*))
	     (*next-load-time-value-index* 0)
	     (*next-load-time-symbol-index* 0)
	     (*fixnum-coalesce* (make-hash-table :test #'eql))
	     (*bignum-coalesce* (make-hash-table :test #'eql))
	     (*symbol-coalesce* (make-hash-table :test #'eq))
	     (*load-time-value-coalesce* (make-hash-table :test #'eq))
	     (*symbol-type-symbol-coalesce* (make-hash-table :test #'eq))
	     #+short-float(*short-float-coalesce* (make-hash-table :test #'eql))
	     (*single-float-coalesce* (make-hash-table :test #'eql))
	     (*double-float-coalesce* (make-hash-table :test #'eql))
	     #+long-float(*long-float-coalesce* (make-hash-table :test #'eql))
	     (*string-coalesce* (make-hash-table :test #'equal))
	     (*character-coalesce* (make-hash-table :test #'eql))
	     (*nil-coalesce* (make-hash-table :test #'eq))
	     (*t-coalesce* (make-hash-table :test #'eq))
	     (*load-time-value-result* (irc-alloca-tmv *load-time-initializer-environment*
						       :irbuilder *irbuilder-ltv-function-alloca*))
	     )
	 ;; Evaluate the body into here - this will generate code in new functions
	 ;; and generate code to invoke those new functions within the *irbuilder-ltv-function-body* irbuilder
	 ;; it will also setup all of the literals, load-time-values and symbols 
	 ;;

	 (with-load-time-value-counters (ltv-value-counter
					 ltv-symbol-counter
					 :postscript (with-irbuilder (,fn-env-gs *irbuilder-ltv-function-alloca*)
						       (cmp-log "Setting up getOrCreateLoadTimeValueArray\n")
						       (irc-intrinsic "getOrCreateLoadTimeValueArray"
								 *load-time-value-holder-global-var*
								 *gv-source-path-name*
								 (jit-constant-i32 ltv-value-counter)
								 (jit-constant-i32 ltv-symbol-counter))))
	   (initialize-special-load-time-values ,fn-env-gs)
	   ,@body)
	 (with-irbuilder (,fn-env-gs *irbuilder-ltv-function-body*)
	   (let ((*gv-current-function-name* (jit-make-global-string-ptr
					      (llvm-sys:get-name ,ltv-init-fn) "fn-name")))
	     (with-landing-pad (irc-get-terminate-landing-pad-block ,fn-env-gs)
	       (irc-function-cleanup-and-return ,fn-env-gs ))
	     ))))))




(defmacro with-ltv-function-codegen ((result env) &rest form)
  "Generate code within the ltv-function - used by codegen-load-time-value"
  `(let ((*irbuilder-function-alloca* *irbuilder-ltv-function-alloca*)
	 (*current-function* *load-time-value-initialization-function*)
	 (,result *load-time-value-result*)
	 (,env *load-time-initializer-environment*)
#||	 (*current-invocation-history-frame* *load-time-value-invocation-history-frame*)||#
	 )
     (with-irbuilder (*load-time-initializer-environment* *irbuilder-ltv-function-body*)
       (with-landing-pad (irc-get-cleanup-landing-pad-block *load-time-initializer-environment*)
       ,@form))))


;;(eval-when (:compile-toplevel) (setq cmp:*debug-compiler* t))


(defun compile-ltv-thunk (name form env)
  "Compile the form into an llvm function and return that function"
  (dbg-set-current-debug-location-here)
  (or (stringp name) (error "name must be a string"))
  (let* ((ltv-index (get-next-available-ltv-entry))
	 (fn (with-new-function (fn fn-env
				    :function-name name
				    :parent-env env
				    :function-form form)
	       (let* ((fn-result (car (llvm-sys:get-argument-list fn)))
		      (given-name (llvm-sys:get-name fn)))
		 ;; Map the function argument names
		 (cmp-log "Creating ltv thunk with name: %s\n" given-name)
		 (let ((ltv-result (irc-intrinsic "loadTimeValueReference"
					     *load-time-value-holder-global-var*
					     (jit-constant-i32 ltv-index))))
;;		   (break "codegen ltv thunk form")
		   (dbg-set-current-debug-location-here)
		   (codegen ltv-result form fn-env)
		   (irc-intrinsic "copyTsp" fn-result ltv-result)
		   (dbg-set-current-debug-location-here) 
		   )))))
    (cmp-log-dump fn)
    (irc-verify-function fn t)
    (values ltv-index fn)))



;; ----------------------------------------------------------
;; ----------------------------------------------------------
;; ----------------------------------------------------------
;;
;; The following are invoked from both COMPILE-FILE and COMPILE
;;
;; ----------------------------------------------------------
;; ----------------------------------------------------------


(defun codegen-literal (result obj env)
  "Generate a load-time-value or run-time-value literal depending if called from COMPILE-FILE or COMPILE respectively"
  (if *generate-compile-file-load-time-values*
      (cond
	((null obj) (codegen-ltv/nil result env))
	((integerp obj) (codegen-ltv/integer result obj env))
	((stringp obj) (codegen-ltv/string result obj env))
	((floatp obj) (codegen-ltv/float result obj env))
	((symbolp obj) (codegen-ltv/symbol result obj env))
	((characterp obj) (codegen-ltv/character result obj env))
	((arrayp obj) (codegen-ltv/array result obj env))
;;	((source-code-cons-p obj) (codegen-ltv-source-code-cons result obj env))
	((consp obj) (codegen-ltv/container result obj env))
	((hash-table-p obj) (codegen-ltv/container result obj env))
	(t (error "In codegen-literal add support to codegen the atom type ~a - value: ~a" (class-name (class-of obj)) obj )))
      ;; Below is how we compile atoms for COMPILE - literal objects are passed into the
      ;; default module without coalescence.
      (codegen-rtv result obj env)))


(defun codegen-quote (result rest env)
  (cmp-log "codegen-quote: %s\n" rest )
  (codegen-literal result (car rest) env))


(defun pretty-load-time-name (literal ltv-idx)
  (cond
    ((symbolp literal) (bformat nil "SYMBOL->%s" literal))
    ((consp literal) "CONS")
    ((arrayp literal) "ARRAY")
    ((numberp literal) (format nil "NUMBER->~a" literal))
    (t (subseq (bformat nil "ltv-idx_%d_val->%s" ltv-idx literal) 0 30))))


(defun compile-reference-to-load-time-value (idx env &optional (name "value"))
  (irc-intrinsic "loadTimeValueReference"
	    (if *generate-compile-file-load-time-values*
		*load-time-value-holder-global-var*
		*run-time-value-holder-global-var*)
	    (jit-constant-i32 idx) name))

(defun compile-reference-to-literal (literal env)
  "Generate a reference to a load-time-value or run-time-value literal depending if called from COMPILE-FILE or COMPILE respectively"
  (let ((ltv-idx (codegen-literal nil literal env)))
    (unless (fixnump ltv-idx) (error "Could not compile-reference-to-literal: ~a" literal))
    (compile-reference-to-load-time-value ltv-idx env (pretty-load-time-name literal ltv-idx))))




(defun codegen-symbol (result obj env)
  "Generate a load-time-symbol or run-time-symbol depending if called from COMPILE-FILE or COMPILE respectively"
  (or (symbolp obj) (error "obj must be a symbol - instead it is: ~A" obj))
  (if *generate-compile-file-load-time-values*
      (codegen-lts/symbol result obj env)
  ;; Below is how we compile atoms for COMPILE - literal objects are passed into the
  ;; default module without coalescence.
      (codegen-rts/symbol result obj env)))



(defun compile-reference-to-symbol (symbol env)
  "Generate a reference to a load-time-symbol or run-time-symbol depending if called from COMPILE-FILE or COMPILE respectively"
  (let ((lts-idx (codegen-symbol nil symbol env)))
    (unless (fixnump lts-idx) (error "Could not compile-reference-to-symbol: ~a" symbol))
    (if *generate-compile-file-load-time-values*
	(progn
	  (unless *load-time-value-holder-global-var* (error "There must be a *load-time-value-holder-global-var* defined"))
	  (irc-intrinsic "loadTimeSymbolReference" *load-time-value-holder-global-var* (jit-constant-i32 lts-idx) (pretty-load-time-name symbol lts-idx)))
	(progn
	  (irc-intrinsic "loadTimeSymbolReference" *run-time-value-holder-global-var* (jit-constant-i32 lts-idx) (pretty-load-time-name symbol lts-idx))))))




