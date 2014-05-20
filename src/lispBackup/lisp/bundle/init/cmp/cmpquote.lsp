

(in-package :cmp)

(defvar *load-time-value-coalesce* nil)
(defvar *load-time-initializer-environment* nil)

(defvar *default-load-time-value-vector-name* "<default-ltv>")
(defvar *default-load-time-value-vector* 
  (load-time-value-array *default-load-time-value-vector-name*))


(defvar *load-time-value-vector-global-var*
  (llvm-sys:make-global-variable *the-module*
				 +ltvsp*+
				 nil
				 'llvm-sys:private-linkage
				 (llvm-sys:constant-pointer-null-get +ltvsp*+ )
				 "default-load-time-value-vector")
  "All load-time-values and quoted values are stored in this array
accessed with an integer index"
  )

(llvm-sys:add-global-mapping-for-load-time-value-vector *the-execution-engine*
							*load-time-value-vector-global-var*
							*default-load-time-value-vector-name*)
(defvar *next-load-time-value-index* nil
  "Each load-time-value and quoted value get's assigned a unique integer index
that is stored here")


(defvar *generate-compile-file-load-time-values* nil
  "This variable controls whether literals are compiled into the
load-time-value manager (true - in COMPILE-FILE) or not (false - in COMPILE)."
)






(defmacro with-next-load-time-value ((ltv-ref obj env) &rest creator)
  "An ltv-index for _obj_ will be created and the maker is codegen'd into the load-time-value function.
Return the ltv index of the value."
  (let ((index-gs (gensym "index")))
    ;; Always create a new load-time-value
    `(let ((,index-gs (get-next-available-ltv-entry)))
       (with-basic-block-insert *current-load-time-value-insert-block*
	 ;;	 (core::hash-table-setf-gethash ,coalesce-hash-table ,key-gs ,index-gs)
	 (let ((,ltv-ref (irc-call env "loadTimeValuePushAndGetReference" *load-time-value-vector-global-var* (jit-constant-i32 ,index-gs))))
	   (with-landing-pad (irc-get-cleanup-landing-pad-block ,env)
	     ,@creator))
	 ;; insert block may have changed in maker
	 (setq *current-load-time-value-insert-block* (irc-get-insert-block)))
;;    (irc-call ,env "copyLoadTimeValue" ,result *load-time-value-vector-global-var* (jit-constant-i32 ,index-gs))
       ,index-gs)))


(defmacro with-initialize-load-time-value ((ltv-ref ltv-idx env) &rest initializer)
  `(with-basic-block-insert *current-load-time-value-insert-block*
     ;;	 (core::hash-table-setf-gethash ,coalesce-hash-table ,key-gs ,index-gs)
     (let ((,ltv-ref (irc-call env "loadTimeValueReference" *load-time-value-vector-global-var* (jit-constant-i32 ,ltv-idx))))
       (with-landing-pad (irc-get-cleanup-landing-pad-block ,env)
	 ,@initializer))
     ;; insert block may have changed in initializer
     (setq *current-load-time-value-insert-block* (irc-get-insert-block))))
	     

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

(defun make-ltv-cons (val env)
  (if (source-code-cons-p val)
      (with-next-load-time-value (ltv-ref val env)
	(let* ((source-file-info (source-file-info val))
	       (source-path-name (if source-file-info
				     (source-file-info-path-name source-file-info)
				     "-no-file-")))
	  (irc-call env "ltv_makeSourceCodeCons"
		    ltv-ref
		    (jit-constant-unique-string-ptr source-path-name)
		    (jit-constant-i32 (source-file-info-lineno val))
		    (jit-constant-i32 (source-file-info-column val)))))
      (with-next-load-time-value (ltv-ref val env)
	(irc-call env "ltv_makeCons" ltv-ref))))

(defun initialize-ltv-cons (obj ltv-idx env)
  (with-initialize-load-time-value (ltv-ref ltv-idx env)
    (let ((car-ref (irc-call env "loadTimeValueReference" *load-time-value-vector-global-var*
			     (jit-constant-i32 (gethash (car obj) *node-table*))))
	  (cdr-ref (irc-call env "loadTimeValueReference" *load-time-value-vector-global-var*
			     (jit-constant-i32 (gethash (cdr obj) *node-table*)))))
      (irc-call env "rplaca" ltv-ref car-ref)
      (irc-call env "rplacd" ltv-ref cdr-ref))))





(defun walk-structure-array-objects (obj env)
  (with-walk-structure (obj env)
    :maker (make-ltv-array-objects obj env)
    :walker (let ((total-size (if (array-has-fill-pointer-p obj)
				  (length obj)
				  (array-total-size obj))))
	      (dotimes (idx total-size)
		(walk-structure (row-major-aref obj idx) env)))))

(defun make-ltv-array-objects (val env)
  (let ((array-element-type (irc-alloca-tsp *load-time-initializer-environment* "array-element-type")))
    (with-basic-block-insert *current-load-time-value-insert-block*
      (codegen-literal array-element-type (array-element-type val) *load-time-initializer-environment*))
    (with-next-load-time-value (ltv-ref val env)
      (irc-call env "ltv_makeArrayObjects" ltv-ref
		array-element-type
		(jit-constant-i32 (array-rank val))
		(jit-constant-i32-vector-ptr (array-dimensions val))))))

(defun initialize-ltv-array-objects (obj ltv-idx env)
  (let* ((total-size (if (array-has-fill-pointer-p obj)
			 (length obj)
			 (array-total-size obj)))
	 (ltv-indices (core::make-vector-objects nil nil total-size)))
    (dotimes (i total-size)
      (setf-svref ltv-indices i (gethash (row-major-aref obj i) *node-table*)))
    (with-initialize-load-time-value (ltv-ref ltv-idx env)
      (irc-call env "ltv_initializeArrayObjectsRowMajorArefOrder"
		ltv-ref
		*load-time-value-vector-global-var*
		(jit-constant-i32-vector-ptr ltv-indices)))))





(defun walk-structure-hash-table (obj env)
  (with-walk-structure (obj env)
    :maker (make-ltv-hash-table obj env)
    :walker (maphash #'(lambda (key val)
			 (walk-structure key env)
			 (walk-structure val env))
		     obj)))

(defun make-ltv-hash-table (val env)
  (let ((ht-test (irc-alloca-tsp *load-time-initializer-environment* "hash-table-test")))
    (with-basic-block-insert *current-load-time-value-insert-block*
      (codegen-literal ht-test (hash-table-test val) *load-time-initializer-environment*))
    (with-next-load-time-value (ltv-ref val env)
      (irc-call env "ltv_makeHashTable" ltv-ref ht-test))))

(defun initialize-ltv-hash-table (obj ltv-idx env)
  (let* ((num-entries (hash-table-count obj))
	 (ltv-key-value-indices (core::make-vector-objects nil nil (* 2 num-entries))))
    ;; Key/value indices are stored as adjacent pairs in ltv-key-value-indices
    (let ((key-idx 0))
      (maphash #'(lambda (key val)
		   (setf-svref ltv-key-value-indices key-idx (gethash key *node-table*))
		   (setf-svref ltv-key-value-indices (1+ key-idx) (gethash val *node-table*))
		   (setq key-idx (+ 2 key-idx)))
	       obj))
    (with-initialize-load-time-value (ltv-ref ltv-idx env)
      (irc-call env "ltv_initializeHashTable" ltv-ref
		(jit-constant-i32 num-entries)
		*load-time-value-vector-global-var*
		(jit-constant-i32-vector-ptr ltv-key-value-indices)))))



(defun walk-structure (cur env)
  (cond
    ((consp cur)   (walk-structure-cons cur env))
    ((vector-objects-p cur) (walk-structure-array-objects cur env))
    ((array-objects-p cur)  (walk-structure-array-objects cur env))
    ((hash-table-p cur) (walk-structure-hash-table cur env))
    (t (walk-structure-simple cur env))))









(defun initialize-ltv-nodes (env)
  (maphash #'(lambda (key val)
	       (cond
		 ((consp key)   (initialize-ltv-cons key val env))
		 ((vector-objects-p key)  (initialize-ltv-array-objects key val env))
		 ((array-objects-p key)  (initialize-ltv-array-objects key val env))
		 ((hash-table-p key) (initialize-ltv-hash-table key val env))
		 (t nil)))
	   *node-table*))


(defun codegen-ltv-container (result obj env)
  (setq *node-table* (make-hash-table :test #'eq))
  (let ((ltv-idx (walk-structure obj env)))
    (initialize-ltv-nodes env)
    (when result
      (irc-call env "getLoadTimeValueReference"
		result
		*load-time-value-vector-global-var*
		(jit-constant-i32 ltv-idx)))
    ltv-idx))





(defun get-next-available-ltv-entry (&optional value)
  "Return the next ltv-index. If this is being invoked from COMPILE then
the value is put into *default-load-time-value-vector* and its index is returned"
  (let ((ltv-idx *next-load-time-value-index*))
    (incf *next-load-time-value-index*)
    ltv-idx))
	


(defmacro with-coalesce-load-time-value ((ltv-ref result obj env) &key coalesce-hash-table maker)
  "An index for _obj_ will be looked up in the coalesce-hash-table
- if not found one is created and the maker is codegen'd into the load-time-value function.
Finally, if _result_ is not nil then the load-time-value index and *load-time-value-vector-global-var* will be used to
codegen a lookup for the value at runtime.
The objument _result_ can be passed as nil - this is for setting up special load-time-values like nil and t.
Return the ltv index of the value."
  (let ((index-gs (gensym "index"))
	(key-gs (gensym "key")))
    `(let ((,index-gs nil)
	   (,key-gs ,obj))
       (setq ,index-gs (gethash ,key-gs ,coalesce-hash-table))
       ;; If there is no index then create one
       (if (null ,index-gs)
	   (with-basic-block-insert *current-load-time-value-insert-block*
	     (setq ,index-gs (get-next-available-ltv-entry))
	     (core::hash-table-setf-gethash ,coalesce-hash-table ,key-gs ,index-gs)
	     (let ((,ltv-ref (irc-call env "loadTimeValuePushAndGetReference" *load-time-value-vector-global-var* (jit-constant-i32 ,index-gs))))
	       (with-landing-pad (irc-get-cleanup-landing-pad-block ,env)
		 ,maker))
	     ;; insert block may have changed in maker
	     (setq *current-load-time-value-insert-block* (irc-get-insert-block))))
       (when ,result
	   (irc-call ,env "copyLoadTimeValue" ,result *load-time-value-vector-global-var* (jit-constant-i32 ,index-gs)))
       ,index-gs
       )))
	     
  


(defvar *fixnum-coalesce* nil 
  "Store a hash-table-eql of fixnums to indices")

(defun codegen-ltv-fixnum (result obj env)
  (with-coalesce-load-time-value (ltv-ref result obj env)
    :coalesce-hash-table *fixnum-coalesce*
    :maker (irc-call env "makeFixnum" ltv-ref (jit-constant-i32 obj))))




(defvar *bignum-coalesce* nil)
(defun codegen-ltv-bignum (result obj env)
  (with-coalesce-load-time-value (ltv-temp result obj env)
    :coalesce-hash-table *bignum-coalesce*
    :maker (let ((string-arg (jit-make-global-string-ptr (bformat nil "%d" obj))))
	     (irc-call env "makeBignum" ltv-temp string-arg))))


(defvar *symbol-coalesce* nil)
(defun codegen-ltv-symbol (result symbol env)
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
		   (irc-call env "internSymbol" ltv-temp sn-value-ptr pn-value-ptr))
		 (irc-call env "makeSymbol" ltv-temp sn-value-ptr)
		 ))))



(defun symbol-indices-in-current-*load-time-value-vector* (symbols env)
  (let (result)
    (dolist (sym symbols)
      ;; Coalesce the symbol into the *symbol-coalesce* hash-table
      (codegen-ltv-symbol nil sym env)
      ;; Get its index
      (setq result (cons (gethash sym *symbol-coalesce*) result)))
    (nreverse result)))





(defvar *character-coalesce* nil)
(defun codegen-ltv-character (result obj env)
  "Return IR code that generates a Character_sp object"
  (with-coalesce-load-time-value (ltv-ref result obj env)
    :coalesce-hash-table *character-coalesce*
    :maker (let ((constant-ap-arg (jit-constant-i32 (char-code obj))))
	     (irc-call env "makeCharacter" ltv-ref constant-ap-arg))))






(defun codegen-ltv-integer (result obj env)
  (cond
    ((fixnump obj) (codegen-ltv-fixnum result obj env))
    ((bignump obj) (codegen-ltv-bignum result obj env))
    (t (error "Illegal argument ~a for codegen-ltv-integer" obj))))



(defvar *string-coalesce* nil)
(defun codegen-ltv-string (result str env)
  "Return IR code that generates a string"
  (with-coalesce-load-time-value (ltv-ref result str env)
    :coalesce-hash-table *string-coalesce*
    :maker (let* ((constant (llvm-sys:make-string-global *the-module* str))
		  (ptr (llvm-sys:create-in-bounds-gep *irbuilder* constant
						      (list (jit-constant-i32 0) (jit-constant-i32 0)) "ptr")))
	     (irc-call env "makeString" ltv-ref ptr))))






(defvar *short-float-coalesce* nil)
(defun codegen-ltv-short-float (result dbl env)
  (with-coalesce-load-time-value (ltv-ref result dbl env)
    :coalesce-hash-table *short-float-coalesce*
    :maker (let* ((constant (llvm-sys:make-apfloat dbl))
		  (constant-ap-arg (llvm-sys:constant-fp-get *llvm-context* constant)))
	     (irc-call env "makeShortFloat" ltv-ref constant-ap-arg))))

(defvar *single-float-coalesce* nil)
(defun codegen-ltv-single-float (result dbl env)
  (with-coalesce-load-time-value (ltv-ref result dbl env)
    :coalesce-hash-table *single-float-coalesce*
    :maker (let* ((constant (llvm-sys:make-apfloat dbl))
		  (constant-ap-arg (llvm-sys:constant-fp-get *llvm-context* constant)))
	     (irc-call env "makeSingleFloat" ltv-ref constant-ap-arg))))

(defvar *double-float-coalesce* nil)
(defun codegen-ltv-double-float (result dbl env)
  "Return IR code that generates a double float"
  (with-coalesce-load-time-value (ltv-ref result dbl env)
    :coalesce-hash-table *double-float-coalesce*
    :maker (let* ((constant (llvm-sys:make-apfloat dbl))
		  (constant-ap-arg (llvm-sys:constant-fp-get *llvm-context* constant)))
	     (irc-call env "makeDoubleFloat" ltv-ref constant-ap-arg))))


(defvar *long-float-coalesce* nil)
(defun codegen-ltv-long-float (result dbl env)
  "Return IR code that generates a long float"
  (with-coalesce-load-time-value (ltv-ref result dbl env)
    :coalesce-hash-table *long-float-coalesce*
    :maker (let* ((constant (llvm-sys:make-apfloat dbl))
		  (constant-ap-arg (llvm-sys:constant-fp-get *llvm-context* constant)))
	     (irc-call env "makeLongFloat" ltv-ref constant-ap-arg))))


(defun codegen-ltv-float (result arg env)
  (cond
    ((short-float-p arg)  (codegen-ltv-short-float result arg env))
    ((single-float-p arg) (codegen-ltv-single-float result arg env))
    ((double-float-p arg) (codegen-ltv-double-float result arg env))
    ((long-float-p arg)   (codegen-ltv-long-float result arg env))
    (t (error "Illegal argument ~a for codegen-float" arg))))





(defvar *nil-coalesce* nil)
(defun codegen-ltv-nil (result env)
  (with-coalesce-load-time-value (ltv-ref result nil env)
    :coalesce-hash-table *nil-coalesce*
    :maker (irc-call env "makeNil" ltv-ref)))

(defvar *t-coalesce* nil)
(defun codegen-ltv-t (result env)
  (with-coalesce-load-time-value (ltv-ref result nil env)
    :coalesce-hash-table *t-coalesce*
    :maker (irc-call env "makeT" ltv-ref)))




(defun codegen-array (result obj env)
  (cond
    ((vector-objects-p obj) (codegen-ltv-container result obj env))
    ((array-objects-p obj) (codegen-ltv-container result obj env))
    (t (error "Add support to codegen array of type ~a" (class-name obj)))))


;; ----------------------------------------------------------
;; ----------------------------------------------------------
;; ----------------------------------------------------------
;;
;; The following are invoked only when within a COMPILE
;;
;; ----------------------------------------------------------
;; ----------------------------------------------------------

#| ;; The old with-coalesce-load-time-value
;; It contains some code that may be required to generate run-time-values



(defun get-next-available-ltv-entry (&optional value)
  "Return the next ltv-index. If this is being invoked from COMPILE then
the value is put into *default-load-time-value-vector* and its index is returned"
  (let (ltv-idx)
    (if *generate-compile-file-load-time-values*
	(progn
	  (setq ltv-idx *next-load-time-value-index*)
	  (incf *next-load-time-value-index*))
	(setq ltv-idx (vector-push-extend value *default-load-time-value-vector*)))
    ltv-idx))
	

(defmacro with-coalesce-load-time-value ((ltv-ref result arg env) &key coalesce-hash-table maker)
  "If called from COMPILE-FILE *generate-compile-file-load-time-values* will be true and the following will happen:
An index for arg will be looked up in the coalesce-hash-table
- if not found one is created and the maker is codegen'd into the load-time-value function.
If called from COMPILE *generate-compile-file-load-time-values* will be false and the the arg will not
be coalesced and it will just be written into the *default-load-time-value-vector* and the index for the new object
will be returned.
Finally, the index generated by any of this code will be used along with *load-time-value-vector-global-var* to
codegen a lookup for the value at runtime.
If result is nil then don't codegen to write the value into result
- this is for setting up special load-time-values like nil and t.
Return the ltv index of the value."
  (let ((index-gs (gensym "index"))
	(key-gs (gensym "key")))
    `(let ((,index-gs nil)
	   (,key-gs ,arg))
       (if *generate-compile-file-load-time-values*
	   (progn
	     (setq ,index-gs (gethash ,key-gs ,coalesce-hash-table))
	     ;; If there is no index then create one
	     (if (null ,index-gs)
		 (with-basic-block-insert *current-load-time-value-insert-block*
		   (setq ,index-gs (get-next-available-ltv-entry))
		   (core::hash-table-setf-gethash ,coalesce-hash-table ,key-gs ,index-gs)
		   (let ((,ltv-ref (irc-call env "loadTimeValuePushAndGetReference" *load-time-value-vector-global-var* (jit-constant-i32 ,index-gs))))
		     (with-landing-pad (irc-get-cleanup-landing-pad-block ,env)
		       ,maker))
		   ;; insert block may have changed in maker
		   (setq *current-load-time-value-insert-block* (irc-get-insert-block)))))
	   ;; We are being invoked from COMPILE and not COMPILE-FILE - put arg directly into the 
	   ;; If there is a coalesce-hash-table defined then coalesce the values using it
	   (if ,coalesce-hash-table
	       (progn
		 (break "COMPILE shouldn't coalesce anything - do I really need to combine COMPILE and COMPILE-FILE code?")
		 (setq ,index-gs (gethash ,key-gs ,coalesce-hash-table))
		 (unless ,index-gs
		   (setq ,index-gs (get-next-available-ltv-entry ,arg))
		   (core::hash-table-setf-gethash ,coalesce-hash-table ,key-gs ,index-gs)))
	       (progn
		 (setq ,index-gs (get-next-available-ltv-entry ,arg)))))
       (if ,result
	   (irc-call ,env "copyLoadTimeValue" ,result *load-time-value-vector-global-var* (jit-constant-i32 ,index-gs)))
       ,index-gs
       )))
|#


(defvar *run-time-values* (make-load-time-values 256)
  "Create one instance of a load-time-value data structure to
store values required by COMPILEd code")

(defvar *run-time-value-nil-index* nil)
(defvar *run-time-value-t-index* nil)
(defvar *run-time-value-symbol-coalesce* (make-hash-table :test #'eq))

(defun initialize-special-run-time-values (env)
  "Initialize special load-time-values like nil and t"
  (setq *run-time-value-nil-index* (vector-push-extend nil *run-time-values*))
  (setq *run-time-value-t-index* (vector-push-extend t *run-time-values*))
)

(defun codegen-run-time-value (result obj env)
  (when *generate-compile-file-load-time-values*
    (error "You cannot generate run-time-values from COMPILE-FILE"))
  (cond
    ((null obj) (codegen-run-time-value-null result env))
    ((eq t obj) (codegen-run-time-value-t result env))
    ((symbolp obj) (codegen-run-time-value-symbol result obj env))
    (t (codegen-run-time-value-general result obj env))))












;; ----------------------------------------------------------
;; ----------------------------------------------------------
;; ----------------------------------------------------------
;;
;; The following are only invoked from COMPILE-FILE
;;
;; ----------------------------------------------------------
;; ----------------------------------------------------------




(defparameter *current-load-time-value-insert-block* nil
  "Save the block into which quoted data and load-time-value forms are codegen")




(defun initialize-special-load-time-values (env)
  "Initialize special load-time-values like nil and t"
  (codegen-ltv-nil nil env)
  (codegen-ltv-t   nil env))



(defmacro with-load-time-value-unit ((ltv-init-fn) &rest body)
  "Wraps generation of load-time-values. This is only invoked from COMPILE-FILE and it creates
the ___loadTimeDataInitializer function and sets up everything for the coalescence and
marshaling of compiled quoted data"
  (let ((cleanup-block-gs (gensym "ltv-cleanup-block"))
	(current-insert-block-gs (gensym "ltv-insert-block"))
	(traceid-gs (gensym "traceid"))
	(fn-env-gs (gensym "ltv-fn-env")))
    `(multiple-value-bind (,ltv-init-fn ,fn-env-gs ,cleanup-block-gs ,traceid-gs ,current-insert-block-gs)
	 (irc-function-create "___loadTimeDataInitializer" nil nil)
       (let ((*generate-compile-file-load-time-values* t)
	     (*load-time-initializer-environment* ,fn-env-gs)
	     (*current-load-time-value-insert-block* ,current-insert-block-gs)
	     (*load-time-value-vector-global-var* (llvm-sys:make-global-variable *the-module*
									  +ltvsp*+
									  nil
									  'llvm-sys:private-linkage
									  (llvm-sys:constant-pointer-null-get +ltvsp*+)
									  "load-time-value-vector"))
	     (*next-load-time-value-index* 0)
	     (*fixnum-coalesce* (make-hash-table :test #'eql))
	     (*bignum-coalesce* (make-hash-table :test #'eql))
	     (*symbol-coalesce* (make-hash-table :test #'eq))
	     (*short-float-coalesce* (make-hash-table :test #'eql))
	     (*single-float-coalesce* (make-hash-table :test #'eql))
	     (*double-float-coalesce* (make-hash-table :test #'eql))
	     (*long-float-coalesce* (make-hash-table :test #'eql))
	     (*string-coalesce* (make-hash-table :test #'equal))
	     (*character-coalesce* (make-hash-table :test #'eql))
	     (*nil-coalesce* (make-hash-table :test #'eq))
	     (*t-coalesce* (make-hash-table :test #'eq))
	     )
	 (initialize-special-load-time-values ,fn-env-gs)
	 ,@body
	 (with-setup-insert-point ,fn-env-gs
	   (irc-call ,fn-env-gs "getLoadTimeValueArray" *load-time-value-vector-global-var* *gv-source-path-name* (jit-constant-i32 *next-load-time-value-index*)))
	 (irc-set-insert-point *current-load-time-value-insert-block*))
       (let ((*gv-current-function-name* (jit-make-global-string-ptr (llvm-sys:get-name ,ltv-init-fn) "fn-name")))
	 (with-landing-pad (irc-get-terminate-landing-pad-block ,fn-env-gs)
	   (irc-function-cleanup-and-return ,fn-env-gs ,traceid-gs))))
    ))



;; ----------------------------------------------------------
;; ----------------------------------------------------------
;; ----------------------------------------------------------
;;
;; The following are invoked from both COMPILE-FILE and COMPILE
;;
;; ----------------------------------------------------------
;; ----------------------------------------------------------


(defun codegen-literal (result obj env)
  (if *generate-compile-file-load-time-values*
      (cond
	((null obj) (codegen-ltv-nil result env))
	((integerp obj) (codegen-ltv-integer result obj env))
	((stringp obj) (codegen-ltv-string result obj env))
	((floatp obj) (codegen-ltv-float result obj env))
	((symbolp obj) (codegen-ltv-symbol result obj env))
	((characterp obj) (codegen-ltv-character result obj env))
	((arrayp obj) (codegen-array result obj env))
;;	((source-code-cons-p obj) (codegen-ltv-source-code-cons result obj env))
	((consp obj) (codegen-ltv-container result obj env))
	((hash-table-p obj) (codegen-ltv-container result obj env))
	(t (error "Add support to codegen the atom type ~a - value: ~a" (class-name obj) obj )))
      ;; Below is how we compile atoms for COMPILE - literal objects are passed into the
      ;; default module without coalescence.
      (codegen-run-time-value result obj env)))


(defun codegen-quote (result rest env)
  (codegen-literal result (car rest) env))



(defun compile-reference-to-literal (literal env)
  (let ((ltv-idx (codegen-literal nil literal env)))
    (unless (fixnump ltv-idx) (error "Could not compile-reference-to-literal: ~a" literal))
    (unless *load-time-value-vector-global-var* (error "There must be a *load-time-value-vector-global-var* defined"))
    (irc-call env "loadTimeValueReference" *load-time-value-vector-global-var* (jit-constant-i32 ltv-idx))))



