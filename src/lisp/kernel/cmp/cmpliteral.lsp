(in-package :cmp)

;;;; implementation of load-time-value and literal logic in compile-file and compile
;;;;
;;;; Conceptually, there are two separate components, the "compiler" and this ("LTV").
;;;; The compiler compiles common lisp forms. Whenever it runs into a literal it can't
;;;;  handle, it calls LTV. When LTV needs some code compiled, it calls the compiler.
;;;; LTV forms code in the @RUN-ALL function to create, at load time,
;;;; a table (vector) with all the values the rest of the code needs to run.
;;;; Once it's done compiling, the compiler checks with LTV to get a table size and
;;;;  the table-initializing code to run, which is injected into the start of
;;;;  @RUN-ALL function.
;;;;
;;;; External protocol:
;;;;
;;;; * compile-file or with-compilation-unit or whatever wraps what it does in WITH-LTV.
;;;; * When the compiler runs into a literal, it calls REFERENCE-LITERAL. This will
;;;;   return, an index into the load-time-values table for run-time.
;;;;   Indices 0 and 1 are special - they represent NIL and T respectively.
;;;;    If these are returned the caller could opt to use other means to get NIL and T
;;;; * When the compiler runs into load-time-value, it calls REFERENCE-LOAD-TIME-VALUE,  *****WRONG????
;;;;   which also returns an index into the load-time-values table that will etc.
;;;; * The compiler provides a function COMPILE-FORM that LTV can call. COMPILE-FORM
;;;;   receives a lisp form, compiles it, and arranges for it to be put into the FASL
;;;;   just like any function compile-file runs into. COMPILE-FORM returns some kind of
;;;;   handle that can be used in the run-all code.
;;;; * Code that will be put into the LTV initialization is added by LTV via
;;;;   ADD-TO-RUN-ALL. This is not lisp code, but rather a restricted language:
;;;;
;;;; * (SET-LTV index form) evaluates form and sets entry number INDEX in the LTV table
;;;;   to the value returned.
;;;; * (CALL handle) calls the function denoted by HANDLE (returned from COMPILE-FORM)


(defvar *load-time-value-result* nil
  "Temporary storage for results of evaluating top-level-forms")

;;; Contains the current RUN-ALL, initialization function
;;; for the current module
(defvar *load-time-value-initialization-function*)
(defvar +run-and-load-time-value-holder-global-var-type+ +ltv*+) ;; Was +ltvsp*+
(defvar *run-time-values-table-name* "run_time_values_table")
(defvar *load-time-initializer-environment*)
;;;------
;;; Set up the run-time-values-table
;;;  set-run-time-values-table MUST be called to set the
;;;  global
(defvar *run-time-values-table* (load-time-value-array *run-time-values-table-name* 0))
(core:set-run-time-values-table *run-time-values-table-name*)

(defvar *load-time-value-holder-global-var* nil
  "Store the current load-time-value data structure for COMPILE-FILE")

(defvar *run-time-values-table-global-var* nil
  "All load-time-values and quoted values are stored in this array accessed with an integer index"
  )

(defvar *generate-compile-file-load-time-values* nil
  "This variable controls whether literals are compiled into the
load-time-value manager (true - in COMPILE-FILE) or not (false - in COMPILE)."
)

;;; ------------------------------------------------------------
;;;
;;;
;;;

(defvar *table-index*)

(defmacro with-load-time-value-counters ((ltv-value-counter &key prologue-code ) &rest body)
  "Wrap code that modifies *table-index* and then
evaluate prologue-code with the final values."
  `(let (,ltv-value-counter
	 (*table-index* 0))
     ,@body
     (setf ,ltv-value-counter *table-index*)
     ,prologue-code))

;;; ------------------------------------------------------------
;;;
;;;

(defvar *everything-else-coalesce*)
(defvar *ratio-coalesce*)
(defvar *cons-coalesce*)
(defvar *complex-coalesce*)
(defvar *array-coalesce*)
(defvar *hash-table-coalesce*)
(defvar *fixnum-coalesce*)
(defvar *bignum-coalesce*)
(defvar *symbol-coalesce*)
(defvar *character-coalesce*)
(defvar *string-coalesce*)
(defvar *pathname-coalesce*)
(defvar *package-coalesce*)
(defvar *built-in-class-coalesce*)
(defvar *single-float-coalesce*)
(defvar *double-float-coalesce*)

(defun new-table-index ()
  "Return the next ltv-index. If this is being invoked from COMPILE then
the value is put into *default-load-time-value-vector* and its index is returned"
  (prog1 *table-index*
    (incf *table-index*)))

(defmacro with-ltv-function-codegen ((result env) &rest form)
  "Generate code within the ltv-function - used by codegen-load-time-value"
  `(let ((*irbuilder-function-alloca* *irbuilder-ltv-function-alloca*)
	 (*current-function* *load-time-value-initialization-function*)
	 (,result *load-time-value-result*)
	 (,env *load-time-initializer-environment*))
     (with-irbuilder (*irbuilder-ltv-function-body*)
       (with-landing-pad (irc-get-cleanup-landing-pad-block *load-time-initializer-environment*)
         ,@form))))

(defmacro with-add-init ((ltv) &rest code)
  "A reference to the ltv entry at (index) will be created
and put into ltv-ref."
  `(let ((,ltv *load-time-value-holder-global-var*))
     (or ,ltv (error "*load-time-value-holder-global-var* is nil"))
     (with-irbuilder (*irbuilder-ltv-function-body*)
       ;;	 (core::hash-table-setf-gethash ,coalesce-hash-table ,key-gs ,index)
       (irc-low-level-trace)
       (with-landing-pad (irc-get-cleanup-landing-pad-block *load-time-initializer-environment*)
         ,@code))))

(defun add-call-args (name args)
  "Call the named function after converting fixnum args to llvm constants"
  (let ((fixed-args (mapcar (lambda (x) (if (fixnump x) (jit-constant-size_t x) x))
                            args)))
    (irc-create-call name fixed-args)))

(defun add-call (name ltv index &rest args)
  "Call the named function after converting fixnum args to llvm constants"
  (let ((fixed-args (mapcar (lambda (x) (if (fixnump x) (jit-constant-size_t x) x))
                            (list* index args))))
    (irc-create-call name (list* ltv fixed-args))))


(defun ltv/nil (object index read-only-p)
  (with-add-init (ltv)
    (add-call "ltvc_make_nil" ltv index)))

(defun ltv/t (object index read-only-p)
  (with-add-init (ltv)
    (add-call "ltvc_make_t" ltv index)))

(defun ltv/ratio (ratio index read-only-p)
  (with-add-init (ltv)
    (add-call "ltvc_make_ratio" ltv index
              (load-time-reference-literal (numerator ratio) read-only-p)
              (load-time-reference-literal (denomenator ratio) read-only-p))))

(defun ltv/cons (cons index read-only-p)
  (with-add-init (ltv)
    (add-call "ltvc_make_cons" ltv index)
    (add-call "ltvc_cons_fill" ltv index
              (load-time-reference-literal (car cons) read-only-p)
              (load-time-reference-literal (cdr cons) read-only-p))))

(defun ltv/complex (complex index read-only-p)
  (with-add-init (ltv)
    (add-call "ltvc_make_complex" ltv index
              (load-time-reference-literal (realpart complex) read-only-p)
              (load-time-reference-literal (imagpart complex) read-only-p))))

(defun ltv/array (array index read-only-p)
  (with-add-init (ltv)
    (add-call "ltvc_make_array" ltv index
              (load-time-reference-literal (array-element-type array) read-only-p)
              (load-time-reference-literal (array-dimensions array) read-only-p))
    (let* ((total-size (if (array-has-fill-pointer-p array)
                           (length array)
                           (array-total-size array))))
      (dotimes (i total-size)
        (add-call "ltvc_setf_row_major_aref" ltv index i
                  (load-time-reference-literal (row-major-aref array i) read-only-p))))))

(defun ltv/hash-table (hash-table index read-only-p)
  (with-add-init (ltv)
    (add-call-args "ltvc_make_hash_table"
                   (list ltv index (load-time-reference-literal (hash-table-test hash-table) read-only-p)))
    (maphash (lambda (key val)
               (add-call "ltvc_setf_gethash" ltv index
                         (load-time-reference-literal key read-only-p)
                         (load-time-reference-literal val read-only-p)))
             hash-table)))

(defun ltv/fixnum (fixnum index read-only-p)
  (with-add-init (ltv)
    (add-call "ltvc_make_fixnum" ltv index (jit-constant-i64 fixnum))))

(defun ltv/bignum (bignum index read-only-p)
  (let ((bn-str (format nil "~a" bignum)))
    (with-add-init (ltv)
      (add-call "ltvc_make_bignum" ltv index (load-time-reference-literal bn-str read-only-p)))))

(defun ltv/symbol (symbol index read-only-p)
  (let ((pkg (symbol-package symbol))
        (sym-str (symbol-name symbol)))
    (with-add-init (ltv)
      (add-call "ltvc_make_symbol" ltv index
                (load-time-reference-literal sym-str read-only-p)
                (load-time-reference-literal pkg read-only-p)))))

(defun ltv/character (char index read-only-p)
  (with-add-init (ltv)
    (add-call "ltvc_make_character" ltv index
              (jit-constant-i64 (char-code char)))))

(defun ltv/base-string (str index read-only-p)
  (with-add-init (ltv)
    (let* ((constant (llvm-sys:make-string-global *the-module* str (bformat nil "::str[%s]" str)))
           (ptr (llvm-sys:create-in-bounds-gep
                 *irbuilder* constant
                 (list (jit-constant-i32 0) (jit-constant-i32 0)) "ptr")))
      (add-call "ltvc_make_base_string" ltv index ptr)
      #+(or)(add-call-args "ltvc_make_base_string" (list ltv index ptr) str))))

(defun ltv/pathname (pathname index read-only-p)
  (with-add-init (ltv)
    (add-call "ltvc_make_pathname" ltv index
              (load-time-reference-literal (pathname-host pathname) read-only-p)
              (load-time-reference-literal (pathname-device pathname) read-only-p)
              (load-time-reference-literal (pathname-directory pathname) read-only-p)
              (load-time-reference-literal (pathname-name pathname) read-only-p)
              (load-time-reference-literal (pathname-type pathname) read-only-p)
              (load-time-reference-literal (pathname-version pathname) read-only-p))))

(defun ltv/package (package index read-only-p)
  (with-add-init (ltv)
    (add-call "ltvc_make_package" ltv index
              (load-time-reference-literal (package-name package) read-only-p))))

(defun ltv/built-in-class (class index read-only-p)
  (with-add-init (ltv)
    (add-call "ltvc_class" ltv index
              (load-time-reference-literal (class-name class) read-only-p))))

(defun ltv/single-float (single index read-only-p)
  (let* ((constant (llvm-sys:make-apfloat-float single))
         (constant-ap-arg (llvm-sys:constant-fp-get *llvm-context* constant)))
    (with-add-init (ltv)
      (add-call "ltvc_make_float" ltv index constant-ap-arg))))

(defun ltv/double-float (double index read-only-p)
  (let* ((constant (llvm-sys:make-apfloat-double double))
         (constant-ap-arg (llvm-sys:constant-fp-get *llvm-context* constant)))
    (with-add-init (ltv)
      (add-call "ltvc_make_double" ltv index constant-ap-arg))))

(defun ltv/mlf (object index read-only-p)
  (multiple-value-bind (create initialize)
      (make-load-form object)
    (with-add-init (ltv)
      (add-call "ltvc_set_ltv_funcall" ltv index (compile-form create))
      (when initialize
        (add-call-args "ltvc_funcall" (list (compile-form initialize)))))))

(defun object-similarity-table-and-creator (object)
  (cond
    ((null object) (values *everything-else-coalesce* #'ltv/nil))
    ((eq t object) (values *everything-else-coalesce* #'ltv/t))
    ((consp object) (values *cons-coalesce* #'ltv/cons))
    ((fixnump object) (values *fixnum-coalesce* #'ltv/fixnum))
    ((characterp object) (values *character-coalesce* #'ltv/character))
    ((core:single-float-p  object) (values *single-float-coalesce* #'ltv/single-float))
    ((symbolp object) (values *symbol-coalesce* #'ltv/symbol))
    ((double-float-p object) (values *double-float-coalesce* #'ltv/double-float))
    ((core:ratio-p object) (values *ratio-coalesce* #'ltv/ratio))
    ((stringp  object) (values *string-coalesce* #'ltv/base-string))
    ((arrayp object) (values *array-coalesce* #'ltv/array))
    ((hash-table-p object) (values *hash-table-coalesce* #'ltv/hash-table))
    ((bignump object) (values *bignum-coalesce* #'ltv/bignum))
    ((pathnamep object) (values *pathname-coalesce* #'ltv/pathname))
    ((packagep object) (values *package-coalesce* #'ltv/package))
    ((complexp object) (values *complex-coalesce* #'ltv/complex))
    ((core:built-in-class-p object) (values *built-in-class-coalesce* #'ltv/built-in-class))
    ((typep object '(or standard-object structure-object condition class)) (values *everything-else-coalesce* #'ltv/mlf))
    (t (error "Handle object ~a" object))))

(defun make-similarity-table (test)
  (make-hash-table :test test))

(defun find-similar (object table)
  (gethash object table))

(defun add-similar (object index table)
  (setf (gethash object table) index))

;;; with-compile-file-dynamic-variables-and-load-time-value-unit (ltv-init-fn)

(defmacro with-ltv ((ltv-init-fn) &body body)
  (let ((cleanup-block-gs (gensym "ltv-cleanup-block"))
	(irbuilder-alloca (gensym "ltv-irbuilder-alloca"))
	(irbuilder-body (gensym "ltv-irbuilder-body"))
	(traceid-gs (gensym "traceid"))
	(fn-env-gs (gensym "ltv-fn-env"))
        (result (gensym "result")))
    `(multiple-value-bind (,ltv-init-fn ,fn-env-gs ,cleanup-block-gs
					,irbuilder-alloca ,irbuilder-body ,result )
	 (irc-function-create core:+run-all-function-name+
                              nil nil
			      :function-type +fn-prototype+
			      :argument-names +fn-prototype-argument-names+)
       (let* ((*load-time-value-initialization-function* ,ltv-init-fn)
              (*current-function* ,ltv-init-fn)
              (*generate-compile-file-load-time-values* t)
              (*load-time-initializer-environment* ,fn-env-gs)
              (*irbuilder-ltv-function-alloca* ,irbuilder-alloca)
              (*irbuilder-ltv-function-body* ,irbuilder-body))
         (with-dbg-function ("runAll-dummy-name"
                                 :linkage-name (llvm-sys:get-name ,ltv-init-fn)
                                 :function ,ltv-init-fn
                                 :function-type +fn-prototype+
                                 :form nil) ;; No form for run-all
           ;; Set up dummy debug info for these irbuilders
           (with-irbuilder (*irbuilder-ltv-function-alloca*)
             (dbg-set-current-source-pos nil))
           (with-irbuilder (*irbuilder-ltv-function-body*)
             (dbg-set-current-source-pos nil))
           (let ((*table-index* 0)
                 (*load-time-value-result* (irc-alloca-tmv *load-time-initializer-environment*
                                                           :irbuilder *irbuilder-ltv-function-alloca*))
                 (*load-time-value-holder-global-var*
                  #|| spacer ||#(llvm-sys:make-global-variable 
                                 *the-module*
                                 +run-and-load-time-value-holder-global-var-type+
                                 nil
                                 'llvm-sys:internal-linkage
                                 (llvm-sys:constant-pointer-null-get 
                                  +run-and-load-time-value-holder-global-var-type+)
                                 *load-time-value-holder-name*))
                 (*everything-else-coalesce* (make-hash-table :test #'eq))
                 (*ratio-coalesce* (make-similarity-table #'eql))
                 (*cons-coalesce* (make-similarity-table #'eq))
                 (*complex-coalesce* (make-similarity-table #'eql))
                 (*array-coalesce* (make-similarity-table #'eq))
                 (*hash-table-coalesce* (make-similarity-table #'eq))
                 (*fixnum-coalesce* (make-similarity-table #'eql))
                 (*bignum-coalesce* (make-similarity-table #'eql))
                 (*symbol-coalesce* (make-similarity-table #'eq))
                 (*character-coalesce* (make-similarity-table #'eql))
                 (*string-coalesce* (make-similarity-table #'equal))
                 (*pathname-coalesce* (make-similarity-table #'equal))
                 (*package-coalesce* (make-similarity-table #'eq))
                 (*built-in-class-coalesce* (make-similarity-table #'eq))
                 (*single-float-coalesce* (make-similarity-table #'eql))
                 (*double-float-coalesce* (make-similarity-table #'eql)))
             (with-load-time-value-counters
                 (ltv-value-counter
                  :prologue-code
                  (with-irbuilder (*irbuilder-ltv-function-alloca*)
                    (cmp-log "Setting up getOrCreateLoad-TimeValueArray\n")
                    (irc-intrinsic "ltvc_get_or_create_load_time_value_array"
                                   *load-time-value-holder-global-var*
                                   *gv-source-namestring*
                                   (jit-constant-size_t ltv-value-counter))
                    (irc-intrinsic "ltvc_assign_source_file_info_handle"
                                   *gv-source-namestring*
                                   *gv-source-debug-namestring*
                                   (jit-constant-i64 *source-debug-offset*)
                                   (jit-constant-i32 (if *source-debug-use-lineno* 1 0))
                                   *gv-source-file-info-handle*)))
               (progn
                 (load-time-reference-literal nil t)
                 (load-time-reference-literal t t))
               ,@body)
             (with-irbuilder (*irbuilder-ltv-function-body*)
               (let ((*gv-current-function-name* (jit-make-global-string-ptr
                                                  (llvm-sys:get-name ,ltv-init-fn) "fn-name")))
                 (with-landing-pad (irc-get-terminate-landing-pad-block ,fn-env-gs)
                   (irc-function-cleanup-and-return ,fn-env-gs ,result ))))))))))

(defun load-time-reference-literal (object read-only-p)
  (multiple-value-bind (similarity creator)
      (object-similarity-table-and-creator object)
    (if read-only-p
        (let* ((existing (find-similar object similarity)))
          (or existing
              (let ((index (new-table-index)))
                (add-similar object index similarity)
                (funcall creator object index read-only-p)
                index)))
        (let ((index (new-table-index)))
          (funcall creator object index read-only-p)
          index))))

#+(or)
(defun load-time-reference-literal (object read-only-p)
  (multiple-value-bind (similarity creator)
      (object-similarity-table-and-creator object)
    (let* ((existing (find-similar object similarity)))
      (or existing
          (let ((index (new-table-index)))
            (add-similar object index similarity)
            (funcall creator object index read-only-p)
            index)))))

(defun reference-evaluated-function (fn)
  (let ((index (new-table-index)))
    (with-add-init (ltv)
      (add-call "ltvc_set_ltv_funcall" ltv index fn))
    index))


(defun pretty-load-time-name (object ltv-idx)
  (cond
    ((symbolp object) (bformat nil "SYMBOL->%s" object))
    ((consp object) "CONS")
    ((arrayp object) "ARRAY")
    ((numberp object) (format nil "NUMBER->~a" object))
    (t (subseq (bformat nil "ltv-idx_%d_val->%s" ltv-idx object) 0 30))))


;;;----------------------------------------------------------------------
;;;
;;; run time values
;;;

(defvar *run-time-coalesce* (make-hash-table :test #'eq))

(defvar *next-load-time-symbol-index*  nil
  "Each load-time-symbol gets assigned a unique integer index
and the next one is stored here")


(defun new-run-time-table-index ()
  "Return the next ltv-index. If this is being invoked from COMPILE then
the value is put into *default-load-time-value-vector* and its index is returned"
  (data-vector-push-extend *run-time-values-table* nil 16))

(defun add-run-time-object (object index)
  (load-time-value-array-setf *run-time-values-table* index object))

(defun run-time-reference-literal (object read-only-p)
  (if read-only-p
      (let* ((similarity *run-time-coalesce*)
             (existing (find-similar object similarity)))
        (or existing
            (let ((index (new-run-time-table-index)))
              (add-similar object index similarity)
              (add-run-time-object object index)
              index)))
      (let ((index (new-run-time-table-index)))
        (add-run-time-object object index)
        index)))

#+(or)
(defun run-time-reference-literal (object read-only-p)
  (let* ((similarity *run-time-coalesce*)
         (existing (find-similar object similarity)))
    (or existing
        (let ((index (new-run-time-table-index)))
          (add-similar object index similarity)
          (add-run-time-object object index)
          index))))









;;; ------------------------------------------------------------
;;;
;;; compile-form
;;;
;;; Compile the form and return a 0-arity function that
;;; returns a result.
;;;


(defun bclasp-compile-form (form)
  (dbg-set-current-debug-location-here)
  (let ((fn (with-new-function (fn fn-env fn-result
                                   :function-name 'bclasp-top-level-form
                                   :parent-env nil
                                   :function-form form)
              (let* ((given-name (llvm-sys:get-name fn)))
                ;; Map the function argument names
                (cmp-log "Creating ltv thunk with name: %s\n" given-name)
                (dbg-set-current-debug-location-here)
                (codegen fn-result form fn-env)
                (dbg-set-current-debug-location-here)))))
    (cmp-log-dump fn)
    (irc-verify-function fn t)
    fn))

(defun compile-form (form)
  (if core:*use-cleavir-compiler*
      (funcall (find-symbol "COMPILE-FORM" "CLASP-CLEAVIR") form)
      (bclasp-compile-form form)))



;;; ------------------------------------------------------------
;;; ------------------------------------------------------------
;;; ------------------------------------------------------------
;;;
;;; reference-literal
;;;
;;; Returns an index for the object for both COMPILE-FILE and COMPILE
;;; ------------------------------------------------------------
;;; ------------------------------------------------------------
;;; ------------------------------------------------------------

(defun reference-literal (object &optional read-only-p)
  (if *generate-compile-file-load-time-values*
      (load-time-reference-literal object read-only-p)
      (run-time-reference-literal object read-only-p)))


;;; ------------------------------------------------------------
;;;
;;; functions that are called by bclasp and cclasp that might
;;;  be refactored to simplify the API

(defun compile-reference-to-symbol (symbol)
  "Generate a reference to a load-time-symbol or 
run-time-symbol depending if called from COMPILE-FILE or COMPILE respectively"
  (let ((lts-idx (reference-literal symbol t)))
    (if *generate-compile-file-load-time-values*
	(progn
	  (unless *load-time-value-holder-global-var* (error "There must be a *load-time-value-holder-global-var* defined"))
	  (load-time-value-reference *load-time-value-holder-global-var* lts-idx (pretty-load-time-name symbol lts-idx)))
	(progn
	  (load-time-value-reference *run-time-values-table-global-var* lts-idx (pretty-load-time-name symbol lts-idx))))))

#+(or)
(defun codegen-symbol (result obj &optional (env *load-time-initializer-environment*))
  "cclasp calls this.  Generate a load-time-symbol or run-time-symbol depending if called from COMPILE-FILE or COMPILE respectively"
  (or (symbolp obj) (error "obj must be a symbol - instead it is: ~A" obj))
  (if *generate-compile-file-load-time-values*
      (codegen-lts/symbol result obj env)
  ;; Below is how we compile atoms for COMPILE - literal objects are passed into the
  ;; default module without coalescence.
      (codegen-rts/symbol result obj env)))


(defun codegen-rtv (result sym env)
  "bclasp calls this to get copy the run-time-value for sym into result"
  (let ((idx (run-time-reference-literal sym t)))
    (copy-load-time-value result *run-time-values-table-global-var* idx)
    idx))

(defun codegen-literal (result object env)
  "This is called by bclasp.  If result is nil then just return the ltv index.
If it isn't NIL then copy the literal from its index in the LTV into result."
  (let ((index (reference-literal object t)))
    (when result
      (if *generate-compile-file-load-time-values*
          (progn
            (unless *load-time-value-holder-global-var* (error "There must be a *load-time-value-holder-global-var* defined"))
            (copy-load-time-value result *load-time-value-holder-global-var* index))
          (progn
            (copy-load-time-value result *run-time-values-table-global-var* index))))
    index))
        
(defun codegen-quote (result rest env)
  (cmp-log "codegen-quote: %s\n" rest )
  (codegen-literal result (car rest) env))

(defun compile-reference-to-load-time-value (idx &optional (name "value"))
  (load-time-value-reference
	    (if *generate-compile-file-load-time-values*
		*load-time-value-holder-global-var*
		*run-time-values-table-global-var*)
	    idx name))

(defun compile-reference-to-literal (literal)
  "Generate a reference to a load-time-value or run-time-value literal depending if called from COMPILE-FILE or COMPILE respectively"
  (let ((ltv-idx (reference-literal literal t)))
    (unless (fixnump ltv-idx) (error "Could not compile-reference-to-literal: ~a" literal))
    (compile-reference-to-load-time-value ltv-idx (pretty-load-time-name literal ltv-idx))))

(defun ltv-global ()
  "called by cclasp"
  (if *generate-compile-file-load-time-values*
      *load-time-value-holder-global-var*
      *run-time-values-table-global-var*))




;; Should be bclasp-compile-ltv-thunk
(defun compile-ltv-thunk (form)
  "bclasp compile the form into an llvm function and return that function"
  (dbg-set-current-debug-location-here)
  (let* ((ltv-index (new-table-index))
	 (fn (with-new-function (fn fn-env fn-result
				    :function-name 'bclasp-top-level-form
				    :parent-env nil
				    :function-form form)
	       (let* ((given-name (llvm-sys:get-name fn)))
		 ;; Map the function argument names
		 (cmp-log "Creating ltv thunk with name: %s\n" given-name)
		 (let ((ltv-result (load-time-value-reference
                                                  *load-time-value-holder-global-var*
                                                  ltv-index)))
                   ;;		   (break "codegen ltv thunk form")
		   (dbg-set-current-debug-location-here)
		   (codegen ltv-result form fn-env)
		   (irc-intrinsic "copyTsp" fn-result ltv-result)
		   (dbg-set-current-debug-location-here))))))
    (cmp-log-dump fn)
    (irc-verify-function fn t)
    (values ltv-index fn)))



;;; ------------------------------------------------------------
;;;
;;; Access load-time-values
;;;

(defun load-time-value-reference (holder index &optional (label "ltv"))
  #+(or)(let* ((tagged-ltv-ptr (irc-load holder "tagged-ltv-ptr"))
               (tagged-ltv-intptr_t (irc-ptr-to-int tagged-ltv-ptr +intptr_t+ "tagged-ltv-intptr_t"))
               (general-pointer-tag (cdr (assoc :general-tag cmp::+cxx-data-structures-info+)))
               (ltvo-address (llvm-sys:create-add *irbuilder* tagged-ltv-intptr_t (jit-constant-uintptr_t (- general-pointer-tag)) "ltvo_address"))
               (ltvo-objects-offset (cdr (assoc :load-time-values-objects-offset cmp::+cxx-data-structures-info+)))
               (ltvo-objects-address (llvm-sys:create-add *irbuilder* ltvo-address (jit-constant-uintptr_t ltvo-objects-offset) "ltvo_objects_address"))
               (tagged-ltvo-objects-smart-ptr (irc-load (irc-int-to-ptr ltvo-objects-address +tsp*+) "tagged-ltvo-objects-ptr"))
               (tagged-ltvo-objects-ptr (irc-smart-ptr-extract tagged-ltvo-objects-smart-ptr "tagged-ltvo-objects-ptr"))
               (tagged-ltvo-objects-addr (irc-ptr-to-int tagged-ltvo-objects-ptr +uintptr_t+ "tagged-ltvo-objects-addr"))
               (ltvo-objects-addr (irc-add tagged-ltvo-objects-addr (jit-constant-uintptr_t (- general-pointer-tag)) "ltvo-objects-addr"))
               (data0-offset (cdr (assoc :gcvector-data0-offset cmp::+cxx-data-structures-info+)))
               (element-size (cdr (assoc 'core:tsp cmp::+cxx-data-structures-info+)))
               (offset (+ data0-offset (* element-size index)))
               (entry-uintptr_t (irc-add ltvo-objects-addr (jit-constant-uintptr_t offset) "entry-uintptr_t"))
               (entry-ptr (irc-int-to-ptr entry-uintptr_t +tsp*+ (bformat nil "entry[%d]-ptr" index))))
          #+(or)(let ((orig (irc-intrinsic "loadTimeValueReference" holder (jit-constant-size_t index) label)))
                  (irc-int-to-ptr (irc-intrinsic "debug_match_two_uintptr_t"
                                                 (irc-ptr-to-int entry-ptr +uintptr_t+)
                                                 (irc-ptr-to-int orig +uintptr_t+))
                                  +tsp*+))
          entry-ptr)
  (irc-intrinsic "loadTimeValueReference" holder (jit-constant-size_t index) label))

(defun get-load-time-value (result holder index)
  #+(or)(irc-intrinsic "getLoadTimeValue"
                 result
                 holder
                 (jit-constant-i32 index))
  (let ((ref (load-time-value-reference holder index )))
    (irc-store (irc-load ref) result)))

(defun copy-load-time-value (result holder index)
  #+(or)(irc-intrinsic "copyLoadTimeValue" result holder (jit-constant-size_t index))
  (get-load-time-value result holder index)
  )

;;; ------------------------------------------------------------
;;;
;;; Set up nil and t in the run-time array
;;;
(run-time-reference-literal nil t)
(run-time-reference-literal t t)
