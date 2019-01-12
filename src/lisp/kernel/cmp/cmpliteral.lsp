(in-package :literal)

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
;;;; * When the compiler runs into load-time-value, it calls REFERENCE-LOAD-TIME-VALUE,  *****WRONG????
;;;;   which also returns an index into the load-time-values table that will etc.
;;;; * The compiler provides a function COMPILE-CST-OR-FORM that LTV can call. COMPILE-FORM
;;;;   receives a lisp form, compiles it, and arranges for it to be put into the FASL
;;;;   just like any function compile-file runs into. COMPILE-CST-OR-FORM returns some kind of
;;;;   handle that can be used in the run-all code.
;;;; * Code that will be put into the LTV initialization is added by LTV via
;;;;   ADD-TO-RUN-ALL. This is not lisp code, but rather a restricted language:
;;;;
;;;; * (SET-LTV index form) evaluates form and sets entry number INDEX in the LTV table
;;;;   to the value returned.
;;;; * (CALL handle) calls the function denoted by HANDLE (returned from COMPILE-CST-OR-FORM)

(defvar *gcroots-in-module*)
#+threads(defvar *value-table-id-lock* (mp:make-lock :name '*value-table-id-lock*))
(defvar *value-table-id* 0)
(defun incf-value-table-id-value ()
  #+threads(unwind-protect
                (progn
                  (mp:lock *value-table-id-lock* t)
                  (incf *value-table-id*))
             (mp:unlock *value-table-id-lock*))
  #-threads (incf *value-table-id*))

(defun next-value-table-holder-name (&optional suffix)
  (if suffix
      (bformat nil "%s-%s%d" suffix core:+contab-name+ (incf-value-table-id-value))
      (bformat nil "%s%d" core:+contab-name+ (incf-value-table-id-value))))

(defstruct (literal-node-toplevel-funcall (:type vector) :named) arguments)
(defstruct (literal-node-creator (:type vector) :named) index name literal-name object arguments)
(defstruct (literal-node-call (:type vector) :named) function source-pos-info holder)
(defstruct (literal-node-side-effect (:type vector) :named) name arguments)
(defstruct (literal-node-runtime (:type vector) :named) index object)
(defstruct (literal-node-closure (:type vector) :named)
   index lambda-name-index function source-info-handle filepos lineno column)

;;; +max-run-all-size+ must be larger than +list-max+ so that
;;;   even a full list will fit into one run-all
(defconstant +max-run-all-size+ (max 200 call-arguments-limit))
;;; +list-max+ must be smaller than call-arguments-limit so that
;;;  no ltvc_make_list doesn't blow the call stack
(defconstant +list-max+ (- call-arguments-limit 8))

(defparameter *run-all-objects* nil)

(defun run-all-add-node (node)
  (push node *run-all-objects*)
  node)

(defun calculate-table-size (nodes)
  "Find the highest index and return 1+ that"
  (let ((highest-index -1))
    (dolist (node nodes)
      #+(or)(bformat t "generate-run-all-code  generating node: %s%N" node)
      (when (literal-node-creator-p node)
        (setf highest-index (max highest-index (literal-node-creator-index node)))))
    (1+ highest-index)))

;;; ------------------------------------------------------------
;;;
;;; Immediate objects don't need to be put into tables
;;;

;;; Return NIL if the object is not immediate - or return
;;;     a fixnum that can be cast directly to a tagged pointer that represents the immediate object.
(defun immediate-object-or-nil (x)
  (let ((immediate (core:create-tagged-immediate-value-or-nil x)))
    (if immediate
        (cmp:irc-maybe-cast-integer-to-t* immediate)
        nil)))

(defvar *table-index*)

;;; ------------------------------------------------------------
;;;
;;;

(defvar *ratio-coalesce*)
;;;(defvar *cons-coalesce*)
(defvar *complex-coalesce*)
;;;(defvar *array-coalesce*)
(defvar *hash-table-coalesce*)
(defvar *bignum-coalesce*)
(defvar *symbol-coalesce*)
(defvar *string-coalesce*)
(defvar *pathname-coalesce*)
(defvar *package-coalesce*)
(defvar *built-in-class-coalesce*)
(defvar *double-float-coalesce*)
(defvar *identity-coalesce*)
(defvar *constant-index-to-literal-node-creator*)
(defvar *llvm-values*)

(defvar *with-ltv-depth* 0)

(defun new-table-index ()
  "Return the next ltv-index. If this is being invoked from COMPILE then
the value is put into *default-load-time-value-vector* and its index is returned"
  (prog1 *table-index* (incf *table-index*)))

(defun lookup-literal-index (object)
  "Given a literal object that has already been added to the literal table and will be recreated at load-time,
return the index in the literal table for that object.  This is used in special cases like defcallback to
rewrite the slot in the literal table to store a closure."
  (maphash (lambda (index literal)
             (when (eq (literal-node-creator-object literal) object)
               (return-from lookup-literal-index index)))
           *constant-index-to-literal-node-creator*)
  (error "Could not find literal ~s" object))

(defun add-named-creator (name index literal-name object &rest args)
  "Call the named function after converting fixnum args to llvm constants"
  (let ((creator (make-literal-node-creator :index index :name name :literal-name literal-name :object object :arguments args)))
    (setf (gethash index *constant-index-to-literal-node-creator*) creator)
    (run-all-add-node creator)
    creator))

(defun add-creator (name index object &rest args)
  "Call the named function after converting fixnum args to llvm constants"
  (apply 'add-named-creator name index nil object args))

(defun add-side-effect-call (name &rest args)
  "Call the named function after converting fixnum args to llvm constants"
  (let ((rase (make-literal-node-side-effect :name name :arguments args)))
    (run-all-add-node rase)
    rase))

(defun ltv/nil (object index read-only-p &key recursive-p)
  (add-named-creator "ltvc_make_nil" index "NIL" object))

(defun ltv/t (object index read-only-p &key recursive-p)
  (add-named-creator "ltvc_make_t" index "T" object))

(defun ltv/ratio (ratio index read-only-p &key recursive-p)
  (add-creator "ltvc_make_ratio" index ratio
               (load-time-reference-literal (numerator ratio) read-only-p :recursive-p t)
               (load-time-reference-literal (denominator ratio) read-only-p :recursive-p t)))

(defun ltv/cons (cons index read-only-p &key recursive-p)
  #+(or)
  (add-creator "ltvc_make_cons" index
               (load-time-reference-literal (car cons) read-only-p)
               (load-time-reference-literal (cdr cons) read-only-p))
  (let ((isproper (core:proper-list-p cons)))
    (cond
      ((and isproper (<= (length cons) +list-max+))
       (apply 'add-creator "ltvc_make_list" index cons
              (length cons) (mapcar (lambda (x)
                                      (load-time-reference-literal x read-only-p :recursive-p t :recursive-p t))
                                    cons)))
      ((null isproper)
       (add-creator "ltvc_make_cons" index cons
                    (load-time-reference-literal (car cons) read-only-p :recursive-p t)
                    (load-time-reference-literal (cdr cons) read-only-p :recursive-p t)))
      ;; Too long list
      (t (let* ((pos +list-max+)
                (front (subseq cons 0 pos))
                (back (nthcdr pos cons)))
           (add-creator "ltvc_nconc" index cons
                        (load-time-reference-literal front read-only-p :recursive-p t)
                        (load-time-reference-literal back read-only-p :recursive-p t)))))))

(defun ltv/complex (complex index read-only-p &key recursive-p)
  (add-creator "ltvc_make_complex" index complex
               (load-time-reference-literal (realpart complex) read-only-p :recursive-p t)
               (load-time-reference-literal (imagpart complex) read-only-p :recursive-p t)))

(defun ltv/array (array index read-only-p &key recursive-p)
  (let ((val (add-creator "ltvc_make_array" index array
                          (load-time-reference-literal (array-element-type array) read-only-p :recursive-p t)
                          (load-time-reference-literal (array-dimensions array) read-only-p :recursive-p t))))
    (let* ((total-size (if (array-has-fill-pointer-p array)
                           (length array)
                           (array-total-size array))))
      (dotimes (i total-size)
        (add-side-effect-call "ltvc_setf_row_major_aref" val i
                              (load-time-reference-literal (row-major-aref array i) read-only-p :recursive-p t))))
    val))

(defun ltv/hash-table (hash-table index read-only-p &key recursive-p)
  (let ((ht (add-creator "ltvc_make_hash_table" index hash-table
                         (load-time-reference-literal (hash-table-test hash-table) read-only-p :recursive-p t))))
    (maphash (lambda (key val)
               (add-side-effect-call "ltvc_setf_gethash" ht
                                     (load-time-reference-literal key read-only-p :recursive-p t)
                                     (load-time-reference-literal val read-only-p :recursive-p t)))
             hash-table)
    ht))

(defun ltv/fixnum (fixnum index read-only-p &key recursive-p)
  (add-creator "ltvc_make_fixnum" index fixnum fixnum))

(defun ltv/bignum (bignum index read-only-p &key recursive-p)
  (let ((bn-str (format nil "~a" bignum)))
    (add-creator "ltvc_make_bignum" index bignum (load-time-reference-literal bn-str read-only-p :recursive-p t))))

(defun ltv/bitvector (bitvector index read-only-p &key recursive-p)
  (let ((sout (make-string-output-stream :element-type 'base-char)))
    (write bitvector :stream sout)
    (let ((bv-str (get-output-stream-string sout)))
      (add-creator "ltvc_make_bitvector" index bitvector (load-time-reference-literal bv-str read-only-p :recursive-p t)))))

(defun ltv/random-state (random-state index read-only-p &key recursive-p)
  (let ((rs-str (format nil "~a" (core:random-state-get random-state))))
    (add-creator "ltvc_make_random_state" index random-state (load-time-reference-literal rs-str read-only-p :recursive-p t))))

(defun ltv/symbol (symbol index read-only-p &key recursive-p)
  (let ((pkg (symbol-package symbol))
        (sym-str (symbol-name symbol)))
    (add-named-creator "ltvc_make_symbol" index sym-str symbol
                       (load-time-reference-literal sym-str read-only-p :recursive-p t)
                       (load-time-reference-literal pkg read-only-p :recursive-p t))))

(defun ltv/character (char index read-only-p &key recursive-p)
  (add-creator "ltvc_make_character" index char
               (cmp:jit-constant-i64 (char-code char))))

(defun ltv/base-string (str index read-only-p &key recursive-p)
  (add-creator "ltvc_make_base_string" index str str))

(defun ltv/pathname (pathname index read-only-p &key recursive-p)
  (add-creator "ltvc_make_pathname" index pathname
               (load-time-reference-literal (pathname-host pathname) read-only-p :recursive-p t)
               (load-time-reference-literal (pathname-device pathname) read-only-p :recursive-p t)
               (load-time-reference-literal (pathname-directory pathname) read-only-p :recursive-p t)
               (load-time-reference-literal (pathname-name pathname) read-only-p :recursive-p t)
               (load-time-reference-literal (pathname-type pathname) read-only-p :recursive-p t)
               (load-time-reference-literal (pathname-version pathname) read-only-p :recursive-p t)))

(defun ltv/package (package index read-only-p &key recursive-p)
  (add-creator "ltvc_make_package" index package
               (load-time-reference-literal (package-name package) read-only-p :recursive-p t)))

(defun ltv/built-in-class (class index read-only-p &key recursive-p)
  (add-creator "ltvc_make_built_in_class" index class
               (load-time-reference-literal (class-name class) read-only-p :recursive-p t)))

(defun ltv/single-float (single index read-only-p &key recursive-p)
  (let* ((constant (llvm-sys:make-apfloat-float single))
         (constant-ap-arg (llvm-sys:constant-fp-get cmp:*llvm-context* constant)))
    (add-creator "ltvc_make_float" index single constant-ap-arg)))

(defun ltv/double-float (double index read-only-p &key recursive-p)
  (let* ((constant (llvm-sys:make-apfloat-double double))
         (constant-ap-arg (llvm-sys:constant-fp-get cmp:*llvm-context* constant)))
    (add-creator "ltvc_make_double" index double constant-ap-arg)))

;;; Should match the return value of make-load-form-saving-slots (in clos/print.lsp)
(defun allocate-instance-form-p (form)
  (and (consp form)
       (eq (car form) 'allocate-instance)
       (consp (cdr form))
       ;; Slightly goofy way of saying "is constant" without
       ;; bothering about an environment.
       ;; As of now, m-l-f-s-s involves a literal class.
       (not (symbolp (cadr form)))
       (not (consp (cadr form)))
       (null (cddr form))))

(defun ltv/mlf (object index read-only-p &key recursive-p)
  (multiple-value-bind (create initialize)
      (make-load-form object)
    (prog1
        (cond
          ((allocate-instance-form-p create)
           (add-creator "ltvc_allocate_instance" index object
                        (load-time-reference-literal (second create) t :recursive-p t)))
          (t (let* ((fn (compile-form create))
                    (name (cmp:jit-constant-unique-string-ptr (llvm-sys:get-name fn))))
               (add-creator "ltvc_set_mlf_creator_funcall" index object fn name))))
      (when initialize
        (let* ((fn (compile-form initialize))
               (name (cmp:jit-constant-unique-string-ptr (llvm-sys:get-name fn))))
          (add-side-effect-call "ltvc_mlf_init_funcall" fn name))))))

(defun object-similarity-table-and-creator (object read-only-p)
  (cond
    ((null object) (values *identity-coalesce* #'ltv/nil))
    ((eq t object) (values *identity-coalesce* #'ltv/t))
    ((consp object) (values nil #+(or)*cons-coalesce* #'ltv/cons))
    ((fixnump object) (values nil #'ltv/fixnum))
    ((characterp object) (values nil #'ltv/character))
    ((core:single-float-p  object) (values nil #'ltv/single-float))
    ((symbolp object) (values *symbol-coalesce* #'ltv/symbol))
    ((double-float-p object) (values *double-float-coalesce* #'ltv/double-float))
    ((core:ratio-p object) (values *ratio-coalesce* #'ltv/ratio))
    ((bit-vector-p object) (values nil #'ltv/bitvector))
    ((stringp  object) (values (if read-only-p *identity-coalesce* *string-coalesce*) #'ltv/base-string))
    ((arrayp object) (values nil #+(or)*array-coalesce* #'ltv/array))
    ((hash-table-p object) (values *hash-table-coalesce* #'ltv/hash-table))
    ((bignump object) (values *bignum-coalesce* #'ltv/bignum))
    ((pathnamep object) (values *pathname-coalesce* #'ltv/pathname))
    ((packagep object) (values *package-coalesce* #'ltv/package))
    ((complexp object) (values *complex-coalesce* #'ltv/complex))
    ((random-state-p object) (values *identity-coalesce* #'ltv/random-state))
    ((core:built-in-class-p object) (values *built-in-class-coalesce* #'ltv/built-in-class))
    (t (values *identity-coalesce* #'ltv/mlf))))

(defun make-similarity-table (test)
  (make-hash-table :test test))

(defun find-similar (object table)
  (gethash object table))

(defun add-similar (object index table)
  (setf (gethash object table) index))

(defun estimate-run-all-size (nodes)
  (let ((estimate 0))
    (dolist (node nodes)
      (cond ((and (literal-node-creator-p node)
                  (string= "ltvc_make_list" (literal-node-creator-name node)))
             (incf estimate (length (literal-node-creator-arguments node))))
            (t (incf estimate))))
    estimate))

(defun ensure-creator-llvm-value (obj)
  "Lookup or create the llvm::Value for obj"
  (or (gethash obj *llvm-values*)
      (setf (gethash obj *llvm-values*)
            (cmp:irc-intrinsic-call (literal-node-creator-name obj)
                                    (list*
                                     *gcroots-in-module*
                                     (cmp:jit-constant-size_t (literal-node-creator-index obj))
                                     (fix-args (literal-node-creator-arguments obj)))))))
(defun lookup-arg (creator)
  (ensure-creator-llvm-value creator)
  (let* ((idx (literal-node-creator-index creator))
         (label (if (literal-node-creator-literal-name creator)
                    (bformat nil "%s[%d]/%s" core:+contab-name+ idx (literal-node-creator-literal-name creator))
                    (bformat nil "%s[%d]%t*" core:+contab-name+ idx)))
         (entry (llvm-sys:create-geparray cmp:*irbuilder*
                                          cmp:*load-time-value-holder-global-var*
                                          (list (cmp:jit-constant-i32 0)
                                                (cmp:jit-constant-i32 idx))
                                          label))
         (arg (cmp:irc-load entry label)))
    arg))

(defun fix-arg (arg)
  (cond
    ((fixnump arg) (cmp:jit-constant-i64 arg))
    ((stringp arg) (cmp:jit-constant-unique-string-ptr arg))
    ((literal-node-creator-p arg) (lookup-arg arg))
    (t arg)))

(defun fix-args (args)
  "Convert the args from Lisp form into llvm::Value*'s"
  (mapcar #'fix-arg args))

(defun generate-run-all-from-literal-nodes (nodes)
  ;; We split up a run-all that would be very big so LLVM doesn't take years to compile.
  (cond
    ((> (estimate-run-all-size nodes) +max-run-all-size+)
     (let* ((half-len (floor (length nodes) 2))
            (middle-node (nthcdr (1- half-len) nodes))
            (front nodes)
            (back (cdr middle-node))
            (_ (rplacd middle-node nil)) ; break the list in two
            (front-run-all (generate-run-all-from-literal-nodes front))
            (back-run-all (generate-run-all-from-literal-nodes back)))
       (cmp::with-make-new-run-all (sub-run-all)
         (cmp:irc-intrinsic-call "cc_invoke_sub_run_all_function" (list front-run-all))
         (cmp:irc-intrinsic-call "cc_invoke_sub_run_all_function" (list back-run-all))
         sub-run-all)))
    (t
     (cmp::with-make-new-run-all (foo)
       (dolist (node nodes)
         (cond
           ((literal-node-creator-p node)
            (ensure-creator-llvm-value node))
           ((literal-node-side-effect-p node)
            (let* ((fn-name (literal-node-side-effect-name node))
                   (args (literal-node-side-effect-arguments node))
                   (fix-args (fix-args args)))
              (cmp:irc-intrinsic-call fn-name fix-args)))
           ((literal-node-toplevel-funcall-p node)
            (cmp:irc-intrinsic-call "ltvc_toplevel_funcall" (literal-node-toplevel-funcall-arguments node)))
           ((literal-node-closure-p node)
            (let ((lambda-name (cmp:irc-intrinsic-call "ltvc_lookup_value"
                                                       (list *gcroots-in-module*
                                                             (fix-arg (literal-node-closure-lambda-name-index node))))))
              (cmp:irc-intrinsic-call "ltvc_enclose"
                                      (list *gcroots-in-module*
                                            (cmp:jit-constant-size_t (literal-node-closure-index node))
                                            lambda-name
                                            (literal-node-closure-function node)
                                            (literal-node-closure-source-info-handle node)
                                            (literal-node-closure-filepos node)
                                            (literal-node-closure-lineno node)
                                            (literal-node-closure-column node)))))
           (t (error "Unknown run-all node ~a" node))))
       foo))))

(defun generate-run-time-code-for-closurette (node irbuilder-alloca array)
  ;; Generate calls to ltvc_enclose for closurettes that are created at JIT startup time
  (declare (ignore array))
  (let ((lambda-name (cmp:irc-intrinsic-call "ltvc_lookup_value"
                                             (list *gcroots-in-module*
                                                   (fix-arg (literal-node-closure-lambda-name-index node))))))
    (cmp:irc-intrinsic-call "ltvc_enclose"
                            (list *gcroots-in-module*
                                  (cmp:jit-constant-size_t (literal-node-closure-index node))
                                  lambda-name
                                  (literal-node-closure-function node)
                                  (literal-node-closure-source-info-handle node)
                                  (literal-node-closure-filepos node)
                                  (literal-node-closure-lineno node)
                                  (literal-node-closure-column node)))))

(defun do-ltv (type body-fn)
  "Evaluate body-fn in an environment where load-time-values, literals and constants are
compiled into a DSL of creators and side-effects that can be used to generate calls
in the RUN-ALL function to recreate those objects in a constants-table.
The body-fn must return an llvm::Function object that results from compiling code that
can be arranged to be evaluated in the RUN-ALL function and that will use all of the values in
the constants-table."
  (let ((body-return-fn (funcall body-fn)))
    (or (llvm-sys:valuep body-return-fn)
        (error "The body of with-ltv MUST return a compiled llvm::Function object resulting from compiling a thunk - instead it returned: ~a" body-return-fn))
    (cond
      ((eq type :toplevel)
       (run-all-add-node (make-literal-node-toplevel-funcall
                          :arguments (list body-return-fn
                                           (cmp:jit-constant-unique-string-ptr (llvm-sys:get-name body-return-fn))))))
      ((eq type :ltv) body-return-fn)
      (t (error "bad ltv type: ~a" type)))))

#+(or)
(defmacro with-ltv ( &body body)
  `(let ((*with-ltv-depth* (1+ *with-ltv-depth*)))
     (do-ltv :ltv (lambda () ,@body))))

(defmacro with-load-time-value (&body body)
  "Evaluate the body and then arrange to evaluate the generated function into a load-time-value.
Return the index of the load-time-value"
  (let ((ltv-func (gensym))
        (index (gensym)))
    `(let* ((*with-ltv-depth* (1+ *with-ltv-depth*))
            (,index (new-table-index))
            (,ltv-func (do-ltv :ltv (lambda () ,@body))))
       (add-creator "ltvc_set_ltv_funcall" ,index nil ,ltv-func (cmp:jit-constant-unique-string-ptr (llvm-sys:get-name ,ltv-func)))
       ,index)))

(defmacro with-load-time-value-cleavir (&body body)
  "Evaluate the body and then arrange to evaluate the generated function into a load-time-value.
Return the index of the load-time-value"
  (let ((ltv-func (gensym))
        (index (gensym)))
    `(let* ((*with-ltv-depth* (1+ *with-ltv-depth*))
            (,index (new-table-index))
            (,ltv-func (do-ltv :ltv (lambda () ,@body))))
       (add-creator "ltvc_set_ltv_funcall_cleavir" ,index nil ,ltv-func (cmp:jit-constant-unique-string-ptr (llvm-sys:get-name ,ltv-func)))
       ,index)))

(defmacro with-top-level-form ( &body body)
  `(let ((*with-ltv-depth* (1+ *with-ltv-depth*)))
     (do-ltv :toplevel (lambda () ,@body))))

(defun do-literal-table (body-fn)
  (let ((*gcroots-in-module*
          (llvm-sys:make-global-variable cmp:*the-module*
                                         cmp:%gcroots-in-module% ; type
                                         nil ; isConstant
                                         'llvm-sys:internal-linkage
                                         (llvm-sys:undef-value-get cmp:%gcroots-in-module%)
                                         ;; nil ; initializer
                                         (core:bformat nil "constants-table*%d" (core:next-number))))
        (cmp:*load-time-value-holder-global-var*
          (llvm-sys:make-global-variable cmp:*the-module*
                                         cmp:%t*[DUMMY]% ; type
                                         nil              ; isConstant
                                         'llvm-sys:internal-linkage
                                         (llvm-sys:undef-value-get cmp:%t*[DUMMY]%)
                                         ;; nil ; initializer
                                         (next-value-table-holder-name "dummy")))
        (*llvm-values* (make-hash-table))
        (cmp:*generate-compile-file-load-time-values* t)
        (*identity-coalesce* (make-hash-table :test #'eq))
        (*ratio-coalesce* (make-similarity-table #'eql))
;;;        (*cons-coalesce* (make-similarity-table #'eq))
        (*complex-coalesce* (make-similarity-table #'eql))
;;;        (*array-coalesce* (make-similarity-table #'eq))
        (*hash-table-coalesce* (make-similarity-table #'eq))
        (*bignum-coalesce* (make-similarity-table #'eql))
        (*symbol-coalesce* (make-similarity-table #'eq))
        (*string-coalesce* (make-similarity-table #'equal))
        (*pathname-coalesce* (make-similarity-table #'equal))
        (*package-coalesce* (make-similarity-table #'eq))
        (*built-in-class-coalesce* (make-similarity-table #'eq))
        (*double-float-coalesce* (make-similarity-table #'eql))
        (*constant-index-to-literal-node-creator* (make-hash-table :test #'eql))
        (*table-index* 0)
        (real-name (next-value-table-holder-name))
        (*run-all-objects* nil))
    (funcall body-fn)
    ;; Generate the run-all function here
    (cmp:with-run-all-body-codegen
        (let* ((ordered-run-all-nodes (nreverse *run-all-objects*))
               (sub-run-all (generate-run-all-from-literal-nodes ordered-run-all-nodes)))
          (cmp:irc-intrinsic-call "cc_invoke_sub_run_all_function" (list sub-run-all))))
    (let* ((table-entries *table-index*))
      (when (> table-entries 0)
        ;; We have a new table, replace the old one and generate code to register the new one
        ;; and gc roots tabl
        (let* ((array-type (llvm-sys:array-type-get cmp:%t*% table-entries))
               (correct-size-holder (llvm-sys:make-global-variable cmp:*the-module*
                                                                   array-type
                                                                   nil ; isConstant
                                                                   'llvm-sys:internal-linkage
                                                                   (llvm-sys:undef-value-get array-type)
                                                                   real-name))
               (bitcast-correct-size-holder (cmp:irc-bit-cast correct-size-holder cmp:%t*[DUMMY]*% "bitcast-table"))
               (holder-ptr (llvm-sys:create-geparray cmp:*irbuilder* correct-size-holder
                                                     (list (cmp:jit-constant-size_t 0)
                                                           (cmp:jit-constant-size_t 0)) "table")))
          (llvm-sys:replace-all-uses-with cmp:*load-time-value-holder-global-var* bitcast-correct-size-holder)
          (cmp:with-run-all-entry-codegen
              (cmp:irc-intrinsic-call "cc_initialize_gcroots_in_module"
                                      (list *gcroots-in-module*
                                            (cmp:irc-pointer-cast correct-size-holder cmp:%t**% "")
                                            (cmp:jit-constant-size_t table-entries)
                                            (cmp:irc-int-to-ptr (cmp:jit-constant-uintptr_t 0) cmp:%t*%))))
          ;; Erase the dummy holder
          (llvm-sys:erase-from-parent cmp:*load-time-value-holder-global-var*))))))

(defmacro with-literal-table (&body body)
  `(do-literal-table (lambda () ,@body)))

(defmacro with-rtv (&body body)
  "Evaluate the code in the body in an environment where run-time values are assigned integer indices
starting from *table-index* into a constants table and the run-time values are accumulated in *run-all-objects*.
References to the run-time values are relative to the *load-time-value-holder-global-var*.
Once the body has evaluated, if there were run-time values accumulated then sort them by index and construct a new
global variable that can hold them all and replace every use of *load-time-value-holder-global-var* with this new constants-table.
Then erase the global variable in *load-time-value-holder-global-var* whether or not run time values were found
and  return the sorted values and the constant-table or (values nil nil)."
  `(let ((cmp:*generate-compile-file-load-time-values* nil)
         (*gcroots-in-module*
           (llvm-sys:make-global-variable cmp:*the-module*
                                          cmp:%gcroots-in-module% ; type
                                          nil ; isConstant
                                          'llvm-sys:internal-linkage
                                          (llvm-sys:undef-value-get cmp:%gcroots-in-module%)
                                          ;; nil ; initializer
                                          (core:bformat nil "constants-table*%d" (core:next-number))))
         (*table-index* 0)
         (cmp:*load-time-value-holder-global-var*
           (llvm-sys:make-global-variable cmp:*the-module*
                                          cmp:%t*[0]% ; type
                                          nil     ; isConstant
                                          'llvm-sys:internal-linkage
                                          nil
                                          (next-value-table-holder-name)))
         (*run-time-coalesce* (make-hash-table :test #'eq))
         (*run-all-objects* nil))
     (progn ,@body)
     (let* ((run-time-values *run-all-objects*)
            (num-elements (length run-time-values))
            (constant-table nil))
       ;; Put the constants in order they will appear in the table.
       ;; Return the orderered-raw-constants-list and the constants-table GlobalVariable
       (when (> num-elements 0)
         (let* ((ordered-literals-list (sort run-time-values #'< :key #'literal-node-runtime-index))
                (array-type (llvm-sys:array-type-get cmp:%t*% (length ordered-literals-list))))
           (setf constant-table (llvm-sys:make-global-variable cmp:*the-module*
                                                               array-type
                                                               nil ; isConstant
                                                               'llvm-sys:internal-linkage
                                                               (llvm-sys:undef-value-get array-type)
                                                               (next-value-table-holder-name)))
           (let ((bitcast-constant-table (cmp:irc-bit-cast constant-table cmp:%t*[0]*% "bitcast-table")))
             (llvm-sys:replace-all-uses-with cmp:*load-time-value-holder-global-var* bitcast-constant-table)
             (llvm-sys:erase-from-parent cmp:*load-time-value-holder-global-var*)
             (multiple-value-bind (startup-fn shutdown-fn ordered-raw-constant-list)
                 (cmp:codegen-startup-shutdown cmp:*the-module* *gcroots-in-module* constant-table num-elements ordered-literals-list bitcast-constant-table)
               (values ordered-raw-constant-list constant-table startup-fn shutdown-fn))))))))

(defun load-time-reference-literal (object read-only-p &key recursive-p)
  "If the object is an immediate object return (values immediate nil).
   Otherwise return (values creator T)."
  (let ((immediate (immediate-object-or-nil object)))
    (if immediate
        (values (cmp:irc-maybe-cast-integer-to-t* immediate) nil)
        (multiple-value-bind (similarity creator)
            (object-similarity-table-and-creator object read-only-p)
          (let ((existing (if similarity (find-similar object similarity) nil)))
            (if existing
                (values (gethash existing *constant-index-to-literal-node-creator*) t)
                (let ((index (new-table-index)))
                  (when similarity (add-similar object index similarity))
                  (setf (gethash index *constant-index-to-literal-node-creator*) creator)
                  (values (funcall creator object index read-only-p :recursive-p t) t))))))))

(defun evaluate-function-into-load-time-value (index fn)
  (add-creator "ltvc_set_ltv_funcall" index nil fn (cmp:jit-constant-unique-string-ptr (llvm-sys:get-name fn)))
  index)

(defun pretty-load-time-name (object ltv-idx)
  (cond
    ((symbolp object) (bformat nil "SYMBOL->%s" object))
    ((consp object) "CONS")
    ((arrayp object) "ARRAY")
    ((numberp object) (format nil "NUMBER->~a" object))
    (t (subseq (bformat nil "ltv-idx_%d_val->%s" ltv-idx object) 0 30))))

;;;----------------------------------------------------------------------
;;;
;;; run time values (i.e., cl:compile)
;;;

(defvar *run-time-coalesce*)

(defun run-time-reference-literal (object read-only-p)
  "If the object is an immediate object return (values immediate nil nil).
   Otherwise return (values creator T index)."
  (let ((immediate (immediate-object-or-nil object)))
    (if immediate
        (values immediate NIL)
        (let* ((similarity *run-time-coalesce*)
               (existing (find-similar object similarity)))
          (if existing
              (values existing T)
              (values (let* ((index (new-table-index))
                             (new-obj (make-literal-node-runtime :index index :object object)))
                        (add-similar object new-obj similarity)
                        (run-all-add-node new-obj)
                        new-obj)
                      T))))))

;;; ------------------------------------------------------------
;;;
;;; compile-form
;;;
;;; Compile the form and return a 0-arity function that
;;; returns a result.
;;;

(defun bclasp-compile-form (form)
  (let ((fn (cmp:with-new-function (fn fn-env fn-result
                                       :function-name 'bclasp-top-level-form
                                       :parent-env nil
                                       :function-form form
                                       :function-info (cmp:make-function-info
                                                       :function-name 'bclasp-top-level-form
                                                       :lambda-list nil
                                                       :docstring nil
                                                       :declares nil
                                                       :form nil
                                                       :lineno (core:source-pos-info-lineno core:*current-source-pos-info*)
                                                       :column (core:source-pos-info-column core:*current-source-pos-info*)
                                                       :filepos (core:source-pos-info-column core:*current-source-pos-info*)))
              (let* ((given-name (llvm-sys:get-name fn)))
                ;; Map the function argument names
                (cmp:cmp-log "Creating ltv thunk with name: %s%N" given-name)
                (cmp:codegen fn-result form fn-env)))))
    (cmp:cmp-log-dump-function fn)
    (unless cmp:*suppress-llvm-output* (cmp:irc-verify-function fn t))
    fn))

(defun compile-form (form)
  (if core:*use-cleavir-compiler*
      (progn
        (funcall (find-symbol "COMPILE-LTV-FORM" "CLASP-CLEAVIR") form))
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
  "Return (values index T) for the literal object in a constants-table.
   Returns (values :poison-value-from-reference-literal nil) if the object is an immediate and doesn't have a place in the constants-table."
  (let ((cmp:*compile-file-debug-dump-module* nil)
        (cmp:*compile-debug-dump-module* nil))
    (if (cmp:generate-load-time-values)
        (multiple-value-bind (data in-array)
            (load-time-reference-literal object read-only-p)
          (if in-array
              (let ((index (literal-node-creator-index data)))
                (values index T (literal-node-creator-literal-name data)))
              (values data nil)))
        (multiple-value-bind (immediate?literal-node-runtime in-array)
            (run-time-reference-literal object read-only-p)
          (if in-array
              (let* ((literal-node-runtime immediate?literal-node-runtime)
                     (index (literal-node-runtime-index literal-node-runtime)))
                (values index T))
              (let ((immediate immediate?literal-node-runtime))
                (values immediate nil)))))))

;;; ------------------------------------------------------------
;;;
;;; reference-closure
;;;
;;; Returns an index for a closure.
;;; We skip similarity testing etc. This could be improved (FIXME)
;;; We could also add the capability to dump actual closures, though
;;;  I'm not sure why we'd want to do so.

(defun reference-closure (lambda-name enclosed-function source-info-handle
                          filepos lineno column)
  (if (cmp:generate-load-time-values)
      (multiple-value-bind (lambda-name-node in-array)
          (load-time-reference-literal lambda-name t)
        (unless in-array
          (error "BUG: Immediate lambda-name ~a- What?" lambda-name))
        (let* ((index (new-table-index))
               (creator (make-literal-node-closure
                         ;; lambda-name will never be immediate. (Should we check?)
                         :lambda-name-index (literal-node-creator-index lambda-name-node)
                         :index index :function enclosed-function
                         :source-info-handle source-info-handle
                         :filepos filepos :lineno lineno :column column)))
          (run-all-add-node creator)
          index))
      (multiple-value-bind (lambda-name-node in-array)
          (run-time-reference-literal lambda-name t)
        (unless in-array
          (error "BUG: Immediate lambda-name ~a- What?" lambda-name))
        (let* ((index (new-table-index))
               (creator (make-literal-node-closure
                         ;; lambda-name will never be immediate. (Should we check?)
                         :lambda-name-index (literal-node-runtime-index lambda-name-node)
                         :index index :function enclosed-function
                         :source-info-handle source-info-handle
                         :filepos filepos :lineno lineno :column column)))
          (run-all-add-node creator)
          index))))

;;; ------------------------------------------------------------
;;;
;;; functions that are called by bclasp and cclasp that might
;;;  be refactored to simplify the API

(defun compile-reference-to-literal (literal)
  "Generate a reference to a load-time-value or run-time-value literal depending if called from COMPILE-FILE or COMPILE respectively"
  (multiple-value-bind (data-or-index in-array literal-name)
      (reference-literal literal t)
    (if in-array
        (values (constants-table-reference data-or-index (pretty-load-time-name literal data-or-index)) literal-name)
        data-or-index)))

(defun codegen-rtv-bclasp (result obj)
  "bclasp calls this to get copy the run-time-value for obj into result.
Returns (value index t) if the value was put in the literal vector or it
returns (value immediate nil) if the value is an immediate value."
  (multiple-value-bind (immediate?literal-node-runtime in-array)
      (run-time-reference-literal obj t)
    (if in-array
        (let* ((literal-node-runtime immediate?literal-node-runtime)
               (index (literal-node-runtime-index literal-node-runtime)))
          (cmp:irc-store (constants-table-value index) result)
          index)
        (let ((immediate immediate?literal-node-runtime))
          (cmp:irc-store immediate result)
          :poison-value-from-codegen-rtv-bclasp))))

(defun codegen-rtv-cclasp (obj)
  "bclasp calls this to get copy the run-time-value for obj into result.
Returns (value index t) if the value was put in the literal vector or it
returns (value immediate nil) if the value is an immediate value."
  (multiple-value-bind (immediate?literal-node-runtime in-array)
      (run-time-reference-literal obj t)
    (if in-array
        (let* ((literal-node-runtime immediate?literal-node-runtime)
               (index (literal-node-runtime-index literal-node-runtime)))
          (values index t))
        (let ((immediate immediate?literal-node-runtime))
          (values immediate nil)))))

(defun codegen-literal (result object env)
  "This is called by bclasp.  If result is nil then just return the ltv index.
If it isn't NIL then copy the literal from its index in the LTV into result."
  (multiple-value-bind (data-or-index in-array)
      (reference-literal object t)
    (if in-array
        (progn
          (when result
            (cmp:irc-store (constants-table-value data-or-index) result))
          data-or-index)
        (progn
          (when result
            (cmp:irc-store (cmp:ensure-jit-constant-i64 data-or-index) result))
          :poison-value-from-codegen-literal))))

;; Should be bclasp-compile-load-time-value-thunk
(defun compile-load-time-value-thunk (form)
  "bclasp compile the form into an llvm function and return that function"
  (let ((fn (cmp:with-new-function (fn fn-env fn-result
                                       :function-name 'bclasp-top-level-form
                                       :parent-env nil
                                       :function-form form
                                       :function-info (cmp:make-function-info
                                                       :function-name 'bclasp-top-level-form
                                                       :lambda-list nil
                                                       :docstring nil
                                                       :declares nil
                                                       :form nil
                                                       :lineno (core:source-pos-info-lineno core:*current-source-pos-info*)
                                                       :column (core:source-pos-info-column core:*current-source-pos-info*)
                                                       :filepos (core:source-pos-info-filepos core:*current-source-pos-info*))
                                       )
              (let ((given-name (llvm-sys:get-name fn)))
                (cmp:codegen fn-result form fn-env)))))
    (unless cmp:*suppress-llvm-output* (cmp:irc-verify-function fn t))
    (or (llvm-sys:valuep fn) (error "compile-load-time-value-thunk must return an llvm::Function object - it will return ~a" fn))
    fn))

;;; ------------------------------------------------------------
;;;
;;; Access load-time-values
;;;

(defun constants-table-reference (index &optional (label "ltv") (holder cmp:*load-time-value-holder-global-var*) literal-name)
  (let ((label (if literal-name
                   (bformat nil "values-table[%d]/%s" index literal-name)
                   (bformat nil "values-table[%d]" index))))
    (llvm-sys:create-const-gep2-64 cmp:*irbuilder* holder 0 index label)))

(defun constants-table-value (index &optional (label "ltv") (holder cmp:*load-time-value-holder-global-var*))
  (cmp:irc-load (constants-table-reference index label holder)))

(defun copy-constants-table-value (result index
                                   &optional (holder cmp:*load-time-value-holder-global-var*))
  (let ((ref (constants-table-reference index "copy-constants-table-value" holder )))
    (cmp:irc-store (cmp:irc-load ref) result)))




;;; ------------------------------------------------------------
;;;
;;; Bytecode interpreter generator
;;;
;;;

#+(or)
(progn
  
  (defparameter *machine*
    '(
      (primitive         "ltvc_assign_source_file_info_handle" %void% (list %i8*% %i8*% %size_t% %i32% %i32*%))
      (primitive         "ltvc_make_nil" %ltvc-return% (list %gcroots-in-module*% %size_t%))
      (primitive         "ltvc_make_t" %ltvc-return% (list %gcroots-in-module*% %size_t%))
      (primitive         "ltvc_make_ratio" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*% %t*%))
      (primitive         "ltvc_make_complex" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*% %t*%))
      (primitive         "ltvc_make_cons" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*% %t*%))
      (primitive         "ltvc_nconc" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*% %t*%))
      (primitive         "ltvc_make_list" %ltvc-return% (list %gcroots-in-module*% %size_t% %size_t%) :varargs t)
      (primitive         "ltvc_make_array" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*% %t*%))
      (primitive         "ltvc_setf_row_major_aref" %ltvc-return% (list %t*% %size_t% %t*%))
      (primitive         "ltvc_make_hash_table" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*%))
      (primitive         "ltvc_setf_gethash" %ltvc-return% (list %t*% %t*% %t*%))
      (primitive         "ltvc_make_fixnum" %ltvc-return% (list %gcroots-in-module*% %size_t% %uintptr_t%))
      (primitive         "ltvc_make_package" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*%))
      (primitive         "ltvc_make_bignum" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*%))
      (primitive         "ltvc_make_bitvector" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*%))
      (primitive         "ltvc_make_symbol" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*% %t*%))
      (primitive         "ltvc_make_character" %ltvc-return% (list %gcroots-in-module*% %size_t% %uintptr_t%))
      (primitive         "ltvc_make_base_string" %ltvc-return% (list %gcroots-in-module*% %size_t% %i8*%))
      (primitive         "ltvc_make_pathname" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*% %t*% %t*% %t*% %t*% %t*%))
      (primitive         "ltvc_make_package" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*%))
      (primitive         "ltvc_make_random_state" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*%))
      (primitive         "ltvc_make_built_in_class" %ltvc-return% (list %gcroots-in-module*% %size_t% %t*%))
      (primitive         "ltvc_make_float" %ltvc-return% (list %gcroots-in-module*% %size_t% %float%))
      (primitive         "ltvc_make_double" %ltvc-return% (list %gcroots-in-module*% %size_t% %double%))
      (primitive         "ltvc_lookup_value" %t*% (list %gcroots-in-module*% %size_t%))
      (primitive         "ltvc_enclose" %ltvc-return% (list %gcroots-in-module*%
                                                       %size_t%
                                                       %t*%
                                                       %i8*%
                                                       %fn-prototype*%
                                                       %i32*%
                                                       %size_t%
                                                       %size_t%
                                                       %size_t%))
      (primitive-unwinds "ltvc_set_mlf_creator_funcall" %ltvc-return% (list %gcroots-in-module*% %size_t% %fn-prototype*% %i8*%))
      (primitive-unwinds "ltvc_mlf_init_funcall" %ltvc-return% (list %fn-prototype*% %i8*%))
      (primitive-unwinds "ltvc_set_ltv_funcall" %ltvc-return% (list %gcroots-in-module*% %size_t% %fn-prototype*% %i8*%))
      (primitive-unwinds "ltvc_set_ltv_funcall_cleavir" %ltvc-return% (list %gcroots-in-module*% %size_t% %fn-prototype*% %i8*%))
      (primitive-unwinds "ltvc_toplevel_funcall" %ltvc-return% (list %fn-prototype*% %i8*%))
      )
    )

  (defstruct c++-info type c++-type)

  (defun set-c++-info (symbol c++-type)
    (setf (gethash symbol *c++-info*) (make-c++-info :type symbol :c++-type c++-type)))

  (defparameter *c++-info* (make-hash-table :test #'equal))
  (eval-when (:load-toplevel :execute)
    (set-c++-info '%size_t% "size_t")
    (set-c++-info '%i32% "int")
    (set-c++-info '%t*% "T_O*")
    (set-c++-info '%i8*% "const char*")
    (set-c++-info '%i32*% "int*")
    (set-c++-info '%float% "float")
    (set-c++-info '%double% "double")
    (set-c++-info '%uintptr_t% "uintptr_t")
    (set-c++-info :unknown "UNKNOWN")
    )


  (defun build-one-c++-function (op &optional (stream *standard-output*))
    (destructuring-bind (op-kind name return-type argument-types &key varargs)
        op
      (let ((index (second argument-types))
            (arg-types (nthcdr 2 argument-types)))
        (format stream "void parse_~a(FILE* fin) {~%" name)
        (let ((vars (loop for arg-type in arg-types
                          for arg-index from 0
                          for c++-info = (let ((info (gethash arg-type *c++-info*)))
                                           (if info
                                               info
                                               (make-c++-info :type (format nil "UNKNOWN_~a" arg-type)
                                                              :c++-type (format nil "UNKNOWN_~a" arg-type))))
                          for c++-arg-type = (c++-info-c++-type c++-info)
                          for variable-name = (format nil "arg~a" arg-index)
                          do (format stream "  ~a ~a = reader<~a>(fin);~%" c++-arg-type variable-name c++-arg-type)
                          collect variable-name)))
          (format stream "  ~a( MACHINE" name)
          (loop for var in vars
                do (format stream ", ~a" var))
          (format stream ");~%")
          (format stream "};~%")))))

  (defun build-c++-functions (primitives &optional (stream *standard-output*))
    (loop for prim in primitives
          do (build-one-c++-function prim stream)))

  (defun build-c++-switch (primitives &optional (stream *standard-output*))
    (loop for prim in primitives
          for func-name = (second prim)
          for code from 0
          do (format stream "  case ~a: ~a(fin);~%" code func-name)
             (format stream "           break;~%")))


  (defun build-byte-codes (primitives map)
    (loop for prim in primitives
          for code from 0
          for name = (second prim)
          do (setf (gethash name map) code))
    map)


  (build-c++-functions *machine*)

  (build-c++-switch *machine*)

  (build-byte-codes *machine* (make-hash-table :test #'equal))
  )
