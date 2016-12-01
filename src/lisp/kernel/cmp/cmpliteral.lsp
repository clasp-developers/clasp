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

(defvar *constants-table-holder*)
(defvar *value-table-id* 0)
(defun next-value-table-holder-name (&optional suffix)
  (if suffix
      (bformat nil "%s-CONTAB%d" suffix *value-table-id*)
      (bformat nil "CONTAB%d" (incf *value-table-id*))))

(defstruct (constant-creator (:type vector) :named) index name arguments)
(defstruct (constant-call (:type vector) :named) function source-pos-info holder)
(defstruct (constant-side-effect (:type vector) :named) name arguments)
(defstruct (constant-runtime (:type vector) :named) index object)

(defun constant-list-dump (constant-list)
  (dolist (e constant-list)
    (constant-table-entry-print e)))

(defparameter *run-all-objects* nil)

(defun run-all-add-node (node)
  #+(or)(progn
    (bformat t "run-all-add-node ")
    (constant-table-entry-print node t))
  (push node *run-all-objects*)
  node)

(defun run-all-nodes-in-order ()
  (reverse *run-all-objects*))

(defun calculate-table-size (nodes)
  "Find the highest index and return 1+ that"
  (let ((highest-index -1))
    (dolist (node nodes)
      #+(or)(bformat t "generate-run-all-code  generating node: %s\n" node)
      (when (literal:constant-creator-p node)
        (setf highest-index (max highest-index (literal:constant-creator-index node)))))
    (1+ highest-index)))

;;; ------------------------------------------------------------
;;;
;;;
;;;

(defvar *table-index*)

;;; ------------------------------------------------------------
;;;
;;;

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
(defvar *identity-coalesce*)
(defvar *constant-index-to-constant-creator*)
(defvar *llvm-values*)

(defvar *with-ltv-depth* 0)

(defun new-table-index ()
  "Return the next ltv-index. If this is being invoked from COMPILE then
the value is put into *default-load-time-value-vector* and its index is returned"
  (let ((index *table-index*))
    #+(or)(bformat t "new-table-index depth %4d - %d\n" *with-ltv-depth* index)
    (incf *table-index*)
    index))

(defun add-creator (name index &rest args)
  "Call the named function after converting fixnum args to llvm constants"
  (let ((creator (make-constant-creator :index index :name name :arguments args)))
    (setf (gethash index *constant-index-to-constant-creator*) creator)
    (run-all-add-node creator)
    creator))

(defun add-side-effect-call (name &rest args)
  "Call the named function after converting fixnum args to llvm constants"
  (let ((rase (make-constant-side-effect :name name :arguments args)))
    (run-all-add-node rase)
    rase))

(defun ltv/nil (object index read-only-p)
  (add-creator "ltvc_make_nil" index))

(defun ltv/t (object index read-only-p)
  (add-creator "ltvc_make_t" index))

(defun ltv/ratio (ratio index read-only-p)
  (add-creator "ltvc_make_ratio" index
            (load-time-reference-literal (numerator ratio) read-only-p)
            (load-time-reference-literal (denomenator ratio) read-only-p)))

(defun ltv/cons (cons index read-only-p)
  (if (core:proper-list-p cons)
      (apply 'add-creator "ltvc_make_list" index
             (length cons) (mapcar (lambda (x)
                                     (load-time-reference-literal x read-only-p))
                                   cons))
      (add-creator "ltvc_make_cons" index
                (load-time-reference-literal (car cons) read-only-p)
                (load-time-reference-literal (cdr cons) read-only-p))))

(defun ltv/complex (complex index read-only-p)
  (add-creator "ltvc_make_complex" index
            (load-time-reference-literal (realpart complex) read-only-p)
            (load-time-reference-literal (imagpart complex) read-only-p)))

(defun ltv/array (array index read-only-p)
  (let ((val (add-creator "ltvc_make_array" index
                       (load-time-reference-literal (array-element-type array) read-only-p)
                       (load-time-reference-literal (array-dimensions array) read-only-p))))
    (let* ((total-size (if (array-has-fill-pointer-p array)
                           (length array)
                           (array-total-size array))))
      (dotimes (i total-size)
        (add-side-effect-call "ltvc_setf_row_major_aref" val i
                              (load-time-reference-literal (row-major-aref array i) read-only-p))))
    val))

(defun ltv/hash-table (hash-table index read-only-p)
  (let ((ht (add-creator "ltvc_make_hash_table" index
                      (load-time-reference-literal (hash-table-test hash-table) read-only-p))))
    (maphash (lambda (key val)
               (add-side-effect-call "ltvc_setf_gethash" ht
                                     (load-time-reference-literal key read-only-p)
                                     (load-time-reference-literal val read-only-p)))
             hash-table)
    ht))

(defun ltv/fixnum (fixnum index read-only-p)
  (add-creator "ltvc_make_fixnum" index fixnum))

(defun ltv/bignum (bignum index read-only-p)
  (let ((bn-str (format nil "~a" bignum)))
    (add-creator "ltvc_make_bignum" index (load-time-reference-literal bn-str read-only-p))))

(defun ltv/random-state (random-state index read-only-p)
  (let ((rs-str (format nil "~a" (core:random-state-get random-state))))
    (add-creator "ltvc_make_random_state" index (load-time-reference-literal rs-str read-only-p))))

(defun ltv/symbol (symbol index read-only-p)
  (let ((pkg (symbol-package symbol))
        (sym-str (symbol-name symbol)))
    (add-creator "ltvc_make_symbol" index
              (load-time-reference-literal sym-str read-only-p)
              (load-time-reference-literal pkg read-only-p))))

(defun ltv/character (char index read-only-p)
  (add-creator "ltvc_make_character" index
            (jit-constant-i64 (char-code char))))

(defun ltv/base-string (str index read-only-p)
    (add-creator "ltvc_make_base_string" index str))

(defun ltv/pathname (pathname index read-only-p)
  (add-creator "ltvc_make_pathname" index
            (load-time-reference-literal (pathname-host pathname) read-only-p)
            (load-time-reference-literal (pathname-device pathname) read-only-p)
            (load-time-reference-literal (pathname-directory pathname) read-only-p)
            (load-time-reference-literal (pathname-name pathname) read-only-p)
            (load-time-reference-literal (pathname-type pathname) read-only-p)
            (load-time-reference-literal (pathname-version pathname) read-only-p)))

(defun ltv/package (package index read-only-p)
  (add-creator "ltvc_make_package" index
            (load-time-reference-literal (package-name package) read-only-p)))

(defun ltv/built-in-class (class index read-only-p)
  (add-creator "ltvc_make_built_in_class" index
            (load-time-reference-literal (class-name class) read-only-p)))

(defun ltv/single-float (single index read-only-p)
  (let* ((constant (llvm-sys:make-apfloat-float single))
         (constant-ap-arg (llvm-sys:constant-fp-get *llvm-context* constant)))
    (add-creator "ltvc_make_float" index constant-ap-arg)))

(defun ltv/double-float (double index read-only-p)
  (let* ((constant (llvm-sys:make-apfloat-double double))
         (constant-ap-arg (llvm-sys:constant-fp-get *llvm-context* constant)))
    (add-creator "ltvc_make_double" index constant-ap-arg)))

(defun ltv/mlf (object index read-only-p)
  (multiple-value-bind (create initialize)
      (make-load-form object)
    (prog1 (add-creator "ltvc_set_mlf_creator_funcall" index (compile-form create))
      (when initialize
        (add-side-effect-call "ltvc_mlf_init_funcall" (compile-form initialize))))))

(defun object-similarity-table-and-creator (object read-only-p)
  (cond
    ((null object) (values *identity-coalesce* #'ltv/nil))
    ((eq t object) (values *identity-coalesce* #'ltv/t))
    ((consp object) (values *cons-coalesce* #'ltv/cons))
    ((fixnump object) (values *fixnum-coalesce* #'ltv/fixnum))
    ((characterp object) (values *character-coalesce* #'ltv/character))
    ((core:single-float-p  object) (values *single-float-coalesce* #'ltv/single-float))
    ((symbolp object) (values *symbol-coalesce* #'ltv/symbol))
    ((double-float-p object) (values *double-float-coalesce* #'ltv/double-float))
    ((core:ratio-p object) (values *ratio-coalesce* #'ltv/ratio))
    ((stringp  object) (values (if read-only-p *identity-coalesce* *string-coalesce*) #'ltv/base-string))
    ((arrayp object) (values *array-coalesce* #'ltv/array))
    ((hash-table-p object) (values *hash-table-coalesce* #'ltv/hash-table))
    ((bignump object) (values *bignum-coalesce* #'ltv/bignum))
    ((pathnamep object) (values *pathname-coalesce* #'ltv/pathname))
    ((packagep object) (values *package-coalesce* #'ltv/package))
    ((complexp object) (values *complex-coalesce* #'ltv/complex))
    ((random-state-p object) (values *identity-coalesce* #'ltv/random-state))
    ((core:built-in-class-p object) (values *built-in-class-coalesce* #'ltv/built-in-class))
    ((typep object '(or standard-object structure-object condition class)) (values *identity-coalesce* #'ltv/mlf))
    (t (error "Handle object ~a" object))))

(defun make-similarity-table (test)
  (make-hash-table :test test))

(defun find-similar (object table)
  (gethash object table))

(defun add-similar (object index table)
  (setf (gethash object table) index))


(defun do-with-ltv (type body-fn)
  "Evaluate body-fn in an environment where load-time-values, literals and constants are
compiled into a DSL of creators and side-effects that can be used to generate calls
in the RUN-ALL function to recreate those objects in a constants-table.
The body-fn must return an llvm::Function object that results from compiling code that
can be arranged to be evaluated in the RUN-ALL function and that will use all of the values in
the constants-table."
  (let* ((*run-all-objects* nil)
         (body-return-fn (funcall body-fn))
         (constants-nodes (nreverse *run-all-objects*)))
    (or (llvm-sys:valuep body-return-fn)
        (error "The body of with-ltv MUST return a compiled llvm::Function object resulting from compiling a thunk - instead it returned: ~a" body-return-fn))
    (with-run-all-body-codegen
        ;; Generate code for the constants-table DSL
        (labels ((ensure-llvm-value (obj)
                   "Lookup or create the llvm::Value for obj"
                   (or (gethash obj *llvm-values*)
                       (setf (gethash obj *llvm-values*)
                             (irc-create-call (literal:constant-creator-name obj)
                                              (list*
                                               *constants-table-holder*
                                               (cmp:jit-constant-size_t (constant-creator-index obj))
                                               (fix-args (constant-creator-arguments obj)))
                                              (bformat nil "CONTAB[%d]" (constant-creator-index obj))))))
                 (fix-args (args)
                   "Convert the args from Lisp form into llvm::Value*'s"
                   (mapcar (lambda (x)
                             (cond
                               ((fixnump x) (jit-constant-size_t x))
                               ((stringp x) (jit-constant-unique-string-ptr x))
                               ((literal:constant-creator-p x) (ensure-llvm-value x))
                               (t x))) ;;(error "Illegal run-all entry ~a" x))))
                           args)))
          (dolist (node constants-nodes)
            #+(or)(bformat t "generate-run-all-code  generating node: %s\n" node)
            (cond
              ((literal:constant-creator-p node)
               (ensure-llvm-value node))
              ((literal:constant-side-effect-p node)
               (let* ((fn-name (literal:constant-side-effect-name node))
                      (args (literal:constant-side-effect-arguments node))
                      (fix-args (fix-args args)))
                 (irc-create-call fn-name fix-args)))
              (t (error "Unknown run-all node ~a" node)))))
      (cond
        ((eq type :toplevel)
         (cmp:irc-create-call "ltvc_toplevel_funcall" (list body-return-fn)))
        ((eq type :ltv) body-return-fn)
        (t (error "bad type"))))))

(defmacro with-ltv ( &body body)
  `(let ((*with-ltv-depth* (1+ *with-ltv-depth*)))
     (do-with-ltv :ltv (lambda () ,@body))))

(defmacro with-load-time-value (&body body)
  "Evaluate the body and then arrange to evaluate the generated function into a load-time-value.
Return the index of the load-time-value"
  (let ((ltv-func (gensym))
        (index (gensym)))
    `(let* ((*with-ltv-depth* (1+ *with-ltv-depth*))
            (,index (new-table-index))
            (,ltv-func (do-with-ltv :ltv (lambda () ,@body))))
       (evaluate-function-into-load-time-value ,index ,ltv-func)
       ,index)))
       
(defmacro with-top-level-form ( &body body)
  `(let ((*with-ltv-depth* (1+ *with-ltv-depth*)))
     (do-with-ltv :toplevel (lambda () ,@body))))



(defun do-with-constants-table (body-fn)
  (let ((*constants-table-holder*
         (llvm-sys:make-global-variable *the-module*
                                        cmp:+constants-table+ ; type
                                        nil ; isConstant
                                        'llvm-sys:internal-linkage
                                        (llvm-sys:undef-value-get cmp:+constants-table+)
                                        ;; nil ; initializer
                                        "constants-table"))
        (cmp:*load-time-value-holder-global-var*
         (llvm-sys:make-global-variable *the-module*
                                        cmp:+tsp[DUMMY]+ ; type
                                        nil              ; isConstant
                                        'llvm-sys:internal-linkage
                                        (llvm-sys:undef-value-get cmp:+tsp[DUMMY]+)
                                        ;; nil ; initializer
                                        (next-value-table-holder-name "dummy")))
        (*llvm-values* (make-hash-table))
        (cmp:*generate-compile-file-load-time-values* t)
        (*identity-coalesce* (make-hash-table :test #'eq))
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
        (*double-float-coalesce* (make-similarity-table #'eql))
        (*constant-index-to-constant-creator* (make-hash-table :test #'eql))
        (*table-index* 0)
        (real-name (next-value-table-holder-name)))
    (funcall body-fn)
    (let* ((table-entries *table-index*))
      (when (> table-entries 0)
        ;; We have a new table, replace the old one and generate code to register the new one
        ;; and gc roots tabl
        (let* ((array-type (llvm-sys:array-type-get cmp:+tsp+ table-entries))
               (correct-size-holder (llvm-sys:make-global-variable *the-module*
                                                                   array-type
                                                                   nil ; isConstant
                                                                   'llvm-sys:internal-linkage
                                                                   (llvm-sys:undef-value-get array-type)
                                                                   real-name))
               (bitcast-correct-size-holder (irc-bit-cast correct-size-holder cmp:+tsp[DUMMY]*+ "bitcast-table"))
               (holder-ptr (llvm-sys:create-geparray *irbuilder* correct-size-holder
                                                     (list (cmp:jit-constant-size_t 0)
                                                           (cmp:jit-constant-size_t 0)) "table")))
          (llvm-sys:replace-all-uses-with cmp:*load-time-value-holder-global-var* bitcast-correct-size-holder)
          (with-run-all-entry-codegen
              (cmp:irc-create-call "cc_allocate_roots" (list *constants-table-holder*
                                                             (irc-pointer-cast correct-size-holder cmp:+tsp*+ "")
                                                             (cmp:jit-constant-size_t table-entries))))
          ;; Erase the dummy holder
          (llvm-sys:erase-from-parent cmp:*load-time-value-holder-global-var*))))))

(defmacro with-constants-table (&body body)
  `(do-with-constants-table (lambda () ,@body)))

(defmacro with-rtv (&body body)
  "Evaluate the code in the body in an environment where run-time values are assigned integer indices
(starting from *table-index* into a constants table and the run-time values are accumulated in *run-all-objects*.
References to the run-time values are relative to the *load-time-value-holder-global-var*.
Once the body has evaluated, if there were run-time values accumulated then sort them by index and construct a new
global variable that can hold them all and replace every use of *load-time-value-holder-global-var* with this new constants-table.
Then erase the global variable in *load-time-value-holder-global-var* whether or not run time values were found
and  return the sorted values and the constant-table (or (values nil nil)."
  `(let ((cmp:*generate-compile-file-load-time-values* nil)
         (*table-index* 0)
         (*load-time-value-holder-global-var*
          (llvm-sys:make-global-variable *the-module*
                                         +tsp[0]+ ; type
                                         nil      ; isConstant
                                         'llvm-sys:internal-linkage
                                         nil
                                         (literal:next-value-table-holder-name)))
         (*run-time-coalesce* (make-hash-table :test #'eq))
         (*run-all-objects* nil))
     (progn ,@body)
     (let* ((run-time-values *run-all-objects*)
            (num-elements (length run-time-values))
            (ordered-raw-constant-list nil)
            (constant-table nil))
       "Put the constants in order they will appear in the table.
Return the orderered-raw-constants-list and the constants-table GlobalVariable"
       #+(or)(progn
               (bformat t "run-time-values: vvvvvvv\n")
               (literal::constant-list-dump run-time-values)
               (bformat t "Number of run-time-values: %d\n" (length run-time-values)))
       (when (> num-elements 0)
         (let* ((ordered-constant-list (sort run-time-values #'< :key #'constant-runtime-index))
                (array-type (llvm-sys:array-type-get +tsp+ (length ordered-constant-list))))
           (setf ordered-raw-constant-list
                 (mapcar (lambda (x) (constant-runtime-object x)) ordered-constant-list)
                 constant-table (llvm-sys:make-global-variable *the-module*
                                                               array-type
                                                               nil ; isConstant
                                                               'llvm-sys:internal-linkage
                                                               (llvm-sys:undef-value-get array-type)
                                                               (literal:next-value-table-holder-name)))
           (let ((bitcast-constant-table (irc-bit-cast constant-table +tsp[0]*+ "bitcast-table")))
             (llvm-sys:replace-all-uses-with *load-time-value-holder-global-var* bitcast-constant-table))))
       (llvm-sys:erase-from-parent *load-time-value-holder-global-var*)
       (values ordered-raw-constant-list constant-table))))

(defun load-time-reference-literal (object read-only-p)
  (multiple-value-bind (similarity creator)
      (object-similarity-table-and-creator object read-only-p)
    (let ((existing (find-similar object similarity)))
      (if existing
          (gethash existing *constant-index-to-constant-creator*)
          (let ((index (new-table-index)))
            (add-similar object index similarity)
            (setf (gethash index *constant-index-to-constant-creator*) creator)
            (funcall creator object index read-only-p))))))

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

(defun evaluate-function-into-load-time-value (index fn)
  (add-creator "ltvc_set_ltv_funcall" index fn)
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
;;; run time values
;;;

(defvar *run-time-coalesce*)

(defun run-time-reference-literal (object read-only-p)
  (let* ((similarity *run-time-coalesce*)
         (existing (find-similar object similarity)))
    (or existing
        (let* ((index (new-table-index))
               (new-obj (make-constant-runtime :index index :object object)))
          (add-similar object new-obj similarity)
          (run-all-add-node new-obj)
          new-obj))))

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
  "Return an index for the literal object in a constants-table"
  (let ((cmp:*compile-file-debug-dump-module* nil)
        (cmp:*compile-debug-dump-module* nil))
    (if (generate-load-time-values)
        (let* ((data (load-time-reference-literal object read-only-p)))
          (let ((index (constant-creator-index data)))
            index))
        (let ((data (run-time-reference-literal object read-only-p)))
          (let ((index (constant-runtime-index data)))
            index)))))

;;; ------------------------------------------------------------
;;;
;;; functions that are called by bclasp and cclasp that might
;;;  be refactored to simplify the API

(defun compile-reference-to-literal (literal)
  "Generate a reference to a load-time-value or run-time-value literal depending if called from COMPILE-FILE or COMPILE respectively"
  (let ((ltv-idx (reference-literal literal t)))
    (constants-table-reference ltv-idx (pretty-load-time-name literal ltv-idx))))

(defun codegen-rtv (result obj)
  "bclasp calls this to get copy the run-time-value for obj into result"
  (let* ((data (run-time-reference-literal obj t))
         (idx (constant-runtime-index data)))
    (when result
      (irc-store (literal:constants-table-value idx) result))
    idx))

(defun codegen-literal (result object env)
  "This is called by bclasp.  If result is nil then just return the ltv index.
If it isn't NIL then copy the literal from its index in the LTV into result."
  (let ((index (reference-literal object t)))
    (when result
      (irc-store (constants-table-value index) result))
    index))
        


;; Should be bclasp-compile-load-time-value-thunk
(defun compile-load-time-value-thunk (form)
  "bclasp compile the form into an llvm function and return that function"
  (dbg-set-current-debug-location-here)
  (let ((fn (with-new-function (fn fn-env fn-result
                                   :function-name 'bclasp-top-level-form
                                   :parent-env nil
                                   :function-form form)
              (let* ((given-name (llvm-sys:get-name fn)))
                (dbg-set-current-debug-location-here)
                (codegen fn-result form fn-env)
                (dbg-set-current-debug-location-here)))))
    (irc-verify-function fn t)
    (or (llvm-sys:valuep fn) (error "compile-load-time-value-thunk must return an llvm::Function object - it will return ~a" fn))
    fn))



;;; ------------------------------------------------------------
;;;
;;; Access load-time-values
;;;

(defun constants-table-reference (index &optional (label "ltv") (holder cmp:*load-time-value-holder-global-var*))
  (llvm-sys:create-const-gep2-64 *irbuilder* holder 0 index (bformat nil "values-table[%d]" index)))

(defun constants-table-value (index &optional (label "ltv") (holder cmp:*load-time-value-holder-global-var*))
  (cmp:irc-load (constants-table-reference index label holder)))

(defun copy-constants-table-value (result index
                                   &optional (holder cmp:*load-time-value-holder-global-var*))
  (let ((ref (constants-table-reference index "copy-constants-table-value" holder )))
    (irc-store (irc-load ref) result)))


