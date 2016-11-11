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


(defvar *value-table-id* 0)
(defun next-value-table-holder-name (&optional suffix)
  (if suffix
      (bformat nil "%s-CONSTANTS-TABLE%d" suffix *value-table-id*)
      (bformat nil "CONSTANTS-TABLE%d" (incf *value-table-id*))))

(eval-when (:execute :load-toplevel)
  (defstruct constant-runtime index object)
  (defstruct constant-creator index name arguments)
  (defstruct constant-side-effect name arguments)
  (defstruct constant-call function source-pos-info holder))

(defun constant-table-entry-print (obj &optional (stream t))
  (break "Trap")
  (cond
    ((constant-creator-p obj)
     (bformat stream "[%d] constant-creator -> (%s %s)\n" (constant-creator-index obj) (constant-creator-name obj) (constant-creator-arguments obj)))
    ((constant-runtime-p obj)
     (bformat stream "[%d] constant-runtime -> a %s value: %s\n" (constant-runtime-index obj) (type-of (constant-runtime-object obj)) (constant-runtime-object obj)))
    (t (bformat stream "Add support to constant-table-entry-print: %s\n" obj))))

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

(defun number-of-entries (nodes)
  (let ((num 0))
    (dolist (n nodes)
      (when (constant-creator-p n)
        (setq num (max (constant-creator-index n) num))))
    num))

;;; ------------------------------------------------------------
;;;
;;;
;;;

(defvar *table-index*)

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
(defvar *llvm-values*)

(defun new-table-index ()
  "Return the next ltv-index. If this is being invoked from COMPILE then
the value is put into *default-load-time-value-vector* and its index is returned"
  (prog1 *table-index*
    (incf *table-index*)))

(defun add-call (name index &rest args)
  "Call the named function after converting fixnum args to llvm constants"
  (let ((rao (make-constant-creator :index index :name name :arguments args)))
    (run-all-add-node rao)
    rao))

(defun add-side-effect-call (name &rest args)
  "Call the named function after converting fixnum args to llvm constants"
  (let ((rase (make-constant-side-effect :name name :arguments args)))
    (run-all-add-node rase)
    rase))

(defun ltv/nil (object index read-only-p)
  (add-call "ltvc_make_nil" index))

(defun ltv/t (object index read-only-p)
  (add-call "ltvc_make_t" index))

(defun ltv/ratio (ratio index read-only-p)
  (add-call "ltvc_make_ratio" index
            (load-time-reference-literal (numerator ratio) read-only-p)
            (load-time-reference-literal (denomenator ratio) read-only-p)))

(defun ltv/cons (cons index read-only-p)
  (if (core:proper-list-p cons)
      (apply 'add-call "ltvc_make_list" index
             (length cons) (mapcar (lambda (x)
                                     (load-time-reference-literal x read-only-p))
                                   cons))
      (add-call "ltvc_make_cons" index
                (load-time-reference-literal (car cons) read-only-p)
                (load-time-reference-literal (cdr cons) read-only-p))))

(defun ltv/complex (complex index read-only-p)
  (add-call "ltvc_make_complex" index
            (load-time-reference-literal (realpart complex) read-only-p)
            (load-time-reference-literal (imagpart complex) read-only-p)))

(defun ltv/array (array index read-only-p)
  (let ((val (add-call "ltvc_make_array" index
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
  (let ((ht (add-call "ltvc_make_hash_table" index
                      (load-time-reference-literal (hash-table-test hash-table) read-only-p))))
    (maphash (lambda (key val)
               (add-side-effect-call "ltvc_setf_gethash" ht
                                     (load-time-reference-literal key read-only-p)
                                     (load-time-reference-literal val read-only-p)))
             hash-table)
    ht))

(defun ltv/fixnum (fixnum index read-only-p)
  (add-call "ltvc_make_fixnum" index fixnum))

(defun ltv/bignum (bignum index read-only-p)
  (let ((bn-str (format nil "~a" bignum)))
    (add-call "ltvc_make_bignum" index (load-time-reference-literal bn-str read-only-p))))

(defun ltv/random-state (random-state index read-only-p)
  (let ((rs-str (format nil "~a" (core:random-state-get random-state))))
    (add-call "ltvc_make_random_state" index (load-time-reference-literal rs-str read-only-p))))

(defun ltv/symbol (symbol index read-only-p)
  (let ((pkg (symbol-package symbol))
        (sym-str (symbol-name symbol)))
    (add-call "ltvc_make_symbol" index
              (load-time-reference-literal sym-str read-only-p)
              (load-time-reference-literal pkg read-only-p))))

(defun ltv/character (char index read-only-p)
  (add-call "ltvc_make_character" index
            (jit-constant-i64 (char-code char))))

(defun ltv/base-string (str index read-only-p)
    (add-call "ltvc_make_base_string" index str))

(defun ltv/pathname (pathname index read-only-p)
  (add-call "ltvc_make_pathname" index
            (load-time-reference-literal (pathname-host pathname) read-only-p)
            (load-time-reference-literal (pathname-device pathname) read-only-p)
            (load-time-reference-literal (pathname-directory pathname) read-only-p)
            (load-time-reference-literal (pathname-name pathname) read-only-p)
            (load-time-reference-literal (pathname-type pathname) read-only-p)
            (load-time-reference-literal (pathname-version pathname) read-only-p)))

(defun ltv/package (package index read-only-p)
  (add-call "ltvc_make_package" index
            (load-time-reference-literal (package-name package) read-only-p)))

(defun ltv/built-in-class (class index read-only-p)
  (add-call "ltvc_class" index
            (load-time-reference-literal (class-name class) read-only-p)))

(defun ltv/single-float (single index read-only-p)
  (let* ((constant (llvm-sys:make-apfloat-float single))
         (constant-ap-arg (llvm-sys:constant-fp-get *llvm-context* constant)))
    (add-call "ltvc_make_float" index constant-ap-arg)))

(defun ltv/double-float (double index read-only-p)
  (let* ((constant (llvm-sys:make-apfloat-double double))
         (constant-ap-arg (llvm-sys:constant-fp-get *llvm-context* constant)))
    (add-call "ltvc_make_double" index constant-ap-arg)))

(defun ltv/mlf (object index read-only-p)
  (multiple-value-bind (create initialize)
      (make-load-form object)
    (add-call "ltvc_set_ltv_funcall" index (compile-form create))
    (when initialize
      (add-side-effect-call "ltvc_funcall" (compile-form initialize)))))

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
    ((random-state-p object) (values *everything-else-coalesce* #'ltv/random-state))
    ((core:built-in-class-p object) (values *built-in-class-coalesce* #'ltv/built-in-class))
    ((typep object '(or standard-object structure-object condition class)) (values *everything-else-coalesce* #'ltv/mlf))
    (t (error "Handle object ~a" object))))

(defun make-similarity-table (test)
  (make-hash-table :test test))

(defun find-similar (object table)
  (gethash object table))

(defun add-similar (object index table)
  (setf (gethash object table) index))

(defmacro with-coalesce-ltv (&body body)
  `(let ()
     (progn ,@body)))

(defmacro with-ltv (&body body)
  `(let ((*llvm-values* (make-hash-table))
         (cmp:*load-time-value-holder-global-var*
          (llvm-sys:make-global-variable *the-module*
                                         cmp:+tsp[0]+ ; type
                                         nil          ; isConstant
                                         'llvm-sys:internal-linkage
                                         nil ; initializer
                                         (literal:next-value-table-holder-name "dummy")))
         (cmp:*generate-compile-file-load-time-values* t)
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
         (*double-float-coalesce* (make-similarity-table #'eql))
         (*run-all-objects* nil)
         (*table-index* 0))
     (let* ((top-level-fn (progn ,@body))
            (constants-nodes *run-all-objects*))
       (labels ((ensure-llvm-value (obj)
                  "Lookup or create the llvm::Value for obj"
                  (or (gethash obj *llvm-values*)
                      (setf (gethash obj *llvm-values*)
                            (irc-create-call (literal:constant-creator-name obj)
                                             (fix-args (literal:constant-creator-arguments obj))))))
                (fix-args (args)
                  "Convert the args from Lisp form into llvm::Value*'s"
                  (mapcar (lambda (x)
                            (cond
                              ((fixnump x) (jit-constant-size_t x))
                              ((stringp x) (jit-constant-unique-string-ptr x))
                              ((literal:constant-creator-p x) (ensure-llvm-value x))
                              (t x))) ;;(error "Illegal run-all entry ~a" x))))
                          args)))
         (with-run-all-body-codegen (result)
           (let ((unsorted-values nil))
             (dolist (node constants-nodes)
               #+(or)(bformat t "generate-run-all-code  generating node: %s\n" node)
               (cond
                 ((literal:constant-creator-p node)
                  (let ((val (ensure-llvm-value node)))
                    (push (cons (literal:constant-creator-index node) val) unsorted-values)))
                 ((literal:constant-side-effect-p node)
                  (irc-create-call (literal:constant-side-effect-name node)
                                   (fix-args (literal:constant-side-effect-arguments node))))
                 (t (error "Unknown run-all node ~a" node))))
             (let* ((sorted-llvm-values (mapcar (lambda (x) (cdr x))
                                                (sort unsorted-values #'< :key #'car)))
                    (array-type (llvm-sys:array-type-get cmp:+tsp+ (literal:number-of-entries constants-nodes)))
                    (correct-size-holder (progn
                                           (quick-module-dump *the-module* "/tmp" "a")
                                           (prog1
                                               (llvm-sys:make-global-variable *the-module*
                                                                              array-type
                                                                              nil ; isConstant
                                                                              'llvm-sys:internal-linkage
                                                                              (llvm-sys:undef-value-get array-type)
                                                                              (literal:next-value-table-holder-name))
                                             (quick-module-dump *the-module* "/tmp" "b"))))
                    (bitcast-correct-size-holder (llvm-sys:create-bit-cast *irbuilder* correct-size-holder cmp:+tsp[0]*+ "bitcast-table"))
                    (holder-ptr (llvm-sys:create-geparray *irbuilder* correct-size-holder
                                                          (list (cmp:jit-constant-size_t 0)
                                                                (cmp:jit-constant-size_t 0)) "table")))
               (llvm-sys:replace-all-uses-with cmp:*load-time-value-holder-global-var* bitcast-correct-size-holder)
               (llvm-sys:erase-from-parent cmp:*load-time-value-holder-global-var*) ;
               (cmp:irc-create-call "cc_register_roots" (list* (llvm-sys:create-pointer-cast cmp:*irbuilder* correct-size-holder cmp:+tsp*+ "")
                                                               (cmp:jit-constant-size_t (length sorted-llvm-values))
                                                               sorted-llvm-values))
               (cmp:irc-create-call "ltvc_funcall" (list top-level-fn))
               (quick-module-dump *the-module* "/tmp" "c"))))))))


(defmacro with-rtv (&body body)
  `(let ((cmp:*generate-compile-file-load-time-values* nil)
         (*table-index* 0)
         (*run-time-coalesce* (make-hash-table :test #'eq))
         (*run-all-objects* nil))
     (progn ,@body)
     (reverse *run-all-objects*)))

(defun load-time-reference-literal (object read-only-p)
  (multiple-value-bind (similarity creator)
      (object-similarity-table-and-creator object)
    (if read-only-p
        (let* ((existing (find-similar object similarity)))
          (or existing
              (let* ((index (new-table-index))
                     (new-obj (funcall creator object index read-only-p)))
                (add-similar object new-obj similarity)
                new-obj)))
        (let* ((index (new-table-index))
               (new-obj (funcall creator object index read-only-p)))
          new-obj))))

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
    (add-call "ltvc_set_ltv_funcall" index fn))
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
  (if (generate-load-time-values)
      (let* ((data (load-time-reference-literal object read-only-p)))
        (let ((index (constant-creator-index data)))
          index))
      (let ((data (run-time-reference-literal object read-only-p)))
        (let ((index (constant-runtime-index data)))
          index))))

;;; ------------------------------------------------------------
;;;
;;; functions that are called by bclasp and cclasp that might
;;;  be refactored to simplify the API

(defun compile-reference-to-symbol (symbol)
  "Generate a reference to a load-time-symbol or 
run-time-symbol depending if called from COMPILE-FILE or COMPILE respectively"
  (let ((lts-idx (reference-literal symbol t)))
    (load-time-value-reference (ltv-global) lts-idx (pretty-load-time-name symbol lts-idx))))

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
      (copy-load-time-value result (cmp:ltv-global) index))
    index))
        
(defun codegen-quote (result rest env)
  (cmp-log "codegen-quote: %s\n" rest )
  (codegen-literal result (car rest) env))

(defun compile-reference-to-load-time-value (idx &optional (name "value"))
  (load-time-value-reference (ltv-global) idx name))

(defun compile-reference-to-literal (literal)
  "Generate a reference to a load-time-value or run-time-value literal depending if called from COMPILE-FILE or COMPILE respectively"
  (let ((ltv-idx (reference-literal literal t)))
    (unless (fixnump ltv-idx) (error "Could not compile-reference-to-literal: ~a" literal))
    (compile-reference-to-load-time-value ltv-idx (pretty-load-time-name literal ltv-idx))))


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
                                    cmp:*load-time-value-holder-global-var*
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
  #+(or)(let ((vec-args (make-array 2 :initial-contents (list (jit-constant-size_t 0) (jit-constant-size_t index)))))
    (bformat t "vec-args %s\n" vec-args)
    (llvm-sys:create-geparray *irbuilder* holder vec-args (bformat nil "values-table[%d]" index)))
  (llvm-sys:create-const-gep2-64 *irbuilder* holder 0 index (bformat nil "values-table[%d]" index)))
#|  #+(or)(let* ((tagged-ltv-ptr (irc-load holder "tagged-ltv-ptr"))
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
  #+(or)(let ((holder-ptr (llvm-sys:create-geparray *irbuilder* holder (list (jit-constant-size_t 0) (jit-constant-size_t 0)) "table")))
          (irc-intrinsic "loadTimeValueReference" holder-ptr (jit-constant-size_t index) label))
  #+(or)(let ((vec-args (make-array 2 :initial-contents (list (jit-constant-size_t 0) (jit-constant-size_t index)))))
          (bformat t "vec-args %s\n" vec-args)
          (llvm-sys:create-geparray *irbuilder* holder vec-args (bformat nil "values-table[%d]" index)))
  (llvm-sys:create-const-in-bounds-gep2-64 *irbuilder* holder index 0 (bformat nil "values-table[%d]" index))
  #+(or)(llvm-sys:constant-expr-get-in-bounds-get-element-ptr nil holder (list (jit-constant-size_t 0) (jit-constant-size_t index))))|#

(defun get-load-time-value (result index &optional (holder cmp:*load-time-value-holder-global-var*))
  #+(or)(irc-intrinsic "getLoadTimeValue"
                 result
                 holder
                 (jit-constant-i32 index))
  (let ((ref (load-time-value-reference holder index )))
    (irc-store (irc-load ref) result)))

(defun copy-load-time-value (result holder index)
  #+(or)(irc-intrinsic "copyLoadTimeValue" result holder (jit-constant-size_t index))
  (get-load-time-value result index holder)
  )


