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
#+(or)
(defmacro llog (fmt &rest args)
  `(format *error-output* ,fmt ,@args))
(defmacro llog (fmt &rest args) nil)

(defvar *gcroots-in-module*)
#+threads(defvar *value-table-id-lock* (mp:make-lock :name '*value-table-id-lock*))
(defvar *value-table-id* 0)
(defun incf-value-table-id-value ()
  #+threads(unwind-protect
                (progn
                  (mp:get-lock *value-table-id-lock*)
                  (incf *value-table-id*))
             (mp:giveup-lock *value-table-id-lock*))
  #-threads (incf *value-table-id*))

(defun next-value-table-holder-name (&optional suffix)
  (if suffix
      (bformat nil "%s-%s%d" suffix core:+contab-name+ (incf-value-table-id-value))
      (bformat nil "%s%d" core:+contab-name+ (incf-value-table-id-value))))

(defstruct (literal-node-toplevel-funcall (:type vector) :named) arguments)
(defstruct (literal-node-call (:type vector) :named) function source-pos-info holder)
(defstruct (literal-node-side-effect (:type vector) :named) name arguments)
(defstruct (literal-dnode (:type vector) :named) datum)
(defstruct (literal-node-creator (:type vector) (:include literal-dnode) :named)
  name literal-name object arguments)
(defstruct (literal-node-runtime (:type vector) (:include literal-dnode) :named) object)
(defstruct (literal-node-closure (:type vector) (:include literal-dnode) :named) lambda-name-index function)

(defstruct (function-datum (:type vector) :named) index)
(defstruct (single-float-datum (:type vector) :named) value)
(defstruct (double-float-datum (:type vector) :named) value)
(defstruct (immediate-datum (:type vector) :named) value)
(defstruct (datum (:type vector) :named) kind index)

(defun literal-datum-p (datum)
  (eq (datum-kind datum) :literal))

(defun transient-datum-p (datum)
  (eq (datum-kind datum) :transient))

(defun make-literal-datum (&key index)
  (make-datum :kind :literal :index index))

(defun make-transient-datum ()
  (make-datum :kind :transient :index nil))

(defun upgrade-transient-datum-to-literal (datum)
  (unless (transient-datum-p datum)
    (error "The datum ~s must be a transient" datum))
  (setf (datum-kind datum) :literal)
  (setf (datum-index datum) (new-table-index)))
  
(defun literal-datum-index (datum)
  (unless (literal-datum-p datum)
    (error "The datum ~s must be a literal" datum))
  (datum-index datum))


(defun datum-tag (datum)
  (cond
    ((literal-datum-p datum) #\l)
    ((transient-datum-p datum) #\t)
    (t (error "No tag for datum ~a" datum))))

(defun datum-index-tag-kind (datum &key allow-transients)
  (let ((index (datum-index datum))
        (tag (datum-tag datum))
        (kind (datum-kind datum)))
    (values index tag kind)))


(defun literal-node-index (node)
  (let ((datum (literal-dnode-datum node)))
    (unless (literal-datum-p datum)
      (error "The node ~a has a non-literal datum ~a" node datum))
    (datum-index datum)))

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
        (let* ((datum (literal-dnode-datum node))
               (raw-index (datum-index datum)))
          (when (literal-datum-p datum)
            (setf highest-index (max highest-index raw-index))))))
    (1+ highest-index)))

;;; ------------------------------------------------------------
;;;
;;; Immediate objects don't need to be put into tables
;;;

;;; Return NIL if the object is not immediate
;;; - if it is an immediate then return an immediate-datum object that
;;; contains the tagged immediate value.
(defun immediate-datum-or-nil (original)
  (let ((immediate (core:create-tagged-immediate-value-or-nil original)))
    (if immediate
        (make-immediate-datum :value immediate)
        nil)))

(defvar *table-index*)
(defvar *function-vector*)
(defvar *function-description-vector*)


;;; ------------------------------------------------------------
;;;
;;;

(defvar *ratio-coalesce*)
(defvar *cons-coalesce*)
(defvar *complex-coalesce*)
(defvar *array-coalesce*)
(defvar *hash-table-coalesce*)
(defvar *bignum-coalesce*)
(defvar *symbol-coalesce*)
(defvar *base-string-coalesce*)
(defvar *pathname-coalesce*)
(defvar *package-coalesce*)
(defvar *double-float-coalesce*)
(defvar *identity-coalesce*)
(defvar *constant-datum-to-literal-node-creator*)
(defvar *llvm-values*)

(defvar *with-ltv-depth* 0)

(defun new-table-index (&optional (toplevelp t))
  "Return the next ltv-index. If this is being invoked from COMPILE then
the value is put into *default-load-time-value-vector* and its index is returned"
  (prog1 *table-index*
    (incf *table-index*)))

(defun finalize-transient-datum-indices (objects)
  "Give each datum a unique index"
  (let ((ht (make-hash-table))
        (index 0))
    (dolist (obj objects)
      (when (literal-dnode-p obj)
        (let ((datum (literal-dnode-datum obj)))
          (when (transient-datum-p datum)
            (unless (gethash datum ht)
              (setf (gethash datum ht) index)
              (setf (datum-index datum) index)
;;;              (format t "obj -> ~s~%" obj)
              (incf index))))))
    index))

(defun new-datum (toplevelp)
  (if toplevelp
      (make-literal-datum :index (new-table-index))
      #+(or)(make-literal-datum :index (new-table-index))
      (make-transient-datum)))

(defun lookup-literal-index (object)
  "Given a literal object that has already been added to the literal table and will be recreated at load-time,
return the index in the literal table for that object.  This is used in special cases like defcallback to
rewrite the slot in the literal table to store a closure."
  (maphash (lambda (datum literal)
             (when (eq (literal-node-creator-object literal) object)
               (unless (literal-datum-p datum)
                 (error "lookup-literal-index must passed an literal-datum - instead it got ~a" datum))
               (return-from lookup-literal-index (literal-datum-index datum))))
           *constant-datum-to-literal-node-creator*)
  (error "Could not find literal ~s" object))

(defun add-named-creator (name index literal-name object &rest args)
  "Call the named function after converting fixnum args to llvm constants"
  (let ((creator (make-literal-node-creator :datum index :name name :literal-name literal-name :object object :arguments args)))
    (setf (gethash index *constant-datum-to-literal-node-creator*) creator)
    (run-all-add-node creator)
    creator))

(defun add-creator (name index object &rest args)
  "Call the named function after converting fixnum args to llvm constants"
  (llog "add-creator name ~s object: ~s~%" name object)
  (apply 'add-named-creator name index nil object args))

(defun add-side-effect-call (name &rest args)
  "Call the named function after converting fixnum args to llvm constants"
  (let ((rase (make-literal-node-side-effect :name name :arguments args)))
    (run-all-add-node rase)
    rase))

(defun add-side-effect-call-arglist (name args)
  ;; identical to above, but without &rest
  ;; because args could be tens of thousands long and i'm wary of blowing the stack
  "Call the named function after converting fixnum args to llvm constants"
  (let ((rase (make-literal-node-side-effect :name name :arguments args)))
    (run-all-add-node rase)
    rase))

(defun register-function (llvm-func &optional (func-desc (llvm-sys:constant-pointer-null-get cmp:%function-description*%)))
  "Add a function to the *function-vector* and return its index"
  (let ((function-index (length *function-vector*)))
    (vector-push-extend llvm-func *function-vector*)
    (vector-push-extend func-desc *function-description-vector*)
    (make-function-datum :index function-index)))

;;; Helper function: we write a few things out as base strings.
;;; FIXME: Use a more efficient representation.
(defun prin1-to-base-string (object)
  (with-output-to-string (s nil :element-type 'base-char)
    (prin1 object s)))

(defun ltv/nil (object index read-only-p &key (toplevelp t))
  (add-named-creator "ltvc_make_nil" index "NIL" object))

(defun ltv/t (object index read-only-p &key (toplevelp t))
  (add-named-creator "ltvc_make_t" index "T" object))

(defun ltv/ratio (ratio index read-only-p &key (toplevelp t))
  (add-creator "ltvc_make_ratio" index ratio
               (load-time-reference-literal (numerator ratio) read-only-p :toplevelp nil)
               (load-time-reference-literal (denominator ratio) read-only-p :toplevelp nil)))

(defun ltv/cons (cons index read-only-p &key (toplevelp t))
  ;; While the general case (make_cons) works for all cases,
  ;; it is far from the most efficient way to store a list.
  ;; More importantly, for a long list we will recurse deeply and break the stack.
  ;; So we have other alternatives for that.
  (cond
    ((core:proper-list-p cons)
     (let* ((len (length cons))
            (val (add-creator "ltvc_make_list" index cons len)))
       (add-side-effect-call-arglist
        "ltvc_fill_list"
        (list* val len
               (mapcar (lambda (o)
                         (load-time-reference-literal o read-only-p :toplevelp nil))
                       cons)))
       val))
    (t
     (let ((val (add-creator "ltvc_make_cons" index cons)))
       (add-side-effect-call "ltvc_rplaca" val
                             (load-time-reference-literal (car cons) read-only-p :toplevelp nil))
       (add-side-effect-call "ltvc_rplacd" val
                             (load-time-reference-literal (cdr cons) read-only-p :toplevelp nil))
       val))))

(defun ltv/complex (complex index read-only-p &key (toplevelp t))
  (add-creator "ltvc_make_complex" index complex
               (load-time-reference-literal (realpart complex) read-only-p :toplevelp nil)
               (load-time-reference-literal (imagpart complex) read-only-p :toplevelp nil)))

(defun ltv/array (array index read-only-p &key (toplevelp t))
  (let ((val (add-creator "ltvc_make_array" index array
                          (load-time-reference-literal (array-element-type array) read-only-p :toplevelp nil)
                          (load-time-reference-literal (array-dimensions array) read-only-p :toplevelp nil))))
    (let* ((total-size (if (array-has-fill-pointer-p array)
                           (length array)
                           (array-total-size array))))
      (dotimes (i total-size)
        (add-side-effect-call "ltvc_setf_row_major_aref" val i
                              (load-time-reference-literal (row-major-aref array i) read-only-p :toplevelp nil))))
    val))

(defun ltv/hash-table (hash-table index read-only-p &key (toplevelp t))
  (let ((ht (add-creator "ltvc_make_hash_table" index hash-table
                         (load-time-reference-literal (hash-table-test hash-table) read-only-p :toplevelp nil))))
    (maphash (lambda (key val)
               (add-side-effect-call "ltvc_setf_gethash" ht
                                     (load-time-reference-literal key read-only-p :toplevelp nil)
                                     (load-time-reference-literal val read-only-p :toplevelp nil)))
             hash-table)
    ht))

(defun ltv/fixnum (fixnum index read-only-p &key (toplevelp t))
  (add-creator "ltvc_make_fixnum" index fixnum fixnum))

(defun ltv/bignum (bignum index read-only-p &key (toplevelp t))
  (let ((bn-str (prin1-to-base-string bignum)))
    (add-creator "ltvc_make_bignum" index bignum (load-time-reference-literal bn-str read-only-p :toplevelp nil))))

(defun ltv/bitvector (bitvector index read-only-p &key (toplevelp t))
  (let ((bv-str (prin1-to-base-string bitvector)))
    (add-creator "ltvc_make_bitvector" index bitvector
                 (load-time-reference-literal bv-str read-only-p :toplevelp nil))))

(defun ltv/random-state (random-state index read-only-p &key (toplevelp t))
  (let ((rs-str (core:random-state-get random-state)))
    (add-creator "ltvc_make_random_state" index random-state
                 (load-time-reference-literal rs-str read-only-p :toplevelp nil))))

(defun ltv/symbol (symbol index read-only-p &key (toplevelp t))
  (let ((pkg (symbol-package symbol))
        (sym-str (symbol-name symbol)))
    (add-named-creator "ltvc_make_symbol" index sym-str symbol
                       (load-time-reference-literal sym-str read-only-p :toplevelp nil)
                       (load-time-reference-literal pkg read-only-p :toplevelp nil))))

(defun ltv/character (char index read-only-p &key (toplevelp t))
  (add-creator "ltvc_make_character" index char
               (char-code char)))

(defun ltv/base-string (str index read-only-p &key (toplevelp t))
  (add-creator "ltvc_make_base_string" index str str))

(defun ltv/pathname (pathname index read-only-p &key (toplevelp t))
  (add-creator "ltvc_make_pathname" index pathname
               (load-time-reference-literal (pathname-host pathname) read-only-p :toplevelp nil)
               (load-time-reference-literal (pathname-device pathname) read-only-p :toplevelp nil)
               (load-time-reference-literal (pathname-directory pathname) read-only-p :toplevelp nil)
               (load-time-reference-literal (pathname-name pathname) read-only-p :toplevelp nil)
               (load-time-reference-literal (pathname-type pathname) read-only-p :toplevelp nil)
               (load-time-reference-literal (pathname-version pathname) read-only-p :toplevelp nil)))

(defun ltv/package (package index read-only-p &key (toplevelp t))
  (add-creator "ltvc_make_package" index package
               (load-time-reference-literal (package-name package) read-only-p :toplevelp nil)))

(defun ltv/single-float (single index read-only-p &key (toplevelp t))
  (let* ((constant (make-single-float-datum :value single)))
    (add-creator "ltvc_make_float" index single constant)))

(defun ltv/double-float (double index read-only-p &key (toplevelp t))
  (let* ((constant (make-double-float-datum :value double)))
    (add-creator "ltvc_make_double" index double constant)))

(defun call-with-constant-arguments-p (form &optional env)
  (and (consp form)
       (core:proper-list-p (rest form))
       (symbolp (first form))
       (when (fboundp (first form))
         (and (not (macro-function (first form)))
              (not (special-operator-p (first form)))))
       (every (lambda (f) (constantp f env)) (rest form))))

(defun ltv/mlf (object index read-only-p &key (toplevelp t))
  (multiple-value-bind (create initialize)
      (make-load-form object)
    (prog1
        ;; The compiler is slow, so we try to avoid it for a few common cases.
        (cond
          ((call-with-constant-arguments-p create)
           (apply #'add-creator "ltvc_mlf_create_basic_call" index object
                  (load-time-reference-literal (first create) t :toplevelp nil)
                  (length (rest create))
                  (mapcar (lambda (form)
                            (load-time-reference-literal
                             (ext:constant-form-value form) t :toplevelp nil))
                          (rest create))))
          ;; General case
          (t (let* ((fn (compile-form create))
                    (name (llvm-sys:get-name fn)))
               (add-creator "ltvc_set_mlf_creator_funcall"
                            index object (register-function fn) name))))
      (when initialize
        ;; If the form is a call to a named function, with all constant arguments,
        ;; special case that to avoid the compiler. This covers e.g. the
        ;; initialize-instance calls ASTs have as initialization forms.
        (if (call-with-constant-arguments-p initialize)
            (add-side-effect-call-arglist
             "ltvc_mlf_init_basic_call"
             (list* (load-time-reference-literal (first initialize) t :toplevelp nil)
                    (length (rest initialize))
                    (mapcar (lambda (form)
                              (load-time-reference-literal
                               (ext:constant-form-value form) t :toplevelp nil))
                            (rest initialize))))
            ;; General case.
            (let* ((fn (compile-form initialize))
                   (name (llvm-sys:get-name fn)))
              (add-side-effect-call "ltvc_mlf_init_funcall" (register-function fn) name)))))))

(defun object-similarity-table-and-creator (object read-only-p)
  ;; Note: If an object has modifiable sub-parts, if we are not read-only-p
  ;; we must use the *identity-coalesce* or else the user will see spooky action at a distance.
  (cond
    ((null object) (values *identity-coalesce* #'ltv/nil))
    ((eq t object) (values *identity-coalesce* #'ltv/t))
    ((consp object) (values *cons-coalesce* #'ltv/cons))
    ((fixnump object) (values nil #'ltv/fixnum))
    ((characterp object) (values nil #'ltv/character))
    ((core:single-float-p  object) (values nil #'ltv/single-float))
    ((symbolp object) (values *symbol-coalesce* #'ltv/symbol))
    ((double-float-p object) (values *double-float-coalesce* #'ltv/double-float))
    ((core:ratio-p object) (values *ratio-coalesce* #'ltv/ratio))
    ((bit-vector-p object) (values nil #'ltv/bitvector))
    ((core:base-string-p object)
     (values (if read-only-p *identity-coalesce* *base-string-coalesce*) #'ltv/base-string))
    ((arrayp object)
     (values (if read-only-p *identity-coalesce* *array-coalesce*) #'ltv/array))
    ((hash-table-p object)
     (values (if read-only-p *identity-coalesce* *hash-table-coalesce*) #'ltv/hash-table))
    ((bignump object) (values *bignum-coalesce* #'ltv/bignum))
    ((pathnamep object) (values *pathname-coalesce* #'ltv/pathname))
    ((packagep object) (values *package-coalesce* #'ltv/package))
    ((complexp object) (values *complex-coalesce* #'ltv/complex))
    ((random-state-p object) (values *identity-coalesce* #'ltv/random-state))
    (t (values *identity-coalesce* #'ltv/mlf))))

(defun make-similarity-table (test)
  (make-hash-table :test test))

(defun find-similar (object kind table)
  (gethash object table))

(defun add-similar (object datum kind table)
  (setf (gethash object table) datum))

(defun ensure-creator-llvm-value (obj)
  "Lookup or create the llvm::Value for obj"
  (let ((llvm-value (gethash obj *llvm-values*)))
    (if llvm-value
        llvm-value
        (let* ((datum (literal-dnode-datum obj))
               (index (datum-index datum))
               (tag (datum-tag datum)))
          (setf (gethash obj *llvm-values*)
                (cmp:irc-intrinsic-call (literal-node-creator-name obj)
                                        (list*
                                         *gcroots-in-module*
                                         (cmp:jit-constant-i8 tag)
                                         (cmp:jit-constant-size_t index)
                                         (fix-args (literal-node-creator-arguments obj)))))))))
(defun lookup-arg (creator)
  (labels ((object-label (creator idx &optional (prefix core:+contab-name+))
             (if (literal-node-creator-literal-name creator)
                 (bformat nil "%s[%d]/%s" prefix idx (literal-node-creator-literal-name creator))
                 (bformat nil "%s[%d]%t*" prefix idx))))
    (ensure-creator-llvm-value creator)
    (let ((datum (literal-dnode-datum creator)))
      (multiple-value-bind (index tag kind)
          (datum-index-tag-kind datum)
        (cond
          ((eq kind :literal)
           (let* ((label (object-label creator index))
                  (entry (llvm-sys:create-geparray cmp:*irbuilder*
                                                   cmp:*load-time-value-holder-global-var*
                                                   (list (cmp:jit-constant-i32 0)
                                                         (cmp:jit-constant-i32 index))
                                                   label))
                  (arg (cmp:irc-load entry label)))
             arg))
          ((eq kind :transient)
           (let* ((label (object-label creator index "TRANSIENT"))
                  (arg (cmp:irc-intrinsic-call "ltvc_lookup_transient" (list *gcroots-in-module*
                                                                           (cmp:jit-constant-i8 tag)
                                                                           (cmp:jit-constant-size_t index)) label)))
             arg))
          (t (error "Illegal creator for lookup-arg ~a" creator)))))))

(defun fix-arg (arg)
  (cond
    ((function-datum-p arg) (cmp:jit-constant-i64 (function-datum-index arg)))
    ((fixnump arg) (cmp:jit-constant-i64 arg))
    ((stringp arg) (cmp:jit-constant-unique-string-ptr arg))
    ((literal-node-creator-p arg) (lookup-arg arg))
    ((single-float-datum-p arg) (llvm-sys:constant-fp-get (cmp:thread-local-llvm-context) (llvm-sys:make-apfloat-float (single-float-datum-value arg))))
    ((double-float-datum-p arg) (llvm-sys:constant-fp-get (cmp:thread-local-llvm-context) (llvm-sys:make-apfloat-double (double-float-datum-value arg))))
    ((immediate-datum-p arg) (cmp:irc-maybe-cast-integer-to-t* (immediate-datum-value arg)))
    (t arg)))

(defun fix-args (args)
  "Convert the args from Lisp form into llvm::Value*'s"
  (mapcar #'fix-arg args))

#+(or)
(defun generate-run-all-from-literal-nodes (nodes)
  (cmp::with-make-new-run-all (foo)
    (dolist (node nodes)
      (cond
        ((literal-node-creator-p node)
         (ensure-creator-llvm-value node))
        ((literal-node-side-effect-p node)
         (let* ((fn-name (literal-node-side-effect-name node))
                (args (literal-node-side-effect-arguments node))
                (fix-args (fix-args args)))
           (cmp:irc-intrinsic-call fn-name (list* *gcroots-in-module* fix-args))))
        ((literal-node-toplevel-funcall-p node)
         (cmp:irc-intrinsic-call "ltvc_toplevel_funcall"
                                 (fix-args (literal-node-toplevel-funcall-arguments node))))
        ((literal-node-closure-p node)
         (let ((lambda-name (cmp:irc-intrinsic-call
                             "ltvc_lookup_literal"
                             (list *gcroots-in-module*
                                   (fix-arg (literal-node-closure-lambda-name-index node))))))
           (cmp:irc-intrinsic-call "ltvc_enclose"
                                   (list *gcroots-in-module*
                                         (cmp:jit-constant-size_t (literal-node-index node))
                                         lambda-name
                                         (fix-arg
                                          (register-function (literal-node-closure-function node)))))))
           (t (error "Unknown run-all node ~a" node))))
    foo))

(defun write-argument-byte-code (arg stream byte-index)
  (llog "    write-argument-byte-code arg: ~s byte-index ~d~%" arg byte-index)
  (cond
    ((function-datum-p arg) (core:ltvc-write-size-t (function-datum-index arg) stream byte-index))
    ((fixnump arg) (core:ltvc-write-size-t arg stream byte-index))
    ((characterp arg) (core:ltvc-write-char arg stream byte-index))
    ((stringp arg) (core:ltvc-write-string arg stream byte-index))
    ((immediate-datum-p arg)
     (core:ltvc-write-object #\i (immediate-datum-value arg) stream byte-index))
    ((single-float-datum-p arg) (core:ltvc-write-float (single-float-datum-value arg) stream byte-index))
    ((double-float-datum-p arg) (core:ltvc-write-double (double-float-datum-value arg) stream byte-index))
    ((literal-dnode-p arg)
     (cond
       ((transient-datum-p (literal-dnode-datum arg))
        (core:ltvc-write-object #\t (datum-index (literal-dnode-datum arg)) stream byte-index))
       ((literal-datum-p (literal-dnode-datum arg))
        (core:ltvc-write-object #\l (datum-index (literal-dnode-datum arg)) stream byte-index))
       (t (error "Illegal literal-dnode object ~s" arg))))
    (t (error "Handle object ~s" arg))))

(defun write-arguments-byte-code (arguments stream byte-index)
  (dolist (arg arguments)
    (setf byte-index (write-argument-byte-code arg stream byte-index)))
  byte-index)

(defun lookup-byte-code (name)
  (let ((code (gethash name *byte-codes*)))
    (unless code
      (error "Could not find byte-code for ~a" name))
    code))

(defun write-literal-nodes-byte-code (nodes)
  (let ((fout (make-string-output-stream))
        (byte-index 0))
    (dolist (node nodes)
      (llog "node-> ~s~%" node)
      (cond
        ((literal-node-creator-p node)
         (let* ((datum (literal-dnode-datum node))
                (index (datum-index datum))
                (tag (datum-tag datum)))
           (setf byte-index (core:ltvc-write-char (lookup-byte-code (literal-node-creator-name node)) fout byte-index))
           (setf byte-index (write-arguments-byte-code (list tag index) fout byte-index))
           (setf byte-index (write-arguments-byte-code (literal-node-creator-arguments node) fout byte-index))))
        ((literal-node-side-effect-p node)
         (let* ((fn-name (literal-node-side-effect-name node))
                (args (literal-node-side-effect-arguments node)))
           (setf byte-index (core:ltvc-write-char (lookup-byte-code fn-name) fout byte-index))
           (setf byte-index (write-arguments-byte-code args fout byte-index))))
        ((literal-node-toplevel-funcall-p node)
         (setf byte-index (core:ltvc-write-char (lookup-byte-code "ltvc_toplevel_funcall") fout byte-index))
         (let ((arguments (cdr (literal-node-toplevel-funcall-arguments node))))
           ;;           (format t "About to write arguments for literal-node-toplevel-funcall: ~s~%" arguments)
           (setf byte-index (write-arguments-byte-code arguments fout byte-index))))
        ((literal-node-closure-p node)
         (setf byte-index (core:ltvc-write-char (lookup-byte-code "ltvc_enclose") fout byte-index))
         (warn "What do I do with the arguments for ~s" node)
         (core:exit 1)
         #|         (write-arguments-byte-code (list (literal-node-closure-lambda-name-index node)
         (let ((lambda-name (cmp:irc-intrinsic-call "ltvc_lookup_literal" ; ; ; ; ;
         (list *gcroots-in-module*      ; ; ; ; ;
         (fix-arg (literal-node-closure-lambda-name-index node)))))) ; ; ; ; ;
         (cmp:irc-intrinsic-call "ltvc_enclose" ; ; ; ; ;
         (list *gcroots-in-module*      ; ; ; ; ;
         (cmp:jit-constant-size_t (literal-node-index node)) ; ; ; ; ;
         lambda-name                    ; ; ; ; ;
         (fix-arg (register-function (literal-node-closure-function node))) ; ; ; ; ;
         #|(literal-node-closure-source-info-handle node) ; ; ; ; ;
         (literal-node-closure-filepos node) ; ; ; ; ;
         (literal-node-closure-lineno node) ; ; ; ; ;
         (literal-node-closure-column node)|#)))) |#
         )
        (t (warn "Add support for node ~s" node))))
    (setf byte-index (core:ltvc-write-char (code-char 0) fout byte-index))
    (llog "----------------- done nodes~%")
    (get-output-stream-string fout)))


(defun generate-run-time-code-for-closurette (node irbuilder-alloca array)
  ;; Generate calls to ltvc_enclose for closurettes that are created at JIT startup time
  (declare (ignore array))
  (let ((lambda-name (cmp:irc-intrinsic-call "ltvc_lookup_literal"
                                             (list *gcroots-in-module*
                                                   (fix-arg (literal-node-closure-lambda-name-index node))))))
    (cmp:irc-intrinsic-call "ltvc_enclose"
                            (list *gcroots-in-module*
                                  (cmp:jit-constant-size_t (literal-node-index node))
                                  lambda-name
                                  (fix-arg (register-function (literal-node-closure-function node)))
                                  #|(literal-node-closure-source-info-handle node)
                                  (literal-node-closure-filepos node)
                                  (literal-node-closure-lineno node)
                                  (literal-node-closure-column node)|#))))

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
                          :arguments (list *gcroots-in-module*
                                           (register-function body-return-fn)
                                           (llvm-sys:get-name body-return-fn)))))
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
        (datum (gensym)))
    `(let* ((*with-ltv-depth* (1+ *with-ltv-depth*))
            (,datum (new-datum t))
            (,ltv-func (do-ltv :ltv (lambda () ,@body))))
       (add-creator "ltvc_set_ltv_funcall" ,datum nil (register-function ,ltv-func) (llvm-sys:get-name ,ltv-func))
       (literal-datum-index ,datum))))

(defmacro with-top-level-form (&body body)
  `(let ((*with-ltv-depth* (1+ *with-ltv-depth*)))
     (do-ltv :toplevel (lambda () ,@body))))

(defun do-literal-table (body-fn)
  (llog "do-literal-table~%")
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
                                         nil             ; isConstant
                                         'llvm-sys:internal-linkage
                                         (llvm-sys:undef-value-get cmp:%t*[DUMMY]%)
                                         ;; nil ; initializer
                                         (next-value-table-holder-name "dummy")))
        (*llvm-values* (make-hash-table))
        (cmp:*generate-compile-file-load-time-values* t)
        (*identity-coalesce* (make-similarity-table #'eq))
        (*ratio-coalesce* (make-similarity-table #'eql))
        (*cons-coalesce* (make-similarity-table #'eq))
        (*complex-coalesce* (make-similarity-table #'eql))
        (*array-coalesce* (make-similarity-table #'eq))
        (*hash-table-coalesce* (make-similarity-table #'eq))
        (*bignum-coalesce* (make-similarity-table #'eql))
        (*symbol-coalesce* (make-similarity-table #'eq))
        (*base-string-coalesce* (make-similarity-table #'equal))
        (*pathname-coalesce* (make-similarity-table #'equal))
        (*package-coalesce* (make-similarity-table #'eq))
        (*double-float-coalesce* (make-similarity-table #'eql))
        (*constant-datum-to-literal-node-creator* (make-hash-table :test #'eql))
        (*table-index* 0)
        (*function-vector* (make-array 16 :fill-pointer 0 :adjustable t))
        (*function-description-vector* (make-array 16 :fill-pointer 0 :adjustable t))
        (real-name (next-value-table-holder-name))
        (*run-all-objects* nil))
    (llog "About to evaluate body-fn~%")
    (funcall body-fn)
    ;; Generate the run-all function here
    (llog "About to generate run-all~%")
    (let ((transient-entries (finalize-transient-datum-indices *run-all-objects*)))
      (cmp:with-run-all-body-codegen
          (let ((ordered-run-all-nodes (nreverse *run-all-objects*)))
            (let* ((byte-code-string (write-literal-nodes-byte-code ordered-run-all-nodes))
                   (byte-code-length (length byte-code-string))
                   (byte-code-global (llvm-sys:make-string-global cmp:*the-module* byte-code-string
                                                                  "startup-byte-code")))
              (cmp:irc-intrinsic-call "cc_invoke_byte_code_interpreter"
                                      (list *gcroots-in-module*
                                            (cmp:irc-bit-cast (cmp:irc-gep byte-code-global (list 0 0))
                                                              cmp:%i8*%)
                                            (cmp:jit-constant-size_t byte-code-length)))
              (cmp:irc-intrinsic-call "cc_finish_gcroots_in_module" (list *gcroots-in-module*)))))
      (let ((literal-entries *table-index*))
        (when (> literal-entries 0)
          ;; We have a new table, replace the old one and generate code to register the new one
          ;; and gc roots tabl
          (let* ((array-type (llvm-sys:array-type-get cmp:%t*% literal-entries))
                 (correct-size-holder (llvm-sys:make-global-variable cmp:*the-module*
                                                                     array-type
                                                                     nil ; isConstant
                                                                     'llvm-sys:internal-linkage
                                                                     (llvm-sys:undef-value-get array-type)
                                                                     real-name))
                 (bitcast-correct-size-holder (cmp:irc-bit-cast correct-size-holder cmp:%t*[DUMMY]*%
                                                                "bitcast-table"))
                 (holder-ptr (llvm-sys:create-geparray cmp:*irbuilder* correct-size-holder
                                                       (list (cmp:jit-constant-size_t 0)
                                                             (cmp:jit-constant-size_t 0)) "table")))
            (llvm-sys:replace-all-uses-with cmp:*load-time-value-holder-global-var*
                                            bitcast-correct-size-holder)
            (let* ((function-vector-type (llvm-sys:array-type-get cmp:%fn-prototype*%
                                                                  (length *function-vector*)))
                   (function-vector (llvm-sys:make-global-variable
                                     cmp:*the-module*
                                     function-vector-type
                                     nil
                                     'llvm-sys:internal-linkage
                                     (llvm-sys:constant-array-get function-vector-type
                                                                  (coerce *function-vector* 'list))
                                     "function-vector"))
                   (function-descs-type (llvm-sys:array-type-get
                                         cmp:%function-description*%
                                         (length *function-description-vector*)))
                   (function-descs (llvm-sys:make-global-variable
                                    cmp:*the-module*
                                    function-descs-type
                                    nil
                                    'llvm-sys:internal-linkage
                                    (llvm-sys:constant-array-get
                                     function-descs-type
                                     (coerce *function-description-vector* 'list))
                                    "function-descs")))
              (cmp:with-run-all-entry-codegen
                  (let ((transient-vector (cmp:alloca-i8* "transients")))
                    (cmp:irc-intrinsic-call "cc_initialize_gcroots_in_module"
                                            (list *gcroots-in-module*
                                                  (cmp:irc-pointer-cast correct-size-holder cmp:%t**% "")
                                                  (cmp:jit-constant-size_t literal-entries)
                                                  (cmp:irc-int-to-ptr (cmp:jit-constant-uintptr_t 0)
                                                                      cmp:%t*%)
                                                  transient-vector
                                                  (cmp:jit-constant-size_t transient-entries)
                                                  (cmp:jit-constant-size_t (length *function-vector*))
                                                  (cmp:irc-bit-cast
                                                   (cmp:irc-gep function-vector
                                                                (list (cmp:jit-constant-size_t 0)
                                                                      (cmp:jit-constant-size_t 0)))
                                                   cmp:%i8**%)
                                                  (cmp:irc-bit-cast
                                                   (cmp:irc-gep function-descs
                                                                (list (cmp:jit-constant-size_t 0)
                                                                      (cmp:jit-constant-size_t 0)))
                                                   cmp:%i8**%))))))
            ;; Erase the dummy holder
            (llvm-sys:erase-from-parent cmp:*load-time-value-holder-global-var*)))))))

(defmacro with-literal-table (&body body)
  `(do-literal-table (lambda () ,@body)))



(defun do-rtv (body-fn)
  (let ((cmp:*generate-compile-file-load-time-values* nil)
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
                                         nil         ; isConstant
                                         'llvm-sys:internal-linkage
                                         nil
                                         (next-value-table-holder-name)))
        (*run-time-coalesce* (make-similarity-table #'eq))
        (*run-all-objects* nil))
    (let* ((THE-REPL-FUNCTION (funcall body-fn))
           (run-time-values *run-all-objects*)
           (num-elements (length run-time-values))
           (constant-table nil))
      ;; Put the constants in order they will appear in the table.
      ;; Return the orderered-raw-constants-list and the constants-table GlobalVariable
      (when (> num-elements 0)
        (let* ((ordered-literals-list (sort run-time-values #'< :key #'literal-node-index))
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
                (cmp:codegen-startup-shutdown cmp:*the-module* THE-REPL-FUNCTION *gcroots-in-module* constant-table num-elements ordered-literals-list bitcast-constant-table)
              (values ordered-raw-constant-list constant-table startup-fn shutdown-fn))))))))

(defmacro with-rtv (&body body)
  "Evaluate the code in the body in an environment where run-time values are assigned integer indices
starting from *table-index* into a constants table and the run-time values are accumulated in *run-all-objects*.
References to the run-time values are relative to the *load-time-value-holder-global-var*.
Once the body has evaluated, if there were run-time values accumulated then sort them by index and construct a new
global variable that can hold them all and replace every use of *load-time-value-holder-global-var* with this new constants-table.
Then erase the global variable in *load-time-value-holder-global-var* whether or not run time values were found
and  return the sorted values and the constant-table or (values nil nil)."
  `(funcall #'do-rtv (lambda () (progn ,@body))))

(defun load-time-reference-literal (object read-only-p &key (toplevelp t))
  "If the object is an immediate object return (values immediate nil).
   Otherwise return (values creator T)."
  (llog "load-time-reference-literal object: ~s~%" object)
  (let ((immediate-datum (immediate-datum-or-nil object))
        (desired-kind (if toplevelp :literal :transient)))
    (if immediate-datum
        (progn
          (llog "immediate-datum ~s~%" immediate-datum)
          (let ((val (immediate-datum-value immediate-datum)))
            (llog "immediate-datum value ~s~%" val)
            (values immediate-datum nil)))
        (multiple-value-bind (similarity creator)
            (object-similarity-table-and-creator object read-only-p)
          (llog "non-immediate~%")
          (let ((existing (if similarity (find-similar object desired-kind similarity) nil)))
            (llog "Looking for ~s object ~s   existing --> ~s~%" desired-kind object existing)
            (cond
              (existing
               (when (and (eq desired-kind :literal) (eq :transient (datum-kind existing)))
                 (llog "    upgrading ~s~%" existing)
                 (upgrade-transient-datum-to-literal existing)
                 (llog "    after upgrade: ~s~%" existing))
               (values (gethash existing *constant-datum-to-literal-node-creator*) t))
              ;; Otherwise create a new datum at the current level of transientness
              (t (let ((datum (new-datum toplevelp)))
                   (when similarity (add-similar object datum desired-kind similarity))
                   (values (funcall creator object datum read-only-p :toplevelp toplevelp) t)))))))))

(defun pretty-load-time-name (object ltv-idx)
  (cond
    ((symbolp object) (bformat nil "SYMBOL->%s" object))
    ((consp object) "CONS")
    ((arrayp object) "ARRAY")
    ((numberp object) (format nil "NUMBER->~a" object))
    (t (subseq (bformat nil "ltv-idx_%d_val->%s" ltv-idx object) 0 30))))

;;;---------------------------------------------------------------------
;;;
;;; run time values (i.e., cl:compile)
;;;

(defvar *run-time-coalesce*)

(declaim (ftype (function (t boolean) (values (or immediate-datum literal-node) boolean)) run-time-reference-literal))
(defun run-time-reference-literal (object read-only-p)
  "If the object is an immediate object return (values immediate nil nil).
   Otherwise return (values creator T index)."
  (let ((immediate-datum (immediate-datum-or-nil object)))
    (if immediate-datum
        (values immediate-datum NIL)
        (let* ((similarity *run-time-coalesce*)
               (existing (find-similar object :literal similarity)))
          (if existing
              (values existing T)
              (values (let* ((datum (new-datum t))
                             (new-obj (make-literal-node-runtime :datum datum :object object)))
                        (add-similar object new-obj :literal similarity)
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
                                                       :spi core:*current-source-pos-info*))
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
        (funcall (find-symbol "COMPILE-FORM" "CLASP-CLEAVIR") form))
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
          (llog "result from load-time-reference-literal -> ~s in-array -> ~s~%" data in-array)
          (if in-array
              (progn
                ;;                (format t "reference-literal data -> ~s~%" data)
                (let ((index (literal-node-index data)))
                  (values index T (literal-node-creator-literal-name data))))
              (values (if (immediate-datum-p data)
                          (progn
                            (cmp:irc-maybe-cast-integer-to-t* (immediate-datum-value data)))
                          (error "data must be a immediate-datum - instead its ~s" data))
                      nil)))
        (multiple-value-bind (immediate-datum?literal-node-runtime in-array)
            (run-time-reference-literal object read-only-p)
          (llog "result from run-time-reference-literal -> ~s in-array -> ~s~%" immediate-datum?literal-node-runtime in-array)
          (if in-array
              (let* ((literal-node-runtime immediate-datum?literal-node-runtime)
                     (index (literal-node-index literal-node-runtime)))
                (values index T))
              (let ((immediate-datum immediate-datum?literal-node-runtime))
                (values (if (immediate-datum-p immediate-datum)
                            (cmp:irc-maybe-cast-integer-to-t* (immediate-datum-value immediate-datum))
                            (error "data must be a immediate-datum - instead its ~s" immediate-datum))
                        nil)))))))

;;; ------------------------------------------------------------
;;;
;;; reference-closure
;;;
;;; Returns an index for a closure.
;;; We skip similarity testing etc. This could be improved (FIXME)
;;; We could also add the capability to dump actual closures, though
;;;  I'm not sure why we'd want to do so.

#+(or)
(defun reference-closure (lambda-name enclosed-function source-info-handle
                          filepos lineno column)
  (if (cmp:generate-load-time-values)
      (multiple-value-bind (lambda-name-node in-array)
          (load-time-reference-literal lambda-name t)
        (unless in-array
          (error "BUG: Immediate lambda-name ~a- What?" lambda-name))
        (let* ((datum (new-datum t))
               (creator (make-literal-node-closure
                         ;; lambda-name will never be immediate. (Should we check?)
                         :lambda-name-index (literal-node-index lambda-name-node)
                         :datum datum :function enclosed-function
                         :source-info-handle source-info-handle
                         :filepos filepos :lineno lineno :column column)))
          (run-all-add-node creator)
          index))
      (multiple-value-bind (lambda-name-node in-array)
          (run-time-reference-literal lambda-name t)
        (unless in-array
          (error "BUG: Immediate lambda-name ~a- What?" lambda-name))
        (let* ((datum (new-datum t))
               (creator (make-literal-node-closure
                         ;; lambda-name will never be immediate. (Should we check?)
                         :lambda-name-index (literal-node-index lambda-name-node)
                         :datum datum :function enclosed-function
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
  (multiple-value-bind (immediate-datum?literal-node-runtime in-array)
      (run-time-reference-literal obj t)
    (if in-array
        (let* ((literal-node-runtime immediate-datum?literal-node-runtime)
               (index (literal-node-index literal-node-runtime)))
          (cmp:irc-t*-result (constants-table-value index) result)
          index)
        (let ((immediate-datum immediate-datum?literal-node-runtime))
          (cmp:irc-t*-result (cmp:jit-constant-i64 (immediate-datum-value immediate-datum)) result)
          :poison-value-from-codegen-rtv-bclasp))))

(defun codegen-rtv-cclasp (obj)
  "bclasp calls this to get copy the run-time-value for obj into result.
Returns (value index t) if the value was put in the literal vector or it
returns (value immediate nil) if the value is an immediate value."
  (multiple-value-bind (immediate-datum?literal-node-runtime in-array)
      (run-time-reference-literal obj t)
    (if in-array
        (let* ((literal-node-runtime immediate-datum?literal-node-runtime)
               (index (literal-node-index literal-node-runtime)))
          (values index t))
        (let ((immediate-datum immediate-datum?literal-node-runtime))
          (values (immediate-datum-value immediate-datum) nil)))))

(defun codegen-literal (result object env)
  "This is called by bclasp.  If result is nil then just return the ltv index.
If it isn't NIL then copy the literal from its index in the LTV into result."
  (multiple-value-bind (data-or-index in-array)
      (reference-literal object t)
    (if in-array
        (progn
          (when result
            (cmp:irc-t*-result (constants-table-value data-or-index) result))
          data-or-index)
        (progn
          (when result
            (cmp:irc-t*-result data-or-index result))
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
                                                       :spi core:*current-source-pos-info*))
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



;;; ------------------------------------------------------------
;;;
;;; Bytecode interpreter generator
;;;
;;;

(defparameter *machine* cmp:*startup-primitives-as-list*)

(defstruct (c++-info (:type vector) :named) type c++-type suffix gcroots)

(defun set-c++-info (symbol c++-type suffix &optional gcroots)
  (setf (gethash symbol *c++-info*) (make-c++-info :type symbol :c++-type c++-type :suffix suffix :gcroots gcroots)))

(defparameter *c++-info* (make-hash-table :test #'equal))
(eval-when (:load-toplevel :execute)
  (set-c++-info 'cmp:%i8% "char" "char")
  (set-c++-info 'cmp:%size_t% "size_t" "size_t")
  (set-c++-info 'cmp:%t*% "T_O*" "object" t)
  (set-c++-info 'cmp:%i8*% "string" "string")
  (set-c++-info 'cmp:%float% "float" "float")
  (set-c++-info 'cmp:%double% "double" "double")
  (set-c++-info 'cmp:%uintptr_t% "uintptr_t" "size_t")
  (set-c++-info :unknown "UNKNOWN" "UNKNOWN")
  )

(defun build-one-c++-function (op &optional (stream *standard-output*))
  (destructuring-bind (op-kind name return-type argument-types &key varargs ltvc)
      op
    (let ((index (second argument-types))
          (arg-types (nthcdr 2 argument-types)))
      (format stream "void parse_~a(gctools::GCRootsInModule* roots, T_sp fin, bool log, size_t& byte_index) {~%" name)
      (format stream "  if (log) printf(\"%s:%d:%s parse_~a\\n\", __FILE__, __LINE__, __FUNCTION__);~%" name)
      (let* ((arg-index 0)
             (vars (let (names)
                     (dolist (arg-type arg-types)
                       (let* ((c++-info (let ((info (gethash arg-type *c++-info*)))
                                          (if info
                                              info
                                              (make-c++-info :type (format nil "UNKNOWN_~a" arg-type)
                                                             :c++-type (format nil "UNKNOWN_~a" arg-type)
                                                             :suffix (format nil "UNKNOWN_~a" arg-type)))))
                              (c++-arg-type (c++-info-c++-type c++-info))
                              (suffix (c++-info-suffix c++-info))
                              (gcroots (c++-info-gcroots c++-info))
                              (variable-name (cond
                                               ((and (= arg-index 0) (string= c++-arg-type "char"))  "tag")
                                               ((and (= arg-index 1) (string= c++-arg-type "size_t")) "index")
                                               (t (format nil "arg~a" arg-index))))
                              (read-variable-name (if (string= c++-arg-type "string")
                                                      (format nil "~a.c_str()" variable-name)
                                                      variable-name)))
                         (format stream "  ~a ~a = ltvc_read_~a(~a fin, log, byte_index );~%" c++-arg-type variable-name
                                 suffix
                                 (if gcroots
                                     "roots, "
                                     ""))
                         (incf arg-index)
                         (push read-variable-name names)))
                     (nreverse names))))
        (when varargs
          (setf name (format nil "~a_varargs" name))
          (format stream "  Cons_O* varargs = ltvc_read_list( roots, ~a, fin, log, byte_index );~%" (car (last vars )))
          (setf vars (append vars (list "varargs"))))
        (format stream "  ~a( roots" name)
        (dolist (var vars)
          (format stream ", ~a" var))
        (format stream ");~%")
        (format stream "};~%")))))

(defun build-c++-functions (primitives &optional (stream *standard-output*))
  (format stream "#ifdef DEFINE_PARSERS~%")
  (dolist (prim primitives)
    (build-one-c++-function prim stream))
  (format stream "#endif // DEFINE_PARSERS~%"))

(defun build-c++-switch (primitives &optional (stream *standard-output*))
  (format stream "#ifdef DEFINE_SWITCH~%")
  (let ((code 65))
    (dolist (prim primitives)
      (let ((func-name (second prim)))
        (format stream "  case ~a: parse_~a(roots,fin,log,byte_index);~%" code func-name)
        (format stream "           break;~%")
        (incf code))))
  (format stream "#endif // DEFINE_SWITCH~%"))

(defun build-c++-byte-codes (primitives)
  (let ((map (make-hash-table :test #'equal)))
    (let ((code 65))
      (dolist (prim primitives)
        (let ((func-name (second prim)))
          (setf (gethash (second prim) map) code)
          (incf code))))
    map))



(defun build-c++-machine (&optional (stream *standard-output*))
  (build-c++-functions *machine* stream)
  (build-c++-switch *machine* stream))


(defvar *byte-codes* (build-c++-byte-codes cmp:*startup-primitives-as-list*))


