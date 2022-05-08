(in-package :literal)

;;;; implementation of load-time-value and literal logic in compile-file and compile
;;;;
;;;; Conceptually, there are two separate components, the "compiler" of ltv bytecode and
;;;;  the ltv bytecode interpreter.
;;;; The compiler compiles common lisp forms. Whenever it runs into a literal it can't
;;;;  handle, it calls LTV. When LTV needs some code compiled, it calls the compiler.
;;;; LTV forms code in bytecode that is evaluated from the @RUN-ALL function to create, at load time,
;;;; a table (vector) with all the values the rest of the code needs to run.
;;;; Once it's done compiling, the compiler checks with LTV to get a table size and
;;;;  the table-initializing bytecode to run, which is injected into a couple of global variables
;;;;  in the module.
;;;;
;;;; External protocol:
;;;;
;;;; * When the compiler runs into a literal, it calls REFERENCE-LITERAL. This will
;;;;   return, an index into the load-time-values table for run-time.
;;;; * The compiler can generate literals as structs and then the literal compiler can recognize 
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
(defmacro llog (fmt &rest args) (declare (ignore fmt args)))

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

(defun next-value-table-holder-name (module-id &optional suffix)
  (if suffix
      (bformat nil "%s-%s%d" suffix core:+literals-name+ module-id)
      (bformat nil "%s%d" core:+literals-name+ module-id)))

(defstruct (literal-node-toplevel-funcall (:type vector) :named) arguments)
(defstruct (literal-node-call (:type vector) :named) function source-pos-info holder)
(defstruct (literal-node-side-effect (:type vector) :named) name arguments)
(defstruct (literal-dnode (:type vector) :named) datum)
(defstruct (literal-node-creator (:type vector) (:include literal-dnode) :named)
  name literal-name object arguments)
(defstruct (literal-node-runtime (:type vector) (:include literal-dnode) :named) object)
(defstruct (literal-node-closure (:type vector) (:include literal-dnode) :named) function-index function entry-point-ref)

(defstruct (function-datum (:type vector) :named) index)
(defstruct (single-float-datum (:type vector) :named) value)
(defstruct (double-float-datum (:type vector) :named) value)
(defstruct (immediate-datum (:type vector) :named) value)
(defstruct (datum (:type vector) :named) kind index literal-node-creator)

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

(defun datum-index-tag-kind (datum)
  (let ((index (datum-index datum))
        (tag (datum-tag datum))
        (kind (datum-kind datum)))
    (values index tag kind)))


(defun literal-node-index (node)
  (let ((datum (literal-dnode-datum node)))
    (unless (literal-datum-p datum)
      (error "The node ~a has a non-literal datum ~a" node datum))
    (datum-index datum)))

(defparameter *literal-machine* nil)

(defun run-all-add-node (node)
  (vector-push-extend node (literal-machine-run-all-objects *literal-machine*))
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



(defun make-similarity-table (test)
  (make-hash-table :test test))

(defun find-similar (object table)
  (gethash object table))

(defun add-similar (object datum table)
  (setf (gethash object table) datum))


(defstruct (literal-machine (:type vector) :named)
  (run-all-objects (make-array 64 :fill-pointer 0 :adjustable t))
  (table-index 0)
  (function-vector (make-array 16 :fill-pointer 0 :adjustable t))
  (constant-data '())
  (identity-coalesce (make-similarity-table #'eq))
  (ratio-coalesce (make-similarity-table #'eql))
  (cons-coalesce (make-similarity-table #'eq))
  (complex-coalesce (make-similarity-table #'eql))
  (array-coalesce (make-similarity-table #'eq))
  (hash-table-coalesce (make-similarity-table #'eq))
  (bignum-coalesce (make-similarity-table #'eql))
  (symbol-coalesce (make-similarity-table #'eq))
  (base-string-coalesce (make-similarity-table #'equal))
  (pathname-coalesce (make-similarity-table #'equal))
  (function-description-coalesce (make-similarity-table #'equal))
  (entry-point-coalesce (make-similarity-table #'eq))
  (package-coalesce (make-similarity-table #'eq))
  (double-float-coalesce (make-similarity-table #'eql))
  (llvm-values (make-hash-table))
)


;;; ------------------------------------------------------------
;;;
;;;

(defun new-table-index ()
  "Return the next ltv-index. If this is being invoked from COMPILE then
the value is put into *default-load-time-value-vector* and its index is returned"
  (prog1 (literal-machine-table-index *literal-machine*)
    (incf (literal-machine-table-index *literal-machine*))))

(defun finalize-transient-datum-indices (literal-machine)
  "Give each datum a unique index"
  (let* ((count (length (literal-machine-run-all-objects literal-machine)))
         (ht (make-hash-table :size count))
         (index 0))
    (dotimes (lm-index count)
      (let ((obj (elt (literal-machine-run-all-objects literal-machine) lm-index)))
        (when (literal-dnode-p obj)
          (let ((datum (literal-dnode-datum obj)))
            (when (transient-datum-p datum)
              (unless (gethash datum ht)
                (setf (gethash datum ht) index)
                (setf (datum-index datum) index)
;;;              (format t "obj -> ~s~%" obj)
                (incf index)))))))
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
  (dolist (datum (literal-machine-constant-data *literal-machine*))
    (when (eq (literal-node-creator-object (datum-literal-node-creator datum)) object)
      (unless (literal-datum-p datum)
        (error "lookup-literal-index must passed an literal-datum - instead it got ~a" datum))
      (return-from lookup-literal-index (literal-datum-index datum))))
  (error "Could not find literal ~s" object))

(defun add-named-creator (name index literal-name object &rest args)
  "Call the named function after converting fixnum args to llvm constants"
  (let* ((creator (make-literal-node-creator :datum index :name name :literal-name literal-name :object object :arguments args))
         (primitive (gethash name cmp:*primitives*))
         (varargs (cmp:primitive-varargs primitive))
         (argument-types (cmp:primitive-argument-types primitive)))
    (when (and (not varargs) (/= (length args) (- (length argument-types) 3)))
      (error "You did not provide correct arguments for the primitive ~a~%  varargs: ~a~%  passed arguments: ~a~%  needs arguments after third: ~a"
             name varargs args argument-types))
    (setf (datum-literal-node-creator index) creator)
    (push index (literal-machine-constant-data *literal-machine*))
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

(defstruct (general-entry-placeholder (:type vector) :named)
  arity
  name
  cleavir-lambda-list-analysis
  )

(defun ensure-not-placeholder (obj)
  (when (general-entry-placeholder-p obj)
    (error "The obj ~a must not be a general-entry-placeholder" obj))
  obj)

(defun entry-point-datum-for-xep-group (xep-group)
  (unless (cmp:xep-group-p xep-group)
    (error "The argument ~a must be a xep-group" xep-group))
  (make-function-datum :index (cmp:entry-point-reference-index (cmp:xep-group-entry-point-reference xep-group))))
  
(defun register-function->function-datum-impl (function)
  "Add a function to the (literal-machine-function-vector *literal-machine*)"
  (unless (or (typep function 'llvm-sys:function)
              (general-entry-placeholder-p function))
    (error "In register-function->function-datum-impl function ~s of ~s must be a function or a general-entry-placeholder" function (class-of function)))
  (when (general-entry-placeholder-p function)
    ;; Lookup a wrong-number-of-arguments function and use that
    (let* ((wna-arity (general-entry-placeholder-arity function))
           (wna-function-name (cmp:general-entry-point-redirect-name wna-arity))
           (wna-function (cmp:get-or-declare-function-or-error cmp:*the-module* wna-function-name)))
      (setf function wna-function)))
  ;; Functions are laid out in linear order and xep-functions have all their entry points
  ;; consecutive, one for each entry point
  (let ((function-index (length (literal-machine-function-vector *literal-machine*))))
    (vector-push-extend function (literal-machine-function-vector *literal-machine*))
    (make-function-datum :index function-index)))

(defun register-local-function->function-datum (local-fn)
  "Add a function to the (literal-machine-function-vector *literal-machine*)"
  (unless (typep local-fn 'llvm-sys:function)
    (error "This ~a must be a llvm-sys:function - it is not" local-fn))
  (register-function->function-datum-impl local-fn))


(defun register-xep-function->function-datums (f-or-p-list)
  "Add a function to the (literal-machine-function-vector *literal-machine*)"
  (let ((rev-datums nil))
    (dolist (xep-function-or-placeholder f-or-p-list)
      (push (register-function->function-datum-impl xep-function-or-placeholder) rev-datums))
    ;; Make sure that all function indices are consecutive
    (let ((datums (nreverse rev-datums))
          (prev-function-index nil))
      (dolist (datum datums)
        (let ((cur-function-index (function-datum-index datum)))
          (when prev-function-index
            (unless (= (1+ prev-function-index) cur-function-index)
              (error "The function indices for a xep-function are not consecutive")))
          (setf prev-function-index cur-function-index)))
      datums)))

(defun register-xep-function-indices (xep-arity-list)
  "Add all functions in xep-function to the (literal-machine-function-vector *literal-machine*)"
  (unless (listp xep-arity-list)
    (error "Argument to register-xep-function-indices ~s must be a list" xep-arity-list))
  (let ((f-or-p-list (mapcar 'cmp:xep-arity-function-or-placeholder xep-arity-list)))
    (unless (every (lambda (f-or-p)
                     (or (typep f-or-p 'llvm-sys:function)
                         (general-entry-placeholder-p f-or-p)))
                   f-or-p-list)
      (error "The argument must be a list of functions or placeholders - it's a ~a" xep-arity-list))
    (let ((function-datums (register-xep-function->function-datums f-or-p-list)))
      (mapcar 'function-datum-index function-datums))))

(defun register-local-function-index (local-function)
  "Add a local function to the literal-machine-function-vector"
  (function-datum-index (register-local-function->function-datum local-function)))

;;; Helper function: we write a few things out as base strings.
;;; FIXME: Use a more efficient representation.
(defun prin1-to-base-string (object)
  (with-output-to-string (s nil :element-type 'base-char)
    (prin1 object s)))

(defun ltv/nil (object index read-only-p &key (toplevelp t))
  (declare (ignore toplevelp read-only-p))
  (add-named-creator "ltvc_make_nil" index "NIL" object))

(defun ltv/t (object index read-only-p &key (toplevelp t))
  (declare (ignore toplevelp read-only-p))
  (add-named-creator "ltvc_make_t" index "T" object))

(defun ltv/ratio (ratio index read-only-p &key (toplevelp t))
  (declare (ignore toplevelp))
  (add-creator "ltvc_make_ratio" index ratio
               (load-time-reference-literal (numerator ratio) read-only-p :toplevelp nil)
               (load-time-reference-literal (denominator ratio) read-only-p :toplevelp nil)))

(defun ltv/cons (cons index read-only-p &key (toplevelp t))
  (declare (ignore toplevelp))
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
  (declare (ignore toplevelp))
  (add-creator "ltvc_make_complex" index complex
               (load-time-reference-literal (realpart complex) read-only-p :toplevelp nil)
               (load-time-reference-literal (imagpart complex) read-only-p :toplevelp nil)))

(defun ltv/array (array index read-only-p &key (toplevelp t))
  (declare (ignore toplevelp))
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
  (declare (ignore toplevelp))
  (let ((ht (add-creator "ltvc_make_hash_table" index hash-table
                         (load-time-reference-literal (hash-table-test hash-table) read-only-p :toplevelp nil))))
    (maphash (lambda (key val)
               (add-side-effect-call "ltvc_setf_gethash" ht
                                     (load-time-reference-literal key read-only-p :toplevelp nil)
                                     (load-time-reference-literal val read-only-p :toplevelp nil)))
             hash-table)
    ht))

(defun ltv/fixnum (fixnum index read-only-p &key (toplevelp t))
  (declare (ignore toplevelp read-only-p))
  (add-creator "ltvc_make_fixnum" index fixnum fixnum))

(defun ltv/bignum (bignum index read-only-p &key (toplevelp t))
  (declare (ignore toplevelp read-only-p))
  (add-creator "ltvc_make_next_bignum" index bignum bignum))

(defun ltv/bitvector (bitvector index read-only-p &key (toplevelp t))
  (declare (ignore toplevelp))
  (let ((bv-str (prin1-to-base-string bitvector)))
    (add-creator "ltvc_make_bitvector" index bitvector
                 (load-time-reference-literal bv-str read-only-p :toplevelp nil))))

(defun ltv/random-state (random-state index read-only-p &key (toplevelp t))
  (declare (ignore toplevelp))
  (let ((rs-str (core:random-state-get random-state)))
    (add-creator "ltvc_make_random_state" index random-state
                 (load-time-reference-literal rs-str read-only-p :toplevelp nil))))

(defun ltv/symbol (symbol index read-only-p &key (toplevelp t))
  (declare (ignore toplevelp))
  (let ((pkg (symbol-package symbol))
        (sym-str (symbol-name symbol)))
    (add-named-creator "ltvc_make_symbol" index sym-str symbol
                       (load-time-reference-literal sym-str read-only-p :toplevelp nil)
                       (load-time-reference-literal pkg read-only-p :toplevelp nil))))

(defun ltv/character (char index read-only-p &key (toplevelp t))
  (declare (ignore toplevelp read-only-p))
  (add-creator "ltvc_make_character" index char
               (char-code char)))

(defun ltv/base-string (str index read-only-p &key (toplevelp t))
  (declare (ignore toplevelp read-only-p))
  (add-creator "ltvc_make_base_string" index str str))

(defun ltv/pathname (pathname index read-only-p &key (toplevelp t))
  (declare (ignore toplevelp))
  (add-creator "ltvc_make_pathname" index pathname
               (load-time-reference-literal (pathname-host pathname) read-only-p :toplevelp nil)
               (load-time-reference-literal (pathname-device pathname) read-only-p :toplevelp nil)
               (load-time-reference-literal (pathname-directory pathname) read-only-p :toplevelp nil)
               (load-time-reference-literal (pathname-name pathname) read-only-p :toplevelp nil)
               (load-time-reference-literal (pathname-type pathname) read-only-p :toplevelp nil)
               (load-time-reference-literal (pathname-version pathname) read-only-p :toplevelp nil)))

(defun ltv/function-description (fdesc-ph index read-only-p &key (toplevelp t))
  (declare (ignore toplevelp))
  (add-creator "ltvc_make_function_description" index fdesc-ph
               (load-time-reference-literal (sys:function-description-source-pathname fdesc-ph) read-only-p :toplevelp nil)
               (load-time-reference-literal (sys:function-description-function-name fdesc-ph) read-only-p :toplevelp nil)
               (load-time-reference-literal (sys:function-description-lambda-list fdesc-ph) read-only-p :toplevelp nil)
               (load-time-reference-literal (sys:function-description-docstring fdesc-ph) read-only-p :toplevelp nil)
               (load-time-reference-literal (sys:function-description-declares fdesc-ph) read-only-p :toplevelp nil)
               (sys:function-description-lineno fdesc-ph)
               (sys:function-description-column fdesc-ph)
               (sys:function-description-filepos fdesc-ph)))

(defun ltv/local-entry-point (entry-point index read-only-p &key (toplevelp t))
  (declare (ignore toplevelp))
  (let ((function-index (first (sys:local-entry-point-generator-entry-point-indices entry-point))))
    (add-creator "ltvc_make_local_entry_point" index entry-point
                 function-index
                 (load-time-reference-literal (sys:entry-point-base-function-description entry-point) read-only-p :toplevelp nil))))

(defun ltv/global-entry-point (entry-point index read-only-p &key (toplevelp t))
  (declare (ignore toplevelp))
  (let ((function-index (first (sys:global-entry-point-generator-entry-point-indices entry-point)))
        (local-entry-point-index (sys:global-entry-point-generator-local-entry-point-index entry-point)))
    (add-creator "ltvc_make_global_entry_point" index entry-point
                 function-index
                 (load-time-reference-literal (sys:entry-point-base-function-description entry-point) read-only-p :toplevelp nil)
                 local-entry-point-index #+(or)(load-time-reference-literal (sys:global-entry-point-local-entry-point entry-point) read-only-p :toplevelp nil))))

(defun ltv/package (package index read-only-p &key (toplevelp t))
  (declare (ignore toplevelp))
  (add-creator "ltvc_make_package" index package
               (load-time-reference-literal (package-name package) read-only-p :toplevelp nil)))

(defun ltv/single-float (single index read-only-p &key (toplevelp t))
  (declare (ignore toplevelp read-only-p))
  (let* ((constant (make-single-float-datum :value single)))
    (add-creator "ltvc_make_float" index single constant)))

(defun ltv/double-float (double index read-only-p &key (toplevelp t))
  (declare (ignore toplevelp read-only-p))
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
  (declare (ignore toplevelp read-only-p))
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
                    (name (cmp:xep-group-name fn)))
               (add-creator "ltvc_set_mlf_creator_funcall"
                            index object (entry-point-datum-for-xep-group fn) name))))
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
                   (name (cmp:xep-group-name fn)))
              (add-side-effect-call "ltvc_mlf_init_funcall" (entry-point-datum-for-xep-group fn) name)))))))

(defun object-similarity-table-and-creator (literal-machine object read-only-p)
  ;; Note: If an object has modifiable sub-parts, if we are not read-only-p
  ;; we must use the (literal-machine-identity-coalesce literal-machine) or else the user will see spooky action at a distance.
  (cond
    ((null object) (values (literal-machine-identity-coalesce literal-machine) #'ltv/nil))
    ((eq t object) (values (literal-machine-identity-coalesce literal-machine) #'ltv/t))
    ((consp object) (values (literal-machine-cons-coalesce *literal-machine*) #'ltv/cons))
    ((fixnump object) (values nil #'ltv/fixnum))
    ((characterp object) (values nil #'ltv/character))
    ((core:single-float-p  object) (values nil #'ltv/single-float))
    ((symbolp object) (values (literal-machine-symbol-coalesce literal-machine) #'ltv/symbol))
    ((double-float-p object) (values (literal-machine-double-float-coalesce literal-machine) #'ltv/double-float))
    ((core:ratiop object) (values (literal-machine-ratio-coalesce literal-machine) #'ltv/ratio))
    ((sys:function-description-p object) (values (literal-machine-function-description-coalesce literal-machine) #'ltv/function-description))
    ((sys:local-entry-point-generator-p object) (values (literal-machine-function-description-coalesce literal-machine) #'ltv/local-entry-point))
    ((sys:global-entry-point-generator-p object) (values (literal-machine-function-description-coalesce literal-machine) #'ltv/global-entry-point))
    ((bit-vector-p object) (values nil #'ltv/bitvector))
    ((core:base-string-p object)
     (values (if read-only-p (literal-machine-identity-coalesce literal-machine) (literal-machine-base-string-coalesce literal-machine)) #'ltv/base-string))
    ((arrayp object)
     (values (if read-only-p (literal-machine-identity-coalesce literal-machine) (literal-machine-array-coalesce literal-machine)) #'ltv/array))
    ((hash-table-p object)
     (values (if read-only-p (literal-machine-identity-coalesce literal-machine) (literal-machine-hash-table-coalesce literal-machine)) #'ltv/hash-table))
    ((bignump object) (values (literal-machine-bignum-coalesce literal-machine) #'ltv/bignum))
    ((pathnamep object) (values (literal-machine-pathname-coalesce literal-machine) #'ltv/pathname))
    ((packagep object) (values (literal-machine-package-coalesce literal-machine) #'ltv/package))
    ((complexp object) (values (literal-machine-complex-coalesce literal-machine) #'ltv/complex))
    ((random-state-p object) (values (literal-machine-identity-coalesce literal-machine) #'ltv/random-state))
    (t #+(or)(warn "object-similarity-table-and-creator object -> ~s" object)
       (values (literal-machine-identity-coalesce literal-machine) #'ltv/mlf))))

(defun ensure-creator-llvm-value (obj)
  "Lookup or create the llvm::Value for obj"
  (let ((llvm-value (gethash obj (literal-machine-llvm-values *literal-machine*))))
    (if llvm-value
        llvm-value
        (let* ((datum (literal-dnode-datum obj))
               (index (datum-index datum))
               (tag (datum-tag datum)))
          (setf (gethash obj (literal-machine-llvm-values *literal-machine*))
                (cmp:irc-intrinsic-call (literal-node-creator-name obj)
                                        (list*
                                         *gcroots-in-module*
                                         (cmp:jit-constant-i8 tag)
                                         (cmp:jit-constant-size_t index)
                                         (fix-args (literal-node-creator-arguments obj)))))))))
(defun lookup-arg (creator)
  (labels ((object-label (creator idx &optional (prefix core:+literals-name+))
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
                  (entry (let ((type (llvm-sys:get-pointer-element-type
                                      (llvm-sys:get-scalar-type
                                       (llvm-sys:get-type
                                        cmp:*load-time-value-holder-global-var*)))))
                           (llvm-sys:create-geparray cmp:*irbuilder*
                                                     type
                                                     cmp:*load-time-value-holder-global-var*
                                                     (list (cmp:jit-constant-i32 0)
                                                           (cmp:jit-constant-i32 index))
                                                     label)))
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

(defun write-argument-byte-code (arg stream byte-index)
  (llog "    write-argument-byte-code arg: ~s byte-index ~d~%" arg byte-index)
  (cond
    ((function-datum-p arg) (core:ltvc-write-size-t (function-datum-index arg) stream byte-index))
    ((fixnump arg) (core:ltvc-write-size-t arg stream byte-index))
    ((characterp arg) (core:ltvc-write-char arg stream byte-index))
    ((stringp arg) (core:ltvc-write-string arg stream byte-index))
    ((core:bignump arg) (core:ltvc-write-bignum arg stream byte-index))
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
    (t (error "write-argument-byte-code: handle object ~s" arg))))

(defun write-arguments-byte-code (arguments stream byte-index)
  (dolist (arg arguments)
    (setf byte-index (write-argument-byte-code arg stream byte-index)))
  byte-index)

(defun lookup-byte-code (name)
  (let ((code (gethash name *byte-codes*)))
    (unless code
      (error "Could not find byte-code for ~a" name))
    code))

(defun write-literal-node-byte-code (fout node byte-index)
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
     (error "What do I do with the arguments for ~s" node)
     (core:exit 1)
     )
    (t (warn "Add support for node ~s" node)))
  byte-index)


(defun write-literal-nodes-byte-code (nodes)
  (let ((fout (make-string-output-stream))
        (byte-index 0))
    (dolist (node nodes)
      (llog "node-> ~s~%" node)
      (setf byte-index (write-literal-node-byte-code fout node byte-index)))
    (setf byte-index (core:ltvc-write-char (code-char 0) fout byte-index))
    (llog "----------------- done nodes~%")
    (get-output-stream-string fout)))


(defun generate-run-time-code-for-closurette (node)
  ;; Generate calls to ltvc_make_closurette for closurettes that are created at JIT startup time
  (let* ((closurette-object (literal-node-creator-object node))
         (datum (literal-dnode-datum closurette-object))
         (index (datum-index datum))
         (entry-point-ref (literal-node-closure-entry-point-ref closurette-object)))
    (cmp:irc-intrinsic-call "ltvc_make_closurette"
                            (list *gcroots-in-module*
                                  (cmp:jit-constant-i8 cmp:+literal-tag-char-code+)
                                  (cmp:jit-constant-size_t index)
                                  (cmp:jit-constant-size_t (cmp:entry-point-reference-index entry-point-ref))))))

(defparameter *ltv-trap* nil)

(defun load-time-value-from-thunk (thunk)
  "Arrange to evaluate the thunk into a load-time-value.
Return the index of the load-time-value"
  (let ((datum (new-datum t)))
    (add-creator "ltvc_set_ltv_funcall" datum nil (entry-point-datum-for-xep-group thunk) (cmp:xep-group-name thunk))
    (literal-datum-index datum)))

(defun arrange-thunk-as-top-level (thunk)
  "Arrange to evaluate the thunk as the top-level form."
  (unless (cmp:xep-group-p thunk)
    (error "The thunk ~s must be a xep-group" thunk))
  (run-all-add-node (make-literal-node-toplevel-funcall
                     :arguments (list *gcroots-in-module*
                                      (entry-point-datum-for-xep-group thunk)
                                      (cmp:xep-group-name thunk)))))

(defun setup-literal-machine-function-vectors (the-module &key (id 0))
  (let* ((function-vector-length (length (literal-machine-function-vector *literal-machine*)))
         (function-vector-type (llvm-sys:array-type-get cmp:%opaque-fn-prototype*% function-vector-length))
         (function-vector-global (llvm-sys:make-global-variable
                                  the-module
                                  function-vector-type
                                  nil
                                  'llvm-sys:internal-linkage
                                  (llvm-sys:constant-array-get function-vector-type
                                                               (mapcar (lambda (fn)
                                                                         (cmp:irc-bit-cast fn cmp:%opaque-fn-prototype*%))
                                                                       (coerce (literal-machine-function-vector *literal-machine*) 'list)))
                                  (core:bformat nil "function-vector-%s" id))))
    (values function-vector-length function-vector-global)))

(defun do-literal-table (id body-fn)
  (llog "do-literal-table~%")
  (let ((*gcroots-in-module*
          (llvm-sys:make-global-variable cmp:*the-module*
                                         cmp:%gcroots-in-module% ; type
                                         nil ; isConstant
                                         'llvm-sys:internal-linkage
                                         (cmp:gcroots-in-module-initial-value)
                                         (core:bformat nil "%s%d" core:+gcroots-in-module-name+ (core:next-number))))
        (cmp:*load-time-value-holder-global-var*
          (llvm-sys:make-global-variable cmp:*the-module*
                                         cmp:%t*[DUMMY]% ; type
                                         nil             ; isConstant
                                         'llvm-sys:internal-linkage
                                         (llvm-sys:undef-value-get cmp:%t*[DUMMY]%)
                                         ;; nil ; initializer
                                         (next-value-table-holder-name (core:next-number) "dummy")))
        (cmp:*generate-compile-file-load-time-values* t)
        (real-name (next-value-table-holder-name (core:next-number))) ;; do we need to use a module-id???
        (*literal-machine* (make-literal-machine)))
    (cmp:cmp-log "do-literal-table cmp:*load-time-value-holder-global-var* -> {}%N" cmp:*load-time-value-holder-global-var*)
    (llog "About to evaluate body-fn~%")
    (funcall body-fn)
    ;; Generate the run-all function here
    (llog "About to generate run-all~%")
    (let ((transient-entries (finalize-transient-datum-indices *literal-machine*)))
      (cmp:with-run-all-body-codegen
          (let ((ordered-run-all-nodes (coerce (literal-machine-run-all-objects *literal-machine*) 'list)))
            (when *ltv-trap*
              (break "Look at *literal-machine* ~a" *literal-machine*))
            (let* ((byte-code-string (write-literal-nodes-byte-code ordered-run-all-nodes))
                   (byte-code-length (length byte-code-string))
                   (byte-code-global (llvm-sys:make-string-global cmp:*the-module* byte-code-string
                                                                  (core:bformat nil "startup-byte-code-%s" id))))
              (cmp:irc-intrinsic-call "cc_invoke_byte_code_interpreter"
                                      (list *gcroots-in-module*
                                            (cmp:irc-bit-cast (cmp:irc-gep byte-code-global (list 0 0))
                                                              cmp:%i8*%)
                                            (cmp:jit-constant-size_t byte-code-length)))
              (cmp:irc-intrinsic-call "cc_finish_gcroots_in_module" (list *gcroots-in-module*)))))
      (let ((literal-entries (literal-machine-table-index *literal-machine*)))
        (when t ;; (> literal-entries 0)
          ;; We have a new table, replace the old one and generate code to register the new one
          ;; and gc roots table
          (let* ((array-type (llvm-sys:array-type-get cmp:%t*% literal-entries))
                 (array-of-nulls (let (vals)
                                   (dotimes (idx literal-entries)
                                     (push (llvm-sys:constant-pointer-null-get cmp:%t*%) vals))
                                   vals))
                 (correct-size-holder (llvm-sys:make-global-variable cmp:*the-module*
                                                                     array-type
                                                                     nil ; isConstant
                                                                     'llvm-sys:internal-linkage
                                                                     (llvm-sys:constant-array-get array-type array-of-nulls)
                                                                     real-name))
                 (bitcast-correct-size-holder (cmp:irc-bit-cast correct-size-holder cmp:%t*[DUMMY]*%
                                                                "bitcast-table")))
            (llvm-sys:replace-all-uses-with cmp:*load-time-value-holder-global-var*
                                            bitcast-correct-size-holder)
            (cmp::cmp-log "Replaced all {} with {}%N" cmp:*load-time-value-holder-global-var* bitcast-correct-size-holder)
            (multiple-value-bind (function-vector-length function-vector)
                (setup-literal-machine-function-vectors cmp:*the-module* :id id)
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
                                                  (cmp:jit-constant-size_t function-vector-length)
                                                  (cmp:irc-bit-cast
                                                   (cmp:irc-gep function-vector
                                                                (list (cmp:jit-constant-size_t 0)
                                                                      (cmp:jit-constant-size_t 0)))
                                                   cmp:%i8**%)
                                                  )))))
            ;; Erase the dummy holder
            (llvm-sys:erase-from-parent cmp:*load-time-value-holder-global-var*)))))))

(defmacro with-literal-table ((&key id)&body body)
  `(do-literal-table ,id (lambda () ,@body)))

(defun do-rtv (body-fn)
  (let* ((cmp:*generate-compile-file-load-time-values* nil)
         (module-id (core:next-jit-compile-counter))
         (cmp:*load-time-value-holder-global-var*
           (llvm-sys:make-global-variable cmp:*the-module*
                                          cmp:%t*[0]% ; type
                                          nil         ; isConstant
                                          'llvm-sys:internal-linkage
                                          nil
                                          (next-value-table-holder-name module-id "dummy")))
         (*gcroots-in-module*
           (llvm-sys:make-global-variable cmp:*the-module*
                                          cmp:%gcroots-in-module% ; type
                                          nil ; isConstant
                                          'llvm-sys:internal-linkage
                                          (cmp:gcroots-in-module-initial-value)
                                          (core:bformat nil "%s%d" core:+gcroots-in-module-name+ module-id)))
         (*run-time-coalesce* (make-similarity-table #'eq))
         (*literal-machine* (make-literal-machine)))
    (cmp:cmp-log "do-rtv cmp:*load-time-value-holder-global-var* -> {}%N" cmp:*load-time-value-holder-global-var*)
    (let* ((THE-REPL-FUNCTION (funcall body-fn))
           (run-time-values (coerce (literal-machine-run-all-objects *literal-machine*) 'list))
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
                                                              (next-value-table-holder-name module-id)))
          (let ((bitcast-constant-table (cmp:irc-bit-cast constant-table cmp:%t*[0]*% "bitcast-table")))
            (llvm-sys:replace-all-uses-with cmp:*load-time-value-holder-global-var* bitcast-constant-table)
            (llvm-sys:erase-from-parent cmp:*load-time-value-holder-global-var*)
            (let ((cmp:*load-time-value-holder-global-var* bitcast-constant-table))
              (cmp::cmp-log "do-rtv Replaced all {} with {}%N" cmp:*load-time-value-holder-global-var* bitcast-constant-table)
              (multiple-value-bind (startup-shutdown-id ordered-raw-constant-list)
                  (cmp:codegen-startup-shutdown cmp:*the-module* module-id THE-REPL-FUNCTION *gcroots-in-module* constant-table num-elements ordered-literals-list bitcast-constant-table)
                (values ordered-raw-constant-list constant-table startup-shutdown-id)))))))))

(defmacro with-rtv (&body body)
  "Evaluate the code in the body in an environment where run-time values are assigned integer indices
starting from (literal-machine-table-index *literal-machine*) into a constants table and the run-time values are accumulated in *literal-machine*.
References to the run-time values are relative to the *load-time-value-holder-global-var*.
Once the body has evaluated, if there were run-time values accumulated then sort them by index and construct a new
global variable that can hold them all and replace every use of *load-time-value-holder-global-var* with this new constants-table.
Then erase the global variable in *load-time-value-holder-global-var* whether or not run time values were found
and  return the sorted values and the constant-table or (values nil nil)."
  `(do-rtv (lambda () (progn ,@body))))

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
            (declare (ignorable val))
            (llog "immediate-datum value ~s~%" val)
            (values immediate-datum nil)))
        (multiple-value-bind (similarity creator)
            (object-similarity-table-and-creator *literal-machine* object read-only-p)
          (llog "non-immediate~%")
          (let ((existing (if similarity (find-similar object similarity) nil)))
            (llog "Looking for ~s object ~s   existing --> ~s~%" desired-kind object existing)
            (cond
              (existing
               (when (and (eq desired-kind :literal) (eq :transient (datum-kind existing)))
                 (llog "    upgrading ~s~%" existing)
                 (upgrade-transient-datum-to-literal existing)
                 (llog "    after upgrade: ~s~%" existing))
               (values (datum-literal-node-creator existing) t))
              ;; Otherwise create a new datum at the current level of transientness
              (t (let ((datum (new-datum toplevelp)))
                   (when similarity (add-similar object datum similarity))
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
  (declare (ignore read-only-p))
  (let ((immediate-datum (immediate-datum-or-nil object)))
    (if immediate-datum
        (values immediate-datum NIL)
        (let* ((similarity *run-time-coalesce*)
               (existing (find-similar object similarity)))
          (if existing
              (values existing T)
              (values (let* ((datum (new-datum t))
                             (new-obj (make-literal-node-runtime :datum datum :object object)))
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
  (let ((fn (cmp:with-new-function (local-fn fn-env fn-result
                                             :function-name 'bclasp-top-level-form
                                             :parent-env nil
                                             :function-info (cmp:make-function-info
                                                             :function-name 'bclasp-top-level-form
                                                             :lambda-list nil
                                                             :docstring nil
                                                             :declares nil
                                                             :spi core:*current-source-pos-info*))
              ;; Map the function argument names
              (cmp:cmp-log "Creating ltv thunk with name: {}%N" (llvm-sys:get-name local-fn))
              (cmp:codegen fn-result form fn-env)
              (cmp:cmp-log-dump-function local-fn)
              (unless cmp:*suppress-llvm-output* (cmp:irc-verify-function local-fn t)))))
    fn))

(defun compile-form (form)
  (if core:*use-cleavir-compiler*
      (progn
        (funcall (find-symbol "COMPILE-FORM" "CLASP-CLEAVIR")
                 form))
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

(defun reference-closure (function)
  (unless (cmp:xep-group-p function)
    (error "In reference-closure function must be a xep-group - it's a ~s" function))
  (let* ((datum (new-datum t))
         (function-datum (entry-point-datum-for-xep-group function))
         (function-index (function-datum-index function-datum))
         (entry-point-ref (cmp:xep-group-entry-point-reference function))
         (creator (make-literal-node-closure :datum datum
                                             :function-index function-index
                                             :function function
                                             :entry-point-ref entry-point-ref)))
    (let ((epi (cmp:entry-point-reference-index entry-point-ref)))
      (add-creator "ltvc_make_closurette" datum creator epi))
    (datum-index datum)))

#|  (register-function function entry-point))

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
|#
;;; ------------------------------------------------------------
;;;
;;; functions that are called by bclasp and cclasp that might
;;;  be refactored to simplify the API

(defun compile-reference-to-literal (literal)
  "Generate a reference to a load-time-value or run-time-value literal depending if called from COMPILE-FILE or COMPILE respectively"
  (multiple-value-bind (data-or-index in-array literal-name)
      (reference-literal literal t)
    (if in-array
        (values (constants-table-reference data-or-index) literal-name)
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
  (declare (ignore env))
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
  (let ((fn (cmp:with-new-function (local-fn fn-env fn-result
                                             :function-name 'bclasp-top-level-form
                                             :parent-env nil
                                             :function-info (cmp:make-function-info
                                                             :function-name 'bclasp-top-level-form
                                                             :lambda-list nil
                                                             :docstring nil
                                                             :declares nil
                                                             :spi core:*current-source-pos-info*))
              (cmp:codegen fn-result form fn-env)
              (unless cmp:*suppress-llvm-output* (cmp:irc-verify-function local-fn t))
              )))
    fn))

;;; ------------------------------------------------------------
;;;
;;; Access load-time-values
;;;

(defun constants-table-reference (index &optional (holder cmp:*load-time-value-holder-global-var*) literal-name)
  (let ((label (if literal-name
                   (bformat nil "values-table[%d]/%s" index literal-name)
                   (bformat nil "values-table[%d]" index))))
    (llvm-sys:create-const-gep2-64 cmp:*irbuilder* holder 0 index label)))

(defun constants-table-value (index &optional (holder cmp:*load-time-value-holder-global-var*))
  (cmp:irc-load (constants-table-reference index holder)))



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
  (set-c++-info :i8 "char" "char")
  (set-c++-info :size_t "size_t" "size_t")
  (set-c++-info :t* "T_O*" "object" t)
  (set-c++-info :i8* "string" "string")
  (set-c++-info :single-float "float" "float")
  (set-c++-info :double-float "double" "double")
  (set-c++-info :uintptr_t "uintptr_t" "size_t")
  (set-c++-info :bignum "T_O*" "bignum")
  (set-c++-info :unknown "UNKNOWN" "UNKNOWN")
  )

(defun build-one-c++-function (op &optional (stream *standard-output*))
  (destructuring-bind (op-kind name return-type argument-types &key varargs ltvc)
      op
    (declare (ignore op-kind return-type ltvc))
    (let ((arg-types (nthcdr 2 argument-types)))
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
          (setf (gethash func-name map) code)
          (incf code))))
    map))



(defun build-c++-machine (&optional (stream *standard-output*))
  (build-c++-functions *machine* stream)
  (build-c++-switch *machine* stream))


(defvar *byte-codes* (build-c++-byte-codes cmp:*startup-primitives-as-list*))


