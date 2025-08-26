(in-package :literal)

(defvar *gcroots-in-module*)
#+threads(defvar *value-table-id-lock* (mp:make-lock :name '*value-table-id-lock*))
(defvar *value-table-id* 0)
(defun incf-value-table-id-value ()
  #+threads(mp:with-lock (*value-table-id-lock*) (incf *value-table-id*))
  #-threads(incf *value-table-id*))

(defun next-value-table-holder-name (module-id &optional suffix)
  (if suffix
      (core:fmt nil "{}-{}{}" suffix core:+literals-name+ module-id)
      (core:fmt nil "{}{}" core:+literals-name+ module-id)))

(defstruct literal-node-toplevel-funcall arguments)
(defstruct literal-node-call function source-pos-info holder)
(defstruct literal-node-side-effect name arguments)
(defstruct literal-dnode datum)
(defstruct (literal-node-creator (:include literal-dnode))
  name literal-name object arguments)
(defstruct (literal-node-runtime (:include literal-dnode)) object)
(defstruct (literal-node-closure (:include literal-dnode)) function-index function entry-point-ref)

(defstruct function-datum index)
#+short-float
(defstruct short-float-datum value)
(defstruct single-float-datum value)
(defstruct double-float-datum value)
#+long-float
(defstruct long-float-datum value)
(defstruct immediate-datum value)
(defstruct datum kind index literal-node-creator)

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


(defstruct literal-machine
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
  #+long-float
  (long-float-coalesce (make-similarity-table #'eql))
  (fcell-coalesce (make-similarity-table #'equal))
  (vcell-coalesce (make-similarity-table #'eq))
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

(defstruct general-entry-placeholder
  arity
  name
  cleavir-lambda-list-analysis
  )

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
  ;; Make sure that all function indices are consecutive
  (let ((datums (mapcar #'register-function->function-datum-impl f-or-p-list))
        (prev-function-index nil))
    (dolist (datum datums)
      (let ((cur-function-index (function-datum-index datum)))
        (when prev-function-index
          (unless (= (1+ prev-function-index) cur-function-index)
            (error "The function indices for a xep-function are not consecutive")))
        (setf prev-function-index cur-function-index)))
    datums))

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
  ;; We strip all non-simplicity out of arrays. As we do so it's important that we treat
  ;; arrays with fill pointers as ending at that fill pointer - we don't want to restore
  ;; the junk past the fill pointer into a simple array.
  (multiple-value-bind (dims total-size)
      (if (array-has-fill-pointer-p array)
          (let ((L (length array))) (values (list L) L))
          (values (array-dimensions array) (array-total-size array)))
    (let ((val (add-creator "ltvc_make_array" index array
                          (load-time-reference-literal (array-element-type array) read-only-p :toplevelp nil)
                          (load-time-reference-literal dims read-only-p :toplevelp nil))))
      (dotimes (i total-size)
        (add-side-effect-call "ltvc_setf_row_major_aref" val i
                              (load-time-reference-literal (row-major-aref array i) read-only-p :toplevelp nil)))
      val)))

(defun ltv/hash-table (hash-table index read-only-p &key (toplevelp t))
  (declare (ignore toplevelp))
  (let ((ht (add-creator "ltvc_make_hash_table" index hash-table
                         (load-time-reference-literal (hash-table-test hash-table) read-only-p :toplevelp nil))))
    (loop for key being the hash-keys of hash-table
            using (hash-value val)
          for lkey = (load-time-reference-literal key read-only-p :toplevelp nil)
          for lval = (load-time-reference-literal val read-only-p :toplevelp nil)
          do (add-side-effect-call "ltvc_setf_gethash" ht lkey lval))
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
  (let ((function-index (first (sys:core-fun-generator-entry-point-indices entry-point))))
    (add-creator "ltvc_make_local_entry_point" index entry-point
                 function-index
                 (load-time-reference-literal (sys:core-fun-generator/function-description entry-point) read-only-p :toplevelp nil))))

(defun ltv/global-entry-point (entry-point index read-only-p &key (toplevelp t))
  (declare (ignore toplevelp))
  (let ((function-index (first (sys:simple-core-fun-generator-entry-point-indices entry-point)))
        (local-entry-point-index (sys:simple-core-fun-generator-local-fun-index entry-point)))
    (add-creator "ltvc_make_global_entry_point" index entry-point
                 function-index
                 (load-time-reference-literal (sys:simple-core-fun-generator/function-description entry-point) read-only-p :toplevelp nil)
                 local-entry-point-index #+(or)(load-time-reference-literal (sys:global-entry-point-local-entry-point entry-point) read-only-p :toplevelp nil))))

(defun ltv/package (package index read-only-p &key (toplevelp t))
  (declare (ignore toplevelp))
  (add-creator "ltvc_make_package" index package
               (load-time-reference-literal (package-name package) read-only-p :toplevelp nil)))

#+short-float/binary16
(defun ltv/short-float (value index read-only-p &key (toplevelp t))
  (declare (ignore toplevelp read-only-p))
  (let* ((constant (make-short-float-datum :value value)))
    (add-creator "ltvc_make_binary16" index value constant)))

(defun ltv/single-float (single index read-only-p &key (toplevelp t))
  (declare (ignore toplevelp read-only-p))
  (let* ((constant (make-single-float-datum :value single)))
    (add-creator "ltvc_make_binary32" index single constant)))

(defun ltv/double-float (double index read-only-p &key (toplevelp t))
  (declare (ignore toplevelp read-only-p))
  (let* ((constant (make-double-float-datum :value double)))
    (add-creator "ltvc_make_binary64" index double constant)))

#+long-float/binary80
(defun ltv/long-float (value index read-only-p &key (toplevelp t))
  (declare (ignore toplevelp read-only-p))
  (let* ((constant (make-long-float-datum :value value)))
    (add-creator "ltvc_make_binary80" index value constant)))

#+long-float/binary128
(defun ltv/long-float (value index read-only-p &key (toplevelp t))
  (declare (ignore toplevelp read-only-p))
  (let* ((constant (make-long-float-datum :value value)))
    (add-creator "ltvc_make_binary128" index value constant)))

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
    #+short-float
    ((core:short-float-p  object) (values nil #'ltv/short-float))
    ((core:single-float-p  object) (values nil #'ltv/single-float))
    ((symbolp object) (values (literal-machine-symbol-coalesce literal-machine) #'ltv/symbol))
    ((double-float-p object) (values (literal-machine-double-float-coalesce literal-machine) #'ltv/double-float))
    #+long-float
    ((long-float-p object) (values (literal-machine-long-float-coalesce literal-machine) #'ltv/long-float))
    ((core:ratiop object) (values (literal-machine-ratio-coalesce literal-machine) #'ltv/ratio))
    ((sys:function-description-p object) (values (literal-machine-function-description-coalesce literal-machine) #'ltv/function-description))
    ((sys:core-fun-generator-p object) (values (literal-machine-function-description-coalesce literal-machine) #'ltv/local-entry-point))
    ((sys:simple-core-fun-generator-p object) (values (literal-machine-function-description-coalesce literal-machine) #'ltv/global-entry-point))
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

(defun write-argument-byte-code (arg stream byte-index)
  (cond
    ((function-datum-p arg) (core:ltvc-write-size-t (function-datum-index arg) stream byte-index))
    ((fixnump arg) (core:ltvc-write-size-t arg stream byte-index))
    ((characterp arg) (core:ltvc-write-char arg stream byte-index))
    ((stringp arg) (core:ltvc-write-string arg stream byte-index))
    ((core:bignump arg) (core:ltvc-write-bignum arg stream byte-index))
    ((immediate-datum-p arg)
     (core:ltvc-write-object #\i (immediate-datum-value arg) stream byte-index))
    #+short-float
    ((short-float-datum-p arg) (core:ltvc-write-short-float (long-float-datum-value arg) stream byte-index))
    ((single-float-datum-p arg) (core:ltvc-write-float (single-float-datum-value arg) stream byte-index))
    ((double-float-datum-p arg) (core:ltvc-write-double (double-float-datum-value arg) stream byte-index))
    #+long-float
    ((long-float-datum-p arg) (core:ltvc-write-long-float (long-float-datum-value arg) stream byte-index))
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

(defvar *byte-codes*)

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
      (setf byte-index (write-literal-node-byte-code fout node byte-index)))
    (setf byte-index (core:ltvc-write-char (code-char 0) fout byte-index))
    (get-output-stream-string fout)))

;; Note that this is set up so that we don't need to actually create a
;; function cell in the compiler's environment.
(defun %reference-function-cell (fname)
  (let* ((similarity (literal-machine-fcell-coalesce *literal-machine*))
         (existing (find-similar fname similarity)))
    (if existing
        (datum-literal-node-creator existing)
        (let ((datum (new-datum t)))
          (add-similar fname datum similarity)
          (add-creator "ltvc_ensure_fcell" datum fname
                       (load-time-reference-literal fname t
                                                    :toplevelp nil))))))

(defun reference-function-cell (fname)
  (let* ((data (if (cmp:generate-load-time-values)
                   (%reference-function-cell fname)
                   (run-time-reference-literal
                    (core:ensure-function-cell fname) nil)))
         (index (literal-node-index data)))
    (values index t)))

(defun %reference-variable-cell (vname)
  (let* ((similarity (literal-machine-vcell-coalesce *literal-machine*))
         (existing (find-similar vname similarity)))
    (if existing
        (datum-literal-node-creator existing)
        (let ((datum (new-datum t)))
          (add-similar vname datum similarity)
          (add-creator "ltvc_ensure_vcell" datum vname
                       (load-time-reference-literal vname t
                                                    :toplevelp nil))))))

(defun reference-variable-cell (vname)
  (let* ((data (if (cmp:generate-load-time-values)
                   (%reference-variable-cell vname)
                   (run-time-reference-literal
                    (core:ensure-variable-cell vname) nil)))
         (index (literal-node-index data)))
    (values index t)))

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
                                                               (map 'list
                                                                    (lambda (fn)
                                                                      (cmp:irc-bit-cast fn cmp:%opaque-fn-prototype*%))
                                                                    (literal-machine-function-vector *literal-machine*)))
                                  (core:fmt nil "function-vector-{}" id))))
    (values function-vector-length function-vector-global function-vector-type)))

(defun do-literal-table (id body-fn)
  (let ((*gcroots-in-module*
          (llvm-sys:make-global-variable cmp:*the-module*
                                         cmp:%gcroots-in-module% ; type
                                         nil ; isConstant
                                         'llvm-sys:internal-linkage
                                         (cmp:gcroots-in-module-initial-value)
                                         (core:fmt nil "{}{}" core:+gcroots-in-module-name+ (core:next-number))))
        (cmp:*load-time-value-holder-global-var-type* cmp:%t*[DUMMY]%)
        (cmp:*load-time-value-holder-global-var*
          (llvm-sys:make-global-variable cmp:*the-module*
                                         cmp:%t*[DUMMY]% ; type
                                         nil ; isConstant
                                         'llvm-sys:internal-linkage
                                         (llvm-sys:undef-value-get cmp:%t*[DUMMY]%)
                                         ;; nil ; initializer
                                         (next-value-table-holder-name (core:next-number) "dummy")))
        (cmp:*generate-compile-file-load-time-values* t)
        (real-name (next-value-table-holder-name (core:next-number))) ;; do we need to use a module-id???
        (*literal-machine* (make-literal-machine)))
    (funcall body-fn)
    ;; Generate the run-all function here
    (let ((transient-entries (finalize-transient-datum-indices *literal-machine*)))
      (cmp:with-run-all-body-codegen
          (let ((ordered-run-all-nodes (coerce (literal-machine-run-all-objects *literal-machine*) 'list)))
            (let* ((byte-code-string (write-literal-nodes-byte-code ordered-run-all-nodes))
                   (byte-code-length (length byte-code-string))
                   (byte-code-global (llvm-sys:make-string-global cmp:*the-module* byte-code-string
                                                                  (core:fmt nil "startup-byte-code-{}" id))))
              (cmp:irc-intrinsic "cc_invoke_start_code_interpreter"
                                 *gcroots-in-module*
                                 (cmp:irc-bit-cast (cmp:irc-typed-gep (llvm-sys:array-type-get cmp:%i8% (1+ byte-code-length))
                                                                      byte-code-global (list 0 0))
                                                   cmp:%i8*%)
                                 (cmp:jit-constant-size_t byte-code-length)
                                 (cmp:irc-bit-cast cmp::*current-function* cmp:%i8*%))
              (cmp:irc-intrinsic "cc_finish_gcroots_in_module" *gcroots-in-module*))))
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
            (multiple-value-bind (function-vector-length function-vector function-vector-type)
                (setup-literal-machine-function-vectors cmp:*the-module* :id id)
              (cmp:with-run-all-entry-codegen
                  (let ((transient-vector (cmp:alloca-i8* "transients")))
                    (cmp:irc-intrinsic "cc_initialize_gcroots_in_module"
                                       *gcroots-in-module*
                                       (cmp:irc-pointer-cast correct-size-holder cmp:%t**% "")
                                       (cmp:jit-constant-size_t literal-entries)
                                       (cmp:irc-int-to-ptr (cmp:jit-constant-uintptr_t 0)
                                                           cmp:%t*%)
                                       transient-vector
                                       (cmp:jit-constant-size_t transient-entries)
                                       (cmp:jit-constant-size_t function-vector-length)
                                       (cmp:irc-bit-cast
                                        (cmp:irc-typed-gep function-vector-type function-vector
                                                           (list 0 0))
                                        cmp:%i8**%)
                                       ))))
            ;; Erase the dummy holder
            (llvm-sys:erase-from-parent cmp:*load-time-value-holder-global-var*)))))))

(defmacro with-literal-table ((&key id)&body body)
  `(do-literal-table ,id (lambda () ,@body)))

(defvar *run-time-coalesce*)

(defun do-rtv (body-fn)
  (let* ((cmp:*generate-compile-file-load-time-values* nil)
         (module-id (core:next-jit-compile-counter))
         (cmp:*load-time-value-holder-global-var-type* cmp:%t*[0]%)
         (cmp:*load-time-value-holder-global-var*
           (llvm-sys:make-global-variable cmp:*the-module*
                                          cmp:*load-time-value-holder-global-var-type* ; type
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
                                          (core:fmt nil "{}{}" core:+gcroots-in-module-name+ module-id)))
         (*run-time-coalesce* (make-similarity-table #'eq))
         (*literal-machine* (make-literal-machine)))
    (funcall body-fn)
    (let* ((run-time-values (coerce (literal-machine-run-all-objects *literal-machine*) 'list))
           (num-elements (length run-time-values)))
      ;; Put the constants in order they will appear in the table.
      ;; Return the orderered-raw-constants-list and the constants-table GlobalVariable
      (when (> num-elements 0)
        (let* ((ordered-literals-list (sort run-time-values #'< :key #'literal-node-index))
               (ordered-raw-constants-list
                 (mapcar (lambda (x)
                                (cond
                                  ((literal-node-runtime-p x)
                                   (literal-node-runtime-object x))
                                  ((and (literal-node-creator-p x)
                                        (literal-node-closure-p
                                         (literal-node-creator-object x)))
                                   nil)
                                  (t (error "Illegal object in ordered-literals-list it is: ~s" x))))
                         ordered-literals-list))
               (array-type (llvm-sys:array-type-get cmp:%t*% (length ordered-literals-list)))
               (constant-table
                 (llvm-sys:make-global-variable cmp:*the-module*
                                                array-type
                                                nil ; isConstant
                                                'llvm-sys:internal-linkage
                                                (llvm-sys:undef-value-get array-type)
                                                (next-value-table-holder-name module-id))))
          (llvm-sys:replace-all-uses-with cmp:*load-time-value-holder-global-var* constant-table)
          (llvm-sys:erase-from-parent cmp:*load-time-value-holder-global-var*)
          (let ((cmp:*load-time-value-holder-global-var-type* array-type)
                (cmp:*load-time-value-holder-global-var* constant-table))
            (cmp:codegen-startup-shutdown cmp:*the-module* module-id *gcroots-in-module* array-type constant-table num-elements ordered-literals-list)
            (values ordered-raw-constants-list constant-table module-id)))))))

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
  (let ((immediate-datum (immediate-datum-or-nil object))
        (desired-kind (if toplevelp :literal :transient)))
    (if immediate-datum
        (values immediate-datum nil)
        (multiple-value-bind (similarity creator)
            (object-similarity-table-and-creator *literal-machine* object read-only-p)
          (let ((existing (if similarity (find-similar object similarity) nil)))
            (cond
              (existing
               (when (and (eq desired-kind :literal) (eq :transient (datum-kind existing)))
                 (upgrade-transient-datum-to-literal existing))
               (values (datum-literal-node-creator existing) t))
              ;; Otherwise create a new datum at the current level of transientness
              (t (let ((datum (new-datum toplevelp)))
                   (when similarity (add-similar object datum similarity))
                   (values (funcall creator object datum read-only-p :toplevelp toplevelp) t)))))))))

(defun pretty-load-time-name (object ltv-idx)
  (cond
    ((symbolp object) (core:fmt nil "SYMBOL->{}" object))
    ((consp object) "CONS")
    ((arrayp object) "ARRAY")
    ((numberp object) (format nil "NUMBER->~a" object))
    (t (subseq (core:fmt nil "ltv-idx_{}_val->{}" ltv-idx object) 0 30))))

;;;---------------------------------------------------------------------
;;;
;;; run time values (i.e., cl:compile)
;;;

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

(defun compile-form (form)
  (funcall (find-symbol "COMPILE-FORM" "CLASP-CLEAVIR")
           form))

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
              (values (literal-node-index data) T
                      (literal-node-creator-literal-name data))
              (values (cmp:irc-maybe-cast-integer-to-t* (immediate-datum-value data))
                      nil)))
        (multiple-value-bind (immediate-datum?literal-node-runtime in-array)
            (run-time-reference-literal object read-only-p)
          (if in-array
              (let* ((literal-node-runtime immediate-datum?literal-node-runtime)
                     (index (literal-node-index literal-node-runtime)))
                (values index T))
              (let ((immediate-datum immediate-datum?literal-node-runtime))
                (values (cmp:irc-maybe-cast-integer-to-t* (immediate-datum-value immediate-datum))
                        nil)))))))

;;; ------------------------------------------------------------
;;;
;;; functions that are called by bclasp and cclasp that might
;;;  be refactored to simplify the API

(defun compile-reference-to-literal (literal
                                     &optional (read-only-p t))
  "Generate a reference to a load-time-value or run-time-value literal depending if called from COMPILE-FILE or COMPILE respectively"
  (multiple-value-bind (data-or-index in-array literal-name)
      (reference-literal literal read-only-p)
    (if in-array
        (values (constants-table-reference data-or-index) literal-name)
        data-or-index)))

;;; ------------------------------------------------------------
;;;
;;; Access load-time-values
;;;

(defun constants-table-reference (index &key
                                          (holder cmp:*load-time-value-holder-global-var*)
                                          (holder-type cmp:*load-time-value-holder-global-var-type*)
                                          literal-name)
  (let ((label (if literal-name
                   (core:fmt nil "values-table[{}]/{}" index literal-name)
                   (core:fmt nil "values-table[{}]" index))))
    (cmp:irc-const-gep2-64 holder-type holder 0 index label)))

(defun constants-table-value (index &key (holder cmp:*load-time-value-holder-global-var*)
                                      (holder-type cmp:*load-time-value-holder-global-var-type*)
                                      literal-name)
  (cmp:irc-t*-load (constants-table-reference index
                                              :holder holder
                                              :holder-type holder-type
                                              :literal-name literal-name)))

(defun build-c++-byte-codes (primitives)
  (let ((map (make-hash-table :test #'equal)))
    (dolist (prim primitives)
      (setf (gethash (third prim) map) (first prim)))
    map))

(defvar *byte-codes* (build-c++-byte-codes cmpref:*startup-primitives-as-list*))
