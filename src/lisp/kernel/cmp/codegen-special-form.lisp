(in-package #:cmp)

;;;; bclasp compilation of special forms.

(export 'llvm-inline)

;;; PROGV

(defmacro progv (symbols values &body forms)
  `(core:progv-function ,symbols ,values
                        (lambda ()
                          (declare (core:lambda-name core::progv-lambda))
                          (progn ,@forms))))

;;; CORE::VECTOR-LENGTH

(defun gen-vector-length-untagged (vector)
  (let* ((type (llvm-sys:type-get-pointer-to (simple-vector-llvm-type 't)))
         (cast (irc-bit-cast vector type)) ; treat the vector as a vector
         ;; find the location of the length
         (length-address
           (irc-typed-in-bounds-gep (simple-vector-llvm-type 't)
                                    cast
                                    (list 0 +simple-vector-length-slot+)
                                    "vector-length-address")))
    (irc-typed-load %i64% length-address "vector-length")))

(defun gen-vector-length (vector)
  (irc-tag-fixnum (gen-vector-length-untagged vector) "vector-length"))

(defun codegen-vector-length (result rest env)
  (let ((form (car rest))
        (vector (alloca-t* "vector-alloca")))
    (codegen vector form env)
    (irc-t*-result (gen-vector-length (irc-t*-load vector "vector")) result)))

;;; CORE::%ARRAY-DIMENSION

(defun gen-%array-dimension (array axisn)
  (let* ((untagged-axisn (irc-untag-fixnum axisn %i64% "untagged-axisn"))
         (untagged-dim (irc-array-dimension array untagged-axisn)))
    (irc-tag-fixnum untagged-dim "array-dimension")))

(defun codegen-%array-dimension (result rest env)
  (let ((array-form (first rest))
        (array-alloca (alloca-t* "array-alloca"))
        (axis-form (second rest))
        (axis-alloca (alloca-t* "axis-alloca")))
    (codegen array-alloca array-form env)
    (codegen axis-alloca axis-form env)
    (irc-t*-result (gen-%array-dimension (irc-t*-load array-alloca) (irc-t*-load axis-alloca))
                   result)))

;;; FENCE

(defun order-spec->order (order-spec)
  (case order-spec
    ((:unordered) 'llvm-sys:unordered)
    ((:relaxed) 'llvm-sys:monotonic)
    ((:acquire) 'llvm-sys:acquire)
    ((:release) 'llvm-sys:release)
    ((:acquire-release) 'llvm-sys:acquire-release)
    ((:sequentially-consistent) 'llvm-sys:sequentially-consistent)
    (t (error "BUG: Unknown atomic order specifier ~a" order-spec))))

(defun gen-fence (order-spec)
  (when (or (eq order-spec :unordered) (eq order-spec :relaxed))
    (error "Can't generate a fence with ~a ordering" order-spec))
  (irc-fence (order-spec->order order-spec)))

(defun codegen-fence (result rest env)
  (declare (ignore result env))
  (gen-fence (first rest)))

;;; CLEAVIR-PRIMOP:CAR, CLEAVIR-PRIMOP:CDR

(defun gen-memref-address (tpointer offset)
  (irc-bit-cast
   ;; memref/set use byte addressing, so treat these as i8 arrays
   (irc-typed-gep-variable %i8%
                     (irc-bit-cast tpointer %i8*%)
                     ;; llvm doesn't actually have signed types,
                     ;; so the u is a misnomer - don't sweat it.
                     (list (cmp:make-uintptr_t offset)))
   %t**% "memref-set-addr"))

(defun codegen-car (result rest env)
  (let ((cons-form (first rest))
        (cons-alloca (alloca-t* "cons")))
    (codegen cons-alloca cons-form env)
    (irc-t*-result (irc-t*-load-atomic (gen-memref-address
                                     (irc-t*-load cons-alloca)
                                     (- +cons-car-offset+ +cons-tag+)))
                   result)))

(defun codegen-cdr (result rest env)
  (let ((cons-form (first rest))
        (cons-alloca (alloca-t* "cons")))
    (codegen cons-alloca cons-form env)
    (irc-t*-result (irc-t*-load-atomic (gen-memref-address
                                     (irc-t*-load cons-alloca)
                                     (- +cons-cdr-offset+ +cons-tag+)))
                   result)))

;;; ATOMIC CAR, CDR, RPLACA, RPLACD, plus CAS

(defun codegen-atomic-car (result rest env)
  (let ((order (order-spec->order (first rest)))
        (cons-form (second rest))
        (cons-alloca (alloca-t* "cons")))
    (codegen cons-alloca cons-form env)
    (irc-t*-result (irc-t*-load-atomic (gen-memref-address
                                     (irc-t*-load cons-alloca)
                                     (- +cons-car-offset+ +cons-tag+))
                                    :order order)
                   result)))

(defun codegen-atomic-cdr (result rest env)
  (let ((order (order-spec->order (first rest)))
        (cons-form (second rest))
        (cons-alloca (alloca-t* "cons")))
    (codegen cons-alloca cons-form env)
    (irc-t*-result (irc-t*-load-atomic (gen-memref-address
                                     (irc-t*-load cons-alloca)
                                     (- +cons-cdr-offset+ +cons-tag+))
                                    :order order)
                   result)))

(defun codegen-atomic-rplaca (result rest env)
  (let ((order (order-spec->order (first rest)))
        (nv-form (second rest))
        (cons-form (third rest))
        (cons-alloca (alloca-t* "cons"))
        (nv-alloca (alloca-t* "nv")))
    (codegen nv-alloca nv-form env)
    (codegen cons-alloca cons-form env)
    (let ((nv (irc-t*-load nv-alloca)))
      (irc-store-atomic nv (gen-memref-address
                            (irc-t*-load cons-alloca)
                            (- +cons-car-offset+ +cons-tag+))
                        :order order)
      (irc-t*-result nv result))))

(defun codegen-atomic-rplacd (result rest env)
  (let ((order (order-spec->order (first rest)))
        (nv-form (second rest))
        (cons-form (third rest))
        (cons-alloca (alloca-t* "cons"))
        (nv-alloca (alloca-t* "nv")))
    (codegen nv-alloca nv-form env)
    (codegen cons-alloca cons-form env)
    (let ((nv (irc-t*-load nv-alloca)))
      (irc-store-atomic nv (gen-memref-address
                            (irc-t*-load cons-alloca)
                            (- +cons-cdr-offset+ +cons-tag+))
                        :order order)
      (irc-t*-result nv result))))

(defun codegen-cas-car (result rest env)
  (let ((order (order-spec->order (first rest)))
        (cmp-form (second rest)) (nv-form (third rest))
        (cons-form (fourth rest))
        (cmp-alloca (alloca-t* "cmp")) (nv-alloca (alloca-t* "nv"))
        (cons-alloca (alloca-t* "cons")))
    (codegen cmp-alloca cmp-form env)
    (codegen nv-alloca nv-form env)
    (codegen cons-alloca cons-form env)
    (irc-t*-result
     (irc-cmpxchg (gen-memref-address (irc-t*-load cons-alloca)
                                      (- +cons-car-offset+ +cons-tag+))
                  (irc-t*-load cmp-alloca) (irc-t*-load nv-alloca)
                  :order order)
     result)))

(defun codegen-cas-cdr (result rest env)
  (let ((order (order-spec->order (first rest)))
        (cmp-form (second rest)) (nv-form (third rest))
        (cons-form (fourth rest))
        (cmp-alloca (alloca-t* "cmp")) (nv-alloca (alloca-t* "nv"))
        (cons-alloca (alloca-t* "cons")))
    (codegen cmp-alloca cmp-form env)
    (codegen nv-alloca nv-form env)
    (codegen cons-alloca cons-form env)
    (irc-t*-result
     (irc-cmpxchg (gen-memref-address (irc-t*-load cons-alloca)
                                      (- +cons-cdr-offset+ +cons-tag+))
                  (irc-t*-load cmp-alloca) (irc-t*-load nv-alloca)
                  :order order)
     result)))

;;; CLEAVIR-PRIMOP:FUNCALL

(defun codegen-primop-funcall (result rest env)
  (let ((func (first rest))
        (funcy (alloca-t* "function"))
        (args (rest rest)))
    (codegen funcy func env)
    (codegen-call result (irc-t*-load funcy) args env)))

;;; CLEAVIR-PRIMOP:UNREACHABLE

(defun codegen-unreachable (result rest env)
  (declare (ignore result rest env))
  (irc-unreachable)
  ;; This is necessary if we keep generating more instructions.
  ;; I don't think we actually do - and clasp compiles without this-
  ;; but if we were to, the llvm error would be kind of hard to understand,
  ;; so I'm leaving this in.
  (irc-begin-block (irc-basic-block-create "unreachable")))

;;; CORE:VASLIST-LENGTH
;;; Get the count of remaining args in a vaslist.

(defun gen-vaslist-length (vaslist)
  (irc-tag-fixnum
   (irc-typed-load %size_t% (c++-field-ptr info.%vaslist% vaslist :nargs "vaslist-nargs"))))

(defun codegen-vaslist-length (result rest env)
  (let ((form (car rest))
        (vaslist (alloca-t* "vaslist-length-vaslist")))
    (codegen vaslist form env)
    (irc-t*-result (gen-vaslist-length (irc-t*-load vaslist)) result)))

;;; CORE:VASLIST-POP
;;; Remove one item from the vaslist and return it.
;;; Without DEBUG_BUILD, does not actually check if there is an element to pop.
;;; Use caution.

(defun codegen-vaslist-pop (result rest env)
  (let ((form (car rest))
        (vaslist-tagged (alloca-t* "vaslist-pop-vaslist")))
    (codegen vaslist-tagged form env)
    (let ((vaslist* (irc-untag-vaslist (irc-t*-load vaslist-tagged))))
      (irc-t*-result (gen-vaslist-pop (irc-bit-cast vaslist* %vaslist*%)) result))))

;;; CLEAVIR-PRIMOP:CASE
;;; CL:CASE when all the keys are immediates. Generated by the compiler macro.
;;; Always has a default. Keys are always lists of objects.

(defun codegen-primop-case (result rest env)
  (let* ((keyform (first rest)) (cases (rest rest))
         (default (rest (first (last cases))))
         (defaultb (irc-basic-block-create "case-default"))
         (mergeb (irc-basic-block-create "case-merge"))
         (main-cases (butlast cases))
         (ncases (let ((sum 0))
                   (dolist (case main-cases sum)
                     (incf sum (length (first case))))))
         (keyt (alloca-t* "case-key")))
    (codegen keyt keyform env)
    (let* ((key64 (irc-ptr-to-int (irc-t*-load keyt) %i64%))
           (sw (irc-switch key64 defaultb ncases)))
      (dolist (case main-cases)
        (let ((keys (first case))
              (body (rest case))
              (block (irc-basic-block-create "case-case")))
          (dolist (key keys)
            (let ((val (core:create-tagged-immediate-value-or-nil key)))
              (irc-add-case sw (jit-constant-i64 val) block)))
          (irc-begin-block block)
          (codegen-progn result body env)
          (irc-branch-if-no-terminator-inst mergeb)))
      (irc-begin-block defaultb)
      (codegen-progn result default env)
      (irc-branch-if-no-terminator-inst mergeb))
    (irc-begin-block mergeb)))

;;; (CORE:HEADER-STAMP-CASE stamp b1 b2 b3 b4)
;;; Branch to one of four places depending on the where tag.

(defun codegen-header-stamp-case (result rest env)
  (let ((stampf (first rest))
        (stampt (alloca-t* "stamp"))
        (derivable (second rest))
        (derivableb (irc-basic-block-create "derivable"))
        (rack (third rest))
        (rackb (irc-basic-block-create "rack"))
        (wrapped (fourth rest))
        (wrappedb (irc-basic-block-create "wrapped"))
        (header (fifth rest))
        (headerb (irc-basic-block-create "header"))
        (defaultb (irc-basic-block-create "impossible-default"))
        (mergeb (irc-basic-block-create "header-stamp-case-after")))
    (codegen stampt stampf env)
    (let* ((stamp-i64 (irc-ptr-to-int (irc-t*-load stampt) %i64%))
           (where (irc-and stamp-i64 (jit-constant-i64 +where-tag-mask+)))
           (sw (irc-switch where defaultb 4)))
      (irc-add-case sw (jit-constant-i64 +derivable-where-tag+) derivableb)
      (irc-add-case sw (jit-constant-i64 +rack-where-tag+) rackb)
      (irc-add-case sw (jit-constant-i64 +wrapped-where-tag+) wrappedb)
      (irc-add-case sw (jit-constant-i64 +header-where-tag+) headerb)
      ;; Now generate all these blocks.
      (irc-begin-block derivableb)
      (codegen result derivable env)
      (irc-branch-if-no-terminator-inst mergeb)
      (irc-begin-block rackb)
      (codegen result rack env)
      (irc-branch-if-no-terminator-inst mergeb)
      (irc-begin-block wrappedb)
      (codegen result wrapped env)
      (irc-branch-if-no-terminator-inst mergeb)
      (irc-begin-block headerb)
      (codegen result header env)
      (irc-branch-if-no-terminator-inst mergeb)
      ;; Generate the default block, which is just unreachable.
      (irc-begin-block defaultb)
      (irc-unreachable)
      ;; Done
      (irc-begin-block mergeb))))

;;; CORE:HEADER-STAMP

(defun codegen-header-stamp (result rest env)
  (let ((form (car rest))
        (object (alloca-t* "read-stamp-obj")))
    (codegen object form env)
    (irc-t*-result (irc-header-stamp (irc-t*-load object)) result)))

;;; CORE:RACK-STAMP

(defun codegen-rack-stamp (result rest env)
  (let ((form (car rest))
        (object (alloca-t* "read-stamp-obj")))
    (codegen object form env)
    (irc-t*-result (irc-rack-stamp (irc-t*-load object)) result)))

;;; CORE:WRAPPED-STAMP

(defun codegen-wrapped-stamp (result rest env)
  (let ((form (car rest))
        (object (alloca-t* "read-stamp-obj")))
    (codegen object form env)
    (irc-t*-result (irc-wrapped-stamp (irc-t*-load object)) result)))

;;; CORE:DERIVABLE-STAMP

(defun codegen-derivable-stamp (result rest env)
  (let ((form (car rest))
        (object (alloca-t* "read-stamp-obj")))
    (codegen object form env)
    (irc-t*-result (irc-derivable-stamp (irc-t*-load object)) result)))

;;; CORE:INSTANCE-REF

(defun gen-instance-ref (instance index)
  (irc-read-slot instance (irc-untag-fixnum index %size_t% "slot-location")))

(defun codegen-instance-ref (result rest env)
  (let ((instance (first rest)) (index (second rest))
        (instancet (alloca-t* "instance-ref-instance"))
        (indext (alloca-t* "instance-ref-index")))
    (codegen instancet instance env)
    (codegen indext index env)
    (irc-t*-result (gen-instance-ref (irc-t*-load instancet) (irc-t*-load indext))
                   result)))

;;; CORE:INSTANCE-SET

(defun gen-instance-set (instance index value)
  (irc-write-slot instance (irc-untag-fixnum index %size_t% "slot-location") value))

(defun codegen-instance-set (result rest env)
  (let ((instance (first rest)) (index (second rest)) (value (third rest))
        (instancet (alloca-t* "instance-set-instance"))
        (indext (alloca-t* "instance-set-index"))
        (valuet (alloca-t* "instance-set-value")))
    (codegen instancet instance env)
    (codegen indext index env)
    (codegen valuet value env)
    (irc-t*-result
     (gen-instance-set (irc-t*-load instancet) (irc-t*-load indext) (irc-t*-load valuet))
     result)))

;;; CORE:INSTANCE-CAS

(defun gen-instance-cas (instance index old new)
  (irc-cmpxchg
   (irc-instance-slot-address
    instance (irc-untag-fixnum index %size_t% "slot-location"))
   old new))

(defun codegen-instance-cas (result rest env)
  (let ((old (first rest)) (new (second rest))
        (instance (third rest)) (index (fourth rest))
        (instancet (alloca-t* "instance")) (indext (alloca-t* "index"))
        (oldt (alloca-t* "old")) (newt (alloca-t* "new")))
    (codegen instancet instance env)
    (codegen indext index env)
    (codegen oldt old env)
    (codegen newt new env)
    (irc-t*-result
     (gen-instance-cas (irc-t*-load instancet) (irc-t*-load indext)
                       (irc-t*-load oldt) (irc-t*-load newt))
     result)))

;;; CORE:INSTANCE-RACK

(defun gen-instance-rack (instance) (irc-rack instance))

(defun codegen-instance-rack (result rest env)
  (let ((instance (first rest))
        (instancet (alloca-t* "instance-rack-instance")))
    (codegen instancet instance env)
    (irc-t*-result (gen-instance-rack (irc-t*-load instancet)) result)))

;;; CORE:INSTANCE-RACK-SET

(defun gen-instance-rack-set (instance rack)
  (irc-rack-set instance rack)
  rack)

(defun codegen-instance-rack-set (result rest env)
  (let ((instance (first rest)) (rack (second rest))
        (instancet (alloca-t* "instance"))
        (rackt (alloca-t* "rack")))
    (codegen instancet instance env)
    (codegen rackt rack env)
    (irc-t*-result
     (gen-instance-rack-set (irc-t*-load instancet) (irc-t*-load rackt))
     result)))

;;; CORE:RACK-REF

(defun gen-rack-ref (rack index &key (order 'llvm-sys:monotonic))
  (irc-rack-read rack (irc-untag-fixnum index %size_t% "slot-location")
                 :order order))

(defun codegen-rack-ref (result rest env)
  (let ((rack (first rest)) (index (second rest))
        (rackt (alloca-t* "rack-ref-rack"))
        (indext (alloca-t* "rack-ref-index")))
    (codegen rackt rack env)
    (codegen indext index env)
    (irc-t*-result (gen-rack-ref (irc-t*-load rackt) (irc-t*-load indext))
                   result)))

;;; CORE:RACK-SET

(defun gen-rack-set (rack index value &key (order 'llvm-sys:monotonic))
  (irc-rack-write rack (irc-untag-fixnum index %size_t% "slot-location") value
                  :order order)
  value)

(defun codegen-rack-set (result rest env)
  (let ((rack (first rest)) (index (second rest)) (value (third rest))
        (rackt (alloca-t* "rack-set-rack"))
        (indext (alloca-t* "rack-set-index"))
        (valuet (alloca-t* "rack-set-value")))
    (codegen rackt rack env)
    (codegen indext index env)
    (codegen valuet value env)
    (irc-t*-result
     (gen-rack-set (irc-t*-load rackt) (irc-t*-load indext) (irc-t*-load valuet))
     result)))

;;; CORE::ATOMIC-RACK-READ, CORE::ATOMIC-RACK-WRITE

(defun codegen-atomic-rack-read (result rest env)
  (let ((order (order-spec->order (first rest)))
        (rack (second rest)) (index (third rest))
        (rackt (alloca-t* "rack-ref-rack"))
        (indext (alloca-t* "rack-ref-index")))
    (codegen rackt rack env)
    (codegen indext index env)
    (irc-t*-result (irc-rack-read
                    (irc-t*-load rackt)
                    (irc-untag-fixnum
                     (irc-t*-load indext) %size_t% "slot-location")
                    :order order)
                   result)))

(defun codegen-atomic-rack-write (result rest env)
  (let ((order (order-spec->order (first rest)))
        (nv (second rest)) (rack (third rest)) (index (fourth rest))
        (nvt (alloca-t* "rack-set-value"))
        (rackt (alloca-t* "rack-set-rack"))
        (indext (alloca-t* "rack-set-index")))
    (codegen nvt nv env)
    (codegen rackt rack env)
    (codegen indext index env)
    (let ((nv (irc-t*-load nvt)))
      (irc-rack-write (irc-t*-load rackt)
                      (irc-untag-fixnum
                       (irc-t*-load indext) %size_t% "slot-location")
                      nv
                      :order order)
      (irc-t*-result nv result))))

(defun codegen-cas-rack (result rest env)
  (let ((order (order-spec->order (first rest)))
        (old (second rest)) (nv (third rest))
        (rack (fourth rest)) (index (fifth rest))
        (oldt (alloca-t* "old")) (newt (alloca-t* "new"))
        (rackt (alloca-t* "rack")) (indext (alloca-t* "index")))
    (codegen oldt old env)
    (codegen newt nv env)
    (codegen rackt rack env)
    (codegen indext index env)
    (irc-t*-result
     (irc-cmpxchg (irc-rack-slot-address (irc-t*-load rackt)
                                         (irc-untag-fixnum
                                          (irc-t*-load indext)
                                          %size_t% "slot-location"))
                  (irc-t*-load oldt) (irc-t*-load newt)
                  :order order)
     result)))

;;; DBG-i32

(defparameter *nexti* 10000)
(defun codegen-dbg-i32 (result rest env)
  (declare (ignore result env))
  (let ((giveni (car rest)))
    (if (null giveni)
	(progn
	  (setq giveni *nexti*)
	  (setq *nexti* (+ 1 *nexti*))))
    (irc-intrinsic "debugPrintI32" (jit-constant-i32 giveni))))

;;; DEBUG-MESSAGE

(defun codegen-debug-message (result rest env)
  (declare (ignore result env))
  (let ((message (jit-constant-unique-string-ptr (car rest))))
    (irc-intrinsic "debugMessage" message)))

(defun codegen-debug-break (result rest env)
  (declare (ignore result rest env))
  (irc-intrinsic "debugBreak"))

;;; LLVM-INLINE

(defun codegen-llvm-inline (result result-env-body compiler-env)
  (destructuring-bind ((result-name env-name) &body body)
      result-env-body
    (eval `(let ((,result-name ,result)
                 (,env-name ,compiler-env))
             ,@body))))

#+(or)
(defmacro blog (fmt &rest fargs)
  `(core:fmt *error-output* ,fmt ,@fargs))
(defmacro blog (fmt &rest fargs)
  (declare (ignore fmt fargs))
  nil)

;;; MULTIPLE-VALUE-FOREIGN-CALL

(defun codegen-multiple-value-foreign-call (result form evaluate-env)
  "Evaluate each of the arguments into an alloca and invoke the function"
  ;; setup the ActivationFrame for passing arguments to this function in the setup arena
  (assert-result-isa-llvm-value result)
  ;;(core:fmt t "In codegen-multiple-value-foreign-call codegen form: {}%N" form)
  (let* ((intrinsic-name (car form))
         args
         (temp-result (alloca-t*)))
    ;; evaluate the arguments into the array
    ;;  used to be done by --->    (codegen-evaluate-arguments (cdr form) evaluate-env)
    (do* ((cur-exp (cdr form) (cdr cur-exp))
          (exp (car cur-exp) (car cur-exp))
          (i 0 (+ 1 i)))
         ((endp cur-exp) nil)
      ;;(core:fmt t "In codegen-multiple-value-foreign-call codegen arg[{}] -> {}%N" i exp)
      (codegen temp-result exp evaluate-env)
      (push (irc-t*-load temp-result) args))
    (let* ((func (or (llvm-sys:get-function *the-module* intrinsic-name)
                     (let ((arg-types (make-list (length args) :initial-element %t*%))
                           (varargs nil))
                       (irc-function-create
                        (llvm-sys:function-type-get %return-type% arg-types varargs)
                        'llvm-sys::External-linkage
                        intrinsic-name
                        *the-module*))))
           (function-type (llvm-sys:get-function-type func))
           (result-in-registers
             (irc-call-or-invoke function-type func (nreverse args))))
      (irc-tmv-result result-in-registers result)))
  (irc-low-level-trace :flow))

;;; FOREIGN-CALL, FOREIGN-CALL-POINTER

(defun function-type-create-on-the-fly (foreign-types)
  (let ((arg-types (mapcar (lambda (type)
                             (clasp-ffi::safe-translator-type type))
                           (second foreign-types)))
        (varargs nil))
    (llvm-sys:function-type-get (clasp-ffi::safe-translator-type (first foreign-types)) arg-types varargs)))

(defun evaluate-foreign-arguments (fargs foreign-types temp-result evaluate-env)
  (let (args)
    (do* ((cur-exp fargs (cdr cur-exp))
          (exp (car cur-exp) (car cur-exp))
          (type-cur (second foreign-types) (cdr type-cur))
          (type (car type-cur) (car type-cur))
          (i 0 (+ 1 i)))
         ((endp cur-exp) nil)
      ;;(core:fmt t "In codegen-multiple-value-foreign-call codegen arg[{}] -> {}%N" i exp)
      (codegen temp-result exp evaluate-env)
      (push (irc-intrinsic-call (clasp-ffi::from-translator-name type)
                             (list (irc-t*-load temp-result))) args))
    args))

(defun codegen-foreign-call (result form evaluate-env)
  "Evaluate each of the arguments into an alloca and invoke the function"
  ;; setup the ActivationFrame for passing arguments to this function in the setup arena
  (assert-result-isa-llvm-value result)
  ;;(core:fmt t "In codegen-multiple-value-foreign-call codegen form: {}%N" form)
  (let* ((foreign-types (first form))
         (intrinsic-name (second form))
         (fargs (cddr form))
         (temp-result (alloca-t*)))
    ;; evaluate the arguments into the array
    ;;  used to be done by --->    (codegen-evaluate-arguments (cddr form) evaluate-env)
    (let* ((args (evaluate-foreign-arguments fargs foreign-types temp-result evaluate-env))
           (function-type (function-type-create-on-the-fly foreign-types))
           (func (or (llvm-sys:get-function *the-module* intrinsic-name)
                     (irc-function-create
                      function-type
                      'llvm-sys::External-linkage
                      intrinsic-name
                      *the-module*)))
           (foreign-result
            (irc-call-or-invoke function-type function-type func (nreverse args)))
           (result-in-t*
            (if (eq :void (first foreign-types))
                (irc-intrinsic-call (clasp-ffi::to-translator-name (first foreign-types)) nil) ; returns :void
                (irc-intrinsic-call (clasp-ffi::to-translator-name (first foreign-types))
                                 (list foreign-result)))))
      (irc-t*-result result-in-t* result)))
  (irc-low-level-trace :flow))

(defun codegen-foreign-call-pointer (result form evaluate-env)
  "Evaluate each of the arguments into an alloca and invoke the function pointer"
  ;; setup the ActivationFrame for passing arguments to this function in the setup arena
  (assert-result-isa-llvm-value result)
  (let* ((foreign-types (first form))
         (func-pointer (second form))
         (fargs (cddr form))
         (temp-result (alloca-t*)))
    ;; evaluate the arguments into the array
    (let ((args (evaluate-foreign-arguments fargs foreign-types temp-result evaluate-env))
          (function-type (function-type-create-on-the-fly foreign-types)))
      ;; evaluate the function pointer
      (codegen temp-result func-pointer evaluate-env)
      (let* ((function-pointer-type (llvm-sys:type-get-pointer-to function-type))
             (pointer-t* (irc-t*-load temp-result))
             (function-pointer (llvm-sys:create-bit-cast *irbuilder* (irc-intrinsic "cc_getPointer" pointer-t*) function-pointer-type "cast-function-pointer"))
             (foreign-result
              (irc-call-or-invoke function-type function-pointer (nreverse args)))
             (result-in-t*
              (if (eq :void (first foreign-types))
                  (irc-intrinsic-call (clasp-ffi::to-translator-name (first foreign-types)) nil) ; returns :void
                  (irc-intrinsic-call (clasp-ffi::to-translator-name (first foreign-types)) (list foreign-result)))))
        (irc-t*-result result-in-t* result)))
    (irc-low-level-trace :flow)))

;;; DEFCALLBACK

;;; shared with cleavir.
;;; What we're doing here is defining a C function
;;; that calls the translators on its arguments, passes those translated arguments
;;; to a Lisp closure, then translates the primary return value of that function
;;; back to C and returns it (or if the C function is return type void, doesn't).
(defun gen-defcallback (c-name convention
                        return-type-name return-translator-name
                        argument-type-names argument-translator-names
                        parameters place-holder closure-value)
  (declare (ignore convention place-holder))         ; FIXME
  ;; parameters should be a list of symbols, i.e. lambda list with only required.
  (unless (= (length argument-type-names) (length parameters) (length argument-translator-names))
    (error "BUG: Callback function parameters and types have a length mismatch"))
;;; Generate a variable and put the closure in it.
  (let* ((closure-literal-slot-index (literal:new-table-index))
         (closure-var-name (core:fmt nil "{}_closure_var" c-name)))
    #+(or)(progn
            (format t "gen-defcallback - the (literal::literal-machine-table-index literal::*literal-machine*) -> ~d~%" (literal::literal-machine-table-index literal::*literal-machine*))
            (format t "gen-defcallback cmp:*load-time-value-holder-global-var* -> ~a~%" cmp:*load-time-value-holder-global-var*)
            (format t "gen-defcallback closure-value -> ~a~%" closure-value)
            (format t "gen-defcallback closure-literal-slot-index -> ~a~%" closure-literal-slot-index))
    (irc-t*-result closure-value (literal:constants-table-reference closure-literal-slot-index))
    ;; Now generate the C function.
    ;; We don't actually "do" anything with it- just leave it there to be linked/used like a C function.
    (with-landing-pad nil ; Since we're in a new function (which should never be an unwind dest)
      (let* ((c-argument-names (mapcar #'string parameters))
             (return-type (clasp-ffi:safe-translator-type return-type-name))
             (argument-types (mapcar #'clasp-ffi:safe-translator-type argument-type-names))
             (c-function-type (llvm-sys:function-type-get return-type argument-types))
             (new-func (llvm-sys:function-create c-function-type
                                                 'llvm-sys:external-linkage
                                                 c-name
                                                 *the-module*))
             (*current-function* new-func)
             (*current-function-name* c-name))
        (unless (llvm-sys:llvmcontext-equal (llvm-sys:get-context *the-module*)
                                            (llvm-sys:get-context new-func))
          (error "The llvm-context for the~%module ~s~%the thread LLVMContext is ~s~% doesn't match the one for the new-func ~s~%the c-function-type context is ~s~% The function return-type context is: ~s~% The argument types are ~s~%"
                 (llvm-sys:get-context *the-module*)
                 (cmp:thread-local-llvm-context)
                 (llvm-sys:get-context new-func)
                 (llvm-sys:get-context c-function-type)
                 (llvm-sys:get-context return-type)
                 (mapcar #'llvm-sys:get-context argument-types)))
        (with-irbuilder ((llvm-sys:make-irbuilder (thread-local-llvm-context)))
          (let ((bb (irc-basic-block-create "entry" new-func)))
            (irc-set-insert-point-basic-block bb)
            (let* ((c-args (mapcar (lambda (arg argname)
                                     (llvm-sys:set-name arg argname)
                                     arg)
                                   (llvm-sys:get-argument-list new-func)
                                   c-argument-names))
                   ;; Generate code to translate the arguments.
                   (cl-args (mapcar (lambda (c-arg c-arg-name translator)
                                      (irc-intrinsic-call
                                       translator
                                       (list c-arg)
                                       (format nil "translated-~a" c-arg-name)))
                                    c-args c-argument-names argument-translator-names))
                   ;; Generate code to get the closure from the global variable from earlier.
                   (closure-to-call (irc-t*-load (literal:constants-table-reference closure-literal-slot-index) closure-var-name))
                   ;; Generate the code to actually call the lisp function.
                   ;; results-in-registers keeps things in the basic tmv format, because
                   ;; here we don't need the store/load values dance.
                   ;; (The C function only gets/needs/wants the primary value.)
                   (cl-result (irc-funcall-results-in-registers
                               closure-to-call cl-args (core:fmt nil "{}_closure" c-name))))
              ;; Now generate a call the translator for the return value if applicable, then return.
              ;; NOTE: (eq return-type %void%) doesn't seem to work - and it's sketchy because it's a symbol macro
              (if (string= return-translator-name "from_object_void")
                  (irc-ret-void)
                  (let ((c-result (irc-intrinsic-call
                                   return-translator-name
                                   ;; get the 0th value.
                                   (list (irc-tmv-primary cl-result))
                                   "c-result")))
                    (irc-ret c-result)))
              )))))))

(defun codegen-defcallback (result form env)
  (declare (ignore result))             ; no return value
  (destructuring-bind (c-name convention
                       return-type return-translator-name
                       argument-types argument-translator-names
                       parameters place-holder function)
      form
    (let ((closure-temp (alloca-t*)))
      (codegen closure-temp function env)
      (gen-defcallback c-name convention
                       return-type return-translator-name
                       argument-types argument-translator-names
                       parameters place-holder (irc-t*-load closure-temp)))))
