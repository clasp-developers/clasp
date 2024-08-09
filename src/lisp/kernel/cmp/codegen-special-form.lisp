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

;;; CORE::%ARRAY-DIMENSION

(defun gen-%array-dimension (array axisn)
  (let* ((untagged-axisn (irc-untag-fixnum axisn %i64% "untagged-axisn"))
         (untagged-dim (irc-array-dimension array untagged-axisn)))
    (irc-tag-fixnum untagged-dim "array-dimension")))

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

;;; CORE:VASLIST-LENGTH
;;; Get the count of remaining args in a vaslist.

(defun gen-vaslist-length (vaslist)
  (irc-tag-fixnum
   (irc-typed-load %size_t% (c++-field-ptr info.%vaslist% vaslist :nargs "vaslist-nargs"))))

;;; CORE:INSTANCE-REF

(defun gen-instance-ref (instance index)
  (irc-read-slot instance (irc-untag-fixnum index %size_t% "slot-location")))

;;; CORE:INSTANCE-SET

(defun gen-instance-set (instance index value)
  (irc-write-slot instance (irc-untag-fixnum index %size_t% "slot-location") value))

;;; CORE:INSTANCE-CAS

(defun gen-instance-cas (instance index old new)
  (irc-cmpxchg
   (irc-instance-slot-address
    instance (irc-untag-fixnum index %size_t% "slot-location"))
   old new))

;;; CORE:INSTANCE-RACK

(defun gen-instance-rack (instance) (irc-rack instance))

;;; CORE:INSTANCE-RACK-SET

(defun gen-instance-rack-set (instance rack)
  (irc-rack-set instance rack)
  rack)

;;; CORE:RACK-REF

(defun gen-rack-ref (rack index &key (order 'llvm-sys:monotonic))
  (irc-rack-read rack (irc-untag-fixnum index %size_t% "slot-location")
                 :order order))

;;; CORE:RACK-SET

(defun gen-rack-set (rack index value &key (order 'llvm-sys:monotonic))
  (irc-rack-write rack (irc-untag-fixnum index %size_t% "slot-location") value
                  :order order)
  value)

;;; DBG-i32

(defparameter *nexti* 10000)

;;; FOREIGN-CALL, FOREIGN-CALL-POINTER

(defun function-type-create-on-the-fly (foreign-types)
  (let ((arg-types (mapcar (lambda (type)
                             (clasp-ffi::safe-translator-type type))
                           (second foreign-types)))
        (varargs nil))
    (llvm-sys:function-type-get (clasp-ffi::safe-translator-type (first foreign-types)) arg-types varargs)))
