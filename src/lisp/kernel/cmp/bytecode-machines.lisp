(in-package :cmpref)

(defconstant +mask-arg+     #b011000)
(defconstant +constant-arg+ #b001000)
(defconstant +keys-arg+     #b011000)
(defconstant +label-arg+    #b010000)

(defun constant-arg (val)
  (logior +constant-arg+ val))

(defun label-arg (val)
  (logior +label-arg+ val))

(defun keys-arg (val)
  (logior +keys-arg+ val))

(macrolet ((defops (&rest ops)
             (let (rev-fullcodes
                   rev-codes
                   rev-defconstants)
               (dolist (op ops)
                 (destructuring-bind (name code &optional argument-codes long-argument-codes)
                     op
                   (let ((arguments (mapcar (lambda (code) (eval code)) argument-codes))
                         (long-arguments (mapcar (lambda (code) (eval code)) long-argument-codes)))
                     (push (list name code arguments long-arguments) rev-fullcodes))
                   (push name rev-codes)
                   (let ((sym (intern (format nil "+~a+" (string-upcase name))))
                         (cd code))
                     (push `(defconstant ,sym ,cd) rev-defconstants))))
               `(progn
                  (defvar *full-codes* ',(reverse rev-fullcodes))
                  (defvar *codes* ',(reverse rev-codes))
                  ,@rev-defconstants))))
  (defops
    ("ref" 0 (1) (2))
    ("const" 1 ((constant-arg 1)) ((constant-arg 2)))
    ("closure" 2 (1) (2))
    ("call" 3 (1) (2))
    ("call-receive-one" 4 (1) (2))
    ("call-receive-fixed" 5 (1 1) (2 2))
    ("bind" 6 (1 1) (2 2))
    ("set" 7 (1) (2))
    ("make-cell" 8)
    ("cell-ref" 9)
    ("cell-set" 10)
    ("make-closure" 11 ((constant-arg 1)) ((constant-arg 2)))
    ("make-uninitialized-closure" 12 ((constant-arg 1)) ((constant-arg 2)))
    ("initialize-closure" 13 (1) (2))
    ("return" 14)
    ("bind-required-args" 15 (1) (2))
    ("bind-optional-args" 16 (1 1) (2 2))
    ("listify-rest-args" 17 (1) (2))
    ("vaslistify-rest-args" 18 (1))
    ("parse-key-args" 19 (1 1 (keys-arg 1)) (2 2 (keys-arg 2)))
    ("jump-8" 20 ((label-arg 1)))
    ("jump-16" 21 ((label-arg 2)))
    ("jump-24" 22 ((label-arg 3)))
    ("jump-if-8" 23 ((label-arg 1)))
    ("jump-if-16" 24 ((label-arg 2)))
    ("jump-if-24" 25 ((label-arg 3)))
    ("jump-if-supplied-8" 26 ((label-arg 1)))
    ("jump-if-supplied-16" 27 ((label-arg 2)))
    ("check-arg-count-LE" 28 (1) (2))
    ("check-arg-count-GE" 29 (1) (2))
    ("check-arg-count-EQ" 30 (1) (2))
    ("push-values" 31)
    ("append-values" 32)
    ("pop-values" 33)
    ("mv-call" 34)
    ("mv-call-receive-one" 35)
    ("mv-call-receive-fixed" 36 (1) (2))
    ("save-sp" 37 (1) (2))
    ("restore-sp" 38 (1) (2))
    ("entry" 39 (1) (2))
    ("exit-8" 40 ((label-arg 1)))
    ("exit-16" 41 ((label-arg 2)))
    ("exit-24" 42 ((label-arg 3)))
    ("entry-close" 43)
    ("catch-8" 44 ((label-arg 1)))
    ("catch-16" 45 ((label-arg 2)))
    ("throw" 46)
    ("catch-close" 47)
    ("special-bind" 48 ((constant-arg 1)) ((constant-arg 2)))
    ("symbol-value" 49 ((constant-arg 1)) ((constant-arg 2)))
    ("symbol-value-set" 50 ((constant-arg 1)) ((constant-arg 2)))
    ("unbind" 51)
    ("progv" 52 ((constant-arg 1)) ((constant-arg 2)))
    ("fdefinition" 53 ((constant-arg 1)) ((constant-arg 2)))
    ("nil" 54)
    ("eq" 55)
    ("push" 56)
    ("pop" 57)
    ("dup" 58)
    ("fdesignator" 59 ((constant-arg 1)) ((constant-arg 2)))
    ("called-fdefinition" 60 ((constant-arg 1)) ((constant-arg 2)))
    ("protect" 61 ((constant-arg 1)) ((constant-arg 2)))
    ("cleanup" 62)
    ("encell" 63 (1) (2))
    ("long" 255)))

(defun pythonify-arguments (args)
  (declare (optimize (debug 3)))
  (let (rev-args)
    (dolist (arg args)
      (when (integerp arg)
        (push (format nil "~d" arg) rev-args))
      (when (consp arg)
        (push (let* ((fn-name (string-downcase (format nil "~a" (car arg))))
                     (fn-underscore-name (substitute #\_ #\- fn-name))
                     (num-arg (second arg)))
                (format nil "~a(~d)" fn-underscore-name num-arg))
              rev-args)))
    (nreverse rev-args)))

(defun generate-python-bytecode-table (fout)
  (format fout "#ifdef PYTHON_OPCODES~%")
  (format fout "R\"opcodes(~%")
  (dolist (full-code *full-codes*)
    (destructuring-bind (name code arguments long-arguments)
        full-code
      (let ((python-arguments (pythonify-arguments arguments))
            (python-long-arguments (pythonify-arguments long-arguments)))
        (format fout "new_instr( ~s, ~d, [~{ ~a~^, ~}], [~{ ~a~^, ~}] )~%"
                name
                code
                python-arguments
                python-long-arguments))))
  (format fout ")opcodes\"~%")
  (format fout "#endif~%"))

#-clasp
(defpackage :clos
  (:use #:common-lisp))

(in-package :clos)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defstruct (dtree-op (:type vector) :named)
    sym name code macro-name macros arguments long-arguments
    constant-argument-indices
    label-argument-indices)

  (defstruct (dtree-macro (:type vector) :named) name value))

(macrolet ((defops (&rest ops)
             (let ((new-dtree-ops (make-array (length ops)))
                   new-isa)
               (dolist (op ops)
                 (destructuring-bind (name code macro-name &optional argument-info)
                     op
                   (let* ((sym (intern (string-upcase name)))
                          (constant-argument-indices (make-array 4 :adjustable t :fill-pointer 0))
                          (label-argument-indices (make-array 4 :adjustable t :fill-pointer 0))
                          rev-arguments
                          rev-long-arguments
                          rev-macros)
                     (push (make-dtree-macro :name macro-name :value code) rev-macros)
                     (dotimes (index (length argument-info))
                       (let ((arg (elt argument-info index)))
                         (destructuring-bind (arg-type arg-name)
                             arg
                           (cond
                             ((eq arg-type 'constant-arg)
                              (vector-push-extend index constant-argument-indices)
                              (push `(constant-arg 1) rev-arguments)
                              (push `(constant-arg 2) rev-long-arguments)
                              (push (make-dtree-macro :name arg-name :value (1+ index)) rev-macros))
                             ((eq arg-type 'label-arg)
                              (vector-push-extend index label-argument-indices)
                              (push `(label-arg 1) rev-arguments)
                              (push `(label-arg 2) rev-long-arguments)
                              (push (make-dtree-macro :name arg-name :value (1+ index)) rev-macros))
                             ((eq arg-type 'register-arg)
                              (push `(register-arg 1) rev-arguments)
                              (push `(register-arg 2) rev-long-arguments)
                              (push (make-dtree-macro :name arg-name :value (1+ index)) rev-macros))
                             ((eq (car arg) 'offset)
                              (push (make-dtree-macro :name arg-name :value (1+ index)) rev-macros))
                             (t (error "Illegal argument type ~s" arg))))))
                     (let ((dtree-op (make-dtree-op :sym sym
                                                    :name name
                                                    :code code
                                                    :macro-name macro-name
                                                    :macros (nreverse rev-macros)
                                                    :arguments (nreverse rev-arguments)
                                                    :long-arguments (nreverse rev-long-arguments)
                                                    :constant-argument-indices (copy-seq constant-argument-indices)
                                                    :label-argument-indices (copy-seq label-argument-indices))))
                       (setf (elt new-dtree-ops code) dtree-op))
                     (push (list sym code) new-isa))))
               `(progn
                  (defparameter *dtree-ops* ',new-dtree-ops)
                  (defparameter *isa* ',new-isa)))))
  (defops
    ("miss" 0 "DTREE_OP_MISS")
    ("advance" 1 "DTREE_OP_ADVANCE")
    ("tag-test" 2 "DTREE_OP_TAG_TEST" ((label-arg "DTREE_FIXNUM_TAG_OFFSET")
                                       (label-arg "DTREE_SINGLE_FLOAT_TAG_OFFSET")
                                       (label-arg "DTREE_CHARACTER_TAG_OFFSET")
                                       (label-arg "DTREE_CONS_TAG_OFFSET")
                                       (offset "DTREE_GENERAL_TAG_OFFSET")))
    ("stamp-read" 3 "DTREE_OP_STAMP_READ" ((label-arg "DTREE_READ_HEADER_OFFSET")
                                           (offset "DTREE_READ_OTHER_OFFSET")))
    ("lt-branch" 4 "DTREE_OP_LT_BRANCH" ((constant-arg "DTREE_LT_PIVOT_OFFSET")
                                         (label-arg "DTREE_LT_LEFT_OFFSET")
                                         (offset "DTREE_LT_RIGHT_OFFSET")))
    ("eq-check" 5 "DTREE_OP_EQ_CHECK" ((constant-arg "DTREE_EQ_PIVOT_OFFSET")
                                       (offset "DTREE_EQ_NEXT_OFFSET")))
    ("range-check" 6 "DTREE_OP_RANGE_CHECK" ((constant-arg "DTREE_RANGE_MIN_OFFSET")
                                             (constant-arg "DTREE_RANGE_MAX_OFFSET")
                                             (offset "DTREE_RANGE_NEXT_OFFSET")))
    ("eql" 7 "DTREE_OP_EQL" ((constant-arg "DTREE_EQL_OBJECT_OFFSET")
                             (label-arg "DTREE_EQL_BRANCH_OFFSET")
                             (offset "DTREE_EQL_NEXT_OFFSET")))
    ("optimized-slot-reader" 8 "DTREE_OP_SLOT_READ" ((constant-arg "DTREE_SLOT_READER_INDEX_OFFSET")
                                                     (constant-arg "DTREE_SLOT_READER_SLOT_NAME_OFFSET")))
    ("optimized-slot-writer" 9 "DTREE_OP_SLOT_WRITE" ((constant-arg "DTREE_SLOT_WRITER_INDEX_OFFSET")))
    ("car" 10 "DTREE_OP_CAR" ((constant-arg "DTREE_CAR_READER_INDEX_OFFSET")
                              (constant-arg "DTREE_CAR_READER_CAR_NAME_OFFSET")))
    ("rplaca" 11 "DTREE_OP_RPLACA" ((constant-arg "DTREE_RPLACA_WRITER_INDEX_OFFSET")))
    ("effective-method-outcome" 12 "DTREE_OP_EFFECTIVE_METHOD" ((constant-arg "DTREE_EFFECTIVE_METHOD_OFFSET")))
    ("farg0" 13 "DTREE_OP_FARG0")
    ("farg1" 14 "DTREE_OP_FARG1")
    ("farg2" 15 "DTREE_OP_FARG2")
    ("farg3" 16 "DTREE_OP_FARG3")
    ("farg4" 17 "DTREE_OP_FARG4")
    ("argn" 18 "DTREE_OP_ARGN" ((register-arg "DTREE_ARGN_OFFSET")
                                (offset "DTREE_ARGN_NEXT_OFFSET")))
    ("sd-eq-branch" 19 "DTREE_OP_SD_EQ_BRANCH" ((constant-arg "DTREE_SD_STAMP_OFFSET")
                                                (label-arg "DTREE_SD_FAIL_OFFSET")
                                                (offset "DTREE_SD_NEXT_OFFSET")))
    ("single-dispatch-miss" 20 "DTREE_OP_SINGLE_DISPATCH_MISS")
    ))

(defun dump-gf-bytecode-virtual-machine (stream)
  (format stream "#ifdef GF_BYTECODE_VM~%")
  (dotimes (index (length *dtree-ops*))
    (let ((dtree-op (elt *dtree-ops* index)))
      (dolist (macro (dtree-op-macros dtree-op))
        (format stream "#define ~a ~a~%" (dtree-macro-name macro) (dtree-macro-value macro))
        (finish-output stream))
      (terpri stream)))
  (format stream "#define DTREE_OP_COUNT ~a~%" (length *dtree-ops*))
  (format stream "#endif // GF_BYTECODE_VM~%"))

(defun dump-gf-bytecode-virtual-machine-macro-names (stream)
  (format stream "#ifdef GF_BYTECODE_VM_NAMES~%")
  (dotimes (index (length *dtree-ops*))
    (let* ((dtree-op (elt *dtree-ops* index))
           (macro-name (dtree-op-macro-name dtree-op)))
      (format stream "  case ~a: return ~s;~%" macro-name (string macro-name))))
  (format stream "#endif // GF_BYTECODE_VM_NAMES~%"))

(defun dump-python-gf-bytecode-virtual-machine (stream)
  (format stream "// This is where I dump the python GF bytecode VM~%"))

(export '(dump-gf-bytecode-virtual-machine
          dump-gf-bytecode-virtual-machine-macro-names
          dump-python-gf-bytecode-virtual-machine) :clos)

(in-package :cmpref)

(defvar +reserved-c++-keywords+
  '("alignas"
    "alignof"
    "and"
    "and_eq"
    "asm"
    "atomic_cancel"
    "atomic_commit"
    "atomic_noexcept"
    "auto"
    "bitand"
    "bitor"
    "bool"
    "break"
    "case"
    "catch"
    "char"
    "char8_t"
    "char16_t"
    "char32_t"
    "class"
    "compl"
    "concept"
    "const"
    "consteval"
    "constexpr"
    "constinit"
    "const_cast"
    "continue"
    "co_await"
    "co_return"
    "co_yield"
    "decltype"
    "default"
    "delete"
    "do"
    "double"
    "dynamic_cast"
    "else"
    "enum"
    "explicit"
    "export"
    "extern"
    "false"
    "float"
    "for"
    "friend"
    "goto"
    "if"
    "inline"
    "int"
    "long"
    "mutable"
    "namespace"
    "new"
    "noexcept"
    "not"
    "not_eq"
    "nullptr"
    "operator"
    "or"
    "or_eq"
    "private"
    "protected"
    "public"
    "reflexpr"
    "register"
    "reinterpret_cast"
    "requires"
    "return"
    "short"
    "signed"
    "sizeof"
    "static"
    "static_assert"
    "static_cast"
    "struct"
    "switch"
    "synchronized"
    "template"
    "this"
    "thread_local"
    "throw"
    "true"
    "try"
    "typedef"
    "typeid"
    "typename"
    "union"
    "unsigned"
    "using"
    "virtual"
    "void"
    "volatile"
    "wchar_t"
    "while"
    "xor"
    "xor_eq"))

(defun c++ify (name)
  (when (member name +reserved-c++-keywords+ :test #'equalp)
    (setf name (concatenate 'string "_" name)))
  (flet ((submatch (substr remain)
           (let ((sublen (length substr)))
             (and (>= (length remain) sublen) (string= substr remain :start2 0 :end2 sublen)))))
    (with-output-to-string (sout)
      (dotimes (index (length name))
        (let* ((remain (subseq name index))
               (chr (elt remain 0)))
          (cond
            ((submatch "/=" remain)
             (format sout "_NE_")
             (incf index))
            ((submatch ">=" remain)
             (format sout "_GE_")
             (incf index))
            ((submatch "<=" remain)
             (format sout "_LE_")
             (incf index))
            ((char= chr #\=) (format sout "_EQ_"))
            ((char= chr #\<) (format sout "_LT_"))
            ((char= chr #\>) (format sout "_GT_"))
            ((char= chr #\-) (format sout "_"))
            (t (format sout "~a" chr))))))))

(defun generate-vm-codes (fout)
  (write-line "#ifdef VM_CODES" fout)
  (terpri fout)
  (let ((enums (let (rev-codes)
                 (dolist (item *full-codes*)
                   (let* ((name (first item))
                          (opcode (second item))
                          (sym-name (c++ify name)))
                     (push (format nil "~a=~a" sym-name opcode) rev-codes)))
                 (nreverse rev-codes))))
    (format fout "enum class vm_code : uint8_t {~%~{   ~a~^,~^~%~} };~%" enums))
  (terpri fout)
  (write-line "#endif // VM_CODES" fout))

;;; load time values machine

(defstruct (ltv-info (:type vector) :named) type c++-type suffix gcroots)

(defparameter *ltv-info* (make-hash-table :test #'equal))

(defun set-ltv-info (symbol c++-type suffix &optional gcroots)
  (setf (gethash symbol *ltv-info*) (make-ltv-info :type symbol :c++-type c++-type :suffix suffix :gcroots gcroots)))

(eval-when (:load-toplevel :execute)
  (set-ltv-info :i8 "char" "char")
  (set-ltv-info :size_t "size_t" "size_t")
  (set-ltv-info :t* "T_O*" "object" t)
  (set-ltv-info :i8* "string" "string")
  (set-ltv-info :short-float "short_float_t" "binary16")
  (set-ltv-info :single-float "float" "float")
  (set-ltv-info :double-float "double" "double")
  (set-ltv-info :binary80 "long_float_t" "binary80")
  (set-ltv-info :binary128 "long_float_t" "binary128")
  (set-ltv-info :uintptr_t "uintptr_t" "size_t")
  (set-ltv-info :bignum "T_O*" "bignum")
  (set-ltv-info :unknown "UNKNOWN" "UNKNOWN")
  )

(defun build-one-ltv-function (op &optional (stream *standard-output*))
  (destructuring-bind (code unwindsp name arg-types &key varargs)
      op
    (declare (ignore code unwindsp))
    (format stream "void parse_~a(gctools::GCRootsInModule* roots, char*& bytecode, char* byteend, bool log) {~%" name)
    (format stream "  if (log) printf(\"%s:%d:%s parse_~a\\n\", __FILE__, __LINE__, __FUNCTION__);~%" name)
    (let* ((arg-index 0)
           (vars (let (names)
                   (dolist (arg-type arg-types)
                     (let* ((ltv-info (let ((info (gethash arg-type *ltv-info*)))
                                        (if info
                                            info
                                            (make-ltv-info :type (format nil "UNKNOWN_~a" arg-type)
                                                           :c++-type (format nil "UNKNOWN_~a" arg-type)
                                                           :suffix (format nil "UNKNOWN_~a" arg-type)))))
                            (c++-arg-type (ltv-info-c++-type ltv-info))
                            (suffix (ltv-info-suffix ltv-info))
                            (gcroots (ltv-info-gcroots ltv-info))
                            (variable-name (cond
                                             ((and (= arg-index 0) (string= c++-arg-type "char"))  "tag")
                                             ((and (= arg-index 1) (string= c++-arg-type "size_t")) "index")
                                             (t (format nil "arg~a" arg-index))))
                            (read-variable-name (if (string= c++-arg-type "string")
                                                    (format nil "~a.c_str()" variable-name)
                                                    variable-name)))
                       (format stream "  ~a ~a = ltvc_read_~a(~a bytecode, byteend, log );~%" c++-arg-type variable-name
                               suffix
                               (if gcroots
                                   "roots, "
                                   ""))
                       (incf arg-index)
                       (push read-variable-name names)))
                   (nreverse names))))
      (when varargs
        (setf name (format nil "~a_varargs" name))
        (format stream "  Cons_O* varargs = ltvc_read_list( roots, ~a, bytecode, byteend, log );~%" (car (last vars )))
        (setf vars (append vars (list "varargs"))))
      (format stream "  ~a( roots" name)
      (dolist (var vars)
        (format stream ", ~a" var))
      (format stream ");~%")
      (format stream "};~%"))))

(defun build-ltv-functions (primitives &optional (stream *standard-output*))
  (format stream "#ifdef DEFINE_LTV_PARSERS~%")
  (dolist (prim primitives)
    (build-one-ltv-function prim stream))
  (format stream "#endif // DEFINE_LTV_PARSERS~%"))

(defun build-ltv-switch (primitives &optional (stream *standard-output*))
  (format stream "#ifdef DEFINE_LTV_SWITCH~%")
  (dolist (prim primitives)
    (format stream "  case ~a:~%    parse_~a(roots, bytecode, byteend, log);~%    break;~%"
            (first prim) (third prim)))
  (format stream "#endif // DEFINE_LTV_SWITCH~%"))

(defun build-ltv-machine (&optional (stream *standard-output*))
  (build-ltv-functions *startup-primitives-as-list* stream)
  (build-ltv-switch *startup-primitives-as-list* stream))

(defun build-bytecode-ltv-ops (&optional (stream *standard-output*))
  (format stream "~%#ifdef DEFINE_BYTECODE_LTV_OPS~%enum class bytecode_ltv : uint8_t {~%")
  (dolist (op +bytecode-ltv-ops+)
    (format stream "  ~(~a~) = ~a,~%"
            (c++ify (symbol-name (first op))) (second op)))
  (format stream "};~%enum class bytecode_uaet : uint8_t {~%")
  (loop for (key code) on +uaet-codes+ by #'cddr
        do (format stream "  ~(~a~) = ~a,~%"
                   (c++ify (symbol-name key)) code))
  (format stream "};~%enum class bytecode_debug_info : uint8_t {~%")
  (loop for (key code) on +debug-info-ops+ by #'cddr
        do (format stream "  ~(~a~) = ~a,~%"
                   (c++ify (symbol-name key)) code))
  (format stream "};~%#endif~%"))

;;; entry point

(defun generate-virtual-machine-header (fout)
  (generate-vm-codes fout)
  (generate-python-bytecode-table fout)
  (clos:dump-gf-bytecode-virtual-machine fout)
  (clos:dump-gf-bytecode-virtual-machine-macro-names fout)
  (clos:dump-python-gf-bytecode-virtual-machine fout)
  (build-ltv-machine fout)
  (build-bytecode-ltv-ops fout))
