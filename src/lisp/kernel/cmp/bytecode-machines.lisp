(in-package :cmpref)

(defconstant +mask-arg+     #b011000)
(defconstant +constant-arg+ #b001000)
(defconstant +keys-arg+     #b011000)
(defconstant +label-arg+    #b010000)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun constant-arg (val)
    (logior +constant-arg+ val))
  
  (defun label-arg (val)
    (logior +label-arg+ val))
  
  (defun keys-arg (val)
    (logior +keys-arg+ val)))

(defun constant-arg-p (val)
  (= (logand +mask-arg+ val) +constant-arg+))

(defun label-arg-p (val)
  (= (logand +mask-arg+ val) +label-arg+))

(defun keys-arg-p (val)
  (= (logand +mask-arg+ val) +keys-arg+))

(defun unmask-arg (val) (logandc2 val +mask-arg+))

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

;;; *full-codes* contains descriptions of the instructions in the following format:
;;; (name opcode (args...) (long-args...))
;;; the name is a string.
;;; the args and long args are encoded as a number of bytes from 1 to 3, LOGIOR'd
;;; with the constant, label, and keys code that is appropriate, if any.
;;; One of these "instruction description" lists is what DECODE-INSTR returns.

(defun decode-instr (opcode)
  (let ((res (member opcode *full-codes* :key #'second)))
    (if res
        (first res)
        nil)))

#-building-clasp
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

#-building-clasp
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

#-building-clasp
(defun dump-gf-bytecode-virtual-machine (stream)
  (format stream "#ifdef GF_BYTECODE_VM~%")
  (loop for (_ opcode name . args) in *dtree-ops-as-list*
        for rargs = (first args) ; may be nil
        do (format stream "#define ~a ~a~%" name opcode)
           (loop for (_ arg) in rargs for i from 1
                 do (format stream "#define ~a ~a~%" arg i))
           (terpri stream))
  (format stream "#define DTREE_OP_COUNT ~a~%" (length *dtree-ops-as-list*))
  (format stream "#endif // GF_BYTECODE_VM~%"))

#-building-clasp
(defun dump-gf-bytecode-virtual-machine-macro-names (stream)
  (format stream "#ifdef GF_BYTECODE_VM_NAMES~%")
  (loop for (_ opcode name) in *dtree-ops-as-list*
        do (format stream "  case ~a: return ~s;~%" name (string name)))
  (format stream "#endif // GF_BYTECODE_VM_NAMES~%"))

#-building-clasp
(defun dump-python-gf-bytecode-virtual-machine (stream)
  (format stream "// This is where I dump the python GF bytecode VM~%"))

#-building-clasp
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

#-building-clasp
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

#-building-clasp
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

(defstruct ltv-info type c++-type suffix gcroots)

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

#-building-clasp
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

#-building-clasp
(defun build-ltv-functions (primitives &optional (stream *standard-output*))
  (format stream "#ifdef DEFINE_LTV_PARSERS~%")
  (dolist (prim primitives)
    (build-one-ltv-function prim stream))
  (format stream "#endif // DEFINE_LTV_PARSERS~%"))

#-building-clasp
(defun build-ltv-switch (primitives &optional (stream *standard-output*))
  (format stream "#ifdef DEFINE_LTV_SWITCH~%")
  (dolist (prim primitives)
    (format stream "  case ~a:~%    parse_~a(roots, bytecode, byteend, log);~%    break;~%"
            (first prim) (third prim)))
  (format stream "#endif // DEFINE_LTV_SWITCH~%"))

#-building-clasp
(defun build-ltv-machine (&optional (stream *standard-output*))
  (build-ltv-functions *startup-primitives-as-list* stream)
  (build-ltv-switch *startup-primitives-as-list* stream))

#-building-clasp
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
#-building-clasp
(defun generate-virtual-machine-header (fout)
  (generate-vm-codes fout)
  (generate-python-bytecode-table fout)
  (dump-gf-bytecode-virtual-machine fout)
  (dump-gf-bytecode-virtual-machine-macro-names fout)
  (dump-python-gf-bytecode-virtual-machine fout)
  (build-ltv-machine fout)
  (build-bytecode-ltv-ops fout))
