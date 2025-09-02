(in-package #:cmpref)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ltvc intrinsics are for initialization of memory at load time
;;; ltvc stands for Load Time Virtual machine Call
;;; They are used to construct the a byte-code machine
;;; Each function takes arguments ( GCRootsInModule* gcroots, char tag, size_t targetIndex, ...args...)
;;;   The gcroots, tag, targetIndex are used to get a reference to a cell in the gcroots
;;;   and the ...args... are used to construct the object.
;;; If you add a new ltvc_xxxx function then do the following to rebuild the virtual machine
;;; To build the machine in the src/core/byte-code-interpreter.cc file manually use:
;;; (0) Erase the code in byte-code-interpreter.cc
;;; (1) ./waf build_rboehm
;;; (2) (literal::build-c++-machine)
;;; (3) copy the result into byte-code-interpreter.cc
;;; But the build system will rebuild the interpreter automatically.
;;;
(defvar *startup-primitives-as-list*
  ;; (unwindsp name argtypes &key varargs)
  '(( 65 nil "ltvc_make_nil"                  (:i8 :size_t))
    ( 66 nil "ltvc_make_t"                    (:i8 :size_t))
    ( 67 nil "ltvc_make_ratio"                (:i8 :size_t :t* :t*))
    ( 68 nil "ltvc_make_complex"              (:i8 :size_t :t* :t*))
    ( 69 nil "ltvc_make_cons"                 (:i8 :size_t))
    ( 70 nil "ltvc_rplaca"                    (:t* :t*))
    ( 71 nil "ltvc_rplacd"                    (:t* :t*))
    ( 72 nil "ltvc_make_list"                 (:i8 :size_t :size_t))
    ( 73 nil "ltvc_fill_list"                 (:t* :size_t) :varargs t)
    ( 74 nil "ltvc_make_array"                (:i8 :size_t :t* :t*))
    ( 75 nil "ltvc_setf_row_major_aref"       (:t* :size_t :t*))
    ( 76 nil "ltvc_make_hash_table"           (:i8 :size_t :t*))
    ( 77 nil "ltvc_setf_gethash"              (:t* :t* :t*))
    ( 78 nil "ltvc_make_fixnum"               (:i8 :size_t :uintptr_t))
    ( 79 nil "ltvc_make_package"              (:i8 :size_t :t*))
    ( 80 nil "ltvc_make_next_bignum"          (:i8 :size_t :bignum))
    ( 81 nil "ltvc_make_bitvector"            (:i8 :size_t :t*))
    ( 82 nil "ltvc_make_symbol"               (:i8 :size_t :t* :t*))
    ( 83 nil "ltvc_make_character"            (:i8 :size_t :uintptr_t))
    ( 84 nil "ltvc_make_base_string"          (:i8 :size_t :i8*))
    ( 85 nil "ltvc_make_pathname"             (:i8 :size_t :t* :t* :t* :t* :t* :t*))
    ( 86 nil "ltvc_make_function_description" (:i8 :size_t :t* :t* :t* :t* :t* :size_t
                                               :size_t :size_t))
    ( 87 nil "ltvc_make_global_entry_point"   (:i8 :size_t :size_t :t* :t*))
    ( 88 nil "ltvc_make_local_entry_point"    (:i8 :size_t :size_t :t*))
    ( 89 nil "ltvc_ensure_fcell"              (:i8 :size_t :t*))
    ( 90 nil "ltvc_ensure_vcell"              (:i8 :size_t :t*))
    ( 91 nil "ltvc_make_random_state"         (:i8 :size_t :t*))
    ( 92 nil "ltvc_make_binary32"             (:i8 :size_t :single-float))
    ( 93 nil "ltvc_make_binary64"             (:i8 :size_t :double-float))
    ( 94 nil "ltvc_make_binary80"             (:i8 :size_t :binary80))
    ( 95 t   "ltvc_set_mlf_creator_funcall"   (:i8 :size_t :size_t :i8*))
    ( 96 t   "ltvc_mlf_init_funcall"          (:size_t :i8*))
    ( 97 t   "ltvc_mlf_init_basic_call"       (:t* :size_t) :varargs t)
    ( 98 t   "ltvc_mlf_create_basic_call"     (:i8 :size_t :t* :size_t) :varargs t)
    ( 99 t   "ltvc_set_ltv_funcall"           (:i8 :size_t :size_t :i8*))
    (100 t   "ltvc_toplevel_funcall"          (:size_t :i8*))
    (102 nil "ltvc_make_binary16"             (:i8 :size_t :short-float))
    (103 nil "ltvc_make_binary128"            (:i8 :size_t :binary128))))

;;; Bytecode LTV Ops
;;; Instruction set is copied from Clasp for now. "sind" in the below means an
;;; index that the allocated object will be stored into. This may need some
;;; review later.
;;; Operations are as follows:
(defparameter +bytecode-ltv-ops+
  '((:nil 65 sind)
    (:t 66 sind)
    (:ratio 67)
    (:complex 68)
    (:cons 69 sind)
    (:init-cons 70 consind carind cdrind)
    (:base-string 72 size . data)
    (:utf8-string 73 nbytes . data)
    (:make-array 74 sind rank . dims)
    (:init-array 75 arrayind . valueinds)
    (:make-hash-table 76 sind test count)
    (:init-hash-table 77 htind keyind valueind)
    (:make-sb64 78 sind sb64)
    (:find-package 79 sind nameind)
    (:make-bignum 80 sind size . words) ; size is signed
    (:make-symbol 81) ; make-bitvector in clasp
    (:intern 82 sind packageind nameind) ; make-symbol in clasp
    (:make-character 83 sind ub32) ; ub64 in clasp, i think?
    (:make-pathname 85)
    (:make-bytecode-function 87) ; ltvc_make_global_entry_point
    (:make-bytecode-module 88) ; ltvc_make_local_entry_point - overriding
    (:setf-literals 89) ; make_random_state. compatibility is a sham here anyway
    (:make-binary32 90 sind ub32)
    (:make-binary64 91 sind ub64)
    (:make-binary80 92 sind ub80)
    (:funcall-create 93 sind find nargs . args)
    (:funcall-initialize 94 find nargs . args)
    (:fdefinition 95 find nameind)
    (:fcell 96 find nameind)
    (:vcell 97 vind nameind)
    (:find-class 98 sind cnind)
    ;; set-ltv-funcall in clasp- redundant
    #+(or) ; obsolete as of v0.3
    (:make-specialized-array 97 sind rank dims etype . elems)
    (:init-object-array 99 ub64)
    (:environment 100)
    (:symbol-value 101)
    (:make-binary16 102 sind ub16)
    (:make-binary128 103 sind ub128)
    (:attribute 255 name nbytes . data)))

(defvar +uaet-codes+
  '(:nil                #b00000000
    :t                  #b00000001
    :base-char          #b00100000
    :character          #b00100001
    :binary16           #b01000000
    :binary32           #b01000001
    :binary64           #b01000010
    :binary80           #b01000011
    :binary128          #b01000111
    :complex-binary16   #b01100000
    :complex-binary32   #b01100001
    :complex-binary64   #b01100010
    :complex-binary80   #b01100011
    :complex-binary128  #b01100100
    :unsigned-byte1     #b10000000
    :unsigned-byte2     #b10000001
    :unsigned-byte4     #b10000010
    :unsigned-byte8     #b10000011
    :unsigned-byte16    #b10000100
    :unsigned-byte32    #b10000101
    :unsigned-byte64    #b10000110
    :unsigned-byte128   #b10000111
    :signed-byte8       #b10100011
    :signed-byte16      #b10100100
    :signed-byte32      #b10100101
    :signed-byte64      #b10100110
    :signed-byte128     #b10100111))

(defvar +debug-info-ops+
  '(:function 0
    :vars 1
    :location 2
    :decls 3
    :the 4
    :block 5
    :exit 6
    :macro 7
    :if 8
    :tagbody 9))
