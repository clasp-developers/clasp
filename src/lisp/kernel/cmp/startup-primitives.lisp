(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "CMPREF")
    (make-package "CMPREF" :use '("CL")))
  (in-package #:cmpref))

(export '(*startup-primitives-as-list*
          generate-virtual-machine-header))

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
  '((nil "ltvc_make_nil"                  (:i8 :size_t))
    (nil "ltvc_make_t"                    (:i8 :size_t))
    (nil "ltvc_make_ratio"                (:i8 :size_t :t* :t*))
    (nil "ltvc_make_complex"              (:i8 :size_t :t* :t*))
    (nil "ltvc_make_cons"                 (:i8 :size_t))
    (nil "ltvc_rplaca"                    (:t* :t*))
    (nil "ltvc_rplacd"                    (:t* :t*))
    (nil "ltvc_make_list"                 (:i8 :size_t :size_t))
    (nil "ltvc_fill_list"                 (:t* :size_t) :varargs t)
    (nil "ltvc_make_array"                (:i8 :size_t :t* :t*))
    (nil "ltvc_setf_row_major_aref"       (:t* :size_t :t*))
    (nil "ltvc_make_hash_table"           (:i8 :size_t :t*))
    (nil "ltvc_setf_gethash"              (:t* :t* :t*))
    (nil "ltvc_make_fixnum"               (:i8 :size_t :uintptr_t))
    (nil "ltvc_make_package"              (:i8 :size_t :t*))
    (nil "ltvc_make_next_bignum"          (:i8 :size_t :bignum))
    (nil "ltvc_make_bitvector"            (:i8 :size_t :t*))
    (nil "ltvc_make_symbol"               (:i8 :size_t :t* :t*))
    (nil "ltvc_make_character"            (:i8 :size_t :uintptr_t))
    (nil "ltvc_make_base_string"          (:i8 :size_t :i8*))
    (nil "ltvc_make_pathname"             (:i8 :size_t :t* :t* :t* :t* :t* :t*))
    (nil "ltvc_make_function_description" (:i8 :size_t :t* :t* :t* :t* :t* :size_t
                                           :size_t :size_t))
    (nil "ltvc_make_global_entry_point"   (:i8 :size_t :size_t :t* :size_t))
    (nil "ltvc_make_local_entry_point"    (:i8 :size_t :size_t :t*))
    (nil "ltvc_ensure_fcell"              (:i8 :size_t :t*))
    (nil "ltvc_ensure_vcell"              (:i8 :size_t :t*))
    (nil "ltvc_make_random_state"         (:i8 :size_t :t*))
    (nil "ltvc_make_float"                (:i8 :size_t :single-float))
    (nil "ltvc_make_double"               (:i8 :size_t :double-float))
    (nil "ltvc_make_closurette"           (:i8 :size_t #|:size_t|# :size_t))
    (t   "ltvc_set_mlf_creator_funcall"   (:i8 :size_t :size_t :i8*))
    (t   "ltvc_mlf_init_funcall"          (:size_t :i8*))
    (t   "ltvc_mlf_init_basic_call"       (:t* :size_t) :varargs t)
    (t   "ltvc_mlf_create_basic_call"     (:i8 :size_t :t* :size_t) :varargs t)
    (t   "ltvc_set_ltv_funcall"           (:i8 :size_t :size_t :i8*))
    (t   "ltvc_toplevel_funcall"          (:size_t :i8*))))
