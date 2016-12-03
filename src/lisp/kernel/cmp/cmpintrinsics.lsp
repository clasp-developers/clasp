;;;
;;;    File: cmpintrinsics.lsp
;;;

;; Copyright (c) 2014, Christian E. Schafmeister
;;
;; CLASP is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Library General Public
;; License as published by the Free Software Foundation; either
;; version 2 of the License, or (at your option) any later version.
;;
;; See directory 'clasp/licenses' for full details.
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;; -^-


(in-package :cmp)

(defvar *irbuilder* nil
  "This is the IRBuilder that defines where all irc-xxx functions that generate IR code put the code.
Set this to other IRBuilders to make code go where you want")


(defvar *irbuilder-function-alloca* nil
  "Maintains an IRBuilder for function alloca instructions")
(defvar *irbuilder-function-body* nil
  "Maintains an IRBuilder for function body IR code")
(defvar *compilation-unit-module-index* 0
  "Incremented for each module build within a compilation-unit.
   It's used to get the proper order for ctor initialization.")

;;
;; Create types
;;

;; OPEN / TODO:
;; - ptrdiff_t
;; - ssize_t
;; - time_t

(defvar +i1+ (llvm-sys:type-get-int1-ty *llvm-context*))

(defvar +i8+ (llvm-sys:type-get-int8-ty *llvm-context*)) ;; -> CHAR / BYTE
(defvar +i8*+ (llvm-sys:type-get-pointer-to +i8+))
(defvar +i8**+ (llvm-sys:type-get-pointer-to +i8*+))

(defvar +i16+ (llvm-sys:type-get-int16-ty *llvm-context*)) ;; -> SHORT
(defvar +i16*+ (llvm-sys:type-get-pointer-to +i16+))
(defvar +i16**+ (llvm-sys:type-get-pointer-to +i16*+))

(defvar +i32+ (llvm-sys:type-get-int32-ty *llvm-context*)) ;; -> INT
(defvar +i32*+ (llvm-sys:type-get-pointer-to +i32+))
(defvar +i32**+ (llvm-sys:type-get-pointer-to +i32*+))

(defvar +i64+ (llvm-sys:type-get-int64-ty *llvm-context*)) ;; -> LONG, LONG LONG
(defvar +i64*+ (llvm-sys:type-get-pointer-to +i64+))
(defvar +i64**+ (llvm-sys:type-get-pointer-to +i64*+))

(defvar +i128+ (llvm-sys:type-get-int128-ty *llvm-context*)) ;; -> NOT USED !!!

(defvar +fixnum+ (if (member :address-model-64 *features*) ;; -> FIXNUM
                     +i64+
                     (error "Add support for non 64-bit address model")))

(defvar +float+ (llvm-sys:type-get-float-ty *llvm-context*))
(defvar +double+ (llvm-sys:type-get-double-ty *llvm-context*))
#+long-float (defvar +long-float+ (llvm-sys:type-get-long-float-ty *llvm-context*))

(defvar +size_t+
  (let ((sizeof-size_t (cdr (assoc 'core:size-t (llvm-sys:cxx-data-structures-info)))))
    (cond
      ((= 8 sizeof-size_t) +i64+)
      ((= 4 sizeof-size_t) +i32+)
      (t (error "Add support for size_t sizeof = ~a" sizeof-size_t)))))
(defvar +size_t*+ (llvm-sys:type-get-pointer-to +size_t+))
(defvar +size_t**+ (llvm-sys:type-get-pointer-to +size_t*+))

(defvar +void+ (llvm-sys:type-get-void-ty *llvm-context*))
(defvar +void*+ (llvm-sys:type-get-pointer-to +void+))

(defvar +vtable*+ +i8*+)

;;(defvar +exception-struct+ (llvm-sys:struct-type-get *llvm-context* (list +i8*+ +i32+) "exception-struct" nil))
(defvar +exception-struct+ (llvm-sys:struct-type-get *llvm-context* (list +i8*+ +i32+) nil))
(defvar +{i32.i1}+ (llvm-sys:struct-type-get *llvm-context* (list +i32+ +i1+) nil))
(defvar +{i64.i1}+ (llvm-sys:struct-type-get *llvm-context* (list +i64+ +i1+) nil))

(defvar +fn-ctor+
  (llvm-sys:function-type-get +void+ nil)
  "A ctor void ()* function prototype")
(defvar +fn-ctor-argument-names+ nil)
(defvar +fn-ctor*+ (llvm-sys:type-get-pointer-to +fn-ctor+)
  "A pointer to the ctor function prototype")


(defvar +global-ctors-struct+ (llvm-sys:struct-type-get *llvm-context* (list +i32+ +fn-ctor*+ +i8*+) nil))
(defvar +global-ctors-struct[1]+ (llvm-sys:array-type-get +global-ctors-struct+ 1)
  "An array of pointers to the global-ctors-struct")



(defvar +cxx-data-structures-info+ (llvm-sys:cxx-data-structures-info))

(defun get-cxx-data-structure-info (name &optional (info +cxx-data-structures-info+))
  (let ((find (assoc name info)))
    (or find (error "Could not find ~a in cxx-data-structures-info --> ~s~%" name info))
    (cdr find)))
(defvar +fixnum-mask+ (get-cxx-data-structure-info :fixnum-mask))
(defvar +tag-mask+ (get-cxx-data-structure-info :tag-mask))
(defvar +immediate-mask+ (get-cxx-data-structure-info :immediate-mask))
(defvar +cons-tag+ (get-cxx-data-structure-info :cons-tag))
(defvar +valist-tag+ (get-cxx-data-structure-info :valist-tag))
(defvar +fixnum-tag+ (get-cxx-data-structure-info :fixnum-tag))
(defvar +character-tag+ (get-cxx-data-structure-info :character-tag))
(defvar +single-float-tag+ (get-cxx-data-structure-info :single-float-tag))
(defvar +general-tag+ (get-cxx-data-structure-info :general-tag))
(defvar +VaList_S-size+ (get-cxx-data-structure-info :VaList_S-size))
(defvar +void*-size+ (get-cxx-data-structure-info :void*-size))
(defvar +alignment+ (get-cxx-data-structure-info :alignment))
(export '(+fixnum-mask+ +tag-mask+ +immediate-mask+
          +cons-tag+ +fixnum-tag+ +character-tag+ +single-float-tag+
          +general-tag+ +VaList_S-size+ +void*-size+ +alignment+ ))
(defvar +cons-car-offset+ (get-cxx-data-structure-info :cons-car-offset))
(defvar +cons-cdr-offset+ (get-cxx-data-structure-info :cons-cdr-offset))
(defvar +uintptr_t-size+ (get-cxx-data-structure-info :uintptr_t-size))
(defvar +intptr_t+
  (cond
    ((= 8 +uintptr_t-size+) +i64+)
    ((= 4 +uintptr_t-size+) +i32+)
    (t (error "Add support for size uintptr_t = ~a" sizeof-uintptr_t))))
(defvar +uintptr_t+
  (cond
    ((= 8 +uintptr_t-size+) +i64+)
    ((= 4 +uintptr_t-size+) +i32+)
    (t (error "Add support for size uintptr_t = ~a" sizeof-uintptr_t))))
(defun make-uintptr_t (x)
  (and (> x most-positive-fixnum) (error "make sure the integer ~s fits in a +i64+" x))
  (cond
    ((= 8 +uintptr_t-size+) (jit-constant-i64 x))
    ((= 4 +uintptr_t-size+) (jit-constant-i32 x))
    (t (error "Add support for size uintptr_t = ~a" sizeof-uintptr_t))))

;;; DO NOT CHANGE THE FOLLOWING STRUCT!!! IT MUST MATCH VaList_S

(defun build-list-of-pointers (size type)
  (multiple-value-bind (num-pointers remainder)
      (floor size +void*-size+)
    (unless (= remainder 0)
      (error "The ~a size ~a is not a multiple of sizeof(void*) ~a"
             type size +void*-size+))
    (make-list num-pointers :initial-element +i8*+)))

(defvar +VaList_S+ (llvm-sys:struct-type-get *llvm-context* (build-list-of-pointers +VaList_S-size+ "VaList") nil))
;;;(defvar +VaList_S+ (llvm-sys:struct-type-get *llvm-context* (list +vtable*+ +va_list+) nil)) ;; +size_t+ +t*+ +bool+) nil))
(defvar +VaList_S*+ (llvm-sys:type-get-pointer-to +VaList_S+))

(defvar +sp-counted-base+ (llvm-sys:struct-type-get *llvm-context* (list +i32+ +i32+) nil)) ;; "sp-counted-base-ty"
(defvar +sp-counted-base-ptr+ (llvm-sys:type-get-pointer-to +sp-counted-base+))
(defvar +shared-count+ (llvm-sys:struct-type-get *llvm-context* (list +sp-counted-base-ptr+) nil)) ;; "shared_count"

;;
;; Setup setjmp_buf type
;;
;; setjmp_buf buffers consist of five words.
;; Word 0 - Stores the frame-ptr (set by the library)
;; Word 1 - Stores the longjmp destination address - we set this
;; Word 2..4 - Three words for target specific data.
;; I'm going to use Word 2.. to represent different things.
;; For TAGBODY/GO Word2 will contain an i32 and the rest is padding
;; For BLOCK/RETURN-FROM Word2..4 will contain a T_mv pointer - it should fit

(defvar +setjmp.buf+ (llvm-sys:struct-type-get *llvm-context* (list +i8*+ +i8*+ +i8*+ +i8*+ +i8*+) nil))
(defvar +setjmp.buf*+ (llvm-sys:type-get-pointer-to +setjmp.buf+))

;;
;; Setup smart-ptr constants
;;
(multiple-value-bind (pointer-type pointer-px-offset pointer-px-size)
    (smart-pointer-details)
  (defvar +using-intrusive-reference-count+
    (eq pointer-type 'core::intrusive-reference-counted-pointer))
  (defvar +smart-ptr-px-offset+ pointer-px-offset))


(defun smart-pointer-fields (data-ptr-type &rest additional-fields)
  "List the types that make up a smart_ptr.
Boehm and MPS use a single pointer"
  (list* data-ptr-type additional-fields))


;;
;; If I use an opaque type then the symbol type gets duplicated and that causes
;; problems - try just using an int
;;(defvar +sym+ (llvm-sys:struct-type-get *llvm-context* nil nil)) ;; "Symbol_O"
(defvar +sym+ (llvm-sys:type-get-int32-ty *llvm-context*))
(defvar +sym-ptr+ (llvm-sys:type-get-pointer-to +sym+))
(defvar +symsp+ (llvm-sys:struct-type-get *llvm-context* (smart-pointer-fields +sym-ptr+) nil)) ;; "Sym_sp"
(defvar +symsp*+ (llvm-sys:type-get-pointer-to +symsp+))


;;
;; Store a core::Function_sp pointer
;;
(defvar +Function+ (llvm-sys:type-get-int32-ty *llvm-context*))
(defvar +Function-ptr+ (llvm-sys:type-get-pointer-to +Function+))
(defvar +Function_sp+ (llvm-sys:struct-type-get *llvm-context* (smart-pointer-fields +Function-ptr+) nil)) ;; "Cfn_sp"
(defvar +Function_sp*+ (llvm-sys:type-get-pointer-to +Function_sp+))



;; Define the T_O struct - right now just put in a dummy i32 - later put real fields here
(defvar +t+ (llvm-sys:struct-type-get *llvm-context* nil  nil)) ;; "T_O"
(defvar +t*+ (llvm-sys:type-get-pointer-to +t+))
(defvar +t**+ (llvm-sys:type-get-pointer-to +t*+))
(defvar +t*[0]+ (llvm-sys:array-type-get +t*+ 0))
(defvar +t*[0]*+ (llvm-sys:type-get-pointer-to +t*[0]+))
(defvar +tsp+ (llvm-sys:struct-type-get *llvm-context* (smart-pointer-fields +t*+) nil))  ;; "T_sp"
(defvar +tsp[0]+ (llvm-sys:array-type-get +tsp+ 0))
(defvar +tsp[0]*+ (llvm-sys:type-get-pointer-to +tsp[0]+))
(defvar +tsp[1]+ (llvm-sys:array-type-get +tsp+ 1))
(defvar +tsp[1]*+ (llvm-sys:type-get-pointer-to +tsp[1]+))
(defvar +tsp[2]+ (llvm-sys:array-type-get +tsp+ 2))
(defvar +tsp[2]*+ (llvm-sys:type-get-pointer-to +tsp[2]+))
(defvar +tsp[DUMMY]+ (llvm-sys:array-type-get +tsp+ 65536))
(defvar +tsp[DUMMY]*+ (llvm-sys:type-get-pointer-to +tsp[DUMMY]+))
(defvar +tsp*+ (llvm-sys:type-get-pointer-to +tsp+))
(defvar +tsp**+ (llvm-sys:type-get-pointer-to +tsp*+))

;; This structure must match the gctools::ConstantsTable structure
(defvar +constants-table+ (llvm-sys:struct-type-get *llvm-context* (list +i8*+ +i8*+ +size_t+) nil))
(defvar +constants-table*+ (llvm-sys:type-get-pointer-to +constants-table+))

;; The definition of +tmv+ doesn't quite match T_mv because T_mv inherits from T_sp
(defvar +tmv+ (llvm-sys:struct-type-get *llvm-context* (smart-pointer-fields +t*+ +size_t+) nil))  ;; "T_mv"
(defvar +return_type+ +tmv+)
(defvar +tmv*+ (llvm-sys:type-get-pointer-to +tmv+))
(defvar +tmv**+ (llvm-sys:type-get-pointer-to +tmv*+))

(defvar +gcvector-tsp+ (llvm-sys:struct-type-get *llvm-context* (list +size_t+ +size_t+ +tsp+) nil))
(defvar +gcvector-symsp+ (llvm-sys:struct-type-get *llvm-context*(list +size_t+ +size_t+ +symsp+) nil))
(defvar +vec0-tsp+ (llvm-sys:struct-type-get *llvm-context*(list +gcvector-tsp+) nil))
(defvar +vec0-symsp+ (llvm-sys:struct-type-get *llvm-context* (list +gcvector-symsp+) nil))

;; Define the LoadTimeValue_O struct - right now just put in a dummy i32 - later put real fields here
(defvar +ltv+ (llvm-sys:struct-type-get *llvm-context* (list +vtable*+ #+(or) +vec0-tsp+)  nil)) ;; "LoadTimeValue_O"
(defvar +ltv*+ (llvm-sys:type-get-pointer-to +ltv+))
(defvar +ltv**+ (llvm-sys:type-get-pointer-to +ltv*+))
(defvar +ltvsp+ (llvm-sys:struct-type-get *llvm-context* (smart-pointer-fields +ltv*+) nil))  ;; "LoadTimeValue_sp"
#+(or)(defvar +ltvsp*+ (llvm-sys:type-get-pointer-to +ltvsp+))
#+(or)(defvar +ltvsp**+ (llvm-sys:type-get-pointer-to +ltvsp*+))


(defvar +mv-limit+ (cdr (assoc :multiple-values-limit (llvm-sys:cxx-data-structures-info))))
(defvar +mv-values-array+ (llvm-sys:array-type-get +t*+ +mv-limit+))
(defvar +mv-struct+ (llvm-sys:struct-type-get *llvm-context* (list +size_t+ +mv-values-array+) nil #|| is-packed ||#))
(defvar +mv-struct*+ (llvm-sys:type-get-pointer-to +mv-struct+))
(defvar +thread-info-struct+ (llvm-sys:struct-type-get *llvm-context* (list +mv-struct+) nil))



#+(or)(progn
        (defvar +af+ (llvm-sys:struct-type-get *llvm-context* nil  nil)) ;; "ActivationFrame_O"
        (defvar +af-ptr+ (llvm-sys:type-get-pointer-to +af+))
        (defvar +afsp+ (llvm-sys:struct-type-get *llvm-context* (smart-pointer-fields +af-ptr+)  nil)) ;; "ActivationFrame_sp"
        (defvar +afsp*+ (llvm-sys:type-get-pointer-to +afsp+))
        )

;; Substitute afsp* with tsp
(defvar +afsp+ +tsp+)
(defvar +afsp*+ +tsp*+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up the calling convention using core:+number-of-fixed-arguments+ to define the types
;; and names of the arguments passed in registers
;;
;; The last passed-arg is va-list
(defstruct (calling-convention-impl (:type vector))
  valist
  nargs
  register-args ;; the last passed-arg is a va-list
  )

;; Parse the function arguments into a (values closed-env calling-convention)
(defun parse-function-arguments (arguments)
  (let ((closed-env (first arguments))
        (cc (make-calling-convention-impl
             :valist (second arguments)
             :nargs (third arguments) ;; The number of arguments
             :register-args (nthcdr 3 arguments))))
    (values closed-env cc)))

(defun calling-convention-nargs (cc)
  (calling-convention-impl-nargs cc))

(defun calling-convention-register-args (cc)
  (calling-convention-impl-register-args cc))

;; If there is no va-list return nil - Why would this ever be so?
(defun calling-convention-va-list (cc)
  (calling-convention-impl-valist cc))


(defun calling-convention-args.va-end (cc)
  (irc-intrinsic "llvm.va_end" (calling-convention-va-list cc)))

;;;
;;; Read the next argument from the va_list
;;; Everytime the arg-idx is incremented, this function must be called.
;;; The arg-idx is not used but it is passed because I used to index into an array
(defun calling-convention-args.va-arg (cc arg-idx &optional target-idx)
  (declare (ignore arg-idx))
  (let ((label (if (and target-idx core::*enable-print-pretty*)
                   (bformat nil "arg-%d" target-idx)
                   "rawarg")))
    (irc-intrinsic "cc_va_arg" (calling-convention-va-list cc))))

  (defvar *register-arg-types* nil)
  (defvar *register-arg-names* nil)
  (let (arg-types arg-names)
    (dotimes (i core:+number-of-fixed-arguments+)
      (push +t*+ arg-types)
      (push (bformat nil "farg%d" i) arg-names))
    ;; va-list arg
    (setf *register-arg-types* (nreverse arg-types)
          *register-arg-names* (nreverse arg-names)))
  (defvar +fn-registers-prototype-argument-names+
    (list* "closure-ptr" "va-list" "nargs" *register-arg-names*))
  (defvar +fn-registers-prototype+
    (llvm-sys:function-type-get
     +tmv+
     (list* +t*+ +VaList_S*+ +size_t+ *register-arg-types*))
    "X86_64 calling convention The general function prototypes pass the following pass:
1) A closed over runtime environment a pointer to a closure.
2) A valist of remaining arguments
3) The number of arguments +size_t+
4) core::+number-of-fixed-arguments+ T_O* pointers,
   the first arguments passed in registers,
5) The remaining arguments are on the stack
      If no argument is passed then pass NULL.")

  (progn
    (defvar +fn-prototype+ +fn-registers-prototype+)
    (defvar +fn-prototype-argument-names+ +fn-registers-prototype-argument-names+))

  (defvar +fn-prototype*+ (llvm-sys:type-get-pointer-to +fn-prototype+)
    "A pointer to the function prototype")

  (defvar +fn-prototype**+ (llvm-sys:type-get-pointer-to +fn-prototype*+)
    "A pointer to a pointer to the function prototype")

  (defvar +fn-prototype*[0]+ (llvm-sys:array-type-get +fn-prototype*+ 0)
    "An array of pointers to the function prototype")

  (defvar +fn-prototype*[1]+ (llvm-sys:array-type-get +fn-prototype*+ 1)
    "An array of pointers to the function prototype")
  (defvar +fn-prototype*[2]+ (llvm-sys:array-type-get +fn-prototype*+ 2)
    "An array of pointers to the function prototype")

  ;;
  ;; Define the InvocationHistoryFrame type for LispCompiledFunctionIHF
  ;;
  ;; %"class.core::InvocationHistoryFrame" = type { i32 (...)**, i32, %"class.core::InvocationHistoryStack"*, %"class.core::InvocationHistoryFrame"*, i8, i32 }
  (defvar +InvocationHistoryStack*+ +i8*+ "Make this a generic pointer")
  (defparameter +InvocationHistoryFrame+ (llvm-sys:struct-type-create *llvm-context* :name "InvocationHistoryFrame"))
  (defparameter +InvocationHistoryFrame*+ (llvm-sys:type-get-pointer-to +InvocationHistoryFrame+))
  (llvm-sys:set-body +InvocationHistoryFrame+ (list +i32**+ +i32+ +InvocationHistoryStack*+ +InvocationHistoryFrame*+ +i8+ +i32+) nil)
  (defvar +LispFunctionIHF+ (llvm-sys:struct-type-create *llvm-context* :elements (list +InvocationHistoryFrame+ +tsp+ +tsp+ +tsp+ +i32+ +i32+) :name "LispFunctionIHF"))
  ;; %"class.core::LispCompiledFunctionIHF" = type { %"class.core::LispFunctionIHF" }
  (defvar +LispCompiledFunctionIHF+ (llvm-sys:struct-type-create *llvm-context* :elements (list +LispFunctionIHF+) :name "LispCompiledFunctionIHF"))


  (defun make-gv-source-file-info-handle (module &optional handle)
    (if (null handle) (setq handle -1))
    (llvm-sys:make-global-variable module
                                   +i32+  ; type
                                   nil    ; constant
                                   'llvm-sys:internal-linkage
                                   (jit-constant-i32 handle)
                                   "source-file-info-handle"))


  (defun add-global-ctor-function (module main-function)
    "Create a function with the name core:+clasp-ctor-function-name+ and
have it call the main-function"
  (let ((*the-module* module))
    (let ((fn (with-new-function
                  (ctor-func func-env result
                             :function-name core:+clasp-ctor-function-name+
                             :parent-env nil
                             :linkage 'llvm-sys:internal-linkage
                             :function-type +fn-ctor+
                             :return-void t
                             :argument-names +fn-ctor-argument-names+ )
                (let* ((bc-bf (irc-bit-cast main-function +fn-prototype*+ "fnptr-pointer")))
                  (irc-intrinsic "cc_register_startup_function" bc-bf)))))
      fn)))

  (defun find-global-ctor-function (module)
    (let ((ctor (llvm-sys:get-function module core:+clasp-ctor-function-name+)))
      (or ctor (error "Couldn't find the ctor-function: ~a" core:+clasp-ctor-function-name+))
      ctor))

  (defun remove-llvm.global_ctors-if-exists (module)
    (let ((global (llvm-sys:get-named-global module "llvm.global_ctors")))
      (if global
          (llvm-sys:erase-from-parent global))))

(defun add-llvm.global_ctors (module priority global-ctor-function)
  (or global-ctor-function (error "global-ctor-function must not be NIL"))
  (llvm-sys:make-global-variable
   module
   +global-ctors-struct[1]+
   nil
   'llvm-sys:appending-linkage
   (llvm-sys:constant-array-get
    +global-ctors-struct[1]+
    (list
     (llvm-sys:constant-struct-get +global-ctors-struct+
                                   (list
                                    (jit-constant-i32 priority)
                                    global-ctor-function
                                    (llvm-sys:constant-pointer-null-get +i8*+)))))
   "llvm.global_ctors"))

(defun make-boot-function-global-variable (module func-ptr)
  "* Arguments
- module :: An llvm module
- func-ptr :: An llvm function
* Description
Add the global variable llvm.global_ctors to the Module (linkage appending)
and initialize it with an array consisting of one function pointer."
  (let* ((global-ctor (add-global-ctor-function module func-ptr)))
    (incf *compilation-unit-module-index*)
    (add-llvm.global_ctors module *compilation-unit-module-index* global-ctor)))

;;
;; Ensure that the LLVM model of
;;   tsp matches shared_ptr<xxx> and
;;   tmv matches multiple_values<xxx>
;;
(let* ((module (llvm-create-module (next-run-time-module-name)))
       (engine-builder (llvm-sys:make-engine-builder module))
       (target-options (llvm-sys:make-target-options)))
  ;; module is invalid after make-engine-builder call
  (llvm-sys:set-target-options engine-builder target-options)
  (let* ((execution-engine (llvm-sys:create engine-builder))
         (data-layout (llvm-sys:get-data-layout execution-engine))
         (tsp-size (llvm-sys:data-layout-get-type-alloc-size data-layout +tsp+))
         (tmv-size (llvm-sys:data-layout-get-type-alloc-size data-layout +tmv+))
         (constants-table-size (llvm-sys:data-layout-get-type-alloc-size data-layout +constants-table+)))
    (llvm-sys:throw-if-mismatched-structure-sizes :tsp tsp-size :tmv tmv-size :contab constants-table-size)))

;;
;; Define exception types in the module
;;
;; TODO:  Holy crap! We are using hard-coded mangled clang names for the typeinfo records for each
;;        exception type!  Find some portable way to do this!!!!!!!!
;; Currently these names are scraped from the libCore library using "nm"

(defvar *exceptions*
  '(
    (typeid-core-catch-throw "_ZTIN4core10CatchThrowE")
    (typeid-core-dynamic-go  "_ZTIN4core9DynamicGoE")
    (typeid-core-return-from "_ZTIN4core10ReturnFromE")
    (typeid-core-unwind      "_ZTIN4core6UnwindE")
    ))

;;#+debug-mps (bformat t "cmp::*exceptions* --> %s\n" *exceptions*)


(defvar *exception-types-hash-table* (make-hash-table :test #'eq)
  "Map exception names to exception class extern 'C' names")

(mapcar #'(lambda (x &aux (name (car x)) (cname (cadr x)))
	    (core::hash-table-setf-gethash *exception-types-hash-table* name cname))
	*exceptions*)

(defun exception-typeid*-from-name (name)
  (let* ((cname (gethash name *exception-types-hash-table*))
	 (i8* (llvm-sys:get-or-create-external-global *the-module* cname +i8+)))
    i8*))

;;
;; Define functions within the module
;;

(defparameter *primitives* (make-hash-table :test 'equal))


(defun matching-arguments (required-type given-type arg-index)
  (if (equal required-type +tsp*-or-tmv*+)
      (if (eql arg-index 1)
	  (if (or (equal given-type +tsp*+) (equal given-type +tmv*+))
	      t
	      nil)
	  (error ":tsp*-or-tmv* can only be specified as the first argument of an intrinsic function"))
      (equal required-type given-type)))

(defun assert-result-isa-llvm-value (result)
  (unless (llvm-sys:llvm-value-p result)
      (error "result must be an instance of llvm-sys:Value_O but instead it has the value %s" result)))

(defun throw-if-mismatched-arguments (fn-name args)
  (let* ((info (gethash fn-name *primitives*))
	 (return-ty (car info))
	 (required-args-ty (cadr info))
	 (passed-args-ty (mapcar #'(lambda (x)
				     (if (llvm-sys:llvm-value-p x)
					 (if (llvm-sys:valid x)
                                             (llvm-sys:get-type x)
                                             (error "Invalid (NULL pointer) value ~a about to be passed to intrinsic function ~a" x fn-name))
					 (core:class-name-as-string x)))
				 args))
	 (i 1))
    (mapc #'(lambda (x y z)
	      (unless (matching-arguments x y i)
		(error "Constructing call to intrinsic ~a - mismatch of arg#~a value[~a], expected type ~a - received type ~a" fn-name i z x y))
	      (setq i (1+ i))
	      ) required-args-ty passed-args-ty args)))

(defconstant +tsp*-or-tmv*+ :tsp*-or-tmv*
  "This is a stand-in for a first argument type that can either be tsp* or tmv*")

(defun dispatch-function-name (name &optional required-first-argument-type)
  (let ((name-dispatch-prefix
	 (cond
	   ((equal required-first-argument-type +tsp*+)
	    "sp_")
	   ((equal required-first-argument-type +tmv*+)
	    "mv_")
	   (t
	    ""))))
    (bformat nil "%s%s" name-dispatch-prefix name)))

;;;
;;; An attempt to inline specific functions from intrinsics.cc
;;;


;;#+(or)
(defun create-primitive-function (module name return-ty args-ty varargs does-not-throw does-not-return)
  (let ((fnattrs nil))
    (when does-not-throw (push 'llvm-sys:attribute-no-unwind fnattrs))
    (when does-not-return (push 'llvm-sys:attribute-no-return fnattrs))
    (push '("no-frame-pointer-elim" "false") fnattrs)
    (push "no-frame-pointer-elim-non-leaf" fnattrs)
    (cmp:irc-function-create (llvm-sys:function-type-get return-ty args-ty varargs)
                             'llvm-sys::External-linkage
                             name module
                             :function-attributes fnattrs)))

(defun primitive (module name return-ty args-ty &key varargs does-not-throw does-not-return )
  (mapc #'(lambda (x)
	    (when (equal +tsp*-or-tmv*+ x)
	      (error "When defining primitive ~a --> :tsp*-or-tmv* is only allowed in the first argument position" name ))) (cdr args-ty))
  (if (equal (car args-ty) +tsp*-or-tmv*+)
      (progn ;; create two versions of the function - one prefixed with sp_ and the other with mv_
	(create-primitive-function module
				   (dispatch-function-name name +tsp*+)
				   return-ty
				   (cons +tsp*+ (cdr args-ty))
				   varargs does-not-throw does-not-return)
	(create-primitive-function module
				   (dispatch-function-name name +tmv*+)
				   return-ty
				   (cons +tmv*+ (cdr args-ty))
				   varargs does-not-throw does-not-return))
      (create-primitive-function module
				 name return-ty args-ty varargs does-not-throw does-not-return))
  (core::hash-table-setf-gethash *primitives* name
				 (list return-ty args-ty '( (:varargs . varargs)
							   (:does-not-throw . does-not-throw)
							   ( :does-not-return . does-not-return) ))))


(defun primitive-nounwind (module name return-ty args-ty &key varargs does-not-return)
  (primitive module name return-ty args-ty :varargs varargs :does-not-throw t :does-not-return does-not-return))

(defun define-primitives-in-module (module)

  (primitive-nounwind module "ltvc_assign_source_file_info_handle" +void+ (list +i8*+ +i8*+ +size_t+ +i32+ +i32*+))
  (primitive-nounwind module "ltvc_make_nil" +t*+ (list +constants-table*+ +size_t+))
  (primitive-nounwind module "ltvc_make_t" +t*+ (list +constants-table*+ +size_t+))
  (primitive-nounwind module "ltvc_make_ratio" +t*+ (list +constants-table*+ +size_t+ +t*+ +t*+))
  (primitive-nounwind module "ltvc_make_cons" +t*+ (list +constants-table*+ +size_t+ +t*+ +t*+))
  (primitive-nounwind module "ltvc_make_list" +t*+ (list +constants-table*+ +size_t+ +size_t+) :varargs t)
  (primitive-nounwind module "ltvc_make_array" +t*+ (list +constants-table*+ +size_t+ +t*+ +t*+))
  (primitive-nounwind module "ltvc_setf_row_major_aref" +t*+ (list +t*+ +size_t+ +t*+))
  (primitive-nounwind module "ltvc_make_hash_table" +t*+ (list +constants-table*+ +size_t+ +t*+))
  (primitive-nounwind module "ltvc_setf_gethash" +t*+ (list +t*+ +t*+ +t*+))
  (primitive-nounwind module "ltvc_make_fixnum" +t*+ (list +constants-table*+ +size_t+ +uintptr_t+))
  (primitive-nounwind module "ltvc_make_package" +t*+ (list +constants-table*+ +size_t+ +t*+))
  (primitive-nounwind module "ltvc_make_bignum" +t*+ (list +constants-table*+ +size_t+ +t*+))
  (primitive-nounwind module "ltvc_make_random_state" +t*+ (list +constants-table*+ +size_t+ +t*+))
  (primitive-nounwind module "ltvc_make_symbol" +t*+ (list +constants-table*+ +size_t+ +t*+ +t*+))
  (primitive-nounwind module "ltvc_make_character" +t*+ (list +constants-table*+ +size_t+ +uintptr_t+))
  (primitive-nounwind module "ltvc_make_base_string" +t*+ (list +constants-table*+ +size_t+ +i8*+))
  (primitive-nounwind module "ltvc_make_pathname" +t*+ (list +constants-table*+ +size_t+ +t*+ +t*+ +t*+ +t*+ +t*+ +t*+))
  (primitive-nounwind module "ltvc_make_package" +t*+ (list +constants-table*+ +size_t+ +t*+))
  (primitive-nounwind module "ltvc_make_built_in_class" +t*+ (list +constants-table*+ +size_t+ +t*+))
  (primitive-nounwind module "ltvc_make_float" +t*+ (list +constants-table*+ +size_t+ +float+))
  (primitive-nounwind module "ltvc_make_double" +t*+ (list +constants-table*+ +size_t+ +double+))
  (primitive-nounwind module "ltvc_make_complex" +t*+ (list +constants-table*+ +size_t+ +t*+ +t*+))
  (primitive          module "ltvc_set_mlf_creator_funcall" +t*+ (list +constants-table*+ +size_t+ +fn-prototype*+))
  (primitive          module "ltvc_mlf_init_funcall" +t*+ (list +fn-prototype*+))
  (primitive          module "ltvc_set_ltv_funcall" +t*+ (list +constants-table*+ +size_t+ +fn-prototype*+))
  (primitive          module "ltvc_toplevel_funcall" +t*+ (list +fn-prototype*+))

  (primitive-nounwind module "newFunction_sp" +void+ (list +Function_sp*+))
  (primitive-nounwind module "newTsp" +void+ (list +tsp*+))
  (primitive-nounwind module "copyTsp" +void+ (list +tsp*-or-tmv*+ +tsp*+))
  (primitive-nounwind module "copyTspTptr" +void+ (list +tsp*-or-tmv*+ +t*+))
  (primitive-nounwind module "compareTspTptr" +i32+ (list +tsp*+ +t*+))

  (primitive-nounwind module "newTmv" +void+ (list +tmv*+))
  (primitive-nounwind module "resetTmv" +void+ (list +tmv*+))
  (primitive-nounwind module "copyTmv" +void+ (list +tmv*+ +tmv*+))
  (primitive-nounwind module "copyTmvOrSlice" +void+ (list +tsp*-or-tmv*+ +tmv*+))

  (primitive-nounwind module "isTrue" +i32+ (list +tsp*+))
  (primitive-nounwind module "isBound" +i32+ (list +tsp*+))

  (primitive-nounwind module "internSymbol_tsp" +void+ (list +tsp*+ +i8*+ +i8*+))
  (primitive-nounwind module "makeSymbol_tsp" +void+ (list +tsp*+ +i8*+))

  (primitive-nounwind module "internSymbol_symsp" +void+ (list +symsp*+ +i8*+ +i8*+))
  (primitive-nounwind module "makeSymbol_symsp" +void+ (list +symsp*+ +i8*+))

  (primitive-nounwind module "makeNil" +void+ (list +tsp*-or-tmv*+))
  (primitive-nounwind module "makeT" +void+ (list +tsp*+))
  (primitive-nounwind module "makeCons" +void+ (list +tsp*+ +tsp*+ +tsp*+))
  (primitive-nounwind module "makeFixnum" +void+ (list +tsp*+ +fixnum+))
  (primitive-nounwind module "makeCharacter" +void+ (list +tsp*+ +i32+))
  (primitive-nounwind module "makeBignum" +void+ (list +tsp*+ +i8*+))
  #+short-float (primitive-nounwind module "makeShortFloat" +void+ (list +tsp*+ +double+))
  (primitive-nounwind module "makeSingleFloat" +void+ (list +tsp*+ +float+))
  (primitive-nounwind module "makeDoubleFloat" +void+ (list +tsp*+ +double+))

  #+long-float (primitive-nounwind module "makeLongFloat" +void+ (list +tsp*+ +long-float+))
  (primitive-nounwind module "makeString" +void+ (list +tsp*+ +i8*+))
  (primitive-nounwind module "makePathname" +void+ (list +tsp*+ +i8*+))
  (primitive-nounwind module "makeCompiledFunction" +void+ (list +tsp*-or-tmv*+ +fn-prototype*+ +i32*+ +size_t+ +size_t+ +size_t+ +tsp*+ +tsp*+ +afsp*+ +tsp*+))

  (primitive          module "symbolValueRead" +void+ (list +tsp*-or-tmv*+ +tsp*+))
  (primitive-nounwind module "symbolValueReference" +tsp*+ (list +tsp*+))
  (primitive-nounwind module "lexicalValueReference" +tsp*+ (list +i32+ +i32+ +afsp*+))
  (primitive-nounwind module "lexicalValueRead" +void+ (list +tsp*-or-tmv*+ +i32+ +i32+ +afsp*+))
  (primitive-nounwind module "symbolFunctionRead" +void+ (list +tsp*-or-tmv*+ +tsp*+))
  (primitive-nounwind module "setfSymbolFunctionRead" +void+ (list +tsp*+ +tsp*+))
  (primitive-nounwind module "lexicalFunctionRead" +void+ (list +tsp*-or-tmv*+ +i32+ +i32+ +afsp*+))


  (primitive-nounwind module "makeTagbodyFrame" +void+ (list +afsp*+))
  (primitive-nounwind module "makeValueFrame" +void+ (list +tsp*+ +i64+))
  (primitive-nounwind module "setParentOfActivationFrameFromClosure" +void+ (list +tsp*+ +t*+))
  (primitive-nounwind module "setParentOfActivationFrame" +void+ (list +tsp*+ +tsp*+))

  (primitive-nounwind module "attachDebuggingInfoToValueFrame" +void+ (list +afsp*+ +tsp*+))

  (primitive-nounwind module "valueFrameReference" +tsp*+ (list +afsp*+ +i32+))

  (primitive          module "makeFunctionFrame" +void+ (list +afsp*+ +i32+ +afsp*+))
  (primitive          module "functionFrameReference" +tsp*+ (list +afsp*+ +i32+))

  (primitive          module "prependMultipleValues" +void+ (list +tsp*-or-tmv*+ +tmv*+))

  (primitive          module "invokeTopLevelFunction" +void+ (list +tmv*+ +fn-prototype*+ +i8*+ +i32*+ +size_t+ +size_t+ +size_t+ +ltv**+))
  (primitive          module "cc_register_startup_function" +void+ (list +fn-prototype*+))

  (primitive-nounwind module "activationFrameSize" +i32+ (list +afsp*+))

  (primitive-nounwind module "copyArgs" +void+ (list +tsp*+ +i32+ +t*+ +t*+ +t*+ +i8*+))
  (primitive          module "throwTooManyArgumentsException" +void+ (list +i8*+ +afsp*+ +i32+ +i32+))
  (primitive          module "throwNotEnoughArgumentsException" +void+ (list +i8*+ +afsp*+ +i32+ +i32+))
  (primitive          module "throwIfExcessKeywordArguments" +void+ (list +i8*+ +afsp*+ +i32+))
  (primitive-nounwind module "cc_trackFirstUnexpectedKeyword" +size_t+ (list +size_t+ +size_t+))
  (primitive          module "gdb" +void+ nil)
  (primitive-nounwind module "debugInvoke" +void+ nil)
  (primitive-nounwind module "debugInspectActivationFrame" +void+ (list +afsp*+))
  (primitive-nounwind module "debugInspectT_sp" +void+ (list +tsp*+))
  (primitive-nounwind module "debugInspectTPtr" +void+ (list +t*+))
  (primitive-nounwind module "debugInspectT_mv" +void+ (list +tmv*+))
  (primitive-nounwind module "debugInspect_return_type" +void+ (list +return_type+))
  (primitive-nounwind module "debugInspect_mvarray" +void+ nil)
  (primitive-nounwind module "debugPointer" +void+ (list +i8*+))
  (primitive-nounwind module "debug_VaList_SPtr" +void+ (list +VaList_S*+))
  (primitive-nounwind module "debugPrintObject" +void+ (list +i8*+ +tsp*+))
  (primitive-nounwind module "debugMessage" +void+ (list +i8*+))
  (primitive-nounwind module "debugPrintI32" +void+ (list +i32+))
  (primitive-nounwind module "debugPrint_size_t" +void+ (list +size_t+))
  (primitive-nounwind module "debug_match_two_uintptr_t" +uintptr_t+ (list +uintptr_t+ +uintptr_t+))
  (primitive-nounwind module "lowLevelTrace" +void+ (list +i32+))
  (primitive-nounwind module "unreachableError" +void+ nil)

  (primitive          module "va_tooManyArgumentsException" +void+ (list +i8*+ +size_t+ +size_t+))
  (primitive          module "va_notEnoughArgumentsException" +void+ (list +i8*+ +size_t+ +size_t+))
  (primitive          module "va_ifExcessKeywordArgumentsException" +void+ (list +i8*+ +size_t+ +VaList_S*+ +size_t+))
  (primitive-nounwind module "va_symbolFunction" +t*+ (list +tsp*+)) ;; void va_symbolFunction(core::Function_sp fn, core::Symbol_sp sym)
  (primitive-nounwind module "va_lexicalFunction" +t*+ (list +i32+ +i32+ +afsp*+))
  (primitive          module "FUNCALL" +return_type+ (list* +t*+ +t*+ +size_t+ (map 'list (lambda (x) x) (make-array core:+number-of-fixed-arguments+ :initial-element +t*+))) :varargs t)

  (primitive-nounwind module "cc_gatherRestArguments" +t*+ (list +size_t+ +VaList_S*+ +size_t+ +i8*+))
  (primitive-nounwind module "cc_gatherVaRestArguments" +t*+ (list +size_t+ +VaList_S*+ +size_t+ +VaList_S*+))
  (primitive          module "cc_ifBadKeywordArgumentException" +void+ (list +size_t+ +size_t+ +t*+))

  (primitive-nounwind module "pushCatchFrame" +size_t+ (list +tsp*+))
  (primitive-nounwind module "pushBlockFrame" +size_t+ (list +tsp*+))
  (primitive-nounwind module "pushTagbodyFrame" +size_t+ (list +tsp*+))

  (primitive          module "throwCatchThrow" +void+ (list +tsp*+ #| +tmv*+ |#) :does-not-return t)
  (primitive          module "throwReturnFrom" +void+ (list +tsp*+) :does-not-return t)
  (primitive          module "throwDynamicGo" +void+ (list +size_t+ +size_t+ +afsp*+) :does-not-return t)

  (primitive          module "ifCatchFrameMatchesStoreResultElseRethrow" +void+ (list +tsp*-or-tmv*+ +size_t+ +i8*+))
  (primitive-nounwind module "exceptionStackUnwind" +void+ (list +size_t+))
  (primitive          module "blockHandleReturnFrom" +void+ (list +tsp*-or-tmv*+ +i8*+ +size_t+))
  (primitive          module "tagbodyDynamicGoIndexElseRethrow" +size_t+ (list +i8*+ +size_t+))
  (primitive          module "throwIllegalSwitchValue" +void+ (list +size_t+ +size_t+) :does-not-return t)

  (primitive-nounwind module "clasp_terminate" +void+ (list +i8*+ +size_t+ +size_t+ +i8*+) )
  (primitive-nounwind module "__gxx_personality_v0" +i32+ nil :varargs t) ;; varargs
  (primitive-nounwind module "__cxa_begin_catch" +i8*+ (list +i8*+) )
  (primitive-nounwind module "__cxa_end_catch" +void+ nil) ;; This was a PRIMITIVE
  (primitive          module "__cxa_rethrow" +void+ nil)
  (primitive-nounwind module "llvm.eh.typeid.for" +i32+ (list +i8*+))

  (primitive-nounwind module "llvm.sadd.with.overflow.i32" +{i32.i1}+ (list +i32+ +i32+))
  (primitive-nounwind module "llvm.sadd.with.overflow.i64" +{i64.i1}+ (list +i64+ +i64+))
  (primitive-nounwind module "llvm.ssub.with.overflow.i32" +{i32.i1}+ (list +i32+ +i32+))
  (primitive-nounwind module "llvm.ssub.with.overflow.i64" +{i64.i1}+ (list +i64+ +i64+))

  (primitive-nounwind module "copyLoadTimeValue" +void+ (list +tsp*-or-tmv*+ +ltv**+ +size_t+))
  (primitive-nounwind module "loadTimeValueReference" +tsp*+ (list +tsp*+ +size_t+))
  (primitive-nounwind module "getLoadTimeValue" +void+ (list +tsp*-or-tmv*+ +ltv**+ +i32+))
  (primitive-nounwind module "dumpLoadTimeValues" +void+ (list +ltv**+))

  (primitive-nounwind module "debugSourceFileInfoHandle" +void+ (list +i32*+))

  (primitive-nounwind module "saveToMultipleValue0" +void+ (list +tmv*+))
  (primitive-nounwind module "restoreFromMultipleValue0" +void+ (list +tsp*-or-tmv*+ ))
  (primitive-nounwind module "saveValues" +void+ (list +tsp*+ +tmv*+))
  (primitive-nounwind module "loadValues" +void+ (list +tmv*+ +tsp*+))

  (primitive-nounwind module "setjmp_set_jump_address" +void+ (list +setjmp.buf*+ +i8*+) )


  (primitive-nounwind module "progvSaveSpecials" +void+ (list +i8**+ +tsp*+ +tsp*+))
  (primitive-nounwind module "progvRestoreSpecials" +void+ (list +i8**+))

  (primitive-nounwind module "pushDynamicBinding" +void+ (list +tsp*+))
  (primitive-nounwind module "popDynamicBinding" +void+ (list +tsp*+))

  (primitive-nounwind module "matchKeywordOnce" +size_t+ (list +tsp*+ +t*+ +i8*+))

  ;; Primitives for Cleavir code

  (primitive-nounwind module "cc_getPointer" +i8*+ (list +t*+))
  (primitive-nounwind module "cc_setTmvToNil" +void+ (list +tmv*+))
  (primitive-nounwind module "cc_precalcSymbol" +t*+ (list +ltv**+ +size_t+))
  (primitive-nounwind module "cc_precalcValue" +t*+ (list +ltv**+ +size_t+))
  (primitive-nounwind module "cc_makeCell" +t*+ nil)
  (primitive-nounwind module "cc_writeCell" +void+ (list +t*+ +t*+))
  (primitive-nounwind module "cc_readCell" +t*+ (list +t*+))
  (primitive-nounwind module "cc_t_reference" +t**+ nil)
  (primitive-nounwind module "cc_nil_reference" +t**+ nil)
  (primitive-nounwind module "cc_loadTimeValueReference" +t**+ (list +tsp*+ +size_t+))
  (primitive-nounwind module "cc_fetch" +t*+ (list +t*+ +size_t+))
  (primitive-nounwind module "cc_va_arg" +t*+ (list +VaList_S*+))
  (primitive-nounwind module "cc_copy_va_list" +void+ (list +size_t+ +t*[0]*+ +VaList_S*+))
  (primitive-nounwind module "cc_allocate_roots" +void+ (list +constants-table*+ +tsp*+ +size_t+ ))
  (primitive-nounwind module "cc_enclose" +t*+ (list +t*+ +fn-prototype*+ +i32*+ +size_t+ +size_t+ +size_t+ +size_t+ ) :varargs t)
  (primitive-nounwind module "cc_stack_enclose" +t*+ (list +i8*+ +t*+ +fn-prototype*+ +i32*+ +size_t+ +size_t+ +size_t+ +size_t+ ) :varargs t)
  (primitive-nounwind module "cc_saveThreadLocalMultipleValues" +void+ (list +tmv*+ +mv-struct*+))
  (primitive-nounwind module "cc_loadThreadLocalMultipleValues" +void+ (list +tmv*+ +mv-struct*+))
  (primitive          module "cc_safe_fdefinition" +t*+ (list +t*+))
  (primitive-nounwind module "cc_unsafe_fdefinition" +t*+ (list +t*+))
  (primitive          module "cc_safe_setfdefinition" +t*+ (list +t*+))
  (primitive-nounwind module "cc_unsafe_setfdefinition" +t*+ (list +t*+))
  (primitive          module "cc_safe_symbol_value" +t*+ (list +t*+))
  (primitive-nounwind module "cc_unsafe_symbol_value" +t*+ (list +t*+))
  (primitive-nounwind module "cc_setSymbolValue" +void+ (list +t*+ +t*+))
  (primitive          module "cc_call_multipleValueOneFormCall" +return_type+ (list +t*+))
  (primitive          module "cc_call"   +return_type+ (list* +t*+ +t*+ +size_t+
                                                              (map 'list (lambda (x) x)
                                                                   (make-array core:+number-of-fixed-arguments+ :initial-element +t*+))) :varargs t)
  (primitive-nounwind module "cc_allowOtherKeywords" +i64+ (list +i64+ +t*+))
  (primitive-nounwind module "cc_matchKeywordOnce" +size_t+ (list +t*+ +t*+ +t*+))
  (primitive          module "cc_ifNotKeywordException" +void+ (list +t*+ +size_t+ +VaList_S*+))
  (primitive-nounwind module "cc_multipleValuesArrayAddress" +t*[0]*+ nil)
  (primitive          module "cc_unwind" +void+ (list +t*+ +size_t+))
  (primitive          module "cc_throw" +void+ (list +t*+) :does-not-return t)
  (primitive-nounwind module "cc_saveMultipleValue0" +void+ (list +tmv*+))
  (primitive-nounwind module "cc_restoreMultipleValue0" +void+ (list +tmv*+))
  (primitive-nounwind module "cc_pushLandingPadFrame" +t*+ nil)
  (primitive-nounwind module "cc_popLandingPadFrame" +void+ (list +t*+))
  (primitive          module "cc_landingpadUnwindMatchFrameElseRethrow" +size_t+ (list +i8*+ +t*+))

  ;; === CLASP-FFI TRANSLATORS ===

  ;; !!! NOTE !!! => PORTING ISSUE/TODO !
  ;; This implementation assumes the following associations:
  ;;
  ;; char      -> i8
  ;; short     -> i16
  ;; int       -> i32
  ;; long      -> i64
  ;; long long -> i64 (!)
  ;; float     -> float
  ;; doubls    -> double

  ;; SHORT & UNSIGNED SHORT
  (primitive          module "tr_from_object_short" +t*+ (list +t*+))
  (primitive          module "tr_to_object_short" +t*+ (list +t*+))
  (primitive          module "tr_from_object_unsigned_short" +t*+ (list +t*+))
  (primitive          module "tr_to_object_unsigned_short" +t*+ (list +t*+))
  (primitive          module "from_object_short" +i16+ (list +t*+))
  (primitive          module "to_object_short" +t*+ (list +i16+))
  (primitive          module "from_object_unsigned_short" +i16+ (list +t*+))
  (primitive          module "to_object_unsigned_short" +t*+ (list +i16+))

  ;; INT & UNSIGNED INT
  (primitive          module "tr_from_object_int" +t*+ (list +t*+))
  (primitive          module "tr_to_object_int" +t*+ (list +t*+))
  (primitive          module "tr_from_object_unsigned_int" +t*+ (list +t*+))
  (primitive          module "tr_to_object_unsigned_int" +t*+ (list +t*+))
  (primitive          module "from_object_int" +i32+ (list +t*+))
  (primitive          module "to_object_int" +t*+ (list +i32+))
  (primitive          module "from_object_unsigned_int" +t*+ (list +t*+))
  (primitive          module "to_object_unsigned_int" +t*+ (list +i32+))

  ;; LONG & UNSIGNED LONG
  (primitive          module "tr_from_object_long" +t*+ (list +t*+))
  (primitive          module "tr_to_object_long" +t*+ (list +t*+))
  (primitive          module "tr_from_object_unsigned_long" +t*+ (list +t*+))
  (primitive          module "tr_to_object_unsigned_long" +t*+ (list +t*+))
  (primitive          module "from_object_long" +i64+ (list +t*+))
  (primitive          module "to_object_long" +t*+ (list +i64+))
  (primitive          module "from_object_unsigned_long" +i64+ (list +t*+))
  (primitive          module "to_object_unsigned_long" +t*+ (list +i64+))

  ;; LONG LONG & UNSIGNED LONG LONG
  (primitive          module "tr_from_object_long_long" +t*+ (list +t*+))
  (primitive          module "tr_to_object_long_long" +t*+ (list +t*+))
  (primitive          module "tr_from_object_unsigned_long_long" +t*+ (list +t*+))
  (primitive          module "tr_to_object_unsigned_long_long" +t*+ (list +t*+))
  (primitive          module "from_object_long_long" +i64+ (list +t*+))
  (primitive          module "to_object_long_long" +t*+ (list +i64+))
  (primitive          module "from_object_unsigned_long_long" +i64+ (list +t*+))
  (primitive          module "to_object_unsigned_long_long" +t*+ (list +i64+))

  ;; INT8 & UINT8
  (primitive          module "tr_from_object_int8" +t*+ (list +t*+))
  (primitive          module "tr_to_object_int8" +t*+ (list +t*+))
  (primitive          module "tr_from_object_uint8" +t*+ (list +t*+))
  (primitive          module "tr_to_object_uint8" +t*+ (list +t*+))
  (primitive          module "from_object_int8" +i8+ (list +t*+))
  (primitive          module "to_object_int8" +t*+ (list +i8+))
  (primitive          module "from_object_uint8" +i8+ (list +t*+))
  (primitive          module "to_object_uint8" +t*+ (list +i8+))

  ;; INT16 & UINT16
  (primitive          module "tr_from_object_int16" +t*+ (list +t*+))
  (primitive          module "tr_to_object_int16" +t*+ (list +t*+))
  (primitive          module "tr_from_object_uint16" +t*+ (list +t*+))
  (primitive          module "tr_to_object_uint16" +t*+ (list +t*+))
  (primitive          module "from_object_int16" +i16+ (list +t*+))
  (primitive          module "to_object_int16" +t*+ (list +i16+))
  (primitive          module "from_object_uint16" +i16+ (list +t*+))
  (primitive          module "to_object_uint16" +t*+ (list +i16+))

  ;; INT32 & UINT32
  (primitive          module "tr_from_object_int32" +t*+ (list +t*+))
  (primitive          module "tr_to_object_int32" +t*+ (list +t*+))
  (primitive          module "tr_from_object_uint32" +t*+ (list +t*+))
  (primitive          module "tr_to_object_uint32" +t*+ (list +t*+))
  (primitive          module "from_object_int32" +i32+ (list +t*+))
  (primitive          module "to_object_int32" +t*+ (list +i32+))
  (primitive          module "from_object_uint32" +i32+ (list +t*+))
  (primitive          module "to_object_uint32" +t*+ (list +i32+))

  ;; INT64 & UINT64
  (primitive          module "tr_from_object_int64" +t*+ (list +t*+))
  (primitive          module "tr_to_object_int64" +t*+ (list +t*+))
  (primitive          module "tr_from_object_uint64" +t*+ (list +t*+))
  (primitive          module "tr_to_object_uint64" +t*+ (list +t*+))
  (primitive          module "from_object_int64" +i64+ (list +t*+))
  (primitive          module "to_object_int64" +t*+ (list +i64+))
  (primitive          module "from_object_uint64" +i64+ (list +t*+))
  (primitive          module "to_object_uint64" +t*+ (list +i64+))

  ;; i128 HANDLING NOT IMPLEMENTED AS IT IS NOT USED

  ;; SIZE_T
  (primitive          module "tr_from_object_size" +t*+ (list +t*+))
  (primitive          module "tr_to_object_size" +t*+ (list +t*+))
  (primitive          module "from_object_size" +size_t+ (list +t*+))
  (primitive          module "to_object_size" +t*+ (list +size_t+))

  ;; SSIZE_T
  (primitive          module "tr_from_object_ssize" +t*+ (list +t*+))
  (primitive          module "tr_to_object_ssize" +t*+ (list +t*+))
  (primitive          module "from_object_ssize" +size_t+ (list +t*+))
  (primitive          module "to_object_ssize" +t*+ (list +size_t+))

  ;; PTRDIFF_T, TIME_T
  (primitive          module "tr_from_object_ptrdiff" +t*+ (list +t*+))
  (primitive          module "tr_to_object_ptrdiff" +t*+ (list +t*+))
  ;; (primitive          module "from_object_ptrdiff" +t*+ (list +t*+)) - FIXME !
  ;; (primitive          module "to_object_ptrdiff" +t*+ (list +uintptr_t+)) - FIXME !

  (primitive          module "tr_from_object_time" +t*+ (list +t*+))
  (primitive          module "tr_to_object_time" +t*+ (list +t*+))
  ;; (primitive          module "from_object_time" +t*+ (list +t*+)) - FIXME !
  ;; (primitive          module "to_object_time" +t*+ (list +t*+)) - FIOXME !

  ;; CHAR & UNSIGNED CHAR
  (primitive          module "tr_from_object_char" +t*+ (list +t*+))
  (primitive          module "tr_to_object_char" +t*+ (list +t*+))
  (primitive          module "tr_from_object_unsigned_char" +t*+ (list +t*+))
  (primitive          module "tr_to_object_unsigned_char" +t*+ (list +t*+))
  (primitive          module "from_object_char" +i8+ (list +t*+))
  (primitive          module "to_object_char" +t*+ (list +i8+))
  (primitive          module "from_object_unsigned_char" +i8+ (list +t*+))
  (primitive          module "to_object_unsigned_char" +t*+ (list +i8+))

  ;; FLOAT, DOUBLE & LONG FLOAT
  (primitive          module "tr_from_object_float" +t*+ (list +t*+))
  (primitive          module "tr_to_object_float" +t*+ (list +t*+))
  (primitive          module "from_object_float" +float+ (list +t*+))
  (primitive          module "to_object_float" +t*+ (list +float+))
  (primitive          module "tr_from_object_double" +t*+ (list +t*+))
  (primitive          module "tr_to_object_double" +t*+ (list +t*+))
  (primitive          module "from_object_double" +double+ (list +t*+))
  (primitive          module "to_object_double" +t*+ (list +double+))
  (primitive          module "tr_from_object_long_double" +t*+ (list +t*+))
  (primitive          module "tr_to_object_long_double" +t*+ (list +t*+))
  #+long-float (primitive module "from_object_long_double" +long-float+ (list +t*+))
  #+long-float (primitive module "to_object_long_double" +t*+ (list +long-float+))

  ;; POINTER / VOID *
  (primitive          module "tr_from_object_pointer" +t*+ (list +t*+))
  (primitive          module "tr_to_object_pointer" +t*+ (list +t*+))

  ;; THIS ERRORS OUT  ...
  (format *debug-io* "~%*** +VOID+ = ~S, +VOID*+ = ~S~%" +void+ +void*+)
  (primitive          module "from_object_pointer" +void*+ (list +t*+))
  (primitive          module "to_object_pointer" +t*+ (list +void*+))
  ;; === END OF TRANSLATORS ===

  )

;;------------------------------------------------------------
;;
;; Setup dynamic variables
;;
;;

(defvar *compile-file-pathname* nil "Store the path-name of the currently compiled file")
(defvar *compile-file-truename* nil "Store the truename of the currently compiled file")
(defvar *compile-file-source-file-info* nil "Store the SourceFileInfo object for the compile-file target")


(defvar *gv-source-namestring* nil
  "Store a global value that defines the filename of the current compilation")
(defvar *gv-source-debug-namestring* nil
  "A global value that defines the spoofed name of the current compilation - used by SLIME")
(defvar *source-debug-offset* 0)
(defvar *source-debug-use-lineno* t)
(defvar *gv-source-file-info-handle* nil
  "Store a global value that stores an integer handle assigned at load-time that uniquely
identifies the current source file.  Used for tracing and debugging")

(defvar *gv-boot-functions* nil
  "A global value that stores a pointer to the boot function for the Module.
It has appending linkage.")
(defvar *current-form* nil "The current form being compiled")
(defvar *current-env* nil "Current environment")
(defvar *current-function* nil "The current function")
(defvar *current-function-name* nil "Store the current function name")
(defvar *gv-current-function-name* nil "Store the global value in the module of the current function name ")


(defparameter *quick-module-index* 0)
(defun compile-file-quick-module-dump (module file-name-modifier)
  "Dump the module as a .ll file"
  (if *compile-file-debug-dump-module*
      (let* ((name-suffix (bformat nil "%05d-%s" (incf *quick-module-index*) file-name-modifier))
             (output-path (make-pathname
                           :name (concatenate
                                  'string
                                  (pathname-name *compile-file-output-pathname*)
                                  "-" name-suffix)
                           :type "ll"
                           :defaults *compile-file-output-pathname*)))
        (ensure-directories-exist output-path)
        (let* ((output-name (namestring output-path))
               (fout (open output-name :direction :output)))
          (unwind-protect
               (llvm-sys:dump-module module fout)
            (close fout))))))


(defun compile-quick-module-dump (module file-name-modifier)
  "Dump the module as a .ll file"
  (if *compile-debug-dump-module*
      (let* ((name-suffix (bformat nil "module-%05d-%s" (incf *quick-module-index*) file-name-modifier))
             (output-path (make-pathname
                           :name name-suffix
                           :directory '(:absolute "tmp")
                           :type "ll")))
        (ensure-directories-exist output-path)
        (let* ((output-name (namestring output-path))
               (fout (open output-name :direction :output)))
          (unwind-protect
               (llvm-sys:dump-module module fout)
            (close fout))))))


(defun quick-module-dump (module name-modifier)
  "If called under COMPILE-FILE the modules are dumped into the
same directory as the COMPILE-FILE output.  If called under COMPILE
they are dumped into /tmp"
  (if *compile-file-output-pathname*
      (compile-file-quick-module-dump module name-modifier)
      (compile-quick-module-dump module name-modifier)))


(defun compile-file-quick-message (file-name-modifier msg args)
  (let* ((name-suffix (bformat nil "%05d-%s" (incf *quick-module-index*) file-name-modifier))
         (output-path (make-pathname
                       :name (concatenate
                              'string
                              (pathname-name *compile-file-output-pathname*)
                              "-" name-suffix)
                       :type "txt"
                       :defaults *compile-file-output-pathname*)))
    (ensure-directories-exist output-path)
    (with-open-file (fout output-path :direction :output)
      (apply #'bformat fout msg args))))

(defun compile-quick-message (file-name-modifier msg args)
  (if *compile-debug-dump-module*
      (let* ((name-suffix (bformat nil "module-%05d-%s" (incf *quick-module-index*) file-name-modifier))
             (output-path (make-pathname
                           :name name-suffix
                           :directory '(:absolute "tmp")
                           :type "txt")))
        (ensure-directories-exist output-path)
        (let* ((output-name (namestring output-path))
               (fout (open output-name :direction :output)))
          (unwind-protect
               (llvm-sys:dump-module module fout)
            (close fout))))))


(defun quick-message (file-name-modifier msg &rest args)
  "Write a message into a file and write it to the same directory
as quick-module-dump would write it"
  (if *compile-file-debug-dump-module*
      (compile-file-quick-message file-name-modifier msg args)
      (compile-quick-message file-name-modifier msg args)))

(defmacro log-module ((info) &rest body)
  "Wrap quick-module-dump around a block of code so that the module
is dumped to a file before the block and after the block."
  `(progn
     (llvm-sys:sanity-check-module *the-module* 2)
     (quick-module-dump *the-module* ,(bformat nil "%s-begin" info))
     (multiple-value-prog1 (progn ,@body)
       (llvm-sys:sanity-check-module *the-module* 2)
       (quick-module-dump *the-module* ,(bformat nil "%s-end" info)))))
