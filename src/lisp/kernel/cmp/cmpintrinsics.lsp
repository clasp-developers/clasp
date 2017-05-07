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

(define-symbol-macro %i1% (llvm-sys:type-get-int1-ty *llvm-context*))

(define-symbol-macro %i8% (llvm-sys:type-get-int8-ty *llvm-context*)) ;; -> CHAR / BYTE
(define-symbol-macro %i8*% (llvm-sys:type-get-pointer-to %i8%))
(define-symbol-macro %i8**% (llvm-sys:type-get-pointer-to %i8*%))

(define-symbol-macro %i16% (llvm-sys:type-get-int16-ty *llvm-context*)) ;; -> SHORT
(define-symbol-macro %i16*% (llvm-sys:type-get-pointer-to %i16%))
(define-symbol-macro %i16**% (llvm-sys:type-get-pointer-to %i16*%))

(define-symbol-macro %i32% (llvm-sys:type-get-int32-ty *llvm-context*)) ;; -> INT
(define-symbol-macro %i32*% (llvm-sys:type-get-pointer-to %i32%))
(define-symbol-macro %i32**% (llvm-sys:type-get-pointer-to %i32*%))

(define-symbol-macro %i64% (llvm-sys:type-get-int64-ty *llvm-context*)) ;; -> LONG, LONG LONG
(define-symbol-macro %i64*% (llvm-sys:type-get-pointer-to %i64%))
(define-symbol-macro %i64**% (llvm-sys:type-get-pointer-to %i64*%))

(define-symbol-macro %i128% (llvm-sys:type-get-int128-ty *llvm-context*)) ;; -> NOT USED !!!

(define-symbol-macro %fixnum% (if (member :address-model-64 *features*) ;; -> FIXNUM
                     %i64%
                     (error "Add support for non 64-bit address model")))

(define-symbol-macro %float% (llvm-sys:type-get-float-ty *llvm-context*))
(define-symbol-macro %double% (llvm-sys:type-get-double-ty *llvm-context*))
#+long-float (define-symbol-macro %long-float% (llvm-sys:type-get-long-float-ty *llvm-context*))

(define-symbol-macro %size_t%
  (let ((sizeof-size_t (cdr (assoc 'core:size-t (llvm-sys:cxx-data-structures-info)))))
    (cond
      ((= 8 sizeof-size_t) %i64%)
      ((= 4 sizeof-size_t) %i32%)
      (t (error "Add support for size_t sizeof = ~a" sizeof-size_t)))))
(define-symbol-macro %size_t*% (llvm-sys:type-get-pointer-to %size_t%))
(define-symbol-macro %size_t**% (llvm-sys:type-get-pointer-to %size_t*%))

(define-symbol-macro %void% (llvm-sys:type-get-void-ty *llvm-context*))
(define-symbol-macro %void*% (llvm-sys:type-get-pointer-to %void%))

(define-symbol-macro %vtable*% %i8*%)

;;(define-symbol-macro %exception-struct% (llvm-sys:struct-type-get *llvm-context* (list %i8*% %i32%) "exception-struct" nil))
(define-symbol-macro %exception-struct% (llvm-sys:struct-type-get *llvm-context* (list %i8*% %i32%) nil))
(define-symbol-macro %{i32.i1}% (llvm-sys:struct-type-get *llvm-context* (list %i32% %i1%) nil))
(define-symbol-macro %{i64.i1}% (llvm-sys:struct-type-get *llvm-context* (list %i64% %i1%) nil))

;;  "A ctor void ()* function prototype"
(define-symbol-macro %fn-ctor%
  (llvm-sys:function-type-get %void% nil))
(defvar +fn-ctor-argument-names+ nil)
;;;  "A pointer to the ctor function prototype")
(define-symbol-macro %fn-ctor*% (llvm-sys:type-get-pointer-to %fn-ctor%))

;;;  "A run-all void ()* function prototype")
(define-symbol-macro %fn-start-up% (llvm-sys:function-type-get %void% nil))
(defvar +fn-start-up-argument-names+ nil)
;;;  "A pointer to the run-all function prototype")
(define-symbol-macro %fn-start-up*% (llvm-sys:type-get-pointer-to %fn-start-up%))


(define-symbol-macro %global-ctors-struct% (llvm-sys:struct-type-get *llvm-context* (list %i32% %fn-ctor*% %i8*%) nil))

;;;  "An array of pointers to the global-ctors-struct")
(define-symbol-macro %global-ctors-struct[1]% (llvm-sys:array-type-get %global-ctors-struct% 1))



(defvar +cxx-data-structures-info+ (llvm-sys:cxx-data-structures-info))

(defun get-cxx-data-structure-info (name &optional (info +cxx-data-structures-info+))
  (let ((find (assoc name info)))
    (or find (error "Could not find ~a in cxx-data-structures-info --> ~s~%" name info))
    (cdr find)))
(defvar +register-save-area-size+ (get-cxx-data-structure-info :register-save-area-size))
(defvar +void*-size+ (get-cxx-data-structure-info :void*-size))
(defvar +value-frame-parent-offset+ (get-cxx-data-structure-info :value-frame-parent-offset))
(defvar +fixnum-stamp+ (get-cxx-data-structure-info :fixnum-stamp))
(defvar +cons-stamp+ (get-cxx-data-structure-info :cons-stamp))
(defvar +valist_s-stamp+ (get-cxx-data-structure-info :va_list_s-stamp))
(defvar +character-stamp+ (get-cxx-data-structure-info :character-stamp))
(defvar +single-float-stamp+ (get-cxx-data-structure-info :single-float-stamp))
(defvar +instance-rack-stamp-offset+ (get-cxx-data-structure-info :instance-rack-stamp-offset))
(defvar +instance-rack-offset+ (get-cxx-data-structure-info :instance-rack-offset))
(defvar +instance-kind+ (get-cxx-data-structure-info :instance-kind))

(defvar +fixnum-mask+ (get-cxx-data-structure-info :fixnum-mask))
(defvar +fixnum-shift+ (get-cxx-data-structure-info :fixnum-shift))
(defvar +kind-shift+ (get-cxx-data-structure-info :kind-shift))
(defvar +tag-mask+ (get-cxx-data-structure-info :tag-mask))
(defvar +immediate-mask+ (get-cxx-data-structure-info :immediate-mask))
(defvar +cons-tag+ (get-cxx-data-structure-info :cons-tag))
(defvar +VaList_S-tag+ (get-cxx-data-structure-info :valist-tag))
(defvar +fixnum-tag+ (get-cxx-data-structure-info :fixnum-tag))
(defvar +fixnum1-tag+ (get-cxx-data-structure-info :fixnum1-tag))
(defvar +character-tag+ (get-cxx-data-structure-info :character-tag))
(defvar +single-float-tag+ (get-cxx-data-structure-info :single-float-tag))
(defvar +general-tag+ (get-cxx-data-structure-info :general-tag))
(defvar +VaList_S-size+ (get-cxx-data-structure-info :VaList_S-size))
(defvar +VaList_S-valist-offset+ (get-cxx-data-structure-info :VaList_S-valist-offset))
(defvar +void*-size+ (get-cxx-data-structure-info :void*-size))
(defvar +alignment+ (get-cxx-data-structure-info :alignment))
(export '(+fixnum-mask+ +tag-mask+ +immediate-mask+
          +cons-tag+ +fixnum-tag+ +character-tag+ +single-float-tag+
          +general-tag+ +VaList_S-size+ +void*-size+ +alignment+ ))
(defvar +cons-car-offset+ (get-cxx-data-structure-info :cons-car-offset))
(defvar +cons-cdr-offset+ (get-cxx-data-structure-info :cons-cdr-offset))
(defvar +uintptr_t-size+ (get-cxx-data-structure-info :uintptr_t-size))
(define-symbol-macro %intptr_t%
  (cond
    ((= 8 +uintptr_t-size+) %i64%)
    ((= 4 +uintptr_t-size+) %i32%)
    (t (error "Add support for size uintptr_t = ~a" +uintptr_t-size+))))
(define-symbol-macro %uintptr_t%
  (cond
    ((= 8 +uintptr_t-size+) %i64%)
    ((= 4 +uintptr_t-size+) %i32%)
    (t (error "Add support for size uintptr_t = ~a" +uintptr_t-size+))))
(define-symbol-macro %uintptr_t*% (llvm-sys:type-get-pointer-to %uintptr_t%))
(defun make-uintptr_t (x)
  (and (> x most-positive-fixnum) (error "make sure the integer ~s fits in a %i64%" x))
  (cond
    ((= 8 +uintptr_t-size+) (jit-constant-i64 x))
    ((= 4 +uintptr_t-size+) (jit-constant-i32 x))
    (t (error "Add support for size uintptr_t = ~a" +uintptr_t-size+))))

;;; DO NOT CHANGE THE FOLLOWING STRUCT!!! IT MUST MATCH VaList_S

(defun build-list-of-pointers (size type)
  (multiple-value-bind (num-pointers remainder)
      (floor size +void*-size+)
    (unless (= remainder 0)
      (error "The ~a size ~a is not a multiple of sizeof(void*) ~a"
             type size +void*-size+))
    (make-list num-pointers :initial-element %i8*%)))

(define-symbol-macro %sp-counted-base% (llvm-sys:struct-type-get *llvm-context* (list %i32% %i32%) nil)) ;; "sp-counted-base-ty"
(define-symbol-macro %sp-counted-base-ptr% (llvm-sys:type-get-pointer-to %sp-counted-base%))
(define-symbol-macro %shared-count% (llvm-sys:struct-type-get *llvm-context* (list %sp-counted-base-ptr%) nil)) ;; "shared_count"

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

(define-symbol-macro %setjmp.buf% (llvm-sys:struct-type-get *llvm-context* (list %i8*% %i8*% %i8*% %i8*% %i8*%) nil))
(define-symbol-macro %setjmp.buf*% (llvm-sys:type-get-pointer-to %setjmp.buf%))

;;
;; Setup smart-ptr constants
;;
(multiple-value-bind (pointer-type pointer-px-offset pointer-px-size)
    (smart-pointer-details)
  #+(or)(defvar +using-intrusive-reference-count+
          (eq pointer-type 'core::intrusive-reference-counted-pointer))
  (defvar +smart-ptr-px-offset+ pointer-px-offset))


(defun smart-pointer-fields (data-ptr-type &rest additional-fields)
  "List the types that make up a smart_ptr.
Boehm and MPS use a single pointer"
  (list* data-ptr-type additional-fields))


;;
;; If I use an opaque type then the symbol type gets duplicated and that causes
;; problems - try just using an int
;;(define-symbol-macro %sym% (llvm-sys:struct-type-get *llvm-context* nil nil)) ;; "Symbol_O"
(define-symbol-macro %sym% (llvm-sys:type-get-int32-ty *llvm-context*))
(define-symbol-macro %sym-ptr% (llvm-sys:type-get-pointer-to %sym%))
(define-symbol-macro %symsp% (llvm-sys:struct-type-get *llvm-context* (smart-pointer-fields %sym-ptr%) nil)) ;; "Sym_sp"
(define-symbol-macro %symsp*% (llvm-sys:type-get-pointer-to %symsp%))


;;
;; Store a core::Function_sp pointer
;;
(define-symbol-macro %Function% (llvm-sys:type-get-int32-ty *llvm-context*))
(define-symbol-macro %Function-ptr% (llvm-sys:type-get-pointer-to %Function%))
(define-symbol-macro %Function_sp% (llvm-sys:struct-type-get *llvm-context* (smart-pointer-fields %Function-ptr%) nil)) ;; "Cfn_sp"
(define-symbol-macro %Function_sp*% (llvm-sys:type-get-pointer-to %Function_sp%))



;; Define the T_O struct - right now just put in a dummy i32 - later put real fields here
(define-symbol-macro %t% (llvm-sys:struct-type-get *llvm-context* nil  nil)) ;; "T_O"
(define-symbol-macro %t*% (llvm-sys:type-get-pointer-to %t%))
(define-symbol-macro %t**% (llvm-sys:type-get-pointer-to %t*%))
(define-symbol-macro %t*[0]% (llvm-sys:array-type-get %t*% 0))
(define-symbol-macro %t*[0]*% (llvm-sys:type-get-pointer-to %t*[0]%))
(define-symbol-macro %tsp% (llvm-sys:struct-type-get *llvm-context* (smart-pointer-fields %t*%) nil))  ;; "T_sp"
(define-symbol-macro %tsp[0]% (llvm-sys:array-type-get %tsp% 0))
(define-symbol-macro %tsp[0]*% (llvm-sys:type-get-pointer-to %tsp[0]%))
;;;(define-symbol-macro %tsp[1]% (llvm-sys:array-type-get %tsp% 1))
;;;(define-symbol-macro %tsp[1]*% (llvm-sys:type-get-pointer-to %tsp[1]%))
;;;(define-symbol-macro %tsp[2]% (llvm-sys:array-type-get %tsp% 2))
;;;(define-symbol-macro %tsp[2]*% (llvm-sys:type-get-pointer-to %tsp[2]%))
(define-symbol-macro %tsp[DUMMY]% (llvm-sys:array-type-get %tsp% 64))
(define-symbol-macro %tsp[DUMMY]*% (llvm-sys:type-get-pointer-to %tsp[DUMMY]%))
(define-symbol-macro %tsp*% (llvm-sys:type-get-pointer-to %tsp%))
(define-symbol-macro %tsp**% (llvm-sys:type-get-pointer-to %tsp*%))

;; This structure must match the gctools::ConstantsTable structure
(define-symbol-macro %gcroots-in-module% (llvm-sys:struct-type-get *llvm-context* (list %i8*% %i8*% %size_t%) nil))
(define-symbol-macro %gcroots-in-module*% (llvm-sys:type-get-pointer-to %gcroots-in-module%))

;; The definition of %tmv% doesn't quite match T_mv because T_mv inherits from T_sp
(define-symbol-macro %tmv% (llvm-sys:struct-type-get *llvm-context* (smart-pointer-fields %t*% %size_t%) nil))  ;; "T_mv"
(define-symbol-macro %return_type% %tmv%)
(define-symbol-macro %tmv*% (llvm-sys:type-get-pointer-to %tmv%))
(define-symbol-macro %tmv**% (llvm-sys:type-get-pointer-to %tmv*%))

(define-symbol-macro %gcvector-tsp% (llvm-sys:struct-type-get *llvm-context* (list %size_t% %size_t% %tsp%) nil))
(define-symbol-macro %gcvector-symsp% (llvm-sys:struct-type-get *llvm-context*(list %size_t% %size_t% %symsp%) nil))
(define-symbol-macro %vec0-tsp% (llvm-sys:struct-type-get *llvm-context*(list %gcvector-tsp%) nil))
(define-symbol-macro %vec0-symsp% (llvm-sys:struct-type-get *llvm-context* (list %gcvector-symsp%) nil))

;; Define the LoadTimeValue_O struct - right now just put in a dummy i32 - later put real fields here
(define-symbol-macro %ltv% (llvm-sys:struct-type-get *llvm-context* (list %vtable*% #+(or) %vec0-tsp%)  nil)) ;; "LoadTimeValue_O"
(define-symbol-macro %ltv*% (llvm-sys:type-get-pointer-to %ltv%))
(define-symbol-macro %ltv**% (llvm-sys:type-get-pointer-to %ltv*%))
(define-symbol-macro %ltvsp% (llvm-sys:struct-type-get *llvm-context* (smart-pointer-fields %ltv*%) nil))  ;; "LoadTimeValue_sp"
#+(or)(defvar +ltvsp*+ (llvm-sys:type-get-pointer-to %ltvsp%))
#+(or)(defvar +ltvsp**+ (llvm-sys:type-get-pointer-to +ltvsp*+))


(define-symbol-macro %mv-limit% (cdr (assoc :multiple-values-limit (llvm-sys:cxx-data-structures-info))))
(define-symbol-macro %mv-values-array% (llvm-sys:array-type-get %t*% %mv-limit%))
(define-symbol-macro %mv-struct% (llvm-sys:struct-type-get *llvm-context* (list %size_t% %mv-values-array%) nil #|| is-packed ||#))
(define-symbol-macro %mv-struct*% (llvm-sys:type-get-pointer-to %mv-struct%))
(define-symbol-macro %thread-info-struct% (llvm-sys:struct-type-get *llvm-context* (list %mv-struct%) nil))



#+(or)(progn
        (defvar +af+ (llvm-sys:struct-type-get *llvm-context* nil  nil)) ;; "ActivationFrame_O"
        (defvar +af-ptr+ (llvm-sys:type-get-pointer-to +af+))
        (define-symbol-macro %afsp% (llvm-sys:struct-type-get *llvm-context* (smart-pointer-fields +af-ptr+)  nil)) ;; "ActivationFrame_sp"
        (define-symbol-macro %afsp*% (llvm-sys:type-get-pointer-to %afsp%))
        )

;; Substitute afsp* with tsp
(define-symbol-macro %afsp% %tsp%)
(define-symbol-macro %afsp*% %tsp*%)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Stuff needed for generic functions
;;;
;;;
(progn
  ;; Tack on a size_t to store the number of remaining arguments
  #+X86-64(define-symbol-macro %va_list% (llvm-sys:struct-type-get *llvm-context* (list %i32% %i32% %i8*% %i8*% %size_t%) nil))
  #-X86-64
  (error "I need a va_list struct definition for this system")

  (define-symbol-macro %va_list*% (llvm-sys:type-get-pointer-to %va_list%))
  (define-symbol-macro %VaList_S% (llvm-sys:struct-type-get *llvm-context* (list %va_list%) nil))
  (define-symbol-macro %VaList_S*% (llvm-sys:type-get-pointer-to %VaList_S%))

;;;    "Function prototype for generic functions")
  (define-symbol-macro %fn-gf% (llvm-sys:function-type-get %tmv% (list %t*% %t*%)))
  (define-symbol-macro %fn-gf-arguments% (list "gf" "args"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up the calling convention using core:+number-of-fixed-arguments+ to define the types
;; and names of the arguments passed in registers
;;
;; The last passed-arg is va-list
(defstruct (calling-convention-impl (:type vector))
  closure
  nargs
  register-args ;; the last passed-arg is a va-list
  valist
  remaining-nargs
  )

;; Parse the function arguments into a calling-convention
(defun parse-function-arguments (arguments)
  (let* ((closed-env            (first arguments))
         (va-list               (irc-alloca-va_list :label "va-list"))
         (reg-save-area         (irc-alloca-register-save-area :label "reg-save-area"))
         (remaining-nargs       (irc-alloca-size_t-no-cleanup :label "remnargs"))
         (_                     (spill-to-register-save-area arguments va-list reg-save-area)))
    #| TODO:  Add a InvocationStackEntry here |#
    (make-calling-convention-impl
     :nargs (second arguments) ;; The number of arguments
     :register-args (nthcdr 2 arguments)
     :valist va-list
     :remaining-nargs remaining-nargs)))

(defun calling-convention-closure (cc)
  (calling-convention-impl-closure cc))

(defun calling-convention-nargs (cc)
  (calling-convention-impl-nargs cc))

(defun calling-convention-register-args (cc)
  (calling-convention-impl-register-args cc))

;;; The remaining-nargs is the 4th element of the %va_list%
#+x86-64(defun calling-convention-remaining-nargs-addr (cc)
          (irc-gep (calling-convention-impl-valist cc) (list 0 4)))

(defun calling-convention-va-list (cc)
  (calling-convention-impl-valist cc))

(defun calling-convention-args.va-end (cc)
  (let* ((irc-intrinsic "llvm.va_end" (calling-convention-va-list cc)))))

;;;
;;; Read the next argument from the va_list
;;; Everytime the arg-idx is incremented, this function must be called.
;;; The arg-idx is not used but it is passed because I used to index into an array
(defun calling-convention-args.va-arg (cc arg-idx &optional target-idx)
  (declare (ignore arg-idx))
  (let* ((label (if (and target-idx core::*enable-print-pretty*)
                    (bformat nil "arg-%d" target-idx)
                    "rawarg"))
         (remaining-nargs-addr (calling-convention-remaining-nargs-addr cc))
         (remaining-nargs      (irc-load remaining-nargs-addr "rem-nargs"))
         (rem-nargs-1          (irc-sub  remaining-nargs (jit-constant-size_t 1) "rem-nargs-1"))
         (_                    (irc-store rem-nargs-1 remaining-nargs-addr))
         (result               (irc-va_arg (calling-convention-va-list cc))))
    result))


#+x86-64
(progn
;;; X86_64 calling convention The general function prototypes pass the following pass:
;;; 1) A closed over runtime environment a pointer to a closure.
;;; 2) A valist of remaining arguments
;;; 3) The number of arguments %size_t%
;;; 4) core::+number-of-fixed-arguments+ T_O* pointers,
;;;    the first arguments passed in registers,
;;; 5) The remaining arguments are on the stack
;;;       If no argument is passed then pass NULL.")

  (define-symbol-macro %register-arg-types% (list %t*% %t*% %t*% %t*%))
  (defvar *register-arg-names* (list "farg0" "farg1" "farg2" "farg3"))
  (defvar +fn-registers-prototype-argument-names+
    (list* "closure-ptr" "nargs" *register-arg-names*))
  (define-symbol-macro %fn-registers-prototype%
      (llvm-sys:function-type-get %tmv% (list* %t*% %size_t% %register-arg-types%) T #|VARARGS!|#))
  (define-symbol-macro %register-save-area% (llvm-sys:array-type-get
                                             %i8*%
                                             (/ +register-save-area-size+ +void*-size+)))
  (defun spill-to-register-save-area (registers va-list register-save-area)
    (bformat t "In spill-to-register-save-area arguments -> %s\n" registers)
    (labels ((spill-reg (idx)
               (let* ((addr-name     (bformat nil "addr%d" idx))
                      (addr          (irc-gep register-save-area (list (jit-constant-size_t 0) (jit-constant-size_t 0)) addr-name))
                      (reg-i8*       (irc-bit-cast (elt registers idx) %i8*% "reg-i8*"))
                      (_             (irc-store reg-i8* addr)))
                 addr)))
      (let* (
             (addr-closure  (spill-reg 0))
             (addr-nargs    (spill-reg 1))
             (addr-farg0    (spill-reg 2)) ; this is the first fixed arg currently.
             (addr-farg1    (spill-reg 3))
             (addr-farg2    (spill-reg 4))
             (addr-farg3    (spill-reg 5))))))

  (defun calling-convention-args.va-start (cc)
    (let* ((irc-intrinsic "llvm.va_start" (calling-convention-va-list cc))
           (nargs-addr                    (irc-gep (calling-convention-register-save-area cc) (list (jit-constant-size_t 0) (jit-constant-size_t 1)) "nargs-addr"))
           (nargs-i8*                     (irc-load nargs-addr))
           (nargs                         (irc-bit-cast nargs-i8* %size_t*))
           (_                             (irc-store nargs (calling-convention-remaining-args cc)))
           ;; save the register-save-area ptr
           (_                             (irc-insert-value va-list (calling-convention-register-save-area cc) (list 3)))
           ;; set the gp_offset to point to first reg arg
           (_                             (irc-insert-value va-list (jit-constant-i32 (* 8 2)) (list 0)))))))


#-(and x86-64)
(error "Define calling convention for system")

(define-symbol-macro %fn-prototype% %fn-registers-prototype%)
(defvar +fn-prototype-argument-names+ +fn-registers-prototype-argument-names+)

;;;  "A pointer to the function prototype"
(define-symbol-macro %fn-prototype*% (llvm-sys:type-get-pointer-to %fn-prototype%))
;;;  "A pointer to a pointer to the function prototype"
(define-symbol-macro %fn-prototype**% (llvm-sys:type-get-pointer-to %fn-prototype*%))
;;;  "An array of pointers to the function prototype"
(define-symbol-macro %fn-prototype*[0]% (llvm-sys:array-type-get %fn-prototype*% 0))
;;;  "An array of pointers to the function prototype"
(define-symbol-macro %fn-prototype*[1]% (llvm-sys:array-type-get %fn-prototype*% 1))
;;;  "An array of pointers to the function prototype"
(define-symbol-macro %fn-prototype*[2]% (llvm-sys:array-type-get %fn-prototype*% 2))

;;
;; Define the InvocationHistoryFrame type for LispCompiledFunctionIHF
;;
;; %"class.core::InvocationHistoryFrame" = type { i32 (...)**, i32, %"class.core::InvocationHistoryStack"*, %"class.core::InvocationHistoryFrame"*, i8, i32 }
;;"Make this a generic pointer"
(define-symbol-macro %InvocationHistoryStack*% %i8*%)
(define-symbol-macro %InvocationHistoryFrame% (llvm-sys:struct-type-create *llvm-context* :elements (list %i32**% %i32% #|%InvocationHistoryStack*% %InvocationHistoryFrame*%|# %i8*% %i8*% %i8% %i32%) :name "InvocationHistoryFrame"))
(define-symbol-macro %InvocationHistoryFrame*% (llvm-sys:type-get-pointer-to %InvocationHistoryFrame%))
;;  (llvm-sys:set-body %InvocationHistoryFrame% (list %i32**% %i32% #|%InvocationHistoryStack*% %InvocationHistoryFrame*%|# %i8*% %i8*% %i8% %i32%) nil)
(define-symbol-macro %LispFunctionIHF% (llvm-sys:struct-type-create *llvm-context* :elements (list %InvocationHistoryFrame% %tsp% %tsp% %tsp% %i32% %i32%) :name "LispFunctionIHF"))
;; %"class.core::LispCompiledFunctionIHF" = type { %"class.core::LispFunctionIHF" }
(define-symbol-macro %LispCompiledFunctionIHF% (llvm-sys:struct-type-create *llvm-context* :elements (list %LispFunctionIHF%) :name "LispCompiledFunctionIHF"))


  (defun make-gv-source-file-info-handle (module &optional handle)
    (if (null handle) (setq handle -1))
    (llvm-sys:make-global-variable module
                                   %i32%  ; type
                                   nil    ; constant
                                   'llvm-sys:internal-linkage
                                   (jit-constant-i32 handle)
                                   "source-file-info-handle"))


  (defun add-global-ctor-function (module main-function)
    "Create a function with the name core:+clasp-ctor-function-name+ and
have it call the main-function"
    (let* ((*the-module* module)
           (fn (irc-simple-function-create
                core::+clasp-ctor-function-name+
                %fn-ctor%
                'llvm-sys:internal-linkage
                *the-module*
                :argument-names +fn-ctor-argument-names+))
           #++(with-new-function
                  (ctor-func func-env result
                             :function-name core:+clasp-ctor-function-name+
                             :parent-env nil
                             :linkage 'llvm-sys:internal-linkage
                             :function-type %fn-ctor%
                             :return-void t
                             :argument-names +fn-ctor-argument-names+ )
                (let* ((bc-bf (irc-bit-cast main-function %fn-start-up*% "fnptr-pointer")))
                  (irc-intrinsic "cc_register_startup_function" bc-bf))))
      (let* ((irbuilder-body (llvm-sys:make-irbuilder *llvm-context*))
             (*current-function* fn)
             (entry-bb (irc-basic-block-create "entry" fn)))
        (llvm-sys:set-insert-point-basic-block irbuilder-body entry-bb)
        (with-irbuilder (irbuilder-body)
        (let* ((bc-bf (irc-bit-cast main-function %fn-start-up*% "fnptr-pointer"))
               (_     (irc-intrinsic "cc_register_startup_function" bc-bf))
               (_     (irc-ret-void))))
          (llvm-sys:dump fn)
          fn))))

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
   %global-ctors-struct[1]%
   nil
   'llvm-sys:appending-linkage
   (llvm-sys:constant-array-get
    %global-ctors-struct[1]%
    (list
     (llvm-sys:constant-struct-get %global-ctors-struct%
                                   (list
                                    (jit-constant-i32 priority)
                                    global-ctor-function
                                    (llvm-sys:constant-pointer-null-get %i8*%)))))
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
         (tsp-size (llvm-sys:data-layout-get-type-alloc-size data-layout %tsp%))
         (tmv-size (llvm-sys:data-layout-get-type-alloc-size data-layout %tmv%))
         (valist_s-size (llvm-sys:data-layout-get-type-alloc-size data-layout %VaList_S%))
         (gcroots-in-module-size (llvm-sys:data-layout-get-type-alloc-size data-layout %gcroots-in-module%)))
    (llvm-sys:throw-if-mismatched-structure-sizes :tsp tsp-size :tmv tmv-size :contab gcroots-in-module-size
                                                   :valist valist_s-size)))

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
	 (i8* (llvm-sys:get-or-create-external-global *the-module* cname %i8%)))
    i8*))

;;
;; Define functions within the module
;;

(defvar *primitives* (make-hash-table :test 'equal))


(defun matching-arguments (required-type given-type arg-index)
  (if (equal required-type +tsp*-or-tmv*+)
      (if (eql arg-index 1)
	  (if (or (equal given-type %tsp*%) (equal given-type %tmv*%))
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
	   ((equal required-first-argument-type %tsp*%)
	    "sp_")
	   ((equal required-first-argument-type %tmv*%)
	    "mv_")
	   (t
	    ""))))
    (bformat nil "%s%s" name-dispatch-prefix name)))



(defun codegen-startup-shutdown (gcroots-in-module roots-array number-of-roots)
  (let ((startup-fn (irc-simple-function-create core:*module-startup-function-name*
                                                (llvm-sys:function-type-get %void% (list %t*%))
                                                'llvm-sys::External-linkage
                                                *the-module*
                                                :argument-names (list "values" ))))
    (let* ((irbuilder-alloca (llvm-sys:make-irbuilder *llvm-context*))
           (irbuilder-body (llvm-sys:make-irbuilder *llvm-context*))
           (*irbuilder-function-alloca* irbuilder-alloca)
           (*irbuilder-function-body* irbuilder-body)
           (*current-function* startup-fn)
           (entry-bb (irc-basic-block-create "entry" startup-fn))
           (arguments (llvm-sys:get-argument-list startup-fn))
           (values (first arguments))
           (size (second arguments))
           (gf-args (second arguments)))
      (llvm-sys:set-insert-point-basic-block irbuilder-alloca entry-bb)
      (with-irbuilder (irbuilder-alloca)
        (let ((start (irc-gep roots-array
                              (list (jit-constant-size_t 0)
                                    (jit-constant-size_t 0)))))
          (irc-create-call "cc_initialize_gcroots_in_module" (list gcroots-in-module start (jit-constant-size_t number-of-roots) values))
          (irc-ret-void))))
    (let ((shutdown-fn (irc-simple-function-create core:*module-shutdown-function-name*
                                                   (llvm-sys:function-type-get %void% nil)
                                                   'llvm-sys::External-linkage
                                                   *the-module*
                                                   :argument-names nil)))
      (let* ((irbuilder-alloca (llvm-sys:make-irbuilder *llvm-context*))
             (irbuilder-body (llvm-sys:make-irbuilder *llvm-context*))
             (*irbuilder-function-alloca* irbuilder-alloca)
             (*irbuilder-function-body* irbuilder-body)
             (*current-function* shutdown-fn)
             (entry-bb (irc-basic-block-create "entry" shutdown-fn))
             (arguments (llvm-sys:get-argument-list shutdown-fn))
             (values (first arguments))
             (size (second arguments))
             (gf-args (second arguments)))
        (llvm-sys:set-insert-point-basic-block irbuilder-alloca entry-bb)
        (with-irbuilder (irbuilder-alloca)
          (let ((start (irc-gep roots-array
                                (list (jit-constant-size_t 0)
                                      (jit-constant-size_t 0)))))
            (irc-create-call "cc_shutdown_gcroots_in_module" (list gcroots-in-module))
            (irc-ret-void))))
      (values startup-fn shutdown-fn))))






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
				   (dispatch-function-name name %tsp*%)
				   return-ty
				   (cons %tsp*% (cdr args-ty))
				   varargs does-not-throw does-not-return)
	(create-primitive-function module
				   (dispatch-function-name name %tmv*%)
				   return-ty
				   (cons %tmv*% (cdr args-ty))
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

  (primitive-nounwind module "ltvc_assign_source_file_info_handle" %void% (list %i8*% %i8*% %size_t% %i32% %i32*%))
  (primitive-nounwind module "ltvc_make_nil" %t*% (list %gcroots-in-module*% %size_t%))
  (primitive-nounwind module "ltvc_make_t" %t*% (list %gcroots-in-module*% %size_t%))
  (primitive-nounwind module "ltvc_make_ratio" %t*% (list %gcroots-in-module*% %size_t% %t*% %t*%))
  (primitive-nounwind module "ltvc_make_cons" %t*% (list %gcroots-in-module*% %size_t% %t*% %t*%))
  (primitive-nounwind module "ltvc_make_list" %t*% (list %gcroots-in-module*% %size_t% %size_t%) :varargs t)
  (primitive-nounwind module "ltvc_make_array" %t*% (list %gcroots-in-module*% %size_t% %t*% %t*%))
  (primitive-nounwind module "ltvc_setf_row_major_aref" %t*% (list %t*% %size_t% %t*%))
  (primitive-nounwind module "ltvc_make_hash_table" %t*% (list %gcroots-in-module*% %size_t% %t*%))
  (primitive-nounwind module "ltvc_setf_gethash" %t*% (list %t*% %t*% %t*%))
  (primitive-nounwind module "ltvc_make_fixnum" %t*% (list %gcroots-in-module*% %size_t% %uintptr_t%))
  (primitive-nounwind module "ltvc_make_package" %t*% (list %gcroots-in-module*% %size_t% %t*%))
  (primitive-nounwind module "ltvc_make_bignum" %t*% (list %gcroots-in-module*% %size_t% %t*%))
  (primitive-nounwind module "ltvc_make_bitvector" %t*% (list %gcroots-in-module*% %size_t% %t*%))
  (primitive-nounwind module "ltvc_make_random_state" %t*% (list %gcroots-in-module*% %size_t% %t*%))
  (primitive-nounwind module "ltvc_make_symbol" %t*% (list %gcroots-in-module*% %size_t% %t*% %t*%))
  (primitive-nounwind module "ltvc_make_character" %t*% (list %gcroots-in-module*% %size_t% %uintptr_t%))
  (primitive-nounwind module "ltvc_make_base_string" %t*% (list %gcroots-in-module*% %size_t% %i8*%))
  (primitive-nounwind module "ltvc_make_pathname" %t*% (list %gcroots-in-module*% %size_t% %t*% %t*% %t*% %t*% %t*% %t*%))
  (primitive-nounwind module "ltvc_make_package" %t*% (list %gcroots-in-module*% %size_t% %t*%))
  (primitive-nounwind module "ltvc_make_built_in_class" %t*% (list %gcroots-in-module*% %size_t% %t*%))
  (primitive-nounwind module "ltvc_make_float" %t*% (list %gcroots-in-module*% %size_t% %float%))
  (primitive-nounwind module "ltvc_make_double" %t*% (list %gcroots-in-module*% %size_t% %double%))
  (primitive-nounwind module "ltvc_make_complex" %t*% (list %gcroots-in-module*% %size_t% %t*% %t*%))
  (primitive          module "ltvc_set_mlf_creator_funcall" %t*% (list %gcroots-in-module*% %size_t% %fn-prototype*%))
  (primitive          module "ltvc_mlf_init_funcall" %t*% (list %fn-prototype*%))
  (primitive          module "ltvc_set_ltv_funcall" %t*% (list %gcroots-in-module*% %size_t% %fn-prototype*%))
  (primitive          module "ltvc_toplevel_funcall" %t*% (list %fn-prototype*%))

  (primitive-nounwind module "newFunction_sp" %void% (list %Function_sp*%))
  (primitive-nounwind module "newTsp" %void% (list %tsp*%))
  (primitive-nounwind module "copyTsp" %void% (list +tsp*-or-tmv*+ %tsp*%))
  (primitive-nounwind module "copyTspTptr" %void% (list +tsp*-or-tmv*+ %t*%))
  (primitive-nounwind module "compareTspTptr" %i32% (list %tsp*% %t*%))

  (primitive-nounwind module "newTmv" %void% (list %tmv*%))
  (primitive-nounwind module "resetTmv" %void% (list %tmv*%))
  (primitive-nounwind module "copyTmv" %void% (list %tmv*% %tmv*%))
  (primitive-nounwind module "copyTmvOrSlice" %void% (list +tsp*-or-tmv*+ %tmv*%))

  (primitive-nounwind module "isTrue" %i32% (list %tsp*%))
  (primitive-nounwind module "isBound" %i32% (list %tsp*%))

  (primitive-nounwind module "internSymbol_tsp" %void% (list %tsp*% %i8*% %i8*%))
  (primitive-nounwind module "makeSymbol_tsp" %void% (list %tsp*% %i8*%))

  (primitive-nounwind module "internSymbol_symsp" %void% (list %symsp*% %i8*% %i8*%))
  (primitive-nounwind module "makeSymbol_symsp" %void% (list %symsp*% %i8*%))

  (primitive-nounwind module "makeNil" %void% (list +tsp*-or-tmv*+))
  (primitive-nounwind module "makeT" %void% (list %tsp*%))
  (primitive-nounwind module "makeCons" %void% (list %tsp*% %tsp*% %tsp*%))
  (primitive-nounwind module "makeFixnum" %void% (list %tsp*% %fixnum%))
  (primitive-nounwind module "makeCharacter" %void% (list %tsp*% %i32%))
  (primitive-nounwind module "makeBignum" %void% (list %tsp*% %i8*%))
  #+short-float (primitive-nounwind module "makeShortFloat" %void% (list %tsp*% %double%))
  (primitive-nounwind module "makeSingleFloat" %void% (list %tsp*% %float%))
  (primitive-nounwind module "makeDoubleFloat" %void% (list %tsp*% %double%))

  #+long-float (primitive-nounwind module "makeLongFloat" %void% (list %tsp*% %long-float%))
  (primitive-nounwind module "makeString" %void% (list %tsp*% %i8*%))
  (primitive-nounwind module "makePathname" %void% (list %tsp*% %i8*%))
  (primitive-nounwind module "makeCompiledFunction" %void% (list +tsp*-or-tmv*+ %fn-prototype*% %i32*% %size_t% %size_t% %size_t% %tsp*% %tsp*% %afsp*% %tsp*%))

  (primitive          module "symbolValueRead" %void% (list +tsp*-or-tmv*+ %tsp*%))
  (primitive-nounwind module "symbolValueReference" %tsp*% (list %tsp*%))
  (primitive-nounwind module "lexicalValueReference" %tsp*% (list %i32% %i32% %afsp*%))
  (primitive-nounwind module "lexicalValueRead" %void% (list +tsp*-or-tmv*+ %i32% %i32% %afsp*%))
  (primitive-nounwind module "symbolFunctionRead" %void% (list +tsp*-or-tmv*+ %tsp*%))
  (primitive-nounwind module "setfSymbolFunctionRead" %void% (list %tsp*% %tsp*%))
  (primitive-nounwind module "lexicalFunctionRead" %void% (list +tsp*-or-tmv*+ %i32% %i32% %afsp*%))


  (primitive-nounwind module "makeTagbodyFrame" %void% (list %afsp*%))
  (primitive-nounwind module "makeValueFrame" %void% (list %tsp*% %i64%))
  (primitive-nounwind module "setParentOfActivationFrameFromClosure" %void% (list %tsp*% %t*%))
  (primitive-nounwind module "setParentOfActivationFrame" %void% (list %tsp*% %tsp*%))

;;  (primitive-nounwind module "attachDebuggingInfoToValueFrame" %void% (list %afsp*% %tsp*%))

  (primitive-nounwind module "valueFrameReference" %tsp*% (list %afsp*% %i32%))

  (primitive          module "makeFunctionFrame" %void% (list %afsp*% %i32% %afsp*%))
  (primitive          module "functionFrameReference" %tsp*% (list %afsp*% %i32%))

  (primitive          module "prependMultipleValues" %void% (list +tsp*-or-tmv*+ %tmv*%))

  (primitive          module "invokeTopLevelFunction" %void% (list %tmv*% %fn-prototype*% %i8*% %i32*% %size_t% %size_t% %size_t% %ltv**%))
  (primitive          module "cc_register_startup_function" %void% (list %fn-start-up*%))

  (primitive-nounwind module "activationFrameSize" %i32% (list %afsp*%))

  (primitive-nounwind module "copyArgs" %void% (list %tsp*% %i32% %t*% %t*% %t*% %i8*%))
  (primitive          module "throwTooManyArgumentsException" %void% (list %i8*% %afsp*% %i32% %i32%))
  (primitive          module "throwNotEnoughArgumentsException" %void% (list %i8*% %afsp*% %i32% %i32%))
  (primitive          module "throwIfExcessKeywordArguments" %void% (list %i8*% %afsp*% %i32%))
  (primitive-nounwind module "cc_trackFirstUnexpectedKeyword" %size_t% (list %size_t% %size_t%))
  (primitive          module "gdb" %void% nil)
  (primitive-nounwind module "debugInvoke" %void% nil)
  (primitive-nounwind module "debugInspectActivationFrame" %void% (list %afsp*%))
  (primitive-nounwind module "debugInspectT_sp" %void% (list %tsp*%))
  (primitive-nounwind module "debugInspectTPtr" %void% (list %t*%))
  (primitive-nounwind module "debugInspectT_mv" %void% (list %tmv*%))
  (primitive-nounwind module "debugInspect_return_type" %void% (list %return_type%))
  (primitive-nounwind module "debugInspect_mvarray" %void% nil)
  (primitive-nounwind module "debugPointer" %void% (list %i8*%))
  (primitive-nounwind module "debug_VaList_SPtr" %void% (list %VaList_S*%))
  (primitive-nounwind module "debugPrintObject" %void% (list %i8*% %tsp*%))
  (primitive-nounwind module "debugMessage" %void% (list %i8*%))
  (primitive-nounwind module "debugPrintI32" %void% (list %i32%))
  (primitive-nounwind module "debugPrint_size_t" %void% (list %size_t%))
  (primitive-nounwind module "debug_match_two_uintptr_t" %uintptr_t% (list %uintptr_t% %uintptr_t%))
  (primitive-nounwind module "lowLevelTrace" %void% (list %i32%))
  (primitive-nounwind module "unreachableError" %void% nil)

  (primitive          module "va_tooManyArgumentsException" %void% (list %i8*% %size_t% %size_t%))
  (primitive          module "va_notEnoughArgumentsException" %void% (list %i8*% %size_t% %size_t%))
  (primitive          module "va_ifExcessKeywordArgumentsException" %void% (list %i8*% %size_t% %VaList_S*% %size_t%))
  (primitive-nounwind module "va_symbolFunction" %t*% (list %tsp*%)) ;; void va_symbolFunction(core::Function_sp fn, core::Symbol_sp sym)
  (primitive-nounwind module "va_lexicalFunction" %t*% (list %i32% %i32% %afsp*%))
  (primitive          module "FUNCALL" %return_type% (list* %t*% %t*% %size_t% (map 'list (lambda (x) x) (make-array core:+number-of-fixed-arguments+ :initial-element %t*%))) :varargs t)

  (primitive-nounwind module "cc_gatherRestArguments" %t*% (list %size_t% %VaList_S*% %size_t% %i8*%))
  (primitive-nounwind module "cc_gatherVaRestArguments" %t*% (list %size_t% %VaList_S*% %size_t% %VaList_S*%))
  (primitive          module "cc_ifBadKeywordArgumentException" %void% (list %size_t% %size_t% %t*%))

  (primitive-nounwind module "pushCatchFrame" %size_t% (list %tsp*%))
  (primitive-nounwind module "pushBlockFrame" %size_t% (list %tsp*%))
  (primitive-nounwind module "pushTagbodyFrame" %size_t% (list %tsp*%))

  (primitive          module "throwCatchThrow" %void% (list %tsp*% #| %tmv*% |#) :does-not-return t)
  (primitive          module "throwReturnFrom" %void% (list %tsp*%) :does-not-return t)
  (primitive          module "throwDynamicGo" %void% (list %size_t% %size_t% %afsp*%) :does-not-return t)

  (primitive          module "ifCatchFrameMatchesStoreResultElseRethrow" %void% (list +tsp*-or-tmv*+ %size_t% %i8*%))
  (primitive-nounwind module "exceptionStackUnwind" %void% (list %size_t%))
  (primitive          module "blockHandleReturnFrom" %void% (list +tsp*-or-tmv*+ %i8*% %size_t%))
  (primitive          module "tagbodyDynamicGoIndexElseRethrow" %size_t% (list %i8*% %size_t%))
  (primitive          module "throwIllegalSwitchValue" %void% (list %size_t% %size_t%) :does-not-return t)

  (primitive-nounwind module "clasp_terminate" %void% (list %i8*% %size_t% %size_t% %i8*%) )
  (primitive-nounwind module "__gxx_personality_v0" %i32% nil :varargs t) ;; varargs
  (primitive-nounwind module "__cxa_begin_catch" %i8*% (list %i8*%) )
  (primitive-nounwind module "__cxa_end_catch" %void% nil) ;; This was a PRIMITIVE
  (primitive          module "__cxa_rethrow" %void% nil)
  (primitive-nounwind module "llvm.eh.typeid.for" %i32% (list %i8*%))

  (primitive-nounwind module "llvm.sadd.with.overflow.i32" %{i32.i1}% (list %i32% %i32%))
  (primitive-nounwind module "llvm.sadd.with.overflow.i64" %{i64.i1}% (list %i64% %i64%))
  (primitive-nounwind module "llvm.ssub.with.overflow.i32" %{i32.i1}% (list %i32% %i32%))
  (primitive-nounwind module "llvm.ssub.with.overflow.i64" %{i64.i1}% (list %i64% %i64%))
  
  (primitive-nounwind module "llvm.va_copy" %void% (list %i8*% %i8*%))
  (primitive-nounwind module "llvm.va_start" %void% (list %i8*%))
  (primitive-nounwind module "llvm.va_end" %void% (list %i8*%))

  (primitive-nounwind module "copyLoadTimeValue" %void% (list +tsp*-or-tmv*+ %ltv**% %size_t%))
  (primitive-nounwind module "getLoadTimeValue" %void% (list +tsp*-or-tmv*+ %ltv**% %i32%))
  (primitive-nounwind module "dumpLoadTimeValues" %void% (list %ltv**%))

  (primitive-nounwind module "debugSourceFileInfoHandle" %void% (list %i32*%))

  (primitive-nounwind module "saveToMultipleValue0" %void% (list %tmv*%))
  (primitive-nounwind module "restoreFromMultipleValue0" %void% (list +tsp*-or-tmv*+ ))
  (primitive-nounwind module "saveValues" %void% (list %tsp*% %tmv*%))
  (primitive-nounwind module "loadValues" %void% (list %tmv*% %tsp*%))

  (primitive-nounwind module "setjmp_set_jump_address" %void% (list %setjmp.buf*% %i8*%) )


  (primitive-nounwind module "progvSaveSpecials" %void% (list %i8**% %tsp*% %tsp*%))
  (primitive-nounwind module "progvRestoreSpecials" %void% (list %i8**%))

  (primitive-nounwind module "pushDynamicBinding" %void% (list %tsp*%))
  (primitive-nounwind module "popDynamicBinding" %void% (list %tsp*%))

  (primitive-nounwind module "matchKeywordOnce" %size_t% (list %tsp*% %t*% %i8*%))

  ;; Primitives for Cleavir code

  (primitive-nounwind module "cc_eql" %i32% (list %t*% %t*%)) ;; eql test
  (primitive          module "cc_bad_tag" %void% (list %t*% %t*%)) ;; gf gf-args
  (primitive          module "cc_dispatch_invalid" %return_type% (list %t*% %t*%)) ;; gf gf-args
  (primitive          module "cc_dispatch_miss" %return_type% (list %t*% %t*%)) ;; gf gf-args
  (primitive          module "cc_dispatch_slot_reader"   %return_type% (list %t*% %t*% %t*%)) ; effective-method gf gf-args
  (primitive          module "cc_dispatch_slot_writer"   %return_type% (list %t*% %t*% %t*%)) ; effective-method gf gf-args
  (primitive          module "cc_dispatch_effective_method"   %return_type% (list %t*% %t*% %t*%)) ; effective-method gf gf-args
  (primitive          module "cc_dispatch_debug" %void% (list %i32% %uintptr_t%))
  
  (primitive-nounwind module "cc_getPointer" %i8*% (list %t*%))
  (primitive-nounwind module "cc_setTmvToNil" %void% (list %tmv*%))
  (primitive-nounwind module "cc_precalcSymbol" %t*% (list %ltv**% %size_t%))
  (primitive-nounwind module "cc_precalcValue" %t*% (list %ltv**% %size_t%))
  (primitive-nounwind module "cc_makeCell" %t*% nil)
  (primitive-nounwind module "cc_writeCell" %void% (list %t*% %t*%))
  (primitive-nounwind module "cc_readCell" %t*% (list %t*%))
  (primitive-nounwind module "cc_t_reference" %t**% nil)
  (primitive-nounwind module "cc_nil_reference" %t**% nil)
  (primitive-nounwind module "cc_fetch" %t*% (list %t*% %size_t%))
  (primitive-nounwind module "cc_va_arg" %t*% (list %VaList_S*%))
  (primitive-nounwind module "cc_va_list_length" %size_t% (list %VaList_S*%))
  (primitive-nounwind module "cc_copy_va_list" %void% (list %size_t% %t*[0]*% %VaList_S*%))
  
  (primitive-nounwind module "cc_initialize_gcroots_in_module" %void% (list %gcroots-in-module*% %tsp*% %size_t% %t*%))
  (primitive-nounwind module "cc_shutdown_gcroots_in_module" %void% (list %gcroots-in-module*% ))
  
  (primitive-nounwind module "cc_enclose" %t*% (list %t*% %fn-prototype*% %i32*% %size_t% %size_t% %size_t% %size_t% ) :varargs t)
  (primitive-nounwind module "cc_stack_enclose" %t*% (list %i8*% %t*% %fn-prototype*% %i32*% %size_t% %size_t% %size_t% %size_t% ) :varargs t)
  (primitive-nounwind module "cc_saveThreadLocalMultipleValues" %void% (list %tmv*% %mv-struct*%))
  (primitive-nounwind module "cc_loadThreadLocalMultipleValues" %void% (list %tmv*% %mv-struct*%))
  (primitive          module "cc_safe_fdefinition" %t*% (list %t*%))
  (primitive-nounwind module "cc_unsafe_fdefinition" %t*% (list %t*%))
  (primitive          module "cc_safe_setfdefinition" %t*% (list %t*%))
  (primitive-nounwind module "cc_unsafe_setfdefinition" %t*% (list %t*%))
  (primitive          module "cc_safe_symbol_value" %t*% (list %t*%))
  (primitive-nounwind module "cc_unsafe_symbol_value" %t*% (list %t*%))
  (primitive-nounwind module "cc_setSymbolValue" %void% (list %t*% %t*%))
  (primitive          module "cc_call_multipleValueOneFormCall" %return_type% (list %t*%))
  (primitive          module "cc_call"   %return_type% (list* %t*% %t*% %size_t%
                                                              (map 'list (lambda (x) x)
                                                                   (make-array core:+number-of-fixed-arguments+ :initial-element %t*%))) :varargs t)
  (primitive          module "cc_call_callback"   %return_type% (list* %t*% %t*% %size_t%
                                                                         (map 'list (lambda (x) x)
                                                                              (make-array core:+number-of-fixed-arguments+ :initial-element %t*%))) :varargs t)
  (primitive-nounwind module "cc_allowOtherKeywords" %i64% (list %i64% %t*%))
  (primitive-nounwind module "cc_matchKeywordOnce" %size_t% (list %t*% %t*% %t*%))
  (primitive          module "cc_ifNotKeywordException" %void% (list %t*% %size_t% %VaList_S*%))
  (primitive-nounwind module "cc_multipleValuesArrayAddress" %t*[0]*% nil)
  (primitive          module "cc_unwind" %void% (list %t*% %size_t%))
  (primitive          module "cc_throw" %void% (list %t*%) :does-not-return t)
  (primitive-nounwind module "cc_saveMultipleValue0" %void% (list %tmv*%))
  (primitive-nounwind module "cc_restoreMultipleValue0" %void% (list %tmv*%))
  (primitive-nounwind module "cc_pushLandingPadFrame" %t*% nil)
  (primitive-nounwind module "cc_popLandingPadFrame" %void% (list %t*%))
  (primitive          module "cc_landingpadUnwindMatchFrameElseRethrow" %size_t% (list %i8*% %t*%))

  ;; === CLASP-FFI TRANSLATORS ===

  ;; !!! NOTE !!! => PORTING ISSUE/TODO !
  ;; This implementation assumes the following associations:
  ;;
  ;; C++          -> LLVM         (!)
  ;; --------------------------------
  ;; char         -> i8
  ;; short        -> i16
  ;; int          -> i32
  ;; long         -> i64
  ;; long long    -> i64          (!)
  ;; float        -> float
  ;; doubls       -> double
  ;; long double  -> long float   (!)
  ;; size_t       -> size_t
  ;; ssize_t      -> size_t       (!)
  ;; void *       -> i64*         (!)

  ;; SHORT & UNSIGNED SHORT
  (primitive          module "from_object_short" %i16% (list %t*%))
  (primitive          module "to_object_short" %t*% (list %i16%))
  (primitive          module "from_object_unsigned_short" %i16% (list %t*%))
  (primitive          module "to_object_unsigned_short" %t*% (list %i16%))

  ;; INT & UNSIGNED INT
  (primitive          module "from_object_int" %i32% (list %t*%))
  (primitive          module "to_object_int" %t*% (list %i32%))
  (primitive          module "from_object_unsigned_int" %i32% (list %t*%))
  (primitive          module "to_object_unsigned_int" %t*% (list %i32%))

  ;; LONG & UNSIGNED LONG
  (primitive          module "from_object_long" %i64% (list %t*%))
  (primitive          module "to_object_long" %t*% (list %i64%))
  (primitive          module "from_object_unsigned_long" %i64% (list %t*%))
  (primitive          module "to_object_unsigned_long" %t*% (list %i64%))

  ;; LONG LONG & UNSIGNED LONG LONG
  (primitive          module "from_object_long_long" %i64% (list %t*%))
  (primitive          module "to_object_long_long" %t*% (list %i64%))
  (primitive          module "from_object_unsigned_long_long" %i64% (list %t*%))
  (primitive          module "to_object_unsigned_long_long" %t*% (list %i64%))

  ;; INT8 & UINT8
  (primitive          module "from_object_int8" %i8% (list %t*%))
  (primitive          module "to_object_int8" %t*% (list %i8%))
  (primitive          module "from_object_uint8" %i8% (list %t*%))
  (primitive          module "to_object_uint8" %t*% (list %i8%))

  ;; INT16 & UINT16
  (primitive          module "from_object_int16" %i16% (list %t*%))
  (primitive          module "to_object_int16" %t*% (list %i16%))
  (primitive          module "from_object_uint16" %i16% (list %t*%))
  (primitive          module "to_object_uint16" %t*% (list %i16%))

  ;; INT32 & UINT32
  (primitive          module "from_object_int32" %i32% (list %t*%))
  (primitive          module "to_object_int32" %t*% (list %i32%))
  (primitive          module "from_object_uint32" %i32% (list %t*%))
  (primitive          module "to_object_uint32" %t*% (list %i32%))

  ;; INT64 & UINT64
  (primitive          module "from_object_int64" %i64% (list %t*%))
  (primitive          module "to_object_int64" %t*% (list %i64%))
  (primitive          module "from_object_uint64" %i64% (list %t*%))
  (primitive          module "to_object_uint64" %t*% (list %i64%))

  ;; i128 HANDLING NOT IMPLEMENTED AS IT IS NOT USED

  ;; SIZE_T
  (primitive          module "from_object_size" %size_t% (list %t*%))
  (primitive          module "to_object_size" %t*% (list %size_t%))

  ;; SSIZE_T
  (primitive          module "from_object_ssize" %size_t% (list %t*%))
  (primitive          module "to_object_ssize" %t*% (list %size_t%))

  ;; PTRDIFF_T, TIME_T
  ;; (primitive          module "from_object_ptrdiff" %t*% (list %t*%)) - FIXME !
  ;; (primitive          module "to_object_ptrdiff" %t*% (list %uintptr_t%)) - FIXME !

  ;; (primitive          module "from_object_time" %t*% (list %t*%)) - FIXME !
  ;; (primitive          module "to_object_time" %t*% (list %t*%)) - FIOXME !

  ;; CHAR & UNSIGNED CHAR
  (primitive          module "from_object_char" %i8% (list %t*%))
  (primitive          module "to_object_char" %t*% (list %i8%))
  (primitive          module "from_object_unsigned_char" %i8% (list %t*%))
  (primitive          module "to_object_unsigned_char" %t*% (list %i8%))

  ;; FLOAT, DOUBLE & LONG FLOAT
  (primitive          module "from_object_float" %float% (list %t*%))
  (primitive          module "to_object_float" %t*% (list %float%))
  (primitive          module "from_object_double" %double% (list %t*%))
  (primitive          module "to_object_double" %t*% (list %double%))
  #+long-float (primitive module "from_object_long_double" %long-float% (list %t*%))
  #+long-float (primitive module "to_object_long_double" %t*% (list %long-float%))

  ;; POINTER / VOID *

  ;;(format *debug-io* "~%*** +VOID+ = ~S, +VOID*+ = ~S~%" %void% %void*%)
  ;; Note: using %void*% causes an error - so we use %i64*% instead here!
  (primitive          module "from_object_pointer" %i64*% (list %t*%))
  (primitive          module "to_object_pointer" %t*% (list %i64*%))
  (primitive          module "to_object_void" %t*% (list))
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
(defun compile-file-quick-module-pathname (file-name-modifier)
  (let* ((name-suffix (bformat nil "%05d-%s" (incf *quick-module-index*) file-name-modifier))
         (output-path (make-pathname
                       :name (concatenate
                              'string
                              (pathname-name *compile-file-output-pathname*)
                              "-" name-suffix)
                       :type "ll"
                       :defaults *compile-file-output-pathname*)))
    (ensure-directories-exist output-path)
    output-path))
(defun compile-file-quick-module-dump (module file-name-modifier)
  "Dump the module as a .ll file"
  (if *compile-file-debug-dump-module*
      (let* ((output-path (compile-file-quick-module-pathname file-name-modifier)))
        (let* ((output-name (namestring output-path))
               (fout (open output-name :direction :output)))
          (unwind-protect
               (llvm-sys:dump-module module fout)
            (close fout))))))

(defun compile-quick-module-pathname (file-name-modifier)
  (let* ((name-suffix (bformat nil "module-%05d-%s" (incf *quick-module-index*) file-name-modifier))
         (output-path (make-pathname
                       :name name-suffix
                       :directory '(:absolute "tmp")
                       :type "ll")))
    (ensure-directories-exist output-path)
    output-path))

(defun compile-quick-module-dump (module file-name-modifier)
  "Dump the module as a .ll file"
  (if *compile-debug-dump-module*
      (let* ((output-path (compile-quick-module-pathname file-name-modifier)))
        (let* ((output-name (namestring output-path))
               (fout (open output-name :direction :output)))
          (unwind-protect
               (llvm-sys:dump-module module fout)
            (close fout))))))

(defun quick-module-pathname (name-modifier)
  "If called under COMPILE-FILE the modules are dumped into the
same directory as the COMPILE-FILE output.  If called under COMPILE
they are dumped into /tmp"
  (if *compile-file-output-pathname*
      (compile-file-quick-module-pathname name-modifier)
      (compile-quick-module-pathname name-modifier)))

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
               (llvm-sys:dump-module *the-module* fout)
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
