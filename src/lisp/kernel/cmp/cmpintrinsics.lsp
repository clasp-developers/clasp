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

(defun llvm-print (msg)
  (irc-intrinsic "debugMessage" (irc-bit-cast (module-make-global-string msg) %i8*%)))

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

(define-symbol-macro %fixnum% #+address-model-64 %i64%
                              #+address-model-32 %i32%)
(define-symbol-macro %uint% %i32%) ; FIXME: export from C++ probably

(define-symbol-macro %float% (llvm-sys:type-get-float-ty *llvm-context*))
(define-symbol-macro %double% (llvm-sys:type-get-double-ty *llvm-context*))
#+long-float (define-symbol-macro %long-float% (llvm-sys:type-get-long-float-ty *llvm-context*))

(define-symbol-macro %size_t% #+address-model-64 %i64%
                              #+address-model-32 %i32%)
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


(define-symbol-macro %intptr_t% #+address-model-64 %i64%
                                #+address-model-32 %i32%)
(define-symbol-macro %uintptr_t% #+address-model-64 %i64%
                                #+address-model-32 %i32%)
(define-symbol-macro %uintptr_t*% (llvm-sys:type-get-pointer-to %uintptr_t%))
(defun make-uintptr_t (x)
  (and (> x most-positive-fixnum) (error "make sure the integer ~s fits in a %i64%" x))
  (cond
    ((= 8 +uintptr_t-size+) (jit-constant-i64 x))
    ((= 4 +uintptr_t-size+) (jit-constant-i32 x))
    (t (error "Add support for size uintptr_t = ~a" +uintptr_t-size+))))

;;; DO NOT CHANGE THE FOLLOWING STRUCT!!! IT MUST MATCH vaslist

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
(define-symbol-macro %t*[DUMMY]% (llvm-sys:array-type-get %t*% 64))
(define-symbol-macro %t*[DUMMY]*% (llvm-sys:type-get-pointer-to %t*[DUMMY]%))
(define-symbol-macro %tsp% (llvm-sys:struct-type-get *llvm-context* (smart-pointer-fields %t*%) nil))  ;; "T_sp"
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
(define-symbol-macro %gcvector-symsp% (llvm-sys:struct-type-get *llvm-context* (list %size_t% %size_t% %symsp%) nil))
(define-symbol-macro %vec0-tsp% (llvm-sys:struct-type-get *llvm-context* (list %gcvector-tsp%) nil))
(define-symbol-macro %vec0-symsp% (llvm-sys:struct-type-get *llvm-context* (list %gcvector-symsp%) nil))

;; Define the LoadTimeValue_O struct - right now just put in a dummy i32 - later put real fields here
(define-symbol-macro %ltv% (llvm-sys:struct-type-get *llvm-context* (list %vtable*% #+(or) %vec0-tsp%)  nil)) ;; "LoadTimeValue_O"
(define-symbol-macro %ltv*% (llvm-sys:type-get-pointer-to %ltv%))
(define-symbol-macro %ltv**% (llvm-sys:type-get-pointer-to %ltv*%))
(define-symbol-macro %ltvsp% (llvm-sys:struct-type-get *llvm-context* (smart-pointer-fields %ltv*%) nil))  ;; "LoadTimeValue_sp"
#+(or)(defvar +ltvsp*+ (llvm-sys:type-get-pointer-to %ltvsp%))
#+(or)(defvar +ltvsp**+ (llvm-sys:type-get-pointer-to +ltvsp*+))


(define-symbol-macro %mv-limit% +multiple-values-limit+)
(define-symbol-macro %mv-values-array% (llvm-sys:array-type-get %t*% %mv-limit%))
(define-symbol-macro %mv-struct% (llvm-sys:struct-type-get *llvm-context* (list %size_t% %mv-values-array%) nil #|| is-packed ||#))
(define-symbol-macro %mv-struct*% (llvm-sys:type-get-pointer-to %mv-struct%))
(define-symbol-macro %thread-info-struct% (llvm-sys:struct-type-get *llvm-context* (list %mv-struct%) nil))



#+(or)(progn
        (defvar +af+ (llvm-sys:struct-type-get *llvm-context* nil  nil)) ;; "ActivationFrame_O"
        (defvar +af*+ (llvm-sys:type-get-pointer-to +af+))
        (define-symbol-macro %afsp% (llvm-sys:struct-type-get *llvm-context* (smart-pointer-fields +af*+)  nil)) ;; "ActivationFrame_sp"
        (define-symbol-macro %afsp*% (llvm-sys:type-get-pointer-to %afsp%))
        )

;; Substitute afsp* with tsp
(define-symbol-macro %af% %t%)
(define-symbol-macro %af*% %t*%)
(define-symbol-macro %afsp% %tsp%)
(define-symbol-macro %afsp*% %tsp*%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Vector access and unboxed value stuff

(defun element-type->llvm-type (element-type)
  (case element-type
    ((t) %t*%)
    (ext:byte8 %i8%)
    (ext:integer8 %i8%)
    (ext:byte16 %i16%)
    (ext:integer16 %i16%)
    (ext:byte32 %i32%)
    (ext:integer32 %i32%)
    (ext:byte64 %i64%)
    (ext:integer64 %i64%)
    (fixnum %i64%) ; FIXME: should we store fixnums in arrays tagged? we do now.
    (single-float %float%)
    (double-float %double%)
    ;; should be less hardcoded
    (base-char %i8%) ; in core as C unsigned char
    (character %i32%) ; in core as C int
    (otherwise
     (error "BUG: Unknown element type ~a" element-type))))

(defun simple-vector-llvm-type (element-type)
  (llvm-sys:struct-type-get
   *llvm-context*
   (list
    ;; Spacer to get to the data
    (llvm-sys:array-type-get %i8% (- +simple-vector._data-offset+ +general-tag+))
    ;; The data, a flexible member
    (llvm-sys:array-type-get (element-type->llvm-type element-type) 0))
   ;; Not totally sure it should be packed.
   t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Stuff needed for generic functions
;;;
;;;
(progn
  ;; Tack on a size_t to store the number of remaining arguments
  #+X86-64(define-symbol-macro %va_list% (llvm-sys:struct-type-get *llvm-context* (list %i32% %i32% %i8*% %i8*%) nil))
  #-X86-64
  (error "I need a va_list struct definition for this system")

  (define-symbol-macro %va_list*% (llvm-sys:type-get-pointer-to %va_list%))
  (define-symbol-macro %vaslist% (llvm-sys:struct-type-get *llvm-context* (list %va_list% %size_t%) nil))
  (define-symbol-macro %vaslist*% (llvm-sys:type-get-pointer-to %vaslist%))

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

(defstruct (calling-convention-configuration (:type vector))
  use-only-registers
  invocation-history-frame*
  vaslist*
  register-save-area*)

;; Provide the arguments passed to the function in a convenient manner.
;; Either the register arguments are available in register-args
;;   or the va-list/remaining-nargs is used to access the arguments
;;   one after the other with calling-convention.va-arg
(defstruct (calling-convention-impl
            (:conc-name "CALLING-CONVENTION-")
            (:type vector))
  closure
  nargs
  use-only-registers ; If T then use only the register-args
  register-args ; The arguments that were passed in registers
  va-list*       ; The address of vaslist.va_list on the stack
  remaining-nargs* ; The address of vaslist._remaining_nargs on the stack
  register-save-area*
  invocation-history-frame*
  )

;; Parse the function arguments into a calling-convention
;;
;; What if we don't want/need to spill the registers to the register-save-area?
(defun initialize-calling-convention (arguments setup &optional (rewind t))
  (if (null (calling-convention-configuration-vaslist* setup))
      ;; If there is no vaslist then only register arguments are available
      ;;    no registers are spilled to the register-save-area and no InvocationHistoryFrame
      ;;    will be available to initialize
      (progn
        (make-calling-convention-impl :closure (first arguments)
                                      :nargs (second arguments)
                                      :use-only-registers t
                                      :register-args (nthcdr 2 arguments)))
      ;; The register arguments need to be spilled to the register-save-area
      ;;    and the vaslist needs to be initialized.
      ;;    If a InvocationHistoryFrame is available, then initialize it.
      (progn
        (let* ((vaslist                    (calling-convention-configuration-vaslist* setup))
               #++(_dbg                        (progn
                                                 (llvm-print "vaslist%N")
                                                 (irc-intrinsic "debugPointer" (irc-bit-cast vaslist %i8*%))))
               (register-save-area*         (calling-convention-configuration-register-save-area* setup))
               #++(_dbg                        (progn
                                                 (llvm-print "register-save-area%N")
                                                 (irc-intrinsic "debugPointer" (irc-bit-cast register-save-area* %i8*%))))
               (vaslist-addr-uint           (irc-ptr-to-int vaslist %uintptr_t% "vaslist-tagged-uint"))
               (va-list-addr                (irc-add vaslist-addr-uint (jit-constant-uintptr_t +vaslist-valist-offset+) "va-list-addr"))
               (va-list*                    (irc-int-to-ptr va-list-addr %va_list*% "va-list*"))
               (remaining-nargs*-uint       (irc-add vaslist-addr-uint (jit-constant-uintptr_t +vaslist-remaining-nargs-offset+) "remaining-nargs*-uint"))
               (remaining-nargs*            (irc-int-to-ptr remaining-nargs*-uint %size_t*% "remaining-nargs*"))
               #++(_dbg                        (irc-intrinsic "debugPointer" (irc-bit-cast remaining-nargs* %i8*%)))
               (_                           (maybe-spill-to-register-save-area arguments register-save-area*))
               (cc                          (make-calling-convention-impl :closure (first arguments)
                                                                          :nargs (second arguments) ;; The number of arguments
                                                                          :register-args (nthcdr 2 arguments)
                                                                          :use-only-registers (calling-convention-configuration-use-only-registers setup)
                                                                          :va-list* va-list*
                                                                          :remaining-nargs* remaining-nargs*
                                                                          :register-save-area* (calling-convention-configuration-register-save-area* setup)
                                                                          :invocation-history-frame* (calling-convention-configuration-invocation-history-frame* setup)))
               ;; va-start is done in caller
               #+(or)(_                           (calling-convention-args.va-start cc))
               #++(_dbg                        (progn
                                                 (llvm-print "After calling-convention-args.va-start%N")
                                                 (irc-intrinsic "debug_va_list" va-list*))))
          cc))))

(defun calling-convention-maybe-push-invocation-history-frame (cc)
  (when (calling-convention-invocation-history-frame* cc)
    (irc-intrinsic "cc_push_InvocationHistoryFrame"
                   (calling-convention-closure cc)
                   (calling-convention-invocation-history-frame* cc)
                   (calling-convention-va-list* cc)
                   (irc-load (calling-convention-remaining-nargs* cc)))))

(defun calling-convention-maybe-pop-invocation-history-frame (cc)
  (when (calling-convention-invocation-history-frame* cc)
    (irc-intrinsic "cc_pop_InvocationHistoryFrame"
                   (calling-convention-closure cc)
                   (calling-convention-invocation-history-frame* cc))))


(defun calling-convention-args.va-end (cc)
  (when (calling-convention-va-list* cc)
    (irc-intrinsic "llvm.va_end" (calling-convention-va-list* cc))))

;;;
;;; Read the next argument from the va_list
;;; Everytime the arg-idx is incremented, this function must be called.
(defun calling-convention-args.va-arg (cc &optional target-idx)
  (let* ((label                 (if (and target-idx core::*enable-print-pretty*)
                                    (bformat nil "arg-%d" target-idx)
                                    "rawarg"))
         (remaining-nargs*-addr (calling-convention-remaining-nargs* cc))
         (remaining-nargs       (irc-load remaining-nargs*-addr "rem-nargs"))
         (rem-nargs-1           (irc-sub  remaining-nargs (jit-constant-size_t 1) "rem-nargs-1"))
         (_                     (irc-store rem-nargs-1 remaining-nargs*-addr))
         (result                (irc-va_arg (calling-convention-va-list* cc) %t*% )))
    result))

(defun calling-convention-args.va-start (cc &optional (rewind t))
  "Like va-start - but it rewinds the va-list to start at the third argument. 
This allows all of the arguments to be accessed with successive calls to calling-convention-args.va-arg.
eg:  (f closure-ptr nargs a b c d ...)
                          ^-----  after calling-convention-args.va-start the va-list will point here.
                                  and remaining-nargs will contain nargs."
                                        ; Initialize the va_list - the only valid field will be overflow-area
  (let* ((va-list*                      (calling-convention-va-list* cc)))
    (when va-list*
      (irc-intrinsic "llvm.va_start" (irc-bit-cast va-list* %i8*%))
      (when rewind (calling-convention-rewind-va-list-to-start-on-third-argument cc)))))


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
  (define-symbol-macro %reglist-types% (list %t*% %t*% %t*% %t*% %t*%)) ; VaList follows register arguments
  (defvar *register-arg-names* (list "farg0" "farg1" "farg2" "farg3"))
  (defvar *reglist-arg-names* (list "farg0" "farg1" "farg2" "farg3" "VaList"))
  (defvar +fn-registers-prototype-argument-names+
    (list* "closure-ptr" "nargs" *register-arg-names*))
  (defvar +fn-reglist-prototype-argument-names+
    (list* "closure-ptr" "nargs" *reglist-arg-names*))
  (define-symbol-macro %fn-registers-prototype%
      (llvm-sys:function-type-get %tmv% (list* %t*% %size_t% %register-arg-types%) T #|VARARGS!|#))
  (define-symbol-macro %fn-reglist-prototype%
      (llvm-sys:function-type-get %tmv% (list* %t*% %size_t% %reglist-arg-types%) T #|VARARGS!|#))
  (define-symbol-macro %register-save-area% (llvm-sys:array-type-get
                                             %i8*%
                                             (/ +register-save-area-size+ +void*-size+)))
  (define-symbol-macro %register-save-area*% (llvm-sys:type-get-pointer-to %register-save-area%))
  (defun maybe-spill-to-register-save-area (registers register-save-area*)
    (unless registers
      (unless register-save-area*
        (error "If registers is NIL then register-save-area* also must be NIL")))
    (when registers
      (labels ((spill-reg (idx reg)
                 (let* ((addr-name     (bformat nil "addr%d" idx))
                        (addr          (irc-gep register-save-area* (list (jit-constant-size_t 0) (jit-constant-size_t idx)) addr-name))
                        (reg-i8*       (irc-bit-cast reg %i8*% "reg-i8*"))
                        (_             (irc-store reg-i8* addr)))
                   addr)))
        (let* (
               (addr-closure  (spill-reg 0 (elt registers 0)))
               (addr-nargs    (spill-reg 1 (irc-int-to-ptr (elt registers 1) %i8*%)))
               (addr-farg0    (spill-reg 2 (elt registers 2))) ; this is the first fixed arg currently.
               (addr-farg1    (spill-reg 3 (elt registers 3)))
               (addr-farg2    (spill-reg 4 (elt registers 4)))
               (addr-farg3    (spill-reg 5 (elt registers 5))))))))


  (defun calling-convention-rewind-va-list-to-start-on-third-argument (cc)
    (let* ((va-list*                      (calling-convention-va-list* cc))
           (register-save-area*           (calling-convention-register-save-area* cc))
           (remaining-nargs*              (calling-convention-remaining-nargs* cc)))
      (irc-intrinsic "cc_rewind_va_list" va-list* remaining-nargs* register-save-area*)))
;;; end of x86-64 specific stuff
  )


#-(and x86-64)
(error "Define calling convention for system")

;;; This is the normal C-style prototype for a function
(define-symbol-macro %fn-prototype% %fn-registers-prototype%)
(defvar +fn-prototype-argument-names+ +fn-registers-prototype-argument-names+)
;;; This is the C-style prototype with an extra argument that contains the vaslist for all arguments
(define-symbol-macro %fn-va-prototype% %fn-reglist-prototype%)
(defvar +fn-va-prototype-argument-names+ +fn-reglist-prototype-argument-names+)

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


(define-symbol-macro %function-description%
    (llvm-sys:struct-type-get *llvm-context*
                              (list %fn-prototype*%
                                    %gcroots-in-module*%
                                    %i32% ; source name index
                                    %i32% ; function name literal index
                                    %i32% ; lambda-list literal index
                                    %i32% ; docstring literal index
                                    %i32% ; lineno
                                    %i32% ; column
                                    %i32% ; filepos
                                    %i32% ; declareIndex
				    %i32% ; macroP
                                    ) nil ))
(define-symbol-macro %function-description*% (llvm-sys:type-get-pointer-to %function-description%))

;;
;; Define the InvocationHistoryFrame type for LispCompiledFunctionIHF
;;
;; %"class.core::InvocationHistoryFrame" = type { i32 (...)**, i32, %"class.core::InvocationHistoryStack"*, %"class.core::InvocationHistoryFrame"*, i8, i32 }
;;"Make this a generic pointer"
(define-symbol-macro %InvocationHistoryStack*% %i8*%)
(define-symbol-macro %InvocationHistoryFrame% (llvm-sys:struct-type-get *llvm-context* (list %i8*% %va_list% %size_t% #| %size_t% #|Removed BDS|# |#) "InvocationHistoryFrame"))
(define-symbol-macro %InvocationHistoryFrame*% (llvm-sys:type-get-pointer-to %InvocationHistoryFrame%))
;;  (llvm-sys:set-body %InvocationHistoryFrame% (list %i32**% %i32% #|%InvocationHistoryStack*% %InvocationHistoryFrame*%|# %i8*% %i8*% %i8% %i32%) nil)
;(define-symbol-macro %LispFunctionIHF% (llvm-sys:struct-type-create *llvm-context* :elements (list %InvocationHistoryFrame% %tsp% %tsp% %tsp% %i32% %i32%) :name "LispFunctionIHF"))
;; %"class.core::LispCompiledFunctionIHF" = type { %"class.core::LispFunctionIHF" }
;(define-symbol-macro %LispCompiledFunctionIHF% (llvm-sys:struct-type-create *llvm-context* :elements (list %LispFunctionIHF%) :name "LispCompiledFunctionIHF"))


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
        (irc-set-insert-point-basic-block entry-bb irbuilder-body)
        (with-irbuilder (irbuilder-body)
          (let* ((bc-bf (irc-bit-cast main-function %fn-start-up*% "fnptr-pointer"))
                 (_     (irc-intrinsic "cc_register_startup_function" bc-bf))
                 (_     (irc-ret-void))))
          ;;(llvm-sys:dump fn)
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
         (vaslist-size (llvm-sys:data-layout-get-type-alloc-size data-layout %vaslist%))
         (register-save-area-size (llvm-sys:data-layout-get-type-alloc-size data-layout %register-save-area%))
         (invocation-history-frame-size (llvm-sys:data-layout-get-type-alloc-size data-layout %InvocationHistoryFrame%))
         (gcroots-in-module-size (llvm-sys:data-layout-get-type-alloc-size data-layout %gcroots-in-module%))
         (function-description-size (llvm-sys:data-layout-get-type-alloc-size data-layout %function-description%)))
    (llvm-sys:throw-if-mismatched-structure-sizes :tsp tsp-size
                                                  :tmv tmv-size
                                                  :contab gcroots-in-module-size
                                                  :valist vaslist-size
                                                  :ihf invocation-history-frame-size
                                                  :register-save-area register-save-area-size
                                                  :function-description function-description-size)))

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

;;#+debug-mps (bformat t "cmp::*exceptions* --> %s%N" *exceptions*)


(defvar *exception-types-hash-table* (make-hash-table :test #'eq)
  "Map exception names to exception class extern 'C' names")

(mapcar #'(lambda (x &aux (name (car x)) (cname (cadr x)))
	    (core::hash-table-setf-gethash *exception-types-hash-table* name cname))
	*exceptions*)

(defun exception-typeid*-from-name (name)
  (let* ((cname (gethash name *exception-types-hash-table*))
	 (i8* (llvm-sys:get-or-create-external-global *the-module* cname %i8%)))
    i8*))




(defun assert-result-isa-llvm-value (result)
  (unless (llvm-sys:llvm-value-p result)
      (error "result must be an instance of llvm-sys:Value_O but instead it has the value %s" result)))


(defun codegen-startup-shutdown (gcroots-in-module roots-array-or-nil number-of-roots &optional ordered-literals array)
  (let ((startup-fn (irc-simple-function-create core:*module-startup-function-name*
                                                (llvm-sys:function-type-get %void% (list %t*%))
                                                'llvm-sys::External-linkage
                                                *the-module*
                                                :argument-names (list "values" )))

        (ordered-raw-literals-list nil))
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
      (cmp:irc-set-insert-point-basic-block entry-bb irbuilder-alloca)
      (with-irbuilder (irbuilder-alloca)
        (let ((start (if roots-array-or-nil
                         (irc-gep roots-array-or-nil
                                  (list (jit-constant-size_t 0)
                                        (jit-constant-size_t 0)))
                         (llvm-sys:constant-pointer-null-get %t**%))))
          (irc-intrinsic-call "cc_initialize_gcroots_in_module" (list gcroots-in-module start (jit-constant-size_t number-of-roots) values))
          ;; If the constant/literal list is provided - then we may need to generate code for closurettes
          (when ordered-literals
            (setf ordered-raw-literals-list (mapcar (lambda (x)
                                                     (cond
                                                       ((literal-node-runtime-p x)
                                                        (literal-node-runtime-object x))
                                                       ((literal:literal-node-closure-p x)
                                                        (literal:generate-run-time-code-for-closurette x irbuilder-alloca array)
                                                        nil)
                                                       (t (error "Illegal object ~s in ordered-literals list" x))))
                                                   ordered-literals)))
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
        (irc-set-insert-point-basic-block entry-bb irbuilder-alloca)
        (with-irbuilder (irbuilder-alloca)
          (progn
            (irc-intrinsic-call "cc_shutdown_gcroots_in_module" (list gcroots-in-module))
            (irc-ret-void))))
      (values startup-fn shutdown-fn ordered-raw-literals-list))))






;;; Define what ltvc_xxx functions return
(define-symbol-macro %ltvc-return% %void%)


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
(defvar *current-function-description* nil "The current function description")
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
    (cmp-log "Dumping module to %s%N" output-path)
    (ensure-directories-exist output-path)
    output-path))
(defun compile-file-quick-module-dump (module file-name-modifier compile-file-debug-dump-module)
  "Dump the module as a .ll file"
  (if compile-file-debug-dump-module
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

(defun compile-quick-module-dump (module file-name-modifier &optional (compile-debug-dump-module *compile-debug-dump-module*))
  "Dump the module as a .ll file"
  (if compile-debug-dump-module
      (let* ((output-path (compile-quick-module-pathname file-name-modifier))
             (output-name (namestring output-path))
             fout)
        (unwind-protect
             (progn
               (setf fout (open output-name :direction :output))
               (llvm-sys:dump-module module fout))
          (when fout (close fout))))))

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
  (cmp-log "About to dump module - %s%N" name-modifier)
  (if *compile-file-output-pathname*
      (compile-file-quick-module-dump module name-modifier *compile-file-debug-dump-module*)
      (compile-quick-module-dump module name-modifier *compile-debug-dump-module*)))


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
