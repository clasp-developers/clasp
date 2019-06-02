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
(defvar *compilation-module-index* 0
  "Incremented for each module build within a compile-file.
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
(define-symbol-macro %atomic<size_t>% %size_t%)
(define-symbol-macro %size_t*% (llvm-sys:type-get-pointer-to %size_t%))
(define-symbol-macro %size_t**% (llvm-sys:type-get-pointer-to %size_t*%))
(define-symbol-macro %size_t[0]% (llvm-sys:array-type-get %size_t% 0))

(define-symbol-macro %void% (llvm-sys:type-get-void-ty *llvm-context*))
(define-symbol-macro %void*% (llvm-sys:type-get-pointer-to %void%))
(define-symbol-macro %void**% (llvm-sys:type-get-pointer-to %void*%))

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

;; Define the T_O struct - right now just put in a dummy i32 - later put real fields here
(define-symbol-macro %t% (llvm-sys:struct-type-get *llvm-context* nil  nil)) ;; "T_O"
(define-symbol-macro %t*% (llvm-sys:type-get-pointer-to %t%))
(define-symbol-macro %t**% (llvm-sys:type-get-pointer-to %t*%))
(define-symbol-macro %t*[0]% (llvm-sys:array-type-get %t*% 0))
(define-symbol-macro %t*[0]*% (llvm-sys:type-get-pointer-to %t*[0]%))
(define-symbol-macro %t*[DUMMY]% (llvm-sys:array-type-get %t*% 64))
(define-symbol-macro %t*[DUMMY]*% (llvm-sys:type-get-pointer-to %t*[DUMMY]%))
(define-symbol-macro %tsp% (llvm-sys:struct-type-get *llvm-context* (smart-pointer-fields %t*%) nil))  ;; "T_sp"
(define-symbol-macro %atomic<tsp>% %tsp%)
(define-symbol-macro %tsp*% (llvm-sys:type-get-pointer-to %tsp%))
(define-symbol-macro %tsp**% (llvm-sys:type-get-pointer-to %tsp*%))
(define-symbol-macro %tsp[0]% (llvm-sys:array-type-get %tsp% 0))

(define-symbol-macro %metadata% (llvm-sys:type-get-metadata-ty *llvm-context*))

;;; MUST match Instance_O layout
(define-symbol-macro %instance%
  (llvm-sys:struct-type-get
   *llvm-context*
   (list %i8*%     ; 0 vtable
         %t*%      ; 1 _Sig
         %t*%      ; 2 _Class
         %t*%      ; 3 _Rack
         )
   nil))
(defconstant +instance.rack-index+ 3)
(define-symbol-macro %instance*% (llvm-sys:type-get-pointer-to %instance%))


;;; Must match SimpleVector_O aka GCArray_moveable<T_sp>
(define-symbol-macro %simple-vector%
  (llvm-sys:struct-type-get
   *llvm-context*
   (list %i8*%     ; 0 vtable
         %size_t%  ; 1 length
         %tsp[0]%  ; 2 zeroth element of data
         )
   nil))
(defconstant +simple-vector.length-index+ 1)
(defconstant +simple-vector.data-index+ 2)

(define-symbol-macro %rack%
  (llvm-sys:struct-type-get
   *llvm-context*
   (list %i8*%     ; 0 vtable
         %size_t%  ; 1 length
         %tsp%     ; 2 Stamp
         %t*[0]%  ; 3 zeroth element of data
         )
   nil))
(define-symbol-macro %rack*% (llvm-sys:type-get-pointer-to %rack%))

(defconstant +rack.stamp-index+ 2)
(defconstant +rack.data-index+ 3)


(define-symbol-macro %mdarray%
  (llvm-sys:struct-type-get
   *llvm-context*
   (list %i8*%   ; 0 vtable
         %size_t% ; 1 _FillPointerOrLengthOrDummy
         %size_t% ; 2 _ArrayTotalSize
         %t*%     ; 3 _Data
         %size_t% ; 4 _DisplacedIndexOffset
         %size_t% ; 5 _Flags
         %size_t% ; 6 rank
         %size_t[0]% ; 7 dimensions
         )
   nil))


(defconstant +mdarray._FillPointerOrLengthOrDummy-index+ 1)
(defconstant +mdarray._ArrayTotalSize-index+ 2)
(defconstant +mdarray._Data-index+ 3)
(defconstant +mdarray._DisplacedIndexOffset-index+ 4)
(defconstant +mdarray._Flags-index+ 5)
(defconstant +mdarray.rank-index+ 6)
(defconstant +mdarray.dimensions-index+ 7)


(define-symbol-macro %value-frame%
  (llvm-sys:struct-type-get
   *llvm-context*
   (list %i8*%     ; 0 vtable
         %tsp%     ; 1 _Parent
         %size_t%  ; 2 length
         %tsp[0]%  ; 3 zeroth element of data
         )
   nil))
(define-symbol-macro %value-frame*% (llvm-sys:type-get-pointer-to %value-frame%))
(defconstant +value-frame.parent-index+ 1)
(defconstant +value-frame.length-index+ 2)
(defconstant +value-frame.data-index+ 3)



;;; MUST match FuncallableInstance_O layout
(define-symbol-macro %funcallable-instance%
  (llvm-sys:struct-type-get
   *llvm-context*
   (list %i8*%     ; 0 vtable
         %i8*%     ; 1 entry (From Function_O)
         %t*%      ; 2 _Class
         %t*%      ; 3 _Rack
         %t*%      ; 4 _Sig
         %function-description*%   ; 5 FunctionDescription*
         %atomic<size_t>%          ; 6 _Compilations
         %atomic<tsp>%             ; 7 _CallHistory
         %atomic<tsp>%             ; 7 _SpecializerProfile
         %atomic<tsp>%             ; 8 _CompiledDispatchFunction
         )
   nil))
(define-symbol-macro %funcallable-instance*% (llvm-sys:type-get-pointer-to %funcallable-instance%))
(defconstant +funcallable-instance.rack-index+ 3)
(define-symbol-macro %funcallable-instance*% (llvm-sys:type-get-pointer-to %funcallable-instance%))

;;;
;;; The %symbol% type MUST match the layout and size of Symbol_O in symbol.h
;;;
(define-symbol-macro %symbol%
    (llvm-sys:struct-type-get
     *llvm-context*
     (list %i8*%                        ; 0 vtable
           %t*%                         ; 1 _Name
           %t*%                         ; 2 _HomePackage
           %t*%                         ; 3 _GlobalValue
           %t*%                         ; 4 _Function
           %t*%                         ; 5 _SetfFunction
           %i32%                        ; 6 _BindingIdx
           %i32%                        ; 7 _Flags
           %t*%                         ; 8 _PropertyList
           ) nil))
(defconstant +symbol.function-index+ 4)
(defconstant +symbol.setf-function-index+ 5)

(define-symbol-macro %symbol*% (llvm-sys:type-get-pointer-to %symbol%))
(define-symbol-macro %symsp% (llvm-sys:struct-type-get *llvm-context* (smart-pointer-fields %symbol*%) nil)) ;; "Sym_sp"
(define-symbol-macro %symsp*% (llvm-sys:type-get-pointer-to %symsp%))

(define-symbol-macro %cons% (llvm-sys:struct-type-get *llvm-context* (smart-pointer-fields %t*% %t*%) nil))
(define-symbol-macro %cons*% (llvm-sys:type-get-pointer-to %cons%))

;; This structure must match the gctools::GCRootsInModule structure
(define-symbol-macro %gcroots-in-module% (llvm-sys:struct-type-get
                                          *llvm-context*
                                          (list
                                           %size_t% ; _index_offset
                                           %i8*% ; _boehm_shadow_memory
                                           %i8*% ; _module_memory
                                           %size_t% ; _num_entries
                                           %size_t% ; _capacity
                                           %i8**% ; function pointers
                                           %i8**% ; function descriptions
                                           %size_t% ; number of functions
                                           ) nil))
(define-symbol-macro %gcroots-in-module*% (llvm-sys:type-get-pointer-to %gcroots-in-module%))

;; The definition of %tmv% doesn't quite match T_mv because T_mv inherits from T_sp
(define-symbol-macro %tmv% (llvm-sys:struct-type-get *llvm-context* (smart-pointer-fields %t*% %size_t%) nil))  ;; "T_mv"
(define-symbol-macro %return-type% %tmv%)
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
    ;; Spacer to get to the stuff that matters
    (llvm-sys:array-type-get %i8% (- +simple-vector._length-offset+ +general-tag+))
    ;; The length, an untagged integer
    %size_t%
    ;; The data, a flexible member
    (llvm-sys:array-type-get (element-type->llvm-type element-type) 0))
   ;; Not totally sure it should be packed.
   t))

(defvar +simple-vector-length-slot+ 1)
(defvar +simple-vector-data-slot+ 2)

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
  register-save-area*)

;; Provide the arguments passed to the function in a convenient manner.
;; Either the register arguments are available in register-args
;;   or the va-list is used to access the arguments
;;   one after the other with calling-convention.va-arg
(defstruct (calling-convention-impl
            (:conc-name "CALLING-CONVENTION-")
            (:type vector))
  closure
  nargs
  use-only-registers ; If T then use only the register-args
  register-args ; The arguments that were passed in registers
  va-list*       ; The address of the va_list on the stack
  register-save-area*
  cleavir-lambda-list ; cleavir-style lambda-list
  rest-alloc ; whether we can dx or ignore a &rest argument
  )

;; Parse the function arguments into a calling-convention
;;
;; What if we don't want/need to spill the registers to the register-save-area?
(defun initialize-calling-convention (arguments setup &key cleavir-lambda-list rest-alloc)
  (let ((register-save-area* (calling-convention-configuration-register-save-area* setup)))
    (if (null register-save-area*)
        ;; If there's no RSA, we determined we only need registers and don't need to dump things.
        (make-calling-convention-impl :closure (first arguments)
                                      :nargs (second arguments)
                                      :use-only-registers t
                                      :register-args (nthcdr 2 arguments)
                                      :cleavir-lambda-list cleavir-lambda-list
                                      :rest-alloc rest-alloc)
        ;; The register arguments need to be spilled to the register-save-area
        ;;    and the va_list needs to be initialized.
        ;;    If a InvocationHistoryFrame is available, then initialize it.
        (let* ((use-only-registers (calling-convention-configuration-use-only-registers setup))
               (create-a-va-list (null use-only-registers)))
          (maybe-spill-to-register-save-area arguments register-save-area*)
          (make-calling-convention-impl :closure (first arguments)
                                        :nargs (second arguments) ;; The number of arguments
                                        :register-args (nthcdr 2 arguments)
                                        :use-only-registers use-only-registers
                                        :va-list* (when create-a-va-list (alloca-va_list)) ; Only create va-list* if we need to rewind it
                                        :register-save-area* register-save-area*
                                        :cleavir-lambda-list cleavir-lambda-list
                                        :rest-alloc rest-alloc)))))

(defun calling-convention-args.va-end (cc)
  (when (calling-convention-va-list* cc)
    (irc-intrinsic "llvm.va_end" (calling-convention-va-list* cc))))

;;;
;;; Read the next argument from the va_list
(defun calling-convention-args.va-arg (cc)
  (irc-va_arg (calling-convention-va-list* cc) %t*%))

(defun calling-convention-args.va-start (cc &optional (rewind t))
  "Like va-start - but it rewinds the va-list to start at the third argument. 
This allows all of the arguments to be accessed with successive calls to calling-convention-args.va-arg.
eg:  (f closure-ptr nargs a b c d ...)
                          ^-----  after calling-convention-args.va-start the va-list will point here."
                                        ; Initialize the va_list - the only valid field will be overflow-area
  (let* ((va-list*                      (calling-convention-va-list* cc)))
    (when va-list*
      (irc-intrinsic "llvm.va_start" (irc-bit-cast va-list* %i8*%))
      (when rewind (calling-convention-rewind-va-list-to-start-on-third-argument cc)))))


(defparameter *debug-register-parameter* nil)

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
  ;; (Maybe) generate code to store registers in memory. Return value unspecified.

  (defun dbg-register-parameter (register name argno &optional (type-name "T_O*") (type llvm-sys:+dw-ate-address+))
    (let* ((dbg-arg0 (dbg-create-parameter-variable :name name
                                                    :argno argno
                                                    :lineno *dbg-current-function-lineno*
                                                    :type (llvm-sys:create-basic-type *the-module-dibuilder* type-name 64 type)
                                                    :always-preserve t))
           (diexpression (llvm-sys:create-expression-none *the-module-dibuilder*))
           (dbg-arg0-value (llvm-sys:metadata-as-value-get *llvm-context* dbg-arg0))
           (diexpr-value (llvm-sys:metadata-as-value-get *llvm-context* diexpression)))
      (if *debug-register-parameter*
          (irc-intrinsic "llvm.dbg.value" (llvm-sys:metadata-as-value-get *llvm-context* (llvm-sys:value-as-metadata-get register)) dbg-arg0-value diexpr-value))))
  
  (defun maybe-spill-to-register-save-area (registers register-save-area*)
    (if registers
        (labels ((spill-reg (idx reg addr-name)
                   (let* ((addr          (irc-gep register-save-area* (list (jit-constant-size_t 0) (jit-constant-size_t idx)) addr-name))
                          (reg-i8*       (irc-bit-cast reg %i8*% "reg-i8*"))
                          (_             (irc-store reg-i8* addr "" t)))
                     addr)))
          (let* ((addr-closure  (spill-reg 0 (elt registers 0) "closure0"))
                 (addr-nargs    (spill-reg 1 (irc-int-to-ptr (elt registers 1) %i8*%) "nargs1"))
                 (addr-farg0    (spill-reg 2 (elt registers 2) "arg0")) ; this is the first fixed arg currently.
                 (addr-farg1    (spill-reg 3 (elt registers 3) "arg1"))
                 (addr-farg2    (spill-reg 4 (elt registers 4) "arg2"))
                 (addr-farg3    (spill-reg 5 (elt registers 5) "arg3")))
            (dbg-register-parameter (elt registers 0) "closure" 0)
            (dbg-register-parameter (elt registers 1) "nargs" 1 "int" llvm-sys:+dw-ate-signed-fixed+)
            (dbg-register-parameter (elt registers 2) "farg0" 2)
            (dbg-register-parameter (elt registers 3) "farg1" 3)
            (dbg-register-parameter (elt registers 4) "farg2" 4)
            (dbg-register-parameter (elt registers 5) "farg3" 5)
            ))
        (unless register-save-area*
          (error "If registers is NIL then register-save-area* also must be NIL"))))

  (defun calling-convention-rewind-va-list-to-start-on-third-argument (cc)
    (let* ((va-list*                      (calling-convention-va-list* cc))
           (register-save-area*           (calling-convention-register-save-area* cc)))
      (irc-intrinsic "cc_rewind_va_list" va-list* register-save-area*)))
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


;;
;; The %function% type MUST match the layout and size of Function_O in functor.h
;;
(define-symbol-macro %Function% (llvm-sys:struct-type-get *llvm-context* (list %i8*%    ; vtable
                                                                               %fn-prototype*%     ; entry
                                                                               ) nil))
(defconstant +function.entry-index+ 1)

(define-symbol-macro %Function-ptr% (llvm-sys:type-get-pointer-to %Function%))
(define-symbol-macro %Function_sp% (llvm-sys:struct-type-get *llvm-context* (smart-pointer-fields %Function-ptr%) nil)) ;; "Cfn_sp"
(define-symbol-macro %Function_sp*% (llvm-sys:type-get-pointer-to %Function_sp%))

;;; ------------------------------------------------------------
;;;
;;; This must match FunctionDescription in functor.h
;;;
;;; source-info/function-name are stored in a CONS cell CAR/CDR
;;; lambda-list/docstring are stored in a CONS cell CAR/CDR
(define-symbol-macro %function-description%
    (llvm-sys:struct-type-get *llvm-context*
                              (list %fn-prototype*%
                                    %gcroots-in-module*%
                                    %size_t% ; source-info.function-name index
                                    %size_t% ; lambda-list./docstring literal index
                                    %i32% ; lineno
                                    %i32% ; column
                                    %i32% ; filepos
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

#|
  (defun make-gv-file-scope-handle (module &optional handle)
    (if (null handle) (setq handle -1))
    (llvm-sys:make-global-variable module
                                   %i32%  ; type
                                   nil    ; constant
                                   'llvm-sys:internal-linkage
                                   (jit-constant-i32 handle)
                                   "file-scope-handle"))
|#

(defun add-global-ctor-function (module main-function &key position register-library)
  "Create a function with the name core:+clasp-ctor-function-name+ and
have it call the main-function"
  #+(or)(unless (eql module (llvm-sys:get-parent main-function))
    (error "The parent of the func-ptr ~a (a module) does not match the module ~a" (llvm-sys:get-parent main-function) module))
  (let* ((*the-module* module)
         (fn (irc-simple-function-create
              (core:bformat nil "%s_%d" core::+clasp-ctor-function-name+ (core:next-number))
              %fn-ctor%
              'llvm-sys:internal-linkage
              *the-module*
              :argument-names +fn-ctor-argument-names+)))
    (let* ((irbuilder-body (llvm-sys:make-irbuilder *llvm-context*))
           (*current-function* fn)
           (entry-bb (irc-basic-block-create "entry" fn)))
      (irc-set-insert-point-basic-block entry-bb irbuilder-body)
      (with-irbuilder (irbuilder-body)
        #+(or)(let* ((internal-functions-names-global (llvm-sys:get-named-global cmp:*the-module* *internal-functions-names-global-name*))
              (internal-functions-global (llvm-sys:get-named-global cmp:*the-module* *internal-functions-global-name*))
              (internal-functions-length-global (llvm-sys:get-named-global cmp:*the-module* *internal-functions-length-global-name*))
              (internal-functions-names-global-bf (irc-bit-cast internal-functions-names-global %i8*% "internal-functions-names-ptr"))
              (internal-functions-global-bf (irc-bit-cast internal-functions-global %i8*% "internal-functions-ptr")))
          (irc-intrinsic "cc_register_library" fn internal-functions-names-global-bf internal-functions-global-bf internal-functions-length-global))
        (let* ((bc-bf (irc-bit-cast main-function %fn-start-up*% "fnptr-pointer"))
               (_     (irc-intrinsic "cc_register_startup_function" (jit-constant-size_t position) bc-bf))
               (_     (irc-ret-void))))
        ;;(llvm-sys:dump fn)
        fn))))

(defun add-main-function (module run-all-function)
  "Create an external function with the name main have it call the run-all-function"
  (let* ((*the-module* module)
         (fn (irc-simple-function-create
              "MAIN"
              %fn-start-up%
              cmp:*default-linkage*
              *the-module*
              :argument-names +fn-start-up-argument-names+)))
    (let* ((irbuilder-body (llvm-sys:make-irbuilder *llvm-context*))
           (*current-function* fn)
           (entry-bb (irc-basic-block-create "entry" fn)))
      (irc-set-insert-point-basic-block entry-bb irbuilder-body)
      (with-irbuilder (irbuilder-body)
        (let* (
               (bc-bf (irc-bit-cast run-all-function %fn-start-up*% "run-all-pointer"))
               (_     (irc-intrinsic "cc_invoke_sub_run_all_function" bc-bf))
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

  (defun remove-llvm.used-if-exists (module)
    (let ((global (llvm-sys:get-named-global module "llvm.used")))
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

(defun make-boot-function-global-variable (module func-designator &key position register-library)
  "* Arguments
- module :: An llvm module
- func-ptr :: An llvm function
* Description
Add the global variable llvm.global_ctors to the Module (linkage appending)
and initialize it with an array consisting of one function pointer."
  (let ((startup-fn (cond
                      ((stringp func-designator)
                        (llvm-sys:get-function module func-designator))
                      ((typep func-designator 'llvm-sys:function)
                       func-designator)
                      (t (error "~a must be a function name or llvm-sys:function" func-designator)))))
    (unless startup-fn
      (error "Could not find ~a in module" func-designator))
    #+(or)(unless (eql module (llvm-sys:get-parent func-ptr))
            (error "The parent of the func-ptr ~a (a module) does not match the module ~a" (llvm-sys:get-parent func-ptr) module))
    (let* ((global-ctor (add-global-ctor-function module startup-fn
                                                  :position position
                                                  :register-library register-library)))
      (incf *compilation-module-index*)
      (add-llvm.global_ctors module *compilation-module-index* global-ctor))))

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
         (symbol-size (llvm-sys:data-layout-get-type-alloc-size data-layout %symbol%))
         (symbol-layout (llvm-sys:data-layout-get-struct-layout data-layout %symbol%))
         (symbol-function-offset (llvm-sys:struct-layout-get-element-offset symbol-layout +symbol.function-index+))
         (symbol-setf-function-offset (llvm-sys:struct-layout-get-element-offset symbol-layout +symbol.setf-function-index+))
         (function-size (llvm-sys:data-layout-get-type-alloc-size data-layout %function%))
         (function-layout (llvm-sys:data-layout-get-struct-layout data-layout %function%))
         (function-entry-offset (llvm-sys:struct-layout-get-element-offset function-layout +function.entry-index+))
         (vaslist-size (llvm-sys:data-layout-get-type-alloc-size data-layout %vaslist%))
         (register-save-area-size (llvm-sys:data-layout-get-type-alloc-size data-layout %register-save-area%))
         (invocation-history-frame-size (llvm-sys:data-layout-get-type-alloc-size data-layout %InvocationHistoryFrame%))
         (gcroots-in-module-size (llvm-sys:data-layout-get-type-alloc-size data-layout %gcroots-in-module%))
         (function-description-size (llvm-sys:data-layout-get-type-alloc-size data-layout %function-description%)))
    (llvm-sys:throw-if-mismatched-structure-sizes :tsp tsp-size
                                                  :tmv tmv-size
                                                  :symbol symbol-size
                                                  :symbol-function-offset symbol-function-offset
                                                  :symbol-setf-function-offset symbol-setf-function-offset
                                                  :function function-size
                                                  :function-entry-offset function-entry-offset
                                                  :contab gcroots-in-module-size
                                                  :valist vaslist-size
                                                  :ihf invocation-history-frame-size
                                                  :register-save-area register-save-area-size
                                                  :function-description function-description-size)

    (let* ((instance-size (llvm-sys:data-layout-get-type-alloc-size data-layout %instance%))
           (instance-layout (llvm-sys:data-layout-get-struct-layout data-layout %instance%))
           (instance-rack-offset (llvm-sys:struct-layout-get-element-offset instance-layout +instance.rack-index+)))
      (core:verify-instance-layout instance-size instance-rack-offset))

    (unless (= +instance.rack-index+ +funcallable-instance.rack-index+)
      (error "The +instance.rack-index+ ~d MUST match +funcallable-instance.rack-index+ ~d"
             +instance.rack-index+ +funcallable-instance.rack-index+))
    (let* ((funcallable-instance-size (llvm-sys:data-layout-get-type-alloc-size data-layout %funcallable-instance%))
           (funcallable-instance-layout (llvm-sys:data-layout-get-struct-layout data-layout %funcallable-instance%))
           (funcallable-instance-rack-offset (llvm-sys:struct-layout-get-element-offset funcallable-instance-layout +funcallable-instance.rack-index+)))
      (core:verify-funcallable-instance-layout funcallable-instance-size funcallable-instance-rack-offset))
    (let* ((simple-vector-layout (llvm-sys:data-layout-get-struct-layout data-layout %simple-vector%))
           (simple-vector-length-offset (llvm-sys:struct-layout-get-element-offset simple-vector-layout +simple-vector.length-index+))
           (simple-vector-data-offset (llvm-sys:struct-layout-get-element-offset simple-vector-layout +simple-vector.data-index+)))
      (core:verify-simple-vector-layout simple-vector-length-offset simple-vector-data-offset))
    (let* ((rack-layout (llvm-sys:data-layout-get-struct-layout data-layout %rack%))
           (rack-stamp-offset (llvm-sys:struct-layout-get-element-offset rack-layout +rack.stamp-index+))
           (rack-data-offset (llvm-sys:struct-layout-get-element-offset rack-layout +rack.data-index+)))
      (core:verify-rack-layout rack-stamp-offset rack-data-offset))
    (let* ((mdarray-layout (llvm-sys:data-layout-get-struct-layout data-layout %mdarray%))
           (mdarray._FillPointerOrLengthOrDummy-offset (llvm-sys:struct-layout-get-element-offset mdarray-layout +mdarray._FillPointerOrLengthOrDummy-index+))
           (mdarray._ArrayTotalSize-offset (llvm-sys:struct-layout-get-element-offset mdarray-layout +mdarray._ArrayTotalSize-index+))
           (mdarray._Data-offset (llvm-sys:struct-layout-get-element-offset mdarray-layout +mdarray._Data-index+))
           (mdarray._DisplacedIndexOffset-offset (llvm-sys:struct-layout-get-element-offset mdarray-layout +mdarray._DisplacedIndexOffset-index+))
           (mdarray._Flags-offset (llvm-sys:struct-layout-get-element-offset mdarray-layout +mdarray._Flags-index+))
           (mdarray.rank-offset (llvm-sys:struct-layout-get-element-offset mdarray-layout +mdarray.rank-index+))
           (mdarray.dimensions-offset (llvm-sys:struct-layout-get-element-offset mdarray-layout +mdarray.dimensions-index+)))
      (core:verify-mdarray-layout mdarray._FillPointerOrLengthOrDummy-offset
                                  mdarray._ArrayTotalSize-offset
                                  mdarray._Data-offset
                                  mdarray._DisplacedIndexOffset-offset
                                  mdarray._Flags-offset
                                  mdarray.rank-offset
                                  mdarray.dimensions-offset))
    (let* ((value-frame-layout (llvm-sys:data-layout-get-struct-layout data-layout %value-frame%))
           (value-frame-parent-offset (llvm-sys:struct-layout-get-element-offset value-frame-layout +value-frame.parent-index+))
           (value-frame-length-offset (llvm-sys:struct-layout-get-element-offset value-frame-layout +value-frame.length-index+))
           (value-frame-data-offset (llvm-sys:struct-layout-get-element-offset value-frame-layout +value-frame.data-index+)))
      (core:verify-value-frame-layout value-frame-parent-offset value-frame-length-offset value-frame-data-offset))
    ))

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

(defvar *exception-types-hash-table* (make-hash-table :test #'eq)
  "Map exception names to exception class extern 'C' names")

(mapcar #'(lambda (x &aux (name (car x)) (cname (cadr x)))
	    (core::hash-table-setf-gethash *exception-types-hash-table* name cname))
	*exceptions*)

(defun exception-typeid*-from-name (name)
  (let* ((cname (gethash name *exception-types-hash-table*))
	 (i8* (llvm-sys:get-or-create-external-global *the-module* cname %i8% 'llvm-sys:external-linkage)))
    i8*))

(defun assert-result-isa-llvm-value (result)
  (unless (llvm-sys:llvm-value-p result)
      (error "result must be an instance of llvm-sys:Value_O but instead it has the value %s" result)))


(defun codegen-startup-shutdown (module &optional gcroots-in-module roots-array-or-nil (number-of-roots 0) ordered-literals array)
  (let ((startup-fn (irc-simple-function-create core:*module-startup-function-name*
                                                (llvm-sys:function-type-get %void% (list %t*%))
                                                'llvm-sys::External-linkage
                                                module
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
          (when gcroots-in-module
            (irc-intrinsic-call "cc_initialize_gcroots_in_module" (list gcroots-in-module ; holder
                                                                        start ; root_address
                                                                        (jit-constant-size_t number-of-roots) ; num_roots
                                                                        values ; initial_data
                                                                        (llvm-sys:constant-pointer-null-get %i8**%) ; transient_alloca
                                                                        (jit-constant-size_t 0) ; transient_entries
                                                                        (jit-constant-size_t 0) ; function_pointer_count
                                                                        (irc-bit-cast (llvm-sys:constant-pointer-null-get %fn-prototype*%) %i8**%) ; fptrs
                                                                        (irc-bit-cast (llvm-sys:constant-pointer-null-get %function-description*%) %i8**%) ; fdescs
                                                                        )))
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
          (when gcroots-in-module
            (irc-intrinsic-call "cc_finish_gcroots_in_module" (list gcroots-in-module)))
          (irc-ret-void))))
    (let ((shutdown-fn (irc-simple-function-create core:*module-shutdown-function-name*
                                                   (llvm-sys:function-type-get %void% nil)
                                                   'llvm-sys::External-linkage
                                                   module
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
            (if gcroots-in-module
                (irc-intrinsic-call "cc_remove_gcroots_in_module" (list gcroots-in-module)))
            (irc-ret-void))))
      (values startup-fn shutdown-fn ordered-raw-literals-list))))






;;; Define what ltvc_xxx functions return
(define-symbol-macro %ltvc-return% %void%)


;;------------------------------------------------------------
;;
;; Setup dynamic variables
;;
;;

(defvar *compile-file-pathname* nil "Store the pathname of the currently compiled file")
(defvar *compile-file-truename* nil "Store the truename of the currently compiled file")
(defvar *compile-file-unique-symbol-prefix* "" "Store a unique prefix for symbols that are external-linkage")
(defvar *compile-file-file-scope* nil "Store the SourceFileInfo object for the compile-file target")

(defvar *source-debug-pathname*)
(defvar *source-debug-offset* 0)

(defvar *gv-boot-functions* nil
  "A global value that stores a pointer to the boot function for the Module.
It has appending linkage.")
(defvar *current-form* nil "The current form being compiled")
(defvar *current-env* nil "Current environment")
(defvar *current-function* nil "The current function")
(defvar *current-function-description* nil "The current function description")
(defvar *current-function-name* nil "Store the current function name")
(defvar *gv-current-function-name* nil "Store the global value in the module of the current function name ")


(defun compile-file-quick-module-pathname (file-name-modifier &optional (cfo-pathname *compile-file-output-pathname*))
  (let* ((name-suffix (bformat nil "%05d-%s" (core:next-number) file-name-modifier))
         (base-path (pathname (core:monitor-directory)))
         (cfo-directory0 (pathname-directory cfo-pathname))
;;;         (_ (format t "cfo-directory0: ~s~%" cfo-directory0))
;;;         (_ (format t "test ~s~%" (and (consp cfo-directory0) (eq (car cfo-directory0) :absolute))))
         (cfo-directory (make-pathname :directory
                                       (cond
                                         ((and (consp cfo-directory0)
                                               (eq (car cfo-directory0) :absolute))
                                          (list* :relative (cdr cfo-directory0)))
                                         (t cfo-directory0))))
;;;         (_ (format t "cfo-directory: ~s~%" cfo-directory))
         (full-directory (if cfo-directory
                             (merge-pathnames cfo-directory (core:monitor-directory))
                             (core:monitor-directory)))
;;;         (_ (format t "full-directory: ~s~%" full-directory))
         (output-path (make-pathname
                       :name (concatenate
                              'string
                              (pathname-name cfo-pathname)
                              "-" name-suffix)
                       :type "ll"
                       :defaults full-directory)))
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
  (let* ((name-suffix (bformat nil "module-%05d-%s" (core:next-number) file-name-modifier))
         (output-path (make-pathname
                       :name name-suffix
                       :type "ll"
                       :defaults (core:monitor-directory))))
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

(defmacro log-module ((info) &rest body)
  "Wrap quick-module-dump around a block of code so that the module
is dumped to a file before the block and after the block."
  `(progn
     (llvm-sys:sanity-check-module *the-module* 2)
     (quick-module-dump *the-module* ,(bformat nil "%s-begin" info))
     (multiple-value-prog1 (progn ,@body)
       (llvm-sys:sanity-check-module *the-module* 2)
       (quick-module-dump *the-module* ,(bformat nil "%s-end" info)))))


(defun module-report (module)
  (let ((total 0))
    (dolist (func (llvm-sys:module-get-function-list module))
      (let ((num-instructions 0))
        (dolist (bb (llvm-sys:basic-blocks func))
          (dolist (instr (llvm-sys:instructions bb))
            (incf num-instructions)
            (multiple-value-bind (valid lineno column)
                (llvm-sys:get-debug-loc-info instr)
              (if valid
                  (format t "(Line: ~s col: ~s) ~s~%" lineno column instr)
                  (format t "INVALID-DebugLoc ~s~%" instr)))
            (incf total num-instructions)))
        (format t "~a Function ~a~%" num-instructions func)))
    (format t "~a total~%" total)))

