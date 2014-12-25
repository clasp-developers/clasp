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




(defun debug-write-module (module filename)
  (bformat t "!!!!!!!!!!!\n")
  (bformat t "!!!!!!!!!!!\n")
  (bformat t "!!!      WRITING MODULE TO FILE: %s\n" filename)
  (bformat t "!!!!!!!!!!!\n")
  (llvm-sys:write-text-bitcode-to-path module (pathname filename)))


(defvar *irbuilder* nil
  "This is the IRBuilder that defines where all irc-xxx functions that generate IR code put the code.
Set this to other IRBuilders to make code go where you want")


(defvar *irbuilder-ltv-function-alloca* nil
  "Maintains an IRBuilder for the load-time-value function alloca area")
(defvar *irbuilder-ltv-function-body* nil
  "Maintain an IRBuilder for the load-time-value body area")

(defvar *irbuilder-function-alloca* nil
  "Maintains an IRBuilder for function alloca instructions")
(defvar *irbuilder-function-body* nil
  "Maintains an IRBuilder for function body IR code")




;;
;; Create types
;;

(defconstant +float+ (llvm-sys:type-get-float-ty *llvm-context*))
(defconstant +double+ (llvm-sys:type-get-double-ty *llvm-context*))
#+long-float(defconstant +long-float+ (llvm-sys:type-get-long-float-ty *llvm-context*))
(defconstant +void+ (llvm-sys:type-get-void-ty *llvm-context*))
(defconstant +i1+ (llvm-sys:type-get-int1-ty *llvm-context*))
(defconstant +i8+ (llvm-sys:type-get-int8-ty *llvm-context*))
(defconstant +i8*+ (llvm-sys:type-get-pointer-to +i8+))
(defconstant +i8**+ (llvm-sys:type-get-pointer-to +i8*+))
(defconstant +i32+ (llvm-sys:type-get-int32-ty *llvm-context*))
(defconstant +i32*+ (llvm-sys:type-get-pointer-to +i32+))
(defconstant +i32**+ (llvm-sys:type-get-pointer-to +i32*+))
(defconstant +i64+ (llvm-sys:type-get-int64-ty *llvm-context*))
;;(defconstant +exception-struct+ (llvm-sys:struct-type-get *llvm-context* (list +i8*+ +i32+) "exception-struct" nil))
(defconstant +exception-struct+ (llvm-sys:struct-type-get *llvm-context* (list +i8*+ +i32+) nil))

(defconstant +sp-counted-base+ (llvm-sys:struct-type-get *llvm-context* (list +i32+ +i32+) nil)) ;; "sp-counted-base-ty"
(defconstant +sp-counted-base-ptr+ (llvm-sys:type-get-pointer-to +sp-counted-base+))
(defconstant +shared-count+ (llvm-sys:struct-type-get *llvm-context* (list +sp-counted-base-ptr+) nil)) ;; "shared_count"

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

(defconstant +setjmp.buf+ (llvm-sys:struct-type-get *llvm-context* (list +i8*+ +i8*+ +i8*+ +i8*+ +i8*+) nil))
(defconstant +setjmp.buf*+ (llvm-sys:type-get-pointer-to +setjmp.buf+))

;;
;; Setup smart-ptr constants
;;
(multiple-value-bind (pointer-type pointer-px-offset pointer-px-size)
    (smart-pointer-details)
  (defconstant +using-intrusive-reference-count+
    (eq pointer-type 'core::intrusive-reference-counted-pointer))
  (defconstant +smart-ptr-px-offset+ pointer-px-offset))


(defun smart-pointer-fields (data-ptr-type &rest additional-fields)
  "List the types that make up a smart_ptr.
Boehm and MPS use a single pointer"
  (list* data-ptr-type additional-fields))


;;
;; If I use an opaque type then the symbol type gets duplicated and that causes
;; problems - try just using an int
;;(defconstant +sym+ (llvm-sys:struct-type-get *llvm-context* nil nil)) ;; "Symbol_O"
(defconstant +sym+ (llvm-sys:type-get-int32-ty *llvm-context*))
(defconstant +sym-ptr+ (llvm-sys:type-get-pointer-to +sym+))
(defconstant +symsp+ (llvm-sys:struct-type-get *llvm-context* (smart-pointer-fields +sym-ptr+) nil)) ;; "Sym_sp"
(defconstant +symsp*+ (llvm-sys:type-get-pointer-to +symsp+))


;;
;; Store a core::Function_sp pointer
;;
(defconstant +Function+ (llvm-sys:type-get-int32-ty *llvm-context*))
(defconstant +Function-ptr+ (llvm-sys:type-get-pointer-to +Function+))
(defconstant +Function_sp+ (llvm-sys:struct-type-get *llvm-context* (smart-pointer-fields +Function-ptr+) nil)) ;; "Cfn_sp"
(defconstant +Function_sp*+ (llvm-sys:type-get-pointer-to +Function_sp+))



;; Define the T_O struct - right now just put in a dummy i32 - later put real fields here
(defconstant +t+ (llvm-sys:struct-type-get *llvm-context* nil  nil)) ;; "T_O"
(defconstant +t-ptr+ (llvm-sys:type-get-pointer-to +t+))
(defconstant +tsp+ (llvm-sys:struct-type-get *llvm-context* (smart-pointer-fields +t-ptr+) nil))  ;; "T_sp"
(defconstant +tsp[0]+ (llvm-sys:array-type-get +tsp+ 0))
(defconstant +tsp[0]*+ (llvm-sys:type-get-pointer-to +tsp[0]+))
(defconstant +tsp*+ (llvm-sys:type-get-pointer-to +tsp+))
(defconstant +tsp**+ (llvm-sys:type-get-pointer-to +tsp*+))


(defconstant +tmv+ (llvm-sys:struct-type-get *llvm-context* (smart-pointer-fields +t-ptr+ +i32+) nil))  ;; "T_mv"
(defconstant +tmv*+ (llvm-sys:type-get-pointer-to +tmv+))
(defconstant +tmv**+ (llvm-sys:type-get-pointer-to +tmv*+))


;; Define the LoadTimeValue_O struct - right now just put in a dummy i32 - later put real fields here
(defconstant +ltv+ (llvm-sys:struct-type-get *llvm-context* nil  nil)) ;; "LoadTimeValue_O"
(defconstant +ltv*+ (llvm-sys:type-get-pointer-to +ltv+))
(defconstant +ltv**+ (llvm-sys:type-get-pointer-to +ltv*+))
(defconstant +ltvsp+ (llvm-sys:struct-type-get *llvm-context* (smart-pointer-fields +ltv*+) nil))  ;; "LoadTimeValue_sp"
(defconstant +ltvsp*+ (llvm-sys:type-get-pointer-to +ltvsp+))
(defconstant +ltvsp**+ (llvm-sys:type-get-pointer-to +ltvsp*+))


#+(or)(progn
        (defconstant +af+ (llvm-sys:struct-type-get *llvm-context* nil  nil)) ;; "ActivationFrame_O"
        (defconstant +af-ptr+ (llvm-sys:type-get-pointer-to +af+))
        (defconstant +afsp+ (llvm-sys:struct-type-get *llvm-context* (smart-pointer-fields +af-ptr+)  nil)) ;; "ActivationFrame_sp"
        (defconstant +afsp*+ (llvm-sys:type-get-pointer-to +afsp+))
        )

;; Substitute afsp* with tsp
(defconstant +afsp+ +tsp+)
(defconstant +afsp*+ +tsp*+)



(defconstant +va-list+ +i8*+)
(defconstant +closure*+ +i8*+)

;;
;; Set up the calling convention using core:+number-of-fixed-arguments+ to define the types
;; and names of the arguments passed in registers
;;

(defvar *register-arg-types* nil)
(defvar *register-arg-names* nil)
(let (arg-types arg-names)
  (dotimes (i core:+number-of-fixed-arguments+)
    (push +t-ptr+ arg-types)
    (push (bformat nil "farg%d" i) arg-names))
  (setf *register-arg-types* (nreverse arg-types)
	*register-arg-names* (nreverse arg-names)))
(defconstant +fn-registers-prototype-argument-names+ (list* "result-ptr" "closed-af-ptr" "nargs" *register-arg-names*))
(defconstant +fn-registers-prototype+ (llvm-sys:function-type-get +void+ (list* +tmv*+ +t-ptr+ +i32+ *register-arg-types*))
  "The general function prototypes pass the following pass:
1) An sret pointer for where to put the result
2) A closed over runtime environment (linked list of activation frames)
3) The number of arguments +i32+
4) core::+number-of-fixed-arguments+ T_O* pointers, the first arguments passed in registers,
      If no argument is passed then pass NULL.
5) If additional arguments are needed then they must be put in the multiple-values array on the stack")

(progn
  (defconstant +fn-prototype+ +fn-registers-prototype+)
  (defconstant +fn-prototype-argument-names+ +fn-registers-prototype-argument-names+))


(defconstant +fn-prototype*+ (llvm-sys:type-get-pointer-to +fn-prototype+)
  "A pointer to the function prototype")

(defconstant +fn-void+ (llvm-sys:function-type-get +void+ nil))
(defconstant +fn-void-ptr+ (llvm-sys:type-get-pointer-to +fn-void+))
(defconstant +fn-void-ptr-array0+ (llvm-sys:array-type-get +fn-void-ptr+ 0))
(defconstant +fn-void-ptr-array0*+ (llvm-sys:type-get-pointer-to +fn-void-ptr-array0+))
(defconstant +fn-void-ptr-array1+ (llvm-sys:array-type-get +fn-void-ptr+ 1))
(defconstant +fn-void-ptr-array1*+ (llvm-sys:type-get-pointer-to +fn-void-ptr-array1+))
(defconstant +fn-void-ptr-pointer+ (llvm-sys:pointer-type-get +fn-void-ptr+ 0))
(defconstant +fn-void-ptr-pointer*+ (llvm-sys:type-get-pointer-to +fn-void-ptr-pointer+))

;;
;; Define the InvocationHistoryFrame type for LispCompiledFunctionIHF
;;
;; %"class.core::InvocationHistoryFrame" = type { i32 (...)**, i32, %"class.core::InvocationHistoryStack"*, %"class.core::InvocationHistoryFrame"*, i8, i32 }
(defconstant +InvocationHistoryStack*+ +i8*+ "Make this a generic pointer")
(defconstant +InvocationHistoryFrame+ (llvm-sys:struct-type-create *llvm-context* :name "InvocationHistoryFrame"))
(defconstant +InvocationHistoryFrame*+ (llvm-sys:type-get-pointer-to +InvocationHistoryFrame+))
(llvm-sys:set-body +InvocationHistoryFrame+ (list +i32**+ +i32+ +InvocationHistoryStack*+ +InvocationHistoryFrame*+ +i8+ +i32+) nil)
;; %"class.core::LispFunctionIHF" = type { %"class.core::InvocationHistoryFrame", %"class.mem::smart_ptr.51", %"class.mem::smart_ptr.51", %"class.mem::smart_ptr.51", i32, i32 }
(defconstant +LispFunctionIHF+ (llvm-sys:struct-type-create *llvm-context* :elements (list +InvocationHistoryFrame+ +tsp+ +tsp+ +tsp+ +i32+ +i32+) :name "LispFunctionIHF"))
;; %"class.core::LispCompiledFunctionIHF" = type { %"class.core::LispFunctionIHF" }
(defconstant +LispCompiledFunctionIHF+ (llvm-sys:struct-type-create *llvm-context* :elements (list +LispFunctionIHF+) :name "LispCompiledFunctionIHF"))





;;
;; Ensure that the LLVM model of
;;   tsp matches shared_ptr<xxx> and
;;   tmv matches multiple_values<xxx>
;;
(let ((tsp-size (llvm-sys:data-layout-get-type-alloc-size *data-layout* +tsp+))
      (tmv-size (llvm-sys:data-layout-get-type-alloc-size *data-layout* +tmv+)))
  (llvm-sys:throw-if-mismatched-structure-sizes :tsp tsp-size :tmv tmv-size))


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





(defun create-primitive-function (module name return-ty args-ty varargs does-not-throw does-not-return)
  (let ((fn (llvm-sys:function-create (llvm-sys:function-type-get return-ty args-ty varargs)
				      'llvm-sys::External-linkage
				      name
				      module)))
    (when does-not-throw (llvm-sys:set-does-not-throw fn))
    (when does-not-return (llvm-sys:set-does-not-return fn))))


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


(defun primitive-does-not-throw (module name return-ty args-ty &key varargs does-not-return)
  (primitive module name return-ty args-ty :varargs varargs :does-not-throw t :does-not-return does-not-return))

(defun define-primitives-in-module (module)


;  (primitive module "lccGlobalFunction" +lisp-calling-convention-ptr+ (list +symsp+))

  (primitive-does-not-throw module "newFunction_sp" +void+ (list +Function_sp*+))
  (primitive-does-not-throw module "destructFunction_sp" +void+ (list +Function_sp*+))
  (primitive-does-not-throw module "newTsp" +void+ (list +tsp*+))
  (primitive-does-not-throw module "resetTsp" +void+ (list +tsp*+))
  (primitive-does-not-throw module "makeUnboundTsp" +void+ (list +tsp*+))
  (primitive-does-not-throw module "copyTsp" +void+ (list +tsp*-or-tmv*+ +tsp*+))
  (primitive-does-not-throw module "destructTsp" +void+ (list +tsp*+))
  (primitive-does-not-throw module "compareTsp" +i32+ (list +tsp*+ +tsp*+))

  (primitive-does-not-throw module "newTmv" +void+ (list +tmv*+))
  (primitive-does-not-throw module "resetTmv" +void+ (list +tmv*+))
  (primitive-does-not-throw module "copyTmv" +void+ (list +tmv*+ +tmv*+))
  (primitive-does-not-throw module "copyTmvOrSlice" +void+ (list +tsp*-or-tmv*+ +tmv*+))
  (primitive-does-not-throw module "destructTmv" +void+ (list +tmv*+))

  (primitive-does-not-throw module "newAFsp" +void+ (list +afsp*+))
  (primitive-does-not-throw module "newAFsp_ValueFrameOfSize" +void+ (list +afsp*+ +i32+))
  (primitive-does-not-throw module "resetAFsp" +void+ (list +afsp*+))
  (primitive-does-not-throw module "copyAFsp" +void+ (list +afsp*+ +afsp*+))
  (primitive-does-not-throw module "destructAFsp" +void+ (list +afsp*+))

  (primitive-does-not-throw module "getMultipleValues" +tsp[0]*+ (list +i32+))

  (primitive-does-not-throw module "isNilTsp" +i32+ (list +tsp*+))
  (primitive-does-not-throw module "isTrue" +i32+ (list +tsp*+))
  (primitive-does-not-throw module "isBound" +i32+ (list +tsp*+))


  (primitive-does-not-throw module "internSymbol_tsp" +void+ (list +tsp*+ +i8*+ +i8*+))
  (primitive-does-not-throw module "makeSymbol_tsp" +void+ (list +tsp*+ +i8*+))

  (primitive-does-not-throw module "internSymbol_symsp" +void+ (list +symsp*+ +i8*+ +i8*+))
  (primitive-does-not-throw module "makeSymbol_symsp" +void+ (list +symsp*+ +i8*+))

  (primitive-does-not-throw module "makeNil" +void+ (list +tsp*-or-tmv*+))
  (primitive-does-not-throw module "makeT" +void+ (list +tsp*+))
  (primitive-does-not-throw module "makeCons" +void+ (list +tsp*+ +tsp*+ +tsp*+))
  (primitive-does-not-throw module "makeFixnum" +void+ (list +tsp*+ +i32+))
  (primitive-does-not-throw module "makeCharacter" +void+ (list +tsp*+ +i32+))
  (primitive-does-not-throw module "makeBignum" +void+ (list +tsp*+ +i8*+))
  #+short-float (primitive-does-not-throw module "makeShortFloat" +void+ (list +tsp*+ +double+))
  (primitive-does-not-throw module "makeSingleFloat" +void+ (list +tsp*+ +float+))
  (primitive-does-not-throw module "makeDoubleFloat" +void+ (list +tsp*+ +double+))
  (primitive-does-not-throw module "makeComplex" +void+ (list +tsp*+ +double+ +double+))
 #+long-float (primitive-does-not-throw module "makeLongFloat" +void+ (list +tsp*+ +long-float+))
  (primitive-does-not-throw module "makeString" +void+ (list +tsp*+ +i8*+))
  (primitive-does-not-throw module "makePathname" +void+ (list +tsp*+ +i8*+))
  (primitive-does-not-throw module "findPackage" +void+ (list +tsp*+ +i8*+))
  (primitive module "makeCompiledFunction" +void+ (list +tsp*-or-tmv*+ +fn-prototype*+ +i8*+ +i64+ +i32+ +i32+ +tsp*+ +tsp*+ +afsp*+ +tsp*+))


  (primitive module "fillRestTarget" +void+ (list +tsp*+ +afsp*+ +i32+ +i8*+))

  (primitive-does-not-throw module "symbolValueRead" +void+ (list +tsp*-or-tmv*+ +symsp*+))
  (primitive-does-not-throw module "symbolValueReadOrUnbound" +void+ (list +tsp*-or-tmv*+ +symsp*+))
  (primitive-does-not-throw module "symbolValueReference" +tsp*+ (list +symsp*+))
  (primitive-does-not-throw module "lexicalValueReference" +tsp*+ (list +i32+ +i32+ +afsp*+))
  (primitive-does-not-throw module "lexicalValueRead" +void+ (list +tsp*-or-tmv*+ +i32+ +i32+ +afsp*+))
  (primitive-does-not-throw module "symbolFunctionRead" +void+ (list +tsp*-or-tmv*+ +symsp*+))
  (primitive-does-not-throw module "setfSymbolFunctionRead" +void+ (list +tsp*+ +symsp*+))
  (primitive-does-not-throw module "lexicalFunctionRead" +void+ (list +tsp*-or-tmv*+ +i32+ +i32+ +afsp*+))


  (primitive-does-not-throw module "makeTagbodyFrame" +void+ (list +afsp*+))
  (primitive-does-not-throw module "makeValueFrame" +void+ (list +afsp*+ +i32+ +i32+))
  (primitive-does-not-throw module "makeValueFrameFromReversedCons" +void+ (list +afsp*+ +tsp*+ +i32+ ))
  (primitive-does-not-throw module "setParentOfActivationFrameTPtr" +void+ (list +tsp*+ +t-ptr+))
  (primitive-does-not-throw module "setParentOfActivationFrame" +void+ (list +tsp*+ +tsp*+))

  (primitive-does-not-throw module "attachDebuggingInfoToValueFrame" +void+ (list +afsp*+ +tsp*+))

  (primitive-does-not-throw module "valueFrameReference" +tsp*+ (list +afsp*+ +i32+))

  (primitive module "makeFunctionFrame" +void+ (list +afsp*+ +i32+ +afsp*+))
  (primitive module "functionFrameReference" +tsp*+ (list +afsp*+ +i32+))

  (primitive module "prependMultipleValues" +void+ (list +tsp*-or-tmv*+ +tmv*+))

;;  (primitive module "symbolFunction" +void+ (list +Function_sp*+ +tsp*+))
;;  (primitive module "lexicalFunction" +void+ (list +Function_sp*+ +i32+ +i32+ +afsp*+))


;;  (primitive module "invokePossibleMultipleValueFunction" +void+ (list +tmv*+ +tsp*+ +afsp*+))
;;  (primitive module "invokePossibleMultipleValueSymbolFunction" +void+ (list +tsp*-or-tmv*+ +symsp*+ +afsp*+))
;;  (primitive module "invokePossibleMultipleValueLexicalFunction" +void+ (list +tsp*-or-tmv*+ +i32+ +i32+ +afsp*+ +afsp*+))

  (primitive module "invokeMainFunctions" +void+ (list +fn-void-ptr-pointer+ +i32*+))
  (primitive module "invokeTopLevelFunction" +void+ (list +tmv*+ +fn-prototype*+ +afsp*+ +i8*+ +i32*+ +i64+ +i32+ +i32+))
  (primitive module "invokeLlvmFunctionVoid" +void+ (list +fn-void-ptr+))

  (primitive module "invokeFASLLlvmFunctionVoid" +void+ (list +fn-void-ptr+ +i8*+))

;;  (primitive-does-not-throw module "activationFrameNil" +afsp*+ nil)
  (primitive-does-not-throw module "activationFrameSize" +i32+ (list +afsp*+))
;;  (primitive-does-not-throw module "activationFrameParentRef" +tsp*+ (list +afsp*+))

  (primitive-does-not-throw module "copyArgs" +void+ (list +tsp*+ +i32+ +t-ptr+ +t-ptr+ +t-ptr+ +i8*+))
  (primitive module "throwTooManyArgumentsException" +void+ (list +i8*+ +afsp*+ +i32+ +i32+))
  (primitive module "throwNotEnoughArgumentsException" +void+ (list +i8*+ +afsp*+ +i32+ +i32+))
  (primitive module "throwIfExcessKeywordArguments" +void+ (list +i8*+ +afsp*+ +i32+))
  (primitive-does-not-throw module "kw_allowOtherKeywords" +i32+ (list +i32+ +afsp*+ +i32+))
  (primitive-does-not-throw module "kw_trackFirstUnexpectedKeyword" +i32+ (list +i32+ +i32+))
  (primitive module "kw_throwIfBadKeywordArgument" +void+ (list +i32+ +i32+ +afsp*+))
  (primitive module "kw_throwIfNotKeyword" +void+ (list +tsp*+))

  (primitive-does-not-throw module "gdb" +void+ nil)
  (primitive-does-not-throw module "debugInvoke" +void+ nil)
  (primitive-does-not-throw module "debugInspectActivationFrame" +void+ (list +afsp*+))
  (primitive-does-not-throw module "debugInspectT_sp" +void+ (list +tsp*+))
  (primitive-does-not-throw module "debugInspectTPtr" +void+ (list +t-ptr+))
  (primitive-does-not-throw module "debugInspectT_mv" +void+ (list +tmv*+))

  (primitive-does-not-throw module "debugPointer" +void+ (list +i8*+))
  (primitive-does-not-throw module "debugPrintObject" +void+ (list +i8*+ +tsp*+))
  (primitive-does-not-throw module "debugPrintI32" +void+ (list +i32+))
  (primitive-does-not-throw module "lowLevelTrace" +void+ (list +i32+))
  (primitive-does-not-throw module "unreachableError" +void+ nil)

  (primitive-does-not-throw module "singleStepCallback" +void+ nil)


;;  (primitive-does-not-throw module "va_vararg" +tagged-ptr+ (list +va-list+))
;;  (primitive-does-not-throw module "generateTspFromTAGGED_PTR" +void+ (list +tsp*+ +tagged-ptr+))
  (primitive module "va_throwTooManyArgumentsException" +void+ (list +i8*+ +i32+ +i32+))
  (primitive module "va_throwNotEnoughArgumentsException" +void+ (list +i8*+ +i32+ +i32+))
  (primitive module "va_throwIfExcessKeywordArguments" +void+ (list +i8*+ +i32+ +tsp[0]*+ +i32+))
  (primitive module "va_fillActivationFrameWithRequiredVarargs" +void+ (list +afsp*+ +i32+ +tsp*+))
  (primitive module "va_coerceToClosure" +closure*+ (list +tsp*+))
  (primitive module "va_symbolFunction" +closure*+ (list +symsp*+))  ;; void va_symbolFunction(core::Function_sp fn, core::Symbol_sp sym)
  (primitive module "va_lexicalFunction" +closure*+ (list +i32+ +i32+ +afsp*+))
  (primitive module "FUNCALL" +void+ (list* +tsp*-or-tmv*+ +closure*+ +i32+ (map 'list (lambda (x) x) (make-array core:+number-of-fixed-arguments+ :initial-element +t-ptr+))))
  (primitive module "FUNCALL_activationFrame" +void+ (list +tsp*-or-tmv*+ +closure*+ +afsp*+))


  (primitive module "va_fillRestTarget" +void+ (list +tsp*+ +i32+ +tsp[0]*+ +i32+ +i8*+))
  (primitive-does-not-throw module "va_allowOtherKeywords" +i32+ (list +i32+ +i32+ +tsp[0]*+ +i32+))
  (primitive module "va_throwIfBadKeywordArgument" +void+ (list +i32+ +i32+ +i32+ +tsp[0]*+))


  (primitive-does-not-throw module "trace_setActivationFrameForIHSTop" +void+ (list +afsp*+))
  (primitive-does-not-throw module "trace_setLineNumberColumnForIHSTop" +void+ (list +i8*+ +i32*+ +i64+ +i32+ +i32+))

  (primitive-does-not-throw module "trace_exitFunctionScope" +void+ (list +i32+) )
  (primitive-does-not-throw module "trace_exitBlockScope" +void+ (list +i32+ ) )
  (primitive-does-not-throw module "trace_exitLetScope" +void+ (list +i32+ ) )
  (primitive-does-not-throw module "trace_exitLetSTARScope" +void+ (list +i32+ ) )
  (primitive-does-not-throw module "trace_exitFletScope" +void+ (list +i32+ ) )
  (primitive-does-not-throw module "trace_exitLabelsScope" +void+ (list +i32+ ) )
  (primitive-does-not-throw module "trace_exitCallScope" +void+ (list +i32+ ) )
  (primitive-does-not-throw module "trace_exitCatchScope" +void+ (list +i32+ ) )
  (primitive-does-not-throw module "trace_exitUnwindProtectScope" +void+ (list +i32+ ) )

  (primitive-does-not-throw module "pushCatchFrame" +i32+ (list +tsp*+))
  (primitive-does-not-throw module "pushBlockFrame" +i32+ (list +symsp*+))
  (primitive-does-not-throw module "pushTagbodyFrame" +i32+ (list +tsp*+))

  (primitive module "throwCatchThrow" +void+ (list +tsp*+ #| +tmv*+ |#) :does-not-return t)
  (primitive module "throwReturnFrom" +void+ (list +symsp*+) :does-not-return t)
  (primitive module "throwDynamicGo" +void+ (list +i32+ +i32+ +afsp*+) :does-not-return t)

  (primitive module "ifCatchFrameMatchesStoreResultElseRethrow" +void+ (list +tsp*-or-tmv*+ +i32+ +i8*+))
  (primitive-does-not-throw module "exceptionStackUnwind" +void+ (list +i32+))


  (primitive module "blockHandleReturnFrom" +void+ (list +tsp*-or-tmv*+ +i8*+ +i32+))

  (primitive module "tagbodyDynamicGoIndexElseRethrow" +i32+ (list +i8*+ +i32+))

  (primitive module "throwIllegalSwitchValue" +void+ (list +i32+ +i32+) :does-not-return t)

  (primitive-does-not-throw module "clasp_terminate" +void+ (list +i8*+ +i32+ +i32+ +i8*+) )
  (primitive-does-not-throw module "__gxx_personality_v0" +i32+ nil :varargs t) ;; varargs
  (primitive-does-not-throw module "__cxa_begin_catch" +i8*+ (list +i8*+) )
  (primitive module "__cxa_end_catch" +void+ nil)
  (primitive module "__cxa_rethrow" +void+ nil)
  (primitive-does-not-throw module "llvm.eh.typeid.for" +i32+ (list +i8*+))
;;  (primitive-does-not-throw module "_Unwind_Resume" +void+ (list +i8*+))

  (primitive-does-not-throw module "getOrCreateLoadTimeValueArray" +void+ (list +ltv**+ +i8*+ +i32+ +i32+))

  (primitive-does-not-throw module "copyLoadTimeValue" +void+ (list +tsp*-or-tmv*+ +ltv**+ +i32+))

  (primitive-does-not-throw module "loadTimeValueReference" +tsp*+ (list +ltv**+ +i32+))
  (primitive-does-not-throw module "loadTimeSymbolReference" +symsp*+ (list +ltv**+ +i32+))
  (primitive-does-not-throw module "getLoadTimeValue" +void+ (list +tsp*-or-tmv*+ +ltv**+ +i32+))
  (primitive-does-not-throw module "dumpLoadTimeValues" +void+ (list +ltv**+))

  (primitive-does-not-throw module "assignSourceFileInfoHandle" +void+ (list +i8*+ +i8*+ +i64+ +i32+ +i32*+))
  (primitive-does-not-throw module "debugSourceFileInfoHandle" +void+ (list +i32*+))

  (primitive-does-not-throw module "ltv_makeCons" +void+ (list +tsp*+))
  (primitive-does-not-throw module "ltv_makeSourceCodeCons" +void+ (list +tsp*+ +i8*+ +i32+ +i32+))
  (primitive-does-not-throw module "ltv_makeArrayObjects" +void+ (list +tsp*+ +tsp*+ +i32+ +i32*+))
  (primitive-does-not-throw module "ltv_makeHashTable" +void+ (list +tsp*+ +tsp*+))
  (primitive-does-not-throw module "rplaca" +void+ (list +tsp*+ +tsp*+))
  (primitive-does-not-throw module "rplacd" +void+ (list +tsp*+ +tsp*+))
  (primitive-does-not-throw module "ltv_initializeArrayObjectsRowMajorArefOrder" +void+ (list +tsp*+ +ltv**+ +i32*+))
  (primitive-does-not-throw module "ltv_initializeHashTable" +void+ (list +tsp*+ +i32+ +ltv**+ +i32*+))

  (primitive-does-not-throw module "saveToMultipleValue0" +void+ (list +tmv*+))
  (primitive-does-not-throw module "saveValues" +void+ (list +tsp*+ +tmv*+))
  (primitive-does-not-throw module "loadValues" +void+ (list +tmv*+ +tsp*+))

  (primitive-does-not-throw module "setjmp_set_jump_address" +void+ (list +setjmp.buf*+ +i8*+) )

  (primitive-does-not-throw module "setjmp_user0_set_i32" +void+ (list +setjmp.buf*+ +i32+) )
  (primitive-does-not-throw module "setjmp_user0_get_i32" +i32+ (list +setjmp.buf*+) )

  (primitive-does-not-throw module "setjmp_user0_allocate_set_tmv" +void+ (list +setjmp.buf*+ +tmv*+) )
  (primitive-does-not-throw module "setjmp_user0_get_tmv" +void+ (list +tmv*+ +setjmp.buf*+) )
  (primitive-does-not-throw module "setjmp_user0_delete_tmv" +void+ (list +setjmp.buf*+) )

  (primitive-does-not-throw module "llvm.eh.sjlj.setjmp" +i32+ (list +i8*+) )
  (primitive-does-not-throw module "llvm.eh.sjlj.longjmp" +void+ (list +i8*+) )

  (primitive-does-not-throw module "progvSaveSpecials" +void+ (list +i8**+ +tsp*+ +tsp*+))
  (primitive-does-not-throw module "progvRestoreSpecials" +void+ (list +i8**+))

  (primitive-does-not-throw module "pushDynamicBinding" +void+ (list +symsp*+))
  (primitive-does-not-throw module "popDynamicBinding" +void+ (list +symsp*+))

  (primitive-does-not-throw module "matchKeywordOnce" +i32+ (list +tsp*+ +tsp*+ +i8*+))


  )




;;------------------------------------------------------------
;;
;; Setup dynamic variables
;;
;;



(defvar *compile-file-pathname* nil "Store the path-name of the currently compiled file")
(defvar *compile-file-truename* nil "Store the truename of the currently compiled file")
(defvar *compile-file-source-file-info* nil "Store the SourceFileInfo object for the compile-file target")


(defvar *gv-source-pathname* nil
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
