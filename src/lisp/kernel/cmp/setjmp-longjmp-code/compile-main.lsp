

(in-package :cmp)

#|
(defvar *cmp-env* nil
"The compiler environment consists of a pair or cons of two
lists, one containing variable records, the other one macro and
function recors:

variable-record = (:block block-name) |
                  (:tag ({tag-name}*)) |
                  (:function function-name) |
                  (var-name {:special | nil} bound-p) |
                  (symbol si::symbol-macro macro-function) |
                  CB | LB | UNWIND-PROTECT
macro-record =	(function-name function) |
                (macro-name si::macro macro-function)
                CB | LB | UNWIND-PROTECT

A *-NAME is a symbol. A TAG-ID is either a symbol or a number. A
MACRO-FUNCTION is a function that provides us with the expansion
for that local macro or symbol macro. BOUND-P is true when the
variable has been bound by an enclosing form, while it is NIL if
the variable-record corresponds just to a special declaration.
CB, LB and UNWIND-PROTECT are only used by the C compiler and
they denote closure, lexical environment and unwind-protect
boundaries. Note that compared with the bytecodes compiler, these
records contain an additional variable, block, tag or function
object at the end.")

(defvar *cmp-env-root*
  (cons nil (list (list '#:no-macro 'si::macro (constantly nil))))


"This is the common environment shared by all toplevel forms. It can
only be altered by DECLAIM forms and it is used to initialize the
value of *CMP-ENV*.")
|#

;; Store the repl-functions 
(defparameter *repl-functions* :illegal)





(in-package :cmp)

;;(require 'llvm-ir)




#|
(defmacro cmp-log (fmt &rest args ) nil)
(defmacro cmp-log-dump (fn) nil)
(defmacro debug-print-i32 (num) nil)
|#



(defun is-debug-compiler-on ()
  *debug-compiler*)

(defmacro debug-print-i32 (num)
  `(if (is-debug-compiler-on)
       (irc-call env "debugPrintI32" (jit-constant-i32 ,num))
       nil))


(defmacro cmp-log (fmt &rest args)
  `(if (is-debug-compiler-on)
       (progn
	 (bformat t "%s:%s " (source-file-name) (source-line-column))
	 (bformat t ,fmt ,@args))
       nil))

(defmacro cmp-log-dump (fn-or-module)
  `(if (is-debug-compiler-on)
       (llvm-sys:dump ,fn-or-module)
      nil))




(defun debug-write-module (module filename)
  (bformat t "!!!!!!!!!!!\n")
  (bformat t "!!!!!!!!!!!\n")
  (bformat t "!!!      WRITING MODULE TO FILE: %s\n" filename)
  (bformat t "!!!!!!!!!!!\n")
  (llvm-sys:write-text-bitcode-to-path module (make-path filename)))


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



(defmacro with-compiler-env ( (&rest options) &rest body )
  "Initialize the environment to protect nested compilations from each other"
  `(let ((*the-module* nil)
	 (*irbuilder-ltv-function-alloca* nil)
	 (*irbuilder-ltv-function-body* nil)
	 (*irbuilder-function-alloca* nil)
	 (*irbuilder-function-body* nil)
	 (*generate-compile-file-load-time-values* nil)
	 (*next-load-time-value-index* nil)
	 (*load-time-value-holder-global-var* nil)
	 (*load-time-value-coalesce* nil)
	 (*load-time-initializer-environment* nil)
	 (*the-module-dibuilder* nil)
	 (*readtable* *readtable*)
	 (*package* *package*)
	 )
     ,@body
     ))
     


;;
;; Create types
;;

(defconstant +double+ (llvm-sys:type-get-double-ty *llvm-context*))
(defconstant +void+ (llvm-sys:type-get-void-ty *llvm-context*))
(defconstant +i1+ (llvm-sys:type-get-int1-ty *llvm-context*))
(defconstant +i8+ (llvm-sys:type-get-int8-ty *llvm-context*))
(defconstant +i8*+ (llvm-sys:type-get-pointer-to +i8+))
(defconstant +i8**+ (llvm-sys:type-get-pointer-to +i8*+))
(defconstant +i32+ (llvm-sys:type-get-int32-ty *llvm-context*))
(defconstant +i64+ (llvm-sys:type-get-int64-ty *llvm-context*))
;;(defconstant +exception-struct+ (llvm-sys:struct-type-get *llvm-context* (list +i8*+ +i32+) "exception-struct" nil))
(defconstant +exception-struct+ (llvm-sys:struct-type-get *llvm-context* (list +i8*+ +i32+) nil))
(defconstant +i32*+ (llvm-sys:type-get-pointer-to +i32+))
(defconstant +i32**+ (llvm-sys:type-get-pointer-to +i32*+))

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
I'm using smart_ptr which have a vtable associated with them"
  (if +using-intrusive-reference-count+
      (list* +i32*+    ;; vtable ptr
	     data-ptr-type ;; data ptr
	     additional-fields)
      (list* +i32*+    ;; vtable ptr
	     data-ptr-type ;; data ptr
	     +shared-count+ ;; reference counter
	     additional-fields))
      )


;;
;; If I use an opaque type then the symbol type gets duplicated and that causes
;; problems - try just using an int
;;(defconstant +sym+ (llvm-sys:struct-type-get *llvm-context* nil nil)) ;; "Symbol_O" 
(defconstant +sym+ (llvm-sys:type-get-int32-ty *llvm-context*))
(defconstant +sym-ptr+ (llvm-sys:type-get-pointer-to +sym+))
(defconstant +symsp+ (llvm-sys:struct-type-get *llvm-context* (smart-pointer-fields +sym-ptr+) nil)) ;; "Sym_sp" 
(defconstant +symsp*+ (llvm-sys:type-get-pointer-to +symsp+))



;; Define the T_O struct - right now just put in a dummy i32 - later put real fields here
(defconstant +t+ (llvm-sys:struct-type-get *llvm-context* nil  nil)) ;; "T_O"
(defconstant +t-ptr+ (llvm-sys:type-get-pointer-to +t+))
(defconstant +tsp+ (llvm-sys:struct-type-get *llvm-context* (smart-pointer-fields +t-ptr+) nil))  ;; "T_sp" 
(defconstant +tsp*+ (llvm-sys:type-get-pointer-to +tsp+))
(defconstant +tsp**+ (llvm-sys:type-get-pointer-to +tsp*+))


(defconstant +tmv+ (llvm-sys:struct-type-get *llvm-context* (smart-pointer-fields +t-ptr+ +i32+) nil))  ;; "T_mv" 
(defconstant +tmv*+ (llvm-sys:type-get-pointer-to +tmv+))
(defconstant +tmv**+ (llvm-sys:type-get-pointer-to +tmv*+))


;; Define the VectorObjectsWithFillPtr_O struct - right now just put in a dummy i32 - later put real fields here
(defconstant +ltv+ (llvm-sys:struct-type-get *llvm-context* nil  nil)) ;; "VectorObjectsWithFillPtr_O"
(defconstant +ltv-ptr+ (llvm-sys:type-get-pointer-to +ltv+))
(defconstant +ltvsp+ (llvm-sys:struct-type-get *llvm-context* (smart-pointer-fields +ltv-ptr+) nil))  ;; "T_sp" 
(defconstant +ltvsp*+ (llvm-sys:type-get-pointer-to +ltvsp+))
(defconstant +ltvsp**+ (llvm-sys:type-get-pointer-to +ltvsp*+))



(defconstant +af+ (llvm-sys:struct-type-get *llvm-context* nil  nil)) ;; "ActivationFrame_O"
(defconstant +af-ptr+ (llvm-sys:type-get-pointer-to +af+))
(defconstant +afsp+ (llvm-sys:struct-type-get *llvm-context* (smart-pointer-fields +af-ptr+)  nil)) ;; "ActivationFrame_sp"
(defconstant +afsp*+ (llvm-sys:type-get-pointer-to +afsp+))



#|
;; Define the ActivationFrame struct
(defconstant +activation-frame-type+ (llvm-sys:type-get-int32-ty *llvm-context*))
;; fill in more stuff here
(defconstant +activation-frame+ (llvm-sys:struct-type-create *llvm-context* (list +activation-frame-type+) "ActivationFrame" nil))
(defconstant +activation-frame*+ (llvm-sys:type-get-pointer-to +activation-frame+))
;; 


;; Function that is passed a pointer to a T_sp for the result and an activation frame
(defconstant +fn-tsp*-activation-frame*+ (llvm-sys:function-type-get +void+ (list +tsp*+ +activation-frame*+)))
|#

(defconstant +fn-tmv*-afsp*+ (llvm-sys:function-type-get +void+ (list +tmv*+ +afsp*+)))
(defconstant +fn*+ (llvm-sys:type-get-pointer-to +fn-tmv*-afsp*+))


;;
;; Ensure that the LLVM model of
;;   tsp matches shared_ptr<xxx> and
;;   tmv matches multiple_values<xxx>
;;
(let ((data-layout (llvm-sys:get-data-layout *run-time-execution-engine*)))
  (llvm-sys:throw-if-mismatched-tsp-tmv-sizes (llvm-sys:data-layout-get-type-alloc-size data-layout +tsp+)
					      (llvm-sys:data-layout-get-type-alloc-size data-layout +tmv+)))




;; 
;; Define exception types in the module
;;
;; TODO:  Holy crap! We are using hard-coded mangled clang names for the typeinfo records for each
;;        exception type!  Find some portable way to do this!!!!!!!!
;; Currently these names are scraped from the libCore library using "nm"

(defvar *exceptions*
  '(
    (typeid-core-catch-throw "_ZTIN4core10CatchThrowE")
    (typeid-core-go          "_ZTIN4core2GoE")
    (typeid-core-return-from "_ZTIN4core10ReturnFromE")
    ))

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
  (if (eq required-type +tsp*-or-tmv*+)
      (if (eql arg-index 1)
	  (if (or (eq given-type +tsp*+) (eq given-type +tmv*+))
	      t
	      nil)
	  (error ":tsp*-or-tmv* can only be specified as the first argument of an intrinsic function"))
      (eq required-type given-type)))

(defun assert-result-isa-llvm-sys-value (result)
  (unless (llvm-sys-value-p result)
      (error "result must be an instance of llvm-sys:Value_O but instead it has the value %s" result)))

(defun throw-if-mismatched-arguments (fn-name args)
  (let* ((info (gethash fn-name *primitives*))
	 (return-ty (car info))
	 (required-args-ty (cadr info))
	 (passed-args-ty (mapcar #'(lambda (x)
				     (if (llvm-sys-value-p x)
					 (llvm-sys:get-type x)
					 (class-name x)))
				 args))
	 (i 1))
    (mapc #'(lambda (x y z)
	      (unless (matching-arguments x y i)
		(error "Calling %s - mismatch of arg#%d value[%s], expected type %s received type %s" fn-name i z x y))
	      (setq i (1+ i))
	      ) required-args-ty passed-args-ty args)))
    
(defconstant +tsp*-or-tmv*+ :tsp*-or-tmv*
  "This is a stand-in for a first argument type that can either be tsp* or tmv*")

(defun dispatch-function-name (name &optional required-first-argument-type)
  (let ((name-dispatch-prefix
	 (cond
	   ((eq required-first-argument-type +tsp*+)
	    "sp_")
	   ((eq required-first-argument-type +tmv*+)
	    "mv_")
	   (t
	    ""))))
    (bformat nil "%s%s" name-dispatch-prefix name)))




      
(defun create-primitive-function (module name return-ty args-ty var-args does-not-throw does-not-return)
  (let ((fn (llvm-sys:function-create (llvm-sys:function-type-get return-ty args-ty var-args)
				      'llvm-sys::External-linkage
				      name
				      module)))
    (when does-not-throw (llvm-sys:set-does-not-throw fn))
    (when does-not-return (llvm-sys:set-does-not-return fn))))


(defun primitive (module name return-ty args-ty &key var-args does-not-throw does-not-return )
  (mapc #'(lambda (x)
	    (when (eq +tsp*-or-tmv*+ x)
	      (error "When defining primitive ~a --> :tsp*-or-tmv* is only allowed in the first argument position" name ))) (cdr args-ty))
  (if (eq (car args-ty) +tsp*-or-tmv*+)
      (progn ;; create two versions of the function - one prefixed with sp_ and the other with mv_
	(create-primitive-function module
				   (dispatch-function-name name +tsp*+)
				   return-ty
				   (cons +tsp*+ (cdr args-ty))
				   var-args does-not-throw does-not-return)
	(create-primitive-function module
				   (dispatch-function-name name +tmv*+)
				   return-ty
				   (cons +tmv*+ (cdr args-ty))
				   var-args does-not-throw does-not-return))
      (create-primitive-function module
				 name return-ty args-ty var-args does-not-throw does-not-return))
  (core::hash-table-setf-gethash *primitives* name
				 (list return-ty args-ty '( (:var-args . var-args)
							   (:does-not-throw . does-not-throw)
							   ( :does-not-return . does-not-return) ))))


(defun primitive-does-not-throw (module name return-ty args-ty &key var-args does-not-return)
  (primitive module name return-ty args-ty :var-args var-args :does-not-throw t :does-not-return does-not-return))

(defun define-primitives-in-module (module)
  (primitive-does-not-throw module "newTsp" +void+ (list +tsp*+))
  (primitive-does-not-throw module "resetTsp" +void+ (list +tsp*+))
  (primitive-does-not-throw module "makeUnboundTsp" +void+ (list +tsp*+))
  (primitive-does-not-throw module "copyTsp" +void+ (list +tsp*-or-tmv*+ +tsp*+))
  (primitive-does-not-throw module "destructTsp" +void+ (list +tsp*+))
  (primitive-does-not-throw module "compareTsp" +i32+ (list +tsp*+ +tsp*+))

  (primitive-does-not-throw module "newTmv" +void+ (list +tmv*+))
  (primitive-does-not-throw module "resetTmv" +void+ (list +tmv*+))
  (primitive-does-not-throw module "copyTmv" +void+ (list +tmv*+ +tmv*+))
  (primitive-does-not-throw module "destructTmv" +void+ (list +tmv*+))

  (primitive-does-not-throw module "newAFsp" +void+ (list +afsp*+))
  (primitive-does-not-throw module "newAFsp_ValueFrameOfSize" +void+ (list +afsp*+ +i32+))
  (primitive-does-not-throw module "resetAFsp" +void+ (list +afsp*+))
  (primitive-does-not-throw module "copyAFsp" +void+ (list +afsp*+ +afsp*+))
  (primitive-does-not-throw module "destructAFsp" +void+ (list +afsp*+))

  (primitive-does-not-throw module "copyTmvOrSlice" +void+ (list +tsp*-or-tmv*+ +tmv*+))

  (primitive-does-not-throw module "isNilTsp" +i32+ (list +tsp*+))
  (primitive-does-not-throw module "isTrueTsp" +i32+ (list +tsp*+))
  (primitive-does-not-throw module "isBoundTsp" +i32+ (list +tsp*+))


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
  (primitive-does-not-throw module "makeShortFloat" +void+ (list +tsp*+ +double+))
  (primitive-does-not-throw module "makeSingleFloat" +void+ (list +tsp*+ +double+))
  (primitive-does-not-throw module "makeDoubleFloat" +void+ (list +tsp*+ +double+))
  (primitive-does-not-throw module "makeLongFloat" +void+ (list +tsp*+ +double+))
  (primitive-does-not-throw module "makeString" +void+ (list +tsp*+ +i8*+))
  (primitive module "makeCompiledFunction" +void+ (list +tsp*-or-tmv*+ +fn*+ +tsp*+ +afsp*+))


  (primitive module "fillRestTarget" +void+ (list +tsp*+ +afsp*+ +i32+ +i8*+))

  (primitive-does-not-throw module "symbolValueRead" +void+ (list +tsp*-or-tmv*+ +symsp*+))
  (primitive-does-not-throw module "symbolValueReadOrUnbound" +void+ (list +tsp*-or-tmv*+ +symsp*+))
  (primitive-does-not-throw module "symbolValueReference" +tsp*+ (list +symsp*+))
  (primitive-does-not-throw module "lexicalValueReference" +tsp*+ (list +i32+ +i32+ +afsp*+))
  (primitive-does-not-throw module "lexicalValueRead" +void+ (list +tsp*-or-tmv*+ +i32+ +i32+ +afsp*+))
  (primitive-does-not-throw module "symbolFunctionRead" +void+ (list +tsp*-or-tmv*+ +symsp*+))
  (primitive-does-not-throw module "setfSymbolFunctionRead" +void+ (list +tsp*+ +symsp*+))
  (primitive-does-not-throw module "lexicalFunctionRead" +void+ (list +tsp*-or-tmv*+ +i32+ +i32+ +afsp*+))


  (primitive-does-not-throw module "makeValueFrame" +void+ (list +afsp*+ +i32+))
  (primitive-does-not-throw module "makeValueFrameFromReversedCons" +void+ (list +afsp*+ +tsp*+))
  (primitive-does-not-throw module "setParentOfActivationFrame" +void+ (list +afsp*+ +afsp*+))

  (primitive-does-not-throw module "attachDebuggingInfoToValueFrame" +void+ (list +afsp*+ +tsp*+))

  (primitive-does-not-throw module "valueFrameReference" +tsp*+ (list +afsp*+ +i32+))

  (primitive module "makeFunctionFrame" +void+ (list +afsp*+ +i32+ +afsp*+))
  (primitive module "functionFrameReference" +tsp*+ (list +afsp*+ +i32+))

  (primitive module "prependMultipleValues" +void+ (list +tsp*-or-tmv*+ +tmv*+))
  
  (primitive module "invokePossibleMultipleValueFunction" +void+ (list +tmv*+ +tsp*+ +afsp*+))
  (primitive module "invokePossibleMultipleValueSymbolFunction" +void+ (list +tsp*-or-tmv*+ +symsp*+ +afsp*+))
  (primitive module "invokePossibleMultipleValueLexicalFunction" +void+ (list +tsp*-or-tmv*+ +i32+ +i32+ +afsp*+))

  (primitive module "invokeLlvmFunction" +void+ (list +tmv*+ +fn*+ +afsp*+))

  (primitive-does-not-throw module "activationFrameNil" +afsp*+ nil)
  (primitive-does-not-throw module "activationFrameSize" +i32+ (list +afsp*+))
  (primitive-does-not-throw module "activationFrameParentRef" +afsp*+ (list +afsp*+))
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
  (primitive-does-not-throw module "debugInspectObject_sp" +void+ (list +tsp*+))
  (primitive-does-not-throw module "debugInspectObject_mv" +void+ (list +tmv*+))

  (primitive-does-not-throw module "debugPointer" +void+ (list +i8*+))
  (primitive-does-not-throw module "debugPrintObject" +void+ (list +i8*+ +tsp*+))
  (primitive-does-not-throw module "debugPrintI32" +void+ (list +i32+))
  (primitive-does-not-throw module "lowLevelTrace" +void+ (list +i32+))

  (primitive-does-not-throw module "singleStepCallback" +void+ nil)

  (primitive-does-not-throw module "trace_enterFunctionScope" +i32+ (list +i8*+ +i32+ +i32+ +afsp*+ +i8*+ ))
  (primitive-does-not-throw module "trace_enterBlockScope" +i32+ (list +i8*+ +i32+ +i32+ +afsp*+ +i8*+))
  (primitive-does-not-throw module "trace_enterLetScope" +i32+ (list +i8*+ +i32+ +i32+ +afsp*+ +i8*+))
  (primitive-does-not-throw module "trace_enterLetSTARScope" +i32+ (list +i8*+ +i32+ +i32+ +afsp*+ +i8*+))
  (primitive-does-not-throw module "trace_enterFletScope" +i32+ (list +i8*+ +i32+ +i32+ +afsp*+ +i8*+))
  (primitive-does-not-throw module "trace_enterLabelsScope" +i32+ (list +i8*+ +i32+ +i32+ +afsp*+ +i8*+))
  (primitive-does-not-throw module "trace_enterCallScope" +i32+ (list +i8*+ +i32+ +i32+ +afsp*+ +i8*+))
  (primitive-does-not-throw module "trace_enterCatchScope" +i32+ (list +i8*+ +i32+ +i32+ +afsp*+ +i8*+))
  (primitive-does-not-throw module "trace_enterUnwindProtectScope" +i32+ (list +i8*+ +i32+ +i32+ +afsp*+ +i8*+))

  (primitive-does-not-throw module "trace_setActivationFrameForLexicalScope" +void+ (list +i32+ +afsp*+))

  (primitive-does-not-throw module "trace_exitFunctionScope" +void+ (list +i32+) )
  (primitive-does-not-throw module "trace_exitBlockScope" +void+ (list +i32+ ) )
  (primitive-does-not-throw module "trace_exitLetScope" +void+ (list +i32+ ) )
  (primitive-does-not-throw module "trace_exitLetSTARScope" +void+ (list +i32+ ) )
  (primitive-does-not-throw module "trace_exitFletScope" +void+ (list +i32+ ) )
  (primitive-does-not-throw module "trace_exitLabelsScope" +void+ (list +i32+ ) )
  (primitive-does-not-throw module "trace_exitCallScope" +void+ (list +i32+ ) )
  (primitive-does-not-throw module "trace_exitCatchScope" +void+ (list +i32+ ) )
  (primitive-does-not-throw module "trace_exitUnwindProtectScope" +void+ (list +i32+ ) )

  (primitive module "throwCatchThrow" +void+ (list +tsp*+ +tmv*+) :does-not-return t)
  (primitive module "throwReturnFrom" +void+ (list +i32+ +tmv*+) :does-not-return t)

  (primitive-does-not-throw module "catchStoreTag" +void+ (list +tsp*+ +tsp*+))
  (primitive module "catchIfTagMatchesStoreResultElseRethrow" +void+ (list +tsp*-or-tmv*+ +tsp*+ +i8*+))
  (primitive-does-not-throw module "catchUnwind" +void+ (list +tsp*+))

  ;; depreciated
  ;;(primitive-does-not-throw module "catchTagMatches" +i32+ (list +tsp*+ +i8*+))
  ;; depreciated
  ;;  (primitive-does-not-throw module "catchStoreResult" +void+ (list +tsp*+ +i8*+))
  

  (primitive module "blockHandleReturnFrom" +void+ (list +tsp*-or-tmv*+ +i8*+))

  (primitive module "throw_Go" +void+ (list +i32+ +i32+) :does-not-return t)
  (primitive module "tagbodyGoIndexElseRethrow" +i32+ (list +i8*+))

  (primitive module "throwIllegalSwitchValue" +void+ (list +i32+ +i32+) :does-not-return t)

  (primitive-does-not-throw module "brcl_terminate" +void+ (list +i8*+ +i32+ +i32+ +i8*+) )
  (primitive-does-not-throw module "__gxx_personality_v0" +i32+ nil :var-args t) ;; var-args
  (primitive-does-not-throw module "__cxa_begin_catch" +i8*+ (list +i8*+) )
  (primitive module "__cxa_end_catch" +void+ nil)
  (primitive module "__cxa_rethrow" +void+ nil)
  (primitive-does-not-throw module "llvm.eh.typeid.for" +i32+ (list +i8*+))
  (primitive-does-not-throw module "_Unwind_Resume" +void+ (list +i8*+))

  (primitive-does-not-throw module "getLoadTimeValueArray" +void+ (list +ltvsp**+ +i8*+ +i32+ +i32+))

  (primitive-does-not-throw module "copyLoadTimeValue" +void+ (list +tsp*-or-tmv*+ +ltvsp**+ +i32+))

  (primitive-does-not-throw module "loadTimeValuePushAndGetReference" +tsp*+ (list +ltvsp**+ +i32+))
  (primitive-does-not-throw module "loadTimeSymbolPushAndGetReference" +symsp*+ (list +ltvsp**+ +i32+))
  (primitive-does-not-throw module "loadTimeValueReference" +tsp*+ (list +ltvsp**+ +i32+))
  (primitive-does-not-throw module "loadTimeSymbolReference" +symsp*+ (list +ltvsp**+ +i32+))
  (primitive-does-not-throw module "getLoadTimeValue" +void+ (list +tsp*-or-tmv*+ +ltvsp**+ +i32+))

  (primitive-does-not-throw module "ltv_makeCons" +void+ (list +tsp*+))
  (primitive-does-not-throw module "ltv_makeSourceCodeCons" +void+ (list +tsp*+ +i8*+ +i32+ +i32+))
  (primitive-does-not-throw module "ltv_makeArrayObjects" +void+ (list +tsp*+ +tsp*+ +i32+ +i32*+))
  (primitive-does-not-throw module "ltv_makeHashTable" +void+ (list +tsp*+ +tsp*+))
  (primitive-does-not-throw module "rplaca" +void+ (list +tsp*+ +tsp*+))
  (primitive-does-not-throw module "rplacd" +void+ (list +tsp*+ +tsp*+))
  (primitive-does-not-throw module "ltv_initializeArrayObjectsRowMajorArefOrder" +void+ (list +tsp*+ +ltvsp**+ +i32*+))
  (primitive-does-not-throw module "ltv_initializeHashTable" +void+ (list +tsp*+ +i32+ +ltvsp**+ +i32*+))

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

  )


;; --------------------------------------------------------
;;
;; Now we have the primitives - put them in the default module
;;
(define-primitives-in-module *run-time-module*)



;;------------------------------------------------------------
;;
;; Setup dynamic variables 
;;
;;



(defvar *source-path-name* "-internal-code-no-file-" "Store the path-name of the currently compiled file")
(defvar *gv-source-path-name* nil ;; (jit-make-global-string-ptr *source-path-name* "source-path-name")
  "Store a global value that defines the filename of the current compilation")
(defvar *current-line-number* 0 "Store the line number of the currently compiled form")
(defvar *current-column* 0 "Store the column of the currently compiled form")
(defvar *current-form* nil "The current form being compiled")
(defvar *current-env* nil "Current environment")
(defvar *current-function* nil "The current function")
(defvar *current-function-name* nil "Store the current function name")
(defvar *gv-current-function-name* nil "Store the global value in the module of the current function name ")
