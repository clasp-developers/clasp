

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
(defmacro log-dump (fn) nil)
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
      (bformat t ,fmt ,@args)
      nil))

(defmacro log-dump (fn)
  `(if (is-debug-compiler-on)
       (llvm-sys:dump ,fn)
      nil))








(defun debug-write-module (module filename)
  (bformat t "!!!!!!!!!!!\n")
  (bformat t "!!!!!!!!!!!\n")
  (bformat t "!!!      WRITING MODULE TO FILE: %s\n" filename)
  (bformat t "!!!!!!!!!!!\n")
  (llvm-sys:write-text-bitcode-to-path module (make-path filename)))


(defparameter *irbuilder* (llvm-sys:make-irbuilder *llvm-context*))





;;
;; Create types
;;

(defconstant +double+ (llvm-sys:type-get-double-ty *llvm-context*))
(defconstant +void+ (llvm-sys:type-get-void-ty *llvm-context*))
(defconstant +i8+ (llvm-sys:type-get-int8-ty *llvm-context*))
(defconstant +i8*+ (llvm-sys:type-get-pointer-to +i8+))
(defconstant +i8**+ (llvm-sys:type-get-pointer-to +i8*+))
(defconstant +i32+ (llvm-sys:type-get-int32-ty *llvm-context*))
;;(defconstant +exception-struct+ (llvm-sys:struct-type-get *llvm-context* (list +i8*+ +i32+) "exception-struct" nil))
(defconstant +exception-struct+ (llvm-sys:struct-type-get *llvm-context* (list +i8*+ +i32+) nil))
(defconstant +i32*+ (llvm-sys:type-get-pointer-to +i32+))
(defconstant +i32**+ (llvm-sys:type-get-pointer-to +i32*+))

(defconstant +sp-counted-base+ (llvm-sys:struct-type-get *llvm-context* (list +i32+ +i32+) nil)) ;; "sp-counted-base-ty" 
(defconstant +sp-counted-base-ptr+ (llvm-sys:type-get-pointer-to +sp-counted-base+))
(defconstant +shared-count+ (llvm-sys:struct-type-get *llvm-context* (list +sp-counted-base-ptr+) nil)) ;; "shared_count" 
;;
;; If I use an opaque type then the symbol type gets duplicated and that causes
;; problems - try just using an int
;;(defconstant +sym+ (llvm-sys:struct-type-get *llvm-context* nil nil)) ;; "Symbol_O" 
(defconstant +sym+ (llvm-sys:type-get-int32-ty *llvm-context*))
(defconstant +sym-ptr+ (llvm-sys:type-get-pointer-to +sym+))
(defconstant +symsp+ (llvm-sys:struct-type-get *llvm-context* (list +sym-ptr+ +shared-count+) nil)) ;; "Sym_sp" 
(defconstant +symsp*+ (llvm-sys:type-get-pointer-to +symsp+))




;; Define the T_O struct - right now just put in a dummy i32 - later put real fields here
(defconstant +t+ (llvm-sys:struct-type-get *llvm-context* nil  nil)) ;; "T_O"
(defconstant +t-ptr+ (llvm-sys:type-get-pointer-to +t+))
(defconstant +tsp+ (llvm-sys:struct-type-get *llvm-context* (list +t-ptr+ +shared-count+) nil))  ;; "T_sp" 
(defconstant +tsp*+ (llvm-sys:type-get-pointer-to +tsp+))
(defconstant +tsp**+ (llvm-sys:type-get-pointer-to +tsp*+))


;; Define the VectorObjectsWithFillPtr_O struct - right now just put in a dummy i32 - later put real fields here
(defconstant +ltv+ (llvm-sys:struct-type-get *llvm-context* nil  nil)) ;; "VectorObjectsWithFillPtr_O"
(defconstant +ltv-ptr+ (llvm-sys:type-get-pointer-to +ltv+))
(defconstant +ltvsp+ (llvm-sys:struct-type-get *llvm-context* (list +ltv-ptr+ +shared-count+) nil))  ;; "T_sp" 
(defconstant +ltvsp*+ (llvm-sys:type-get-pointer-to +ltvsp+))
(defconstant +ltvsp**+ (llvm-sys:type-get-pointer-to +ltvsp*+))



(defconstant +af+ (llvm-sys:struct-type-get *llvm-context* nil  nil)) ;; "ActivationFrame_O"
(defconstant +af-ptr+ (llvm-sys:type-get-pointer-to +af+))
(defconstant +afsp+ (llvm-sys:struct-type-get *llvm-context* (list +af-ptr+ +shared-count+)  nil)) ;; "ActivationFrame_sp"
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

(defconstant +fn-tsp*-afsp*+ (llvm-sys:function-type-get +void+ (list +tsp*+ +afsp*+)))
(defconstant +fn*+ (llvm-sys:type-get-pointer-to +fn-tsp*-afsp*+))





;;
;; Define functions within the module
;;

(defparameter *primitives* (make-hash-table :test 'equal))


(defun throw-if-mismatched-arguments (fn-name args)
  (let* ((info (gethash fn-name *primitives*))
	 (return-ty (car info))
	 (args-ty (cadr info))
	 (passed-args-ty (mapcar #'(lambda (x)
				     (if (is-assignable-to x (find-class 'llvm-sys:VALUE))
					 (llvm-sys:get-type x)
					 (class-name x)))
				 args))
	 (i 1))
    (mapc #'(lambda (x y z) (unless (eq x y)
			      (error "Calling %s - mismatch of arg#%d value[%s], expected type %s received type %s" fn-name i z x y))) args-ty passed-args-ty args)))
    


(defun primitive (name return-ty args-ty &key var-args does-not-throw does-not-return)
  (let ((fn (llvm-sys:function-create (llvm-sys:function-type-get return-ty args-ty var-args) 'llvm-sys::External-linkage name *the-module*)))
    (when does-not-throw (llvm-sys:set-does-not-throw fn))
    (when does-not-return (llvm-sys:set-does-not-return fn))
    (core::hash-table-setf-gethash *primitives* name (list return-ty args-ty var-args does-not-throw does-not-return))))


(defun primitive-does-not-throw (name return-ty args-ty &key var-args does-not-return)
  (primitive name return-ty args-ty :var-args var-args :does-not-throw t :does-not-return does-not-return))

(defun define-primitives-in-*the-module* ()
  (primitive-does-not-throw "newTsp" +void+ (list +tsp*+))
  (primitive-does-not-throw "resetTsp" +void+ (list +tsp*+))
  (primitive-does-not-throw "copyTsp" +void+ (list +tsp*+ +tsp*+))
  (primitive-does-not-throw "destructTsp" +void+ (list +tsp*+))

  (primitive-does-not-throw "newAFsp" +void+ (list +afsp*+))
  (primitive-does-not-throw "resetAFsp" +void+ (list +afsp*+))
  (primitive-does-not-throw "copyAFsp" +void+ (list +afsp*+ +afsp*+))
  (primitive-does-not-throw "destructAFsp" +void+ (list +afsp*+))

  (primitive-does-not-throw "isNilTsp" +i32+ (list +tsp*+))
  (primitive-does-not-throw "isTrueTsp" +i32+ (list +tsp*+))


  (primitive-does-not-throw "internSymbol" +void+ (list +tsp*+ +i8*+ +i8*+))
  (primitive-does-not-throw "makeSymbol" +void+ (list +tsp*+ +i8*+))

  (primitive-does-not-throw "makeNil" +void+ (list +tsp*+))
  (primitive-does-not-throw "makeT" +void+ (list +tsp*+))
  (primitive-does-not-throw "makeCons" +void+ (list +tsp*+ +tsp*+ +tsp*+))
  (primitive-does-not-throw "makeFixnum" +void+ (list +tsp*+ +i32+))
  (primitive-does-not-throw "makeCharacter" +void+ (list +tsp*+ +i32+))
  (primitive-does-not-throw "makeBignum" +void+ (list +tsp*+ +i8*+))
  (primitive-does-not-throw "makeShortFloat" +void+ (list +tsp*+ +double+))
  (primitive-does-not-throw "makeSingleFloat" +void+ (list +tsp*+ +double+))
  (primitive-does-not-throw "makeDoubleFloat" +void+ (list +tsp*+ +double+))
  (primitive-does-not-throw "makeLongFloat" +void+ (list +tsp*+ +double+))
  (primitive-does-not-throw "makeString" +void+ (list +tsp*+ +i8*+))
  (primitive "makeClosure" +void+ (list +tsp*+ +fn*+ +afsp*+))


  (primitive "fillRestTarget" +void+ (list +tsp*+ +afsp*+ +i32+ +i8*+))
  (primitive "checkForAllowOtherKeywords" +i32+ (list +i32+ +afsp*+ +i32+))
  (primitive "lookupKeyword" +i32+ (list +tsp*+ +symsp*+ +afsp*+ +i32+))
  (primitive "throwIfOtherKeywords" +void+ (list +afsp*+ +i32+ +i8*+))

  (primitive-does-not-throw "symbolValueRead" +void+ (list +tsp*+ +symsp*+))
  (primitive-does-not-throw "symbolValueReference" +tsp*+ (list +symsp*+))
  (primitive-does-not-throw "lexicalValueReference" +tsp*+ (list +i32+ +i32+ +afsp*+))
  (primitive-does-not-throw "lexicalValueRead" +void+ (list +tsp*+ +i32+ +i32+ +afsp*+))
  (primitive-does-not-throw "symbolFunctionRead" +void+ (list +tsp*+ +symsp*+))
  (primitive-does-not-throw "setfSymbolFunctionRead" +void+ (list +tsp*+ +symsp*+))
  (primitive-does-not-throw "lexicalFunctionRead" +void+ (list +tsp*+ +i32+ +i32+ +afsp*+))
  (primitive-does-not-throw "makeValueFrameWithNilParent" +void+ (list +afsp*+ +i32+))
  (primitive-does-not-throw "makeValueFrame" +void+ (list +afsp*+ +i32+ +afsp*+))
  (primitive-does-not-throw "makeValueFrameFromReversedCons" +void+ (list +afsp*+ +tsp*+ +afsp*+))

  (primitive-does-not-throw "attachDebuggingInfoToValueFrame" +void+ (list +afsp*+ +tsp*+))

  (primitive-does-not-throw "valueFrameReference" +tsp*+ (list +afsp*+ +i32+))

  (primitive "makeFunctionFrame" +void+ (list +afsp*+ +i32+ +afsp*+))
  (primitive "functionFrameReference" +tsp*+ (list +afsp*+ +i32+))

  (primitive "prependMultipleValues" +void+ (list +tsp*+ +tsp*+))
  
  (primitive-does-not-throw "firstValueIfMultipleValue" +void+ ( list +tsp*+ ))


  (primitive "invokePossibleMultipleValueFunction" +void+ (list +tsp*+ +tsp*+ +afsp*+))
  (primitive "invokePossibleMultipleValueSymbolFunction" +void+ (list +tsp*+ +symsp*+ +afsp*+))
  (primitive "invokePossibleMultipleValueLexicalFunction" +void+ (list +tsp*+ +i32+ +i32+ +afsp*+))

  (primitive "invokeLlvmFunction" +void+ (list +tsp*+ +fn*+ +afsp*+))

  (primitive-does-not-throw "activationFrameNil" +afsp*+ nil)
  (primitive-does-not-throw "activationFrameSize" +i32+ (list +afsp*+))
  (primitive-does-not-throw "activationFrameParentRef" +afsp*+ (list +afsp*+))
  (primitive "throwTooManyArgumentsException" +void+ (list +i8*+ +afsp*+ +i32+ +i32+))
  (primitive "throwNotEnoughArgumentsException" +void+ (list +i8*+ +afsp*+ +i32+ +i32+))
  (primitive "throwIfExcessKeywordArguments" +void+ (list +i8*+ +afsp*+ +i32+))

  (primitive-does-not-throw "gdb" +void+ nil)
  (primitive-does-not-throw "debugInvoke" +void+ nil)
  (primitive-does-not-throw "debugInspectActivationFrame" +void+ (list +afsp*+))
  (primitive-does-not-throw "debugInspectObject" +void+ (list +tsp*+))

  (primitive-does-not-throw "debugTrace" +void+ (list +i8*+))
  (primitive-does-not-throw "debugPrintObject" +void+ (list +i8*+ +tsp*+))
  (primitive-does-not-throw "debugPrintI32" +void+ (list +i32+))
  (primitive-does-not-throw "lowLevelTrace" +void+ (list +i32+))

  (primitive-does-not-throw "trace_enterFunctionScope" +i32+ (list +i8*+ +i32+ +i32+ +afsp*+ +i8*+ ))
  (primitive-does-not-throw "trace_enterBlockScope" +i32+ (list +i8*+ +i32+ +i32+ +afsp*+ +i8*+))
  (primitive-does-not-throw "trace_enterLetScope" +i32+ (list +i8*+ +i32+ +i32+ +afsp*+ +i8*+))
  (primitive-does-not-throw "trace_enterLetSTARScope" +i32+ (list +i8*+ +i32+ +i32+ +afsp*+ +i8*+))
  (primitive-does-not-throw "trace_enterFletScope" +i32+ (list +i8*+ +i32+ +i32+ +afsp*+ +i8*+))
  (primitive-does-not-throw "trace_enterLabelsScope" +i32+ (list +i8*+ +i32+ +i32+ +afsp*+ +i8*+))
  (primitive-does-not-throw "trace_enterCallScope" +i32+ (list +i8*+ +i32+ +i32+ +afsp*+ +i8*+))
  (primitive-does-not-throw "trace_enterCatchScope" +i32+ (list +i8*+ +i32+ +i32+ +afsp*+ +i8*+))
  (primitive-does-not-throw "trace_enterUnwindProtectScope" +i32+ (list +i8*+ +i32+ +i32+ +afsp*+ +i8*+))

  (primitive-does-not-throw "trace_exitFunctionScope" +void+ (list +i32+) )
  (primitive-does-not-throw "trace_exitBlockScope" +void+ (list +i32+ ) )
  (primitive-does-not-throw "trace_exitLetScope" +void+ (list +i32+ ) )
  (primitive-does-not-throw "trace_exitLetSTARScope" +void+ (list +i32+ ) )
  (primitive-does-not-throw "trace_exitFletScope" +void+ (list +i32+ ) )
  (primitive-does-not-throw "trace_exitLabelsScope" +void+ (list +i32+ ) )
  (primitive-does-not-throw "trace_exitCallScope" +void+ (list +i32+ ) )
  (primitive-does-not-throw "trace_exitCatchScope" +void+ (list +i32+ ) )
  (primitive-does-not-throw "trace_exitUnwindProtectScope" +void+ (list +i32+ ) )

  (primitive "throwCatchThrow" +void+ (list +tsp*+ +tsp*+) :does-not-return t)
  (primitive "throwReturnFrom" +void+ (list +i32+ +tsp*+) :does-not-return t)

  (primitive-does-not-throw "catchStoreTag" +void+ (list +tsp*+ +tsp*+))
  (primitive "catchIfTagMatchesStoreResultElseRethrow" +void+ (list +tsp*+ +tsp*+ +i8*+))
  (primitive-does-not-throw "catchUnwind" +void+ (list +tsp*+))

  ;; depreciated
  (primitive-does-not-throw "catchTagMatches" +i32+ (list +tsp*+ +i8*+))
  ;; depreciated
  (primitive-does-not-throw "catchStoreResult" +void+ (list +tsp*+ +i8*+))
  

  (primitive "blockHandleReturnFrom" +void+ (list +tsp*+ +i8*+))

  (primitive "throw_Go" +void+ (list +i32+ +i32+) :does-not-return t)
  (primitive "tagbodyGoIndexElseRethrow" +i32+ (list +i8*+))

  (primitive "throwIllegalSwitchValue" +void+ (list +i32+ +i32+) :does-not-return t)

  (primitive-does-not-throw "cando_terminate" +void+ (list +i8*+ +i32+ +i32+ +i8*+) )
  (primitive-does-not-throw "__gxx_personality_v0" +i32+ nil :var-args t) ;; var-args
  (primitive-does-not-throw "__cxa_begin_catch" +i8*+ (list +i8*+) )
  (primitive "__cxa_end_catch" +void+ nil)
  (primitive "__cxa_rethrow" +void+ nil)
  (primitive-does-not-throw "llvm.eh.typeid.for" +i32+ (list +i8*+))
  (primitive-does-not-throw "_Unwind_Resume" +void+ (list +i8*+))

  (primitive-does-not-throw "getLoadTimeValueArray" +void+ (list +ltvsp**+ +i8*+ +i32+))
  (primitive-does-not-throw "copyLoadTimeValue" +void+ (list +tsp*+ +ltvsp**+ +i32+))
  (primitive-does-not-throw "loadTimeValuePushAndGetReference" +tsp*+ (list +ltvsp**+ +i32+))
  (primitive-does-not-throw "loadTimeValueReference" +tsp*+ (list +ltvsp**+ +i32+))
  (primitive-does-not-throw "getLoadTimeValueReference" +void+ (list +tsp*+ +ltvsp**+ +i32+))

  (primitive-does-not-throw "ltv_makeCons" +void+ (list +tsp*+))
  (primitive-does-not-throw "ltv_makeSourceCodeCons" +void+ (list +tsp*+ +i8*+ +i32+ +i32+))
  (primitive-does-not-throw "ltv_makeArrayObjects" +void+ (list +tsp*+ +tsp*+ +i32+ +i32*+))
  (primitive-does-not-throw "ltv_makeHashTable" +void+ (list +tsp*+ +tsp*+))
  (primitive-does-not-throw "rplaca" +void+ (list +tsp*+ +tsp*+))
  (primitive-does-not-throw "rplacd" +void+ (list +tsp*+ +tsp*+))
  (primitive-does-not-throw "ltv_initializeArrayObjectsRowMajorArefOrder" +void+ (list +tsp*+ +ltvsp**+ +i32*+))
  (primitive-does-not-throw "ltv_initializeHashTable" +void+ (list +tsp*+ +i32+ +ltvsp**+ +i32*+))






)



;;------------------------------------------------------------
;;
;; Setup the module
;;
;;
;;(debug-log-on)
;;(cmp-log "About to define primitives")
(define-primitives-in-*the-module*)
;;(debug-log-off)


;;------------------------------------------------------------
;;
;; Setup dynamic variables 
;;
;;



(defvar *source-path-name* "-internal-code-no-file-" "Store the path-name of the currently compiled file")
(defvar *gv-source-path-name* (jit-make-global-string-ptr *source-path-name* "source-path-name")
  "Store a global value that defines the filename of the current compilation")
(defvar *current-line-number* 0 "Store the line number of the currently compiled form")
(defvar *current-column* 0 "Store the column of the currently compiled form")
(defvar *current-form* nil "The current form being compiled")
(defvar *current-env* nil "Current environment")
(defvar *current-function* nil "The current function")
(defvar *current-function-name* nil "Store the current function name")
(defvar *gv-current-function-name* nil "Store the global value in the module of the current function name ")
