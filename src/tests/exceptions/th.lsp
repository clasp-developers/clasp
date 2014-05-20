(defparameter *ht* (make-hash-table :test 'equal))


(defun primitive (name)
  (core::hash-table-setf-gethash *ht* name name))



(debug-log-on)
(primitive "newTsp" )
(primitive "resetTsp" )
(primitive "copyTsp" )
(primitive "destructTsp" )


(bformat t "%s\n" (hash-table-dump *ht*))


(print (gethash "copyTsp" *ht*))


#|
(primitive "newAFsp" )
(primitive "resetAFsp" )
(primitive "copyAFsp" )
(primitive "destructAFsp" )

(primitive "isNilTsp" )
(primitive "isTrueTsp" )


(primitive "internSymbol" )

(primitive "makeNil" )
(primitive "makeT" )
(primitive "makeCons" )
(primitive "makeFixnum" )
(primitive "makeBignum" )
(primitive "makeShortFloat" )
(primitive "makeSingleFloat" )
(primitive "makeDoubleFloat" )
(primitive "makeLongFloat" )
(primitive "makeString" )
(primitive "makeClosure" )

(primitive "fillRestTarget" )
(primitive "checkForAllowOtherKeywords" )
(primitive "lookupKeyword" )
(primitive "throwIfOtherKeywords" )

(primitive "symbolValueRead" )
(primitive "symbolValueReference" )
(primitive "lexicalValueReference" )
(primitive "lexicalValueRead" )
(primitive "symbolFunctionRead" )
(primitive "lexicalFunctionRead" )
(primitive "makeValueFrameWithNilParent" )
(primitive "makeValueFrame" )
(primitive "valueFrameReference" )

(primitive "makeFunctionFrame" )
(primitive "functionFrameReference" )

(primitive "prependMultipleValues" )
(primitive "makeValueFrameFromReversedCons" )

(primitive "firstValueIfMultipleValue" )


(primitive "invokePossibleMultipleValueFunction" )
(primitive "invokePossibleMultipleValueSymbolFunction" )
(primitive "invokePossibleMultipleValueLexicalFunction" )

(primitive "invokeLlvmFunction" )

(primitive "activationFrameNil" )
(primitive "activationFrameSize" )
(primitive "activationFrameParentRef" )
(primitive "throwTooManyArgumentsException" )
(primitive "throwNotEnoughArgumentsException" )
(primitive "throwIfExcessKeywordArguments" )

(primitive "gdb" )
(primitive "debugInvoke" )
(primitive "debugInspectActivationFrame" )
(primitive "debugInspectObject" )

(primitive "debugTrace" )
(primitive "debugPrintObject" )
(primitive "debugPrintI32" )

(primitive "trace_enterFunctionScope" )
(primitive "trace_enterBlockScope" )
(primitive "trace_enterLetScope" )
(primitive "trace_enterLetSTARScope" )
(primitive "trace_lineNumberAndColumn" )
(primitive "trace_exitLexicalScope" )

(primitive "throwCatchThrow" )
(primitive "catchStoreTag" )
(primitive "catchTagMatches" )
(primitive "catchUnwind" )
(primitive "catchStoreResult" )

(primitive "terminate" )
(primitive "__gxx_personality_v0" )
(primitive "__cxa_begin_catch" )
(primitive "__cxa_end_catch" )
(primitive "__cxa_rethrow" )
(primitive "llvm.eh.typeid.for" )
(primitive "_Unwind_Resume" )

|#
