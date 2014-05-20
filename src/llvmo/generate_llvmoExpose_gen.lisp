 

(eval-when (:compile-toplevel :load-toplevel :execute) (load "../common/expose.lisp"))

(set-package "LlvmoPkg" "llvmo" "llvm")






(e-ptr-class
 :lisp-name "llvm-context"
 :class-name "LLVMContext"
 :base-classo-name "core::ExternalObject_O"
 :ptr-type "llvm::LLVMContext"
 :exposed-static-methods (list
		  (make-exposed-static-method
		   :lisp-name "get-global-context"
		   :c++-name "get_global_context"
		   :returns "LLVMContext_sp"
		   :arguments "()"
		   :body "
{_G();
    LLVMContext_sp context = RP_Create<LLVMContext_O>(_lisp);
    context->_ptr = &(llvm::getGlobalContext());
    llvm::LLVMContext& lc = llvm::getGlobalContext();
    ASSERT(context->_ptr == &lc );
    return context;
}")
		  )
 :from-object-translators ( list
			    (make-from-object-translator
			     :to-type "llvm::LLVMContext&"
			     :return-type "llvm::LLVMContext&"
			     :code "{_G(); return *(object->as<llvmo::LLVMContext_O>()->wrappedPtr());};"
			     )
			    )
 )




;; ------------------------------------------------------------
;; ------------------------------------------------------------
;; ------------------------------------------------------------
;; ------------------------------------------------------------
;;
;; 
(e-ptr-class
 :lisp-name "Pass"
 :class-name "Pass"
 :base-classo-name "core::ExternalObject_O"
 :ptr-type "llvm::Pass"
 :wrapped-method-names '(
;;		    "dump"
		    )
 :from-object-translators ( list
			    (make-from-object-translator
			     :to-type "llvm::Pass*"
			     :return-type "llvm::Pass*"
			     :code "{_G(); return (object->as<llvmo::Pass_O>()->wrappedPtr());};"
			     )
			    )

 :to-object-translators (list
			 (make-to-object-translator
			  :from-type "llvm::Pass*"
			  :code "{_G(); return core::RP_Create_wrapped<llvmo::Pass_O,llvm::Pass*>(ptr);}"
			  )
			 )

 )



(e-derived-class
 :lisp-name "FunctionPass"
 :class-name "FunctionPass"
 :base-classo-name "Pass_O"
 :ptr-type "llvm::FunctionPass"
 :from-object-translators (list
			   (make-from-object-translator
			    :to-type "llvm::FunctionPass*"
			    :code "{return object->as<llvmo::FunctionPass_O>()->wrappedPtr();};"
			    )
			   )
 :to-object-translators (list
			 (make-to-object-translator
			  :from-type "llvm::FunctionPass*"
			  :code "{_G(); return core::RP_Create_wrapped<llvmo::FunctionPass_O,llvm::FunctionPass*>(ptr);}"
			  )
			 )
 )


(e-derived-class
 :lisp-name "ModulePass"
 :class-name "ModulePass"
 :base-classo-name "Pass_O"
 :ptr-type "llvm::ModulePass"
 :from-object-translators (list
			   (make-from-object-translator
			    :to-type "llvm::ModulePass*"
			    :code "{return object->as<llvmo::ModulePass_O>()->wrappedPtr();};"
			    )
			   )
 :to-object-translators (list
			 (make-to-object-translator
			  :from-type "llvm::ModulePass*"
			  :code "{_G(); return core::RP_Create_wrapped<llvmo::ModulePass_O,llvm::ModulePass*>(ptr);}"
			  )
			 )
 )

(e-derived-class
 :lisp-name "ImmutablePass"
 :class-name "ImmutablePass"
 :base-classo-name "ModulePass_O"
 :ptr-type "llvm::ImmutablePass"
 :from-object-translators (list
			   (make-from-object-translator
			    :to-type "llvm::ImmutablePass*"
			    :code "{return object->as<llvmo::ImmutablePass_O>()->wrappedPtr();};"
			    )
			   )
 :to-object-translators (list
			 (make-to-object-translator
			  :from-type "llvm::ImmutablePass*"
			  :code "{_G(); return core::RP_Create_wrapped<llvmo::ImmutablePass_O,llvm::ImmutablePass*>(ptr);}"
			  )
			 )
 )
#|
(e-derived-class
 :lisp-name "DataLayout"
 :class-name "DataLayout"
 :base-classo-name "ImmutablePass_O"
 :ptr-type "llvm::DataLayout"
 :from-object-translators (list
			   (make-from-object-translator
			    :to-type "llvm::DataLayout*"
			    :code "{return object->as<llvmo::DataLayout_O>()->wrappedPtr();};"
			    )
			   )
 :to-object-translators (list
			 (make-to-object-translator
			  :from-type "llvm::DataLayout*"
			  :code "{_G(); return core::RP_Create_wrapped<llvmo::DataLayout_O,llvm::DataLayout*>(ptr);}"
			  )
			 )
 )
|#
#|(e-derived-class
 :lisp-name "TargetData"
 :class-name "TargetData"
 :base-classo-name "ImmutablePass_O"
 :ptr-type "llvm::TargetData"
 :from-object-translators (list
			   (make-from-object-translator
			    :to-type "llvm::TargetData*"
			    :code "{return object->as<llvmo::TargetData_O>()->wrappedPtr();};"
			    )
			   )
 :to-object-translators `(
			  ,(make-to-object-translator
			    :from-type "llvm::TargetData*"
			    :code "{_G(); return core::RP_Create_wrapped<llvmo::TargetData_O,llvm::TargetData*>(ptr);}"
			    )
			  ,(make-to-object-translator
			    :from-type "const llvm::TargetData*"
			    :code "{_G(); return core::RP_Create_wrapped<llvmo::TargetData_O,llvm::TargetData*>(const_cast<llvm::TargetData*>(ptr));}"
			    )
			  )
 )
|#

















(e-ptr-class
 :lisp-name "PassManagerBase"
 :class-name "PassManagerBase"
 :base-classo-name "core::ExternalObject_O"
 :ptr-type "llvm::PassManagerBase"
 :wrapped-method-names '(
;;		    "dump"
		    )
 :from-object-translators ( list
			    (make-from-object-translator
			     :to-type "llvm::PassManagerBase*"
			     :return-type "llvm::PassManagerBase*"
			     :code "{_G(); return (object->as<llvmo::PassManagerBase_O>()->wrappedPtr());};"
			     )
			    )

 :to-object-translators (list
			 (make-to-object-translator
			  :from-type "llvm::PassManagerBase*"
			  :code "{_G(); return core::RP_Create_wrapped<llvmo::PassManagerBase_O,llvm::PassManagerBase*>(ptr);}"
			  )
			 )

 )




#|
(e-derived-class
 :lisp-name "FunctionPassManager"
 :class-name "FunctionPassManager"
 :base-classo-name "PassManagerBase_O"
 :ptr-type "llvm::FunctionPassManager"
 :from-object-translators (list
			   (make-from-object-translator
			    :to-type "llvm::FunctionPassManager*"
			    :code "{return object->as<llvmo::FunctionPassManager_O>()->wrappedPtr();};"
			    )
			   )
 :to-object-translators (list
			 (make-to-object-translator
			  :from-type "llvm::FunctionPassManager*"
			  :code "{_G(); return core::RP_Create_wrapped<llvmo::FunctionPassManager_O,llvm::FunctionPassManager*>(ptr);}"
			  )
			 )
 )
|#





#|
(e-ptr-class
 :lisp-name "executionEngine"
 :class-name "ExecutionEngine"
 :base-classo-name "core::ExternalObject_O"
 :ptr-type "llvm::ExecutionEngine"
 :wrapped-method-names '(
;;		    "dump"
			 "getTargetData"
			 "getPointerToFunction"
		    )
 :from-object-translators ( list
			    (make-from-object-translator
			     :to-type "llvm::ExecutionEngine*"
			     :return-type "llvm::ExecutionEngine*"
			     :code "{_G(); return (object->as<llvmo::ExecutionEngine_O>()->wrappedPtr());};"
			     )
			    )

 :to-object-translators (list
			 (make-to-object-translator
			  :from-type "llvm::ExecutionEngine*"
			  :code "{_G(); return core::RP_Create_wrapped<llvmo::ExecutionEngine_O,llvm::ExecutionEngine*>(ptr);}"
			  )
			 )

 )
|#



#|
(e-ptr-class
 :lisp-name "module"
 :class-name "Module"
 :base-classo-name "core::ExternalObject_O"
 :ptr-type "llvm::Module"
 :init '( :lisp-args "(module-name context)"
	 :c-args "(llvm::StringRef module_name, LLVMContext_sp context)"
	 :c-code "
    llvm::LLVMContext& lc = llvm::getGlobalContext();
    ASSERT(&lc == context->wrappedPtr());
    self->_ptr = new llvm::Module(module_name,*(context->wrappedPtr()));"
	 )
 :wrapped-method-names '(
			 "dump"
			 "getFunction"
			 "getGlobalVariable"
			 "getNamedGlobal"
			 "getOrInsertGlobal"
		    )
 :from-object-translators ( list
			    (make-from-object-translator
			     :to-type "llvm::Module*"
			     :return-type "llvm::Module*"
			     :code "{_G(); return (object->as<llvmo::Module_O>()->wrappedPtr());};"
			     )
			    )

 :to-object-translators (list
			 (make-to-object-translator
			  :from-type "llvm::Module*"
			  :code "{_G(); return core::RP_Create_wrapped<llvmo::Module_O,llvm::Module*>(ptr);}"
			  )
			 )

 )
|#












(e-ptr-class
 :lisp-name "value"
 :class-name "Value"
 :ptr-type "llvm::Value"
 :from-object-translators (list
			   (make-from-object-translator
			    :to-type "llvm::Value*"
			    :code "{return object->as<llvmo::Value_O>()->wrappedPtr();};"
			    )
			   )
 :to-object-translators (list
			 (make-to-object-translator
			  :from-type "llvm::Value*"
			  :code "{_G(); return llvmo::Value_O::create(ptr);}"
			  )
			 )
 :other-function-prototypes "string __repr__() const;
bool valid() const;"
 :static-methods (list
		  (make-static-method
		   :c++-name "create"
		   :returns "Value_sp"
		   :arguments "(llvm::Value* ptr)"
		   :body "{_G(); return core::RP_Create_wrapped<Value_O,llvm::Value*>(ptr);}"
		   )
		  )

 :wrapped-method-names '(
			 "dump"
			 "getName"
			 "setName"
			 "getType"
			 )
 )

#|
(e-derived-class
 :lisp-name "MDNode"
 :class-name "MDNode"
 :base-classo-name "Value_O"
 :ptr-type "llvm::MDNode"
 :from-object-translators (list
			   (make-from-object-translator
			    :to-type "llvm::MDNode*"
			    :code "{return object->as<llvmo::MDNode_O>()->wrappedPtr();};"
			    )
			   )
 :to-object-translators (list
			 (make-to-object-translator
			  :from-type "llvm::MDNode*"
			  :code "{_G(); return core::RP_Create_wrapped<llvmo::MDNode_O,llvm::MDNode*>(ptr);}"
			  )
			 )
 )
|#


(e-derived-class
 :lisp-name "user"
 :class-name "User"
 :base-classo-name "Value_O"
 :ptr-type "llvm::User"
 )




#|
(e-derived-class
 :lisp-name "Instruction"
 :class-name "Instruction"
 :base-classo-name "User_O"
 :ptr-type "llvm::Instruction"
 :wrapped-method-names '(
			 "setMetaData"
			 "getFunction"
			 "getGlobalVariable"
			 "getNamedGlobal"
			 "getOrInsertGlobal"
		    )
 )


(e-derived-class
 :lisp-name "StoreInst"
 :class-name "StoreInst"
 :base-classo-name "Instruction_O"
 :ptr-type "llvm::StoreInst"
 :from-object-translators (list
			   (make-from-object-translator
			    :to-type "llvm::StoreInst*"
			    :code "{return object->as<llvmo::StoreInst_O>()->wrappedPtr();};"
			    )
			   )
 :to-object-translators (list
			 (make-to-object-translator
			  :from-type "llvm::StoreInst*"
			  :code "{_G(); return core::RP_Create_wrapped<llvmo::StoreInst_O,llvm::StoreInst*>(ptr);}"
			  )
			 )
 )




(e-derived-class
 :lisp-name "FenceInst"
 :class-name "FenceInst"
 :base-classo-name "Instruction_O"
 :ptr-type "llvm::FenceInst"
 :from-object-translators (list
			   (make-from-object-translator
			    :to-type "llvm::FenceInst*"
			    :code "{return object->as<llvmo::FenceInst_O>()->wrappedPtr();};"
			    )
			   )
 :to-object-translators (list
			 (make-to-object-translator
			  :from-type "llvm::FenceInst*"
			  :code "{_G(); return core::RP_Create_wrapped<llvmo::FenceInst_O,llvm::FenceInst*>(ptr);}"
			  )
			 )
 )


(e-derived-class
 :lisp-name "AtomicCmpXchgInst"
 :class-name "AtomicCmpXchgInst"
 :base-classo-name "Instruction_O"
 :ptr-type "llvm::AtomicCmpXchgInst"
 :from-object-translators (list
			   (make-from-object-translator
			    :to-type "llvm::AtomicCmpXchgInst*"
			    :code "{return object->as<llvmo::AtomicCmpXchgInst_O>()->wrappedPtr();};"
			    )
			   )
 :to-object-translators (list
			 (make-to-object-translator
			  :from-type "llvm::AtomicCmpXchgInst*"
			  :code "{_G(); return core::RP_Create_wrapped<llvmo::AtomicCmpXchgInst_O,llvm::AtomicCmpXchgInst*>(ptr);}"
			  )
			 )
 )



(e-derived-class
 :lisp-name "AtomicRMWInst"
 :class-name "AtomicRMWInst"
 :base-classo-name "Instruction_O"
 :ptr-type "llvm::AtomicRMWInst"
 :from-object-translators (list
			   (make-from-object-translator
			    :to-type "llvm::AtomicRMWInst*"
			    :code "{return object->as<llvmo::AtomicRMWInst_O>()->wrappedPtr();};"
			    )
			   )
 :to-object-translators (list
			 (make-to-object-translator
			  :from-type "llvm::AtomicRMWInst*"
			  :code "{_G(); return core::RP_Create_wrapped<llvmo::AtomicRMWInst_O,llvm::AtomicRMWInst*>(ptr);}"
			  )
			 )
 )



(e-derived-class
 :lisp-name "PHINode"
 :class-name "PHINode"
 :base-classo-name "Instruction_O"
 :ptr-type "llvm::PHINode"
 :from-object-translators (list
			   (make-from-object-translator
			    :to-type "llvm::PHINode*"
			    :code "{return object->as<llvmo::PHINode_O>()->wrappedPtr();};"
			    )
			   )
 :to-object-translators (list
			 (make-to-object-translator
			  :from-type "llvm::PHINode*"
			  :code "{_G(); return core::RP_Create_wrapped<llvmo::PHINode_O,llvm::PHINode*>(ptr);}"
			  )
			 )
 )


(e-derived-class
 :lisp-name "CallInst"
 :class-name "CallInst"
 :base-classo-name "Instruction_O"
 :ptr-type "llvm::CallInst"
 :from-object-translators (list
			   (make-from-object-translator
			    :to-type "llvm::CallInst*"
			    :code "{return object->as<llvmo::CallInst_O>()->wrappedPtr();};"
			    )
			   )
 :to-object-translators (list
			 (make-to-object-translator
			  :from-type "llvm::CallInst*"
			  :code "{_G(); return core::RP_Create_wrapped<llvmo::CallInst_O,llvm::CallInst*>(ptr);}"
			  )
			 )
 )


(e-derived-class
 :lisp-name "LandingPadInst"
 :class-name "LandingPadInst"
 :base-classo-name "Instruction_O"
 :ptr-type "llvm::LandingPadInst"
 :from-object-translators (list
			   (make-from-object-translator
			    :to-type "llvm::LandingPadInst*"
			    :code "{return object->as<llvmo::LandingPadInst_O>()->wrappedPtr();};"
			    )
			   )
 :to-object-translators (list
			 (make-to-object-translator
			  :from-type "llvm::LandingPadInst*"
			  :code "{_G(); return core::RP_Create_wrapped<llvmo::LandingPadInst_O,llvm::LandingPadInst*>(ptr);}"
			  )
			 )
 )



(e-derived-class
 :lisp-name "UnaryInstruction"
 :class-name "UnaryInstruction"
 :base-classo-name "Instruction_O"
 :ptr-type "llvm::UnaryInstruction"
 )


(e-derived-class
 :lisp-name "AllocaInst"
 :class-name "AllocaInst"
 :base-classo-name "UnaryInstruction_O"
 :ptr-type "llvm::AllocaInst"
 :from-object-translators (list
			   (make-from-object-translator
			    :to-type "llvm::AllocaInst*"
			    :code "{return object->as<llvmo::AllocaInst_O>()->wrappedPtr();};"
			    )
			   )
 :to-object-translators (list
			 (make-to-object-translator
			  :from-type "llvm::AllocaInst*"
			  :code "{_G(); return core::RP_Create_wrapped<llvmo::AllocaInst_O,llvm::AllocaInst*>(ptr);}"
			  )
			 )
 )


(e-derived-class
 :lisp-name "VAArgInst"
 :class-name "VAArgInst"
 :base-classo-name "UnaryInstruction_O"
 :ptr-type "llvm::VAArgInst"
 :from-object-translators (list
			   (make-from-object-translator
			    :to-type "llvm::VAArgInst*"
			    :code "{return object->as<llvmo::VAArgInst_O>()->wrappedPtr();};"
			    )
			   )
 :to-object-translators (list
			 (make-to-object-translator
			  :from-type "llvm::VAArgInst*"
			  :code "{_G(); return core::RP_Create_wrapped<llvmo::VAArgInst_O,llvm::VAArgInst*>(ptr);}"
			  )
			 )
 )





(e-derived-class
 :lisp-name "LoadInst"
 :class-name "LoadInst"
 :base-classo-name "UnaryInstruction_O"
 :ptr-type "llvm::LoadInst"
 :from-object-translators (list
			   (make-from-object-translator
			    :to-type "llvm::LoadInst*"
			    :code "{return object->as<llvmo::LoadInst_O>()->wrappedPtr();};"
			    )
			   )
 :to-object-translators (list
			 (make-to-object-translator
			  :from-type "llvm::LoadInst*"
			  :code "{_G(); return core::RP_Create_wrapped<llvmo::LoadInst_O,llvm::LoadInst*>(ptr);}"
			  )
			 )
 )







(e-derived-class
 :lisp-name "TerminatorInst"
 :class-name "TerminatorInst"
 :base-classo-name "Instruction_O"
 :ptr-type "llvm::TerminatorInst"
 )

(e-derived-class
 :lisp-name "BranchInst"
 :class-name "BranchInst"
 :base-classo-name "TerminatorInst_O"
 :ptr-type "llvm::BranchInst"
 :from-object-translators (list
			   (make-from-object-translator
			    :to-type "llvm::BranchInst*"
			    :code "{return object->as<llvmo::BranchInst_O>()->wrappedPtr();};"
			    )
			   )
 :to-object-translators (list
			 (make-to-object-translator
			  :from-type "llvm::BranchInst*"
			  :code "{_G(); return core::RP_Create_wrapped<llvmo::BranchInst_O,llvm::BranchInst*>(ptr);}"
			  )
			 )
)




(e-derived-class
 :lisp-name "SwitchInst"
 :class-name "SwitchInst"
 :base-classo-name "TerminatorInst_O"
 :ptr-type "llvm::SwitchInst"
 :from-object-translators (list
			   (make-from-object-translator
			    :to-type "llvm::SwitchInst*"
			    :code "{return object->as<llvmo::SwitchInst_O>()->wrappedPtr();};"
			    )
			   )
 :to-object-translators (list
			 (make-to-object-translator
			  :from-type "llvm::SwitchInst*"
			  :code "{_G(); return core::RP_Create_wrapped<llvmo::SwitchInst_O,llvm::SwitchInst*>(ptr);}"
			  )
			 )
)


(e-derived-class
 :lisp-name "IndirectBrInst"
 :class-name "IndirectBrInst"
 :base-classo-name "TerminatorInst_O"
 :ptr-type "llvm::IndirectBrInst"
 :from-object-translators (list
			   (make-from-object-translator
			    :to-type "llvm::IndirectBrInst*"
			    :code "{return object->as<llvmo::IndirectBrInst_O>()->wrappedPtr();};"
			    )
			   )
 :to-object-translators (list
			 (make-to-object-translator
			  :from-type "llvm::IndirectBrInst*"
			  :code "{_G(); return core::RP_Create_wrapped<llvmo::IndirectBrInst_O,llvm::IndirectBrInst*>(ptr);}"
			  )
			 )
)


(e-derived-class
 :lisp-name "InvokeInst"
 :class-name "InvokeInst"
 :base-classo-name "TerminatorInst_O"
 :ptr-type "llvm::InvokeInst"
 :from-object-translators (list
			   (make-from-object-translator
			    :to-type "llvm::InvokeInst*"
			    :code "{return object->as<llvmo::InvokeInst_O>()->wrappedPtr();};"
			    )
			   )
 :to-object-translators (list
			 (make-to-object-translator
			  :from-type "llvm::InvokeInst*"
			  :code "{_G(); return core::RP_Create_wrapped<llvmo::InvokeInst_O,llvm::InvokeInst*>(ptr);}"
			  )
			 )
)




(e-derived-class
 :lisp-name "ResumeInst"
 :class-name "ResumeInst"
 :base-classo-name "TerminatorInst_O"
 :ptr-type "llvm::ResumeInst"
 :from-object-translators (list
			   (make-from-object-translator
			    :to-type "llvm::ResumeInst*"
			    :code "{return object->as<llvmo::ResumeInst_O>()->wrappedPtr();};"
			    )
			   )
 :to-object-translators (list
			 (make-to-object-translator
			  :from-type "llvm::ResumeInst*"
			  :code "{_G(); return core::RP_Create_wrapped<llvmo::ResumeInst_O,llvm::ResumeInst*>(ptr);}"
			  )
			 )
)


(e-derived-class
 :lisp-name "UnreachableInst"
 :class-name "UnreachableInst"
 :base-classo-name "TerminatorInst_O"
 :ptr-type "llvm::UnreachableInst"
 :from-object-translators (list
			   (make-from-object-translator
			    :to-type "llvm::UnreachableInst*"
			    :code "{return object->as<llvmo::UnreachableInst_O>()->wrappedPtr();};"
			    )
			   )
 :to-object-translators (list
			 (make-to-object-translator
			  :from-type "llvm::UnreachableInst*"
			  :code "{_G(); return core::RP_Create_wrapped<llvmo::UnreachableInst_O,llvm::UnreachableInst*>(ptr);}"
			  )
			 )
)






(e-derived-class
 :lisp-name "ReturnInst"
 :class-name "ReturnInst"
 :base-classo-name "TerminatorInst_O"
 :ptr-type "llvm::ReturnInst"
 :from-object-translators (list
			   (make-from-object-translator
			    :to-type "llvm::ReturnInst*"
			    :code "{return object->as<llvmo::ReturnInst_O>()->wrappedPtr();};"
			    )
			   )
 :to-object-translators (list
			 (make-to-object-translator
			  :from-type "llvm::ReturnInst*"
			  :code "{_G(); return core::RP_Create_wrapped<llvmo::ReturnInst_O,llvm::ReturnInst*>(ptr);}"
			  )
			 )
 )

|#








#|
(e-derived-class
 :lisp-name "constant"
 :class-name "Constant"
 :base-classo-name "User_O"
 :ptr-type "llvm::Constant"
 :static-methods (list
		  (make-static-method
		   :c++-name "create"
		   :returns "Constant_sp"
		   :arguments "(llvm::Constant* ptr)"
		   :body "{_G(); return core::RP_Create_wrapped<Constant_O,llvm::Constant*>(ptr);}")
		  )
 :to-object-translators (list
			 (make-to-object-translator
			  :from-type "llvm::Constant*"
			  :code "{_G(); return core::RP_Create_wrapped<llvmo::Constant_O,llvm::Constant*>(ptr);};"
			  )
			 )

 )

|#




#|
(e-derived-class
 :lisp-name "global-value"
 :class-name "GlobalValue"
 :base-classo-name "Constant_O"
 :ptr-type "llvm::GlobalValue"
 )



(e-derived-class
 :lisp-name "GlobalVariable"
 :class-name "GlobalVariable"
 :base-classo-name "GlobalValue_O"
 :ptr-type "llvm::GlobalVariable"
 :from-object-translators (list
			   (make-from-object-translator
			    :to-type "llvm::GlobalVariable*"
			    :code "{return object->as<llvmo::GlobalVariable_O>()->wrappedPtr();};"
			    )
			   )
 :to-object-translators (list
			 (make-to-object-translator
			  :from-type "llvm::GlobalVariable*"
			  :code "{_G(); return core::RP_Create_wrapped<llvmo::GlobalVariable_O,llvm::GlobalVariable*>(ptr);}"
			  )
			 )
 )

|#
















(e-write-all-classes "llvmoExpose"
		     :header-prefix "
#include \"llvm/IR/DerivedTypes.h\"
#include \"llvm/IR/LLVMContext.h\"
#include \"llvm/IR/Module.h\"
#include \"llvm/ExecutionEngine/ExecutionEngine.h\"
#include \"llvm/ExecutionEngine/JIT.h\"
#include \"llvm/ExecutionEngine/Interpreter.h\"
#include \"llvm/Analysis/Verifier.h\"
#include \"llvm/Analysis/Passes.h\"
#include \"llvm/PassManager.h\"
#include \"llvm/Support/TargetSelect.h\"
#include \"llvm/Transforms/Scalar.h\"
#include \"llvm/IR/Instructions.h\"
#include \"llvm/IR/IRBuilder.h\"

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include \"core/foundation.h\"
#include \"core/object.h\"
#include \"core/metaClass.fwd.h\"
#include \"core/externalObject.h\"
#include \"llvmo/llvmoPackage.h\"
"
		     :code-prefix "
#define DEBUG_LEVEL_FULL


#include \"core/common.h\"
#include \"core/cons.h\"
#include \"core/evaluator.h\"
#include \"core/package.h\"
#include \"core/stringList.h\"
#include \"core/environment.h\"
#include \"core/builtInClass.h\"
#include \"core/str.h\"
#include \"llvmoExpose.h\"
#include \"llvmoExpose.generated.h\"
#include \"core/wrappers.h\"
#include \"core/external_wrappers.h\"
"





)



#|

(trace expose-wrapped-static-method)




|#


*current-namespace*
