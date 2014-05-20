// start
// define cpp macros: SET_SYMBOL, CREATE_CLASS, SET_CLASS, DEFINE_BASE_CLASSES, DEFINE_CLASS_NAMES, EXPOSE_TO_CANDO 
// define cpp macro: ALL_STAGES to get the effect of defining all of the macros above
// define cpp macro: EXPOSE_PYTHON to expose python
// Associating namespace(llvmo) with package(LlvmoPkg)
 // class DebugInfo_O : public core::T_O
 // class DIDescriptor_O : public DebugInfo_O
 // class DIScope_O : public DebugInfo_O
 // class DIArray_O : public DebugInfo_O
 // class DIFile_O : public DebugInfo_O
 // class DISubprogram_O : public DebugInfo_O
 // class DIType_O : public DebugInfo_O
 // class DIDerivedType_O : public DebugInfo_O
 // class DIBasicType_O : public DebugInfo_O
 // class DICompositeType_O : public DebugInfo_O
 // class DILexicalBlock_O : public DebugInfo_O
 // class DICompileUnit_O : public DebugInfo_O
 // class DIBuilder_O : public core::ExternalObject_O
 // class DebugLoc_O : public core::T_O
 // class InsertPoint_O : public core::T_O
 // class LLVMContext_O : public core::ExternalObject_O
 // class Linker_O : public core::ExternalObject_O
 // class Pass_O : public core::ExternalObject_O
 // class FunctionPass_O : public Pass_O
 // class ModulePass_O : public Pass_O
 // class ImmutablePass_O : public ModulePass_O
 // class PassManagerBase_O : public core::ExternalObject_O
 // class Value_O : public core::ExternalObject_O
 // class User_O : public Value_O
 // class Attribute_O : public core::T_O
 // class DataLayout_O : public core::ExternalObject_O
 // class Constant_O : public User_O
 // class ConstantArray_O : public Constant_O
 // class BlockAddress_O : public Constant_O
 // class ConstantDataSequential_O : public Constant_O
 // class ConstantDataArray_O : public ConstantDataSequential_O
 // class ConstantExpr_O : public Constant_O
 // class GlobalValue_O : public Constant_O
 // class GlobalVariable_O : public GlobalValue_O
 // class ExecutionEngine_O : public core::ExternalObject_O
 // class Module_O : public core::ExternalObject_O
 // class DataLayoutPass_O : public ImmutablePass_O
 // class FunctionPassManager_O : public PassManagerBase_O
 // class PassManager_O : public PassManagerBase_O
 // class EngineBuilder_O : public core::ExternalObject_O
 // class PassManagerBuilder_O : public core::ExternalObject_O
 // class APFloat_O : public core::ExternalObject_O
 // class APInt_O : public core::ExternalObject_O
 // class IRBuilderBase_O : public core::ExternalObject_O
 // class IRBuilder_O : public IRBuilderBase_O
 // class Instruction_O : public User_O
 // class StoreInst_O : public Instruction_O
 // class FenceInst_O : public Instruction_O
 // class AtomicCmpXchgInst_O : public Instruction_O
 // class AtomicRMWInst_O : public Instruction_O
 // class PHINode_O : public Instruction_O
 // class CallInst_O : public Instruction_O
 // class LandingPadInst_O : public Instruction_O
 // class UnaryInstruction_O : public Instruction_O
 // class AllocaInst_O : public UnaryInstruction_O
 // class VAArgInst_O : public UnaryInstruction_O
 // class LoadInst_O : public UnaryInstruction_O
 // class TerminatorInst_O : public Instruction_O
 // class BranchInst_O : public TerminatorInst_O
 // class SwitchInst_O : public TerminatorInst_O
 // class IndirectBrInst_O : public TerminatorInst_O
 // class InvokeInst_O : public TerminatorInst_O
 // class ResumeInst_O : public TerminatorInst_O
 // class UnreachableInst_O : public TerminatorInst_O
 // class ReturnInst_O : public TerminatorInst_O
 // class ConstantFP_O : public Constant_O
 // class ConstantInt_O : public Constant_O
 // class UndefValue_O : public Constant_O
 // class ConstantPointerNull_O : public Constant_O
 // class MDNode_O : public Value_O
 // class MDString_O : public Value_O
 // class NamedMDNode_O : public core::ExternalObject_O
 // class Function_O : public GlobalValue_O
 // class BasicBlock_O : public Value_O
 // class Argument_O : public Value_O
 // class Type_O : public core::ExternalObject_O
 // class FunctionType_O : public Type_O
 // class IntegerType_O : public Type_O
 // class CompositeType_O : public Type_O
 // class StructType_O : public CompositeType_O
 // class SequentialType_O : public CompositeType_O
 // class PointerType_O : public SequentialType_O
 // class ArrayType_O : public SequentialType_O
 // class VectorType_O : public SequentialType_O
// Associating namespace(llvmo) with package(LlvmoPkg)
#ifdef HEADER_INCLUDES
#include "llvmoExpose.h"
#include "debugInfoExpose.h"
#include "debugLoc.h"
#include "insertPoint.h"
#endif // HEADER_INCLUDES
#undef HEADER_INCLUDES
#if defined(SET_SYMBOL) || defined(ALL_STAGES)
// requires LOOKUP_SYMBOL(pkg,symbolName) be defined
llvmo::APFloat_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::APFloat_O::static_packageName(),llvmo::APFloat_O::static_className()));
llvmo::APInt_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::APInt_O::static_packageName(),llvmo::APInt_O::static_className()));
llvmo::Attribute_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::Attribute_O::static_packageName(),llvmo::Attribute_O::static_className()));
llvmo::DIBuilder_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::DIBuilder_O::static_packageName(),llvmo::DIBuilder_O::static_className()));
llvmo::DataLayout_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::DataLayout_O::static_packageName(),llvmo::DataLayout_O::static_className()));
llvmo::DebugInfo_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::DebugInfo_O::static_packageName(),llvmo::DebugInfo_O::static_className()));
llvmo::DebugLoc_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::DebugLoc_O::static_packageName(),llvmo::DebugLoc_O::static_className()));
llvmo::EngineBuilder_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::EngineBuilder_O::static_packageName(),llvmo::EngineBuilder_O::static_className()));
llvmo::ExecutionEngine_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::ExecutionEngine_O::static_packageName(),llvmo::ExecutionEngine_O::static_className()));
llvmo::IRBuilderBase_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::IRBuilderBase_O::static_packageName(),llvmo::IRBuilderBase_O::static_className()));
llvmo::InsertPoint_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::InsertPoint_O::static_packageName(),llvmo::InsertPoint_O::static_className()));
llvmo::LLVMContext_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::LLVMContext_O::static_packageName(),llvmo::LLVMContext_O::static_className()));
llvmo::Linker_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::Linker_O::static_packageName(),llvmo::Linker_O::static_className()));
llvmo::Module_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::Module_O::static_packageName(),llvmo::Module_O::static_className()));
llvmo::NamedMDNode_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::NamedMDNode_O::static_packageName(),llvmo::NamedMDNode_O::static_className()));
llvmo::PassManagerBase_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::PassManagerBase_O::static_packageName(),llvmo::PassManagerBase_O::static_className()));
llvmo::PassManagerBuilder_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::PassManagerBuilder_O::static_packageName(),llvmo::PassManagerBuilder_O::static_className()));
llvmo::Pass_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::Pass_O::static_packageName(),llvmo::Pass_O::static_className()));
llvmo::Type_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::Type_O::static_packageName(),llvmo::Type_O::static_className()));
llvmo::Value_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::Value_O::static_packageName(),llvmo::Value_O::static_className()));
llvmo::Argument_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::Argument_O::static_packageName(),llvmo::Argument_O::static_className()));
llvmo::BasicBlock_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::BasicBlock_O::static_packageName(),llvmo::BasicBlock_O::static_className()));
llvmo::CompositeType_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::CompositeType_O::static_packageName(),llvmo::CompositeType_O::static_className()));
llvmo::DIArray_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::DIArray_O::static_packageName(),llvmo::DIArray_O::static_className()));
llvmo::DIBasicType_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::DIBasicType_O::static_packageName(),llvmo::DIBasicType_O::static_className()));
llvmo::DICompileUnit_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::DICompileUnit_O::static_packageName(),llvmo::DICompileUnit_O::static_className()));
llvmo::DICompositeType_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::DICompositeType_O::static_packageName(),llvmo::DICompositeType_O::static_className()));
llvmo::DIDerivedType_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::DIDerivedType_O::static_packageName(),llvmo::DIDerivedType_O::static_className()));
llvmo::DIDescriptor_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::DIDescriptor_O::static_packageName(),llvmo::DIDescriptor_O::static_className()));
llvmo::DIFile_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::DIFile_O::static_packageName(),llvmo::DIFile_O::static_className()));
llvmo::DILexicalBlock_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::DILexicalBlock_O::static_packageName(),llvmo::DILexicalBlock_O::static_className()));
llvmo::DIScope_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::DIScope_O::static_packageName(),llvmo::DIScope_O::static_className()));
llvmo::DISubprogram_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::DISubprogram_O::static_packageName(),llvmo::DISubprogram_O::static_className()));
llvmo::DIType_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::DIType_O::static_packageName(),llvmo::DIType_O::static_className()));
llvmo::FunctionPassManager_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::FunctionPassManager_O::static_packageName(),llvmo::FunctionPassManager_O::static_className()));
llvmo::FunctionPass_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::FunctionPass_O::static_packageName(),llvmo::FunctionPass_O::static_className()));
llvmo::FunctionType_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::FunctionType_O::static_packageName(),llvmo::FunctionType_O::static_className()));
llvmo::IRBuilder_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::IRBuilder_O::static_packageName(),llvmo::IRBuilder_O::static_className()));
llvmo::IntegerType_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::IntegerType_O::static_packageName(),llvmo::IntegerType_O::static_className()));
llvmo::MDNode_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::MDNode_O::static_packageName(),llvmo::MDNode_O::static_className()));
llvmo::MDString_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::MDString_O::static_packageName(),llvmo::MDString_O::static_className()));
llvmo::ModulePass_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::ModulePass_O::static_packageName(),llvmo::ModulePass_O::static_className()));
llvmo::PassManager_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::PassManager_O::static_packageName(),llvmo::PassManager_O::static_className()));
llvmo::User_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::User_O::static_packageName(),llvmo::User_O::static_className()));
llvmo::Constant_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::Constant_O::static_packageName(),llvmo::Constant_O::static_className()));
llvmo::ImmutablePass_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::ImmutablePass_O::static_packageName(),llvmo::ImmutablePass_O::static_className()));
llvmo::Instruction_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::Instruction_O::static_packageName(),llvmo::Instruction_O::static_className()));
llvmo::SequentialType_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::SequentialType_O::static_packageName(),llvmo::SequentialType_O::static_className()));
llvmo::StructType_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::StructType_O::static_packageName(),llvmo::StructType_O::static_className()));
llvmo::ArrayType_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::ArrayType_O::static_packageName(),llvmo::ArrayType_O::static_className()));
llvmo::AtomicCmpXchgInst_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::AtomicCmpXchgInst_O::static_packageName(),llvmo::AtomicCmpXchgInst_O::static_className()));
llvmo::AtomicRMWInst_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::AtomicRMWInst_O::static_packageName(),llvmo::AtomicRMWInst_O::static_className()));
llvmo::BlockAddress_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::BlockAddress_O::static_packageName(),llvmo::BlockAddress_O::static_className()));
llvmo::CallInst_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::CallInst_O::static_packageName(),llvmo::CallInst_O::static_className()));
llvmo::ConstantArray_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::ConstantArray_O::static_packageName(),llvmo::ConstantArray_O::static_className()));
llvmo::ConstantDataSequential_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::ConstantDataSequential_O::static_packageName(),llvmo::ConstantDataSequential_O::static_className()));
llvmo::ConstantExpr_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::ConstantExpr_O::static_packageName(),llvmo::ConstantExpr_O::static_className()));
llvmo::ConstantFP_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::ConstantFP_O::static_packageName(),llvmo::ConstantFP_O::static_className()));
llvmo::ConstantInt_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::ConstantInt_O::static_packageName(),llvmo::ConstantInt_O::static_className()));
llvmo::ConstantPointerNull_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::ConstantPointerNull_O::static_packageName(),llvmo::ConstantPointerNull_O::static_className()));
llvmo::DataLayoutPass_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::DataLayoutPass_O::static_packageName(),llvmo::DataLayoutPass_O::static_className()));
llvmo::FenceInst_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::FenceInst_O::static_packageName(),llvmo::FenceInst_O::static_className()));
llvmo::GlobalValue_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::GlobalValue_O::static_packageName(),llvmo::GlobalValue_O::static_className()));
llvmo::LandingPadInst_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::LandingPadInst_O::static_packageName(),llvmo::LandingPadInst_O::static_className()));
llvmo::PHINode_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::PHINode_O::static_packageName(),llvmo::PHINode_O::static_className()));
llvmo::PointerType_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::PointerType_O::static_packageName(),llvmo::PointerType_O::static_className()));
llvmo::StoreInst_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::StoreInst_O::static_packageName(),llvmo::StoreInst_O::static_className()));
llvmo::TerminatorInst_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::TerminatorInst_O::static_packageName(),llvmo::TerminatorInst_O::static_className()));
llvmo::UnaryInstruction_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::UnaryInstruction_O::static_packageName(),llvmo::UnaryInstruction_O::static_className()));
llvmo::UndefValue_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::UndefValue_O::static_packageName(),llvmo::UndefValue_O::static_className()));
llvmo::VectorType_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::VectorType_O::static_packageName(),llvmo::VectorType_O::static_className()));
llvmo::AllocaInst_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::AllocaInst_O::static_packageName(),llvmo::AllocaInst_O::static_className()));
llvmo::BranchInst_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::BranchInst_O::static_packageName(),llvmo::BranchInst_O::static_className()));
llvmo::ConstantDataArray_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::ConstantDataArray_O::static_packageName(),llvmo::ConstantDataArray_O::static_className()));
llvmo::Function_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::Function_O::static_packageName(),llvmo::Function_O::static_className()));
llvmo::GlobalVariable_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::GlobalVariable_O::static_packageName(),llvmo::GlobalVariable_O::static_className()));
llvmo::IndirectBrInst_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::IndirectBrInst_O::static_packageName(),llvmo::IndirectBrInst_O::static_className()));
llvmo::InvokeInst_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::InvokeInst_O::static_packageName(),llvmo::InvokeInst_O::static_className()));
llvmo::LoadInst_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::LoadInst_O::static_packageName(),llvmo::LoadInst_O::static_className()));
llvmo::ResumeInst_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::ResumeInst_O::static_packageName(),llvmo::ResumeInst_O::static_className()));
llvmo::ReturnInst_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::ReturnInst_O::static_packageName(),llvmo::ReturnInst_O::static_className()));
llvmo::SwitchInst_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::SwitchInst_O::static_packageName(),llvmo::SwitchInst_O::static_className()));
llvmo::UnreachableInst_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::UnreachableInst_O::static_packageName(),llvmo::UnreachableInst_O::static_className()));
llvmo::VAArgInst_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::VAArgInst_O::static_packageName(),llvmo::VAArgInst_O::static_className()));
#endif // SET_SYMBOL
#undef SET_SYMBOL
#if defined(CREATE_CLASS) || defined(ALL_STAGES)

    LOG(BF("Creating class[classllvmo__APFloat_Oval]"));
    core::BuiltInClass_sp classllvmo__APFloat_Oval = core::BuiltInClass_O::create();
    classllvmo__APFloat_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__APFloat_Oval,_lisp,llvmo::APFloat_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::APFloat_O>::id,llvmo::APFloat_O::static_classSymbol());
    llvmo::APFloat_O::___staticClass = classllvmo__APFloat_Oval;
    llvmo::APFloat_O::static_Kind = gctools::GCInfo<llvmo::APFloat_O>::Kind;
    core::af_setf_findClass(classllvmo__APFloat_Oval,llvmo::APFloat_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::APFloat_O>* cb = new LispObjectAllocatorFunctor<llvmo::APFloat_O>();
        llvmo::APFloat_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::APFloat_O::static_className() % (void*)(llvmo::APFloat_O::static_allocator) );
    classllvmo__APFloat_Oval->setAllocator(llvmo::APFloat_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::APFloat_O::static_className() );
//        llvmo::APFloat_O::___staticDereferencedNilInstance = new llvmo::APFloat_O();
//        llvmo::APFloat_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::APFloat_O::___staticDereferencedUnboundInstance = new llvmo::APFloat_O();
//        llvmo::APFloat_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::APFloat_O::_nil = _Nil<llvmo::APFloat_O>();
//        llvmo::APFloat_O::_unbound = _Unbound<llvmo::APFloat_O>();
    }
//    classllvmo__APFloat_Oval->setSupportsSlots(llvmo::APFloat_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__APFloat_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__APInt_Oval]"));
    core::BuiltInClass_sp classllvmo__APInt_Oval = core::BuiltInClass_O::create();
    classllvmo__APInt_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__APInt_Oval,_lisp,llvmo::APInt_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::APInt_O>::id,llvmo::APInt_O::static_classSymbol());
    llvmo::APInt_O::___staticClass = classllvmo__APInt_Oval;
    llvmo::APInt_O::static_Kind = gctools::GCInfo<llvmo::APInt_O>::Kind;
    core::af_setf_findClass(classllvmo__APInt_Oval,llvmo::APInt_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::APInt_O>* cb = new LispObjectAllocatorFunctor<llvmo::APInt_O>();
        llvmo::APInt_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::APInt_O::static_className() % (void*)(llvmo::APInt_O::static_allocator) );
    classllvmo__APInt_Oval->setAllocator(llvmo::APInt_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::APInt_O::static_className() );
//        llvmo::APInt_O::___staticDereferencedNilInstance = new llvmo::APInt_O();
//        llvmo::APInt_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::APInt_O::___staticDereferencedUnboundInstance = new llvmo::APInt_O();
//        llvmo::APInt_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::APInt_O::_nil = _Nil<llvmo::APInt_O>();
//        llvmo::APInt_O::_unbound = _Unbound<llvmo::APInt_O>();
    }
//    classllvmo__APInt_Oval->setSupportsSlots(llvmo::APInt_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__APInt_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__Attribute_Oval]"));
    core::BuiltInClass_sp classllvmo__Attribute_Oval = core::BuiltInClass_O::create();
    classllvmo__Attribute_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__Attribute_Oval,_lisp,llvmo::Attribute_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::Attribute_O>::id,llvmo::Attribute_O::static_classSymbol());
    llvmo::Attribute_O::___staticClass = classllvmo__Attribute_Oval;
    llvmo::Attribute_O::static_Kind = gctools::GCInfo<llvmo::Attribute_O>::Kind;
    core::af_setf_findClass(classllvmo__Attribute_Oval,llvmo::Attribute_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::Attribute_O>* cb = new LispObjectAllocatorFunctor<llvmo::Attribute_O>();
        llvmo::Attribute_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::Attribute_O::static_className() % (void*)(llvmo::Attribute_O::static_allocator) );
    classllvmo__Attribute_Oval->setAllocator(llvmo::Attribute_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::Attribute_O::static_className() );
//        llvmo::Attribute_O::___staticDereferencedNilInstance = new llvmo::Attribute_O();
//        llvmo::Attribute_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::Attribute_O::___staticDereferencedUnboundInstance = new llvmo::Attribute_O();
//        llvmo::Attribute_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::Attribute_O::_nil = _Nil<llvmo::Attribute_O>();
//        llvmo::Attribute_O::_unbound = _Unbound<llvmo::Attribute_O>();
    }
//    classllvmo__Attribute_Oval->setSupportsSlots(llvmo::Attribute_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__Attribute_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__DIBuilder_Oval]"));
    core::BuiltInClass_sp classllvmo__DIBuilder_Oval = core::BuiltInClass_O::create();
    classllvmo__DIBuilder_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__DIBuilder_Oval,_lisp,llvmo::DIBuilder_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::DIBuilder_O>::id,llvmo::DIBuilder_O::static_classSymbol());
    llvmo::DIBuilder_O::___staticClass = classllvmo__DIBuilder_Oval;
    llvmo::DIBuilder_O::static_Kind = gctools::GCInfo<llvmo::DIBuilder_O>::Kind;
    core::af_setf_findClass(classllvmo__DIBuilder_Oval,llvmo::DIBuilder_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::DIBuilder_O>* cb = new LispObjectAllocatorFunctor<llvmo::DIBuilder_O>();
        llvmo::DIBuilder_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::DIBuilder_O::static_className() % (void*)(llvmo::DIBuilder_O::static_allocator) );
    classllvmo__DIBuilder_Oval->setAllocator(llvmo::DIBuilder_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::DIBuilder_O::static_className() );
//        llvmo::DIBuilder_O::___staticDereferencedNilInstance = new llvmo::DIBuilder_O();
//        llvmo::DIBuilder_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::DIBuilder_O::___staticDereferencedUnboundInstance = new llvmo::DIBuilder_O();
//        llvmo::DIBuilder_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::DIBuilder_O::_nil = _Nil<llvmo::DIBuilder_O>();
//        llvmo::DIBuilder_O::_unbound = _Unbound<llvmo::DIBuilder_O>();
    }
//    classllvmo__DIBuilder_Oval->setSupportsSlots(llvmo::DIBuilder_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__DIBuilder_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__DataLayout_Oval]"));
    core::BuiltInClass_sp classllvmo__DataLayout_Oval = core::BuiltInClass_O::create();
    classllvmo__DataLayout_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__DataLayout_Oval,_lisp,llvmo::DataLayout_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::DataLayout_O>::id,llvmo::DataLayout_O::static_classSymbol());
    llvmo::DataLayout_O::___staticClass = classllvmo__DataLayout_Oval;
    llvmo::DataLayout_O::static_Kind = gctools::GCInfo<llvmo::DataLayout_O>::Kind;
    core::af_setf_findClass(classllvmo__DataLayout_Oval,llvmo::DataLayout_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::DataLayout_O>* cb = new LispObjectAllocatorFunctor<llvmo::DataLayout_O>();
        llvmo::DataLayout_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::DataLayout_O::static_className() % (void*)(llvmo::DataLayout_O::static_allocator) );
    classllvmo__DataLayout_Oval->setAllocator(llvmo::DataLayout_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::DataLayout_O::static_className() );
//        llvmo::DataLayout_O::___staticDereferencedNilInstance = new llvmo::DataLayout_O();
//        llvmo::DataLayout_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::DataLayout_O::___staticDereferencedUnboundInstance = new llvmo::DataLayout_O();
//        llvmo::DataLayout_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::DataLayout_O::_nil = _Nil<llvmo::DataLayout_O>();
//        llvmo::DataLayout_O::_unbound = _Unbound<llvmo::DataLayout_O>();
    }
//    classllvmo__DataLayout_Oval->setSupportsSlots(llvmo::DataLayout_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__DataLayout_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__DebugInfo_Oval]"));
    core::BuiltInClass_sp classllvmo__DebugInfo_Oval = core::BuiltInClass_O::create();
    classllvmo__DebugInfo_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__DebugInfo_Oval,_lisp,llvmo::DebugInfo_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::DebugInfo_O>::id,llvmo::DebugInfo_O::static_classSymbol());
    llvmo::DebugInfo_O::___staticClass = classllvmo__DebugInfo_Oval;
    llvmo::DebugInfo_O::static_Kind = gctools::GCInfo<llvmo::DebugInfo_O>::Kind;
    core::af_setf_findClass(classllvmo__DebugInfo_Oval,llvmo::DebugInfo_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::DebugInfo_O>* cb = new LispObjectAllocatorFunctor<llvmo::DebugInfo_O>();
        llvmo::DebugInfo_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::DebugInfo_O::static_className() % (void*)(llvmo::DebugInfo_O::static_allocator) );
    classllvmo__DebugInfo_Oval->setAllocator(llvmo::DebugInfo_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::DebugInfo_O::static_className() );
//        llvmo::DebugInfo_O::___staticDereferencedNilInstance = new llvmo::DebugInfo_O();
//        llvmo::DebugInfo_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::DebugInfo_O::___staticDereferencedUnboundInstance = new llvmo::DebugInfo_O();
//        llvmo::DebugInfo_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::DebugInfo_O::_nil = _Nil<llvmo::DebugInfo_O>();
//        llvmo::DebugInfo_O::_unbound = _Unbound<llvmo::DebugInfo_O>();
    }
//    classllvmo__DebugInfo_Oval->setSupportsSlots(llvmo::DebugInfo_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__DebugInfo_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__DebugLoc_Oval]"));
    core::BuiltInClass_sp classllvmo__DebugLoc_Oval = core::BuiltInClass_O::create();
    classllvmo__DebugLoc_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__DebugLoc_Oval,_lisp,llvmo::DebugLoc_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::DebugLoc_O>::id,llvmo::DebugLoc_O::static_classSymbol());
    llvmo::DebugLoc_O::___staticClass = classllvmo__DebugLoc_Oval;
    llvmo::DebugLoc_O::static_Kind = gctools::GCInfo<llvmo::DebugLoc_O>::Kind;
    core::af_setf_findClass(classllvmo__DebugLoc_Oval,llvmo::DebugLoc_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::DebugLoc_O>* cb = new LispObjectAllocatorFunctor<llvmo::DebugLoc_O>();
        llvmo::DebugLoc_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::DebugLoc_O::static_className() % (void*)(llvmo::DebugLoc_O::static_allocator) );
    classllvmo__DebugLoc_Oval->setAllocator(llvmo::DebugLoc_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::DebugLoc_O::static_className() );
//        llvmo::DebugLoc_O::___staticDereferencedNilInstance = new llvmo::DebugLoc_O();
//        llvmo::DebugLoc_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::DebugLoc_O::___staticDereferencedUnboundInstance = new llvmo::DebugLoc_O();
//        llvmo::DebugLoc_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::DebugLoc_O::_nil = _Nil<llvmo::DebugLoc_O>();
//        llvmo::DebugLoc_O::_unbound = _Unbound<llvmo::DebugLoc_O>();
    }
//    classllvmo__DebugLoc_Oval->setSupportsSlots(llvmo::DebugLoc_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__DebugLoc_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__EngineBuilder_Oval]"));
    core::BuiltInClass_sp classllvmo__EngineBuilder_Oval = core::BuiltInClass_O::create();
    classllvmo__EngineBuilder_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__EngineBuilder_Oval,_lisp,llvmo::EngineBuilder_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::EngineBuilder_O>::id,llvmo::EngineBuilder_O::static_classSymbol());
    llvmo::EngineBuilder_O::___staticClass = classllvmo__EngineBuilder_Oval;
    llvmo::EngineBuilder_O::static_Kind = gctools::GCInfo<llvmo::EngineBuilder_O>::Kind;
    core::af_setf_findClass(classllvmo__EngineBuilder_Oval,llvmo::EngineBuilder_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::EngineBuilder_O>* cb = new LispObjectAllocatorFunctor<llvmo::EngineBuilder_O>();
        llvmo::EngineBuilder_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::EngineBuilder_O::static_className() % (void*)(llvmo::EngineBuilder_O::static_allocator) );
    classllvmo__EngineBuilder_Oval->setAllocator(llvmo::EngineBuilder_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::EngineBuilder_O::static_className() );
//        llvmo::EngineBuilder_O::___staticDereferencedNilInstance = new llvmo::EngineBuilder_O();
//        llvmo::EngineBuilder_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::EngineBuilder_O::___staticDereferencedUnboundInstance = new llvmo::EngineBuilder_O();
//        llvmo::EngineBuilder_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::EngineBuilder_O::_nil = _Nil<llvmo::EngineBuilder_O>();
//        llvmo::EngineBuilder_O::_unbound = _Unbound<llvmo::EngineBuilder_O>();
    }
//    classllvmo__EngineBuilder_Oval->setSupportsSlots(llvmo::EngineBuilder_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__EngineBuilder_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__ExecutionEngine_Oval]"));
    core::BuiltInClass_sp classllvmo__ExecutionEngine_Oval = core::BuiltInClass_O::create();
    classllvmo__ExecutionEngine_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ExecutionEngine_Oval,_lisp,llvmo::ExecutionEngine_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::ExecutionEngine_O>::id,llvmo::ExecutionEngine_O::static_classSymbol());
    llvmo::ExecutionEngine_O::___staticClass = classllvmo__ExecutionEngine_Oval;
    llvmo::ExecutionEngine_O::static_Kind = gctools::GCInfo<llvmo::ExecutionEngine_O>::Kind;
    core::af_setf_findClass(classllvmo__ExecutionEngine_Oval,llvmo::ExecutionEngine_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::ExecutionEngine_O>* cb = new LispObjectAllocatorFunctor<llvmo::ExecutionEngine_O>();
        llvmo::ExecutionEngine_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::ExecutionEngine_O::static_className() % (void*)(llvmo::ExecutionEngine_O::static_allocator) );
    classllvmo__ExecutionEngine_Oval->setAllocator(llvmo::ExecutionEngine_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::ExecutionEngine_O::static_className() );
//        llvmo::ExecutionEngine_O::___staticDereferencedNilInstance = new llvmo::ExecutionEngine_O();
//        llvmo::ExecutionEngine_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::ExecutionEngine_O::___staticDereferencedUnboundInstance = new llvmo::ExecutionEngine_O();
//        llvmo::ExecutionEngine_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::ExecutionEngine_O::_nil = _Nil<llvmo::ExecutionEngine_O>();
//        llvmo::ExecutionEngine_O::_unbound = _Unbound<llvmo::ExecutionEngine_O>();
    }
//    classllvmo__ExecutionEngine_Oval->setSupportsSlots(llvmo::ExecutionEngine_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__ExecutionEngine_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__IRBuilderBase_Oval]"));
    core::BuiltInClass_sp classllvmo__IRBuilderBase_Oval = core::BuiltInClass_O::create();
    classllvmo__IRBuilderBase_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__IRBuilderBase_Oval,_lisp,llvmo::IRBuilderBase_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::IRBuilderBase_O>::id,llvmo::IRBuilderBase_O::static_classSymbol());
    llvmo::IRBuilderBase_O::___staticClass = classllvmo__IRBuilderBase_Oval;
    llvmo::IRBuilderBase_O::static_Kind = gctools::GCInfo<llvmo::IRBuilderBase_O>::Kind;
    core::af_setf_findClass(classllvmo__IRBuilderBase_Oval,llvmo::IRBuilderBase_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::IRBuilderBase_O>* cb = new LispObjectAllocatorFunctor<llvmo::IRBuilderBase_O>();
        llvmo::IRBuilderBase_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::IRBuilderBase_O::static_className() % (void*)(llvmo::IRBuilderBase_O::static_allocator) );
    classllvmo__IRBuilderBase_Oval->setAllocator(llvmo::IRBuilderBase_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::IRBuilderBase_O::static_className() );
//        llvmo::IRBuilderBase_O::___staticDereferencedNilInstance = new llvmo::IRBuilderBase_O();
//        llvmo::IRBuilderBase_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::IRBuilderBase_O::___staticDereferencedUnboundInstance = new llvmo::IRBuilderBase_O();
//        llvmo::IRBuilderBase_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::IRBuilderBase_O::_nil = _Nil<llvmo::IRBuilderBase_O>();
//        llvmo::IRBuilderBase_O::_unbound = _Unbound<llvmo::IRBuilderBase_O>();
    }
//    classllvmo__IRBuilderBase_Oval->setSupportsSlots(llvmo::IRBuilderBase_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__IRBuilderBase_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__InsertPoint_Oval]"));
    core::BuiltInClass_sp classllvmo__InsertPoint_Oval = core::BuiltInClass_O::create();
    classllvmo__InsertPoint_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__InsertPoint_Oval,_lisp,llvmo::InsertPoint_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::InsertPoint_O>::id,llvmo::InsertPoint_O::static_classSymbol());
    llvmo::InsertPoint_O::___staticClass = classllvmo__InsertPoint_Oval;
    llvmo::InsertPoint_O::static_Kind = gctools::GCInfo<llvmo::InsertPoint_O>::Kind;
    core::af_setf_findClass(classllvmo__InsertPoint_Oval,llvmo::InsertPoint_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::InsertPoint_O>* cb = new LispObjectAllocatorFunctor<llvmo::InsertPoint_O>();
        llvmo::InsertPoint_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::InsertPoint_O::static_className() % (void*)(llvmo::InsertPoint_O::static_allocator) );
    classllvmo__InsertPoint_Oval->setAllocator(llvmo::InsertPoint_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::InsertPoint_O::static_className() );
//        llvmo::InsertPoint_O::___staticDereferencedNilInstance = new llvmo::InsertPoint_O();
//        llvmo::InsertPoint_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::InsertPoint_O::___staticDereferencedUnboundInstance = new llvmo::InsertPoint_O();
//        llvmo::InsertPoint_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::InsertPoint_O::_nil = _Nil<llvmo::InsertPoint_O>();
//        llvmo::InsertPoint_O::_unbound = _Unbound<llvmo::InsertPoint_O>();
    }
//    classllvmo__InsertPoint_Oval->setSupportsSlots(llvmo::InsertPoint_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__InsertPoint_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__LLVMContext_Oval]"));
    core::BuiltInClass_sp classllvmo__LLVMContext_Oval = core::BuiltInClass_O::create();
    classllvmo__LLVMContext_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__LLVMContext_Oval,_lisp,llvmo::LLVMContext_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::LLVMContext_O>::id,llvmo::LLVMContext_O::static_classSymbol());
    llvmo::LLVMContext_O::___staticClass = classllvmo__LLVMContext_Oval;
    llvmo::LLVMContext_O::static_Kind = gctools::GCInfo<llvmo::LLVMContext_O>::Kind;
    core::af_setf_findClass(classllvmo__LLVMContext_Oval,llvmo::LLVMContext_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::LLVMContext_O>* cb = new LispObjectAllocatorFunctor<llvmo::LLVMContext_O>();
        llvmo::LLVMContext_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::LLVMContext_O::static_className() % (void*)(llvmo::LLVMContext_O::static_allocator) );
    classllvmo__LLVMContext_Oval->setAllocator(llvmo::LLVMContext_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::LLVMContext_O::static_className() );
//        llvmo::LLVMContext_O::___staticDereferencedNilInstance = new llvmo::LLVMContext_O();
//        llvmo::LLVMContext_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::LLVMContext_O::___staticDereferencedUnboundInstance = new llvmo::LLVMContext_O();
//        llvmo::LLVMContext_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::LLVMContext_O::_nil = _Nil<llvmo::LLVMContext_O>();
//        llvmo::LLVMContext_O::_unbound = _Unbound<llvmo::LLVMContext_O>();
    }
//    classllvmo__LLVMContext_Oval->setSupportsSlots(llvmo::LLVMContext_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__LLVMContext_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__Linker_Oval]"));
    core::BuiltInClass_sp classllvmo__Linker_Oval = core::BuiltInClass_O::create();
    classllvmo__Linker_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__Linker_Oval,_lisp,llvmo::Linker_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::Linker_O>::id,llvmo::Linker_O::static_classSymbol());
    llvmo::Linker_O::___staticClass = classllvmo__Linker_Oval;
    llvmo::Linker_O::static_Kind = gctools::GCInfo<llvmo::Linker_O>::Kind;
    core::af_setf_findClass(classllvmo__Linker_Oval,llvmo::Linker_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::Linker_O>* cb = new LispObjectAllocatorFunctor<llvmo::Linker_O>();
        llvmo::Linker_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::Linker_O::static_className() % (void*)(llvmo::Linker_O::static_allocator) );
    classllvmo__Linker_Oval->setAllocator(llvmo::Linker_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::Linker_O::static_className() );
//        llvmo::Linker_O::___staticDereferencedNilInstance = new llvmo::Linker_O();
//        llvmo::Linker_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::Linker_O::___staticDereferencedUnboundInstance = new llvmo::Linker_O();
//        llvmo::Linker_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::Linker_O::_nil = _Nil<llvmo::Linker_O>();
//        llvmo::Linker_O::_unbound = _Unbound<llvmo::Linker_O>();
    }
//    classllvmo__Linker_Oval->setSupportsSlots(llvmo::Linker_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__Linker_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__Module_Oval]"));
    core::BuiltInClass_sp classllvmo__Module_Oval = core::BuiltInClass_O::create();
    classllvmo__Module_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__Module_Oval,_lisp,llvmo::Module_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::Module_O>::id,llvmo::Module_O::static_classSymbol());
    llvmo::Module_O::___staticClass = classllvmo__Module_Oval;
    llvmo::Module_O::static_Kind = gctools::GCInfo<llvmo::Module_O>::Kind;
    core::af_setf_findClass(classllvmo__Module_Oval,llvmo::Module_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::Module_O>* cb = new LispObjectAllocatorFunctor<llvmo::Module_O>();
        llvmo::Module_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::Module_O::static_className() % (void*)(llvmo::Module_O::static_allocator) );
    classllvmo__Module_Oval->setAllocator(llvmo::Module_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::Module_O::static_className() );
//        llvmo::Module_O::___staticDereferencedNilInstance = new llvmo::Module_O();
//        llvmo::Module_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::Module_O::___staticDereferencedUnboundInstance = new llvmo::Module_O();
//        llvmo::Module_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::Module_O::_nil = _Nil<llvmo::Module_O>();
//        llvmo::Module_O::_unbound = _Unbound<llvmo::Module_O>();
    }
//    classllvmo__Module_Oval->setSupportsSlots(llvmo::Module_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__Module_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__NamedMDNode_Oval]"));
    core::BuiltInClass_sp classllvmo__NamedMDNode_Oval = core::BuiltInClass_O::create();
    classllvmo__NamedMDNode_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__NamedMDNode_Oval,_lisp,llvmo::NamedMDNode_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::NamedMDNode_O>::id,llvmo::NamedMDNode_O::static_classSymbol());
    llvmo::NamedMDNode_O::___staticClass = classllvmo__NamedMDNode_Oval;
    llvmo::NamedMDNode_O::static_Kind = gctools::GCInfo<llvmo::NamedMDNode_O>::Kind;
    core::af_setf_findClass(classllvmo__NamedMDNode_Oval,llvmo::NamedMDNode_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::NamedMDNode_O>* cb = new LispObjectAllocatorFunctor<llvmo::NamedMDNode_O>();
        llvmo::NamedMDNode_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::NamedMDNode_O::static_className() % (void*)(llvmo::NamedMDNode_O::static_allocator) );
    classllvmo__NamedMDNode_Oval->setAllocator(llvmo::NamedMDNode_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::NamedMDNode_O::static_className() );
//        llvmo::NamedMDNode_O::___staticDereferencedNilInstance = new llvmo::NamedMDNode_O();
//        llvmo::NamedMDNode_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::NamedMDNode_O::___staticDereferencedUnboundInstance = new llvmo::NamedMDNode_O();
//        llvmo::NamedMDNode_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::NamedMDNode_O::_nil = _Nil<llvmo::NamedMDNode_O>();
//        llvmo::NamedMDNode_O::_unbound = _Unbound<llvmo::NamedMDNode_O>();
    }
//    classllvmo__NamedMDNode_Oval->setSupportsSlots(llvmo::NamedMDNode_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__NamedMDNode_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__PassManagerBase_Oval]"));
    core::BuiltInClass_sp classllvmo__PassManagerBase_Oval = core::BuiltInClass_O::create();
    classllvmo__PassManagerBase_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__PassManagerBase_Oval,_lisp,llvmo::PassManagerBase_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::PassManagerBase_O>::id,llvmo::PassManagerBase_O::static_classSymbol());
    llvmo::PassManagerBase_O::___staticClass = classllvmo__PassManagerBase_Oval;
    llvmo::PassManagerBase_O::static_Kind = gctools::GCInfo<llvmo::PassManagerBase_O>::Kind;
    core::af_setf_findClass(classllvmo__PassManagerBase_Oval,llvmo::PassManagerBase_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::PassManagerBase_O>* cb = new LispObjectAllocatorFunctor<llvmo::PassManagerBase_O>();
        llvmo::PassManagerBase_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::PassManagerBase_O::static_className() % (void*)(llvmo::PassManagerBase_O::static_allocator) );
    classllvmo__PassManagerBase_Oval->setAllocator(llvmo::PassManagerBase_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::PassManagerBase_O::static_className() );
//        llvmo::PassManagerBase_O::___staticDereferencedNilInstance = new llvmo::PassManagerBase_O();
//        llvmo::PassManagerBase_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::PassManagerBase_O::___staticDereferencedUnboundInstance = new llvmo::PassManagerBase_O();
//        llvmo::PassManagerBase_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::PassManagerBase_O::_nil = _Nil<llvmo::PassManagerBase_O>();
//        llvmo::PassManagerBase_O::_unbound = _Unbound<llvmo::PassManagerBase_O>();
    }
//    classllvmo__PassManagerBase_Oval->setSupportsSlots(llvmo::PassManagerBase_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__PassManagerBase_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__PassManagerBuilder_Oval]"));
    core::BuiltInClass_sp classllvmo__PassManagerBuilder_Oval = core::BuiltInClass_O::create();
    classllvmo__PassManagerBuilder_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__PassManagerBuilder_Oval,_lisp,llvmo::PassManagerBuilder_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::PassManagerBuilder_O>::id,llvmo::PassManagerBuilder_O::static_classSymbol());
    llvmo::PassManagerBuilder_O::___staticClass = classllvmo__PassManagerBuilder_Oval;
    llvmo::PassManagerBuilder_O::static_Kind = gctools::GCInfo<llvmo::PassManagerBuilder_O>::Kind;
    core::af_setf_findClass(classllvmo__PassManagerBuilder_Oval,llvmo::PassManagerBuilder_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::PassManagerBuilder_O>* cb = new LispObjectAllocatorFunctor<llvmo::PassManagerBuilder_O>();
        llvmo::PassManagerBuilder_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::PassManagerBuilder_O::static_className() % (void*)(llvmo::PassManagerBuilder_O::static_allocator) );
    classllvmo__PassManagerBuilder_Oval->setAllocator(llvmo::PassManagerBuilder_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::PassManagerBuilder_O::static_className() );
//        llvmo::PassManagerBuilder_O::___staticDereferencedNilInstance = new llvmo::PassManagerBuilder_O();
//        llvmo::PassManagerBuilder_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::PassManagerBuilder_O::___staticDereferencedUnboundInstance = new llvmo::PassManagerBuilder_O();
//        llvmo::PassManagerBuilder_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::PassManagerBuilder_O::_nil = _Nil<llvmo::PassManagerBuilder_O>();
//        llvmo::PassManagerBuilder_O::_unbound = _Unbound<llvmo::PassManagerBuilder_O>();
    }
//    classllvmo__PassManagerBuilder_Oval->setSupportsSlots(llvmo::PassManagerBuilder_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__PassManagerBuilder_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__Pass_Oval]"));
    core::BuiltInClass_sp classllvmo__Pass_Oval = core::BuiltInClass_O::create();
    classllvmo__Pass_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__Pass_Oval,_lisp,llvmo::Pass_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::Pass_O>::id,llvmo::Pass_O::static_classSymbol());
    llvmo::Pass_O::___staticClass = classllvmo__Pass_Oval;
    llvmo::Pass_O::static_Kind = gctools::GCInfo<llvmo::Pass_O>::Kind;
    core::af_setf_findClass(classllvmo__Pass_Oval,llvmo::Pass_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::Pass_O>* cb = new LispObjectAllocatorFunctor<llvmo::Pass_O>();
        llvmo::Pass_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::Pass_O::static_className() % (void*)(llvmo::Pass_O::static_allocator) );
    classllvmo__Pass_Oval->setAllocator(llvmo::Pass_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::Pass_O::static_className() );
//        llvmo::Pass_O::___staticDereferencedNilInstance = new llvmo::Pass_O();
//        llvmo::Pass_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::Pass_O::___staticDereferencedUnboundInstance = new llvmo::Pass_O();
//        llvmo::Pass_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::Pass_O::_nil = _Nil<llvmo::Pass_O>();
//        llvmo::Pass_O::_unbound = _Unbound<llvmo::Pass_O>();
    }
//    classllvmo__Pass_Oval->setSupportsSlots(llvmo::Pass_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__Pass_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__Type_Oval]"));
    core::BuiltInClass_sp classllvmo__Type_Oval = core::BuiltInClass_O::create();
    classllvmo__Type_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__Type_Oval,_lisp,llvmo::Type_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::Type_O>::id,llvmo::Type_O::static_classSymbol());
    llvmo::Type_O::___staticClass = classllvmo__Type_Oval;
    llvmo::Type_O::static_Kind = gctools::GCInfo<llvmo::Type_O>::Kind;
    core::af_setf_findClass(classllvmo__Type_Oval,llvmo::Type_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::Type_O>* cb = new LispObjectAllocatorFunctor<llvmo::Type_O>();
        llvmo::Type_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::Type_O::static_className() % (void*)(llvmo::Type_O::static_allocator) );
    classllvmo__Type_Oval->setAllocator(llvmo::Type_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::Type_O::static_className() );
//        llvmo::Type_O::___staticDereferencedNilInstance = new llvmo::Type_O();
//        llvmo::Type_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::Type_O::___staticDereferencedUnboundInstance = new llvmo::Type_O();
//        llvmo::Type_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::Type_O::_nil = _Nil<llvmo::Type_O>();
//        llvmo::Type_O::_unbound = _Unbound<llvmo::Type_O>();
    }
//    classllvmo__Type_Oval->setSupportsSlots(llvmo::Type_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__Type_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__Value_Oval]"));
    core::BuiltInClass_sp classllvmo__Value_Oval = core::BuiltInClass_O::create();
    classllvmo__Value_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__Value_Oval,_lisp,llvmo::Value_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::Value_O>::id,llvmo::Value_O::static_classSymbol());
    llvmo::Value_O::___staticClass = classllvmo__Value_Oval;
    llvmo::Value_O::static_Kind = gctools::GCInfo<llvmo::Value_O>::Kind;
    core::af_setf_findClass(classllvmo__Value_Oval,llvmo::Value_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::Value_O>* cb = new LispObjectAllocatorFunctor<llvmo::Value_O>();
        llvmo::Value_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::Value_O::static_className() % (void*)(llvmo::Value_O::static_allocator) );
    classllvmo__Value_Oval->setAllocator(llvmo::Value_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::Value_O::static_className() );
//        llvmo::Value_O::___staticDereferencedNilInstance = new llvmo::Value_O();
//        llvmo::Value_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::Value_O::___staticDereferencedUnboundInstance = new llvmo::Value_O();
//        llvmo::Value_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::Value_O::_nil = _Nil<llvmo::Value_O>();
//        llvmo::Value_O::_unbound = _Unbound<llvmo::Value_O>();
    }
//    classllvmo__Value_Oval->setSupportsSlots(llvmo::Value_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__Value_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__Argument_Oval]"));
    core::BuiltInClass_sp classllvmo__Argument_Oval = core::BuiltInClass_O::create();
    classllvmo__Argument_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__Argument_Oval,_lisp,llvmo::Argument_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::Argument_O>::id,llvmo::Argument_O::static_classSymbol());
    llvmo::Argument_O::___staticClass = classllvmo__Argument_Oval;
    llvmo::Argument_O::static_Kind = gctools::GCInfo<llvmo::Argument_O>::Kind;
    core::af_setf_findClass(classllvmo__Argument_Oval,llvmo::Argument_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::Argument_O>* cb = new LispObjectAllocatorFunctor<llvmo::Argument_O>();
        llvmo::Argument_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::Argument_O::static_className() % (void*)(llvmo::Argument_O::static_allocator) );
    classllvmo__Argument_Oval->setAllocator(llvmo::Argument_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::Argument_O::static_className() );
//        llvmo::Argument_O::___staticDereferencedNilInstance = new llvmo::Argument_O();
//        llvmo::Argument_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::Argument_O::___staticDereferencedUnboundInstance = new llvmo::Argument_O();
//        llvmo::Argument_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::Argument_O::_nil = _Nil<llvmo::Argument_O>();
//        llvmo::Argument_O::_unbound = _Unbound<llvmo::Argument_O>();
    }
//    classllvmo__Argument_Oval->setSupportsSlots(llvmo::Argument_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__Argument_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__BasicBlock_Oval]"));
    core::BuiltInClass_sp classllvmo__BasicBlock_Oval = core::BuiltInClass_O::create();
    classllvmo__BasicBlock_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__BasicBlock_Oval,_lisp,llvmo::BasicBlock_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::BasicBlock_O>::id,llvmo::BasicBlock_O::static_classSymbol());
    llvmo::BasicBlock_O::___staticClass = classllvmo__BasicBlock_Oval;
    llvmo::BasicBlock_O::static_Kind = gctools::GCInfo<llvmo::BasicBlock_O>::Kind;
    core::af_setf_findClass(classllvmo__BasicBlock_Oval,llvmo::BasicBlock_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::BasicBlock_O>* cb = new LispObjectAllocatorFunctor<llvmo::BasicBlock_O>();
        llvmo::BasicBlock_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::BasicBlock_O::static_className() % (void*)(llvmo::BasicBlock_O::static_allocator) );
    classllvmo__BasicBlock_Oval->setAllocator(llvmo::BasicBlock_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::BasicBlock_O::static_className() );
//        llvmo::BasicBlock_O::___staticDereferencedNilInstance = new llvmo::BasicBlock_O();
//        llvmo::BasicBlock_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::BasicBlock_O::___staticDereferencedUnboundInstance = new llvmo::BasicBlock_O();
//        llvmo::BasicBlock_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::BasicBlock_O::_nil = _Nil<llvmo::BasicBlock_O>();
//        llvmo::BasicBlock_O::_unbound = _Unbound<llvmo::BasicBlock_O>();
    }
//    classllvmo__BasicBlock_Oval->setSupportsSlots(llvmo::BasicBlock_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__BasicBlock_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__CompositeType_Oval]"));
    core::BuiltInClass_sp classllvmo__CompositeType_Oval = core::BuiltInClass_O::create();
    classllvmo__CompositeType_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__CompositeType_Oval,_lisp,llvmo::CompositeType_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::CompositeType_O>::id,llvmo::CompositeType_O::static_classSymbol());
    llvmo::CompositeType_O::___staticClass = classllvmo__CompositeType_Oval;
    llvmo::CompositeType_O::static_Kind = gctools::GCInfo<llvmo::CompositeType_O>::Kind;
    core::af_setf_findClass(classllvmo__CompositeType_Oval,llvmo::CompositeType_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::CompositeType_O>* cb = new LispObjectAllocatorFunctor<llvmo::CompositeType_O>();
        llvmo::CompositeType_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::CompositeType_O::static_className() % (void*)(llvmo::CompositeType_O::static_allocator) );
    classllvmo__CompositeType_Oval->setAllocator(llvmo::CompositeType_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::CompositeType_O::static_className() );
//        llvmo::CompositeType_O::___staticDereferencedNilInstance = new llvmo::CompositeType_O();
//        llvmo::CompositeType_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::CompositeType_O::___staticDereferencedUnboundInstance = new llvmo::CompositeType_O();
//        llvmo::CompositeType_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::CompositeType_O::_nil = _Nil<llvmo::CompositeType_O>();
//        llvmo::CompositeType_O::_unbound = _Unbound<llvmo::CompositeType_O>();
    }
//    classllvmo__CompositeType_Oval->setSupportsSlots(llvmo::CompositeType_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__CompositeType_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__DIArray_Oval]"));
    core::BuiltInClass_sp classllvmo__DIArray_Oval = core::BuiltInClass_O::create();
    classllvmo__DIArray_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__DIArray_Oval,_lisp,llvmo::DIArray_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::DIArray_O>::id,llvmo::DIArray_O::static_classSymbol());
    llvmo::DIArray_O::___staticClass = classllvmo__DIArray_Oval;
    llvmo::DIArray_O::static_Kind = gctools::GCInfo<llvmo::DIArray_O>::Kind;
    core::af_setf_findClass(classllvmo__DIArray_Oval,llvmo::DIArray_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::DIArray_O>* cb = new LispObjectAllocatorFunctor<llvmo::DIArray_O>();
        llvmo::DIArray_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::DIArray_O::static_className() % (void*)(llvmo::DIArray_O::static_allocator) );
    classllvmo__DIArray_Oval->setAllocator(llvmo::DIArray_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::DIArray_O::static_className() );
//        llvmo::DIArray_O::___staticDereferencedNilInstance = new llvmo::DIArray_O();
//        llvmo::DIArray_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::DIArray_O::___staticDereferencedUnboundInstance = new llvmo::DIArray_O();
//        llvmo::DIArray_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::DIArray_O::_nil = _Nil<llvmo::DIArray_O>();
//        llvmo::DIArray_O::_unbound = _Unbound<llvmo::DIArray_O>();
    }
//    classllvmo__DIArray_Oval->setSupportsSlots(llvmo::DIArray_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__DIArray_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__DIBasicType_Oval]"));
    core::BuiltInClass_sp classllvmo__DIBasicType_Oval = core::BuiltInClass_O::create();
    classllvmo__DIBasicType_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__DIBasicType_Oval,_lisp,llvmo::DIBasicType_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::DIBasicType_O>::id,llvmo::DIBasicType_O::static_classSymbol());
    llvmo::DIBasicType_O::___staticClass = classllvmo__DIBasicType_Oval;
    llvmo::DIBasicType_O::static_Kind = gctools::GCInfo<llvmo::DIBasicType_O>::Kind;
    core::af_setf_findClass(classllvmo__DIBasicType_Oval,llvmo::DIBasicType_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::DIBasicType_O>* cb = new LispObjectAllocatorFunctor<llvmo::DIBasicType_O>();
        llvmo::DIBasicType_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::DIBasicType_O::static_className() % (void*)(llvmo::DIBasicType_O::static_allocator) );
    classllvmo__DIBasicType_Oval->setAllocator(llvmo::DIBasicType_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::DIBasicType_O::static_className() );
//        llvmo::DIBasicType_O::___staticDereferencedNilInstance = new llvmo::DIBasicType_O();
//        llvmo::DIBasicType_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::DIBasicType_O::___staticDereferencedUnboundInstance = new llvmo::DIBasicType_O();
//        llvmo::DIBasicType_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::DIBasicType_O::_nil = _Nil<llvmo::DIBasicType_O>();
//        llvmo::DIBasicType_O::_unbound = _Unbound<llvmo::DIBasicType_O>();
    }
//    classllvmo__DIBasicType_Oval->setSupportsSlots(llvmo::DIBasicType_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__DIBasicType_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__DICompileUnit_Oval]"));
    core::BuiltInClass_sp classllvmo__DICompileUnit_Oval = core::BuiltInClass_O::create();
    classllvmo__DICompileUnit_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__DICompileUnit_Oval,_lisp,llvmo::DICompileUnit_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::DICompileUnit_O>::id,llvmo::DICompileUnit_O::static_classSymbol());
    llvmo::DICompileUnit_O::___staticClass = classllvmo__DICompileUnit_Oval;
    llvmo::DICompileUnit_O::static_Kind = gctools::GCInfo<llvmo::DICompileUnit_O>::Kind;
    core::af_setf_findClass(classllvmo__DICompileUnit_Oval,llvmo::DICompileUnit_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::DICompileUnit_O>* cb = new LispObjectAllocatorFunctor<llvmo::DICompileUnit_O>();
        llvmo::DICompileUnit_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::DICompileUnit_O::static_className() % (void*)(llvmo::DICompileUnit_O::static_allocator) );
    classllvmo__DICompileUnit_Oval->setAllocator(llvmo::DICompileUnit_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::DICompileUnit_O::static_className() );
//        llvmo::DICompileUnit_O::___staticDereferencedNilInstance = new llvmo::DICompileUnit_O();
//        llvmo::DICompileUnit_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::DICompileUnit_O::___staticDereferencedUnboundInstance = new llvmo::DICompileUnit_O();
//        llvmo::DICompileUnit_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::DICompileUnit_O::_nil = _Nil<llvmo::DICompileUnit_O>();
//        llvmo::DICompileUnit_O::_unbound = _Unbound<llvmo::DICompileUnit_O>();
    }
//    classllvmo__DICompileUnit_Oval->setSupportsSlots(llvmo::DICompileUnit_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__DICompileUnit_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__DICompositeType_Oval]"));
    core::BuiltInClass_sp classllvmo__DICompositeType_Oval = core::BuiltInClass_O::create();
    classllvmo__DICompositeType_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__DICompositeType_Oval,_lisp,llvmo::DICompositeType_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::DICompositeType_O>::id,llvmo::DICompositeType_O::static_classSymbol());
    llvmo::DICompositeType_O::___staticClass = classllvmo__DICompositeType_Oval;
    llvmo::DICompositeType_O::static_Kind = gctools::GCInfo<llvmo::DICompositeType_O>::Kind;
    core::af_setf_findClass(classllvmo__DICompositeType_Oval,llvmo::DICompositeType_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::DICompositeType_O>* cb = new LispObjectAllocatorFunctor<llvmo::DICompositeType_O>();
        llvmo::DICompositeType_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::DICompositeType_O::static_className() % (void*)(llvmo::DICompositeType_O::static_allocator) );
    classllvmo__DICompositeType_Oval->setAllocator(llvmo::DICompositeType_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::DICompositeType_O::static_className() );
//        llvmo::DICompositeType_O::___staticDereferencedNilInstance = new llvmo::DICompositeType_O();
//        llvmo::DICompositeType_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::DICompositeType_O::___staticDereferencedUnboundInstance = new llvmo::DICompositeType_O();
//        llvmo::DICompositeType_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::DICompositeType_O::_nil = _Nil<llvmo::DICompositeType_O>();
//        llvmo::DICompositeType_O::_unbound = _Unbound<llvmo::DICompositeType_O>();
    }
//    classllvmo__DICompositeType_Oval->setSupportsSlots(llvmo::DICompositeType_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__DICompositeType_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__DIDerivedType_Oval]"));
    core::BuiltInClass_sp classllvmo__DIDerivedType_Oval = core::BuiltInClass_O::create();
    classllvmo__DIDerivedType_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__DIDerivedType_Oval,_lisp,llvmo::DIDerivedType_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::DIDerivedType_O>::id,llvmo::DIDerivedType_O::static_classSymbol());
    llvmo::DIDerivedType_O::___staticClass = classllvmo__DIDerivedType_Oval;
    llvmo::DIDerivedType_O::static_Kind = gctools::GCInfo<llvmo::DIDerivedType_O>::Kind;
    core::af_setf_findClass(classllvmo__DIDerivedType_Oval,llvmo::DIDerivedType_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::DIDerivedType_O>* cb = new LispObjectAllocatorFunctor<llvmo::DIDerivedType_O>();
        llvmo::DIDerivedType_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::DIDerivedType_O::static_className() % (void*)(llvmo::DIDerivedType_O::static_allocator) );
    classllvmo__DIDerivedType_Oval->setAllocator(llvmo::DIDerivedType_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::DIDerivedType_O::static_className() );
//        llvmo::DIDerivedType_O::___staticDereferencedNilInstance = new llvmo::DIDerivedType_O();
//        llvmo::DIDerivedType_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::DIDerivedType_O::___staticDereferencedUnboundInstance = new llvmo::DIDerivedType_O();
//        llvmo::DIDerivedType_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::DIDerivedType_O::_nil = _Nil<llvmo::DIDerivedType_O>();
//        llvmo::DIDerivedType_O::_unbound = _Unbound<llvmo::DIDerivedType_O>();
    }
//    classllvmo__DIDerivedType_Oval->setSupportsSlots(llvmo::DIDerivedType_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__DIDerivedType_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__DIDescriptor_Oval]"));
    core::BuiltInClass_sp classllvmo__DIDescriptor_Oval = core::BuiltInClass_O::create();
    classllvmo__DIDescriptor_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__DIDescriptor_Oval,_lisp,llvmo::DIDescriptor_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::DIDescriptor_O>::id,llvmo::DIDescriptor_O::static_classSymbol());
    llvmo::DIDescriptor_O::___staticClass = classllvmo__DIDescriptor_Oval;
    llvmo::DIDescriptor_O::static_Kind = gctools::GCInfo<llvmo::DIDescriptor_O>::Kind;
    core::af_setf_findClass(classllvmo__DIDescriptor_Oval,llvmo::DIDescriptor_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::DIDescriptor_O>* cb = new LispObjectAllocatorFunctor<llvmo::DIDescriptor_O>();
        llvmo::DIDescriptor_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::DIDescriptor_O::static_className() % (void*)(llvmo::DIDescriptor_O::static_allocator) );
    classllvmo__DIDescriptor_Oval->setAllocator(llvmo::DIDescriptor_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::DIDescriptor_O::static_className() );
//        llvmo::DIDescriptor_O::___staticDereferencedNilInstance = new llvmo::DIDescriptor_O();
//        llvmo::DIDescriptor_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::DIDescriptor_O::___staticDereferencedUnboundInstance = new llvmo::DIDescriptor_O();
//        llvmo::DIDescriptor_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::DIDescriptor_O::_nil = _Nil<llvmo::DIDescriptor_O>();
//        llvmo::DIDescriptor_O::_unbound = _Unbound<llvmo::DIDescriptor_O>();
    }
//    classllvmo__DIDescriptor_Oval->setSupportsSlots(llvmo::DIDescriptor_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__DIDescriptor_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__DIFile_Oval]"));
    core::BuiltInClass_sp classllvmo__DIFile_Oval = core::BuiltInClass_O::create();
    classllvmo__DIFile_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__DIFile_Oval,_lisp,llvmo::DIFile_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::DIFile_O>::id,llvmo::DIFile_O::static_classSymbol());
    llvmo::DIFile_O::___staticClass = classllvmo__DIFile_Oval;
    llvmo::DIFile_O::static_Kind = gctools::GCInfo<llvmo::DIFile_O>::Kind;
    core::af_setf_findClass(classllvmo__DIFile_Oval,llvmo::DIFile_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::DIFile_O>* cb = new LispObjectAllocatorFunctor<llvmo::DIFile_O>();
        llvmo::DIFile_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::DIFile_O::static_className() % (void*)(llvmo::DIFile_O::static_allocator) );
    classllvmo__DIFile_Oval->setAllocator(llvmo::DIFile_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::DIFile_O::static_className() );
//        llvmo::DIFile_O::___staticDereferencedNilInstance = new llvmo::DIFile_O();
//        llvmo::DIFile_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::DIFile_O::___staticDereferencedUnboundInstance = new llvmo::DIFile_O();
//        llvmo::DIFile_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::DIFile_O::_nil = _Nil<llvmo::DIFile_O>();
//        llvmo::DIFile_O::_unbound = _Unbound<llvmo::DIFile_O>();
    }
//    classllvmo__DIFile_Oval->setSupportsSlots(llvmo::DIFile_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__DIFile_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__DILexicalBlock_Oval]"));
    core::BuiltInClass_sp classllvmo__DILexicalBlock_Oval = core::BuiltInClass_O::create();
    classllvmo__DILexicalBlock_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__DILexicalBlock_Oval,_lisp,llvmo::DILexicalBlock_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::DILexicalBlock_O>::id,llvmo::DILexicalBlock_O::static_classSymbol());
    llvmo::DILexicalBlock_O::___staticClass = classllvmo__DILexicalBlock_Oval;
    llvmo::DILexicalBlock_O::static_Kind = gctools::GCInfo<llvmo::DILexicalBlock_O>::Kind;
    core::af_setf_findClass(classllvmo__DILexicalBlock_Oval,llvmo::DILexicalBlock_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::DILexicalBlock_O>* cb = new LispObjectAllocatorFunctor<llvmo::DILexicalBlock_O>();
        llvmo::DILexicalBlock_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::DILexicalBlock_O::static_className() % (void*)(llvmo::DILexicalBlock_O::static_allocator) );
    classllvmo__DILexicalBlock_Oval->setAllocator(llvmo::DILexicalBlock_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::DILexicalBlock_O::static_className() );
//        llvmo::DILexicalBlock_O::___staticDereferencedNilInstance = new llvmo::DILexicalBlock_O();
//        llvmo::DILexicalBlock_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::DILexicalBlock_O::___staticDereferencedUnboundInstance = new llvmo::DILexicalBlock_O();
//        llvmo::DILexicalBlock_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::DILexicalBlock_O::_nil = _Nil<llvmo::DILexicalBlock_O>();
//        llvmo::DILexicalBlock_O::_unbound = _Unbound<llvmo::DILexicalBlock_O>();
    }
//    classllvmo__DILexicalBlock_Oval->setSupportsSlots(llvmo::DILexicalBlock_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__DILexicalBlock_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__DIScope_Oval]"));
    core::BuiltInClass_sp classllvmo__DIScope_Oval = core::BuiltInClass_O::create();
    classllvmo__DIScope_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__DIScope_Oval,_lisp,llvmo::DIScope_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::DIScope_O>::id,llvmo::DIScope_O::static_classSymbol());
    llvmo::DIScope_O::___staticClass = classllvmo__DIScope_Oval;
    llvmo::DIScope_O::static_Kind = gctools::GCInfo<llvmo::DIScope_O>::Kind;
    core::af_setf_findClass(classllvmo__DIScope_Oval,llvmo::DIScope_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::DIScope_O>* cb = new LispObjectAllocatorFunctor<llvmo::DIScope_O>();
        llvmo::DIScope_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::DIScope_O::static_className() % (void*)(llvmo::DIScope_O::static_allocator) );
    classllvmo__DIScope_Oval->setAllocator(llvmo::DIScope_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::DIScope_O::static_className() );
//        llvmo::DIScope_O::___staticDereferencedNilInstance = new llvmo::DIScope_O();
//        llvmo::DIScope_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::DIScope_O::___staticDereferencedUnboundInstance = new llvmo::DIScope_O();
//        llvmo::DIScope_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::DIScope_O::_nil = _Nil<llvmo::DIScope_O>();
//        llvmo::DIScope_O::_unbound = _Unbound<llvmo::DIScope_O>();
    }
//    classllvmo__DIScope_Oval->setSupportsSlots(llvmo::DIScope_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__DIScope_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__DISubprogram_Oval]"));
    core::BuiltInClass_sp classllvmo__DISubprogram_Oval = core::BuiltInClass_O::create();
    classllvmo__DISubprogram_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__DISubprogram_Oval,_lisp,llvmo::DISubprogram_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::DISubprogram_O>::id,llvmo::DISubprogram_O::static_classSymbol());
    llvmo::DISubprogram_O::___staticClass = classllvmo__DISubprogram_Oval;
    llvmo::DISubprogram_O::static_Kind = gctools::GCInfo<llvmo::DISubprogram_O>::Kind;
    core::af_setf_findClass(classllvmo__DISubprogram_Oval,llvmo::DISubprogram_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::DISubprogram_O>* cb = new LispObjectAllocatorFunctor<llvmo::DISubprogram_O>();
        llvmo::DISubprogram_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::DISubprogram_O::static_className() % (void*)(llvmo::DISubprogram_O::static_allocator) );
    classllvmo__DISubprogram_Oval->setAllocator(llvmo::DISubprogram_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::DISubprogram_O::static_className() );
//        llvmo::DISubprogram_O::___staticDereferencedNilInstance = new llvmo::DISubprogram_O();
//        llvmo::DISubprogram_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::DISubprogram_O::___staticDereferencedUnboundInstance = new llvmo::DISubprogram_O();
//        llvmo::DISubprogram_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::DISubprogram_O::_nil = _Nil<llvmo::DISubprogram_O>();
//        llvmo::DISubprogram_O::_unbound = _Unbound<llvmo::DISubprogram_O>();
    }
//    classllvmo__DISubprogram_Oval->setSupportsSlots(llvmo::DISubprogram_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__DISubprogram_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__DIType_Oval]"));
    core::BuiltInClass_sp classllvmo__DIType_Oval = core::BuiltInClass_O::create();
    classllvmo__DIType_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__DIType_Oval,_lisp,llvmo::DIType_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::DIType_O>::id,llvmo::DIType_O::static_classSymbol());
    llvmo::DIType_O::___staticClass = classllvmo__DIType_Oval;
    llvmo::DIType_O::static_Kind = gctools::GCInfo<llvmo::DIType_O>::Kind;
    core::af_setf_findClass(classllvmo__DIType_Oval,llvmo::DIType_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::DIType_O>* cb = new LispObjectAllocatorFunctor<llvmo::DIType_O>();
        llvmo::DIType_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::DIType_O::static_className() % (void*)(llvmo::DIType_O::static_allocator) );
    classllvmo__DIType_Oval->setAllocator(llvmo::DIType_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::DIType_O::static_className() );
//        llvmo::DIType_O::___staticDereferencedNilInstance = new llvmo::DIType_O();
//        llvmo::DIType_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::DIType_O::___staticDereferencedUnboundInstance = new llvmo::DIType_O();
//        llvmo::DIType_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::DIType_O::_nil = _Nil<llvmo::DIType_O>();
//        llvmo::DIType_O::_unbound = _Unbound<llvmo::DIType_O>();
    }
//    classllvmo__DIType_Oval->setSupportsSlots(llvmo::DIType_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__DIType_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__FunctionPassManager_Oval]"));
    core::BuiltInClass_sp classllvmo__FunctionPassManager_Oval = core::BuiltInClass_O::create();
    classllvmo__FunctionPassManager_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__FunctionPassManager_Oval,_lisp,llvmo::FunctionPassManager_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::FunctionPassManager_O>::id,llvmo::FunctionPassManager_O::static_classSymbol());
    llvmo::FunctionPassManager_O::___staticClass = classllvmo__FunctionPassManager_Oval;
    llvmo::FunctionPassManager_O::static_Kind = gctools::GCInfo<llvmo::FunctionPassManager_O>::Kind;
    core::af_setf_findClass(classllvmo__FunctionPassManager_Oval,llvmo::FunctionPassManager_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::FunctionPassManager_O>* cb = new LispObjectAllocatorFunctor<llvmo::FunctionPassManager_O>();
        llvmo::FunctionPassManager_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::FunctionPassManager_O::static_className() % (void*)(llvmo::FunctionPassManager_O::static_allocator) );
    classllvmo__FunctionPassManager_Oval->setAllocator(llvmo::FunctionPassManager_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::FunctionPassManager_O::static_className() );
//        llvmo::FunctionPassManager_O::___staticDereferencedNilInstance = new llvmo::FunctionPassManager_O();
//        llvmo::FunctionPassManager_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::FunctionPassManager_O::___staticDereferencedUnboundInstance = new llvmo::FunctionPassManager_O();
//        llvmo::FunctionPassManager_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::FunctionPassManager_O::_nil = _Nil<llvmo::FunctionPassManager_O>();
//        llvmo::FunctionPassManager_O::_unbound = _Unbound<llvmo::FunctionPassManager_O>();
    }
//    classllvmo__FunctionPassManager_Oval->setSupportsSlots(llvmo::FunctionPassManager_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__FunctionPassManager_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__FunctionPass_Oval]"));
    core::BuiltInClass_sp classllvmo__FunctionPass_Oval = core::BuiltInClass_O::create();
    classllvmo__FunctionPass_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__FunctionPass_Oval,_lisp,llvmo::FunctionPass_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::FunctionPass_O>::id,llvmo::FunctionPass_O::static_classSymbol());
    llvmo::FunctionPass_O::___staticClass = classllvmo__FunctionPass_Oval;
    llvmo::FunctionPass_O::static_Kind = gctools::GCInfo<llvmo::FunctionPass_O>::Kind;
    core::af_setf_findClass(classllvmo__FunctionPass_Oval,llvmo::FunctionPass_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::FunctionPass_O>* cb = new LispObjectAllocatorFunctor<llvmo::FunctionPass_O>();
        llvmo::FunctionPass_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::FunctionPass_O::static_className() % (void*)(llvmo::FunctionPass_O::static_allocator) );
    classllvmo__FunctionPass_Oval->setAllocator(llvmo::FunctionPass_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::FunctionPass_O::static_className() );
//        llvmo::FunctionPass_O::___staticDereferencedNilInstance = new llvmo::FunctionPass_O();
//        llvmo::FunctionPass_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::FunctionPass_O::___staticDereferencedUnboundInstance = new llvmo::FunctionPass_O();
//        llvmo::FunctionPass_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::FunctionPass_O::_nil = _Nil<llvmo::FunctionPass_O>();
//        llvmo::FunctionPass_O::_unbound = _Unbound<llvmo::FunctionPass_O>();
    }
//    classllvmo__FunctionPass_Oval->setSupportsSlots(llvmo::FunctionPass_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__FunctionPass_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__FunctionType_Oval]"));
    core::BuiltInClass_sp classllvmo__FunctionType_Oval = core::BuiltInClass_O::create();
    classllvmo__FunctionType_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__FunctionType_Oval,_lisp,llvmo::FunctionType_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::FunctionType_O>::id,llvmo::FunctionType_O::static_classSymbol());
    llvmo::FunctionType_O::___staticClass = classllvmo__FunctionType_Oval;
    llvmo::FunctionType_O::static_Kind = gctools::GCInfo<llvmo::FunctionType_O>::Kind;
    core::af_setf_findClass(classllvmo__FunctionType_Oval,llvmo::FunctionType_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::FunctionType_O>* cb = new LispObjectAllocatorFunctor<llvmo::FunctionType_O>();
        llvmo::FunctionType_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::FunctionType_O::static_className() % (void*)(llvmo::FunctionType_O::static_allocator) );
    classllvmo__FunctionType_Oval->setAllocator(llvmo::FunctionType_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::FunctionType_O::static_className() );
//        llvmo::FunctionType_O::___staticDereferencedNilInstance = new llvmo::FunctionType_O();
//        llvmo::FunctionType_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::FunctionType_O::___staticDereferencedUnboundInstance = new llvmo::FunctionType_O();
//        llvmo::FunctionType_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::FunctionType_O::_nil = _Nil<llvmo::FunctionType_O>();
//        llvmo::FunctionType_O::_unbound = _Unbound<llvmo::FunctionType_O>();
    }
//    classllvmo__FunctionType_Oval->setSupportsSlots(llvmo::FunctionType_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__FunctionType_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__IRBuilder_Oval]"));
    core::BuiltInClass_sp classllvmo__IRBuilder_Oval = core::BuiltInClass_O::create();
    classllvmo__IRBuilder_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__IRBuilder_Oval,_lisp,llvmo::IRBuilder_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::IRBuilder_O>::id,llvmo::IRBuilder_O::static_classSymbol());
    llvmo::IRBuilder_O::___staticClass = classllvmo__IRBuilder_Oval;
    llvmo::IRBuilder_O::static_Kind = gctools::GCInfo<llvmo::IRBuilder_O>::Kind;
    core::af_setf_findClass(classllvmo__IRBuilder_Oval,llvmo::IRBuilder_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::IRBuilder_O>* cb = new LispObjectAllocatorFunctor<llvmo::IRBuilder_O>();
        llvmo::IRBuilder_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::IRBuilder_O::static_className() % (void*)(llvmo::IRBuilder_O::static_allocator) );
    classllvmo__IRBuilder_Oval->setAllocator(llvmo::IRBuilder_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::IRBuilder_O::static_className() );
//        llvmo::IRBuilder_O::___staticDereferencedNilInstance = new llvmo::IRBuilder_O();
//        llvmo::IRBuilder_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::IRBuilder_O::___staticDereferencedUnboundInstance = new llvmo::IRBuilder_O();
//        llvmo::IRBuilder_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::IRBuilder_O::_nil = _Nil<llvmo::IRBuilder_O>();
//        llvmo::IRBuilder_O::_unbound = _Unbound<llvmo::IRBuilder_O>();
    }
//    classllvmo__IRBuilder_Oval->setSupportsSlots(llvmo::IRBuilder_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__IRBuilder_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__IntegerType_Oval]"));
    core::BuiltInClass_sp classllvmo__IntegerType_Oval = core::BuiltInClass_O::create();
    classllvmo__IntegerType_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__IntegerType_Oval,_lisp,llvmo::IntegerType_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::IntegerType_O>::id,llvmo::IntegerType_O::static_classSymbol());
    llvmo::IntegerType_O::___staticClass = classllvmo__IntegerType_Oval;
    llvmo::IntegerType_O::static_Kind = gctools::GCInfo<llvmo::IntegerType_O>::Kind;
    core::af_setf_findClass(classllvmo__IntegerType_Oval,llvmo::IntegerType_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::IntegerType_O>* cb = new LispObjectAllocatorFunctor<llvmo::IntegerType_O>();
        llvmo::IntegerType_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::IntegerType_O::static_className() % (void*)(llvmo::IntegerType_O::static_allocator) );
    classllvmo__IntegerType_Oval->setAllocator(llvmo::IntegerType_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::IntegerType_O::static_className() );
//        llvmo::IntegerType_O::___staticDereferencedNilInstance = new llvmo::IntegerType_O();
//        llvmo::IntegerType_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::IntegerType_O::___staticDereferencedUnboundInstance = new llvmo::IntegerType_O();
//        llvmo::IntegerType_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::IntegerType_O::_nil = _Nil<llvmo::IntegerType_O>();
//        llvmo::IntegerType_O::_unbound = _Unbound<llvmo::IntegerType_O>();
    }
//    classllvmo__IntegerType_Oval->setSupportsSlots(llvmo::IntegerType_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__IntegerType_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__MDNode_Oval]"));
    core::BuiltInClass_sp classllvmo__MDNode_Oval = core::BuiltInClass_O::create();
    classllvmo__MDNode_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__MDNode_Oval,_lisp,llvmo::MDNode_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::MDNode_O>::id,llvmo::MDNode_O::static_classSymbol());
    llvmo::MDNode_O::___staticClass = classllvmo__MDNode_Oval;
    llvmo::MDNode_O::static_Kind = gctools::GCInfo<llvmo::MDNode_O>::Kind;
    core::af_setf_findClass(classllvmo__MDNode_Oval,llvmo::MDNode_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::MDNode_O>* cb = new LispObjectAllocatorFunctor<llvmo::MDNode_O>();
        llvmo::MDNode_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::MDNode_O::static_className() % (void*)(llvmo::MDNode_O::static_allocator) );
    classllvmo__MDNode_Oval->setAllocator(llvmo::MDNode_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::MDNode_O::static_className() );
//        llvmo::MDNode_O::___staticDereferencedNilInstance = new llvmo::MDNode_O();
//        llvmo::MDNode_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::MDNode_O::___staticDereferencedUnboundInstance = new llvmo::MDNode_O();
//        llvmo::MDNode_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::MDNode_O::_nil = _Nil<llvmo::MDNode_O>();
//        llvmo::MDNode_O::_unbound = _Unbound<llvmo::MDNode_O>();
    }
//    classllvmo__MDNode_Oval->setSupportsSlots(llvmo::MDNode_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__MDNode_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__MDString_Oval]"));
    core::BuiltInClass_sp classllvmo__MDString_Oval = core::BuiltInClass_O::create();
    classllvmo__MDString_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__MDString_Oval,_lisp,llvmo::MDString_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::MDString_O>::id,llvmo::MDString_O::static_classSymbol());
    llvmo::MDString_O::___staticClass = classllvmo__MDString_Oval;
    llvmo::MDString_O::static_Kind = gctools::GCInfo<llvmo::MDString_O>::Kind;
    core::af_setf_findClass(classllvmo__MDString_Oval,llvmo::MDString_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::MDString_O>* cb = new LispObjectAllocatorFunctor<llvmo::MDString_O>();
        llvmo::MDString_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::MDString_O::static_className() % (void*)(llvmo::MDString_O::static_allocator) );
    classllvmo__MDString_Oval->setAllocator(llvmo::MDString_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::MDString_O::static_className() );
//        llvmo::MDString_O::___staticDereferencedNilInstance = new llvmo::MDString_O();
//        llvmo::MDString_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::MDString_O::___staticDereferencedUnboundInstance = new llvmo::MDString_O();
//        llvmo::MDString_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::MDString_O::_nil = _Nil<llvmo::MDString_O>();
//        llvmo::MDString_O::_unbound = _Unbound<llvmo::MDString_O>();
    }
//    classllvmo__MDString_Oval->setSupportsSlots(llvmo::MDString_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__MDString_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__ModulePass_Oval]"));
    core::BuiltInClass_sp classllvmo__ModulePass_Oval = core::BuiltInClass_O::create();
    classllvmo__ModulePass_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ModulePass_Oval,_lisp,llvmo::ModulePass_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::ModulePass_O>::id,llvmo::ModulePass_O::static_classSymbol());
    llvmo::ModulePass_O::___staticClass = classllvmo__ModulePass_Oval;
    llvmo::ModulePass_O::static_Kind = gctools::GCInfo<llvmo::ModulePass_O>::Kind;
    core::af_setf_findClass(classllvmo__ModulePass_Oval,llvmo::ModulePass_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::ModulePass_O>* cb = new LispObjectAllocatorFunctor<llvmo::ModulePass_O>();
        llvmo::ModulePass_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::ModulePass_O::static_className() % (void*)(llvmo::ModulePass_O::static_allocator) );
    classllvmo__ModulePass_Oval->setAllocator(llvmo::ModulePass_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::ModulePass_O::static_className() );
//        llvmo::ModulePass_O::___staticDereferencedNilInstance = new llvmo::ModulePass_O();
//        llvmo::ModulePass_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::ModulePass_O::___staticDereferencedUnboundInstance = new llvmo::ModulePass_O();
//        llvmo::ModulePass_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::ModulePass_O::_nil = _Nil<llvmo::ModulePass_O>();
//        llvmo::ModulePass_O::_unbound = _Unbound<llvmo::ModulePass_O>();
    }
//    classllvmo__ModulePass_Oval->setSupportsSlots(llvmo::ModulePass_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__ModulePass_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__PassManager_Oval]"));
    core::BuiltInClass_sp classllvmo__PassManager_Oval = core::BuiltInClass_O::create();
    classllvmo__PassManager_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__PassManager_Oval,_lisp,llvmo::PassManager_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::PassManager_O>::id,llvmo::PassManager_O::static_classSymbol());
    llvmo::PassManager_O::___staticClass = classllvmo__PassManager_Oval;
    llvmo::PassManager_O::static_Kind = gctools::GCInfo<llvmo::PassManager_O>::Kind;
    core::af_setf_findClass(classllvmo__PassManager_Oval,llvmo::PassManager_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::PassManager_O>* cb = new LispObjectAllocatorFunctor<llvmo::PassManager_O>();
        llvmo::PassManager_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::PassManager_O::static_className() % (void*)(llvmo::PassManager_O::static_allocator) );
    classllvmo__PassManager_Oval->setAllocator(llvmo::PassManager_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::PassManager_O::static_className() );
//        llvmo::PassManager_O::___staticDereferencedNilInstance = new llvmo::PassManager_O();
//        llvmo::PassManager_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::PassManager_O::___staticDereferencedUnboundInstance = new llvmo::PassManager_O();
//        llvmo::PassManager_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::PassManager_O::_nil = _Nil<llvmo::PassManager_O>();
//        llvmo::PassManager_O::_unbound = _Unbound<llvmo::PassManager_O>();
    }
//    classllvmo__PassManager_Oval->setSupportsSlots(llvmo::PassManager_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__PassManager_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__User_Oval]"));
    core::BuiltInClass_sp classllvmo__User_Oval = core::BuiltInClass_O::create();
    classllvmo__User_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__User_Oval,_lisp,llvmo::User_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::User_O>::id,llvmo::User_O::static_classSymbol());
    llvmo::User_O::___staticClass = classllvmo__User_Oval;
    llvmo::User_O::static_Kind = gctools::GCInfo<llvmo::User_O>::Kind;
    core::af_setf_findClass(classllvmo__User_Oval,llvmo::User_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::User_O>* cb = new LispObjectAllocatorFunctor<llvmo::User_O>();
        llvmo::User_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::User_O::static_className() % (void*)(llvmo::User_O::static_allocator) );
    classllvmo__User_Oval->setAllocator(llvmo::User_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::User_O::static_className() );
//        llvmo::User_O::___staticDereferencedNilInstance = new llvmo::User_O();
//        llvmo::User_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::User_O::___staticDereferencedUnboundInstance = new llvmo::User_O();
//        llvmo::User_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::User_O::_nil = _Nil<llvmo::User_O>();
//        llvmo::User_O::_unbound = _Unbound<llvmo::User_O>();
    }
//    classllvmo__User_Oval->setSupportsSlots(llvmo::User_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__User_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__Constant_Oval]"));
    core::BuiltInClass_sp classllvmo__Constant_Oval = core::BuiltInClass_O::create();
    classllvmo__Constant_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__Constant_Oval,_lisp,llvmo::Constant_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::Constant_O>::id,llvmo::Constant_O::static_classSymbol());
    llvmo::Constant_O::___staticClass = classllvmo__Constant_Oval;
    llvmo::Constant_O::static_Kind = gctools::GCInfo<llvmo::Constant_O>::Kind;
    core::af_setf_findClass(classllvmo__Constant_Oval,llvmo::Constant_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::Constant_O>* cb = new LispObjectAllocatorFunctor<llvmo::Constant_O>();
        llvmo::Constant_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::Constant_O::static_className() % (void*)(llvmo::Constant_O::static_allocator) );
    classllvmo__Constant_Oval->setAllocator(llvmo::Constant_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::Constant_O::static_className() );
//        llvmo::Constant_O::___staticDereferencedNilInstance = new llvmo::Constant_O();
//        llvmo::Constant_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::Constant_O::___staticDereferencedUnboundInstance = new llvmo::Constant_O();
//        llvmo::Constant_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::Constant_O::_nil = _Nil<llvmo::Constant_O>();
//        llvmo::Constant_O::_unbound = _Unbound<llvmo::Constant_O>();
    }
//    classllvmo__Constant_Oval->setSupportsSlots(llvmo::Constant_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__Constant_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__ImmutablePass_Oval]"));
    core::BuiltInClass_sp classllvmo__ImmutablePass_Oval = core::BuiltInClass_O::create();
    classllvmo__ImmutablePass_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ImmutablePass_Oval,_lisp,llvmo::ImmutablePass_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::ImmutablePass_O>::id,llvmo::ImmutablePass_O::static_classSymbol());
    llvmo::ImmutablePass_O::___staticClass = classllvmo__ImmutablePass_Oval;
    llvmo::ImmutablePass_O::static_Kind = gctools::GCInfo<llvmo::ImmutablePass_O>::Kind;
    core::af_setf_findClass(classllvmo__ImmutablePass_Oval,llvmo::ImmutablePass_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::ImmutablePass_O>* cb = new LispObjectAllocatorFunctor<llvmo::ImmutablePass_O>();
        llvmo::ImmutablePass_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::ImmutablePass_O::static_className() % (void*)(llvmo::ImmutablePass_O::static_allocator) );
    classllvmo__ImmutablePass_Oval->setAllocator(llvmo::ImmutablePass_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::ImmutablePass_O::static_className() );
//        llvmo::ImmutablePass_O::___staticDereferencedNilInstance = new llvmo::ImmutablePass_O();
//        llvmo::ImmutablePass_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::ImmutablePass_O::___staticDereferencedUnboundInstance = new llvmo::ImmutablePass_O();
//        llvmo::ImmutablePass_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::ImmutablePass_O::_nil = _Nil<llvmo::ImmutablePass_O>();
//        llvmo::ImmutablePass_O::_unbound = _Unbound<llvmo::ImmutablePass_O>();
    }
//    classllvmo__ImmutablePass_Oval->setSupportsSlots(llvmo::ImmutablePass_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__ImmutablePass_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__Instruction_Oval]"));
    core::BuiltInClass_sp classllvmo__Instruction_Oval = core::BuiltInClass_O::create();
    classllvmo__Instruction_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__Instruction_Oval,_lisp,llvmo::Instruction_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::Instruction_O>::id,llvmo::Instruction_O::static_classSymbol());
    llvmo::Instruction_O::___staticClass = classllvmo__Instruction_Oval;
    llvmo::Instruction_O::static_Kind = gctools::GCInfo<llvmo::Instruction_O>::Kind;
    core::af_setf_findClass(classllvmo__Instruction_Oval,llvmo::Instruction_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::Instruction_O>* cb = new LispObjectAllocatorFunctor<llvmo::Instruction_O>();
        llvmo::Instruction_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::Instruction_O::static_className() % (void*)(llvmo::Instruction_O::static_allocator) );
    classllvmo__Instruction_Oval->setAllocator(llvmo::Instruction_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::Instruction_O::static_className() );
//        llvmo::Instruction_O::___staticDereferencedNilInstance = new llvmo::Instruction_O();
//        llvmo::Instruction_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::Instruction_O::___staticDereferencedUnboundInstance = new llvmo::Instruction_O();
//        llvmo::Instruction_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::Instruction_O::_nil = _Nil<llvmo::Instruction_O>();
//        llvmo::Instruction_O::_unbound = _Unbound<llvmo::Instruction_O>();
    }
//    classllvmo__Instruction_Oval->setSupportsSlots(llvmo::Instruction_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__Instruction_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__SequentialType_Oval]"));
    core::BuiltInClass_sp classllvmo__SequentialType_Oval = core::BuiltInClass_O::create();
    classllvmo__SequentialType_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__SequentialType_Oval,_lisp,llvmo::SequentialType_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::SequentialType_O>::id,llvmo::SequentialType_O::static_classSymbol());
    llvmo::SequentialType_O::___staticClass = classllvmo__SequentialType_Oval;
    llvmo::SequentialType_O::static_Kind = gctools::GCInfo<llvmo::SequentialType_O>::Kind;
    core::af_setf_findClass(classllvmo__SequentialType_Oval,llvmo::SequentialType_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::SequentialType_O>* cb = new LispObjectAllocatorFunctor<llvmo::SequentialType_O>();
        llvmo::SequentialType_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::SequentialType_O::static_className() % (void*)(llvmo::SequentialType_O::static_allocator) );
    classllvmo__SequentialType_Oval->setAllocator(llvmo::SequentialType_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::SequentialType_O::static_className() );
//        llvmo::SequentialType_O::___staticDereferencedNilInstance = new llvmo::SequentialType_O();
//        llvmo::SequentialType_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::SequentialType_O::___staticDereferencedUnboundInstance = new llvmo::SequentialType_O();
//        llvmo::SequentialType_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::SequentialType_O::_nil = _Nil<llvmo::SequentialType_O>();
//        llvmo::SequentialType_O::_unbound = _Unbound<llvmo::SequentialType_O>();
    }
//    classllvmo__SequentialType_Oval->setSupportsSlots(llvmo::SequentialType_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__SequentialType_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__StructType_Oval]"));
    core::BuiltInClass_sp classllvmo__StructType_Oval = core::BuiltInClass_O::create();
    classllvmo__StructType_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__StructType_Oval,_lisp,llvmo::StructType_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::StructType_O>::id,llvmo::StructType_O::static_classSymbol());
    llvmo::StructType_O::___staticClass = classllvmo__StructType_Oval;
    llvmo::StructType_O::static_Kind = gctools::GCInfo<llvmo::StructType_O>::Kind;
    core::af_setf_findClass(classllvmo__StructType_Oval,llvmo::StructType_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::StructType_O>* cb = new LispObjectAllocatorFunctor<llvmo::StructType_O>();
        llvmo::StructType_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::StructType_O::static_className() % (void*)(llvmo::StructType_O::static_allocator) );
    classllvmo__StructType_Oval->setAllocator(llvmo::StructType_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::StructType_O::static_className() );
//        llvmo::StructType_O::___staticDereferencedNilInstance = new llvmo::StructType_O();
//        llvmo::StructType_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::StructType_O::___staticDereferencedUnboundInstance = new llvmo::StructType_O();
//        llvmo::StructType_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::StructType_O::_nil = _Nil<llvmo::StructType_O>();
//        llvmo::StructType_O::_unbound = _Unbound<llvmo::StructType_O>();
    }
//    classllvmo__StructType_Oval->setSupportsSlots(llvmo::StructType_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__StructType_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__ArrayType_Oval]"));
    core::BuiltInClass_sp classllvmo__ArrayType_Oval = core::BuiltInClass_O::create();
    classllvmo__ArrayType_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ArrayType_Oval,_lisp,llvmo::ArrayType_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::ArrayType_O>::id,llvmo::ArrayType_O::static_classSymbol());
    llvmo::ArrayType_O::___staticClass = classllvmo__ArrayType_Oval;
    llvmo::ArrayType_O::static_Kind = gctools::GCInfo<llvmo::ArrayType_O>::Kind;
    core::af_setf_findClass(classllvmo__ArrayType_Oval,llvmo::ArrayType_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::ArrayType_O>* cb = new LispObjectAllocatorFunctor<llvmo::ArrayType_O>();
        llvmo::ArrayType_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::ArrayType_O::static_className() % (void*)(llvmo::ArrayType_O::static_allocator) );
    classllvmo__ArrayType_Oval->setAllocator(llvmo::ArrayType_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::ArrayType_O::static_className() );
//        llvmo::ArrayType_O::___staticDereferencedNilInstance = new llvmo::ArrayType_O();
//        llvmo::ArrayType_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::ArrayType_O::___staticDereferencedUnboundInstance = new llvmo::ArrayType_O();
//        llvmo::ArrayType_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::ArrayType_O::_nil = _Nil<llvmo::ArrayType_O>();
//        llvmo::ArrayType_O::_unbound = _Unbound<llvmo::ArrayType_O>();
    }
//    classllvmo__ArrayType_Oval->setSupportsSlots(llvmo::ArrayType_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__ArrayType_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__AtomicCmpXchgInst_Oval]"));
    core::BuiltInClass_sp classllvmo__AtomicCmpXchgInst_Oval = core::BuiltInClass_O::create();
    classllvmo__AtomicCmpXchgInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__AtomicCmpXchgInst_Oval,_lisp,llvmo::AtomicCmpXchgInst_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::AtomicCmpXchgInst_O>::id,llvmo::AtomicCmpXchgInst_O::static_classSymbol());
    llvmo::AtomicCmpXchgInst_O::___staticClass = classllvmo__AtomicCmpXchgInst_Oval;
    llvmo::AtomicCmpXchgInst_O::static_Kind = gctools::GCInfo<llvmo::AtomicCmpXchgInst_O>::Kind;
    core::af_setf_findClass(classllvmo__AtomicCmpXchgInst_Oval,llvmo::AtomicCmpXchgInst_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::AtomicCmpXchgInst_O>* cb = new LispObjectAllocatorFunctor<llvmo::AtomicCmpXchgInst_O>();
        llvmo::AtomicCmpXchgInst_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::AtomicCmpXchgInst_O::static_className() % (void*)(llvmo::AtomicCmpXchgInst_O::static_allocator) );
    classllvmo__AtomicCmpXchgInst_Oval->setAllocator(llvmo::AtomicCmpXchgInst_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::AtomicCmpXchgInst_O::static_className() );
//        llvmo::AtomicCmpXchgInst_O::___staticDereferencedNilInstance = new llvmo::AtomicCmpXchgInst_O();
//        llvmo::AtomicCmpXchgInst_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::AtomicCmpXchgInst_O::___staticDereferencedUnboundInstance = new llvmo::AtomicCmpXchgInst_O();
//        llvmo::AtomicCmpXchgInst_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::AtomicCmpXchgInst_O::_nil = _Nil<llvmo::AtomicCmpXchgInst_O>();
//        llvmo::AtomicCmpXchgInst_O::_unbound = _Unbound<llvmo::AtomicCmpXchgInst_O>();
    }
//    classllvmo__AtomicCmpXchgInst_Oval->setSupportsSlots(llvmo::AtomicCmpXchgInst_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__AtomicCmpXchgInst_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__AtomicRMWInst_Oval]"));
    core::BuiltInClass_sp classllvmo__AtomicRMWInst_Oval = core::BuiltInClass_O::create();
    classllvmo__AtomicRMWInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__AtomicRMWInst_Oval,_lisp,llvmo::AtomicRMWInst_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::AtomicRMWInst_O>::id,llvmo::AtomicRMWInst_O::static_classSymbol());
    llvmo::AtomicRMWInst_O::___staticClass = classllvmo__AtomicRMWInst_Oval;
    llvmo::AtomicRMWInst_O::static_Kind = gctools::GCInfo<llvmo::AtomicRMWInst_O>::Kind;
    core::af_setf_findClass(classllvmo__AtomicRMWInst_Oval,llvmo::AtomicRMWInst_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::AtomicRMWInst_O>* cb = new LispObjectAllocatorFunctor<llvmo::AtomicRMWInst_O>();
        llvmo::AtomicRMWInst_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::AtomicRMWInst_O::static_className() % (void*)(llvmo::AtomicRMWInst_O::static_allocator) );
    classllvmo__AtomicRMWInst_Oval->setAllocator(llvmo::AtomicRMWInst_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::AtomicRMWInst_O::static_className() );
//        llvmo::AtomicRMWInst_O::___staticDereferencedNilInstance = new llvmo::AtomicRMWInst_O();
//        llvmo::AtomicRMWInst_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::AtomicRMWInst_O::___staticDereferencedUnboundInstance = new llvmo::AtomicRMWInst_O();
//        llvmo::AtomicRMWInst_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::AtomicRMWInst_O::_nil = _Nil<llvmo::AtomicRMWInst_O>();
//        llvmo::AtomicRMWInst_O::_unbound = _Unbound<llvmo::AtomicRMWInst_O>();
    }
//    classllvmo__AtomicRMWInst_Oval->setSupportsSlots(llvmo::AtomicRMWInst_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__AtomicRMWInst_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__BlockAddress_Oval]"));
    core::BuiltInClass_sp classllvmo__BlockAddress_Oval = core::BuiltInClass_O::create();
    classllvmo__BlockAddress_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__BlockAddress_Oval,_lisp,llvmo::BlockAddress_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::BlockAddress_O>::id,llvmo::BlockAddress_O::static_classSymbol());
    llvmo::BlockAddress_O::___staticClass = classllvmo__BlockAddress_Oval;
    llvmo::BlockAddress_O::static_Kind = gctools::GCInfo<llvmo::BlockAddress_O>::Kind;
    core::af_setf_findClass(classllvmo__BlockAddress_Oval,llvmo::BlockAddress_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::BlockAddress_O>* cb = new LispObjectAllocatorFunctor<llvmo::BlockAddress_O>();
        llvmo::BlockAddress_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::BlockAddress_O::static_className() % (void*)(llvmo::BlockAddress_O::static_allocator) );
    classllvmo__BlockAddress_Oval->setAllocator(llvmo::BlockAddress_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::BlockAddress_O::static_className() );
//        llvmo::BlockAddress_O::___staticDereferencedNilInstance = new llvmo::BlockAddress_O();
//        llvmo::BlockAddress_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::BlockAddress_O::___staticDereferencedUnboundInstance = new llvmo::BlockAddress_O();
//        llvmo::BlockAddress_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::BlockAddress_O::_nil = _Nil<llvmo::BlockAddress_O>();
//        llvmo::BlockAddress_O::_unbound = _Unbound<llvmo::BlockAddress_O>();
    }
//    classllvmo__BlockAddress_Oval->setSupportsSlots(llvmo::BlockAddress_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__BlockAddress_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__CallInst_Oval]"));
    core::BuiltInClass_sp classllvmo__CallInst_Oval = core::BuiltInClass_O::create();
    classllvmo__CallInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__CallInst_Oval,_lisp,llvmo::CallInst_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::CallInst_O>::id,llvmo::CallInst_O::static_classSymbol());
    llvmo::CallInst_O::___staticClass = classllvmo__CallInst_Oval;
    llvmo::CallInst_O::static_Kind = gctools::GCInfo<llvmo::CallInst_O>::Kind;
    core::af_setf_findClass(classllvmo__CallInst_Oval,llvmo::CallInst_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::CallInst_O>* cb = new LispObjectAllocatorFunctor<llvmo::CallInst_O>();
        llvmo::CallInst_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::CallInst_O::static_className() % (void*)(llvmo::CallInst_O::static_allocator) );
    classllvmo__CallInst_Oval->setAllocator(llvmo::CallInst_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::CallInst_O::static_className() );
//        llvmo::CallInst_O::___staticDereferencedNilInstance = new llvmo::CallInst_O();
//        llvmo::CallInst_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::CallInst_O::___staticDereferencedUnboundInstance = new llvmo::CallInst_O();
//        llvmo::CallInst_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::CallInst_O::_nil = _Nil<llvmo::CallInst_O>();
//        llvmo::CallInst_O::_unbound = _Unbound<llvmo::CallInst_O>();
    }
//    classllvmo__CallInst_Oval->setSupportsSlots(llvmo::CallInst_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__CallInst_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__ConstantArray_Oval]"));
    core::BuiltInClass_sp classllvmo__ConstantArray_Oval = core::BuiltInClass_O::create();
    classllvmo__ConstantArray_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ConstantArray_Oval,_lisp,llvmo::ConstantArray_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::ConstantArray_O>::id,llvmo::ConstantArray_O::static_classSymbol());
    llvmo::ConstantArray_O::___staticClass = classllvmo__ConstantArray_Oval;
    llvmo::ConstantArray_O::static_Kind = gctools::GCInfo<llvmo::ConstantArray_O>::Kind;
    core::af_setf_findClass(classllvmo__ConstantArray_Oval,llvmo::ConstantArray_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::ConstantArray_O>* cb = new LispObjectAllocatorFunctor<llvmo::ConstantArray_O>();
        llvmo::ConstantArray_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::ConstantArray_O::static_className() % (void*)(llvmo::ConstantArray_O::static_allocator) );
    classllvmo__ConstantArray_Oval->setAllocator(llvmo::ConstantArray_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::ConstantArray_O::static_className() );
//        llvmo::ConstantArray_O::___staticDereferencedNilInstance = new llvmo::ConstantArray_O();
//        llvmo::ConstantArray_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::ConstantArray_O::___staticDereferencedUnboundInstance = new llvmo::ConstantArray_O();
//        llvmo::ConstantArray_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::ConstantArray_O::_nil = _Nil<llvmo::ConstantArray_O>();
//        llvmo::ConstantArray_O::_unbound = _Unbound<llvmo::ConstantArray_O>();
    }
//    classllvmo__ConstantArray_Oval->setSupportsSlots(llvmo::ConstantArray_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__ConstantArray_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__ConstantDataSequential_Oval]"));
    core::BuiltInClass_sp classllvmo__ConstantDataSequential_Oval = core::BuiltInClass_O::create();
    classllvmo__ConstantDataSequential_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ConstantDataSequential_Oval,_lisp,llvmo::ConstantDataSequential_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::ConstantDataSequential_O>::id,llvmo::ConstantDataSequential_O::static_classSymbol());
    llvmo::ConstantDataSequential_O::___staticClass = classllvmo__ConstantDataSequential_Oval;
    llvmo::ConstantDataSequential_O::static_Kind = gctools::GCInfo<llvmo::ConstantDataSequential_O>::Kind;
    core::af_setf_findClass(classllvmo__ConstantDataSequential_Oval,llvmo::ConstantDataSequential_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::ConstantDataSequential_O>* cb = new LispObjectAllocatorFunctor<llvmo::ConstantDataSequential_O>();
        llvmo::ConstantDataSequential_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::ConstantDataSequential_O::static_className() % (void*)(llvmo::ConstantDataSequential_O::static_allocator) );
    classllvmo__ConstantDataSequential_Oval->setAllocator(llvmo::ConstantDataSequential_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::ConstantDataSequential_O::static_className() );
//        llvmo::ConstantDataSequential_O::___staticDereferencedNilInstance = new llvmo::ConstantDataSequential_O();
//        llvmo::ConstantDataSequential_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::ConstantDataSequential_O::___staticDereferencedUnboundInstance = new llvmo::ConstantDataSequential_O();
//        llvmo::ConstantDataSequential_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::ConstantDataSequential_O::_nil = _Nil<llvmo::ConstantDataSequential_O>();
//        llvmo::ConstantDataSequential_O::_unbound = _Unbound<llvmo::ConstantDataSequential_O>();
    }
//    classllvmo__ConstantDataSequential_Oval->setSupportsSlots(llvmo::ConstantDataSequential_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__ConstantDataSequential_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__ConstantExpr_Oval]"));
    core::BuiltInClass_sp classllvmo__ConstantExpr_Oval = core::BuiltInClass_O::create();
    classllvmo__ConstantExpr_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ConstantExpr_Oval,_lisp,llvmo::ConstantExpr_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::ConstantExpr_O>::id,llvmo::ConstantExpr_O::static_classSymbol());
    llvmo::ConstantExpr_O::___staticClass = classllvmo__ConstantExpr_Oval;
    llvmo::ConstantExpr_O::static_Kind = gctools::GCInfo<llvmo::ConstantExpr_O>::Kind;
    core::af_setf_findClass(classllvmo__ConstantExpr_Oval,llvmo::ConstantExpr_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::ConstantExpr_O>* cb = new LispObjectAllocatorFunctor<llvmo::ConstantExpr_O>();
        llvmo::ConstantExpr_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::ConstantExpr_O::static_className() % (void*)(llvmo::ConstantExpr_O::static_allocator) );
    classllvmo__ConstantExpr_Oval->setAllocator(llvmo::ConstantExpr_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::ConstantExpr_O::static_className() );
//        llvmo::ConstantExpr_O::___staticDereferencedNilInstance = new llvmo::ConstantExpr_O();
//        llvmo::ConstantExpr_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::ConstantExpr_O::___staticDereferencedUnboundInstance = new llvmo::ConstantExpr_O();
//        llvmo::ConstantExpr_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::ConstantExpr_O::_nil = _Nil<llvmo::ConstantExpr_O>();
//        llvmo::ConstantExpr_O::_unbound = _Unbound<llvmo::ConstantExpr_O>();
    }
//    classllvmo__ConstantExpr_Oval->setSupportsSlots(llvmo::ConstantExpr_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__ConstantExpr_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__ConstantFP_Oval]"));
    core::BuiltInClass_sp classllvmo__ConstantFP_Oval = core::BuiltInClass_O::create();
    classllvmo__ConstantFP_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ConstantFP_Oval,_lisp,llvmo::ConstantFP_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::ConstantFP_O>::id,llvmo::ConstantFP_O::static_classSymbol());
    llvmo::ConstantFP_O::___staticClass = classllvmo__ConstantFP_Oval;
    llvmo::ConstantFP_O::static_Kind = gctools::GCInfo<llvmo::ConstantFP_O>::Kind;
    core::af_setf_findClass(classllvmo__ConstantFP_Oval,llvmo::ConstantFP_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::ConstantFP_O>* cb = new LispObjectAllocatorFunctor<llvmo::ConstantFP_O>();
        llvmo::ConstantFP_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::ConstantFP_O::static_className() % (void*)(llvmo::ConstantFP_O::static_allocator) );
    classllvmo__ConstantFP_Oval->setAllocator(llvmo::ConstantFP_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::ConstantFP_O::static_className() );
//        llvmo::ConstantFP_O::___staticDereferencedNilInstance = new llvmo::ConstantFP_O();
//        llvmo::ConstantFP_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::ConstantFP_O::___staticDereferencedUnboundInstance = new llvmo::ConstantFP_O();
//        llvmo::ConstantFP_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::ConstantFP_O::_nil = _Nil<llvmo::ConstantFP_O>();
//        llvmo::ConstantFP_O::_unbound = _Unbound<llvmo::ConstantFP_O>();
    }
//    classllvmo__ConstantFP_Oval->setSupportsSlots(llvmo::ConstantFP_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__ConstantFP_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__ConstantInt_Oval]"));
    core::BuiltInClass_sp classllvmo__ConstantInt_Oval = core::BuiltInClass_O::create();
    classllvmo__ConstantInt_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ConstantInt_Oval,_lisp,llvmo::ConstantInt_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::ConstantInt_O>::id,llvmo::ConstantInt_O::static_classSymbol());
    llvmo::ConstantInt_O::___staticClass = classllvmo__ConstantInt_Oval;
    llvmo::ConstantInt_O::static_Kind = gctools::GCInfo<llvmo::ConstantInt_O>::Kind;
    core::af_setf_findClass(classllvmo__ConstantInt_Oval,llvmo::ConstantInt_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::ConstantInt_O>* cb = new LispObjectAllocatorFunctor<llvmo::ConstantInt_O>();
        llvmo::ConstantInt_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::ConstantInt_O::static_className() % (void*)(llvmo::ConstantInt_O::static_allocator) );
    classllvmo__ConstantInt_Oval->setAllocator(llvmo::ConstantInt_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::ConstantInt_O::static_className() );
//        llvmo::ConstantInt_O::___staticDereferencedNilInstance = new llvmo::ConstantInt_O();
//        llvmo::ConstantInt_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::ConstantInt_O::___staticDereferencedUnboundInstance = new llvmo::ConstantInt_O();
//        llvmo::ConstantInt_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::ConstantInt_O::_nil = _Nil<llvmo::ConstantInt_O>();
//        llvmo::ConstantInt_O::_unbound = _Unbound<llvmo::ConstantInt_O>();
    }
//    classllvmo__ConstantInt_Oval->setSupportsSlots(llvmo::ConstantInt_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__ConstantInt_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__ConstantPointerNull_Oval]"));
    core::BuiltInClass_sp classllvmo__ConstantPointerNull_Oval = core::BuiltInClass_O::create();
    classllvmo__ConstantPointerNull_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ConstantPointerNull_Oval,_lisp,llvmo::ConstantPointerNull_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::ConstantPointerNull_O>::id,llvmo::ConstantPointerNull_O::static_classSymbol());
    llvmo::ConstantPointerNull_O::___staticClass = classllvmo__ConstantPointerNull_Oval;
    llvmo::ConstantPointerNull_O::static_Kind = gctools::GCInfo<llvmo::ConstantPointerNull_O>::Kind;
    core::af_setf_findClass(classllvmo__ConstantPointerNull_Oval,llvmo::ConstantPointerNull_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::ConstantPointerNull_O>* cb = new LispObjectAllocatorFunctor<llvmo::ConstantPointerNull_O>();
        llvmo::ConstantPointerNull_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::ConstantPointerNull_O::static_className() % (void*)(llvmo::ConstantPointerNull_O::static_allocator) );
    classllvmo__ConstantPointerNull_Oval->setAllocator(llvmo::ConstantPointerNull_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::ConstantPointerNull_O::static_className() );
//        llvmo::ConstantPointerNull_O::___staticDereferencedNilInstance = new llvmo::ConstantPointerNull_O();
//        llvmo::ConstantPointerNull_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::ConstantPointerNull_O::___staticDereferencedUnboundInstance = new llvmo::ConstantPointerNull_O();
//        llvmo::ConstantPointerNull_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::ConstantPointerNull_O::_nil = _Nil<llvmo::ConstantPointerNull_O>();
//        llvmo::ConstantPointerNull_O::_unbound = _Unbound<llvmo::ConstantPointerNull_O>();
    }
//    classllvmo__ConstantPointerNull_Oval->setSupportsSlots(llvmo::ConstantPointerNull_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__ConstantPointerNull_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__DataLayoutPass_Oval]"));
    core::BuiltInClass_sp classllvmo__DataLayoutPass_Oval = core::BuiltInClass_O::create();
    classllvmo__DataLayoutPass_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__DataLayoutPass_Oval,_lisp,llvmo::DataLayoutPass_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::DataLayoutPass_O>::id,llvmo::DataLayoutPass_O::static_classSymbol());
    llvmo::DataLayoutPass_O::___staticClass = classllvmo__DataLayoutPass_Oval;
    llvmo::DataLayoutPass_O::static_Kind = gctools::GCInfo<llvmo::DataLayoutPass_O>::Kind;
    core::af_setf_findClass(classllvmo__DataLayoutPass_Oval,llvmo::DataLayoutPass_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::DataLayoutPass_O>* cb = new LispObjectAllocatorFunctor<llvmo::DataLayoutPass_O>();
        llvmo::DataLayoutPass_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::DataLayoutPass_O::static_className() % (void*)(llvmo::DataLayoutPass_O::static_allocator) );
    classllvmo__DataLayoutPass_Oval->setAllocator(llvmo::DataLayoutPass_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::DataLayoutPass_O::static_className() );
//        llvmo::DataLayoutPass_O::___staticDereferencedNilInstance = new llvmo::DataLayoutPass_O();
//        llvmo::DataLayoutPass_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::DataLayoutPass_O::___staticDereferencedUnboundInstance = new llvmo::DataLayoutPass_O();
//        llvmo::DataLayoutPass_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::DataLayoutPass_O::_nil = _Nil<llvmo::DataLayoutPass_O>();
//        llvmo::DataLayoutPass_O::_unbound = _Unbound<llvmo::DataLayoutPass_O>();
    }
//    classllvmo__DataLayoutPass_Oval->setSupportsSlots(llvmo::DataLayoutPass_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__DataLayoutPass_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__FenceInst_Oval]"));
    core::BuiltInClass_sp classllvmo__FenceInst_Oval = core::BuiltInClass_O::create();
    classllvmo__FenceInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__FenceInst_Oval,_lisp,llvmo::FenceInst_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::FenceInst_O>::id,llvmo::FenceInst_O::static_classSymbol());
    llvmo::FenceInst_O::___staticClass = classllvmo__FenceInst_Oval;
    llvmo::FenceInst_O::static_Kind = gctools::GCInfo<llvmo::FenceInst_O>::Kind;
    core::af_setf_findClass(classllvmo__FenceInst_Oval,llvmo::FenceInst_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::FenceInst_O>* cb = new LispObjectAllocatorFunctor<llvmo::FenceInst_O>();
        llvmo::FenceInst_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::FenceInst_O::static_className() % (void*)(llvmo::FenceInst_O::static_allocator) );
    classllvmo__FenceInst_Oval->setAllocator(llvmo::FenceInst_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::FenceInst_O::static_className() );
//        llvmo::FenceInst_O::___staticDereferencedNilInstance = new llvmo::FenceInst_O();
//        llvmo::FenceInst_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::FenceInst_O::___staticDereferencedUnboundInstance = new llvmo::FenceInst_O();
//        llvmo::FenceInst_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::FenceInst_O::_nil = _Nil<llvmo::FenceInst_O>();
//        llvmo::FenceInst_O::_unbound = _Unbound<llvmo::FenceInst_O>();
    }
//    classllvmo__FenceInst_Oval->setSupportsSlots(llvmo::FenceInst_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__FenceInst_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__GlobalValue_Oval]"));
    core::BuiltInClass_sp classllvmo__GlobalValue_Oval = core::BuiltInClass_O::create();
    classllvmo__GlobalValue_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__GlobalValue_Oval,_lisp,llvmo::GlobalValue_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::GlobalValue_O>::id,llvmo::GlobalValue_O::static_classSymbol());
    llvmo::GlobalValue_O::___staticClass = classllvmo__GlobalValue_Oval;
    llvmo::GlobalValue_O::static_Kind = gctools::GCInfo<llvmo::GlobalValue_O>::Kind;
    core::af_setf_findClass(classllvmo__GlobalValue_Oval,llvmo::GlobalValue_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::GlobalValue_O>* cb = new LispObjectAllocatorFunctor<llvmo::GlobalValue_O>();
        llvmo::GlobalValue_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::GlobalValue_O::static_className() % (void*)(llvmo::GlobalValue_O::static_allocator) );
    classllvmo__GlobalValue_Oval->setAllocator(llvmo::GlobalValue_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::GlobalValue_O::static_className() );
//        llvmo::GlobalValue_O::___staticDereferencedNilInstance = new llvmo::GlobalValue_O();
//        llvmo::GlobalValue_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::GlobalValue_O::___staticDereferencedUnboundInstance = new llvmo::GlobalValue_O();
//        llvmo::GlobalValue_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::GlobalValue_O::_nil = _Nil<llvmo::GlobalValue_O>();
//        llvmo::GlobalValue_O::_unbound = _Unbound<llvmo::GlobalValue_O>();
    }
//    classllvmo__GlobalValue_Oval->setSupportsSlots(llvmo::GlobalValue_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__GlobalValue_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__LandingPadInst_Oval]"));
    core::BuiltInClass_sp classllvmo__LandingPadInst_Oval = core::BuiltInClass_O::create();
    classllvmo__LandingPadInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__LandingPadInst_Oval,_lisp,llvmo::LandingPadInst_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::LandingPadInst_O>::id,llvmo::LandingPadInst_O::static_classSymbol());
    llvmo::LandingPadInst_O::___staticClass = classllvmo__LandingPadInst_Oval;
    llvmo::LandingPadInst_O::static_Kind = gctools::GCInfo<llvmo::LandingPadInst_O>::Kind;
    core::af_setf_findClass(classllvmo__LandingPadInst_Oval,llvmo::LandingPadInst_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::LandingPadInst_O>* cb = new LispObjectAllocatorFunctor<llvmo::LandingPadInst_O>();
        llvmo::LandingPadInst_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::LandingPadInst_O::static_className() % (void*)(llvmo::LandingPadInst_O::static_allocator) );
    classllvmo__LandingPadInst_Oval->setAllocator(llvmo::LandingPadInst_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::LandingPadInst_O::static_className() );
//        llvmo::LandingPadInst_O::___staticDereferencedNilInstance = new llvmo::LandingPadInst_O();
//        llvmo::LandingPadInst_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::LandingPadInst_O::___staticDereferencedUnboundInstance = new llvmo::LandingPadInst_O();
//        llvmo::LandingPadInst_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::LandingPadInst_O::_nil = _Nil<llvmo::LandingPadInst_O>();
//        llvmo::LandingPadInst_O::_unbound = _Unbound<llvmo::LandingPadInst_O>();
    }
//    classllvmo__LandingPadInst_Oval->setSupportsSlots(llvmo::LandingPadInst_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__LandingPadInst_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__PHINode_Oval]"));
    core::BuiltInClass_sp classllvmo__PHINode_Oval = core::BuiltInClass_O::create();
    classllvmo__PHINode_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__PHINode_Oval,_lisp,llvmo::PHINode_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::PHINode_O>::id,llvmo::PHINode_O::static_classSymbol());
    llvmo::PHINode_O::___staticClass = classllvmo__PHINode_Oval;
    llvmo::PHINode_O::static_Kind = gctools::GCInfo<llvmo::PHINode_O>::Kind;
    core::af_setf_findClass(classllvmo__PHINode_Oval,llvmo::PHINode_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::PHINode_O>* cb = new LispObjectAllocatorFunctor<llvmo::PHINode_O>();
        llvmo::PHINode_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::PHINode_O::static_className() % (void*)(llvmo::PHINode_O::static_allocator) );
    classllvmo__PHINode_Oval->setAllocator(llvmo::PHINode_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::PHINode_O::static_className() );
//        llvmo::PHINode_O::___staticDereferencedNilInstance = new llvmo::PHINode_O();
//        llvmo::PHINode_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::PHINode_O::___staticDereferencedUnboundInstance = new llvmo::PHINode_O();
//        llvmo::PHINode_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::PHINode_O::_nil = _Nil<llvmo::PHINode_O>();
//        llvmo::PHINode_O::_unbound = _Unbound<llvmo::PHINode_O>();
    }
//    classllvmo__PHINode_Oval->setSupportsSlots(llvmo::PHINode_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__PHINode_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__PointerType_Oval]"));
    core::BuiltInClass_sp classllvmo__PointerType_Oval = core::BuiltInClass_O::create();
    classllvmo__PointerType_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__PointerType_Oval,_lisp,llvmo::PointerType_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::PointerType_O>::id,llvmo::PointerType_O::static_classSymbol());
    llvmo::PointerType_O::___staticClass = classllvmo__PointerType_Oval;
    llvmo::PointerType_O::static_Kind = gctools::GCInfo<llvmo::PointerType_O>::Kind;
    core::af_setf_findClass(classllvmo__PointerType_Oval,llvmo::PointerType_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::PointerType_O>* cb = new LispObjectAllocatorFunctor<llvmo::PointerType_O>();
        llvmo::PointerType_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::PointerType_O::static_className() % (void*)(llvmo::PointerType_O::static_allocator) );
    classllvmo__PointerType_Oval->setAllocator(llvmo::PointerType_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::PointerType_O::static_className() );
//        llvmo::PointerType_O::___staticDereferencedNilInstance = new llvmo::PointerType_O();
//        llvmo::PointerType_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::PointerType_O::___staticDereferencedUnboundInstance = new llvmo::PointerType_O();
//        llvmo::PointerType_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::PointerType_O::_nil = _Nil<llvmo::PointerType_O>();
//        llvmo::PointerType_O::_unbound = _Unbound<llvmo::PointerType_O>();
    }
//    classllvmo__PointerType_Oval->setSupportsSlots(llvmo::PointerType_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__PointerType_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__StoreInst_Oval]"));
    core::BuiltInClass_sp classllvmo__StoreInst_Oval = core::BuiltInClass_O::create();
    classllvmo__StoreInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__StoreInst_Oval,_lisp,llvmo::StoreInst_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::StoreInst_O>::id,llvmo::StoreInst_O::static_classSymbol());
    llvmo::StoreInst_O::___staticClass = classllvmo__StoreInst_Oval;
    llvmo::StoreInst_O::static_Kind = gctools::GCInfo<llvmo::StoreInst_O>::Kind;
    core::af_setf_findClass(classllvmo__StoreInst_Oval,llvmo::StoreInst_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::StoreInst_O>* cb = new LispObjectAllocatorFunctor<llvmo::StoreInst_O>();
        llvmo::StoreInst_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::StoreInst_O::static_className() % (void*)(llvmo::StoreInst_O::static_allocator) );
    classllvmo__StoreInst_Oval->setAllocator(llvmo::StoreInst_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::StoreInst_O::static_className() );
//        llvmo::StoreInst_O::___staticDereferencedNilInstance = new llvmo::StoreInst_O();
//        llvmo::StoreInst_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::StoreInst_O::___staticDereferencedUnboundInstance = new llvmo::StoreInst_O();
//        llvmo::StoreInst_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::StoreInst_O::_nil = _Nil<llvmo::StoreInst_O>();
//        llvmo::StoreInst_O::_unbound = _Unbound<llvmo::StoreInst_O>();
    }
//    classllvmo__StoreInst_Oval->setSupportsSlots(llvmo::StoreInst_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__StoreInst_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__TerminatorInst_Oval]"));
    core::BuiltInClass_sp classllvmo__TerminatorInst_Oval = core::BuiltInClass_O::create();
    classllvmo__TerminatorInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__TerminatorInst_Oval,_lisp,llvmo::TerminatorInst_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::TerminatorInst_O>::id,llvmo::TerminatorInst_O::static_classSymbol());
    llvmo::TerminatorInst_O::___staticClass = classllvmo__TerminatorInst_Oval;
    llvmo::TerminatorInst_O::static_Kind = gctools::GCInfo<llvmo::TerminatorInst_O>::Kind;
    core::af_setf_findClass(classllvmo__TerminatorInst_Oval,llvmo::TerminatorInst_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::TerminatorInst_O>* cb = new LispObjectAllocatorFunctor<llvmo::TerminatorInst_O>();
        llvmo::TerminatorInst_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::TerminatorInst_O::static_className() % (void*)(llvmo::TerminatorInst_O::static_allocator) );
    classllvmo__TerminatorInst_Oval->setAllocator(llvmo::TerminatorInst_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::TerminatorInst_O::static_className() );
//        llvmo::TerminatorInst_O::___staticDereferencedNilInstance = new llvmo::TerminatorInst_O();
//        llvmo::TerminatorInst_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::TerminatorInst_O::___staticDereferencedUnboundInstance = new llvmo::TerminatorInst_O();
//        llvmo::TerminatorInst_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::TerminatorInst_O::_nil = _Nil<llvmo::TerminatorInst_O>();
//        llvmo::TerminatorInst_O::_unbound = _Unbound<llvmo::TerminatorInst_O>();
    }
//    classllvmo__TerminatorInst_Oval->setSupportsSlots(llvmo::TerminatorInst_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__TerminatorInst_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__UnaryInstruction_Oval]"));
    core::BuiltInClass_sp classllvmo__UnaryInstruction_Oval = core::BuiltInClass_O::create();
    classllvmo__UnaryInstruction_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__UnaryInstruction_Oval,_lisp,llvmo::UnaryInstruction_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::UnaryInstruction_O>::id,llvmo::UnaryInstruction_O::static_classSymbol());
    llvmo::UnaryInstruction_O::___staticClass = classllvmo__UnaryInstruction_Oval;
    llvmo::UnaryInstruction_O::static_Kind = gctools::GCInfo<llvmo::UnaryInstruction_O>::Kind;
    core::af_setf_findClass(classllvmo__UnaryInstruction_Oval,llvmo::UnaryInstruction_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::UnaryInstruction_O>* cb = new LispObjectAllocatorFunctor<llvmo::UnaryInstruction_O>();
        llvmo::UnaryInstruction_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::UnaryInstruction_O::static_className() % (void*)(llvmo::UnaryInstruction_O::static_allocator) );
    classllvmo__UnaryInstruction_Oval->setAllocator(llvmo::UnaryInstruction_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::UnaryInstruction_O::static_className() );
//        llvmo::UnaryInstruction_O::___staticDereferencedNilInstance = new llvmo::UnaryInstruction_O();
//        llvmo::UnaryInstruction_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::UnaryInstruction_O::___staticDereferencedUnboundInstance = new llvmo::UnaryInstruction_O();
//        llvmo::UnaryInstruction_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::UnaryInstruction_O::_nil = _Nil<llvmo::UnaryInstruction_O>();
//        llvmo::UnaryInstruction_O::_unbound = _Unbound<llvmo::UnaryInstruction_O>();
    }
//    classllvmo__UnaryInstruction_Oval->setSupportsSlots(llvmo::UnaryInstruction_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__UnaryInstruction_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__UndefValue_Oval]"));
    core::BuiltInClass_sp classllvmo__UndefValue_Oval = core::BuiltInClass_O::create();
    classllvmo__UndefValue_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__UndefValue_Oval,_lisp,llvmo::UndefValue_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::UndefValue_O>::id,llvmo::UndefValue_O::static_classSymbol());
    llvmo::UndefValue_O::___staticClass = classllvmo__UndefValue_Oval;
    llvmo::UndefValue_O::static_Kind = gctools::GCInfo<llvmo::UndefValue_O>::Kind;
    core::af_setf_findClass(classllvmo__UndefValue_Oval,llvmo::UndefValue_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::UndefValue_O>* cb = new LispObjectAllocatorFunctor<llvmo::UndefValue_O>();
        llvmo::UndefValue_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::UndefValue_O::static_className() % (void*)(llvmo::UndefValue_O::static_allocator) );
    classllvmo__UndefValue_Oval->setAllocator(llvmo::UndefValue_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::UndefValue_O::static_className() );
//        llvmo::UndefValue_O::___staticDereferencedNilInstance = new llvmo::UndefValue_O();
//        llvmo::UndefValue_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::UndefValue_O::___staticDereferencedUnboundInstance = new llvmo::UndefValue_O();
//        llvmo::UndefValue_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::UndefValue_O::_nil = _Nil<llvmo::UndefValue_O>();
//        llvmo::UndefValue_O::_unbound = _Unbound<llvmo::UndefValue_O>();
    }
//    classllvmo__UndefValue_Oval->setSupportsSlots(llvmo::UndefValue_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__UndefValue_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__VectorType_Oval]"));
    core::BuiltInClass_sp classllvmo__VectorType_Oval = core::BuiltInClass_O::create();
    classllvmo__VectorType_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__VectorType_Oval,_lisp,llvmo::VectorType_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::VectorType_O>::id,llvmo::VectorType_O::static_classSymbol());
    llvmo::VectorType_O::___staticClass = classllvmo__VectorType_Oval;
    llvmo::VectorType_O::static_Kind = gctools::GCInfo<llvmo::VectorType_O>::Kind;
    core::af_setf_findClass(classllvmo__VectorType_Oval,llvmo::VectorType_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::VectorType_O>* cb = new LispObjectAllocatorFunctor<llvmo::VectorType_O>();
        llvmo::VectorType_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::VectorType_O::static_className() % (void*)(llvmo::VectorType_O::static_allocator) );
    classllvmo__VectorType_Oval->setAllocator(llvmo::VectorType_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::VectorType_O::static_className() );
//        llvmo::VectorType_O::___staticDereferencedNilInstance = new llvmo::VectorType_O();
//        llvmo::VectorType_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::VectorType_O::___staticDereferencedUnboundInstance = new llvmo::VectorType_O();
//        llvmo::VectorType_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::VectorType_O::_nil = _Nil<llvmo::VectorType_O>();
//        llvmo::VectorType_O::_unbound = _Unbound<llvmo::VectorType_O>();
    }
//    classllvmo__VectorType_Oval->setSupportsSlots(llvmo::VectorType_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__VectorType_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__AllocaInst_Oval]"));
    core::BuiltInClass_sp classllvmo__AllocaInst_Oval = core::BuiltInClass_O::create();
    classllvmo__AllocaInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__AllocaInst_Oval,_lisp,llvmo::AllocaInst_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::AllocaInst_O>::id,llvmo::AllocaInst_O::static_classSymbol());
    llvmo::AllocaInst_O::___staticClass = classllvmo__AllocaInst_Oval;
    llvmo::AllocaInst_O::static_Kind = gctools::GCInfo<llvmo::AllocaInst_O>::Kind;
    core::af_setf_findClass(classllvmo__AllocaInst_Oval,llvmo::AllocaInst_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::AllocaInst_O>* cb = new LispObjectAllocatorFunctor<llvmo::AllocaInst_O>();
        llvmo::AllocaInst_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::AllocaInst_O::static_className() % (void*)(llvmo::AllocaInst_O::static_allocator) );
    classllvmo__AllocaInst_Oval->setAllocator(llvmo::AllocaInst_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::AllocaInst_O::static_className() );
//        llvmo::AllocaInst_O::___staticDereferencedNilInstance = new llvmo::AllocaInst_O();
//        llvmo::AllocaInst_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::AllocaInst_O::___staticDereferencedUnboundInstance = new llvmo::AllocaInst_O();
//        llvmo::AllocaInst_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::AllocaInst_O::_nil = _Nil<llvmo::AllocaInst_O>();
//        llvmo::AllocaInst_O::_unbound = _Unbound<llvmo::AllocaInst_O>();
    }
//    classllvmo__AllocaInst_Oval->setSupportsSlots(llvmo::AllocaInst_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__AllocaInst_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__BranchInst_Oval]"));
    core::BuiltInClass_sp classllvmo__BranchInst_Oval = core::BuiltInClass_O::create();
    classllvmo__BranchInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__BranchInst_Oval,_lisp,llvmo::BranchInst_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::BranchInst_O>::id,llvmo::BranchInst_O::static_classSymbol());
    llvmo::BranchInst_O::___staticClass = classllvmo__BranchInst_Oval;
    llvmo::BranchInst_O::static_Kind = gctools::GCInfo<llvmo::BranchInst_O>::Kind;
    core::af_setf_findClass(classllvmo__BranchInst_Oval,llvmo::BranchInst_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::BranchInst_O>* cb = new LispObjectAllocatorFunctor<llvmo::BranchInst_O>();
        llvmo::BranchInst_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::BranchInst_O::static_className() % (void*)(llvmo::BranchInst_O::static_allocator) );
    classllvmo__BranchInst_Oval->setAllocator(llvmo::BranchInst_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::BranchInst_O::static_className() );
//        llvmo::BranchInst_O::___staticDereferencedNilInstance = new llvmo::BranchInst_O();
//        llvmo::BranchInst_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::BranchInst_O::___staticDereferencedUnboundInstance = new llvmo::BranchInst_O();
//        llvmo::BranchInst_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::BranchInst_O::_nil = _Nil<llvmo::BranchInst_O>();
//        llvmo::BranchInst_O::_unbound = _Unbound<llvmo::BranchInst_O>();
    }
//    classllvmo__BranchInst_Oval->setSupportsSlots(llvmo::BranchInst_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__BranchInst_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__ConstantDataArray_Oval]"));
    core::BuiltInClass_sp classllvmo__ConstantDataArray_Oval = core::BuiltInClass_O::create();
    classllvmo__ConstantDataArray_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ConstantDataArray_Oval,_lisp,llvmo::ConstantDataArray_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::ConstantDataArray_O>::id,llvmo::ConstantDataArray_O::static_classSymbol());
    llvmo::ConstantDataArray_O::___staticClass = classllvmo__ConstantDataArray_Oval;
    llvmo::ConstantDataArray_O::static_Kind = gctools::GCInfo<llvmo::ConstantDataArray_O>::Kind;
    core::af_setf_findClass(classllvmo__ConstantDataArray_Oval,llvmo::ConstantDataArray_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::ConstantDataArray_O>* cb = new LispObjectAllocatorFunctor<llvmo::ConstantDataArray_O>();
        llvmo::ConstantDataArray_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::ConstantDataArray_O::static_className() % (void*)(llvmo::ConstantDataArray_O::static_allocator) );
    classllvmo__ConstantDataArray_Oval->setAllocator(llvmo::ConstantDataArray_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::ConstantDataArray_O::static_className() );
//        llvmo::ConstantDataArray_O::___staticDereferencedNilInstance = new llvmo::ConstantDataArray_O();
//        llvmo::ConstantDataArray_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::ConstantDataArray_O::___staticDereferencedUnboundInstance = new llvmo::ConstantDataArray_O();
//        llvmo::ConstantDataArray_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::ConstantDataArray_O::_nil = _Nil<llvmo::ConstantDataArray_O>();
//        llvmo::ConstantDataArray_O::_unbound = _Unbound<llvmo::ConstantDataArray_O>();
    }
//    classllvmo__ConstantDataArray_Oval->setSupportsSlots(llvmo::ConstantDataArray_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__ConstantDataArray_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__Function_Oval]"));
    core::BuiltInClass_sp classllvmo__Function_Oval = core::BuiltInClass_O::create();
    classllvmo__Function_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__Function_Oval,_lisp,llvmo::Function_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::Function_O>::id,llvmo::Function_O::static_classSymbol());
    llvmo::Function_O::___staticClass = classllvmo__Function_Oval;
    llvmo::Function_O::static_Kind = gctools::GCInfo<llvmo::Function_O>::Kind;
    core::af_setf_findClass(classllvmo__Function_Oval,llvmo::Function_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::Function_O>* cb = new LispObjectAllocatorFunctor<llvmo::Function_O>();
        llvmo::Function_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::Function_O::static_className() % (void*)(llvmo::Function_O::static_allocator) );
    classllvmo__Function_Oval->setAllocator(llvmo::Function_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::Function_O::static_className() );
//        llvmo::Function_O::___staticDereferencedNilInstance = new llvmo::Function_O();
//        llvmo::Function_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::Function_O::___staticDereferencedUnboundInstance = new llvmo::Function_O();
//        llvmo::Function_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::Function_O::_nil = _Nil<llvmo::Function_O>();
//        llvmo::Function_O::_unbound = _Unbound<llvmo::Function_O>();
    }
//    classllvmo__Function_Oval->setSupportsSlots(llvmo::Function_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__Function_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__GlobalVariable_Oval]"));
    core::BuiltInClass_sp classllvmo__GlobalVariable_Oval = core::BuiltInClass_O::create();
    classllvmo__GlobalVariable_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__GlobalVariable_Oval,_lisp,llvmo::GlobalVariable_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::GlobalVariable_O>::id,llvmo::GlobalVariable_O::static_classSymbol());
    llvmo::GlobalVariable_O::___staticClass = classllvmo__GlobalVariable_Oval;
    llvmo::GlobalVariable_O::static_Kind = gctools::GCInfo<llvmo::GlobalVariable_O>::Kind;
    core::af_setf_findClass(classllvmo__GlobalVariable_Oval,llvmo::GlobalVariable_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::GlobalVariable_O>* cb = new LispObjectAllocatorFunctor<llvmo::GlobalVariable_O>();
        llvmo::GlobalVariable_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::GlobalVariable_O::static_className() % (void*)(llvmo::GlobalVariable_O::static_allocator) );
    classllvmo__GlobalVariable_Oval->setAllocator(llvmo::GlobalVariable_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::GlobalVariable_O::static_className() );
//        llvmo::GlobalVariable_O::___staticDereferencedNilInstance = new llvmo::GlobalVariable_O();
//        llvmo::GlobalVariable_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::GlobalVariable_O::___staticDereferencedUnboundInstance = new llvmo::GlobalVariable_O();
//        llvmo::GlobalVariable_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::GlobalVariable_O::_nil = _Nil<llvmo::GlobalVariable_O>();
//        llvmo::GlobalVariable_O::_unbound = _Unbound<llvmo::GlobalVariable_O>();
    }
//    classllvmo__GlobalVariable_Oval->setSupportsSlots(llvmo::GlobalVariable_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__GlobalVariable_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__IndirectBrInst_Oval]"));
    core::BuiltInClass_sp classllvmo__IndirectBrInst_Oval = core::BuiltInClass_O::create();
    classllvmo__IndirectBrInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__IndirectBrInst_Oval,_lisp,llvmo::IndirectBrInst_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::IndirectBrInst_O>::id,llvmo::IndirectBrInst_O::static_classSymbol());
    llvmo::IndirectBrInst_O::___staticClass = classllvmo__IndirectBrInst_Oval;
    llvmo::IndirectBrInst_O::static_Kind = gctools::GCInfo<llvmo::IndirectBrInst_O>::Kind;
    core::af_setf_findClass(classllvmo__IndirectBrInst_Oval,llvmo::IndirectBrInst_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::IndirectBrInst_O>* cb = new LispObjectAllocatorFunctor<llvmo::IndirectBrInst_O>();
        llvmo::IndirectBrInst_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::IndirectBrInst_O::static_className() % (void*)(llvmo::IndirectBrInst_O::static_allocator) );
    classllvmo__IndirectBrInst_Oval->setAllocator(llvmo::IndirectBrInst_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::IndirectBrInst_O::static_className() );
//        llvmo::IndirectBrInst_O::___staticDereferencedNilInstance = new llvmo::IndirectBrInst_O();
//        llvmo::IndirectBrInst_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::IndirectBrInst_O::___staticDereferencedUnboundInstance = new llvmo::IndirectBrInst_O();
//        llvmo::IndirectBrInst_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::IndirectBrInst_O::_nil = _Nil<llvmo::IndirectBrInst_O>();
//        llvmo::IndirectBrInst_O::_unbound = _Unbound<llvmo::IndirectBrInst_O>();
    }
//    classllvmo__IndirectBrInst_Oval->setSupportsSlots(llvmo::IndirectBrInst_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__IndirectBrInst_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__InvokeInst_Oval]"));
    core::BuiltInClass_sp classllvmo__InvokeInst_Oval = core::BuiltInClass_O::create();
    classllvmo__InvokeInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__InvokeInst_Oval,_lisp,llvmo::InvokeInst_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::InvokeInst_O>::id,llvmo::InvokeInst_O::static_classSymbol());
    llvmo::InvokeInst_O::___staticClass = classllvmo__InvokeInst_Oval;
    llvmo::InvokeInst_O::static_Kind = gctools::GCInfo<llvmo::InvokeInst_O>::Kind;
    core::af_setf_findClass(classllvmo__InvokeInst_Oval,llvmo::InvokeInst_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::InvokeInst_O>* cb = new LispObjectAllocatorFunctor<llvmo::InvokeInst_O>();
        llvmo::InvokeInst_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::InvokeInst_O::static_className() % (void*)(llvmo::InvokeInst_O::static_allocator) );
    classllvmo__InvokeInst_Oval->setAllocator(llvmo::InvokeInst_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::InvokeInst_O::static_className() );
//        llvmo::InvokeInst_O::___staticDereferencedNilInstance = new llvmo::InvokeInst_O();
//        llvmo::InvokeInst_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::InvokeInst_O::___staticDereferencedUnboundInstance = new llvmo::InvokeInst_O();
//        llvmo::InvokeInst_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::InvokeInst_O::_nil = _Nil<llvmo::InvokeInst_O>();
//        llvmo::InvokeInst_O::_unbound = _Unbound<llvmo::InvokeInst_O>();
    }
//    classllvmo__InvokeInst_Oval->setSupportsSlots(llvmo::InvokeInst_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__InvokeInst_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__LoadInst_Oval]"));
    core::BuiltInClass_sp classllvmo__LoadInst_Oval = core::BuiltInClass_O::create();
    classllvmo__LoadInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__LoadInst_Oval,_lisp,llvmo::LoadInst_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::LoadInst_O>::id,llvmo::LoadInst_O::static_classSymbol());
    llvmo::LoadInst_O::___staticClass = classllvmo__LoadInst_Oval;
    llvmo::LoadInst_O::static_Kind = gctools::GCInfo<llvmo::LoadInst_O>::Kind;
    core::af_setf_findClass(classllvmo__LoadInst_Oval,llvmo::LoadInst_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::LoadInst_O>* cb = new LispObjectAllocatorFunctor<llvmo::LoadInst_O>();
        llvmo::LoadInst_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::LoadInst_O::static_className() % (void*)(llvmo::LoadInst_O::static_allocator) );
    classllvmo__LoadInst_Oval->setAllocator(llvmo::LoadInst_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::LoadInst_O::static_className() );
//        llvmo::LoadInst_O::___staticDereferencedNilInstance = new llvmo::LoadInst_O();
//        llvmo::LoadInst_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::LoadInst_O::___staticDereferencedUnboundInstance = new llvmo::LoadInst_O();
//        llvmo::LoadInst_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::LoadInst_O::_nil = _Nil<llvmo::LoadInst_O>();
//        llvmo::LoadInst_O::_unbound = _Unbound<llvmo::LoadInst_O>();
    }
//    classllvmo__LoadInst_Oval->setSupportsSlots(llvmo::LoadInst_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__LoadInst_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__ResumeInst_Oval]"));
    core::BuiltInClass_sp classllvmo__ResumeInst_Oval = core::BuiltInClass_O::create();
    classllvmo__ResumeInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ResumeInst_Oval,_lisp,llvmo::ResumeInst_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::ResumeInst_O>::id,llvmo::ResumeInst_O::static_classSymbol());
    llvmo::ResumeInst_O::___staticClass = classllvmo__ResumeInst_Oval;
    llvmo::ResumeInst_O::static_Kind = gctools::GCInfo<llvmo::ResumeInst_O>::Kind;
    core::af_setf_findClass(classllvmo__ResumeInst_Oval,llvmo::ResumeInst_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::ResumeInst_O>* cb = new LispObjectAllocatorFunctor<llvmo::ResumeInst_O>();
        llvmo::ResumeInst_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::ResumeInst_O::static_className() % (void*)(llvmo::ResumeInst_O::static_allocator) );
    classllvmo__ResumeInst_Oval->setAllocator(llvmo::ResumeInst_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::ResumeInst_O::static_className() );
//        llvmo::ResumeInst_O::___staticDereferencedNilInstance = new llvmo::ResumeInst_O();
//        llvmo::ResumeInst_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::ResumeInst_O::___staticDereferencedUnboundInstance = new llvmo::ResumeInst_O();
//        llvmo::ResumeInst_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::ResumeInst_O::_nil = _Nil<llvmo::ResumeInst_O>();
//        llvmo::ResumeInst_O::_unbound = _Unbound<llvmo::ResumeInst_O>();
    }
//    classllvmo__ResumeInst_Oval->setSupportsSlots(llvmo::ResumeInst_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__ResumeInst_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__ReturnInst_Oval]"));
    core::BuiltInClass_sp classllvmo__ReturnInst_Oval = core::BuiltInClass_O::create();
    classllvmo__ReturnInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ReturnInst_Oval,_lisp,llvmo::ReturnInst_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::ReturnInst_O>::id,llvmo::ReturnInst_O::static_classSymbol());
    llvmo::ReturnInst_O::___staticClass = classllvmo__ReturnInst_Oval;
    llvmo::ReturnInst_O::static_Kind = gctools::GCInfo<llvmo::ReturnInst_O>::Kind;
    core::af_setf_findClass(classllvmo__ReturnInst_Oval,llvmo::ReturnInst_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::ReturnInst_O>* cb = new LispObjectAllocatorFunctor<llvmo::ReturnInst_O>();
        llvmo::ReturnInst_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::ReturnInst_O::static_className() % (void*)(llvmo::ReturnInst_O::static_allocator) );
    classllvmo__ReturnInst_Oval->setAllocator(llvmo::ReturnInst_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::ReturnInst_O::static_className() );
//        llvmo::ReturnInst_O::___staticDereferencedNilInstance = new llvmo::ReturnInst_O();
//        llvmo::ReturnInst_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::ReturnInst_O::___staticDereferencedUnboundInstance = new llvmo::ReturnInst_O();
//        llvmo::ReturnInst_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::ReturnInst_O::_nil = _Nil<llvmo::ReturnInst_O>();
//        llvmo::ReturnInst_O::_unbound = _Unbound<llvmo::ReturnInst_O>();
    }
//    classllvmo__ReturnInst_Oval->setSupportsSlots(llvmo::ReturnInst_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__ReturnInst_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__SwitchInst_Oval]"));
    core::BuiltInClass_sp classllvmo__SwitchInst_Oval = core::BuiltInClass_O::create();
    classllvmo__SwitchInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__SwitchInst_Oval,_lisp,llvmo::SwitchInst_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::SwitchInst_O>::id,llvmo::SwitchInst_O::static_classSymbol());
    llvmo::SwitchInst_O::___staticClass = classllvmo__SwitchInst_Oval;
    llvmo::SwitchInst_O::static_Kind = gctools::GCInfo<llvmo::SwitchInst_O>::Kind;
    core::af_setf_findClass(classllvmo__SwitchInst_Oval,llvmo::SwitchInst_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::SwitchInst_O>* cb = new LispObjectAllocatorFunctor<llvmo::SwitchInst_O>();
        llvmo::SwitchInst_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::SwitchInst_O::static_className() % (void*)(llvmo::SwitchInst_O::static_allocator) );
    classllvmo__SwitchInst_Oval->setAllocator(llvmo::SwitchInst_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::SwitchInst_O::static_className() );
//        llvmo::SwitchInst_O::___staticDereferencedNilInstance = new llvmo::SwitchInst_O();
//        llvmo::SwitchInst_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::SwitchInst_O::___staticDereferencedUnboundInstance = new llvmo::SwitchInst_O();
//        llvmo::SwitchInst_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::SwitchInst_O::_nil = _Nil<llvmo::SwitchInst_O>();
//        llvmo::SwitchInst_O::_unbound = _Unbound<llvmo::SwitchInst_O>();
    }
//    classllvmo__SwitchInst_Oval->setSupportsSlots(llvmo::SwitchInst_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__SwitchInst_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__UnreachableInst_Oval]"));
    core::BuiltInClass_sp classllvmo__UnreachableInst_Oval = core::BuiltInClass_O::create();
    classllvmo__UnreachableInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__UnreachableInst_Oval,_lisp,llvmo::UnreachableInst_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::UnreachableInst_O>::id,llvmo::UnreachableInst_O::static_classSymbol());
    llvmo::UnreachableInst_O::___staticClass = classllvmo__UnreachableInst_Oval;
    llvmo::UnreachableInst_O::static_Kind = gctools::GCInfo<llvmo::UnreachableInst_O>::Kind;
    core::af_setf_findClass(classllvmo__UnreachableInst_Oval,llvmo::UnreachableInst_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::UnreachableInst_O>* cb = new LispObjectAllocatorFunctor<llvmo::UnreachableInst_O>();
        llvmo::UnreachableInst_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::UnreachableInst_O::static_className() % (void*)(llvmo::UnreachableInst_O::static_allocator) );
    classllvmo__UnreachableInst_Oval->setAllocator(llvmo::UnreachableInst_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::UnreachableInst_O::static_className() );
//        llvmo::UnreachableInst_O::___staticDereferencedNilInstance = new llvmo::UnreachableInst_O();
//        llvmo::UnreachableInst_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::UnreachableInst_O::___staticDereferencedUnboundInstance = new llvmo::UnreachableInst_O();
//        llvmo::UnreachableInst_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::UnreachableInst_O::_nil = _Nil<llvmo::UnreachableInst_O>();
//        llvmo::UnreachableInst_O::_unbound = _Unbound<llvmo::UnreachableInst_O>();
    }
//    classllvmo__UnreachableInst_Oval->setSupportsSlots(llvmo::UnreachableInst_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__UnreachableInst_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */

    LOG(BF("Creating class[classllvmo__VAArgInst_Oval]"));
    core::BuiltInClass_sp classllvmo__VAArgInst_Oval = core::BuiltInClass_O::create();
    classllvmo__VAArgInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__VAArgInst_Oval,_lisp,llvmo::VAArgInst_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::VAArgInst_O>::id,llvmo::VAArgInst_O::static_classSymbol());
    llvmo::VAArgInst_O::___staticClass = classllvmo__VAArgInst_Oval;
    llvmo::VAArgInst_O::static_Kind = gctools::GCInfo<llvmo::VAArgInst_O>::Kind;
    core::af_setf_findClass(classllvmo__VAArgInst_Oval,llvmo::VAArgInst_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        LispObjectAllocatorFunctor<llvmo::VAArgInst_O>* cb = new LispObjectAllocatorFunctor<llvmo::VAArgInst_O>();
        llvmo::VAArgInst_O::___set_static_allocator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::VAArgInst_O::static_className() % (void*)(llvmo::VAArgInst_O::static_allocator) );
    classllvmo__VAArgInst_Oval->setAllocator(llvmo::VAArgInst_O::static_allocator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::VAArgInst_O::static_className() );
//        llvmo::VAArgInst_O::___staticDereferencedNilInstance = new llvmo::VAArgInst_O();
//        llvmo::VAArgInst_O::___staticDereferencedNilInstance->___make_nil(); /* Unique NIL for class*/
//        llvmo::VAArgInst_O::___staticDereferencedUnboundInstance = new llvmo::VAArgInst_O();
//        llvmo::VAArgInst_O::___staticDereferencedUnboundInstance->___make_unbound(); /* Unique UNBOUND for class */
//        llvmo::VAArgInst_O::_nil = _Nil<llvmo::VAArgInst_O>();
//        llvmo::VAArgInst_O::_unbound = _Unbound<llvmo::VAArgInst_O>();
    }
//    classllvmo__VAArgInst_Oval->setSupportsSlots(llvmo::VAArgInst_O::static_supportsSlots());
    /* ----- the class and its nil are now defined and so is classllvmo__VAArgInst_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to T_O::_nil in stage3   ----- */
#endif // CREATE_CLASS
#undef CREATE_CLASS
#ifdef DUMP_INFO_CLASS // {
// Depends on nothing

    LOG(BF("---    dump_info   --- className: llvmo::APFloat_O @ %X") % classllvmo__APFloat_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::APFloat_O::static_className() % llvmo::APFloat_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::APInt_O @ %X") % classllvmo__APInt_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::APInt_O::static_className() % llvmo::APInt_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::Attribute_O @ %X") % classllvmo__Attribute_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::Attribute_O::static_className() % llvmo::Attribute_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::DIBuilder_O @ %X") % classllvmo__DIBuilder_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::DIBuilder_O::static_className() % llvmo::DIBuilder_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::DataLayout_O @ %X") % classllvmo__DataLayout_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::DataLayout_O::static_className() % llvmo::DataLayout_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::DebugInfo_O @ %X") % classllvmo__DebugInfo_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::DebugInfo_O::static_className() % llvmo::DebugInfo_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::DebugLoc_O @ %X") % classllvmo__DebugLoc_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::DebugLoc_O::static_className() % llvmo::DebugLoc_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::EngineBuilder_O @ %X") % classllvmo__EngineBuilder_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::EngineBuilder_O::static_className() % llvmo::EngineBuilder_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::ExecutionEngine_O @ %X") % classllvmo__ExecutionEngine_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::ExecutionEngine_O::static_className() % llvmo::ExecutionEngine_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::IRBuilderBase_O @ %X") % classllvmo__IRBuilderBase_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::IRBuilderBase_O::static_className() % llvmo::IRBuilderBase_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::InsertPoint_O @ %X") % classllvmo__InsertPoint_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::InsertPoint_O::static_className() % llvmo::InsertPoint_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::LLVMContext_O @ %X") % classllvmo__LLVMContext_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::LLVMContext_O::static_className() % llvmo::LLVMContext_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::Linker_O @ %X") % classllvmo__Linker_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::Linker_O::static_className() % llvmo::Linker_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::Module_O @ %X") % classllvmo__Module_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::Module_O::static_className() % llvmo::Module_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::NamedMDNode_O @ %X") % classllvmo__NamedMDNode_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::NamedMDNode_O::static_className() % llvmo::NamedMDNode_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::PassManagerBase_O @ %X") % classllvmo__PassManagerBase_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::PassManagerBase_O::static_className() % llvmo::PassManagerBase_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::PassManagerBuilder_O @ %X") % classllvmo__PassManagerBuilder_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::PassManagerBuilder_O::static_className() % llvmo::PassManagerBuilder_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::Pass_O @ %X") % classllvmo__Pass_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::Pass_O::static_className() % llvmo::Pass_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::Type_O @ %X") % classllvmo__Type_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::Type_O::static_className() % llvmo::Type_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::Value_O @ %X") % classllvmo__Value_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::Value_O::static_className() % llvmo::Value_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::Argument_O @ %X") % classllvmo__Argument_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::Argument_O::static_className() % llvmo::Argument_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::BasicBlock_O @ %X") % classllvmo__BasicBlock_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::BasicBlock_O::static_className() % llvmo::BasicBlock_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::CompositeType_O @ %X") % classllvmo__CompositeType_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::CompositeType_O::static_className() % llvmo::CompositeType_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::DIArray_O @ %X") % classllvmo__DIArray_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::DIArray_O::static_className() % llvmo::DIArray_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::DIBasicType_O @ %X") % classllvmo__DIBasicType_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::DIBasicType_O::static_className() % llvmo::DIBasicType_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::DICompileUnit_O @ %X") % classllvmo__DICompileUnit_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::DICompileUnit_O::static_className() % llvmo::DICompileUnit_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::DICompositeType_O @ %X") % classllvmo__DICompositeType_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::DICompositeType_O::static_className() % llvmo::DICompositeType_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::DIDerivedType_O @ %X") % classllvmo__DIDerivedType_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::DIDerivedType_O::static_className() % llvmo::DIDerivedType_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::DIDescriptor_O @ %X") % classllvmo__DIDescriptor_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::DIDescriptor_O::static_className() % llvmo::DIDescriptor_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::DIFile_O @ %X") % classllvmo__DIFile_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::DIFile_O::static_className() % llvmo::DIFile_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::DILexicalBlock_O @ %X") % classllvmo__DILexicalBlock_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::DILexicalBlock_O::static_className() % llvmo::DILexicalBlock_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::DIScope_O @ %X") % classllvmo__DIScope_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::DIScope_O::static_className() % llvmo::DIScope_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::DISubprogram_O @ %X") % classllvmo__DISubprogram_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::DISubprogram_O::static_className() % llvmo::DISubprogram_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::DIType_O @ %X") % classllvmo__DIType_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::DIType_O::static_className() % llvmo::DIType_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::FunctionPassManager_O @ %X") % classllvmo__FunctionPassManager_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::FunctionPassManager_O::static_className() % llvmo::FunctionPassManager_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::FunctionPass_O @ %X") % classllvmo__FunctionPass_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::FunctionPass_O::static_className() % llvmo::FunctionPass_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::FunctionType_O @ %X") % classllvmo__FunctionType_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::FunctionType_O::static_className() % llvmo::FunctionType_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::IRBuilder_O @ %X") % classllvmo__IRBuilder_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::IRBuilder_O::static_className() % llvmo::IRBuilder_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::IntegerType_O @ %X") % classllvmo__IntegerType_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::IntegerType_O::static_className() % llvmo::IntegerType_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::MDNode_O @ %X") % classllvmo__MDNode_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::MDNode_O::static_className() % llvmo::MDNode_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::MDString_O @ %X") % classllvmo__MDString_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::MDString_O::static_className() % llvmo::MDString_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::ModulePass_O @ %X") % classllvmo__ModulePass_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::ModulePass_O::static_className() % llvmo::ModulePass_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::PassManager_O @ %X") % classllvmo__PassManager_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::PassManager_O::static_className() % llvmo::PassManager_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::User_O @ %X") % classllvmo__User_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::User_O::static_className() % llvmo::User_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::Constant_O @ %X") % classllvmo__Constant_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::Constant_O::static_className() % llvmo::Constant_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::ImmutablePass_O @ %X") % classllvmo__ImmutablePass_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::ImmutablePass_O::static_className() % llvmo::ImmutablePass_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::Instruction_O @ %X") % classllvmo__Instruction_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::Instruction_O::static_className() % llvmo::Instruction_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::SequentialType_O @ %X") % classllvmo__SequentialType_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::SequentialType_O::static_className() % llvmo::SequentialType_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::StructType_O @ %X") % classllvmo__StructType_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::StructType_O::static_className() % llvmo::StructType_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::ArrayType_O @ %X") % classllvmo__ArrayType_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::ArrayType_O::static_className() % llvmo::ArrayType_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::AtomicCmpXchgInst_O @ %X") % classllvmo__AtomicCmpXchgInst_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::AtomicCmpXchgInst_O::static_className() % llvmo::AtomicCmpXchgInst_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::AtomicRMWInst_O @ %X") % classllvmo__AtomicRMWInst_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::AtomicRMWInst_O::static_className() % llvmo::AtomicRMWInst_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::BlockAddress_O @ %X") % classllvmo__BlockAddress_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::BlockAddress_O::static_className() % llvmo::BlockAddress_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::CallInst_O @ %X") % classllvmo__CallInst_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::CallInst_O::static_className() % llvmo::CallInst_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::ConstantArray_O @ %X") % classllvmo__ConstantArray_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::ConstantArray_O::static_className() % llvmo::ConstantArray_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::ConstantDataSequential_O @ %X") % classllvmo__ConstantDataSequential_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::ConstantDataSequential_O::static_className() % llvmo::ConstantDataSequential_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::ConstantExpr_O @ %X") % classllvmo__ConstantExpr_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::ConstantExpr_O::static_className() % llvmo::ConstantExpr_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::ConstantFP_O @ %X") % classllvmo__ConstantFP_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::ConstantFP_O::static_className() % llvmo::ConstantFP_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::ConstantInt_O @ %X") % classllvmo__ConstantInt_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::ConstantInt_O::static_className() % llvmo::ConstantInt_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::ConstantPointerNull_O @ %X") % classllvmo__ConstantPointerNull_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::ConstantPointerNull_O::static_className() % llvmo::ConstantPointerNull_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::DataLayoutPass_O @ %X") % classllvmo__DataLayoutPass_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::DataLayoutPass_O::static_className() % llvmo::DataLayoutPass_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::FenceInst_O @ %X") % classllvmo__FenceInst_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::FenceInst_O::static_className() % llvmo::FenceInst_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::GlobalValue_O @ %X") % classllvmo__GlobalValue_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::GlobalValue_O::static_className() % llvmo::GlobalValue_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::LandingPadInst_O @ %X") % classllvmo__LandingPadInst_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::LandingPadInst_O::static_className() % llvmo::LandingPadInst_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::PHINode_O @ %X") % classllvmo__PHINode_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::PHINode_O::static_className() % llvmo::PHINode_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::PointerType_O @ %X") % classllvmo__PointerType_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::PointerType_O::static_className() % llvmo::PointerType_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::StoreInst_O @ %X") % classllvmo__StoreInst_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::StoreInst_O::static_className() % llvmo::StoreInst_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::TerminatorInst_O @ %X") % classllvmo__TerminatorInst_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::TerminatorInst_O::static_className() % llvmo::TerminatorInst_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::UnaryInstruction_O @ %X") % classllvmo__UnaryInstruction_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::UnaryInstruction_O::static_className() % llvmo::UnaryInstruction_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::UndefValue_O @ %X") % classllvmo__UndefValue_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::UndefValue_O::static_className() % llvmo::UndefValue_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::VectorType_O @ %X") % classllvmo__VectorType_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::VectorType_O::static_className() % llvmo::VectorType_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::AllocaInst_O @ %X") % classllvmo__AllocaInst_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::AllocaInst_O::static_className() % llvmo::AllocaInst_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::BranchInst_O @ %X") % classllvmo__BranchInst_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::BranchInst_O::static_className() % llvmo::BranchInst_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::ConstantDataArray_O @ %X") % classllvmo__ConstantDataArray_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::ConstantDataArray_O::static_className() % llvmo::ConstantDataArray_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::Function_O @ %X") % classllvmo__Function_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::Function_O::static_className() % llvmo::Function_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::GlobalVariable_O @ %X") % classllvmo__GlobalVariable_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::GlobalVariable_O::static_className() % llvmo::GlobalVariable_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::IndirectBrInst_O @ %X") % classllvmo__IndirectBrInst_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::IndirectBrInst_O::static_className() % llvmo::IndirectBrInst_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::InvokeInst_O @ %X") % classllvmo__InvokeInst_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::InvokeInst_O::static_className() % llvmo::InvokeInst_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::LoadInst_O @ %X") % classllvmo__LoadInst_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::LoadInst_O::static_className() % llvmo::LoadInst_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::ResumeInst_O @ %X") % classllvmo__ResumeInst_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::ResumeInst_O::static_className() % llvmo::ResumeInst_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::ReturnInst_O @ %X") % classllvmo__ReturnInst_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::ReturnInst_O::static_className() % llvmo::ReturnInst_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::SwitchInst_O @ %X") % classllvmo__SwitchInst_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::SwitchInst_O::static_className() % llvmo::SwitchInst_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::UnreachableInst_O @ %X") % classllvmo__UnreachableInst_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::UnreachableInst_O::static_className() % llvmo::UnreachableInst_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::VAArgInst_O @ %X") % classllvmo__VAArgInst_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::VAArgInst_O::static_className() % llvmo::VAArgInst_O::static_classSymbol() );
#endif // } DUMP_INFO_CLASS
#undef DUMP_INFO_CLASS
#if defined(DEFINE_BASE_CLASSES) || defined(ALL_STAGES) // {
// Depends on nothing
classllvmo__APFloat_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::ExternalObject_O::static_classSymbol());
classllvmo__APInt_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::ExternalObject_O::static_classSymbol());
classllvmo__Attribute_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classllvmo__DIBuilder_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::ExternalObject_O::static_classSymbol());
classllvmo__DataLayout_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::ExternalObject_O::static_classSymbol());
classllvmo__DebugInfo_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classllvmo__DebugLoc_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classllvmo__EngineBuilder_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::ExternalObject_O::static_classSymbol());
classllvmo__ExecutionEngine_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::ExternalObject_O::static_classSymbol());
classllvmo__IRBuilderBase_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::ExternalObject_O::static_classSymbol());
classllvmo__InsertPoint_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classllvmo__LLVMContext_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::ExternalObject_O::static_classSymbol());
classllvmo__Linker_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::ExternalObject_O::static_classSymbol());
classllvmo__Module_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::ExternalObject_O::static_classSymbol());
classllvmo__NamedMDNode_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::ExternalObject_O::static_classSymbol());
classllvmo__PassManagerBase_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::ExternalObject_O::static_classSymbol());
classllvmo__PassManagerBuilder_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::ExternalObject_O::static_classSymbol());
classllvmo__Pass_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::ExternalObject_O::static_classSymbol());
classllvmo__Type_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::ExternalObject_O::static_classSymbol());
classllvmo__Value_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::ExternalObject_O::static_classSymbol());
classllvmo__Argument_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Value_O::static_classSymbol());
classllvmo__BasicBlock_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Value_O::static_classSymbol());
classllvmo__CompositeType_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Type_O::static_classSymbol());
classllvmo__DIArray_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::DebugInfo_O::static_classSymbol());
classllvmo__DIBasicType_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::DebugInfo_O::static_classSymbol());
classllvmo__DICompileUnit_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::DebugInfo_O::static_classSymbol());
classllvmo__DICompositeType_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::DebugInfo_O::static_classSymbol());
classllvmo__DIDerivedType_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::DebugInfo_O::static_classSymbol());
classllvmo__DIDescriptor_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::DebugInfo_O::static_classSymbol());
classllvmo__DIFile_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::DebugInfo_O::static_classSymbol());
classllvmo__DILexicalBlock_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::DebugInfo_O::static_classSymbol());
classllvmo__DIScope_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::DebugInfo_O::static_classSymbol());
classllvmo__DISubprogram_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::DebugInfo_O::static_classSymbol());
classllvmo__DIType_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::DebugInfo_O::static_classSymbol());
classllvmo__FunctionPassManager_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::PassManagerBase_O::static_classSymbol());
classllvmo__FunctionPass_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Pass_O::static_classSymbol());
classllvmo__FunctionType_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Type_O::static_classSymbol());
classllvmo__IRBuilder_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::IRBuilderBase_O::static_classSymbol());
classllvmo__IntegerType_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Type_O::static_classSymbol());
classllvmo__MDNode_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Value_O::static_classSymbol());
classllvmo__MDString_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Value_O::static_classSymbol());
classllvmo__ModulePass_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Pass_O::static_classSymbol());
classllvmo__PassManager_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::PassManagerBase_O::static_classSymbol());
classllvmo__User_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Value_O::static_classSymbol());
classllvmo__Constant_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::User_O::static_classSymbol());
classllvmo__ImmutablePass_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::ModulePass_O::static_classSymbol());
classllvmo__Instruction_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::User_O::static_classSymbol());
classllvmo__SequentialType_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::CompositeType_O::static_classSymbol());
classllvmo__StructType_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::CompositeType_O::static_classSymbol());
classllvmo__ArrayType_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::SequentialType_O::static_classSymbol());
classllvmo__AtomicCmpXchgInst_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Instruction_O::static_classSymbol());
classllvmo__AtomicRMWInst_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Instruction_O::static_classSymbol());
classllvmo__BlockAddress_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Constant_O::static_classSymbol());
classllvmo__CallInst_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Instruction_O::static_classSymbol());
classllvmo__ConstantArray_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Constant_O::static_classSymbol());
classllvmo__ConstantDataSequential_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Constant_O::static_classSymbol());
classllvmo__ConstantExpr_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Constant_O::static_classSymbol());
classllvmo__ConstantFP_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Constant_O::static_classSymbol());
classllvmo__ConstantInt_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Constant_O::static_classSymbol());
classllvmo__ConstantPointerNull_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Constant_O::static_classSymbol());
classllvmo__DataLayoutPass_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::ImmutablePass_O::static_classSymbol());
classllvmo__FenceInst_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Instruction_O::static_classSymbol());
classllvmo__GlobalValue_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Constant_O::static_classSymbol());
classllvmo__LandingPadInst_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Instruction_O::static_classSymbol());
classllvmo__PHINode_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Instruction_O::static_classSymbol());
classllvmo__PointerType_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::SequentialType_O::static_classSymbol());
classllvmo__StoreInst_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Instruction_O::static_classSymbol());
classllvmo__TerminatorInst_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Instruction_O::static_classSymbol());
classllvmo__UnaryInstruction_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Instruction_O::static_classSymbol());
classllvmo__UndefValue_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Constant_O::static_classSymbol());
classllvmo__VectorType_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::SequentialType_O::static_classSymbol());
classllvmo__AllocaInst_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::UnaryInstruction_O::static_classSymbol());
classllvmo__BranchInst_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::TerminatorInst_O::static_classSymbol());
classllvmo__ConstantDataArray_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::ConstantDataSequential_O::static_classSymbol());
classllvmo__Function_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::GlobalValue_O::static_classSymbol());
classllvmo__GlobalVariable_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::GlobalValue_O::static_classSymbol());
classllvmo__IndirectBrInst_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::TerminatorInst_O::static_classSymbol());
classllvmo__InvokeInst_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::TerminatorInst_O::static_classSymbol());
classllvmo__LoadInst_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::UnaryInstruction_O::static_classSymbol());
classllvmo__ResumeInst_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::TerminatorInst_O::static_classSymbol());
classllvmo__ReturnInst_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::TerminatorInst_O::static_classSymbol());
classllvmo__SwitchInst_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::TerminatorInst_O::static_classSymbol());
classllvmo__UnreachableInst_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::TerminatorInst_O::static_classSymbol());
classllvmo__VAArgInst_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::UnaryInstruction_O::static_classSymbol());
#endif // } DEFINE_BASE_CLASSES
#undef DEFINE_BASE_CLASSES
#if defined(DEFINE_CLASS_NAMES) || defined(ALL_STAGES) // {
 core::Package_sp _curPkg = _lisp->findPackage(CurrentPkg);
// Depends on nothing

    classllvmo__APFloat_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::APFloat_O::static_classSymbol());

    classllvmo__APInt_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::APInt_O::static_classSymbol());

    classllvmo__Attribute_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::Attribute_O::static_classSymbol());

    classllvmo__DIBuilder_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::DIBuilder_O::static_classSymbol());

    classllvmo__DataLayout_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::DataLayout_O::static_classSymbol());

    classllvmo__DebugInfo_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::DebugInfo_O::static_classSymbol());

    classllvmo__DebugLoc_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::DebugLoc_O::static_classSymbol());

    classllvmo__EngineBuilder_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::EngineBuilder_O::static_classSymbol());

    classllvmo__ExecutionEngine_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::ExecutionEngine_O::static_classSymbol());

    classllvmo__IRBuilderBase_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::IRBuilderBase_O::static_classSymbol());

    classllvmo__InsertPoint_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::InsertPoint_O::static_classSymbol());

    classllvmo__LLVMContext_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::LLVMContext_O::static_classSymbol());

    classllvmo__Linker_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::Linker_O::static_classSymbol());

    classllvmo__Module_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::Module_O::static_classSymbol());

    classllvmo__NamedMDNode_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::NamedMDNode_O::static_classSymbol());

    classllvmo__PassManagerBase_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::PassManagerBase_O::static_classSymbol());

    classllvmo__PassManagerBuilder_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::PassManagerBuilder_O::static_classSymbol());

    classllvmo__Pass_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::Pass_O::static_classSymbol());

    classllvmo__Type_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::Type_O::static_classSymbol());

    classllvmo__Value_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::Value_O::static_classSymbol());

    classllvmo__Argument_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::Argument_O::static_classSymbol());

    classllvmo__BasicBlock_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::BasicBlock_O::static_classSymbol());

    classllvmo__CompositeType_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::CompositeType_O::static_classSymbol());

    classllvmo__DIArray_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::DIArray_O::static_classSymbol());

    classllvmo__DIBasicType_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::DIBasicType_O::static_classSymbol());

    classllvmo__DICompileUnit_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::DICompileUnit_O::static_classSymbol());

    classllvmo__DICompositeType_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::DICompositeType_O::static_classSymbol());

    classllvmo__DIDerivedType_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::DIDerivedType_O::static_classSymbol());

    classllvmo__DIDescriptor_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::DIDescriptor_O::static_classSymbol());

    classllvmo__DIFile_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::DIFile_O::static_classSymbol());

    classllvmo__DILexicalBlock_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::DILexicalBlock_O::static_classSymbol());

    classllvmo__DIScope_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::DIScope_O::static_classSymbol());

    classllvmo__DISubprogram_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::DISubprogram_O::static_classSymbol());

    classllvmo__DIType_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::DIType_O::static_classSymbol());

    classllvmo__FunctionPassManager_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::FunctionPassManager_O::static_classSymbol());

    classllvmo__FunctionPass_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::FunctionPass_O::static_classSymbol());

    classllvmo__FunctionType_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::FunctionType_O::static_classSymbol());

    classllvmo__IRBuilder_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::IRBuilder_O::static_classSymbol());

    classllvmo__IntegerType_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::IntegerType_O::static_classSymbol());

    classllvmo__MDNode_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::MDNode_O::static_classSymbol());

    classllvmo__MDString_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::MDString_O::static_classSymbol());

    classllvmo__ModulePass_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::ModulePass_O::static_classSymbol());

    classllvmo__PassManager_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::PassManager_O::static_classSymbol());

    classllvmo__User_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::User_O::static_classSymbol());

    classllvmo__Constant_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::Constant_O::static_classSymbol());

    classllvmo__ImmutablePass_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::ImmutablePass_O::static_classSymbol());

    classllvmo__Instruction_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::Instruction_O::static_classSymbol());

    classllvmo__SequentialType_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::SequentialType_O::static_classSymbol());

    classllvmo__StructType_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::StructType_O::static_classSymbol());

    classllvmo__ArrayType_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::ArrayType_O::static_classSymbol());

    classllvmo__AtomicCmpXchgInst_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::AtomicCmpXchgInst_O::static_classSymbol());

    classllvmo__AtomicRMWInst_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::AtomicRMWInst_O::static_classSymbol());

    classllvmo__BlockAddress_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::BlockAddress_O::static_classSymbol());

    classllvmo__CallInst_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::CallInst_O::static_classSymbol());

    classllvmo__ConstantArray_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::ConstantArray_O::static_classSymbol());

    classllvmo__ConstantDataSequential_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::ConstantDataSequential_O::static_classSymbol());

    classllvmo__ConstantExpr_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::ConstantExpr_O::static_classSymbol());

    classllvmo__ConstantFP_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::ConstantFP_O::static_classSymbol());

    classllvmo__ConstantInt_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::ConstantInt_O::static_classSymbol());

    classllvmo__ConstantPointerNull_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::ConstantPointerNull_O::static_classSymbol());

    classllvmo__DataLayoutPass_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::DataLayoutPass_O::static_classSymbol());

    classllvmo__FenceInst_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::FenceInst_O::static_classSymbol());

    classllvmo__GlobalValue_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::GlobalValue_O::static_classSymbol());

    classllvmo__LandingPadInst_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::LandingPadInst_O::static_classSymbol());

    classllvmo__PHINode_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::PHINode_O::static_classSymbol());

    classllvmo__PointerType_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::PointerType_O::static_classSymbol());

    classllvmo__StoreInst_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::StoreInst_O::static_classSymbol());

    classllvmo__TerminatorInst_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::TerminatorInst_O::static_classSymbol());

    classllvmo__UnaryInstruction_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::UnaryInstruction_O::static_classSymbol());

    classllvmo__UndefValue_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::UndefValue_O::static_classSymbol());

    classllvmo__VectorType_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::VectorType_O::static_classSymbol());

    classllvmo__AllocaInst_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::AllocaInst_O::static_classSymbol());

    classllvmo__BranchInst_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::BranchInst_O::static_classSymbol());

    classllvmo__ConstantDataArray_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::ConstantDataArray_O::static_classSymbol());

    classllvmo__Function_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::Function_O::static_classSymbol());

    classllvmo__GlobalVariable_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::GlobalVariable_O::static_classSymbol());

    classllvmo__IndirectBrInst_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::IndirectBrInst_O::static_classSymbol());

    classllvmo__InvokeInst_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::InvokeInst_O::static_classSymbol());

    classllvmo__LoadInst_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::LoadInst_O::static_classSymbol());

    classllvmo__ResumeInst_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::ResumeInst_O::static_classSymbol());

    classllvmo__ReturnInst_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::ReturnInst_O::static_classSymbol());

    classllvmo__SwitchInst_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::SwitchInst_O::static_classSymbol());

    classllvmo__UnreachableInst_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::UnreachableInst_O::static_classSymbol());

    classllvmo__VAArgInst_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::VAArgInst_O::static_classSymbol());
#endif // } DEFINE_CLASS_NAMES
#undef DEFINE_CLASS_NAMES
#if defined(EXPOSE_TO_CANDO) || defined(ALL_STAGES)
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__APFloat_O(core::Lisp_sp); // base(s): set(['core::ExternalObject_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__APFloat_O");
    llvmo::Register_llvmo__APFloat_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__APInt_O(core::Lisp_sp); // base(s): set(['core::ExternalObject_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__APInt_O");
    llvmo::Register_llvmo__APInt_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__Attribute_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__Attribute_O");
    llvmo::Register_llvmo__Attribute_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__DIBuilder_O(core::Lisp_sp); // base(s): set(['core::ExternalObject_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__DIBuilder_O");
    llvmo::Register_llvmo__DIBuilder_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__DataLayout_O(core::Lisp_sp); // base(s): set(['core::ExternalObject_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__DataLayout_O");
    llvmo::Register_llvmo__DataLayout_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__DebugInfo_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__DebugInfo_O");
    llvmo::Register_llvmo__DebugInfo_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__DebugLoc_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__DebugLoc_O");
    llvmo::Register_llvmo__DebugLoc_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__EngineBuilder_O(core::Lisp_sp); // base(s): set(['core::ExternalObject_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__EngineBuilder_O");
    llvmo::Register_llvmo__EngineBuilder_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__ExecutionEngine_O(core::Lisp_sp); // base(s): set(['core::ExternalObject_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__ExecutionEngine_O");
    llvmo::Register_llvmo__ExecutionEngine_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__IRBuilderBase_O(core::Lisp_sp); // base(s): set(['core::ExternalObject_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__IRBuilderBase_O");
    llvmo::Register_llvmo__IRBuilderBase_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__InsertPoint_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__InsertPoint_O");
    llvmo::Register_llvmo__InsertPoint_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__LLVMContext_O(core::Lisp_sp); // base(s): set(['core::ExternalObject_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__LLVMContext_O");
    llvmo::Register_llvmo__LLVMContext_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__Linker_O(core::Lisp_sp); // base(s): set(['core::ExternalObject_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__Linker_O");
    llvmo::Register_llvmo__Linker_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__Module_O(core::Lisp_sp); // base(s): set(['core::ExternalObject_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__Module_O");
    llvmo::Register_llvmo__Module_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__NamedMDNode_O(core::Lisp_sp); // base(s): set(['core::ExternalObject_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__NamedMDNode_O");
    llvmo::Register_llvmo__NamedMDNode_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__PassManagerBase_O(core::Lisp_sp); // base(s): set(['core::ExternalObject_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__PassManagerBase_O");
    llvmo::Register_llvmo__PassManagerBase_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__PassManagerBuilder_O(core::Lisp_sp); // base(s): set(['core::ExternalObject_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__PassManagerBuilder_O");
    llvmo::Register_llvmo__PassManagerBuilder_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__Pass_O(core::Lisp_sp); // base(s): set(['core::ExternalObject_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__Pass_O");
    llvmo::Register_llvmo__Pass_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__Type_O(core::Lisp_sp); // base(s): set(['core::ExternalObject_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__Type_O");
    llvmo::Register_llvmo__Type_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__Value_O(core::Lisp_sp); // base(s): set(['core::ExternalObject_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__Value_O");
    llvmo::Register_llvmo__Value_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__Argument_O(core::Lisp_sp); // base(s): set(['llvmo::Value_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__Argument_O");
    llvmo::Register_llvmo__Argument_O(_lisp); // base(s): set(['llvmo::Value_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__BasicBlock_O(core::Lisp_sp); // base(s): set(['llvmo::Value_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__BasicBlock_O");
    llvmo::Register_llvmo__BasicBlock_O(_lisp); // base(s): set(['llvmo::Value_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__CompositeType_O(core::Lisp_sp); // base(s): set(['llvmo::Type_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__CompositeType_O");
    llvmo::Register_llvmo__CompositeType_O(_lisp); // base(s): set(['llvmo::Type_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__DIArray_O(core::Lisp_sp); // base(s): set(['llvmo::DebugInfo_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__DIArray_O");
    llvmo::Register_llvmo__DIArray_O(_lisp); // base(s): set(['llvmo::DebugInfo_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__DIBasicType_O(core::Lisp_sp); // base(s): set(['llvmo::DebugInfo_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__DIBasicType_O");
    llvmo::Register_llvmo__DIBasicType_O(_lisp); // base(s): set(['llvmo::DebugInfo_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__DICompileUnit_O(core::Lisp_sp); // base(s): set(['llvmo::DebugInfo_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__DICompileUnit_O");
    llvmo::Register_llvmo__DICompileUnit_O(_lisp); // base(s): set(['llvmo::DebugInfo_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__DICompositeType_O(core::Lisp_sp); // base(s): set(['llvmo::DebugInfo_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__DICompositeType_O");
    llvmo::Register_llvmo__DICompositeType_O(_lisp); // base(s): set(['llvmo::DebugInfo_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__DIDerivedType_O(core::Lisp_sp); // base(s): set(['llvmo::DebugInfo_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__DIDerivedType_O");
    llvmo::Register_llvmo__DIDerivedType_O(_lisp); // base(s): set(['llvmo::DebugInfo_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__DIDescriptor_O(core::Lisp_sp); // base(s): set(['llvmo::DebugInfo_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__DIDescriptor_O");
    llvmo::Register_llvmo__DIDescriptor_O(_lisp); // base(s): set(['llvmo::DebugInfo_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__DIFile_O(core::Lisp_sp); // base(s): set(['llvmo::DebugInfo_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__DIFile_O");
    llvmo::Register_llvmo__DIFile_O(_lisp); // base(s): set(['llvmo::DebugInfo_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__DILexicalBlock_O(core::Lisp_sp); // base(s): set(['llvmo::DebugInfo_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__DILexicalBlock_O");
    llvmo::Register_llvmo__DILexicalBlock_O(_lisp); // base(s): set(['llvmo::DebugInfo_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__DIScope_O(core::Lisp_sp); // base(s): set(['llvmo::DebugInfo_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__DIScope_O");
    llvmo::Register_llvmo__DIScope_O(_lisp); // base(s): set(['llvmo::DebugInfo_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__DISubprogram_O(core::Lisp_sp); // base(s): set(['llvmo::DebugInfo_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__DISubprogram_O");
    llvmo::Register_llvmo__DISubprogram_O(_lisp); // base(s): set(['llvmo::DebugInfo_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__DIType_O(core::Lisp_sp); // base(s): set(['llvmo::DebugInfo_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__DIType_O");
    llvmo::Register_llvmo__DIType_O(_lisp); // base(s): set(['llvmo::DebugInfo_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__FunctionPassManager_O(core::Lisp_sp); // base(s): set(['llvmo::PassManagerBase_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__FunctionPassManager_O");
    llvmo::Register_llvmo__FunctionPassManager_O(_lisp); // base(s): set(['llvmo::PassManagerBase_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__FunctionPass_O(core::Lisp_sp); // base(s): set(['llvmo::Pass_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__FunctionPass_O");
    llvmo::Register_llvmo__FunctionPass_O(_lisp); // base(s): set(['llvmo::Pass_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__FunctionType_O(core::Lisp_sp); // base(s): set(['llvmo::Type_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__FunctionType_O");
    llvmo::Register_llvmo__FunctionType_O(_lisp); // base(s): set(['llvmo::Type_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__IRBuilder_O(core::Lisp_sp); // base(s): set(['llvmo::IRBuilderBase_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__IRBuilder_O");
    llvmo::Register_llvmo__IRBuilder_O(_lisp); // base(s): set(['llvmo::IRBuilderBase_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__IntegerType_O(core::Lisp_sp); // base(s): set(['llvmo::Type_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__IntegerType_O");
    llvmo::Register_llvmo__IntegerType_O(_lisp); // base(s): set(['llvmo::Type_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__MDNode_O(core::Lisp_sp); // base(s): set(['llvmo::Value_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__MDNode_O");
    llvmo::Register_llvmo__MDNode_O(_lisp); // base(s): set(['llvmo::Value_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__MDString_O(core::Lisp_sp); // base(s): set(['llvmo::Value_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__MDString_O");
    llvmo::Register_llvmo__MDString_O(_lisp); // base(s): set(['llvmo::Value_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__ModulePass_O(core::Lisp_sp); // base(s): set(['llvmo::Pass_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__ModulePass_O");
    llvmo::Register_llvmo__ModulePass_O(_lisp); // base(s): set(['llvmo::Pass_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__PassManager_O(core::Lisp_sp); // base(s): set(['llvmo::PassManagerBase_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__PassManager_O");
    llvmo::Register_llvmo__PassManager_O(_lisp); // base(s): set(['llvmo::PassManagerBase_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__User_O(core::Lisp_sp); // base(s): set(['llvmo::Value_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__User_O");
    llvmo::Register_llvmo__User_O(_lisp); // base(s): set(['llvmo::Value_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__Constant_O(core::Lisp_sp); // base(s): set(['llvmo::User_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__Constant_O");
    llvmo::Register_llvmo__Constant_O(_lisp); // base(s): set(['llvmo::User_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__ImmutablePass_O(core::Lisp_sp); // base(s): set(['llvmo::ModulePass_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__ImmutablePass_O");
    llvmo::Register_llvmo__ImmutablePass_O(_lisp); // base(s): set(['llvmo::ModulePass_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__Instruction_O(core::Lisp_sp); // base(s): set(['llvmo::User_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__Instruction_O");
    llvmo::Register_llvmo__Instruction_O(_lisp); // base(s): set(['llvmo::User_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__SequentialType_O(core::Lisp_sp); // base(s): set(['llvmo::CompositeType_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__SequentialType_O");
    llvmo::Register_llvmo__SequentialType_O(_lisp); // base(s): set(['llvmo::CompositeType_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__StructType_O(core::Lisp_sp); // base(s): set(['llvmo::CompositeType_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__StructType_O");
    llvmo::Register_llvmo__StructType_O(_lisp); // base(s): set(['llvmo::CompositeType_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__ArrayType_O(core::Lisp_sp); // base(s): set(['llvmo::SequentialType_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__ArrayType_O");
    llvmo::Register_llvmo__ArrayType_O(_lisp); // base(s): set(['llvmo::SequentialType_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__AtomicCmpXchgInst_O(core::Lisp_sp); // base(s): set(['llvmo::Instruction_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__AtomicCmpXchgInst_O");
    llvmo::Register_llvmo__AtomicCmpXchgInst_O(_lisp); // base(s): set(['llvmo::Instruction_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__AtomicRMWInst_O(core::Lisp_sp); // base(s): set(['llvmo::Instruction_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__AtomicRMWInst_O");
    llvmo::Register_llvmo__AtomicRMWInst_O(_lisp); // base(s): set(['llvmo::Instruction_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__BlockAddress_O(core::Lisp_sp); // base(s): set(['llvmo::Constant_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__BlockAddress_O");
    llvmo::Register_llvmo__BlockAddress_O(_lisp); // base(s): set(['llvmo::Constant_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__CallInst_O(core::Lisp_sp); // base(s): set(['llvmo::Instruction_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__CallInst_O");
    llvmo::Register_llvmo__CallInst_O(_lisp); // base(s): set(['llvmo::Instruction_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__ConstantArray_O(core::Lisp_sp); // base(s): set(['llvmo::Constant_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__ConstantArray_O");
    llvmo::Register_llvmo__ConstantArray_O(_lisp); // base(s): set(['llvmo::Constant_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__ConstantDataSequential_O(core::Lisp_sp); // base(s): set(['llvmo::Constant_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__ConstantDataSequential_O");
    llvmo::Register_llvmo__ConstantDataSequential_O(_lisp); // base(s): set(['llvmo::Constant_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__ConstantExpr_O(core::Lisp_sp); // base(s): set(['llvmo::Constant_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__ConstantExpr_O");
    llvmo::Register_llvmo__ConstantExpr_O(_lisp); // base(s): set(['llvmo::Constant_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__ConstantFP_O(core::Lisp_sp); // base(s): set(['llvmo::Constant_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__ConstantFP_O");
    llvmo::Register_llvmo__ConstantFP_O(_lisp); // base(s): set(['llvmo::Constant_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__ConstantInt_O(core::Lisp_sp); // base(s): set(['llvmo::Constant_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__ConstantInt_O");
    llvmo::Register_llvmo__ConstantInt_O(_lisp); // base(s): set(['llvmo::Constant_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__ConstantPointerNull_O(core::Lisp_sp); // base(s): set(['llvmo::Constant_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__ConstantPointerNull_O");
    llvmo::Register_llvmo__ConstantPointerNull_O(_lisp); // base(s): set(['llvmo::Constant_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__DataLayoutPass_O(core::Lisp_sp); // base(s): set(['llvmo::ImmutablePass_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__DataLayoutPass_O");
    llvmo::Register_llvmo__DataLayoutPass_O(_lisp); // base(s): set(['llvmo::ImmutablePass_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__FenceInst_O(core::Lisp_sp); // base(s): set(['llvmo::Instruction_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__FenceInst_O");
    llvmo::Register_llvmo__FenceInst_O(_lisp); // base(s): set(['llvmo::Instruction_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__GlobalValue_O(core::Lisp_sp); // base(s): set(['llvmo::Constant_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__GlobalValue_O");
    llvmo::Register_llvmo__GlobalValue_O(_lisp); // base(s): set(['llvmo::Constant_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__LandingPadInst_O(core::Lisp_sp); // base(s): set(['llvmo::Instruction_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__LandingPadInst_O");
    llvmo::Register_llvmo__LandingPadInst_O(_lisp); // base(s): set(['llvmo::Instruction_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__PHINode_O(core::Lisp_sp); // base(s): set(['llvmo::Instruction_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__PHINode_O");
    llvmo::Register_llvmo__PHINode_O(_lisp); // base(s): set(['llvmo::Instruction_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__PointerType_O(core::Lisp_sp); // base(s): set(['llvmo::SequentialType_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__PointerType_O");
    llvmo::Register_llvmo__PointerType_O(_lisp); // base(s): set(['llvmo::SequentialType_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__StoreInst_O(core::Lisp_sp); // base(s): set(['llvmo::Instruction_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__StoreInst_O");
    llvmo::Register_llvmo__StoreInst_O(_lisp); // base(s): set(['llvmo::Instruction_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__TerminatorInst_O(core::Lisp_sp); // base(s): set(['llvmo::Instruction_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__TerminatorInst_O");
    llvmo::Register_llvmo__TerminatorInst_O(_lisp); // base(s): set(['llvmo::Instruction_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__UnaryInstruction_O(core::Lisp_sp); // base(s): set(['llvmo::Instruction_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__UnaryInstruction_O");
    llvmo::Register_llvmo__UnaryInstruction_O(_lisp); // base(s): set(['llvmo::Instruction_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__UndefValue_O(core::Lisp_sp); // base(s): set(['llvmo::Constant_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__UndefValue_O");
    llvmo::Register_llvmo__UndefValue_O(_lisp); // base(s): set(['llvmo::Constant_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__VectorType_O(core::Lisp_sp); // base(s): set(['llvmo::SequentialType_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__VectorType_O");
    llvmo::Register_llvmo__VectorType_O(_lisp); // base(s): set(['llvmo::SequentialType_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__AllocaInst_O(core::Lisp_sp); // base(s): set(['llvmo::UnaryInstruction_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__AllocaInst_O");
    llvmo::Register_llvmo__AllocaInst_O(_lisp); // base(s): set(['llvmo::UnaryInstruction_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__BranchInst_O(core::Lisp_sp); // base(s): set(['llvmo::TerminatorInst_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__BranchInst_O");
    llvmo::Register_llvmo__BranchInst_O(_lisp); // base(s): set(['llvmo::TerminatorInst_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__ConstantDataArray_O(core::Lisp_sp); // base(s): set(['llvmo::ConstantDataSequential_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__ConstantDataArray_O");
    llvmo::Register_llvmo__ConstantDataArray_O(_lisp); // base(s): set(['llvmo::ConstantDataSequential_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__Function_O(core::Lisp_sp); // base(s): set(['llvmo::GlobalValue_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__Function_O");
    llvmo::Register_llvmo__Function_O(_lisp); // base(s): set(['llvmo::GlobalValue_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__GlobalVariable_O(core::Lisp_sp); // base(s): set(['llvmo::GlobalValue_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__GlobalVariable_O");
    llvmo::Register_llvmo__GlobalVariable_O(_lisp); // base(s): set(['llvmo::GlobalValue_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__IndirectBrInst_O(core::Lisp_sp); // base(s): set(['llvmo::TerminatorInst_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__IndirectBrInst_O");
    llvmo::Register_llvmo__IndirectBrInst_O(_lisp); // base(s): set(['llvmo::TerminatorInst_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__InvokeInst_O(core::Lisp_sp); // base(s): set(['llvmo::TerminatorInst_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__InvokeInst_O");
    llvmo::Register_llvmo__InvokeInst_O(_lisp); // base(s): set(['llvmo::TerminatorInst_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__LoadInst_O(core::Lisp_sp); // base(s): set(['llvmo::UnaryInstruction_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__LoadInst_O");
    llvmo::Register_llvmo__LoadInst_O(_lisp); // base(s): set(['llvmo::UnaryInstruction_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__ResumeInst_O(core::Lisp_sp); // base(s): set(['llvmo::TerminatorInst_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__ResumeInst_O");
    llvmo::Register_llvmo__ResumeInst_O(_lisp); // base(s): set(['llvmo::TerminatorInst_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__ReturnInst_O(core::Lisp_sp); // base(s): set(['llvmo::TerminatorInst_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__ReturnInst_O");
    llvmo::Register_llvmo__ReturnInst_O(_lisp); // base(s): set(['llvmo::TerminatorInst_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__SwitchInst_O(core::Lisp_sp); // base(s): set(['llvmo::TerminatorInst_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__SwitchInst_O");
    llvmo::Register_llvmo__SwitchInst_O(_lisp); // base(s): set(['llvmo::TerminatorInst_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__UnreachableInst_O(core::Lisp_sp); // base(s): set(['llvmo::TerminatorInst_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__UnreachableInst_O");
    llvmo::Register_llvmo__UnreachableInst_O(_lisp); // base(s): set(['llvmo::TerminatorInst_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__VAArgInst_O(core::Lisp_sp); // base(s): set(['llvmo::UnaryInstruction_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__VAArgInst_O");
    llvmo::Register_llvmo__VAArgInst_O(_lisp); // base(s): set(['llvmo::UnaryInstruction_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#endif // EXPOSE_TO_CANDO
#undef EXPOSE_TO_CANDO
#ifdef EXPOSE_TO_PYTHON
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__APFloat_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__APFloat_O");
	Call_exposePython_llvmo__APFloat_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__APInt_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__APInt_O");
	Call_exposePython_llvmo__APInt_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__Attribute_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__Attribute_O");
	Call_exposePython_llvmo__Attribute_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__DIBuilder_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__DIBuilder_O");
	Call_exposePython_llvmo__DIBuilder_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__DataLayout_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__DataLayout_O");
	Call_exposePython_llvmo__DataLayout_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__DebugInfo_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__DebugInfo_O");
	Call_exposePython_llvmo__DebugInfo_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__DebugLoc_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__DebugLoc_O");
	Call_exposePython_llvmo__DebugLoc_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__EngineBuilder_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__EngineBuilder_O");
	Call_exposePython_llvmo__EngineBuilder_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__ExecutionEngine_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__ExecutionEngine_O");
	Call_exposePython_llvmo__ExecutionEngine_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__IRBuilderBase_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__IRBuilderBase_O");
	Call_exposePython_llvmo__IRBuilderBase_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__InsertPoint_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__InsertPoint_O");
	Call_exposePython_llvmo__InsertPoint_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__LLVMContext_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__LLVMContext_O");
	Call_exposePython_llvmo__LLVMContext_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__Linker_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__Linker_O");
	Call_exposePython_llvmo__Linker_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__Module_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__Module_O");
	Call_exposePython_llvmo__Module_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__NamedMDNode_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__NamedMDNode_O");
	Call_exposePython_llvmo__NamedMDNode_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__PassManagerBase_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__PassManagerBase_O");
	Call_exposePython_llvmo__PassManagerBase_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__PassManagerBuilder_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__PassManagerBuilder_O");
	Call_exposePython_llvmo__PassManagerBuilder_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__Pass_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__Pass_O");
	Call_exposePython_llvmo__Pass_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__Type_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__Type_O");
	Call_exposePython_llvmo__Type_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__Value_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__Value_O");
	Call_exposePython_llvmo__Value_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__Argument_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__Argument_O");
	Call_exposePython_llvmo__Argument_O(_lisp); // base(s): set(['llvmo::Value_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__BasicBlock_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__BasicBlock_O");
	Call_exposePython_llvmo__BasicBlock_O(_lisp); // base(s): set(['llvmo::Value_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__CompositeType_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__CompositeType_O");
	Call_exposePython_llvmo__CompositeType_O(_lisp); // base(s): set(['llvmo::Type_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__DIArray_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__DIArray_O");
	Call_exposePython_llvmo__DIArray_O(_lisp); // base(s): set(['llvmo::DebugInfo_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__DIBasicType_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__DIBasicType_O");
	Call_exposePython_llvmo__DIBasicType_O(_lisp); // base(s): set(['llvmo::DebugInfo_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__DICompileUnit_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__DICompileUnit_O");
	Call_exposePython_llvmo__DICompileUnit_O(_lisp); // base(s): set(['llvmo::DebugInfo_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__DICompositeType_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__DICompositeType_O");
	Call_exposePython_llvmo__DICompositeType_O(_lisp); // base(s): set(['llvmo::DebugInfo_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__DIDerivedType_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__DIDerivedType_O");
	Call_exposePython_llvmo__DIDerivedType_O(_lisp); // base(s): set(['llvmo::DebugInfo_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__DIDescriptor_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__DIDescriptor_O");
	Call_exposePython_llvmo__DIDescriptor_O(_lisp); // base(s): set(['llvmo::DebugInfo_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__DIFile_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__DIFile_O");
	Call_exposePython_llvmo__DIFile_O(_lisp); // base(s): set(['llvmo::DebugInfo_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__DILexicalBlock_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__DILexicalBlock_O");
	Call_exposePython_llvmo__DILexicalBlock_O(_lisp); // base(s): set(['llvmo::DebugInfo_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__DIScope_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__DIScope_O");
	Call_exposePython_llvmo__DIScope_O(_lisp); // base(s): set(['llvmo::DebugInfo_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__DISubprogram_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__DISubprogram_O");
	Call_exposePython_llvmo__DISubprogram_O(_lisp); // base(s): set(['llvmo::DebugInfo_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__DIType_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__DIType_O");
	Call_exposePython_llvmo__DIType_O(_lisp); // base(s): set(['llvmo::DebugInfo_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__FunctionPassManager_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__FunctionPassManager_O");
	Call_exposePython_llvmo__FunctionPassManager_O(_lisp); // base(s): set(['llvmo::PassManagerBase_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__FunctionPass_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__FunctionPass_O");
	Call_exposePython_llvmo__FunctionPass_O(_lisp); // base(s): set(['llvmo::Pass_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__FunctionType_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__FunctionType_O");
	Call_exposePython_llvmo__FunctionType_O(_lisp); // base(s): set(['llvmo::Type_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__IRBuilder_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__IRBuilder_O");
	Call_exposePython_llvmo__IRBuilder_O(_lisp); // base(s): set(['llvmo::IRBuilderBase_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__IntegerType_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__IntegerType_O");
	Call_exposePython_llvmo__IntegerType_O(_lisp); // base(s): set(['llvmo::Type_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__MDNode_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__MDNode_O");
	Call_exposePython_llvmo__MDNode_O(_lisp); // base(s): set(['llvmo::Value_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__MDString_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__MDString_O");
	Call_exposePython_llvmo__MDString_O(_lisp); // base(s): set(['llvmo::Value_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__ModulePass_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__ModulePass_O");
	Call_exposePython_llvmo__ModulePass_O(_lisp); // base(s): set(['llvmo::Pass_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__PassManager_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__PassManager_O");
	Call_exposePython_llvmo__PassManager_O(_lisp); // base(s): set(['llvmo::PassManagerBase_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__User_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__User_O");
	Call_exposePython_llvmo__User_O(_lisp); // base(s): set(['llvmo::Value_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__Constant_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__Constant_O");
	Call_exposePython_llvmo__Constant_O(_lisp); // base(s): set(['llvmo::User_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__ImmutablePass_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__ImmutablePass_O");
	Call_exposePython_llvmo__ImmutablePass_O(_lisp); // base(s): set(['llvmo::ModulePass_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__Instruction_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__Instruction_O");
	Call_exposePython_llvmo__Instruction_O(_lisp); // base(s): set(['llvmo::User_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__SequentialType_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__SequentialType_O");
	Call_exposePython_llvmo__SequentialType_O(_lisp); // base(s): set(['llvmo::CompositeType_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__StructType_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__StructType_O");
	Call_exposePython_llvmo__StructType_O(_lisp); // base(s): set(['llvmo::CompositeType_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__ArrayType_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__ArrayType_O");
	Call_exposePython_llvmo__ArrayType_O(_lisp); // base(s): set(['llvmo::SequentialType_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__AtomicCmpXchgInst_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__AtomicCmpXchgInst_O");
	Call_exposePython_llvmo__AtomicCmpXchgInst_O(_lisp); // base(s): set(['llvmo::Instruction_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__AtomicRMWInst_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__AtomicRMWInst_O");
	Call_exposePython_llvmo__AtomicRMWInst_O(_lisp); // base(s): set(['llvmo::Instruction_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__BlockAddress_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__BlockAddress_O");
	Call_exposePython_llvmo__BlockAddress_O(_lisp); // base(s): set(['llvmo::Constant_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__CallInst_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__CallInst_O");
	Call_exposePython_llvmo__CallInst_O(_lisp); // base(s): set(['llvmo::Instruction_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__ConstantArray_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__ConstantArray_O");
	Call_exposePython_llvmo__ConstantArray_O(_lisp); // base(s): set(['llvmo::Constant_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__ConstantDataSequential_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__ConstantDataSequential_O");
	Call_exposePython_llvmo__ConstantDataSequential_O(_lisp); // base(s): set(['llvmo::Constant_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__ConstantExpr_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__ConstantExpr_O");
	Call_exposePython_llvmo__ConstantExpr_O(_lisp); // base(s): set(['llvmo::Constant_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__ConstantFP_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__ConstantFP_O");
	Call_exposePython_llvmo__ConstantFP_O(_lisp); // base(s): set(['llvmo::Constant_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__ConstantInt_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__ConstantInt_O");
	Call_exposePython_llvmo__ConstantInt_O(_lisp); // base(s): set(['llvmo::Constant_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__ConstantPointerNull_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__ConstantPointerNull_O");
	Call_exposePython_llvmo__ConstantPointerNull_O(_lisp); // base(s): set(['llvmo::Constant_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__DataLayoutPass_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__DataLayoutPass_O");
	Call_exposePython_llvmo__DataLayoutPass_O(_lisp); // base(s): set(['llvmo::ImmutablePass_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__FenceInst_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__FenceInst_O");
	Call_exposePython_llvmo__FenceInst_O(_lisp); // base(s): set(['llvmo::Instruction_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__GlobalValue_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__GlobalValue_O");
	Call_exposePython_llvmo__GlobalValue_O(_lisp); // base(s): set(['llvmo::Constant_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__LandingPadInst_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__LandingPadInst_O");
	Call_exposePython_llvmo__LandingPadInst_O(_lisp); // base(s): set(['llvmo::Instruction_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__PHINode_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__PHINode_O");
	Call_exposePython_llvmo__PHINode_O(_lisp); // base(s): set(['llvmo::Instruction_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__PointerType_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__PointerType_O");
	Call_exposePython_llvmo__PointerType_O(_lisp); // base(s): set(['llvmo::SequentialType_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__StoreInst_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__StoreInst_O");
	Call_exposePython_llvmo__StoreInst_O(_lisp); // base(s): set(['llvmo::Instruction_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__TerminatorInst_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__TerminatorInst_O");
	Call_exposePython_llvmo__TerminatorInst_O(_lisp); // base(s): set(['llvmo::Instruction_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__UnaryInstruction_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__UnaryInstruction_O");
	Call_exposePython_llvmo__UnaryInstruction_O(_lisp); // base(s): set(['llvmo::Instruction_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__UndefValue_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__UndefValue_O");
	Call_exposePython_llvmo__UndefValue_O(_lisp); // base(s): set(['llvmo::Constant_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__VectorType_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__VectorType_O");
	Call_exposePython_llvmo__VectorType_O(_lisp); // base(s): set(['llvmo::SequentialType_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__AllocaInst_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__AllocaInst_O");
	Call_exposePython_llvmo__AllocaInst_O(_lisp); // base(s): set(['llvmo::UnaryInstruction_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__BranchInst_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__BranchInst_O");
	Call_exposePython_llvmo__BranchInst_O(_lisp); // base(s): set(['llvmo::TerminatorInst_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__ConstantDataArray_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__ConstantDataArray_O");
	Call_exposePython_llvmo__ConstantDataArray_O(_lisp); // base(s): set(['llvmo::ConstantDataSequential_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__Function_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__Function_O");
	Call_exposePython_llvmo__Function_O(_lisp); // base(s): set(['llvmo::GlobalValue_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__GlobalVariable_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__GlobalVariable_O");
	Call_exposePython_llvmo__GlobalVariable_O(_lisp); // base(s): set(['llvmo::GlobalValue_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__IndirectBrInst_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__IndirectBrInst_O");
	Call_exposePython_llvmo__IndirectBrInst_O(_lisp); // base(s): set(['llvmo::TerminatorInst_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__InvokeInst_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__InvokeInst_O");
	Call_exposePython_llvmo__InvokeInst_O(_lisp); // base(s): set(['llvmo::TerminatorInst_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__LoadInst_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__LoadInst_O");
	Call_exposePython_llvmo__LoadInst_O(_lisp); // base(s): set(['llvmo::UnaryInstruction_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__ResumeInst_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__ResumeInst_O");
	Call_exposePython_llvmo__ResumeInst_O(_lisp); // base(s): set(['llvmo::TerminatorInst_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__ReturnInst_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__ReturnInst_O");
	Call_exposePython_llvmo__ReturnInst_O(_lisp); // base(s): set(['llvmo::TerminatorInst_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__SwitchInst_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__SwitchInst_O");
	Call_exposePython_llvmo__SwitchInst_O(_lisp); // base(s): set(['llvmo::TerminatorInst_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__UnreachableInst_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__UnreachableInst_O");
	Call_exposePython_llvmo__UnreachableInst_O(_lisp); // base(s): set(['llvmo::TerminatorInst_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__VAArgInst_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__VAArgInst_O");
	Call_exposePython_llvmo__VAArgInst_O(_lisp); // base(s): set(['llvmo::UnaryInstruction_O'])
}
#endif // ifdef Use_LlvmoPkg
#endif // EXPOSE_TO_PYTHON
#undef EXPOSE_TO_PYTHON
#if defined(EXPAND_CLASS_MACROS)
_CLASS_MACRO(llvmo::APFloat_O)
_CLASS_MACRO(llvmo::APInt_O)
_CLASS_MACRO(llvmo::Attribute_O)
_CLASS_MACRO(llvmo::DIBuilder_O)
_CLASS_MACRO(llvmo::DataLayout_O)
_CLASS_MACRO(llvmo::DebugInfo_O)
_CLASS_MACRO(llvmo::DebugLoc_O)
_CLASS_MACRO(llvmo::EngineBuilder_O)
_CLASS_MACRO(llvmo::ExecutionEngine_O)
_CLASS_MACRO(llvmo::IRBuilderBase_O)
_CLASS_MACRO(llvmo::InsertPoint_O)
_CLASS_MACRO(llvmo::LLVMContext_O)
_CLASS_MACRO(llvmo::Linker_O)
_CLASS_MACRO(llvmo::Module_O)
_CLASS_MACRO(llvmo::NamedMDNode_O)
_CLASS_MACRO(llvmo::PassManagerBase_O)
_CLASS_MACRO(llvmo::PassManagerBuilder_O)
_CLASS_MACRO(llvmo::Pass_O)
_CLASS_MACRO(llvmo::Type_O)
_CLASS_MACRO(llvmo::Value_O)
_CLASS_MACRO(llvmo::Argument_O)
_CLASS_MACRO(llvmo::BasicBlock_O)
_CLASS_MACRO(llvmo::CompositeType_O)
_CLASS_MACRO(llvmo::DIArray_O)
_CLASS_MACRO(llvmo::DIBasicType_O)
_CLASS_MACRO(llvmo::DICompileUnit_O)
_CLASS_MACRO(llvmo::DICompositeType_O)
_CLASS_MACRO(llvmo::DIDerivedType_O)
_CLASS_MACRO(llvmo::DIDescriptor_O)
_CLASS_MACRO(llvmo::DIFile_O)
_CLASS_MACRO(llvmo::DILexicalBlock_O)
_CLASS_MACRO(llvmo::DIScope_O)
_CLASS_MACRO(llvmo::DISubprogram_O)
_CLASS_MACRO(llvmo::DIType_O)
_CLASS_MACRO(llvmo::FunctionPassManager_O)
_CLASS_MACRO(llvmo::FunctionPass_O)
_CLASS_MACRO(llvmo::FunctionType_O)
_CLASS_MACRO(llvmo::IRBuilder_O)
_CLASS_MACRO(llvmo::IntegerType_O)
_CLASS_MACRO(llvmo::MDNode_O)
_CLASS_MACRO(llvmo::MDString_O)
_CLASS_MACRO(llvmo::ModulePass_O)
_CLASS_MACRO(llvmo::PassManager_O)
_CLASS_MACRO(llvmo::User_O)
_CLASS_MACRO(llvmo::Constant_O)
_CLASS_MACRO(llvmo::ImmutablePass_O)
_CLASS_MACRO(llvmo::Instruction_O)
_CLASS_MACRO(llvmo::SequentialType_O)
_CLASS_MACRO(llvmo::StructType_O)
_CLASS_MACRO(llvmo::ArrayType_O)
_CLASS_MACRO(llvmo::AtomicCmpXchgInst_O)
_CLASS_MACRO(llvmo::AtomicRMWInst_O)
_CLASS_MACRO(llvmo::BlockAddress_O)
_CLASS_MACRO(llvmo::CallInst_O)
_CLASS_MACRO(llvmo::ConstantArray_O)
_CLASS_MACRO(llvmo::ConstantDataSequential_O)
_CLASS_MACRO(llvmo::ConstantExpr_O)
_CLASS_MACRO(llvmo::ConstantFP_O)
_CLASS_MACRO(llvmo::ConstantInt_O)
_CLASS_MACRO(llvmo::ConstantPointerNull_O)
_CLASS_MACRO(llvmo::DataLayoutPass_O)
_CLASS_MACRO(llvmo::FenceInst_O)
_CLASS_MACRO(llvmo::GlobalValue_O)
_CLASS_MACRO(llvmo::LandingPadInst_O)
_CLASS_MACRO(llvmo::PHINode_O)
_CLASS_MACRO(llvmo::PointerType_O)
_CLASS_MACRO(llvmo::StoreInst_O)
_CLASS_MACRO(llvmo::TerminatorInst_O)
_CLASS_MACRO(llvmo::UnaryInstruction_O)
_CLASS_MACRO(llvmo::UndefValue_O)
_CLASS_MACRO(llvmo::VectorType_O)
_CLASS_MACRO(llvmo::AllocaInst_O)
_CLASS_MACRO(llvmo::BranchInst_O)
_CLASS_MACRO(llvmo::ConstantDataArray_O)
_CLASS_MACRO(llvmo::Function_O)
_CLASS_MACRO(llvmo::GlobalVariable_O)
_CLASS_MACRO(llvmo::IndirectBrInst_O)
_CLASS_MACRO(llvmo::InvokeInst_O)
_CLASS_MACRO(llvmo::LoadInst_O)
_CLASS_MACRO(llvmo::ResumeInst_O)
_CLASS_MACRO(llvmo::ReturnInst_O)
_CLASS_MACRO(llvmo::SwitchInst_O)
_CLASS_MACRO(llvmo::UnreachableInst_O)
_CLASS_MACRO(llvmo::VAArgInst_O)
#endif // EXPAND_CLASS_MACROS
#undef ALL_STAGES
