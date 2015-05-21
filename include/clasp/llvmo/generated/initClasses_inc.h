// start
// define cpp macros: SET_SYMBOL, CREATE_CLASS, SET_CLASS, DEFINE_BASE_CLASSES, DEFINE_CLASS_NAMES, EXPOSE_TO_CANDO 
// define cpp macro: ALL_STAGES to get the effect of defining all of the macros above
// define cpp macro: EXPOSE_PYTHON to expose python
// Associating namespace(llvmo) with package(LlvmoPkg)
 // class DebugInfo_O : public core::T_O {
 //class DIDescriptor_O : public DebugInfo_O, public llvm::DIDescriptor {
 //class DIScope_O : public DebugInfo_O, public llvm::DIScope {
 //class DIArray_O : public DebugInfo_O, public llvm::DIArray {
 //class DITypeArray_O : public DebugInfo_O, public llvm::DITypeArray {
 //class DIFile_O : public DebugInfo_O, public llvm::DIFile {
 //class DISubprogram_O : public DebugInfo_O, public llvm::DISubprogram {
 //class DIType_O : public DebugInfo_O, public llvm::DIType {
 //class DIDerivedType_O : public DebugInfo_O, public llvm::DIDerivedType {
 //class DIBasicType_O : public DebugInfo_O, public llvm::DIBasicType {
 //class DICompositeType_O : public DebugInfo_O, public llvm::DICompositeType {
 //class DISubroutineType_O : public DebugInfo_O, public llvm::DISubroutineType {
 //class DILexicalBlock_O : public DebugInfo_O, public llvm::DILexicalBlock {
 //class DICompileUnit_O : public DebugInfo_O, public llvm::DICompileUnit {
 // class DIBuilder_O : public core::ExternalObject_O {
 // class DebugLoc_O : public core::T_O {
 // class InsertPoint_O : public core::T_O {
 // class LLVMContext_O : public core::ExternalObject_O {
 // class Linker_O : public core::ExternalObject_O {
 // class Pass_O : public core::ExternalObject_O {
 // class AttributeSet_O : public core::T_O {
 // class Triple_O : public core::ExternalObject_O {
 // class TargetOptions_O : public core::ExternalObject_O {
 // class Target_O : public core::ExternalObject_O {
 // class MCSubtargetInfo_O : public core::ExternalObject_O {
 // class TargetSubtargetInfo_O : public MCSubtargetInfo_O {
 // class TargetMachine_O : public core::ExternalObject_O {
 // class LLVMTargetMachine_O : public TargetMachine_O {
 // class FunctionPass_O : public Pass_O {
 // class ModulePass_O : public Pass_O {
 // class ImmutablePass_O : public ModulePass_O {
 // class PassManagerBase_O : public core::ExternalObject_O {
 // class Value_O : public core::ExternalObject_O {
 // class Metadata_O : public core::ExternalObject_O {
 // class User_O : public Value_O {
 // class Attribute_O : public core::T_O {
 // class DataLayout_O : public core::ExternalObject_O {
 // class Constant_O : public User_O {
 // class ConstantArray_O : public Constant_O {
 // class BlockAddress_O : public Constant_O {
 // class ConstantDataSequential_O : public Constant_O {
 // class ConstantDataArray_O : public ConstantDataSequential_O {
 // class ConstantExpr_O : public Constant_O {
 // class GlobalValue_O : public Constant_O {
 // class GlobalVariable_O : public GlobalValue_O {
 // class ExecutionEngine_O : public core::ExternalObject_O {
 // class Module_O : public core::ExternalObject_O {
 // class DataLayoutPass_O : public ImmutablePass_O {
 // class TargetLibraryInfo_O : public ImmutablePass_O {
 // class FunctionPassManager_O : public PassManagerBase_O {
 // class PassManager_O : public PassManagerBase_O {
 // class EngineBuilder_O : public core::ExternalObject_O {
 // class PassManagerBuilder_O : public core::ExternalObject_O {
 // class APFloat_O : public core::ExternalObject_O {
 // class APInt_O : public core::ExternalObject_O {
 // class IRBuilderBase_O : public core::ExternalObject_O {
 // class IRBuilder_O : public IRBuilderBase_O {
 // class Instruction_O : public User_O {
 // class StoreInst_O : public Instruction_O {
 // class FenceInst_O : public Instruction_O {
 // class AtomicCmpXchgInst_O : public Instruction_O {
 // class AtomicRMWInst_O : public Instruction_O {
 // class PHINode_O : public Instruction_O {
 // class CallInst_O : public Instruction_O {
 // class LandingPadInst_O : public Instruction_O {
 // class UnaryInstruction_O : public Instruction_O {
 // class AllocaInst_O : public UnaryInstruction_O {
 // class VAArgInst_O : public UnaryInstruction_O {
 // class LoadInst_O : public UnaryInstruction_O {
 // class TerminatorInst_O : public Instruction_O {
 // class BranchInst_O : public TerminatorInst_O {
 // class SwitchInst_O : public TerminatorInst_O {
 // class IndirectBrInst_O : public TerminatorInst_O {
 // class InvokeInst_O : public TerminatorInst_O {
 // class ResumeInst_O : public TerminatorInst_O {
 // class UnreachableInst_O : public TerminatorInst_O {
 // class ReturnInst_O : public TerminatorInst_O {
 // class ConstantFP_O : public Constant_O {
 // class ConstantInt_O : public Constant_O {
 // class ConstantStruct_O : public Constant_O {
 // class UndefValue_O : public Constant_O {
 // class ConstantPointerNull_O : public Constant_O {
 // class MDNode_O : public Metadata_O {
 // class MDString_O : public Metadata_O {
 // class ValueAsMetadata_O : public Metadata_O {
 // class NamedMDNode_O : public core::ExternalObject_O {
 // class Function_O : public GlobalValue_O {
 // class BasicBlock_O : public Value_O {
 // class Argument_O : public Value_O {
 // class Type_O : public core::ExternalObject_O {
 // class FunctionType_O : public Type_O {
 // class IntegerType_O : public Type_O {
 // class CompositeType_O : public Type_O {
 // class StructType_O : public CompositeType_O {
 // class SequentialType_O : public CompositeType_O {
 // class PointerType_O : public SequentialType_O {
 // class ArrayType_O : public SequentialType_O {
 // class VectorType_O : public SequentialType_O {
// Associating namespace(llvmo) with package(LlvmoPkg)
#ifdef HEADER_INCLUDES
#include "include/llvmoExpose.h"
#include "include/debugInfoExpose.h"
#include "include/debugLoc.h"
#include "include/insertPoint.h"
#endif // HEADER_INCLUDES
#undef HEADER_INCLUDES
#if defined(SET_SYMBOL) || defined(ALL_STAGES)
// requires LOOKUP_SYMBOL(pkg,symbolName) be defined
llvmo::APFloat_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::APFloat_O::static_packageName(),llvmo::APFloat_O::static_className()));
llvmo::APInt_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::APInt_O::static_packageName(),llvmo::APInt_O::static_className()));
llvmo::AttributeSet_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::AttributeSet_O::static_packageName(),llvmo::AttributeSet_O::static_className()));
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
llvmo::MCSubtargetInfo_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::MCSubtargetInfo_O::static_packageName(),llvmo::MCSubtargetInfo_O::static_className()));
llvmo::Metadata_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::Metadata_O::static_packageName(),llvmo::Metadata_O::static_className()));
llvmo::Module_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::Module_O::static_packageName(),llvmo::Module_O::static_className()));
llvmo::NamedMDNode_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::NamedMDNode_O::static_packageName(),llvmo::NamedMDNode_O::static_className()));
llvmo::PassManagerBase_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::PassManagerBase_O::static_packageName(),llvmo::PassManagerBase_O::static_className()));
llvmo::PassManagerBuilder_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::PassManagerBuilder_O::static_packageName(),llvmo::PassManagerBuilder_O::static_className()));
llvmo::Pass_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::Pass_O::static_packageName(),llvmo::Pass_O::static_className()));
llvmo::TargetMachine_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::TargetMachine_O::static_packageName(),llvmo::TargetMachine_O::static_className()));
llvmo::TargetOptions_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::TargetOptions_O::static_packageName(),llvmo::TargetOptions_O::static_className()));
llvmo::Target_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::Target_O::static_packageName(),llvmo::Target_O::static_className()));
llvmo::Triple_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::Triple_O::static_packageName(),llvmo::Triple_O::static_className()));
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
llvmo::DISubroutineType_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::DISubroutineType_O::static_packageName(),llvmo::DISubroutineType_O::static_className()));
llvmo::DITypeArray_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::DITypeArray_O::static_packageName(),llvmo::DITypeArray_O::static_className()));
llvmo::DIType_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::DIType_O::static_packageName(),llvmo::DIType_O::static_className()));
llvmo::FunctionPassManager_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::FunctionPassManager_O::static_packageName(),llvmo::FunctionPassManager_O::static_className()));
llvmo::FunctionPass_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::FunctionPass_O::static_packageName(),llvmo::FunctionPass_O::static_className()));
llvmo::FunctionType_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::FunctionType_O::static_packageName(),llvmo::FunctionType_O::static_className()));
llvmo::IRBuilder_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::IRBuilder_O::static_packageName(),llvmo::IRBuilder_O::static_className()));
llvmo::IntegerType_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::IntegerType_O::static_packageName(),llvmo::IntegerType_O::static_className()));
llvmo::LLVMTargetMachine_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::LLVMTargetMachine_O::static_packageName(),llvmo::LLVMTargetMachine_O::static_className()));
llvmo::MDNode_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::MDNode_O::static_packageName(),llvmo::MDNode_O::static_className()));
llvmo::MDString_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::MDString_O::static_packageName(),llvmo::MDString_O::static_className()));
llvmo::ModulePass_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::ModulePass_O::static_packageName(),llvmo::ModulePass_O::static_className()));
llvmo::PassManager_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::PassManager_O::static_packageName(),llvmo::PassManager_O::static_className()));
llvmo::TargetSubtargetInfo_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::TargetSubtargetInfo_O::static_packageName(),llvmo::TargetSubtargetInfo_O::static_className()));
llvmo::User_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::User_O::static_packageName(),llvmo::User_O::static_className()));
llvmo::ValueAsMetadata_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::ValueAsMetadata_O::static_packageName(),llvmo::ValueAsMetadata_O::static_className()));
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
llvmo::ConstantStruct_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::ConstantStruct_O::static_packageName(),llvmo::ConstantStruct_O::static_className()));
llvmo::DataLayoutPass_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::DataLayoutPass_O::static_packageName(),llvmo::DataLayoutPass_O::static_className()));
llvmo::FenceInst_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::FenceInst_O::static_packageName(),llvmo::FenceInst_O::static_className()));
llvmo::GlobalValue_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::GlobalValue_O::static_packageName(),llvmo::GlobalValue_O::static_className()));
llvmo::LandingPadInst_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::LandingPadInst_O::static_packageName(),llvmo::LandingPadInst_O::static_className()));
llvmo::PHINode_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::PHINode_O::static_packageName(),llvmo::PHINode_O::static_className()));
llvmo::PointerType_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::PointerType_O::static_packageName(),llvmo::PointerType_O::static_className()));
llvmo::StoreInst_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::StoreInst_O::static_packageName(),llvmo::StoreInst_O::static_className()));
llvmo::TargetLibraryInfo_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::TargetLibraryInfo_O::static_packageName(),llvmo::TargetLibraryInfo_O::static_className()));
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
    core::BuiltInClass_sp classllvmo__APFloat_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__APFloat_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__APFloat_Oval,_lisp,llvmo::APFloat_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::APFloat_O>::id,llvmo::APFloat_O::static_classSymbol());
    llvmo::APFloat_O::___staticClass = classllvmo__APFloat_Oval;
#ifdef USE_MPS
    llvmo::APFloat_O::static_Kind = gctools::GCKind<llvmo::APFloat_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__APFloat_Oval,llvmo::APFloat_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::APFloat_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::APFloat_O>>::allocateClass();
        llvmo::APFloat_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::APFloat_O::static_className() % (void*)(llvmo::APFloat_O::static_allocator) );
    classllvmo__APFloat_Oval->setCreator(llvmo::APFloat_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::APFloat_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__APFloat_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__APInt_Oval]"));
    core::BuiltInClass_sp classllvmo__APInt_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__APInt_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__APInt_Oval,_lisp,llvmo::APInt_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::APInt_O>::id,llvmo::APInt_O::static_classSymbol());
    llvmo::APInt_O::___staticClass = classllvmo__APInt_Oval;
#ifdef USE_MPS
    llvmo::APInt_O::static_Kind = gctools::GCKind<llvmo::APInt_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__APInt_Oval,llvmo::APInt_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::APInt_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::APInt_O>>::allocateClass();
        llvmo::APInt_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::APInt_O::static_className() % (void*)(llvmo::APInt_O::static_allocator) );
    classllvmo__APInt_Oval->setCreator(llvmo::APInt_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::APInt_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__APInt_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__AttributeSet_Oval]"));
    core::BuiltInClass_sp classllvmo__AttributeSet_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__AttributeSet_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__AttributeSet_Oval,_lisp,llvmo::AttributeSet_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::AttributeSet_O>::id,llvmo::AttributeSet_O::static_classSymbol());
    llvmo::AttributeSet_O::___staticClass = classllvmo__AttributeSet_Oval;
#ifdef USE_MPS
    llvmo::AttributeSet_O::static_Kind = gctools::GCKind<llvmo::AttributeSet_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__AttributeSet_Oval,llvmo::AttributeSet_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::AttributeSet_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::AttributeSet_O>>::allocateClass();
        llvmo::AttributeSet_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::AttributeSet_O::static_className() % (void*)(llvmo::AttributeSet_O::static_allocator) );
    classllvmo__AttributeSet_Oval->setCreator(llvmo::AttributeSet_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::AttributeSet_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__AttributeSet_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__Attribute_Oval]"));
    core::BuiltInClass_sp classllvmo__Attribute_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__Attribute_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__Attribute_Oval,_lisp,llvmo::Attribute_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::Attribute_O>::id,llvmo::Attribute_O::static_classSymbol());
    llvmo::Attribute_O::___staticClass = classllvmo__Attribute_Oval;
#ifdef USE_MPS
    llvmo::Attribute_O::static_Kind = gctools::GCKind<llvmo::Attribute_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__Attribute_Oval,llvmo::Attribute_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::Attribute_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::Attribute_O>>::allocateClass();
        llvmo::Attribute_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::Attribute_O::static_className() % (void*)(llvmo::Attribute_O::static_allocator) );
    classllvmo__Attribute_Oval->setCreator(llvmo::Attribute_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::Attribute_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__Attribute_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__DIBuilder_Oval]"));
    core::BuiltInClass_sp classllvmo__DIBuilder_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__DIBuilder_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__DIBuilder_Oval,_lisp,llvmo::DIBuilder_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::DIBuilder_O>::id,llvmo::DIBuilder_O::static_classSymbol());
    llvmo::DIBuilder_O::___staticClass = classllvmo__DIBuilder_Oval;
#ifdef USE_MPS
    llvmo::DIBuilder_O::static_Kind = gctools::GCKind<llvmo::DIBuilder_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__DIBuilder_Oval,llvmo::DIBuilder_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::DIBuilder_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::DIBuilder_O>>::allocateClass();
        llvmo::DIBuilder_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::DIBuilder_O::static_className() % (void*)(llvmo::DIBuilder_O::static_allocator) );
    classllvmo__DIBuilder_Oval->setCreator(llvmo::DIBuilder_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::DIBuilder_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__DIBuilder_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__DataLayout_Oval]"));
    core::BuiltInClass_sp classllvmo__DataLayout_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__DataLayout_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__DataLayout_Oval,_lisp,llvmo::DataLayout_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::DataLayout_O>::id,llvmo::DataLayout_O::static_classSymbol());
    llvmo::DataLayout_O::___staticClass = classllvmo__DataLayout_Oval;
#ifdef USE_MPS
    llvmo::DataLayout_O::static_Kind = gctools::GCKind<llvmo::DataLayout_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__DataLayout_Oval,llvmo::DataLayout_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::DataLayout_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::DataLayout_O>>::allocateClass();
        llvmo::DataLayout_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::DataLayout_O::static_className() % (void*)(llvmo::DataLayout_O::static_allocator) );
    classllvmo__DataLayout_Oval->setCreator(llvmo::DataLayout_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::DataLayout_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__DataLayout_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__DebugInfo_Oval]"));
    core::BuiltInClass_sp classllvmo__DebugInfo_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__DebugInfo_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__DebugInfo_Oval,_lisp,llvmo::DebugInfo_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::DebugInfo_O>::id,llvmo::DebugInfo_O::static_classSymbol());
    llvmo::DebugInfo_O::___staticClass = classllvmo__DebugInfo_Oval;
#ifdef USE_MPS
    llvmo::DebugInfo_O::static_Kind = gctools::GCKind<llvmo::DebugInfo_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__DebugInfo_Oval,llvmo::DebugInfo_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::DebugInfo_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::DebugInfo_O>>::allocateClass();
        llvmo::DebugInfo_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::DebugInfo_O::static_className() % (void*)(llvmo::DebugInfo_O::static_allocator) );
    classllvmo__DebugInfo_Oval->setCreator(llvmo::DebugInfo_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::DebugInfo_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__DebugInfo_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__DebugLoc_Oval]"));
    core::BuiltInClass_sp classllvmo__DebugLoc_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__DebugLoc_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__DebugLoc_Oval,_lisp,llvmo::DebugLoc_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::DebugLoc_O>::id,llvmo::DebugLoc_O::static_classSymbol());
    llvmo::DebugLoc_O::___staticClass = classllvmo__DebugLoc_Oval;
#ifdef USE_MPS
    llvmo::DebugLoc_O::static_Kind = gctools::GCKind<llvmo::DebugLoc_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__DebugLoc_Oval,llvmo::DebugLoc_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::DebugLoc_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::DebugLoc_O>>::allocateClass();
        llvmo::DebugLoc_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::DebugLoc_O::static_className() % (void*)(llvmo::DebugLoc_O::static_allocator) );
    classllvmo__DebugLoc_Oval->setCreator(llvmo::DebugLoc_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::DebugLoc_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__DebugLoc_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__EngineBuilder_Oval]"));
    core::BuiltInClass_sp classllvmo__EngineBuilder_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__EngineBuilder_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__EngineBuilder_Oval,_lisp,llvmo::EngineBuilder_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::EngineBuilder_O>::id,llvmo::EngineBuilder_O::static_classSymbol());
    llvmo::EngineBuilder_O::___staticClass = classllvmo__EngineBuilder_Oval;
#ifdef USE_MPS
    llvmo::EngineBuilder_O::static_Kind = gctools::GCKind<llvmo::EngineBuilder_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__EngineBuilder_Oval,llvmo::EngineBuilder_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::EngineBuilder_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::EngineBuilder_O>>::allocateClass();
        llvmo::EngineBuilder_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::EngineBuilder_O::static_className() % (void*)(llvmo::EngineBuilder_O::static_allocator) );
    classllvmo__EngineBuilder_Oval->setCreator(llvmo::EngineBuilder_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::EngineBuilder_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__EngineBuilder_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__ExecutionEngine_Oval]"));
    core::BuiltInClass_sp classllvmo__ExecutionEngine_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__ExecutionEngine_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ExecutionEngine_Oval,_lisp,llvmo::ExecutionEngine_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::ExecutionEngine_O>::id,llvmo::ExecutionEngine_O::static_classSymbol());
    llvmo::ExecutionEngine_O::___staticClass = classllvmo__ExecutionEngine_Oval;
#ifdef USE_MPS
    llvmo::ExecutionEngine_O::static_Kind = gctools::GCKind<llvmo::ExecutionEngine_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__ExecutionEngine_Oval,llvmo::ExecutionEngine_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::ExecutionEngine_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::ExecutionEngine_O>>::allocateClass();
        llvmo::ExecutionEngine_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::ExecutionEngine_O::static_className() % (void*)(llvmo::ExecutionEngine_O::static_allocator) );
    classllvmo__ExecutionEngine_Oval->setCreator(llvmo::ExecutionEngine_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::ExecutionEngine_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__ExecutionEngine_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__IRBuilderBase_Oval]"));
    core::BuiltInClass_sp classllvmo__IRBuilderBase_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__IRBuilderBase_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__IRBuilderBase_Oval,_lisp,llvmo::IRBuilderBase_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::IRBuilderBase_O>::id,llvmo::IRBuilderBase_O::static_classSymbol());
    llvmo::IRBuilderBase_O::___staticClass = classllvmo__IRBuilderBase_Oval;
#ifdef USE_MPS
    llvmo::IRBuilderBase_O::static_Kind = gctools::GCKind<llvmo::IRBuilderBase_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__IRBuilderBase_Oval,llvmo::IRBuilderBase_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::IRBuilderBase_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::IRBuilderBase_O>>::allocateClass();
        llvmo::IRBuilderBase_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::IRBuilderBase_O::static_className() % (void*)(llvmo::IRBuilderBase_O::static_allocator) );
    classllvmo__IRBuilderBase_Oval->setCreator(llvmo::IRBuilderBase_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::IRBuilderBase_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__IRBuilderBase_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__InsertPoint_Oval]"));
    core::BuiltInClass_sp classllvmo__InsertPoint_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__InsertPoint_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__InsertPoint_Oval,_lisp,llvmo::InsertPoint_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::InsertPoint_O>::id,llvmo::InsertPoint_O::static_classSymbol());
    llvmo::InsertPoint_O::___staticClass = classllvmo__InsertPoint_Oval;
#ifdef USE_MPS
    llvmo::InsertPoint_O::static_Kind = gctools::GCKind<llvmo::InsertPoint_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__InsertPoint_Oval,llvmo::InsertPoint_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::InsertPoint_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::InsertPoint_O>>::allocateClass();
        llvmo::InsertPoint_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::InsertPoint_O::static_className() % (void*)(llvmo::InsertPoint_O::static_allocator) );
    classllvmo__InsertPoint_Oval->setCreator(llvmo::InsertPoint_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::InsertPoint_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__InsertPoint_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__LLVMContext_Oval]"));
    core::BuiltInClass_sp classllvmo__LLVMContext_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__LLVMContext_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__LLVMContext_Oval,_lisp,llvmo::LLVMContext_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::LLVMContext_O>::id,llvmo::LLVMContext_O::static_classSymbol());
    llvmo::LLVMContext_O::___staticClass = classllvmo__LLVMContext_Oval;
#ifdef USE_MPS
    llvmo::LLVMContext_O::static_Kind = gctools::GCKind<llvmo::LLVMContext_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__LLVMContext_Oval,llvmo::LLVMContext_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::LLVMContext_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::LLVMContext_O>>::allocateClass();
        llvmo::LLVMContext_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::LLVMContext_O::static_className() % (void*)(llvmo::LLVMContext_O::static_allocator) );
    classllvmo__LLVMContext_Oval->setCreator(llvmo::LLVMContext_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::LLVMContext_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__LLVMContext_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__Linker_Oval]"));
    core::BuiltInClass_sp classllvmo__Linker_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__Linker_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__Linker_Oval,_lisp,llvmo::Linker_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::Linker_O>::id,llvmo::Linker_O::static_classSymbol());
    llvmo::Linker_O::___staticClass = classllvmo__Linker_Oval;
#ifdef USE_MPS
    llvmo::Linker_O::static_Kind = gctools::GCKind<llvmo::Linker_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__Linker_Oval,llvmo::Linker_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::Linker_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::Linker_O>>::allocateClass();
        llvmo::Linker_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::Linker_O::static_className() % (void*)(llvmo::Linker_O::static_allocator) );
    classllvmo__Linker_Oval->setCreator(llvmo::Linker_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::Linker_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__Linker_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__MCSubtargetInfo_Oval]"));
    core::BuiltInClass_sp classllvmo__MCSubtargetInfo_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__MCSubtargetInfo_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__MCSubtargetInfo_Oval,_lisp,llvmo::MCSubtargetInfo_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::MCSubtargetInfo_O>::id,llvmo::MCSubtargetInfo_O::static_classSymbol());
    llvmo::MCSubtargetInfo_O::___staticClass = classllvmo__MCSubtargetInfo_Oval;
#ifdef USE_MPS
    llvmo::MCSubtargetInfo_O::static_Kind = gctools::GCKind<llvmo::MCSubtargetInfo_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__MCSubtargetInfo_Oval,llvmo::MCSubtargetInfo_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::MCSubtargetInfo_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::MCSubtargetInfo_O>>::allocateClass();
        llvmo::MCSubtargetInfo_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::MCSubtargetInfo_O::static_className() % (void*)(llvmo::MCSubtargetInfo_O::static_allocator) );
    classllvmo__MCSubtargetInfo_Oval->setCreator(llvmo::MCSubtargetInfo_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::MCSubtargetInfo_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__MCSubtargetInfo_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__Metadata_Oval]"));
    core::BuiltInClass_sp classllvmo__Metadata_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__Metadata_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__Metadata_Oval,_lisp,llvmo::Metadata_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::Metadata_O>::id,llvmo::Metadata_O::static_classSymbol());
    llvmo::Metadata_O::___staticClass = classllvmo__Metadata_Oval;
#ifdef USE_MPS
    llvmo::Metadata_O::static_Kind = gctools::GCKind<llvmo::Metadata_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__Metadata_Oval,llvmo::Metadata_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::Metadata_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::Metadata_O>>::allocateClass();
        llvmo::Metadata_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::Metadata_O::static_className() % (void*)(llvmo::Metadata_O::static_allocator) );
    classllvmo__Metadata_Oval->setCreator(llvmo::Metadata_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::Metadata_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__Metadata_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__Module_Oval]"));
    core::BuiltInClass_sp classllvmo__Module_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__Module_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__Module_Oval,_lisp,llvmo::Module_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::Module_O>::id,llvmo::Module_O::static_classSymbol());
    llvmo::Module_O::___staticClass = classllvmo__Module_Oval;
#ifdef USE_MPS
    llvmo::Module_O::static_Kind = gctools::GCKind<llvmo::Module_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__Module_Oval,llvmo::Module_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::Module_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::Module_O>>::allocateClass();
        llvmo::Module_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::Module_O::static_className() % (void*)(llvmo::Module_O::static_allocator) );
    classllvmo__Module_Oval->setCreator(llvmo::Module_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::Module_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__Module_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__NamedMDNode_Oval]"));
    core::BuiltInClass_sp classllvmo__NamedMDNode_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__NamedMDNode_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__NamedMDNode_Oval,_lisp,llvmo::NamedMDNode_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::NamedMDNode_O>::id,llvmo::NamedMDNode_O::static_classSymbol());
    llvmo::NamedMDNode_O::___staticClass = classllvmo__NamedMDNode_Oval;
#ifdef USE_MPS
    llvmo::NamedMDNode_O::static_Kind = gctools::GCKind<llvmo::NamedMDNode_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__NamedMDNode_Oval,llvmo::NamedMDNode_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::NamedMDNode_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::NamedMDNode_O>>::allocateClass();
        llvmo::NamedMDNode_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::NamedMDNode_O::static_className() % (void*)(llvmo::NamedMDNode_O::static_allocator) );
    classllvmo__NamedMDNode_Oval->setCreator(llvmo::NamedMDNode_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::NamedMDNode_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__NamedMDNode_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__PassManagerBase_Oval]"));
    core::BuiltInClass_sp classllvmo__PassManagerBase_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__PassManagerBase_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__PassManagerBase_Oval,_lisp,llvmo::PassManagerBase_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::PassManagerBase_O>::id,llvmo::PassManagerBase_O::static_classSymbol());
    llvmo::PassManagerBase_O::___staticClass = classllvmo__PassManagerBase_Oval;
#ifdef USE_MPS
    llvmo::PassManagerBase_O::static_Kind = gctools::GCKind<llvmo::PassManagerBase_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__PassManagerBase_Oval,llvmo::PassManagerBase_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::PassManagerBase_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::PassManagerBase_O>>::allocateClass();
        llvmo::PassManagerBase_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::PassManagerBase_O::static_className() % (void*)(llvmo::PassManagerBase_O::static_allocator) );
    classllvmo__PassManagerBase_Oval->setCreator(llvmo::PassManagerBase_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::PassManagerBase_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__PassManagerBase_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__PassManagerBuilder_Oval]"));
    core::BuiltInClass_sp classllvmo__PassManagerBuilder_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__PassManagerBuilder_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__PassManagerBuilder_Oval,_lisp,llvmo::PassManagerBuilder_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::PassManagerBuilder_O>::id,llvmo::PassManagerBuilder_O::static_classSymbol());
    llvmo::PassManagerBuilder_O::___staticClass = classllvmo__PassManagerBuilder_Oval;
#ifdef USE_MPS
    llvmo::PassManagerBuilder_O::static_Kind = gctools::GCKind<llvmo::PassManagerBuilder_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__PassManagerBuilder_Oval,llvmo::PassManagerBuilder_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::PassManagerBuilder_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::PassManagerBuilder_O>>::allocateClass();
        llvmo::PassManagerBuilder_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::PassManagerBuilder_O::static_className() % (void*)(llvmo::PassManagerBuilder_O::static_allocator) );
    classllvmo__PassManagerBuilder_Oval->setCreator(llvmo::PassManagerBuilder_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::PassManagerBuilder_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__PassManagerBuilder_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__Pass_Oval]"));
    core::BuiltInClass_sp classllvmo__Pass_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__Pass_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__Pass_Oval,_lisp,llvmo::Pass_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::Pass_O>::id,llvmo::Pass_O::static_classSymbol());
    llvmo::Pass_O::___staticClass = classllvmo__Pass_Oval;
#ifdef USE_MPS
    llvmo::Pass_O::static_Kind = gctools::GCKind<llvmo::Pass_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__Pass_Oval,llvmo::Pass_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::Pass_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::Pass_O>>::allocateClass();
        llvmo::Pass_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::Pass_O::static_className() % (void*)(llvmo::Pass_O::static_allocator) );
    classllvmo__Pass_Oval->setCreator(llvmo::Pass_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::Pass_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__Pass_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__TargetMachine_Oval]"));
    core::BuiltInClass_sp classllvmo__TargetMachine_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__TargetMachine_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__TargetMachine_Oval,_lisp,llvmo::TargetMachine_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::TargetMachine_O>::id,llvmo::TargetMachine_O::static_classSymbol());
    llvmo::TargetMachine_O::___staticClass = classllvmo__TargetMachine_Oval;
#ifdef USE_MPS
    llvmo::TargetMachine_O::static_Kind = gctools::GCKind<llvmo::TargetMachine_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__TargetMachine_Oval,llvmo::TargetMachine_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::TargetMachine_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::TargetMachine_O>>::allocateClass();
        llvmo::TargetMachine_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::TargetMachine_O::static_className() % (void*)(llvmo::TargetMachine_O::static_allocator) );
    classllvmo__TargetMachine_Oval->setCreator(llvmo::TargetMachine_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::TargetMachine_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__TargetMachine_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__TargetOptions_Oval]"));
    core::BuiltInClass_sp classllvmo__TargetOptions_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__TargetOptions_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__TargetOptions_Oval,_lisp,llvmo::TargetOptions_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::TargetOptions_O>::id,llvmo::TargetOptions_O::static_classSymbol());
    llvmo::TargetOptions_O::___staticClass = classllvmo__TargetOptions_Oval;
#ifdef USE_MPS
    llvmo::TargetOptions_O::static_Kind = gctools::GCKind<llvmo::TargetOptions_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__TargetOptions_Oval,llvmo::TargetOptions_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::TargetOptions_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::TargetOptions_O>>::allocateClass();
        llvmo::TargetOptions_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::TargetOptions_O::static_className() % (void*)(llvmo::TargetOptions_O::static_allocator) );
    classllvmo__TargetOptions_Oval->setCreator(llvmo::TargetOptions_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::TargetOptions_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__TargetOptions_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__Target_Oval]"));
    core::BuiltInClass_sp classllvmo__Target_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__Target_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__Target_Oval,_lisp,llvmo::Target_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::Target_O>::id,llvmo::Target_O::static_classSymbol());
    llvmo::Target_O::___staticClass = classllvmo__Target_Oval;
#ifdef USE_MPS
    llvmo::Target_O::static_Kind = gctools::GCKind<llvmo::Target_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__Target_Oval,llvmo::Target_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::Target_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::Target_O>>::allocateClass();
        llvmo::Target_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::Target_O::static_className() % (void*)(llvmo::Target_O::static_allocator) );
    classllvmo__Target_Oval->setCreator(llvmo::Target_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::Target_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__Target_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__Triple_Oval]"));
    core::BuiltInClass_sp classllvmo__Triple_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__Triple_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__Triple_Oval,_lisp,llvmo::Triple_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::Triple_O>::id,llvmo::Triple_O::static_classSymbol());
    llvmo::Triple_O::___staticClass = classllvmo__Triple_Oval;
#ifdef USE_MPS
    llvmo::Triple_O::static_Kind = gctools::GCKind<llvmo::Triple_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__Triple_Oval,llvmo::Triple_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::Triple_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::Triple_O>>::allocateClass();
        llvmo::Triple_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::Triple_O::static_className() % (void*)(llvmo::Triple_O::static_allocator) );
    classllvmo__Triple_Oval->setCreator(llvmo::Triple_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::Triple_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__Triple_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__Type_Oval]"));
    core::BuiltInClass_sp classllvmo__Type_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__Type_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__Type_Oval,_lisp,llvmo::Type_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::Type_O>::id,llvmo::Type_O::static_classSymbol());
    llvmo::Type_O::___staticClass = classllvmo__Type_Oval;
#ifdef USE_MPS
    llvmo::Type_O::static_Kind = gctools::GCKind<llvmo::Type_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__Type_Oval,llvmo::Type_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::Type_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::Type_O>>::allocateClass();
        llvmo::Type_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::Type_O::static_className() % (void*)(llvmo::Type_O::static_allocator) );
    classllvmo__Type_Oval->setCreator(llvmo::Type_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::Type_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__Type_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__Value_Oval]"));
    core::BuiltInClass_sp classllvmo__Value_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__Value_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__Value_Oval,_lisp,llvmo::Value_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::Value_O>::id,llvmo::Value_O::static_classSymbol());
    llvmo::Value_O::___staticClass = classllvmo__Value_Oval;
#ifdef USE_MPS
    llvmo::Value_O::static_Kind = gctools::GCKind<llvmo::Value_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__Value_Oval,llvmo::Value_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::Value_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::Value_O>>::allocateClass();
        llvmo::Value_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::Value_O::static_className() % (void*)(llvmo::Value_O::static_allocator) );
    classllvmo__Value_Oval->setCreator(llvmo::Value_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::Value_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__Value_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__Argument_Oval]"));
    core::BuiltInClass_sp classllvmo__Argument_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__Argument_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__Argument_Oval,_lisp,llvmo::Argument_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::Argument_O>::id,llvmo::Argument_O::static_classSymbol());
    llvmo::Argument_O::___staticClass = classllvmo__Argument_Oval;
#ifdef USE_MPS
    llvmo::Argument_O::static_Kind = gctools::GCKind<llvmo::Argument_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__Argument_Oval,llvmo::Argument_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::Argument_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::Argument_O>>::allocateClass();
        llvmo::Argument_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::Argument_O::static_className() % (void*)(llvmo::Argument_O::static_allocator) );
    classllvmo__Argument_Oval->setCreator(llvmo::Argument_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::Argument_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__Argument_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__BasicBlock_Oval]"));
    core::BuiltInClass_sp classllvmo__BasicBlock_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__BasicBlock_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__BasicBlock_Oval,_lisp,llvmo::BasicBlock_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::BasicBlock_O>::id,llvmo::BasicBlock_O::static_classSymbol());
    llvmo::BasicBlock_O::___staticClass = classllvmo__BasicBlock_Oval;
#ifdef USE_MPS
    llvmo::BasicBlock_O::static_Kind = gctools::GCKind<llvmo::BasicBlock_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__BasicBlock_Oval,llvmo::BasicBlock_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::BasicBlock_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::BasicBlock_O>>::allocateClass();
        llvmo::BasicBlock_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::BasicBlock_O::static_className() % (void*)(llvmo::BasicBlock_O::static_allocator) );
    classllvmo__BasicBlock_Oval->setCreator(llvmo::BasicBlock_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::BasicBlock_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__BasicBlock_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__CompositeType_Oval]"));
    core::BuiltInClass_sp classllvmo__CompositeType_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__CompositeType_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__CompositeType_Oval,_lisp,llvmo::CompositeType_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::CompositeType_O>::id,llvmo::CompositeType_O::static_classSymbol());
    llvmo::CompositeType_O::___staticClass = classllvmo__CompositeType_Oval;
#ifdef USE_MPS
    llvmo::CompositeType_O::static_Kind = gctools::GCKind<llvmo::CompositeType_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__CompositeType_Oval,llvmo::CompositeType_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::CompositeType_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::CompositeType_O>>::allocateClass();
        llvmo::CompositeType_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::CompositeType_O::static_className() % (void*)(llvmo::CompositeType_O::static_allocator) );
    classllvmo__CompositeType_Oval->setCreator(llvmo::CompositeType_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::CompositeType_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__CompositeType_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__DIArray_Oval]"));
    core::BuiltInClass_sp classllvmo__DIArray_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__DIArray_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__DIArray_Oval,_lisp,llvmo::DIArray_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::DIArray_O>::id,llvmo::DIArray_O::static_classSymbol());
    llvmo::DIArray_O::___staticClass = classllvmo__DIArray_Oval;
#ifdef USE_MPS
    llvmo::DIArray_O::static_Kind = gctools::GCKind<llvmo::DIArray_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__DIArray_Oval,llvmo::DIArray_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::DIArray_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::DIArray_O>>::allocateClass();
        llvmo::DIArray_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::DIArray_O::static_className() % (void*)(llvmo::DIArray_O::static_allocator) );
    classllvmo__DIArray_Oval->setCreator(llvmo::DIArray_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::DIArray_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__DIArray_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__DIBasicType_Oval]"));
    core::BuiltInClass_sp classllvmo__DIBasicType_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__DIBasicType_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__DIBasicType_Oval,_lisp,llvmo::DIBasicType_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::DIBasicType_O>::id,llvmo::DIBasicType_O::static_classSymbol());
    llvmo::DIBasicType_O::___staticClass = classllvmo__DIBasicType_Oval;
#ifdef USE_MPS
    llvmo::DIBasicType_O::static_Kind = gctools::GCKind<llvmo::DIBasicType_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__DIBasicType_Oval,llvmo::DIBasicType_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::DIBasicType_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::DIBasicType_O>>::allocateClass();
        llvmo::DIBasicType_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::DIBasicType_O::static_className() % (void*)(llvmo::DIBasicType_O::static_allocator) );
    classllvmo__DIBasicType_Oval->setCreator(llvmo::DIBasicType_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::DIBasicType_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__DIBasicType_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__DICompileUnit_Oval]"));
    core::BuiltInClass_sp classllvmo__DICompileUnit_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__DICompileUnit_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__DICompileUnit_Oval,_lisp,llvmo::DICompileUnit_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::DICompileUnit_O>::id,llvmo::DICompileUnit_O::static_classSymbol());
    llvmo::DICompileUnit_O::___staticClass = classllvmo__DICompileUnit_Oval;
#ifdef USE_MPS
    llvmo::DICompileUnit_O::static_Kind = gctools::GCKind<llvmo::DICompileUnit_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__DICompileUnit_Oval,llvmo::DICompileUnit_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::DICompileUnit_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::DICompileUnit_O>>::allocateClass();
        llvmo::DICompileUnit_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::DICompileUnit_O::static_className() % (void*)(llvmo::DICompileUnit_O::static_allocator) );
    classllvmo__DICompileUnit_Oval->setCreator(llvmo::DICompileUnit_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::DICompileUnit_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__DICompileUnit_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__DICompositeType_Oval]"));
    core::BuiltInClass_sp classllvmo__DICompositeType_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__DICompositeType_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__DICompositeType_Oval,_lisp,llvmo::DICompositeType_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::DICompositeType_O>::id,llvmo::DICompositeType_O::static_classSymbol());
    llvmo::DICompositeType_O::___staticClass = classllvmo__DICompositeType_Oval;
#ifdef USE_MPS
    llvmo::DICompositeType_O::static_Kind = gctools::GCKind<llvmo::DICompositeType_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__DICompositeType_Oval,llvmo::DICompositeType_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::DICompositeType_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::DICompositeType_O>>::allocateClass();
        llvmo::DICompositeType_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::DICompositeType_O::static_className() % (void*)(llvmo::DICompositeType_O::static_allocator) );
    classllvmo__DICompositeType_Oval->setCreator(llvmo::DICompositeType_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::DICompositeType_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__DICompositeType_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__DIDerivedType_Oval]"));
    core::BuiltInClass_sp classllvmo__DIDerivedType_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__DIDerivedType_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__DIDerivedType_Oval,_lisp,llvmo::DIDerivedType_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::DIDerivedType_O>::id,llvmo::DIDerivedType_O::static_classSymbol());
    llvmo::DIDerivedType_O::___staticClass = classllvmo__DIDerivedType_Oval;
#ifdef USE_MPS
    llvmo::DIDerivedType_O::static_Kind = gctools::GCKind<llvmo::DIDerivedType_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__DIDerivedType_Oval,llvmo::DIDerivedType_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::DIDerivedType_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::DIDerivedType_O>>::allocateClass();
        llvmo::DIDerivedType_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::DIDerivedType_O::static_className() % (void*)(llvmo::DIDerivedType_O::static_allocator) );
    classllvmo__DIDerivedType_Oval->setCreator(llvmo::DIDerivedType_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::DIDerivedType_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__DIDerivedType_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__DIDescriptor_Oval]"));
    core::BuiltInClass_sp classllvmo__DIDescriptor_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__DIDescriptor_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__DIDescriptor_Oval,_lisp,llvmo::DIDescriptor_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::DIDescriptor_O>::id,llvmo::DIDescriptor_O::static_classSymbol());
    llvmo::DIDescriptor_O::___staticClass = classllvmo__DIDescriptor_Oval;
#ifdef USE_MPS
    llvmo::DIDescriptor_O::static_Kind = gctools::GCKind<llvmo::DIDescriptor_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__DIDescriptor_Oval,llvmo::DIDescriptor_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::DIDescriptor_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::DIDescriptor_O>>::allocateClass();
        llvmo::DIDescriptor_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::DIDescriptor_O::static_className() % (void*)(llvmo::DIDescriptor_O::static_allocator) );
    classllvmo__DIDescriptor_Oval->setCreator(llvmo::DIDescriptor_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::DIDescriptor_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__DIDescriptor_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__DIFile_Oval]"));
    core::BuiltInClass_sp classllvmo__DIFile_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__DIFile_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__DIFile_Oval,_lisp,llvmo::DIFile_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::DIFile_O>::id,llvmo::DIFile_O::static_classSymbol());
    llvmo::DIFile_O::___staticClass = classllvmo__DIFile_Oval;
#ifdef USE_MPS
    llvmo::DIFile_O::static_Kind = gctools::GCKind<llvmo::DIFile_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__DIFile_Oval,llvmo::DIFile_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::DIFile_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::DIFile_O>>::allocateClass();
        llvmo::DIFile_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::DIFile_O::static_className() % (void*)(llvmo::DIFile_O::static_allocator) );
    classllvmo__DIFile_Oval->setCreator(llvmo::DIFile_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::DIFile_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__DIFile_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__DILexicalBlock_Oval]"));
    core::BuiltInClass_sp classllvmo__DILexicalBlock_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__DILexicalBlock_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__DILexicalBlock_Oval,_lisp,llvmo::DILexicalBlock_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::DILexicalBlock_O>::id,llvmo::DILexicalBlock_O::static_classSymbol());
    llvmo::DILexicalBlock_O::___staticClass = classllvmo__DILexicalBlock_Oval;
#ifdef USE_MPS
    llvmo::DILexicalBlock_O::static_Kind = gctools::GCKind<llvmo::DILexicalBlock_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__DILexicalBlock_Oval,llvmo::DILexicalBlock_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::DILexicalBlock_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::DILexicalBlock_O>>::allocateClass();
        llvmo::DILexicalBlock_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::DILexicalBlock_O::static_className() % (void*)(llvmo::DILexicalBlock_O::static_allocator) );
    classllvmo__DILexicalBlock_Oval->setCreator(llvmo::DILexicalBlock_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::DILexicalBlock_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__DILexicalBlock_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__DIScope_Oval]"));
    core::BuiltInClass_sp classllvmo__DIScope_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__DIScope_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__DIScope_Oval,_lisp,llvmo::DIScope_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::DIScope_O>::id,llvmo::DIScope_O::static_classSymbol());
    llvmo::DIScope_O::___staticClass = classllvmo__DIScope_Oval;
#ifdef USE_MPS
    llvmo::DIScope_O::static_Kind = gctools::GCKind<llvmo::DIScope_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__DIScope_Oval,llvmo::DIScope_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::DIScope_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::DIScope_O>>::allocateClass();
        llvmo::DIScope_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::DIScope_O::static_className() % (void*)(llvmo::DIScope_O::static_allocator) );
    classllvmo__DIScope_Oval->setCreator(llvmo::DIScope_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::DIScope_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__DIScope_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__DISubprogram_Oval]"));
    core::BuiltInClass_sp classllvmo__DISubprogram_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__DISubprogram_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__DISubprogram_Oval,_lisp,llvmo::DISubprogram_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::DISubprogram_O>::id,llvmo::DISubprogram_O::static_classSymbol());
    llvmo::DISubprogram_O::___staticClass = classllvmo__DISubprogram_Oval;
#ifdef USE_MPS
    llvmo::DISubprogram_O::static_Kind = gctools::GCKind<llvmo::DISubprogram_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__DISubprogram_Oval,llvmo::DISubprogram_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::DISubprogram_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::DISubprogram_O>>::allocateClass();
        llvmo::DISubprogram_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::DISubprogram_O::static_className() % (void*)(llvmo::DISubprogram_O::static_allocator) );
    classllvmo__DISubprogram_Oval->setCreator(llvmo::DISubprogram_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::DISubprogram_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__DISubprogram_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__DISubroutineType_Oval]"));
    core::BuiltInClass_sp classllvmo__DISubroutineType_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__DISubroutineType_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__DISubroutineType_Oval,_lisp,llvmo::DISubroutineType_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::DISubroutineType_O>::id,llvmo::DISubroutineType_O::static_classSymbol());
    llvmo::DISubroutineType_O::___staticClass = classllvmo__DISubroutineType_Oval;
#ifdef USE_MPS
    llvmo::DISubroutineType_O::static_Kind = gctools::GCKind<llvmo::DISubroutineType_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__DISubroutineType_Oval,llvmo::DISubroutineType_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::DISubroutineType_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::DISubroutineType_O>>::allocateClass();
        llvmo::DISubroutineType_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::DISubroutineType_O::static_className() % (void*)(llvmo::DISubroutineType_O::static_allocator) );
    classllvmo__DISubroutineType_Oval->setCreator(llvmo::DISubroutineType_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::DISubroutineType_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__DISubroutineType_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__DITypeArray_Oval]"));
    core::BuiltInClass_sp classllvmo__DITypeArray_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__DITypeArray_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__DITypeArray_Oval,_lisp,llvmo::DITypeArray_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::DITypeArray_O>::id,llvmo::DITypeArray_O::static_classSymbol());
    llvmo::DITypeArray_O::___staticClass = classllvmo__DITypeArray_Oval;
#ifdef USE_MPS
    llvmo::DITypeArray_O::static_Kind = gctools::GCKind<llvmo::DITypeArray_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__DITypeArray_Oval,llvmo::DITypeArray_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::DITypeArray_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::DITypeArray_O>>::allocateClass();
        llvmo::DITypeArray_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::DITypeArray_O::static_className() % (void*)(llvmo::DITypeArray_O::static_allocator) );
    classllvmo__DITypeArray_Oval->setCreator(llvmo::DITypeArray_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::DITypeArray_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__DITypeArray_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__DIType_Oval]"));
    core::BuiltInClass_sp classllvmo__DIType_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__DIType_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__DIType_Oval,_lisp,llvmo::DIType_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::DIType_O>::id,llvmo::DIType_O::static_classSymbol());
    llvmo::DIType_O::___staticClass = classllvmo__DIType_Oval;
#ifdef USE_MPS
    llvmo::DIType_O::static_Kind = gctools::GCKind<llvmo::DIType_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__DIType_Oval,llvmo::DIType_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::DIType_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::DIType_O>>::allocateClass();
        llvmo::DIType_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::DIType_O::static_className() % (void*)(llvmo::DIType_O::static_allocator) );
    classllvmo__DIType_Oval->setCreator(llvmo::DIType_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::DIType_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__DIType_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__FunctionPassManager_Oval]"));
    core::BuiltInClass_sp classllvmo__FunctionPassManager_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__FunctionPassManager_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__FunctionPassManager_Oval,_lisp,llvmo::FunctionPassManager_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::FunctionPassManager_O>::id,llvmo::FunctionPassManager_O::static_classSymbol());
    llvmo::FunctionPassManager_O::___staticClass = classllvmo__FunctionPassManager_Oval;
#ifdef USE_MPS
    llvmo::FunctionPassManager_O::static_Kind = gctools::GCKind<llvmo::FunctionPassManager_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__FunctionPassManager_Oval,llvmo::FunctionPassManager_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::FunctionPassManager_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::FunctionPassManager_O>>::allocateClass();
        llvmo::FunctionPassManager_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::FunctionPassManager_O::static_className() % (void*)(llvmo::FunctionPassManager_O::static_allocator) );
    classllvmo__FunctionPassManager_Oval->setCreator(llvmo::FunctionPassManager_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::FunctionPassManager_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__FunctionPassManager_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__FunctionPass_Oval]"));
    core::BuiltInClass_sp classllvmo__FunctionPass_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__FunctionPass_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__FunctionPass_Oval,_lisp,llvmo::FunctionPass_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::FunctionPass_O>::id,llvmo::FunctionPass_O::static_classSymbol());
    llvmo::FunctionPass_O::___staticClass = classllvmo__FunctionPass_Oval;
#ifdef USE_MPS
    llvmo::FunctionPass_O::static_Kind = gctools::GCKind<llvmo::FunctionPass_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__FunctionPass_Oval,llvmo::FunctionPass_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::FunctionPass_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::FunctionPass_O>>::allocateClass();
        llvmo::FunctionPass_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::FunctionPass_O::static_className() % (void*)(llvmo::FunctionPass_O::static_allocator) );
    classllvmo__FunctionPass_Oval->setCreator(llvmo::FunctionPass_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::FunctionPass_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__FunctionPass_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__FunctionType_Oval]"));
    core::BuiltInClass_sp classllvmo__FunctionType_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__FunctionType_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__FunctionType_Oval,_lisp,llvmo::FunctionType_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::FunctionType_O>::id,llvmo::FunctionType_O::static_classSymbol());
    llvmo::FunctionType_O::___staticClass = classllvmo__FunctionType_Oval;
#ifdef USE_MPS
    llvmo::FunctionType_O::static_Kind = gctools::GCKind<llvmo::FunctionType_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__FunctionType_Oval,llvmo::FunctionType_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::FunctionType_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::FunctionType_O>>::allocateClass();
        llvmo::FunctionType_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::FunctionType_O::static_className() % (void*)(llvmo::FunctionType_O::static_allocator) );
    classllvmo__FunctionType_Oval->setCreator(llvmo::FunctionType_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::FunctionType_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__FunctionType_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__IRBuilder_Oval]"));
    core::BuiltInClass_sp classllvmo__IRBuilder_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__IRBuilder_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__IRBuilder_Oval,_lisp,llvmo::IRBuilder_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::IRBuilder_O>::id,llvmo::IRBuilder_O::static_classSymbol());
    llvmo::IRBuilder_O::___staticClass = classllvmo__IRBuilder_Oval;
#ifdef USE_MPS
    llvmo::IRBuilder_O::static_Kind = gctools::GCKind<llvmo::IRBuilder_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__IRBuilder_Oval,llvmo::IRBuilder_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::IRBuilder_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::IRBuilder_O>>::allocateClass();
        llvmo::IRBuilder_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::IRBuilder_O::static_className() % (void*)(llvmo::IRBuilder_O::static_allocator) );
    classllvmo__IRBuilder_Oval->setCreator(llvmo::IRBuilder_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::IRBuilder_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__IRBuilder_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__IntegerType_Oval]"));
    core::BuiltInClass_sp classllvmo__IntegerType_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__IntegerType_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__IntegerType_Oval,_lisp,llvmo::IntegerType_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::IntegerType_O>::id,llvmo::IntegerType_O::static_classSymbol());
    llvmo::IntegerType_O::___staticClass = classllvmo__IntegerType_Oval;
#ifdef USE_MPS
    llvmo::IntegerType_O::static_Kind = gctools::GCKind<llvmo::IntegerType_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__IntegerType_Oval,llvmo::IntegerType_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::IntegerType_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::IntegerType_O>>::allocateClass();
        llvmo::IntegerType_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::IntegerType_O::static_className() % (void*)(llvmo::IntegerType_O::static_allocator) );
    classllvmo__IntegerType_Oval->setCreator(llvmo::IntegerType_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::IntegerType_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__IntegerType_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__LLVMTargetMachine_Oval]"));
    core::BuiltInClass_sp classllvmo__LLVMTargetMachine_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__LLVMTargetMachine_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__LLVMTargetMachine_Oval,_lisp,llvmo::LLVMTargetMachine_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::LLVMTargetMachine_O>::id,llvmo::LLVMTargetMachine_O::static_classSymbol());
    llvmo::LLVMTargetMachine_O::___staticClass = classllvmo__LLVMTargetMachine_Oval;
#ifdef USE_MPS
    llvmo::LLVMTargetMachine_O::static_Kind = gctools::GCKind<llvmo::LLVMTargetMachine_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__LLVMTargetMachine_Oval,llvmo::LLVMTargetMachine_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::LLVMTargetMachine_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::LLVMTargetMachine_O>>::allocateClass();
        llvmo::LLVMTargetMachine_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::LLVMTargetMachine_O::static_className() % (void*)(llvmo::LLVMTargetMachine_O::static_allocator) );
    classllvmo__LLVMTargetMachine_Oval->setCreator(llvmo::LLVMTargetMachine_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::LLVMTargetMachine_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__LLVMTargetMachine_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__MDNode_Oval]"));
    core::BuiltInClass_sp classllvmo__MDNode_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__MDNode_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__MDNode_Oval,_lisp,llvmo::MDNode_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::MDNode_O>::id,llvmo::MDNode_O::static_classSymbol());
    llvmo::MDNode_O::___staticClass = classllvmo__MDNode_Oval;
#ifdef USE_MPS
    llvmo::MDNode_O::static_Kind = gctools::GCKind<llvmo::MDNode_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__MDNode_Oval,llvmo::MDNode_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::MDNode_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::MDNode_O>>::allocateClass();
        llvmo::MDNode_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::MDNode_O::static_className() % (void*)(llvmo::MDNode_O::static_allocator) );
    classllvmo__MDNode_Oval->setCreator(llvmo::MDNode_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::MDNode_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__MDNode_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__MDString_Oval]"));
    core::BuiltInClass_sp classllvmo__MDString_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__MDString_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__MDString_Oval,_lisp,llvmo::MDString_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::MDString_O>::id,llvmo::MDString_O::static_classSymbol());
    llvmo::MDString_O::___staticClass = classllvmo__MDString_Oval;
#ifdef USE_MPS
    llvmo::MDString_O::static_Kind = gctools::GCKind<llvmo::MDString_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__MDString_Oval,llvmo::MDString_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::MDString_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::MDString_O>>::allocateClass();
        llvmo::MDString_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::MDString_O::static_className() % (void*)(llvmo::MDString_O::static_allocator) );
    classllvmo__MDString_Oval->setCreator(llvmo::MDString_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::MDString_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__MDString_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__ModulePass_Oval]"));
    core::BuiltInClass_sp classllvmo__ModulePass_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__ModulePass_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ModulePass_Oval,_lisp,llvmo::ModulePass_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::ModulePass_O>::id,llvmo::ModulePass_O::static_classSymbol());
    llvmo::ModulePass_O::___staticClass = classllvmo__ModulePass_Oval;
#ifdef USE_MPS
    llvmo::ModulePass_O::static_Kind = gctools::GCKind<llvmo::ModulePass_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__ModulePass_Oval,llvmo::ModulePass_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::ModulePass_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::ModulePass_O>>::allocateClass();
        llvmo::ModulePass_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::ModulePass_O::static_className() % (void*)(llvmo::ModulePass_O::static_allocator) );
    classllvmo__ModulePass_Oval->setCreator(llvmo::ModulePass_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::ModulePass_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__ModulePass_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__PassManager_Oval]"));
    core::BuiltInClass_sp classllvmo__PassManager_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__PassManager_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__PassManager_Oval,_lisp,llvmo::PassManager_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::PassManager_O>::id,llvmo::PassManager_O::static_classSymbol());
    llvmo::PassManager_O::___staticClass = classllvmo__PassManager_Oval;
#ifdef USE_MPS
    llvmo::PassManager_O::static_Kind = gctools::GCKind<llvmo::PassManager_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__PassManager_Oval,llvmo::PassManager_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::PassManager_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::PassManager_O>>::allocateClass();
        llvmo::PassManager_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::PassManager_O::static_className() % (void*)(llvmo::PassManager_O::static_allocator) );
    classllvmo__PassManager_Oval->setCreator(llvmo::PassManager_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::PassManager_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__PassManager_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__TargetSubtargetInfo_Oval]"));
    core::BuiltInClass_sp classllvmo__TargetSubtargetInfo_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__TargetSubtargetInfo_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__TargetSubtargetInfo_Oval,_lisp,llvmo::TargetSubtargetInfo_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::TargetSubtargetInfo_O>::id,llvmo::TargetSubtargetInfo_O::static_classSymbol());
    llvmo::TargetSubtargetInfo_O::___staticClass = classllvmo__TargetSubtargetInfo_Oval;
#ifdef USE_MPS
    llvmo::TargetSubtargetInfo_O::static_Kind = gctools::GCKind<llvmo::TargetSubtargetInfo_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__TargetSubtargetInfo_Oval,llvmo::TargetSubtargetInfo_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::TargetSubtargetInfo_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::TargetSubtargetInfo_O>>::allocateClass();
        llvmo::TargetSubtargetInfo_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::TargetSubtargetInfo_O::static_className() % (void*)(llvmo::TargetSubtargetInfo_O::static_allocator) );
    classllvmo__TargetSubtargetInfo_Oval->setCreator(llvmo::TargetSubtargetInfo_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::TargetSubtargetInfo_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__TargetSubtargetInfo_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__User_Oval]"));
    core::BuiltInClass_sp classllvmo__User_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__User_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__User_Oval,_lisp,llvmo::User_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::User_O>::id,llvmo::User_O::static_classSymbol());
    llvmo::User_O::___staticClass = classllvmo__User_Oval;
#ifdef USE_MPS
    llvmo::User_O::static_Kind = gctools::GCKind<llvmo::User_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__User_Oval,llvmo::User_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::User_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::User_O>>::allocateClass();
        llvmo::User_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::User_O::static_className() % (void*)(llvmo::User_O::static_allocator) );
    classllvmo__User_Oval->setCreator(llvmo::User_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::User_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__User_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__ValueAsMetadata_Oval]"));
    core::BuiltInClass_sp classllvmo__ValueAsMetadata_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__ValueAsMetadata_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ValueAsMetadata_Oval,_lisp,llvmo::ValueAsMetadata_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::ValueAsMetadata_O>::id,llvmo::ValueAsMetadata_O::static_classSymbol());
    llvmo::ValueAsMetadata_O::___staticClass = classllvmo__ValueAsMetadata_Oval;
#ifdef USE_MPS
    llvmo::ValueAsMetadata_O::static_Kind = gctools::GCKind<llvmo::ValueAsMetadata_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__ValueAsMetadata_Oval,llvmo::ValueAsMetadata_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::ValueAsMetadata_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::ValueAsMetadata_O>>::allocateClass();
        llvmo::ValueAsMetadata_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::ValueAsMetadata_O::static_className() % (void*)(llvmo::ValueAsMetadata_O::static_allocator) );
    classllvmo__ValueAsMetadata_Oval->setCreator(llvmo::ValueAsMetadata_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::ValueAsMetadata_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__ValueAsMetadata_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__Constant_Oval]"));
    core::BuiltInClass_sp classllvmo__Constant_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__Constant_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__Constant_Oval,_lisp,llvmo::Constant_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::Constant_O>::id,llvmo::Constant_O::static_classSymbol());
    llvmo::Constant_O::___staticClass = classllvmo__Constant_Oval;
#ifdef USE_MPS
    llvmo::Constant_O::static_Kind = gctools::GCKind<llvmo::Constant_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__Constant_Oval,llvmo::Constant_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::Constant_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::Constant_O>>::allocateClass();
        llvmo::Constant_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::Constant_O::static_className() % (void*)(llvmo::Constant_O::static_allocator) );
    classllvmo__Constant_Oval->setCreator(llvmo::Constant_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::Constant_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__Constant_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__ImmutablePass_Oval]"));
    core::BuiltInClass_sp classllvmo__ImmutablePass_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__ImmutablePass_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ImmutablePass_Oval,_lisp,llvmo::ImmutablePass_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::ImmutablePass_O>::id,llvmo::ImmutablePass_O::static_classSymbol());
    llvmo::ImmutablePass_O::___staticClass = classllvmo__ImmutablePass_Oval;
#ifdef USE_MPS
    llvmo::ImmutablePass_O::static_Kind = gctools::GCKind<llvmo::ImmutablePass_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__ImmutablePass_Oval,llvmo::ImmutablePass_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::ImmutablePass_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::ImmutablePass_O>>::allocateClass();
        llvmo::ImmutablePass_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::ImmutablePass_O::static_className() % (void*)(llvmo::ImmutablePass_O::static_allocator) );
    classllvmo__ImmutablePass_Oval->setCreator(llvmo::ImmutablePass_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::ImmutablePass_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__ImmutablePass_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__Instruction_Oval]"));
    core::BuiltInClass_sp classllvmo__Instruction_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__Instruction_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__Instruction_Oval,_lisp,llvmo::Instruction_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::Instruction_O>::id,llvmo::Instruction_O::static_classSymbol());
    llvmo::Instruction_O::___staticClass = classllvmo__Instruction_Oval;
#ifdef USE_MPS
    llvmo::Instruction_O::static_Kind = gctools::GCKind<llvmo::Instruction_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__Instruction_Oval,llvmo::Instruction_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::Instruction_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::Instruction_O>>::allocateClass();
        llvmo::Instruction_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::Instruction_O::static_className() % (void*)(llvmo::Instruction_O::static_allocator) );
    classllvmo__Instruction_Oval->setCreator(llvmo::Instruction_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::Instruction_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__Instruction_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__SequentialType_Oval]"));
    core::BuiltInClass_sp classllvmo__SequentialType_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__SequentialType_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__SequentialType_Oval,_lisp,llvmo::SequentialType_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::SequentialType_O>::id,llvmo::SequentialType_O::static_classSymbol());
    llvmo::SequentialType_O::___staticClass = classllvmo__SequentialType_Oval;
#ifdef USE_MPS
    llvmo::SequentialType_O::static_Kind = gctools::GCKind<llvmo::SequentialType_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__SequentialType_Oval,llvmo::SequentialType_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::SequentialType_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::SequentialType_O>>::allocateClass();
        llvmo::SequentialType_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::SequentialType_O::static_className() % (void*)(llvmo::SequentialType_O::static_allocator) );
    classllvmo__SequentialType_Oval->setCreator(llvmo::SequentialType_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::SequentialType_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__SequentialType_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__StructType_Oval]"));
    core::BuiltInClass_sp classllvmo__StructType_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__StructType_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__StructType_Oval,_lisp,llvmo::StructType_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::StructType_O>::id,llvmo::StructType_O::static_classSymbol());
    llvmo::StructType_O::___staticClass = classllvmo__StructType_Oval;
#ifdef USE_MPS
    llvmo::StructType_O::static_Kind = gctools::GCKind<llvmo::StructType_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__StructType_Oval,llvmo::StructType_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::StructType_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::StructType_O>>::allocateClass();
        llvmo::StructType_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::StructType_O::static_className() % (void*)(llvmo::StructType_O::static_allocator) );
    classllvmo__StructType_Oval->setCreator(llvmo::StructType_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::StructType_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__StructType_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__ArrayType_Oval]"));
    core::BuiltInClass_sp classllvmo__ArrayType_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__ArrayType_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ArrayType_Oval,_lisp,llvmo::ArrayType_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::ArrayType_O>::id,llvmo::ArrayType_O::static_classSymbol());
    llvmo::ArrayType_O::___staticClass = classllvmo__ArrayType_Oval;
#ifdef USE_MPS
    llvmo::ArrayType_O::static_Kind = gctools::GCKind<llvmo::ArrayType_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__ArrayType_Oval,llvmo::ArrayType_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::ArrayType_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::ArrayType_O>>::allocateClass();
        llvmo::ArrayType_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::ArrayType_O::static_className() % (void*)(llvmo::ArrayType_O::static_allocator) );
    classllvmo__ArrayType_Oval->setCreator(llvmo::ArrayType_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::ArrayType_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__ArrayType_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__AtomicCmpXchgInst_Oval]"));
    core::BuiltInClass_sp classllvmo__AtomicCmpXchgInst_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__AtomicCmpXchgInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__AtomicCmpXchgInst_Oval,_lisp,llvmo::AtomicCmpXchgInst_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::AtomicCmpXchgInst_O>::id,llvmo::AtomicCmpXchgInst_O::static_classSymbol());
    llvmo::AtomicCmpXchgInst_O::___staticClass = classllvmo__AtomicCmpXchgInst_Oval;
#ifdef USE_MPS
    llvmo::AtomicCmpXchgInst_O::static_Kind = gctools::GCKind<llvmo::AtomicCmpXchgInst_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__AtomicCmpXchgInst_Oval,llvmo::AtomicCmpXchgInst_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::AtomicCmpXchgInst_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::AtomicCmpXchgInst_O>>::allocateClass();
        llvmo::AtomicCmpXchgInst_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::AtomicCmpXchgInst_O::static_className() % (void*)(llvmo::AtomicCmpXchgInst_O::static_allocator) );
    classllvmo__AtomicCmpXchgInst_Oval->setCreator(llvmo::AtomicCmpXchgInst_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::AtomicCmpXchgInst_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__AtomicCmpXchgInst_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__AtomicRMWInst_Oval]"));
    core::BuiltInClass_sp classllvmo__AtomicRMWInst_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__AtomicRMWInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__AtomicRMWInst_Oval,_lisp,llvmo::AtomicRMWInst_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::AtomicRMWInst_O>::id,llvmo::AtomicRMWInst_O::static_classSymbol());
    llvmo::AtomicRMWInst_O::___staticClass = classllvmo__AtomicRMWInst_Oval;
#ifdef USE_MPS
    llvmo::AtomicRMWInst_O::static_Kind = gctools::GCKind<llvmo::AtomicRMWInst_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__AtomicRMWInst_Oval,llvmo::AtomicRMWInst_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::AtomicRMWInst_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::AtomicRMWInst_O>>::allocateClass();
        llvmo::AtomicRMWInst_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::AtomicRMWInst_O::static_className() % (void*)(llvmo::AtomicRMWInst_O::static_allocator) );
    classllvmo__AtomicRMWInst_Oval->setCreator(llvmo::AtomicRMWInst_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::AtomicRMWInst_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__AtomicRMWInst_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__BlockAddress_Oval]"));
    core::BuiltInClass_sp classllvmo__BlockAddress_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__BlockAddress_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__BlockAddress_Oval,_lisp,llvmo::BlockAddress_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::BlockAddress_O>::id,llvmo::BlockAddress_O::static_classSymbol());
    llvmo::BlockAddress_O::___staticClass = classllvmo__BlockAddress_Oval;
#ifdef USE_MPS
    llvmo::BlockAddress_O::static_Kind = gctools::GCKind<llvmo::BlockAddress_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__BlockAddress_Oval,llvmo::BlockAddress_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::BlockAddress_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::BlockAddress_O>>::allocateClass();
        llvmo::BlockAddress_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::BlockAddress_O::static_className() % (void*)(llvmo::BlockAddress_O::static_allocator) );
    classllvmo__BlockAddress_Oval->setCreator(llvmo::BlockAddress_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::BlockAddress_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__BlockAddress_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__CallInst_Oval]"));
    core::BuiltInClass_sp classllvmo__CallInst_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__CallInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__CallInst_Oval,_lisp,llvmo::CallInst_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::CallInst_O>::id,llvmo::CallInst_O::static_classSymbol());
    llvmo::CallInst_O::___staticClass = classllvmo__CallInst_Oval;
#ifdef USE_MPS
    llvmo::CallInst_O::static_Kind = gctools::GCKind<llvmo::CallInst_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__CallInst_Oval,llvmo::CallInst_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::CallInst_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::CallInst_O>>::allocateClass();
        llvmo::CallInst_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::CallInst_O::static_className() % (void*)(llvmo::CallInst_O::static_allocator) );
    classllvmo__CallInst_Oval->setCreator(llvmo::CallInst_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::CallInst_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__CallInst_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__ConstantArray_Oval]"));
    core::BuiltInClass_sp classllvmo__ConstantArray_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__ConstantArray_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ConstantArray_Oval,_lisp,llvmo::ConstantArray_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::ConstantArray_O>::id,llvmo::ConstantArray_O::static_classSymbol());
    llvmo::ConstantArray_O::___staticClass = classllvmo__ConstantArray_Oval;
#ifdef USE_MPS
    llvmo::ConstantArray_O::static_Kind = gctools::GCKind<llvmo::ConstantArray_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__ConstantArray_Oval,llvmo::ConstantArray_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::ConstantArray_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::ConstantArray_O>>::allocateClass();
        llvmo::ConstantArray_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::ConstantArray_O::static_className() % (void*)(llvmo::ConstantArray_O::static_allocator) );
    classllvmo__ConstantArray_Oval->setCreator(llvmo::ConstantArray_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::ConstantArray_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__ConstantArray_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__ConstantDataSequential_Oval]"));
    core::BuiltInClass_sp classllvmo__ConstantDataSequential_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__ConstantDataSequential_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ConstantDataSequential_Oval,_lisp,llvmo::ConstantDataSequential_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::ConstantDataSequential_O>::id,llvmo::ConstantDataSequential_O::static_classSymbol());
    llvmo::ConstantDataSequential_O::___staticClass = classllvmo__ConstantDataSequential_Oval;
#ifdef USE_MPS
    llvmo::ConstantDataSequential_O::static_Kind = gctools::GCKind<llvmo::ConstantDataSequential_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__ConstantDataSequential_Oval,llvmo::ConstantDataSequential_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::ConstantDataSequential_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::ConstantDataSequential_O>>::allocateClass();
        llvmo::ConstantDataSequential_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::ConstantDataSequential_O::static_className() % (void*)(llvmo::ConstantDataSequential_O::static_allocator) );
    classllvmo__ConstantDataSequential_Oval->setCreator(llvmo::ConstantDataSequential_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::ConstantDataSequential_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__ConstantDataSequential_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__ConstantExpr_Oval]"));
    core::BuiltInClass_sp classllvmo__ConstantExpr_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__ConstantExpr_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ConstantExpr_Oval,_lisp,llvmo::ConstantExpr_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::ConstantExpr_O>::id,llvmo::ConstantExpr_O::static_classSymbol());
    llvmo::ConstantExpr_O::___staticClass = classllvmo__ConstantExpr_Oval;
#ifdef USE_MPS
    llvmo::ConstantExpr_O::static_Kind = gctools::GCKind<llvmo::ConstantExpr_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__ConstantExpr_Oval,llvmo::ConstantExpr_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::ConstantExpr_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::ConstantExpr_O>>::allocateClass();
        llvmo::ConstantExpr_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::ConstantExpr_O::static_className() % (void*)(llvmo::ConstantExpr_O::static_allocator) );
    classllvmo__ConstantExpr_Oval->setCreator(llvmo::ConstantExpr_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::ConstantExpr_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__ConstantExpr_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__ConstantFP_Oval]"));
    core::BuiltInClass_sp classllvmo__ConstantFP_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__ConstantFP_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ConstantFP_Oval,_lisp,llvmo::ConstantFP_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::ConstantFP_O>::id,llvmo::ConstantFP_O::static_classSymbol());
    llvmo::ConstantFP_O::___staticClass = classllvmo__ConstantFP_Oval;
#ifdef USE_MPS
    llvmo::ConstantFP_O::static_Kind = gctools::GCKind<llvmo::ConstantFP_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__ConstantFP_Oval,llvmo::ConstantFP_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::ConstantFP_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::ConstantFP_O>>::allocateClass();
        llvmo::ConstantFP_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::ConstantFP_O::static_className() % (void*)(llvmo::ConstantFP_O::static_allocator) );
    classllvmo__ConstantFP_Oval->setCreator(llvmo::ConstantFP_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::ConstantFP_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__ConstantFP_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__ConstantInt_Oval]"));
    core::BuiltInClass_sp classllvmo__ConstantInt_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__ConstantInt_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ConstantInt_Oval,_lisp,llvmo::ConstantInt_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::ConstantInt_O>::id,llvmo::ConstantInt_O::static_classSymbol());
    llvmo::ConstantInt_O::___staticClass = classllvmo__ConstantInt_Oval;
#ifdef USE_MPS
    llvmo::ConstantInt_O::static_Kind = gctools::GCKind<llvmo::ConstantInt_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__ConstantInt_Oval,llvmo::ConstantInt_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::ConstantInt_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::ConstantInt_O>>::allocateClass();
        llvmo::ConstantInt_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::ConstantInt_O::static_className() % (void*)(llvmo::ConstantInt_O::static_allocator) );
    classllvmo__ConstantInt_Oval->setCreator(llvmo::ConstantInt_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::ConstantInt_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__ConstantInt_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__ConstantPointerNull_Oval]"));
    core::BuiltInClass_sp classllvmo__ConstantPointerNull_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__ConstantPointerNull_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ConstantPointerNull_Oval,_lisp,llvmo::ConstantPointerNull_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::ConstantPointerNull_O>::id,llvmo::ConstantPointerNull_O::static_classSymbol());
    llvmo::ConstantPointerNull_O::___staticClass = classllvmo__ConstantPointerNull_Oval;
#ifdef USE_MPS
    llvmo::ConstantPointerNull_O::static_Kind = gctools::GCKind<llvmo::ConstantPointerNull_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__ConstantPointerNull_Oval,llvmo::ConstantPointerNull_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::ConstantPointerNull_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::ConstantPointerNull_O>>::allocateClass();
        llvmo::ConstantPointerNull_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::ConstantPointerNull_O::static_className() % (void*)(llvmo::ConstantPointerNull_O::static_allocator) );
    classllvmo__ConstantPointerNull_Oval->setCreator(llvmo::ConstantPointerNull_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::ConstantPointerNull_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__ConstantPointerNull_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__ConstantStruct_Oval]"));
    core::BuiltInClass_sp classllvmo__ConstantStruct_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__ConstantStruct_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ConstantStruct_Oval,_lisp,llvmo::ConstantStruct_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::ConstantStruct_O>::id,llvmo::ConstantStruct_O::static_classSymbol());
    llvmo::ConstantStruct_O::___staticClass = classllvmo__ConstantStruct_Oval;
#ifdef USE_MPS
    llvmo::ConstantStruct_O::static_Kind = gctools::GCKind<llvmo::ConstantStruct_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__ConstantStruct_Oval,llvmo::ConstantStruct_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::ConstantStruct_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::ConstantStruct_O>>::allocateClass();
        llvmo::ConstantStruct_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::ConstantStruct_O::static_className() % (void*)(llvmo::ConstantStruct_O::static_allocator) );
    classllvmo__ConstantStruct_Oval->setCreator(llvmo::ConstantStruct_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::ConstantStruct_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__ConstantStruct_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__DataLayoutPass_Oval]"));
    core::BuiltInClass_sp classllvmo__DataLayoutPass_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__DataLayoutPass_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__DataLayoutPass_Oval,_lisp,llvmo::DataLayoutPass_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::DataLayoutPass_O>::id,llvmo::DataLayoutPass_O::static_classSymbol());
    llvmo::DataLayoutPass_O::___staticClass = classllvmo__DataLayoutPass_Oval;
#ifdef USE_MPS
    llvmo::DataLayoutPass_O::static_Kind = gctools::GCKind<llvmo::DataLayoutPass_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__DataLayoutPass_Oval,llvmo::DataLayoutPass_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::DataLayoutPass_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::DataLayoutPass_O>>::allocateClass();
        llvmo::DataLayoutPass_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::DataLayoutPass_O::static_className() % (void*)(llvmo::DataLayoutPass_O::static_allocator) );
    classllvmo__DataLayoutPass_Oval->setCreator(llvmo::DataLayoutPass_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::DataLayoutPass_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__DataLayoutPass_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__FenceInst_Oval]"));
    core::BuiltInClass_sp classllvmo__FenceInst_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__FenceInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__FenceInst_Oval,_lisp,llvmo::FenceInst_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::FenceInst_O>::id,llvmo::FenceInst_O::static_classSymbol());
    llvmo::FenceInst_O::___staticClass = classllvmo__FenceInst_Oval;
#ifdef USE_MPS
    llvmo::FenceInst_O::static_Kind = gctools::GCKind<llvmo::FenceInst_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__FenceInst_Oval,llvmo::FenceInst_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::FenceInst_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::FenceInst_O>>::allocateClass();
        llvmo::FenceInst_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::FenceInst_O::static_className() % (void*)(llvmo::FenceInst_O::static_allocator) );
    classllvmo__FenceInst_Oval->setCreator(llvmo::FenceInst_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::FenceInst_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__FenceInst_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__GlobalValue_Oval]"));
    core::BuiltInClass_sp classllvmo__GlobalValue_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__GlobalValue_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__GlobalValue_Oval,_lisp,llvmo::GlobalValue_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::GlobalValue_O>::id,llvmo::GlobalValue_O::static_classSymbol());
    llvmo::GlobalValue_O::___staticClass = classllvmo__GlobalValue_Oval;
#ifdef USE_MPS
    llvmo::GlobalValue_O::static_Kind = gctools::GCKind<llvmo::GlobalValue_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__GlobalValue_Oval,llvmo::GlobalValue_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::GlobalValue_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::GlobalValue_O>>::allocateClass();
        llvmo::GlobalValue_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::GlobalValue_O::static_className() % (void*)(llvmo::GlobalValue_O::static_allocator) );
    classllvmo__GlobalValue_Oval->setCreator(llvmo::GlobalValue_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::GlobalValue_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__GlobalValue_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__LandingPadInst_Oval]"));
    core::BuiltInClass_sp classllvmo__LandingPadInst_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__LandingPadInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__LandingPadInst_Oval,_lisp,llvmo::LandingPadInst_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::LandingPadInst_O>::id,llvmo::LandingPadInst_O::static_classSymbol());
    llvmo::LandingPadInst_O::___staticClass = classllvmo__LandingPadInst_Oval;
#ifdef USE_MPS
    llvmo::LandingPadInst_O::static_Kind = gctools::GCKind<llvmo::LandingPadInst_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__LandingPadInst_Oval,llvmo::LandingPadInst_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::LandingPadInst_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::LandingPadInst_O>>::allocateClass();
        llvmo::LandingPadInst_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::LandingPadInst_O::static_className() % (void*)(llvmo::LandingPadInst_O::static_allocator) );
    classllvmo__LandingPadInst_Oval->setCreator(llvmo::LandingPadInst_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::LandingPadInst_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__LandingPadInst_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__PHINode_Oval]"));
    core::BuiltInClass_sp classllvmo__PHINode_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__PHINode_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__PHINode_Oval,_lisp,llvmo::PHINode_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::PHINode_O>::id,llvmo::PHINode_O::static_classSymbol());
    llvmo::PHINode_O::___staticClass = classllvmo__PHINode_Oval;
#ifdef USE_MPS
    llvmo::PHINode_O::static_Kind = gctools::GCKind<llvmo::PHINode_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__PHINode_Oval,llvmo::PHINode_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::PHINode_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::PHINode_O>>::allocateClass();
        llvmo::PHINode_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::PHINode_O::static_className() % (void*)(llvmo::PHINode_O::static_allocator) );
    classllvmo__PHINode_Oval->setCreator(llvmo::PHINode_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::PHINode_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__PHINode_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__PointerType_Oval]"));
    core::BuiltInClass_sp classllvmo__PointerType_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__PointerType_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__PointerType_Oval,_lisp,llvmo::PointerType_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::PointerType_O>::id,llvmo::PointerType_O::static_classSymbol());
    llvmo::PointerType_O::___staticClass = classllvmo__PointerType_Oval;
#ifdef USE_MPS
    llvmo::PointerType_O::static_Kind = gctools::GCKind<llvmo::PointerType_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__PointerType_Oval,llvmo::PointerType_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::PointerType_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::PointerType_O>>::allocateClass();
        llvmo::PointerType_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::PointerType_O::static_className() % (void*)(llvmo::PointerType_O::static_allocator) );
    classllvmo__PointerType_Oval->setCreator(llvmo::PointerType_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::PointerType_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__PointerType_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__StoreInst_Oval]"));
    core::BuiltInClass_sp classllvmo__StoreInst_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__StoreInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__StoreInst_Oval,_lisp,llvmo::StoreInst_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::StoreInst_O>::id,llvmo::StoreInst_O::static_classSymbol());
    llvmo::StoreInst_O::___staticClass = classllvmo__StoreInst_Oval;
#ifdef USE_MPS
    llvmo::StoreInst_O::static_Kind = gctools::GCKind<llvmo::StoreInst_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__StoreInst_Oval,llvmo::StoreInst_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::StoreInst_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::StoreInst_O>>::allocateClass();
        llvmo::StoreInst_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::StoreInst_O::static_className() % (void*)(llvmo::StoreInst_O::static_allocator) );
    classllvmo__StoreInst_Oval->setCreator(llvmo::StoreInst_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::StoreInst_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__StoreInst_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__TargetLibraryInfo_Oval]"));
    core::BuiltInClass_sp classllvmo__TargetLibraryInfo_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__TargetLibraryInfo_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__TargetLibraryInfo_Oval,_lisp,llvmo::TargetLibraryInfo_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::TargetLibraryInfo_O>::id,llvmo::TargetLibraryInfo_O::static_classSymbol());
    llvmo::TargetLibraryInfo_O::___staticClass = classllvmo__TargetLibraryInfo_Oval;
#ifdef USE_MPS
    llvmo::TargetLibraryInfo_O::static_Kind = gctools::GCKind<llvmo::TargetLibraryInfo_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__TargetLibraryInfo_Oval,llvmo::TargetLibraryInfo_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::TargetLibraryInfo_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::TargetLibraryInfo_O>>::allocateClass();
        llvmo::TargetLibraryInfo_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::TargetLibraryInfo_O::static_className() % (void*)(llvmo::TargetLibraryInfo_O::static_allocator) );
    classllvmo__TargetLibraryInfo_Oval->setCreator(llvmo::TargetLibraryInfo_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::TargetLibraryInfo_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__TargetLibraryInfo_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__TerminatorInst_Oval]"));
    core::BuiltInClass_sp classllvmo__TerminatorInst_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__TerminatorInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__TerminatorInst_Oval,_lisp,llvmo::TerminatorInst_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::TerminatorInst_O>::id,llvmo::TerminatorInst_O::static_classSymbol());
    llvmo::TerminatorInst_O::___staticClass = classllvmo__TerminatorInst_Oval;
#ifdef USE_MPS
    llvmo::TerminatorInst_O::static_Kind = gctools::GCKind<llvmo::TerminatorInst_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__TerminatorInst_Oval,llvmo::TerminatorInst_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::TerminatorInst_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::TerminatorInst_O>>::allocateClass();
        llvmo::TerminatorInst_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::TerminatorInst_O::static_className() % (void*)(llvmo::TerminatorInst_O::static_allocator) );
    classllvmo__TerminatorInst_Oval->setCreator(llvmo::TerminatorInst_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::TerminatorInst_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__TerminatorInst_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__UnaryInstruction_Oval]"));
    core::BuiltInClass_sp classllvmo__UnaryInstruction_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__UnaryInstruction_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__UnaryInstruction_Oval,_lisp,llvmo::UnaryInstruction_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::UnaryInstruction_O>::id,llvmo::UnaryInstruction_O::static_classSymbol());
    llvmo::UnaryInstruction_O::___staticClass = classllvmo__UnaryInstruction_Oval;
#ifdef USE_MPS
    llvmo::UnaryInstruction_O::static_Kind = gctools::GCKind<llvmo::UnaryInstruction_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__UnaryInstruction_Oval,llvmo::UnaryInstruction_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::UnaryInstruction_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::UnaryInstruction_O>>::allocateClass();
        llvmo::UnaryInstruction_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::UnaryInstruction_O::static_className() % (void*)(llvmo::UnaryInstruction_O::static_allocator) );
    classllvmo__UnaryInstruction_Oval->setCreator(llvmo::UnaryInstruction_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::UnaryInstruction_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__UnaryInstruction_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__UndefValue_Oval]"));
    core::BuiltInClass_sp classllvmo__UndefValue_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__UndefValue_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__UndefValue_Oval,_lisp,llvmo::UndefValue_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::UndefValue_O>::id,llvmo::UndefValue_O::static_classSymbol());
    llvmo::UndefValue_O::___staticClass = classllvmo__UndefValue_Oval;
#ifdef USE_MPS
    llvmo::UndefValue_O::static_Kind = gctools::GCKind<llvmo::UndefValue_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__UndefValue_Oval,llvmo::UndefValue_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::UndefValue_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::UndefValue_O>>::allocateClass();
        llvmo::UndefValue_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::UndefValue_O::static_className() % (void*)(llvmo::UndefValue_O::static_allocator) );
    classllvmo__UndefValue_Oval->setCreator(llvmo::UndefValue_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::UndefValue_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__UndefValue_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__VectorType_Oval]"));
    core::BuiltInClass_sp classllvmo__VectorType_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__VectorType_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__VectorType_Oval,_lisp,llvmo::VectorType_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::VectorType_O>::id,llvmo::VectorType_O::static_classSymbol());
    llvmo::VectorType_O::___staticClass = classllvmo__VectorType_Oval;
#ifdef USE_MPS
    llvmo::VectorType_O::static_Kind = gctools::GCKind<llvmo::VectorType_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__VectorType_Oval,llvmo::VectorType_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::VectorType_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::VectorType_O>>::allocateClass();
        llvmo::VectorType_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::VectorType_O::static_className() % (void*)(llvmo::VectorType_O::static_allocator) );
    classllvmo__VectorType_Oval->setCreator(llvmo::VectorType_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::VectorType_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__VectorType_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__AllocaInst_Oval]"));
    core::BuiltInClass_sp classllvmo__AllocaInst_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__AllocaInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__AllocaInst_Oval,_lisp,llvmo::AllocaInst_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::AllocaInst_O>::id,llvmo::AllocaInst_O::static_classSymbol());
    llvmo::AllocaInst_O::___staticClass = classllvmo__AllocaInst_Oval;
#ifdef USE_MPS
    llvmo::AllocaInst_O::static_Kind = gctools::GCKind<llvmo::AllocaInst_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__AllocaInst_Oval,llvmo::AllocaInst_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::AllocaInst_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::AllocaInst_O>>::allocateClass();
        llvmo::AllocaInst_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::AllocaInst_O::static_className() % (void*)(llvmo::AllocaInst_O::static_allocator) );
    classllvmo__AllocaInst_Oval->setCreator(llvmo::AllocaInst_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::AllocaInst_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__AllocaInst_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__BranchInst_Oval]"));
    core::BuiltInClass_sp classllvmo__BranchInst_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__BranchInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__BranchInst_Oval,_lisp,llvmo::BranchInst_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::BranchInst_O>::id,llvmo::BranchInst_O::static_classSymbol());
    llvmo::BranchInst_O::___staticClass = classllvmo__BranchInst_Oval;
#ifdef USE_MPS
    llvmo::BranchInst_O::static_Kind = gctools::GCKind<llvmo::BranchInst_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__BranchInst_Oval,llvmo::BranchInst_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::BranchInst_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::BranchInst_O>>::allocateClass();
        llvmo::BranchInst_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::BranchInst_O::static_className() % (void*)(llvmo::BranchInst_O::static_allocator) );
    classllvmo__BranchInst_Oval->setCreator(llvmo::BranchInst_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::BranchInst_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__BranchInst_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__ConstantDataArray_Oval]"));
    core::BuiltInClass_sp classllvmo__ConstantDataArray_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__ConstantDataArray_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ConstantDataArray_Oval,_lisp,llvmo::ConstantDataArray_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::ConstantDataArray_O>::id,llvmo::ConstantDataArray_O::static_classSymbol());
    llvmo::ConstantDataArray_O::___staticClass = classllvmo__ConstantDataArray_Oval;
#ifdef USE_MPS
    llvmo::ConstantDataArray_O::static_Kind = gctools::GCKind<llvmo::ConstantDataArray_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__ConstantDataArray_Oval,llvmo::ConstantDataArray_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::ConstantDataArray_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::ConstantDataArray_O>>::allocateClass();
        llvmo::ConstantDataArray_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::ConstantDataArray_O::static_className() % (void*)(llvmo::ConstantDataArray_O::static_allocator) );
    classllvmo__ConstantDataArray_Oval->setCreator(llvmo::ConstantDataArray_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::ConstantDataArray_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__ConstantDataArray_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__Function_Oval]"));
    core::BuiltInClass_sp classllvmo__Function_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__Function_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__Function_Oval,_lisp,llvmo::Function_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::Function_O>::id,llvmo::Function_O::static_classSymbol());
    llvmo::Function_O::___staticClass = classllvmo__Function_Oval;
#ifdef USE_MPS
    llvmo::Function_O::static_Kind = gctools::GCKind<llvmo::Function_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__Function_Oval,llvmo::Function_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::Function_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::Function_O>>::allocateClass();
        llvmo::Function_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::Function_O::static_className() % (void*)(llvmo::Function_O::static_allocator) );
    classllvmo__Function_Oval->setCreator(llvmo::Function_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::Function_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__Function_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__GlobalVariable_Oval]"));
    core::BuiltInClass_sp classllvmo__GlobalVariable_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__GlobalVariable_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__GlobalVariable_Oval,_lisp,llvmo::GlobalVariable_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::GlobalVariable_O>::id,llvmo::GlobalVariable_O::static_classSymbol());
    llvmo::GlobalVariable_O::___staticClass = classllvmo__GlobalVariable_Oval;
#ifdef USE_MPS
    llvmo::GlobalVariable_O::static_Kind = gctools::GCKind<llvmo::GlobalVariable_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__GlobalVariable_Oval,llvmo::GlobalVariable_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::GlobalVariable_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::GlobalVariable_O>>::allocateClass();
        llvmo::GlobalVariable_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::GlobalVariable_O::static_className() % (void*)(llvmo::GlobalVariable_O::static_allocator) );
    classllvmo__GlobalVariable_Oval->setCreator(llvmo::GlobalVariable_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::GlobalVariable_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__GlobalVariable_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__IndirectBrInst_Oval]"));
    core::BuiltInClass_sp classllvmo__IndirectBrInst_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__IndirectBrInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__IndirectBrInst_Oval,_lisp,llvmo::IndirectBrInst_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::IndirectBrInst_O>::id,llvmo::IndirectBrInst_O::static_classSymbol());
    llvmo::IndirectBrInst_O::___staticClass = classllvmo__IndirectBrInst_Oval;
#ifdef USE_MPS
    llvmo::IndirectBrInst_O::static_Kind = gctools::GCKind<llvmo::IndirectBrInst_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__IndirectBrInst_Oval,llvmo::IndirectBrInst_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::IndirectBrInst_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::IndirectBrInst_O>>::allocateClass();
        llvmo::IndirectBrInst_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::IndirectBrInst_O::static_className() % (void*)(llvmo::IndirectBrInst_O::static_allocator) );
    classllvmo__IndirectBrInst_Oval->setCreator(llvmo::IndirectBrInst_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::IndirectBrInst_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__IndirectBrInst_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__InvokeInst_Oval]"));
    core::BuiltInClass_sp classllvmo__InvokeInst_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__InvokeInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__InvokeInst_Oval,_lisp,llvmo::InvokeInst_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::InvokeInst_O>::id,llvmo::InvokeInst_O::static_classSymbol());
    llvmo::InvokeInst_O::___staticClass = classllvmo__InvokeInst_Oval;
#ifdef USE_MPS
    llvmo::InvokeInst_O::static_Kind = gctools::GCKind<llvmo::InvokeInst_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__InvokeInst_Oval,llvmo::InvokeInst_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::InvokeInst_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::InvokeInst_O>>::allocateClass();
        llvmo::InvokeInst_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::InvokeInst_O::static_className() % (void*)(llvmo::InvokeInst_O::static_allocator) );
    classllvmo__InvokeInst_Oval->setCreator(llvmo::InvokeInst_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::InvokeInst_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__InvokeInst_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__LoadInst_Oval]"));
    core::BuiltInClass_sp classllvmo__LoadInst_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__LoadInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__LoadInst_Oval,_lisp,llvmo::LoadInst_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::LoadInst_O>::id,llvmo::LoadInst_O::static_classSymbol());
    llvmo::LoadInst_O::___staticClass = classllvmo__LoadInst_Oval;
#ifdef USE_MPS
    llvmo::LoadInst_O::static_Kind = gctools::GCKind<llvmo::LoadInst_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__LoadInst_Oval,llvmo::LoadInst_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::LoadInst_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::LoadInst_O>>::allocateClass();
        llvmo::LoadInst_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::LoadInst_O::static_className() % (void*)(llvmo::LoadInst_O::static_allocator) );
    classllvmo__LoadInst_Oval->setCreator(llvmo::LoadInst_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::LoadInst_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__LoadInst_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__ResumeInst_Oval]"));
    core::BuiltInClass_sp classllvmo__ResumeInst_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__ResumeInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ResumeInst_Oval,_lisp,llvmo::ResumeInst_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::ResumeInst_O>::id,llvmo::ResumeInst_O::static_classSymbol());
    llvmo::ResumeInst_O::___staticClass = classllvmo__ResumeInst_Oval;
#ifdef USE_MPS
    llvmo::ResumeInst_O::static_Kind = gctools::GCKind<llvmo::ResumeInst_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__ResumeInst_Oval,llvmo::ResumeInst_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::ResumeInst_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::ResumeInst_O>>::allocateClass();
        llvmo::ResumeInst_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::ResumeInst_O::static_className() % (void*)(llvmo::ResumeInst_O::static_allocator) );
    classllvmo__ResumeInst_Oval->setCreator(llvmo::ResumeInst_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::ResumeInst_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__ResumeInst_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__ReturnInst_Oval]"));
    core::BuiltInClass_sp classllvmo__ReturnInst_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__ReturnInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ReturnInst_Oval,_lisp,llvmo::ReturnInst_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::ReturnInst_O>::id,llvmo::ReturnInst_O::static_classSymbol());
    llvmo::ReturnInst_O::___staticClass = classllvmo__ReturnInst_Oval;
#ifdef USE_MPS
    llvmo::ReturnInst_O::static_Kind = gctools::GCKind<llvmo::ReturnInst_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__ReturnInst_Oval,llvmo::ReturnInst_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::ReturnInst_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::ReturnInst_O>>::allocateClass();
        llvmo::ReturnInst_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::ReturnInst_O::static_className() % (void*)(llvmo::ReturnInst_O::static_allocator) );
    classllvmo__ReturnInst_Oval->setCreator(llvmo::ReturnInst_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::ReturnInst_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__ReturnInst_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__SwitchInst_Oval]"));
    core::BuiltInClass_sp classllvmo__SwitchInst_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__SwitchInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__SwitchInst_Oval,_lisp,llvmo::SwitchInst_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::SwitchInst_O>::id,llvmo::SwitchInst_O::static_classSymbol());
    llvmo::SwitchInst_O::___staticClass = classllvmo__SwitchInst_Oval;
#ifdef USE_MPS
    llvmo::SwitchInst_O::static_Kind = gctools::GCKind<llvmo::SwitchInst_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__SwitchInst_Oval,llvmo::SwitchInst_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::SwitchInst_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::SwitchInst_O>>::allocateClass();
        llvmo::SwitchInst_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::SwitchInst_O::static_className() % (void*)(llvmo::SwitchInst_O::static_allocator) );
    classllvmo__SwitchInst_Oval->setCreator(llvmo::SwitchInst_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::SwitchInst_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__SwitchInst_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__UnreachableInst_Oval]"));
    core::BuiltInClass_sp classllvmo__UnreachableInst_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__UnreachableInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__UnreachableInst_Oval,_lisp,llvmo::UnreachableInst_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::UnreachableInst_O>::id,llvmo::UnreachableInst_O::static_classSymbol());
    llvmo::UnreachableInst_O::___staticClass = classllvmo__UnreachableInst_Oval;
#ifdef USE_MPS
    llvmo::UnreachableInst_O::static_Kind = gctools::GCKind<llvmo::UnreachableInst_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__UnreachableInst_Oval,llvmo::UnreachableInst_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::UnreachableInst_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::UnreachableInst_O>>::allocateClass();
        llvmo::UnreachableInst_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::UnreachableInst_O::static_className() % (void*)(llvmo::UnreachableInst_O::static_allocator) );
    classllvmo__UnreachableInst_Oval->setCreator(llvmo::UnreachableInst_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::UnreachableInst_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__UnreachableInst_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classllvmo__VAArgInst_Oval]"));
    core::BuiltInClass_sp classllvmo__VAArgInst_Oval = core::BuiltInClass_O::createUncollectable();
    classllvmo__VAArgInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__VAArgInst_Oval,_lisp,llvmo::VAArgInst_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<llvmo::VAArgInst_O>::id,llvmo::VAArgInst_O::static_classSymbol());
    llvmo::VAArgInst_O::___staticClass = classllvmo__VAArgInst_Oval;
#ifdef USE_MPS
    llvmo::VAArgInst_O::static_Kind = gctools::GCKind<llvmo::VAArgInst_O>::Kind;
#endif
    core::af_setf_findClass(classllvmo__VAArgInst_Oval,llvmo::VAArgInst_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<llvmo::VAArgInst_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<llvmo::VAArgInst_O>>::allocateClass();
        llvmo::VAArgInst_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% llvmo::VAArgInst_O::static_className() % (void*)(llvmo::VAArgInst_O::static_allocator) );
    classllvmo__VAArgInst_Oval->setCreator(llvmo::VAArgInst_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % llvmo::VAArgInst_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classllvmo__VAArgInst_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */
#endif // CREATE_CLASS
#undef CREATE_CLASS
#ifdef DUMP_INFO_CLASS // {
// Depends on nothing

    LOG(BF("---    dump_info   --- className: llvmo::APFloat_O @ %X") % classllvmo__APFloat_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::APFloat_O::static_className() % llvmo::APFloat_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::APInt_O @ %X") % classllvmo__APInt_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::APInt_O::static_className() % llvmo::APInt_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::AttributeSet_O @ %X") % classllvmo__AttributeSet_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::AttributeSet_O::static_className() % llvmo::AttributeSet_O::static_classSymbol() );

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

    LOG(BF("---    dump_info   --- className: llvmo::MCSubtargetInfo_O @ %X") % classllvmo__MCSubtargetInfo_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::MCSubtargetInfo_O::static_className() % llvmo::MCSubtargetInfo_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::Metadata_O @ %X") % classllvmo__Metadata_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::Metadata_O::static_className() % llvmo::Metadata_O::static_classSymbol() );

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

    LOG(BF("---    dump_info   --- className: llvmo::TargetMachine_O @ %X") % classllvmo__TargetMachine_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::TargetMachine_O::static_className() % llvmo::TargetMachine_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::TargetOptions_O @ %X") % classllvmo__TargetOptions_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::TargetOptions_O::static_className() % llvmo::TargetOptions_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::Target_O @ %X") % classllvmo__Target_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::Target_O::static_className() % llvmo::Target_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::Triple_O @ %X") % classllvmo__Triple_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::Triple_O::static_className() % llvmo::Triple_O::static_classSymbol() );

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

    LOG(BF("---    dump_info   --- className: llvmo::DISubroutineType_O @ %X") % classllvmo__DISubroutineType_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::DISubroutineType_O::static_className() % llvmo::DISubroutineType_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::DITypeArray_O @ %X") % classllvmo__DITypeArray_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::DITypeArray_O::static_className() % llvmo::DITypeArray_O::static_classSymbol() );

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

    LOG(BF("---    dump_info   --- className: llvmo::LLVMTargetMachine_O @ %X") % classllvmo__LLVMTargetMachine_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::LLVMTargetMachine_O::static_className() % llvmo::LLVMTargetMachine_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::MDNode_O @ %X") % classllvmo__MDNode_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::MDNode_O::static_className() % llvmo::MDNode_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::MDString_O @ %X") % classllvmo__MDString_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::MDString_O::static_className() % llvmo::MDString_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::ModulePass_O @ %X") % classllvmo__ModulePass_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::ModulePass_O::static_className() % llvmo::ModulePass_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::PassManager_O @ %X") % classllvmo__PassManager_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::PassManager_O::static_className() % llvmo::PassManager_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::TargetSubtargetInfo_O @ %X") % classllvmo__TargetSubtargetInfo_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::TargetSubtargetInfo_O::static_className() % llvmo::TargetSubtargetInfo_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::User_O @ %X") % classllvmo__User_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::User_O::static_className() % llvmo::User_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::ValueAsMetadata_O @ %X") % classllvmo__ValueAsMetadata_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::ValueAsMetadata_O::static_className() % llvmo::ValueAsMetadata_O::static_classSymbol() );

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

    LOG(BF("---    dump_info   --- className: llvmo::ConstantStruct_O @ %X") % classllvmo__ConstantStruct_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::ConstantStruct_O::static_className() % llvmo::ConstantStruct_O::static_classSymbol() );

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

    LOG(BF("---    dump_info   --- className: llvmo::TargetLibraryInfo_O @ %X") % classllvmo__TargetLibraryInfo_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::TargetLibraryInfo_O::static_className() % llvmo::TargetLibraryInfo_O::static_classSymbol() );

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
classllvmo__AttributeSet_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
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
classllvmo__MCSubtargetInfo_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::ExternalObject_O::static_classSymbol());
classllvmo__Metadata_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::ExternalObject_O::static_classSymbol());
classllvmo__Module_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::ExternalObject_O::static_classSymbol());
classllvmo__NamedMDNode_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::ExternalObject_O::static_classSymbol());
classllvmo__PassManagerBase_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::ExternalObject_O::static_classSymbol());
classllvmo__PassManagerBuilder_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::ExternalObject_O::static_classSymbol());
classllvmo__Pass_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::ExternalObject_O::static_classSymbol());
classllvmo__TargetMachine_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::ExternalObject_O::static_classSymbol());
classllvmo__TargetOptions_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::ExternalObject_O::static_classSymbol());
classllvmo__Target_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::ExternalObject_O::static_classSymbol());
classllvmo__Triple_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::ExternalObject_O::static_classSymbol());
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
classllvmo__DISubroutineType_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::DebugInfo_O::static_classSymbol());
classllvmo__DITypeArray_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::DebugInfo_O::static_classSymbol());
classllvmo__DIType_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::DebugInfo_O::static_classSymbol());
classllvmo__FunctionPassManager_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::PassManagerBase_O::static_classSymbol());
classllvmo__FunctionPass_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Pass_O::static_classSymbol());
classllvmo__FunctionType_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Type_O::static_classSymbol());
classllvmo__IRBuilder_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::IRBuilderBase_O::static_classSymbol());
classllvmo__IntegerType_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Type_O::static_classSymbol());
classllvmo__LLVMTargetMachine_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::TargetMachine_O::static_classSymbol());
classllvmo__MDNode_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Metadata_O::static_classSymbol());
classllvmo__MDString_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Metadata_O::static_classSymbol());
classllvmo__ModulePass_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Pass_O::static_classSymbol());
classllvmo__PassManager_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::PassManagerBase_O::static_classSymbol());
classllvmo__TargetSubtargetInfo_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::MCSubtargetInfo_O::static_classSymbol());
classllvmo__User_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Value_O::static_classSymbol());
classllvmo__ValueAsMetadata_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Metadata_O::static_classSymbol());
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
classllvmo__ConstantStruct_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Constant_O::static_classSymbol());
classllvmo__DataLayoutPass_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::ImmutablePass_O::static_classSymbol());
classllvmo__FenceInst_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Instruction_O::static_classSymbol());
classllvmo__GlobalValue_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Constant_O::static_classSymbol());
classllvmo__LandingPadInst_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Instruction_O::static_classSymbol());
classllvmo__PHINode_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Instruction_O::static_classSymbol());
classllvmo__PointerType_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::SequentialType_O::static_classSymbol());
classllvmo__StoreInst_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::Instruction_O::static_classSymbol());
classllvmo__TargetLibraryInfo_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(llvmo::ImmutablePass_O::static_classSymbol());
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
 core::T_sp _curPkg = _lisp->findPackage(CurrentPkg,true);
// Depends on nothing

    classllvmo__APFloat_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::APFloat_O::static_classSymbol());

    classllvmo__APInt_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::APInt_O::static_classSymbol());

    classllvmo__AttributeSet_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::AttributeSet_O::static_classSymbol());

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

    classllvmo__MCSubtargetInfo_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::MCSubtargetInfo_O::static_classSymbol());

    classllvmo__Metadata_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::Metadata_O::static_classSymbol());

    classllvmo__Module_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::Module_O::static_classSymbol());

    classllvmo__NamedMDNode_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::NamedMDNode_O::static_classSymbol());

    classllvmo__PassManagerBase_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::PassManagerBase_O::static_classSymbol());

    classllvmo__PassManagerBuilder_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::PassManagerBuilder_O::static_classSymbol());

    classllvmo__Pass_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::Pass_O::static_classSymbol());

    classllvmo__TargetMachine_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::TargetMachine_O::static_classSymbol());

    classllvmo__TargetOptions_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::TargetOptions_O::static_classSymbol());

    classllvmo__Target_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::Target_O::static_classSymbol());

    classllvmo__Triple_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::Triple_O::static_classSymbol());

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

    classllvmo__DISubroutineType_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::DISubroutineType_O::static_classSymbol());

    classllvmo__DITypeArray_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::DITypeArray_O::static_classSymbol());

    classllvmo__DIType_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::DIType_O::static_classSymbol());

    classllvmo__FunctionPassManager_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::FunctionPassManager_O::static_classSymbol());

    classllvmo__FunctionPass_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::FunctionPass_O::static_classSymbol());

    classllvmo__FunctionType_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::FunctionType_O::static_classSymbol());

    classllvmo__IRBuilder_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::IRBuilder_O::static_classSymbol());

    classllvmo__IntegerType_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::IntegerType_O::static_classSymbol());

    classllvmo__LLVMTargetMachine_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::LLVMTargetMachine_O::static_classSymbol());

    classllvmo__MDNode_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::MDNode_O::static_classSymbol());

    classllvmo__MDString_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::MDString_O::static_classSymbol());

    classllvmo__ModulePass_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::ModulePass_O::static_classSymbol());

    classllvmo__PassManager_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::PassManager_O::static_classSymbol());

    classllvmo__TargetSubtargetInfo_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::TargetSubtargetInfo_O::static_classSymbol());

    classllvmo__User_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::User_O::static_classSymbol());

    classllvmo__ValueAsMetadata_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::ValueAsMetadata_O::static_classSymbol());

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

    classllvmo__ConstantStruct_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::ConstantStruct_O::static_classSymbol());

    classllvmo__DataLayoutPass_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::DataLayoutPass_O::static_classSymbol());

    classllvmo__FenceInst_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::FenceInst_O::static_classSymbol());

    classllvmo__GlobalValue_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::GlobalValue_O::static_classSymbol());

    classllvmo__LandingPadInst_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::LandingPadInst_O::static_classSymbol());

    classllvmo__PHINode_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::PHINode_O::static_classSymbol());

    classllvmo__PointerType_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::PointerType_O::static_classSymbol());

    classllvmo__StoreInst_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::StoreInst_O::static_classSymbol());

    classllvmo__TargetLibraryInfo_Oval->__setupStage3NameAndCalculateClassPrecedenceList(llvmo::TargetLibraryInfo_O::static_classSymbol());

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
extern void Register_llvmo__AttributeSet_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__AttributeSet_O");
    llvmo::Register_llvmo__AttributeSet_O(_lisp); // base(s): set(['core::T_O'])
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
extern void Register_llvmo__MCSubtargetInfo_O(core::Lisp_sp); // base(s): set(['core::ExternalObject_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__MCSubtargetInfo_O");
    llvmo::Register_llvmo__MCSubtargetInfo_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__Metadata_O(core::Lisp_sp); // base(s): set(['core::ExternalObject_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__Metadata_O");
    llvmo::Register_llvmo__Metadata_O(_lisp); // base(s): set(['core::ExternalObject_O'])
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
extern void Register_llvmo__TargetMachine_O(core::Lisp_sp); // base(s): set(['core::ExternalObject_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__TargetMachine_O");
    llvmo::Register_llvmo__TargetMachine_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__TargetOptions_O(core::Lisp_sp); // base(s): set(['core::ExternalObject_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__TargetOptions_O");
    llvmo::Register_llvmo__TargetOptions_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__Target_O(core::Lisp_sp); // base(s): set(['core::ExternalObject_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__Target_O");
    llvmo::Register_llvmo__Target_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__Triple_O(core::Lisp_sp); // base(s): set(['core::ExternalObject_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__Triple_O");
    llvmo::Register_llvmo__Triple_O(_lisp); // base(s): set(['core::ExternalObject_O'])
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
extern void Register_llvmo__DISubroutineType_O(core::Lisp_sp); // base(s): set(['llvmo::DebugInfo_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__DISubroutineType_O");
    llvmo::Register_llvmo__DISubroutineType_O(_lisp); // base(s): set(['llvmo::DebugInfo_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__DITypeArray_O(core::Lisp_sp); // base(s): set(['llvmo::DebugInfo_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__DITypeArray_O");
    llvmo::Register_llvmo__DITypeArray_O(_lisp); // base(s): set(['llvmo::DebugInfo_O'])
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
extern void Register_llvmo__LLVMTargetMachine_O(core::Lisp_sp); // base(s): set(['llvmo::TargetMachine_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__LLVMTargetMachine_O");
    llvmo::Register_llvmo__LLVMTargetMachine_O(_lisp); // base(s): set(['llvmo::TargetMachine_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__MDNode_O(core::Lisp_sp); // base(s): set(['llvmo::Metadata_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__MDNode_O");
    llvmo::Register_llvmo__MDNode_O(_lisp); // base(s): set(['llvmo::Metadata_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
#ifdef EXTERN_REGISTER
extern void Register_llvmo__MDString_O(core::Lisp_sp); // base(s): set(['llvmo::Metadata_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__MDString_O");
    llvmo::Register_llvmo__MDString_O(_lisp); // base(s): set(['llvmo::Metadata_O'])
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
extern void Register_llvmo__TargetSubtargetInfo_O(core::Lisp_sp); // base(s): set(['llvmo::MCSubtargetInfo_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__TargetSubtargetInfo_O");
    llvmo::Register_llvmo__TargetSubtargetInfo_O(_lisp); // base(s): set(['llvmo::MCSubtargetInfo_O'])
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
extern void Register_llvmo__ValueAsMetadata_O(core::Lisp_sp); // base(s): set(['llvmo::Metadata_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__ValueAsMetadata_O");
    llvmo::Register_llvmo__ValueAsMetadata_O(_lisp); // base(s): set(['llvmo::Metadata_O'])
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
extern void Register_llvmo__ConstantStruct_O(core::Lisp_sp); // base(s): set(['llvmo::Constant_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__ConstantStruct_O");
    llvmo::Register_llvmo__ConstantStruct_O(_lisp); // base(s): set(['llvmo::Constant_O'])
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
extern void Register_llvmo__TargetLibraryInfo_O(core::Lisp_sp); // base(s): set(['llvmo::ImmutablePass_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_llvmo__TargetLibraryInfo_O");
    llvmo::Register_llvmo__TargetLibraryInfo_O(_lisp); // base(s): set(['llvmo::ImmutablePass_O'])
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
extern void Call_exposePython_llvmo__AttributeSet_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__AttributeSet_O");
	Call_exposePython_llvmo__AttributeSet_O(_lisp); // base(s): set(['core::T_O'])
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
extern void Call_exposePython_llvmo__MCSubtargetInfo_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__MCSubtargetInfo_O");
	Call_exposePython_llvmo__MCSubtargetInfo_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__Metadata_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__Metadata_O");
	Call_exposePython_llvmo__Metadata_O(_lisp); // base(s): set(['core::ExternalObject_O'])
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
extern void Call_exposePython_llvmo__TargetMachine_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__TargetMachine_O");
	Call_exposePython_llvmo__TargetMachine_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__TargetOptions_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__TargetOptions_O");
	Call_exposePython_llvmo__TargetOptions_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__Target_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__Target_O");
	Call_exposePython_llvmo__Target_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__Triple_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__Triple_O");
	Call_exposePython_llvmo__Triple_O(_lisp); // base(s): set(['core::ExternalObject_O'])
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
extern void Call_exposePython_llvmo__DISubroutineType_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__DISubroutineType_O");
	Call_exposePython_llvmo__DISubroutineType_O(_lisp); // base(s): set(['llvmo::DebugInfo_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__DITypeArray_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__DITypeArray_O");
	Call_exposePython_llvmo__DITypeArray_O(_lisp); // base(s): set(['llvmo::DebugInfo_O'])
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
extern void Call_exposePython_llvmo__LLVMTargetMachine_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__LLVMTargetMachine_O");
	Call_exposePython_llvmo__LLVMTargetMachine_O(_lisp); // base(s): set(['llvmo::TargetMachine_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__MDNode_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__MDNode_O");
	Call_exposePython_llvmo__MDNode_O(_lisp); // base(s): set(['llvmo::Metadata_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__MDString_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__MDString_O");
	Call_exposePython_llvmo__MDString_O(_lisp); // base(s): set(['llvmo::Metadata_O'])
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
extern void Call_exposePython_llvmo__TargetSubtargetInfo_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__TargetSubtargetInfo_O");
	Call_exposePython_llvmo__TargetSubtargetInfo_O(_lisp); // base(s): set(['llvmo::MCSubtargetInfo_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__User_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__User_O");
	Call_exposePython_llvmo__User_O(_lisp); // base(s): set(['llvmo::Value_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__ValueAsMetadata_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__ValueAsMetadata_O");
	Call_exposePython_llvmo__ValueAsMetadata_O(_lisp); // base(s): set(['llvmo::Metadata_O'])
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
extern void Call_exposePython_llvmo__ConstantStruct_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__ConstantStruct_O");
	Call_exposePython_llvmo__ConstantStruct_O(_lisp); // base(s): set(['llvmo::Constant_O'])
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
extern void Call_exposePython_llvmo__TargetLibraryInfo_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__TargetLibraryInfo_O");
	Call_exposePython_llvmo__TargetLibraryInfo_O(_lisp); // base(s): set(['llvmo::ImmutablePass_O'])
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
_CLASS_MACRO(llvmo::AttributeSet_O)
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
_CLASS_MACRO(llvmo::MCSubtargetInfo_O)
_CLASS_MACRO(llvmo::Metadata_O)
_CLASS_MACRO(llvmo::Module_O)
_CLASS_MACRO(llvmo::NamedMDNode_O)
_CLASS_MACRO(llvmo::PassManagerBase_O)
_CLASS_MACRO(llvmo::PassManagerBuilder_O)
_CLASS_MACRO(llvmo::Pass_O)
_CLASS_MACRO(llvmo::TargetMachine_O)
_CLASS_MACRO(llvmo::TargetOptions_O)
_CLASS_MACRO(llvmo::Target_O)
_CLASS_MACRO(llvmo::Triple_O)
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
_CLASS_MACRO(llvmo::DISubroutineType_O)
_CLASS_MACRO(llvmo::DITypeArray_O)
_CLASS_MACRO(llvmo::DIType_O)
_CLASS_MACRO(llvmo::FunctionPassManager_O)
_CLASS_MACRO(llvmo::FunctionPass_O)
_CLASS_MACRO(llvmo::FunctionType_O)
_CLASS_MACRO(llvmo::IRBuilder_O)
_CLASS_MACRO(llvmo::IntegerType_O)
_CLASS_MACRO(llvmo::LLVMTargetMachine_O)
_CLASS_MACRO(llvmo::MDNode_O)
_CLASS_MACRO(llvmo::MDString_O)
_CLASS_MACRO(llvmo::ModulePass_O)
_CLASS_MACRO(llvmo::PassManager_O)
_CLASS_MACRO(llvmo::TargetSubtargetInfo_O)
_CLASS_MACRO(llvmo::User_O)
_CLASS_MACRO(llvmo::ValueAsMetadata_O)
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
_CLASS_MACRO(llvmo::ConstantStruct_O)
_CLASS_MACRO(llvmo::DataLayoutPass_O)
_CLASS_MACRO(llvmo::FenceInst_O)
_CLASS_MACRO(llvmo::GlobalValue_O)
_CLASS_MACRO(llvmo::LandingPadInst_O)
_CLASS_MACRO(llvmo::PHINode_O)
_CLASS_MACRO(llvmo::PointerType_O)
_CLASS_MACRO(llvmo::StoreInst_O)
_CLASS_MACRO(llvmo::TargetLibraryInfo_O)
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
// ---------------- after class initializers
#ifdef EXPOSE_TO_PYTHON
#endif // EXPOSE_TO_PYTHON
