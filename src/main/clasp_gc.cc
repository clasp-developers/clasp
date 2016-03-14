#ifdef DECLARE_FORWARDS
 namespace asttooling {
    class DerivableASTFrontendAction;
    class DerivableMatchCallback;
    class DerivableSyntaxOnlyAction;
    class ErrorContent;
    class Message;
    class DerivableFrontendActionFactory;
    class ContextFrame;
    namespace RegMap {
        class SymbolMatcherDescriptorPair;
    };
 };
 namespace clbind {
    class DummyCreator;
    class ClassRep_O;
    class ClassRegistry_O;
 };
 namespace llvmo {
    class LLVMContext_O;
    class AtomicCmpXchgInst_O;
    class TargetOptions_O;
    class Attribute_O;
    class Pass_O;
    class ValueAsMetadata_O;
    class BlockAddress_O;
    class GlobalVariable_O;
    class SwitchInst_O;
    class Argument_O;
    class VAArgInst_O;
    class CompositeType_O;
    class SequentialType_O;
    class ConstantFP_O;
    class Metadata_O;
    class ArrayType_O;
    class IRBuilderBase_O;
    class StructType_O;
    class MDNode_O;
    class AllocaInst_O;
    class Triple_O;
    class UnreachableInst_O;
    class TargetMachine_O;
    class GlobalValue_O;
    class ConstantDataSequential_O;
    class FunctionPassManager_O;
    class ModulePass_O;
    class TerminatorInst_O;
    class FunctionType_O;
    class BasicBlock_O;
    class Module_O;
    class Target_O;
    class IndirectBrInst_O;
    class PassManagerBase_O;
    class InvokeInst_O;
    class LandingPadInst_O;
    class Type_O;
    class Function_O;
    class Value_O;
    class Constant_O;
    class ConstantPointerNull_O;
    class NamedMDNode_O;
    class DataLayoutPass_O;
    class Linker_O;
    class ExecutionEngine_O;
    class PassManagerBuilder_O;
    class LLVMTargetMachine_O;
    class MDString_O;
    class ReturnInst_O;
    class EngineBuilder_O;
    class UndefValue_O;
    class TargetLibraryInfo_O;
    class ConstantDataArray_O;
    class ResumeInst_O;
    class APFloat_O;
    class DataLayout_O;
    class PointerType_O;
    class PassManager_O;
    class User_O;
    class Instruction_O;
    class TargetSubtargetInfo_O;
    class ConstantStruct_O;
    class FunctionPass_O;
    class VectorType_O;
    class PHINode_O;
    class ImmutablePass_O;
    class CallInst_O;
    class MCSubtargetInfo_O;
    class StoreInst_O;
    class AttributeSet_O;
    class LoadInst_O;
    class ConstantExpr_O;
    class UnaryInstruction_O;
    class AtomicRMWInst_O;
    class BranchInst_O;
    class ConstantInt_O;
    class IntegerType_O;
    class ConstantArray_O;
    class FenceInst_O;
    class IRBuilder_O;
    class APInt_O;
 };
 namespace core {
    class GlueEnvironment_O;
    class SingleFloat_dummy_O;
    class SourceFileInfo_O;
    class General_O;
    class Record_O;
    class CandoException_O;
    class Iterator_O;
    class HashTableEqual_O;
    class RuntimeVisibleEnvironment_O;
    class ValueFrame_O;
    class LongFloat_O;
    class Float_O;
    class SourcePosInfo_O;
    class Specializer_O;
    class SlotData;
    class MacroClosure;
    class Integer_O;
    class TagbodyEnvironment_O;
    class CxxObject_O;
    class WeakKeyHashTable_O;
    class WeakHashTable_O;
    class Ratio_O;
    class ExternalObject_O;
    class Bignum_O;
    class FunctionFrame_O;
    class ValueEnvironment_O;
    class BuiltInClass_O;
    class VectorObjectsWithFillPtr_O;
    class Class_O;
    class Fixnum_dummy_O;
    class BlockEnvironment_O;
    class ShortFloat_O;
    class Array_O;
    class Real_O;
    class Metaobject_O;
    class UnwindProtectEnvironment_O;
    class Vector_O;
    class CacheRecord;
    class SymbolToEnumConverter_O;
    class String_O;
    class MacroletEnvironment_O;
    class TagbodyFrame_O;
    class CompileTimeEnvironment_O;
    class DynamicBinding;
    class StackValueEnvironment_O;
    class SymbolClassPair;
    class SymbolMacroletEnvironment_O;
    class LexicalEnvironment_O;
    class SourceManager_O;
    class Complex_O;
    class Environment_O;
    class Number_O;
    class Pointer_O;
    class Str_O;
    class CatchEnvironment_O;
    class ForeignData_O;
    class Rational_O;
    class CompiledFunction_O;
    class Instance_O;
    class DoubleFloat_O;
    class StandardObject_O;
    class Function_O;
    class ExceptionEntry;
    class FunctionContainerEnvironment_O;
    class VectorObjects_O;
    class FunctionValueEnvironment_O;
    class Symbol_O;
 };
#endif // DECLARE_FORWARDS
#if defined(GC_ENUM)
enum { KIND_null = 0, 
KIND_CONS = 4, 
KIND_CHARACTER = 3, 
KIND_SINGLE_FLOAT = 2, 
KIND_FIXNUM = 1, 
KIND_LISPALLOC_llvmo__APInt_O = 5,
KIND_BOOTSTRAP_core__Symbol_O = 6,
KIND_LISPALLOC_llvmo__IRBuilder_O = 7,
KIND_LISPALLOC_core__FunctionValueEnvironment_O = 8,
KIND_LISPALLOC_llvmo__FenceInst_O = 9,
KIND_LISPALLOC_core__VectorObjects_O = 10,
KIND_LISPALLOC_core__FunctionContainerEnvironment_O = 11,
KIND_LISPALLOC_llvmo__ConstantArray_O = 12,
KIND_LISPALLOC_llvmo__IntegerType_O = 13,
KIND_LISPALLOC_core__Function_O = 14,
KIND_LISPALLOC_llvmo__ConstantInt_O = 15,
KIND_LISPALLOC_llvmo__BranchInst_O = 16,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__ = 17,
KIND_BOOTSTRAP_core__StandardObject_O = 18,
KIND_LISPALLOC_asttooling__DerivableFrontendActionFactory = 19,
KIND_LISPALLOC_llvmo__AtomicRMWInst_O = 20,
KIND_LISPALLOC_llvmo__UnaryInstruction_O = 21,
KIND_LISPALLOC_core__DoubleFloat_O = 22,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__ = 23,
KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_ = 24,
KIND_LISPALLOC_llvmo__ConstantExpr_O = 25,
KIND_LISPALLOC_llvmo__LoadInst_O = 26,
KIND_LISPALLOC_llvmo__AttributeSet_O = 27,
KIND_LISPALLOC_core__Instance_O = 28,
KIND_LISPALLOC_llvmo__StoreInst_O = 29,
KIND_LISPALLOC_llvmo__MCSubtargetInfo_O = 30,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__ = 31,
KIND_LISPALLOC_llvmo__CallInst_O = 32,
KIND_LISPALLOC_llvmo__PHINode_O = 33,
KIND_LISPALLOC_llvmo__ImmutablePass_O = 34,
KIND_LISPALLOC_core__CompiledFunction_O = 35,
KIND_LISPALLOC_core__Rational_O = 36,
KIND_LISPALLOC_core__ForeignData_O = 37,
KIND_LISPALLOC_llvmo__VectorType_O = 38,
KIND_LISPALLOC_llvmo__FunctionPass_O = 39,
KIND_LISPALLOC_llvmo__ConstantStruct_O = 40,
KIND_LISPALLOC_llvmo__TargetSubtargetInfo_O = 41,
KIND_LISPALLOC_llvmo__Instruction_O = 42,
KIND_LISPALLOC_llvmo__User_O = 43,
KIND_LISPALLOC_core__CatchEnvironment_O = 44,
KIND_BOOTSTRAP_core__Str_O = 45,
KIND_LISPALLOC_core__Pointer_O = 46,
KIND_LISPALLOC_llvmo__PassManager_O = 47,
KIND_LISPALLOC_clbind__ClassRegistry_O = 48,
KIND_LISPALLOC_llvmo__PointerType_O = 49,
KIND_LISPALLOC_llvmo__DataLayout_O = 50,
KIND_LISPALLOC_llvmo__APFloat_O = 51,
KIND_LISPALLOC_llvmo__ResumeInst_O = 52,
KIND_LISPALLOC_llvmo__ConstantDataArray_O = 53,
KIND_LISPALLOC_core__Number_O = 54,
KIND_LISPALLOC_llvmo__TargetLibraryInfo_O = 55,
KIND_LISPALLOC_llvmo__UndefValue_O = 56,
KIND_LISPALLOC_llvmo__EngineBuilder_O = 57,
KIND_LISPALLOC_core__Environment_O = 58,
KIND_GCARRAY_gctools__GCArray_moveable_core__SlotData_0_ = 59,
KIND_LISPALLOC_llvmo__ReturnInst_O = 60,
KIND_LISPALLOC_llvmo__MDString_O = 61,
KIND_LISPALLOC_core__Complex_O = 62,
KIND_LISPALLOC_core__SourceManager_O = 63,
KIND_LISPALLOC_llvmo__LLVMTargetMachine_O = 64,
KIND_LISPALLOC_core__LexicalEnvironment_O = 65,
KIND_LISPALLOC_core__SymbolMacroletEnvironment_O = 66,
KIND_LISPALLOC_core__StackValueEnvironment_O = 67,
KIND_LISPALLOC_llvmo__PassManagerBuilder_O = 68,
KIND_LISPALLOC_asttooling__DerivableSyntaxOnlyAction = 69,
KIND_LISPALLOC_core__CompileTimeEnvironment_O = 70,
KIND_LISPALLOC_llvmo__ExecutionEngine_O = 71,
KIND_LISPALLOC_core__TagbodyFrame_O = 72,
KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_ = 73,
KIND_LISPALLOC_core__MacroletEnvironment_O = 74,
KIND_LISPALLOC_llvmo__Linker_O = 75,
KIND_LISPALLOC_llvmo__DataLayoutPass_O = 76,
KIND_LISPALLOC_llvmo__NamedMDNode_O = 77,
KIND_LISPALLOC_llvmo__ConstantPointerNull_O = 78,
KIND_LISPALLOC_llvmo__Constant_O = 79,
KIND_LISPALLOC_core__String_O = 80,
KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_ = 81,
KIND_LISPALLOC_core__SymbolToEnumConverter_O = 82,
KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__Message_ = 83,
KIND_LISPALLOC_clbind__ClassRep_O = 84,
KIND_LISPALLOC_llvmo__Value_O = 85,
KIND_GCSTRING_gctools__GCString_moveable_char_ = 86,
KIND_LISPALLOC_llvmo__Function_O = 87,
KIND_LISPALLOC_llvmo__Type_O = 88,
KIND_LISPALLOC_asttooling__DerivableMatchCallback = 89,
KIND_LISPALLOC_llvmo__LandingPadInst_O = 90,
KIND_LISPALLOC_core__Vector_O = 91,
KIND_LISPALLOC_core__UnwindProtectEnvironment_O = 92,
KIND_BOOTSTRAP_core__Metaobject_O = 93,
KIND_LISPALLOC_llvmo__InvokeInst_O = 94,
KIND_LISPALLOC_core__Real_O = 95,
KIND_LISPALLOC_core__Array_O = 96,
KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_ = 97,
KIND_LISPALLOC_core__ShortFloat_O = 98,
KIND_LISPALLOC_core__BlockEnvironment_O = 99,
KIND_LISPALLOC_core__Fixnum_dummy_O = 100,
KIND_LISPALLOC_llvmo__PassManagerBase_O = 101,
KIND_BOOTSTRAP_core__Class_O = 102,
KIND_LISPALLOC_asttooling__DerivableASTFrontendAction = 103,
KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O = 104,
KIND_BOOTSTRAP_core__BuiltInClass_O = 105,
KIND_LISPALLOC_core__ValueEnvironment_O = 106,
KIND_LISPALLOC_llvmo__IndirectBrInst_O = 107,
KIND_LISPALLOC_llvmo__Target_O = 108,
KIND_LISPALLOC_llvmo__Module_O = 109,
KIND_LISPALLOC_core__FunctionFrame_O = 110,
KIND_LISPALLOC_core__Bignum_O = 111,
KIND_LISPALLOC_core__ExternalObject_O = 112,
KIND_TEMPLATED_LISPALLOC_core__WrappedPointer_O = 113,
KIND_LISPALLOC_core__Ratio_O = 114,
KIND_LISPALLOC_core__WeakHashTable_O = 115,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__ = 116,
KIND_LISPALLOC_llvmo__BasicBlock_O = 117,
KIND_LISPALLOC_llvmo__TerminatorInst_O = 118,
KIND_LISPALLOC_llvmo__FunctionType_O = 119,
KIND_LISPALLOC_core__WeakKeyHashTable_O = 120,
KIND_LISPALLOC_core__CxxObject_O = 121,
KIND_LISPALLOC_llvmo__ModulePass_O = 122,
KIND_LISPALLOC_core__TagbodyEnvironment_O = 123,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__ = 124,
KIND_LISPALLOC_llvmo__FunctionPassManager_O = 125,
KIND_LISPALLOC_llvmo__ConstantDataSequential_O = 126,
KIND_TEMPLATED_CLASSALLOC_clbind__ConstructorCreator = 127,
KIND_LISPALLOC_llvmo__GlobalValue_O = 128,
KIND_LISPALLOC_llvmo__TargetMachine_O = 129,
KIND_CLASSALLOC_core__MacroClosure = 130,
KIND_LISPALLOC_core__Integer_O = 131,
KIND_LISPALLOC_llvmo__UnreachableInst_O = 132,
KIND_LISPALLOC_llvmo__Triple_O = 133,
KIND_LISPALLOC_llvmo__AllocaInst_O = 134,
KIND_LISPALLOC_llvmo__MDNode_O = 135,
KIND_LISPALLOC_llvmo__StructType_O = 136,
KIND_LISPALLOC_llvmo__IRBuilderBase_O = 137,
KIND_BOOTSTRAP_core__Specializer_O = 138,
KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_ = 139,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__ = 140,
KIND_LISPALLOC_core__SourcePosInfo_O = 141,
KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_ = 142,
KIND_LISPALLOC_core__Float_O = 143,
KIND_LISPALLOC_core__LongFloat_O = 144,
KIND_LISPALLOC_llvmo__ArrayType_O = 145,
KIND_LISPALLOC_core__ValueFrame_O = 146,
KIND_LISPALLOC_llvmo__Metadata_O = 147,
KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O = 148,
KIND_LISPALLOC_llvmo__ConstantFP_O = 149,
KIND_CLASSALLOC_clbind__DummyCreator = 150,
KIND_LISPALLOC_llvmo__SequentialType_O = 151,
KIND_LISPALLOC_core__HashTableEqual_O = 152,
KIND_LISPALLOC_llvmo__CompositeType_O = 153,
KIND_LISPALLOC_llvmo__VAArgInst_O = 154,
KIND_LISPALLOC_llvmo__Argument_O = 155,
KIND_LISPALLOC_core__CandoException_O = 156,
KIND_LISPALLOC_core__Iterator_O = 157,
KIND_LISPALLOC_core__Record_O = 158,
KIND_LISPALLOC_llvmo__SwitchInst_O = 159,
KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure = 160,
KIND_LISPALLOC_llvmo__GlobalVariable_O = 161,
KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_ = 162,
KIND_LISPALLOC_core__General_O = 163,
KIND_LISPALLOC_llvmo__BlockAddress_O = 164,
KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ErrorContent_ = 165,
KIND_LISPALLOC_llvmo__ValueAsMetadata_O = 166,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_clbind__ClassRep_O__ = 167,
KIND_LISPALLOC_llvmo__Pass_O = 168,
KIND_LISPALLOC_llvmo__Attribute_O = 169,
KIND_LISPALLOC_core__SourceFileInfo_O = 170,
KIND_LISPALLOC_core__SingleFloat_dummy_O = 171,
KIND_LISPALLOC_llvmo__TargetOptions_O = 172,
KIND_LISPALLOC_llvmo__LLVMContext_O = 173,
KIND_LISPALLOC_llvmo__AtomicCmpXchgInst_O = 174,
KIND_LISPALLOC_core__GlueEnvironment_O = 175,
  KIND_max = 175
}
#endif // defined(GC_ENUM)
#if defined(GC_DYNAMIC_CAST)
#endif // defined(GC_DYNAMIC_CAST)
#if defined(GC_KIND_SELECTORS)
template <> class gctools::GCKind<llvmo::APInt_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__APInt_O ;
};
template <> class gctools::GCKind<core::Symbol_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__Symbol_O ;
};
template <> class gctools::GCKind<llvmo::IRBuilder_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__IRBuilder_O ;
};
template <> class gctools::GCKind<core::FunctionValueEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__FunctionValueEnvironment_O ;
};
template <> class gctools::GCKind<llvmo::FenceInst_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__FenceInst_O ;
};
template <> class gctools::GCKind<core::VectorObjects_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__VectorObjects_O ;
};
template <> class gctools::GCKind<core::FunctionContainerEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__FunctionContainerEnvironment_O ;
};
template <> class gctools::GCKind<llvmo::ConstantArray_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__ConstantArray_O ;
};
template <> class gctools::GCKind<llvmo::IntegerType_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__IntegerType_O ;
};
template <> class gctools::GCKind<core::Function_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Function_O ;
};
template <> class gctools::GCKind<llvmo::ConstantInt_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__ConstantInt_O ;
};
template <> class gctools::GCKind<llvmo::BranchInst_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__BranchInst_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__ ;
};
template <> class gctools::GCKind<core::StandardObject_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__StandardObject_O ;
};
template <> class gctools::GCKind<asttooling::DerivableFrontendActionFactory> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_asttooling__DerivableFrontendActionFactory ;
};
template <> class gctools::GCKind<llvmo::AtomicRMWInst_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__AtomicRMWInst_O ;
};
template <> class gctools::GCKind<llvmo::UnaryInstruction_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__UnaryInstruction_O ;
};
template <> class gctools::GCKind<core::DoubleFloat_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__DoubleFloat_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__ ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<asttooling::RegMap::SymbolMatcherDescriptorPair>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_ ;
};
template <> class gctools::GCKind<llvmo::ConstantExpr_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__ConstantExpr_O ;
};
template <> class gctools::GCKind<llvmo::LoadInst_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__LoadInst_O ;
};
template <> class gctools::GCKind<llvmo::AttributeSet_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__AttributeSet_O ;
};
template <> class gctools::GCKind<core::Instance_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Instance_O ;
};
template <> class gctools::GCKind<llvmo::StoreInst_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__StoreInst_O ;
};
template <> class gctools::GCKind<llvmo::MCSubtargetInfo_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__MCSubtargetInfo_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__ ;
};
template <> class gctools::GCKind<llvmo::CallInst_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__CallInst_O ;
};
template <> class gctools::GCKind<llvmo::PHINode_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__PHINode_O ;
};
template <> class gctools::GCKind<llvmo::ImmutablePass_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__ImmutablePass_O ;
};
template <> class gctools::GCKind<core::CompiledFunction_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__CompiledFunction_O ;
};
template <> class gctools::GCKind<core::Rational_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Rational_O ;
};
template <> class gctools::GCKind<core::ForeignData_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__ForeignData_O ;
};
template <> class gctools::GCKind<llvmo::VectorType_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__VectorType_O ;
};
template <> class gctools::GCKind<llvmo::FunctionPass_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__FunctionPass_O ;
};
template <> class gctools::GCKind<llvmo::ConstantStruct_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__ConstantStruct_O ;
};
template <> class gctools::GCKind<llvmo::TargetSubtargetInfo_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__TargetSubtargetInfo_O ;
};
template <> class gctools::GCKind<llvmo::Instruction_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__Instruction_O ;
};
template <> class gctools::GCKind<llvmo::User_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__User_O ;
};
template <> class gctools::GCKind<core::CatchEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__CatchEnvironment_O ;
};
template <> class gctools::GCKind<core::Str_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__Str_O ;
};
template <> class gctools::GCKind<core::Pointer_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Pointer_O ;
};
template <> class gctools::GCKind<llvmo::PassManager_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__PassManager_O ;
};
template <> class gctools::GCKind<clbind::ClassRegistry_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_clbind__ClassRegistry_O ;
};
template <> class gctools::GCKind<llvmo::PointerType_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__PointerType_O ;
};
template <> class gctools::GCKind<llvmo::DataLayout_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__DataLayout_O ;
};
template <> class gctools::GCKind<llvmo::APFloat_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__APFloat_O ;
};
template <> class gctools::GCKind<llvmo::ResumeInst_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__ResumeInst_O ;
};
template <> class gctools::GCKind<llvmo::ConstantDataArray_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__ConstantDataArray_O ;
};
template <> class gctools::GCKind<core::Number_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Number_O ;
};
template <> class gctools::GCKind<llvmo::TargetLibraryInfo_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__TargetLibraryInfo_O ;
};
template <> class gctools::GCKind<llvmo::UndefValue_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__UndefValue_O ;
};
template <> class gctools::GCKind<llvmo::EngineBuilder_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__EngineBuilder_O ;
};
template <> class gctools::GCKind<core::Environment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Environment_O ;
};
template <> class gctools::GCKind<gctools::GCArray_moveable<core::SlotData,0>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCARRAY_gctools__GCArray_moveable_core__SlotData_0_ ;
};
template <> class gctools::GCKind<llvmo::ReturnInst_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__ReturnInst_O ;
};
template <> class gctools::GCKind<llvmo::MDString_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__MDString_O ;
};
template <> class gctools::GCKind<core::Complex_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Complex_O ;
};
template <> class gctools::GCKind<core::SourceManager_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SourceManager_O ;
};
template <> class gctools::GCKind<llvmo::LLVMTargetMachine_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__LLVMTargetMachine_O ;
};
template <> class gctools::GCKind<core::LexicalEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__LexicalEnvironment_O ;
};
template <> class gctools::GCKind<core::SymbolMacroletEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SymbolMacroletEnvironment_O ;
};
template <> class gctools::GCKind<core::StackValueEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__StackValueEnvironment_O ;
};
template <> class gctools::GCKind<llvmo::PassManagerBuilder_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__PassManagerBuilder_O ;
};
template <> class gctools::GCKind<asttooling::DerivableSyntaxOnlyAction> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_asttooling__DerivableSyntaxOnlyAction ;
};
template <> class gctools::GCKind<core::CompileTimeEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__CompileTimeEnvironment_O ;
};
template <> class gctools::GCKind<llvmo::ExecutionEngine_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__ExecutionEngine_O ;
};
template <> class gctools::GCKind<core::TagbodyFrame_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__TagbodyFrame_O ;
};
template <> class gctools::GCKind<gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_ ;
};
template <> class gctools::GCKind<core::MacroletEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__MacroletEnvironment_O ;
};
template <> class gctools::GCKind<llvmo::Linker_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__Linker_O ;
};
template <> class gctools::GCKind<llvmo::DataLayoutPass_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__DataLayoutPass_O ;
};
template <> class gctools::GCKind<llvmo::NamedMDNode_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__NamedMDNode_O ;
};
template <> class gctools::GCKind<llvmo::ConstantPointerNull_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__ConstantPointerNull_O ;
};
template <> class gctools::GCKind<llvmo::Constant_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__Constant_O ;
};
template <> class gctools::GCKind<core::String_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__String_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::ExceptionEntry>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_ ;
};
template <> class gctools::GCKind<core::SymbolToEnumConverter_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SymbolToEnumConverter_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<asttooling::Message>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__Message_ ;
};
template <> class gctools::GCKind<clbind::ClassRep_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_clbind__ClassRep_O ;
};
template <> class gctools::GCKind<llvmo::Value_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__Value_O ;
};
template <> class gctools::GCKind<gctools::GCString_moveable<char>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCSTRING_gctools__GCString_moveable_char_ ;
};
template <> class gctools::GCKind<llvmo::Function_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__Function_O ;
};
template <> class gctools::GCKind<llvmo::Type_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__Type_O ;
};
template <> class gctools::GCKind<asttooling::DerivableMatchCallback> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_asttooling__DerivableMatchCallback ;
};
template <> class gctools::GCKind<llvmo::LandingPadInst_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__LandingPadInst_O ;
};
template <> class gctools::GCKind<core::Vector_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Vector_O ;
};
template <> class gctools::GCKind<core::UnwindProtectEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__UnwindProtectEnvironment_O ;
};
template <> class gctools::GCKind<core::Metaobject_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__Metaobject_O ;
};
template <> class gctools::GCKind<llvmo::InvokeInst_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__InvokeInst_O ;
};
template <> class gctools::GCKind<core::Real_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Real_O ;
};
template <> class gctools::GCKind<core::Array_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Array_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::CacheRecord>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_ ;
};
template <> class gctools::GCKind<core::ShortFloat_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__ShortFloat_O ;
};
template <> class gctools::GCKind<core::BlockEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__BlockEnvironment_O ;
};
template <> class gctools::GCKind<core::Fixnum_dummy_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Fixnum_dummy_O ;
};
template <> class gctools::GCKind<llvmo::PassManagerBase_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__PassManagerBase_O ;
};
template <> class gctools::GCKind<core::Class_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__Class_O ;
};
template <> class gctools::GCKind<asttooling::DerivableASTFrontendAction> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_asttooling__DerivableASTFrontendAction ;
};
template <> class gctools::GCKind<core::VectorObjectsWithFillPtr_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O ;
};
template <> class gctools::GCKind<core::BuiltInClass_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__BuiltInClass_O ;
};
template <> class gctools::GCKind<core::ValueEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__ValueEnvironment_O ;
};
template <> class gctools::GCKind<llvmo::IndirectBrInst_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__IndirectBrInst_O ;
};
template <> class gctools::GCKind<llvmo::Target_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__Target_O ;
};
template <> class gctools::GCKind<llvmo::Module_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__Module_O ;
};
template <> class gctools::GCKind<core::FunctionFrame_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__FunctionFrame_O ;
};
template <> class gctools::GCKind<core::Bignum_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Bignum_O ;
};
template <> class gctools::GCKind<core::ExternalObject_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__ExternalObject_O ;
};
template <> class gctools::GCKind<core::WrappedPointer_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_TEMPLATED_LISPALLOC_core__WrappedPointer_O ;
};
template <> class gctools::GCKind<core::Ratio_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Ratio_O ;
};
template <> class gctools::GCKind<core::WeakHashTable_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__WeakHashTable_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__ ;
};
template <> class gctools::GCKind<llvmo::BasicBlock_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__BasicBlock_O ;
};
template <> class gctools::GCKind<llvmo::TerminatorInst_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__TerminatorInst_O ;
};
template <> class gctools::GCKind<llvmo::FunctionType_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__FunctionType_O ;
};
template <> class gctools::GCKind<core::WeakKeyHashTable_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__WeakKeyHashTable_O ;
};
template <> class gctools::GCKind<core::CxxObject_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__CxxObject_O ;
};
template <> class gctools::GCKind<llvmo::ModulePass_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__ModulePass_O ;
};
template <> class gctools::GCKind<core::TagbodyEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__TagbodyEnvironment_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__ ;
};
template <> class gctools::GCKind<llvmo::FunctionPassManager_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__FunctionPassManager_O ;
};
template <> class gctools::GCKind<llvmo::ConstantDataSequential_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__ConstantDataSequential_O ;
};
template <> class gctools::GCKind<clbind::ConstructorCreator> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_TEMPLATED_CLASSALLOC_clbind__ConstructorCreator ;
};
template <> class gctools::GCKind<llvmo::GlobalValue_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__GlobalValue_O ;
};
template <> class gctools::GCKind<llvmo::TargetMachine_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__TargetMachine_O ;
};
template <> class gctools::GCKind<core::MacroClosure> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_CLASSALLOC_core__MacroClosure ;
};
template <> class gctools::GCKind<core::Integer_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Integer_O ;
};
template <> class gctools::GCKind<llvmo::UnreachableInst_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__UnreachableInst_O ;
};
template <> class gctools::GCKind<llvmo::Triple_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__Triple_O ;
};
template <> class gctools::GCKind<llvmo::AllocaInst_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__AllocaInst_O ;
};
template <> class gctools::GCKind<llvmo::MDNode_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__MDNode_O ;
};
template <> class gctools::GCKind<llvmo::StructType_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__StructType_O ;
};
template <> class gctools::GCKind<llvmo::IRBuilderBase_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__IRBuilderBase_O ;
};
template <> class gctools::GCKind<core::Specializer_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__Specializer_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<asttooling::ContextFrame>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_ ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__ ;
};
template <> class gctools::GCKind<core::SourcePosInfo_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SourcePosInfo_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::SymbolClassPair>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_ ;
};
template <> class gctools::GCKind<core::Float_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Float_O ;
};
template <> class gctools::GCKind<core::LongFloat_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__LongFloat_O ;
};
template <> class gctools::GCKind<llvmo::ArrayType_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__ArrayType_O ;
};
template <> class gctools::GCKind<core::ValueFrame_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__ValueFrame_O ;
};
template <> class gctools::GCKind<llvmo::Metadata_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__Metadata_O ;
};
template <> class gctools::GCKind<core::RuntimeVisibleEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O ;
};
template <> class gctools::GCKind<llvmo::ConstantFP_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__ConstantFP_O ;
};
template <> class gctools::GCKind<clbind::DummyCreator> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_CLASSALLOC_clbind__DummyCreator ;
};
template <> class gctools::GCKind<llvmo::SequentialType_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__SequentialType_O ;
};
template <> class gctools::GCKind<core::HashTableEqual_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__HashTableEqual_O ;
};
template <> class gctools::GCKind<llvmo::CompositeType_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__CompositeType_O ;
};
template <> class gctools::GCKind<llvmo::VAArgInst_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__VAArgInst_O ;
};
template <> class gctools::GCKind<llvmo::Argument_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__Argument_O ;
};
template <> class gctools::GCKind<core::CandoException_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__CandoException_O ;
};
template <> class gctools::GCKind<core::Iterator_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Iterator_O ;
};
template <> class gctools::GCKind<core::Record_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Record_O ;
};
template <> class gctools::GCKind<llvmo::SwitchInst_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__SwitchInst_O ;
};
template <> class gctools::GCKind<core::BuiltinClosure> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure ;
};
template <> class gctools::GCKind<llvmo::GlobalVariable_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__GlobalVariable_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::DynamicBinding>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_ ;
};
template <> class gctools::GCKind<core::General_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__General_O ;
};
template <> class gctools::GCKind<llvmo::BlockAddress_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__BlockAddress_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<asttooling::ErrorContent>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ErrorContent_ ;
};
template <> class gctools::GCKind<llvmo::ValueAsMetadata_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__ValueAsMetadata_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<clbind::ClassRep_O>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_clbind__ClassRep_O__ ;
};
template <> class gctools::GCKind<llvmo::Pass_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__Pass_O ;
};
template <> class gctools::GCKind<llvmo::Attribute_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__Attribute_O ;
};
template <> class gctools::GCKind<core::SourceFileInfo_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SourceFileInfo_O ;
};
template <> class gctools::GCKind<core::SingleFloat_dummy_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SingleFloat_dummy_O ;
};
template <> class gctools::GCKind<llvmo::TargetOptions_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__TargetOptions_O ;
};
template <> class gctools::GCKind<llvmo::LLVMContext_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__LLVMContext_O ;
};
template <> class gctools::GCKind<llvmo::AtomicCmpXchgInst_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__AtomicCmpXchgInst_O ;
};
template <> class gctools::GCKind<core::GlueEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__GlueEnvironment_O ;
};
#endif // defined(GC_KIND_SELECTORS)
#if defined(GC_OBJ_SKIP)
obj_skip_KIND_TEMPLATED_LISPALLOC_core__WrappedPointer_O:
{
    core::WrappedPointer_O* obj_gc_safe = reinterpret_cast<core::WrappedPointer_O*>(client);
    // client = (char*)client + AlignUp(obj_gc_safe->templatedSizeof()) + global_alignup_sizeof_header;
    size = obj_gc_safe->templatedSizeof();
    goto DONE; //return client;
}
obj_skip_KIND_TEMPLATED_CLASSALLOC_clbind__ConstructorCreator:
{
    clbind::ConstructorCreator* obj_gc_safe = reinterpret_cast<clbind::ConstructorCreator*>(client);
    // client = (char*)client + AlignUp(obj_gc_safe->templatedSizeof()) + global_alignup_sizeof_header;
    size = obj_gc_safe->templatedSizeof();
    goto DONE; //return client;
}
obj_skip_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure:
{
    core::BuiltinClosure* obj_gc_safe = reinterpret_cast<core::BuiltinClosure*>(client);
    // client = (char*)client + AlignUp(obj_gc_safe->templatedSizeof()) + global_alignup_sizeof_header;
    size = obj_gc_safe->templatedSizeof();
    goto DONE; //return client;
}
#endif // defined(GC_OBJ_SKIP)
#if defined(GC_OBJ_SKIP_HELPERS)

#endif // defined(GC_OBJ_SKIP_HELPERS)
#if defined(GC_OBJ_SKIP_TABLE)
static void* OBJ_SKIP_table[] = { 
  /* 113 */ &&obj_skip_KIND_TEMPLATED_LISPALLOC_core__WrappedPointer_O,
  /* 127 */ &&obj_skip_KIND_TEMPLATED_CLASSALLOC_clbind__ConstructorCreator,
  /* 160 */ &&obj_skip_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure,
   NULL
};
#endif // defined(GC_OBJ_SKIP_TABLE)
#if defined(GC_OBJ_SCAN)
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__:
{
}
goto SCAN_ADVANCE;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__:
{
}
goto SCAN_ADVANCE;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_:
{
}
goto SCAN_ADVANCE;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__:
{
}
goto SCAN_ADVANCE;
obj_scan_KIND_GCARRAY_gctools__GCArray_moveable_core__SlotData_0_:
{
}
goto SCAN_ADVANCE;
obj_scan_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_:
{
}
goto SCAN_ADVANCE;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_:
{
}
goto SCAN_ADVANCE;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__Message_:
{
}
goto SCAN_ADVANCE;
obj_scan_KIND_GCSTRING_gctools__GCString_moveable_char_:
{
    // Should never be invoked
}
goto SCAN_ADVANCE;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_:
{
}
goto SCAN_ADVANCE;
obj_scan_KIND_TEMPLATED_LISPALLOC_core__WrappedPointer_O:
{
}
goto SCAN_ADVANCE;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__:
{
}
goto SCAN_ADVANCE;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__:
{
}
goto SCAN_ADVANCE;
obj_scan_KIND_TEMPLATED_CLASSALLOC_clbind__ConstructorCreator:
{
}
goto SCAN_ADVANCE;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_:
{
}
goto SCAN_ADVANCE;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__:
{
}
goto SCAN_ADVANCE;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_:
{
}
goto SCAN_ADVANCE;
obj_scan_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure:
{
}
goto SCAN_ADVANCE;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_:
{
}
goto SCAN_ADVANCE;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ErrorContent_:
{
}
goto SCAN_ADVANCE;
obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_clbind__ClassRep_O__:
{
}
goto SCAN_ADVANCE;
#endif // defined(GC_OBJ_SCAN)
#if defined(GC_OBJ_SCAN_HELPERS)
{ class_kind, KIND_LISPALLOC_llvmo__APInt_O, sizeof(llvmo::APInt_O), 0, "llvmo::APInt_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::APInt_O),_Class), "_Class" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(llvmo::APInt_O),_value.BitWidth), "_value.BitWidth" },
{ class_kind, KIND_BOOTSTRAP_core__Symbol_O, sizeof(core::Symbol_O), 0, "core::Symbol_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Str_O>), offsetof(SAFE_TYPE_MACRO(core::Symbol_O),_Name), "_Name" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Symbol_O),_HomePackage), "_HomePackage" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Symbol_O),_Value), "_Value" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Symbol_O),_Function), "_Function" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Symbol_O),_SetfFunction), "_SetfFunction" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::Symbol_O),_PropertyList), "_PropertyList" },
{ class_kind, KIND_LISPALLOC_llvmo__IRBuilder_O, sizeof(llvmo::IRBuilder_O), 0, "llvmo::IRBuilder_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::IRBuilder_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_core__FunctionValueEnvironment_O, sizeof(core::FunctionValueEnvironment_O), 0, "core::FunctionValueEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::FunctionValueEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionValueEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionValueEnvironment_O),_Metadata), "_Metadata" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionValueEnvironment_O),_RuntimeEnvironment), "_RuntimeEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEqual_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionValueEnvironment_O),_FunctionIndices), "_FunctionIndices" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::FunctionFrame_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionValueEnvironment_O),_FunctionFrame), "_FunctionFrame" },
{ class_kind, KIND_LISPALLOC_llvmo__FenceInst_O, sizeof(llvmo::FenceInst_O), 0, "llvmo::FenceInst_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::FenceInst_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_core__VectorObjects_O, sizeof(core::VectorObjects_O), 0, "core::VectorObjects_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::VectorObjects_O),_ElementType), "_ElementType" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(core::VectorObjects_O),_Values._Vector._Contents), "_Values._Vector._Contents" },
{ class_kind, KIND_LISPALLOC_core__FunctionContainerEnvironment_O, sizeof(core::FunctionContainerEnvironment_O), 0, "core::FunctionContainerEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::FunctionContainerEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionContainerEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionContainerEnvironment_O),_Metadata), "_Metadata" },
{ class_kind, KIND_LISPALLOC_llvmo__ConstantArray_O, sizeof(llvmo::ConstantArray_O), 0, "llvmo::ConstantArray_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::ConstantArray_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__IntegerType_O, sizeof(llvmo::IntegerType_O), 0, "llvmo::IntegerType_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::IntegerType_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_core__Function_O, sizeof(core::Function_O), 0, "core::Function_O" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<core::Closure>), offsetof(SAFE_TYPE_MACRO(core::Function_O),closure), "closure" },
{ class_kind, KIND_LISPALLOC_llvmo__ConstantInt_O, sizeof(llvmo::ConstantInt_O), 0, "llvmo::ConstantInt_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::ConstantInt_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__BranchInst_O, sizeof(llvmo::BranchInst_O), 0, "llvmo::BranchInst_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::BranchInst_O),_Class), "_Class" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__, sizeof(gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>), 0, "gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>),_Data), "_Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), 0, "only" },
{ class_kind, KIND_BOOTSTRAP_core__StandardObject_O, sizeof(core::StandardObject_O), 0, "core::StandardObject_O" },
{ class_kind, KIND_LISPALLOC_asttooling__DerivableFrontendActionFactory, sizeof(asttooling::DerivableFrontendActionFactory), 0, "asttooling::DerivableFrontendActionFactory" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<core::Closure>), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableFrontendActionFactory),closure), "closure" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableFrontendActionFactory),_isgf), "_isgf" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableFrontendActionFactory),_Class), "_Class" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableFrontendActionFactory),_Slots._Vector._Contents), "_Slots._Vector._Contents" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableFrontendActionFactory),_Sig), "_Sig" },
{ class_kind, KIND_LISPALLOC_llvmo__AtomicRMWInst_O, sizeof(llvmo::AtomicRMWInst_O), 0, "llvmo::AtomicRMWInst_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::AtomicRMWInst_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__UnaryInstruction_O, sizeof(llvmo::UnaryInstruction_O), 0, "llvmo::UnaryInstruction_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::UnaryInstruction_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_core__DoubleFloat_O, sizeof(core::DoubleFloat_O), 0, "core::DoubleFloat_O" },
{  fixed_field, ctype_double, sizeof(double), offsetof(SAFE_TYPE_MACRO(core::DoubleFloat_O),_Value), "_Value" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__, sizeof(gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>), 0, "gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>),_Data), "_Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), 0, "only" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_, sizeof(gctools::GCVector_moveable<asttooling::RegMap::SymbolMatcherDescriptorPair>), 0, "gctools::GCVector_moveable<asttooling::RegMap::SymbolMatcherDescriptorPair>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<asttooling::RegMap::SymbolMatcherDescriptorPair>),_Data), "_Data" },
{  variable_capacity, sizeof(asttooling::RegMap::SymbolMatcherDescriptorPair), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<asttooling::RegMap::SymbolMatcherDescriptorPair>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<asttooling::RegMap::SymbolMatcherDescriptorPair>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(asttooling::RegMap::SymbolMatcherDescriptorPair),Name), "Name" },
{    variable_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<asttooling::internal::MatcherDescriptor>), offsetof(SAFE_TYPE_MACRO(asttooling::RegMap::SymbolMatcherDescriptorPair),matcher), "matcher" },
{ class_kind, KIND_LISPALLOC_llvmo__ConstantExpr_O, sizeof(llvmo::ConstantExpr_O), 0, "llvmo::ConstantExpr_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::ConstantExpr_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__LoadInst_O, sizeof(llvmo::LoadInst_O), 0, "llvmo::LoadInst_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::LoadInst_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__AttributeSet_O, sizeof(llvmo::AttributeSet_O), 0, "llvmo::AttributeSet_O" },
{ class_kind, KIND_LISPALLOC_core__Instance_O, sizeof(core::Instance_O), 0, "core::Instance_O" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<core::Closure>), offsetof(SAFE_TYPE_MACRO(core::Instance_O),closure), "closure" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::Instance_O),_isgf), "_isgf" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(core::Instance_O),_Class), "_Class" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(core::Instance_O),_Slots._Vector._Contents), "_Slots._Vector._Contents" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Instance_O),_Sig), "_Sig" },
{ class_kind, KIND_LISPALLOC_llvmo__StoreInst_O, sizeof(llvmo::StoreInst_O), 0, "llvmo::StoreInst_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::StoreInst_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__MCSubtargetInfo_O, sizeof(llvmo::MCSubtargetInfo_O), 0, "llvmo::MCSubtargetInfo_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::MCSubtargetInfo_O),_Class), "_Class" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__, sizeof(gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>), 0, "gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>),_Data), "_Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<core::Cons_O>), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Cons_O>), 0, "only" },
{ class_kind, KIND_LISPALLOC_llvmo__CallInst_O, sizeof(llvmo::CallInst_O), 0, "llvmo::CallInst_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::CallInst_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__PHINode_O, sizeof(llvmo::PHINode_O), 0, "llvmo::PHINode_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::PHINode_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__ImmutablePass_O, sizeof(llvmo::ImmutablePass_O), 0, "llvmo::ImmutablePass_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::ImmutablePass_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_core__CompiledFunction_O, sizeof(core::CompiledFunction_O), 0, "core::CompiledFunction_O" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<core::Closure>), offsetof(SAFE_TYPE_MACRO(core::CompiledFunction_O),closure), "closure" },
{ class_kind, KIND_LISPALLOC_core__Rational_O, sizeof(core::Rational_O), 0, "core::Rational_O" },
{ class_kind, KIND_LISPALLOC_core__ForeignData_O, sizeof(core::ForeignData_O), 0, "core::ForeignData_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(core::ForeignData_O),_Class), "_Class" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::ForeignData_O),_Kind), "_Kind" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::ForeignData_O),_OwnershipFlags), "_OwnershipFlags" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::ForeignData_O),_Size), "_Size" },
{ class_kind, KIND_LISPALLOC_llvmo__VectorType_O, sizeof(llvmo::VectorType_O), 0, "llvmo::VectorType_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::VectorType_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__FunctionPass_O, sizeof(llvmo::FunctionPass_O), 0, "llvmo::FunctionPass_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::FunctionPass_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__ConstantStruct_O, sizeof(llvmo::ConstantStruct_O), 0, "llvmo::ConstantStruct_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::ConstantStruct_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__TargetSubtargetInfo_O, sizeof(llvmo::TargetSubtargetInfo_O), 0, "llvmo::TargetSubtargetInfo_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::TargetSubtargetInfo_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__Instruction_O, sizeof(llvmo::Instruction_O), 0, "llvmo::Instruction_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::Instruction_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__User_O, sizeof(llvmo::User_O), 0, "llvmo::User_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::User_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_core__CatchEnvironment_O, sizeof(core::CatchEnvironment_O), 0, "core::CatchEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::CatchEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::CatchEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::CatchEnvironment_O),_Metadata), "_Metadata" },
{ class_kind, KIND_BOOTSTRAP_core__Str_O, sizeof(core::Str_O), 0, "core::Str_O" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCString_moveable<char>>), offsetof(SAFE_TYPE_MACRO(core::Str_O),_Contents._Contents), "_Contents._Contents" },
{ class_kind, KIND_LISPALLOC_core__Pointer_O, sizeof(core::Pointer_O), 0, "core::Pointer_O" },
{ class_kind, KIND_LISPALLOC_llvmo__PassManager_O, sizeof(llvmo::PassManager_O), 0, "llvmo::PassManager_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::PassManager_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_clbind__ClassRegistry_O, sizeof(clbind::ClassRegistry_O), 0, "clbind::ClassRegistry_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEql_O>), offsetof(SAFE_TYPE_MACRO(clbind::ClassRegistry_O),m_classes), "m_classes" },
{ class_kind, KIND_LISPALLOC_llvmo__PointerType_O, sizeof(llvmo::PointerType_O), 0, "llvmo::PointerType_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::PointerType_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__DataLayout_O, sizeof(llvmo::DataLayout_O), 0, "llvmo::DataLayout_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::DataLayout_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__APFloat_O, sizeof(llvmo::APFloat_O), 0, "llvmo::APFloat_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::APFloat_O),_Class), "_Class" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(llvmo::APFloat_O),_value.sign), "_value.sign" },
{ class_kind, KIND_LISPALLOC_llvmo__ResumeInst_O, sizeof(llvmo::ResumeInst_O), 0, "llvmo::ResumeInst_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::ResumeInst_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__ConstantDataArray_O, sizeof(llvmo::ConstantDataArray_O), 0, "llvmo::ConstantDataArray_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::ConstantDataArray_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_core__Number_O, sizeof(core::Number_O), 0, "core::Number_O" },
{ class_kind, KIND_LISPALLOC_llvmo__TargetLibraryInfo_O, sizeof(llvmo::TargetLibraryInfo_O), 0, "llvmo::TargetLibraryInfo_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::TargetLibraryInfo_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__UndefValue_O, sizeof(llvmo::UndefValue_O), 0, "llvmo::UndefValue_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::UndefValue_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__EngineBuilder_O, sizeof(llvmo::EngineBuilder_O), 0, "llvmo::EngineBuilder_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::EngineBuilder_O),_Class), "_Class" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(llvmo::EngineBuilder_O),_ErrorStr.__r_.__first_.NO-NAME.__l.__cap_), "_ErrorStr.__r_.__first_.NO-NAME.__l.__cap_" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(llvmo::EngineBuilder_O),_ErrorStr.__r_.__first_.NO-NAME.__l.__size_), "_ErrorStr.__r_.__first_.NO-NAME.__l.__size_" },
{  fixed_field, ctype_char, sizeof(char), offsetof(SAFE_TYPE_MACRO(llvmo::EngineBuilder_O),_ErrorStr.__r_.__first_.NO-NAME.__s.NO-NAME.__lx), "_ErrorStr.__r_.__first_.NO-NAME.__s.NO-NAME.__lx" },
{  fixed_field, ARRAY_OFFSET, sizeof(NIL), offsetof(SAFE_TYPE_MACRO(llvmo::EngineBuilder_O),_ErrorStr.__r_.__first_.NO-NAME.__s.__data_), "_ErrorStr.__r_.__first_.NO-NAME.__s.__data_" },
{  fixed_field, ARRAY_OFFSET, sizeof(NIL), offsetof(SAFE_TYPE_MACRO(llvmo::EngineBuilder_O),_ErrorStr.__r_.__first_.NO-NAME.__r.__words), "_ErrorStr.__r_.__first_.NO-NAME.__r.__words" },
{ class_kind, KIND_LISPALLOC_core__Environment_O, sizeof(core::Environment_O), 0, "core::Environment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::Environment_O),_EnvId), "_EnvId" },
{ container_kind, KIND_GCARRAY_gctools__GCArray_moveable_core__SlotData_0_, sizeof(gctools::GCArray_moveable<core::SlotData,0>), 0, "gctools::GCArray_moveable<core::SlotData,0>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCArray_moveable<core::SlotData,0>),_Data), "_Data" },
{  variable_capacity, sizeof(core::SlotData), offsetof(SAFE_TYPE_MACRO(gctools::GCArray_moveable<core::SlotData,0>),_Capacity), offsetof(SAFE_TYPE_MACRO(gctools::GCArray_moveable<core::SlotData,0>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::SlotData),_APtr), "_APtr" },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::SlotData),_BPtr), "_BPtr" },
{    variable_field, ctype_double, sizeof(double), offsetof(SAFE_TYPE_MACRO(core::SlotData),_C), "_C" },
{ class_kind, KIND_LISPALLOC_llvmo__ReturnInst_O, sizeof(llvmo::ReturnInst_O), 0, "llvmo::ReturnInst_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::ReturnInst_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__MDString_O, sizeof(llvmo::MDString_O), 0, "llvmo::MDString_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::MDString_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_core__Complex_O, sizeof(core::Complex_O), 0, "core::Complex_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Real_O>), offsetof(SAFE_TYPE_MACRO(core::Complex_O),_real), "_real" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Real_O>), offsetof(SAFE_TYPE_MACRO(core::Complex_O),_imaginary), "_imaginary" },
{ class_kind, KIND_LISPALLOC_core__SourceManager_O, sizeof(core::SourceManager_O), 0, "core::SourceManager_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::WeakKeyHashTable_O>), offsetof(SAFE_TYPE_MACRO(core::SourceManager_O),_SourcePosInfo), "_SourcePosInfo" },
{ class_kind, KIND_LISPALLOC_llvmo__LLVMTargetMachine_O, sizeof(llvmo::LLVMTargetMachine_O), 0, "llvmo::LLVMTargetMachine_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::LLVMTargetMachine_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_core__LexicalEnvironment_O, sizeof(core::LexicalEnvironment_O), 0, "core::LexicalEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::LexicalEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::LexicalEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::LexicalEnvironment_O),_Metadata), "_Metadata" },
{ class_kind, KIND_LISPALLOC_core__SymbolMacroletEnvironment_O, sizeof(core::SymbolMacroletEnvironment_O), 0, "core::SymbolMacroletEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::SymbolMacroletEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::SymbolMacroletEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::SymbolMacroletEnvironment_O),_Metadata), "_Metadata" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::SymbolMacroletEnvironment_O),_Macros), "_Macros" },
{ class_kind, KIND_LISPALLOC_core__StackValueEnvironment_O, sizeof(core::StackValueEnvironment_O), 0, "core::StackValueEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::StackValueEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::StackValueEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::StackValueEnvironment_O),_Metadata), "_Metadata" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::StackValueEnvironment_O),_Values), "_Values" },
{ class_kind, KIND_LISPALLOC_llvmo__PassManagerBuilder_O, sizeof(llvmo::PassManagerBuilder_O), 0, "llvmo::PassManagerBuilder_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::PassManagerBuilder_O),_Class), "_Class" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(llvmo::PassManagerBuilder_O),_ErrorStr.__r_.__first_.NO-NAME.__l.__cap_), "_ErrorStr.__r_.__first_.NO-NAME.__l.__cap_" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(llvmo::PassManagerBuilder_O),_ErrorStr.__r_.__first_.NO-NAME.__l.__size_), "_ErrorStr.__r_.__first_.NO-NAME.__l.__size_" },
{  fixed_field, ctype_char, sizeof(char), offsetof(SAFE_TYPE_MACRO(llvmo::PassManagerBuilder_O),_ErrorStr.__r_.__first_.NO-NAME.__s.NO-NAME.__lx), "_ErrorStr.__r_.__first_.NO-NAME.__s.NO-NAME.__lx" },
{  fixed_field, ARRAY_OFFSET, sizeof(NIL), offsetof(SAFE_TYPE_MACRO(llvmo::PassManagerBuilder_O),_ErrorStr.__r_.__first_.NO-NAME.__s.__data_), "_ErrorStr.__r_.__first_.NO-NAME.__s.__data_" },
{  fixed_field, ARRAY_OFFSET, sizeof(NIL), offsetof(SAFE_TYPE_MACRO(llvmo::PassManagerBuilder_O),_ErrorStr.__r_.__first_.NO-NAME.__r.__words), "_ErrorStr.__r_.__first_.NO-NAME.__r.__words" },
{ class_kind, KIND_LISPALLOC_asttooling__DerivableSyntaxOnlyAction, sizeof(asttooling::DerivableSyntaxOnlyAction), 0, "asttooling::DerivableSyntaxOnlyAction" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<core::Closure>), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableSyntaxOnlyAction),closure), "closure" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableSyntaxOnlyAction),_isgf), "_isgf" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableSyntaxOnlyAction),_Class), "_Class" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableSyntaxOnlyAction),_Slots._Vector._Contents), "_Slots._Vector._Contents" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableSyntaxOnlyAction),_Sig), "_Sig" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableSyntaxOnlyAction),CurrentInput.File.__r_.__first_.NO-NAME.__l.__cap_), "CurrentInput.File.__r_.__first_.NO-NAME.__l.__cap_" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableSyntaxOnlyAction),CurrentInput.File.__r_.__first_.NO-NAME.__l.__size_), "CurrentInput.File.__r_.__first_.NO-NAME.__l.__size_" },
{  fixed_field, ctype_char, sizeof(char), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableSyntaxOnlyAction),CurrentInput.File.__r_.__first_.NO-NAME.__s.NO-NAME.__lx), "CurrentInput.File.__r_.__first_.NO-NAME.__s.NO-NAME.__lx" },
{  fixed_field, ARRAY_OFFSET, sizeof(NIL), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableSyntaxOnlyAction),CurrentInput.File.__r_.__first_.NO-NAME.__s.__data_), "CurrentInput.File.__r_.__first_.NO-NAME.__s.__data_" },
{  fixed_field, ARRAY_OFFSET, sizeof(NIL), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableSyntaxOnlyAction),CurrentInput.File.__r_.__first_.NO-NAME.__r.__words), "CurrentInput.File.__r_.__first_.NO-NAME.__r.__words" },
{ class_kind, KIND_LISPALLOC_core__CompileTimeEnvironment_O, sizeof(core::CompileTimeEnvironment_O), 0, "core::CompileTimeEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::CompileTimeEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::CompileTimeEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::CompileTimeEnvironment_O),_Metadata), "_Metadata" },
{ class_kind, KIND_LISPALLOC_llvmo__ExecutionEngine_O, sizeof(llvmo::ExecutionEngine_O), 0, "llvmo::ExecutionEngine_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::ExecutionEngine_O),_Class), "_Class" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEqual_O>), offsetof(SAFE_TYPE_MACRO(llvmo::ExecutionEngine_O),_DependentModules), "_DependentModules" },
{ class_kind, KIND_LISPALLOC_core__TagbodyFrame_O, sizeof(core::TagbodyFrame_O), 0, "core::TagbodyFrame_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::TagbodyFrame_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::TagbodyFrame_O),_ParentFrame), "_ParentFrame" },
{ container_kind, KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_, sizeof(gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>), 0, "gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>),_Data), "_Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>),_Capacity), offsetof(SAFE_TYPE_MACRO(gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), 0, "only" },
{ class_kind, KIND_LISPALLOC_core__MacroletEnvironment_O, sizeof(core::MacroletEnvironment_O), 0, "core::MacroletEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::MacroletEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::MacroletEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::MacroletEnvironment_O),_Metadata), "_Metadata" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::MacroletEnvironment_O),_Macros), "_Macros" },
{ class_kind, KIND_LISPALLOC_llvmo__Linker_O, sizeof(llvmo::Linker_O), 0, "llvmo::Linker_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::Linker_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__DataLayoutPass_O, sizeof(llvmo::DataLayoutPass_O), 0, "llvmo::DataLayoutPass_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::DataLayoutPass_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__NamedMDNode_O, sizeof(llvmo::NamedMDNode_O), 0, "llvmo::NamedMDNode_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::NamedMDNode_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__ConstantPointerNull_O, sizeof(llvmo::ConstantPointerNull_O), 0, "llvmo::ConstantPointerNull_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::ConstantPointerNull_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__Constant_O, sizeof(llvmo::Constant_O), 0, "llvmo::Constant_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::Constant_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_core__String_O, sizeof(core::String_O), 0, "core::String_O" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_, sizeof(gctools::GCVector_moveable<core::ExceptionEntry>), 0, "gctools::GCVector_moveable<core::ExceptionEntry>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::ExceptionEntry>),_Data), "_Data" },
{  variable_capacity, sizeof(core::ExceptionEntry), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::ExceptionEntry>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::ExceptionEntry>),_Capacity), NULL },
{    variable_field, ctype_core__FrameKind, sizeof(core::FrameKind), offsetof(SAFE_TYPE_MACRO(core::ExceptionEntry),_FrameKind), "_FrameKind" },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::ExceptionEntry),_Key), "_Key" },
{ class_kind, KIND_LISPALLOC_core__SymbolToEnumConverter_O, sizeof(core::SymbolToEnumConverter_O), 0, "core::SymbolToEnumConverter_O" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCString_moveable<char>>), offsetof(SAFE_TYPE_MACRO(core::SymbolToEnumConverter_O),_WhatTheEnumsRepresent._Contents), "_WhatTheEnumsRepresent._Contents" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEql_O>), offsetof(SAFE_TYPE_MACRO(core::SymbolToEnumConverter_O),_EnumToSymbol), "_EnumToSymbol" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::SymbolToEnumConverter_O),_ArchiveSymbolToEnum), "_ArchiveSymbolToEnum" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEql_O>), offsetof(SAFE_TYPE_MACRO(core::SymbolToEnumConverter_O),_EnumToArchiveSymbol), "_EnumToArchiveSymbol" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::SymbolToEnumConverter_O),_SymbolToEnum), "_SymbolToEnum" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__Message_, sizeof(gctools::GCVector_moveable<asttooling::Message>), 0, "gctools::GCVector_moveable<asttooling::Message>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<asttooling::Message>),_Data), "_Data" },
{  variable_capacity, sizeof(asttooling::Message), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<asttooling::Message>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<asttooling::Message>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Cons_O>), offsetof(SAFE_TYPE_MACRO(asttooling::Message),Range), "Range" },
{ class_kind, KIND_LISPALLOC_clbind__ClassRep_O, sizeof(clbind::ClassRep_O), 0, "clbind::ClassRep_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(clbind::ClassRep_O),_Signature_ClassSlots), "_Signature_ClassSlots" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<core::Creator>), offsetof(SAFE_TYPE_MACRO(clbind::ClassRep_O),_theCreator), "_theCreator" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(clbind::ClassRep_O),_MetaClassSlots._Vector._Contents), "_MetaClassSlots._Vector._Contents" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>>), offsetof(SAFE_TYPE_MACRO(clbind::ClassRep_O),m_bases._Vector._Contents), "m_bases._Vector._Contents" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(clbind::ClassRep_O),m_name.__r_.__first_.NO-NAME.__l.__cap_), "m_name.__r_.__first_.NO-NAME.__l.__cap_" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(clbind::ClassRep_O),m_name.__r_.__first_.NO-NAME.__l.__size_), "m_name.__r_.__first_.NO-NAME.__l.__size_" },
{  fixed_field, ctype_char, sizeof(char), offsetof(SAFE_TYPE_MACRO(clbind::ClassRep_O),m_name.__r_.__first_.NO-NAME.__s.NO-NAME.__lx), "m_name.__r_.__first_.NO-NAME.__s.NO-NAME.__lx" },
{  fixed_field, ARRAY_OFFSET, sizeof(NIL), offsetof(SAFE_TYPE_MACRO(clbind::ClassRep_O),m_name.__r_.__first_.NO-NAME.__s.__data_), "m_name.__r_.__first_.NO-NAME.__s.__data_" },
{  fixed_field, ARRAY_OFFSET, sizeof(NIL), offsetof(SAFE_TYPE_MACRO(clbind::ClassRep_O),m_name.__r_.__first_.NO-NAME.__r.__words), "m_name.__r_.__first_.NO-NAME.__r.__words" },
{ class_kind, KIND_LISPALLOC_llvmo__Value_O, sizeof(llvmo::Value_O), 0, "llvmo::Value_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::Value_O),_Class), "_Class" },
{ container_kind, KIND_GCSTRING_gctools__GCString_moveable_char_, sizeof(gctools::GCString_moveable<char>), 0, "gctools::GCString_moveable<char>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCString_moveable<char>),_Data), "_Data" },
{  variable_capacity, sizeof(char), offsetof(SAFE_TYPE_MACRO(gctools::GCString_moveable<char>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCString_moveable<char>),_Capacity), NULL },
{    variable_field, ctype_char, sizeof(char), 0, "only" },
{ container_jump_table_index, 8, 0, 0, "" },
{ class_kind, KIND_LISPALLOC_llvmo__Function_O, sizeof(llvmo::Function_O), 0, "llvmo::Function_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::Function_O),_Class), "_Class" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::LoadTimeValues_O>), offsetof(SAFE_TYPE_MACRO(llvmo::Function_O),_RunTimeValues), "_RunTimeValues" },
{ class_kind, KIND_LISPALLOC_llvmo__Type_O, sizeof(llvmo::Type_O), 0, "llvmo::Type_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::Type_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_asttooling__DerivableMatchCallback, sizeof(asttooling::DerivableMatchCallback), 0, "asttooling::DerivableMatchCallback" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<core::Closure>), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableMatchCallback),closure), "closure" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableMatchCallback),_isgf), "_isgf" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableMatchCallback),_Class), "_Class" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableMatchCallback),_Slots._Vector._Contents), "_Slots._Vector._Contents" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableMatchCallback),_Sig), "_Sig" },
{ class_kind, KIND_LISPALLOC_llvmo__LandingPadInst_O, sizeof(llvmo::LandingPadInst_O), 0, "llvmo::LandingPadInst_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::LandingPadInst_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_core__Vector_O, sizeof(core::Vector_O), 0, "core::Vector_O" },
{ class_kind, KIND_LISPALLOC_core__UnwindProtectEnvironment_O, sizeof(core::UnwindProtectEnvironment_O), 0, "core::UnwindProtectEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::UnwindProtectEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::UnwindProtectEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::UnwindProtectEnvironment_O),_Metadata), "_Metadata" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::UnwindProtectEnvironment_O),_CleanupForm), "_CleanupForm" },
{ class_kind, KIND_BOOTSTRAP_core__Metaobject_O, sizeof(core::Metaobject_O), 0, "core::Metaobject_O" },
{ class_kind, KIND_LISPALLOC_llvmo__InvokeInst_O, sizeof(llvmo::InvokeInst_O), 0, "llvmo::InvokeInst_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::InvokeInst_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_core__Real_O, sizeof(core::Real_O), 0, "core::Real_O" },
{ class_kind, KIND_LISPALLOC_core__Array_O, sizeof(core::Array_O), 0, "core::Array_O" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_, sizeof(gctools::GCVector_moveable<core::CacheRecord>), 0, "gctools::GCVector_moveable<core::CacheRecord>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::CacheRecord>),_Data), "_Data" },
{  variable_capacity, sizeof(core::CacheRecord), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::CacheRecord>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::CacheRecord>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::CacheRecord),_key), "_key" },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::CacheRecord),_value), "_value" },
{    variable_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::CacheRecord),_generation), "_generation" },
{ class_kind, KIND_LISPALLOC_core__ShortFloat_O, sizeof(core::ShortFloat_O), 0, "core::ShortFloat_O" },
{  fixed_field, ctype_float, sizeof(float), offsetof(SAFE_TYPE_MACRO(core::ShortFloat_O),_Value), "_Value" },
{ class_kind, KIND_LISPALLOC_core__BlockEnvironment_O, sizeof(core::BlockEnvironment_O), 0, "core::BlockEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::BlockEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::BlockEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::BlockEnvironment_O),_Metadata), "_Metadata" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(core::BlockEnvironment_O),_BlockSymbol), "_BlockSymbol" },
{ class_kind, KIND_LISPALLOC_core__Fixnum_dummy_O, sizeof(core::Fixnum_dummy_O), 0, "core::Fixnum_dummy_O" },
{ class_kind, KIND_LISPALLOC_llvmo__PassManagerBase_O, sizeof(llvmo::PassManagerBase_O), 0, "llvmo::PassManagerBase_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::PassManagerBase_O),_Class), "_Class" },
{ class_kind, KIND_BOOTSTRAP_core__Class_O, sizeof(core::Class_O), 0, "core::Class_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Class_O),_Signature_ClassSlots), "_Signature_ClassSlots" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<core::Creator>), offsetof(SAFE_TYPE_MACRO(core::Class_O),_theCreator), "_theCreator" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(core::Class_O),_MetaClassSlots._Vector._Contents), "_MetaClassSlots._Vector._Contents" },
{ class_kind, KIND_LISPALLOC_asttooling__DerivableASTFrontendAction, sizeof(asttooling::DerivableASTFrontendAction), 0, "asttooling::DerivableASTFrontendAction" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<core::Closure>), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableASTFrontendAction),closure), "closure" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableASTFrontendAction),_isgf), "_isgf" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableASTFrontendAction),_Class), "_Class" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableASTFrontendAction),_Slots._Vector._Contents), "_Slots._Vector._Contents" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableASTFrontendAction),_Sig), "_Sig" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableASTFrontendAction),CurrentInput.File.__r_.__first_.NO-NAME.__l.__cap_), "CurrentInput.File.__r_.__first_.NO-NAME.__l.__cap_" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableASTFrontendAction),CurrentInput.File.__r_.__first_.NO-NAME.__l.__size_), "CurrentInput.File.__r_.__first_.NO-NAME.__l.__size_" },
{  fixed_field, ctype_char, sizeof(char), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableASTFrontendAction),CurrentInput.File.__r_.__first_.NO-NAME.__s.NO-NAME.__lx), "CurrentInput.File.__r_.__first_.NO-NAME.__s.NO-NAME.__lx" },
{  fixed_field, ARRAY_OFFSET, sizeof(NIL), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableASTFrontendAction),CurrentInput.File.__r_.__first_.NO-NAME.__s.__data_), "CurrentInput.File.__r_.__first_.NO-NAME.__s.__data_" },
{  fixed_field, ARRAY_OFFSET, sizeof(NIL), offsetof(SAFE_TYPE_MACRO(asttooling::DerivableASTFrontendAction),CurrentInput.File.__r_.__first_.NO-NAME.__r.__words), "CurrentInput.File.__r_.__first_.NO-NAME.__r.__words" },
{ class_kind, KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O, sizeof(core::VectorObjectsWithFillPtr_O), 0, "core::VectorObjectsWithFillPtr_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::VectorObjectsWithFillPtr_O),_ElementType), "_ElementType" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(core::VectorObjectsWithFillPtr_O),_Values._Vector._Contents), "_Values._Vector._Contents" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::VectorObjectsWithFillPtr_O),_FillPtr), "_FillPtr" },
{ class_kind, KIND_BOOTSTRAP_core__BuiltInClass_O, sizeof(core::BuiltInClass_O), 0, "core::BuiltInClass_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::BuiltInClass_O),_Signature_ClassSlots), "_Signature_ClassSlots" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<core::Creator>), offsetof(SAFE_TYPE_MACRO(core::BuiltInClass_O),_theCreator), "_theCreator" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>>), offsetof(SAFE_TYPE_MACRO(core::BuiltInClass_O),_MetaClassSlots._Vector._Contents), "_MetaClassSlots._Vector._Contents" },
{ class_kind, KIND_LISPALLOC_core__ValueEnvironment_O, sizeof(core::ValueEnvironment_O), 0, "core::ValueEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::ValueEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::ValueEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::ValueEnvironment_O),_Metadata), "_Metadata" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::ValueEnvironment_O),_RuntimeEnvironment), "_RuntimeEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::ValueEnvironment_O),_SymbolIndex), "_SymbolIndex" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::ActivationFrame_O>), offsetof(SAFE_TYPE_MACRO(core::ValueEnvironment_O),_ActivationFrame), "_ActivationFrame" },
{ class_kind, KIND_LISPALLOC_llvmo__IndirectBrInst_O, sizeof(llvmo::IndirectBrInst_O), 0, "llvmo::IndirectBrInst_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::IndirectBrInst_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__Target_O, sizeof(llvmo::Target_O), 0, "llvmo::Target_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::Target_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__Module_O, sizeof(llvmo::Module_O), 0, "llvmo::Module_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::Module_O),_Class), "_Class" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEqual_O>), offsetof(SAFE_TYPE_MACRO(llvmo::Module_O),_UniqueGlobalVariableStrings), "_UniqueGlobalVariableStrings" },
{ class_kind, KIND_LISPALLOC_core__FunctionFrame_O, sizeof(core::FunctionFrame_O), 0, "core::FunctionFrame_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::FunctionFrame_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::FunctionFrame_O),_ParentFrame), "_ParentFrame" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>>), offsetof(SAFE_TYPE_MACRO(core::FunctionFrame_O),_Objects._Array._Contents), "_Objects._Array._Contents" },
{ class_kind, KIND_LISPALLOC_core__Bignum_O, sizeof(core::Bignum_O), 0, "core::Bignum_O" },
{  fixed_field, ARRAY_OFFSET, sizeof(NIL), offsetof(SAFE_TYPE_MACRO(core::Bignum_O),_value.mp), "_value.mp" },
{ class_kind, KIND_LISPALLOC_core__ExternalObject_O, sizeof(core::ExternalObject_O), 0, "core::ExternalObject_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(core::ExternalObject_O),_Class), "_Class" },
{ templated_kind, KIND_TEMPLATED_LISPALLOC_core__WrappedPointer_O, sizeof(core::WrappedPointer_O), 0, "core::WrappedPointer_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(core::WrappedPointer_O),_Class), "_Class" },
{ templated_class_jump_table_index, 10, 0, 0, "" },
{ class_kind, KIND_LISPALLOC_core__Ratio_O, sizeof(core::Ratio_O), 0, "core::Ratio_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Integer_O>), offsetof(SAFE_TYPE_MACRO(core::Ratio_O),_numerator), "_numerator" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Integer_O>), offsetof(SAFE_TYPE_MACRO(core::Ratio_O),_denominator), "_denominator" },
{ class_kind, KIND_LISPALLOC_core__WeakHashTable_O, sizeof(core::WeakHashTable_O), 0, "core::WeakHashTable_O" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__, sizeof(gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>), 0, "gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>),_Data), "_Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), 0, "only" },
{ class_kind, KIND_LISPALLOC_llvmo__BasicBlock_O, sizeof(llvmo::BasicBlock_O), 0, "llvmo::BasicBlock_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::BasicBlock_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__TerminatorInst_O, sizeof(llvmo::TerminatorInst_O), 0, "llvmo::TerminatorInst_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::TerminatorInst_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__FunctionType_O, sizeof(llvmo::FunctionType_O), 0, "llvmo::FunctionType_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::FunctionType_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_core__WeakKeyHashTable_O, sizeof(core::WeakKeyHashTable_O), 0, "core::WeakKeyHashTable_O" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::WeakKeyHashTable_O),_HashTable._Length), "_HashTable._Length" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::Buckets<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>,gctools::WeakLinks>>), offsetof(SAFE_TYPE_MACRO(core::WeakKeyHashTable_O),_HashTable._Keys), "_HashTable._Keys" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::Buckets<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>,gctools::StrongLinks>>), offsetof(SAFE_TYPE_MACRO(core::WeakKeyHashTable_O),_HashTable._Values), "_HashTable._Values" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::WeakKeyHashTable_O),_HashTable._LocationDependency._epoch), "_HashTable._LocationDependency._epoch" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::WeakKeyHashTable_O),_HashTable._LocationDependency._rs), "_HashTable._LocationDependency._rs" },
{ class_kind, KIND_LISPALLOC_core__CxxObject_O, sizeof(core::CxxObject_O), 0, "core::CxxObject_O" },
{ class_kind, KIND_LISPALLOC_llvmo__ModulePass_O, sizeof(llvmo::ModulePass_O), 0, "llvmo::ModulePass_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::ModulePass_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_core__TagbodyEnvironment_O, sizeof(core::TagbodyEnvironment_O), 0, "core::TagbodyEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::TagbodyEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::TagbodyEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::TagbodyEnvironment_O),_Metadata), "_Metadata" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::TagbodyEnvironment_O),_RuntimeEnvironment), "_RuntimeEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::TagbodyEnvironment_O),_Tags), "_Tags" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>>), offsetof(SAFE_TYPE_MACRO(core::TagbodyEnvironment_O),_TagCode._Vector._Contents), "_TagCode._Vector._Contents" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::ActivationFrame_O>), offsetof(SAFE_TYPE_MACRO(core::TagbodyEnvironment_O),_ActivationFrame), "_ActivationFrame" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__, sizeof(gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>), 0, "gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>),_Data), "_Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<core::SourceFileInfo_O>), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::SourceFileInfo_O>), 0, "only" },
{ class_kind, KIND_LISPALLOC_llvmo__FunctionPassManager_O, sizeof(llvmo::FunctionPassManager_O), 0, "llvmo::FunctionPassManager_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::FunctionPassManager_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__ConstantDataSequential_O, sizeof(llvmo::ConstantDataSequential_O), 0, "llvmo::ConstantDataSequential_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::ConstantDataSequential_O),_Class), "_Class" },
{ templated_kind, KIND_TEMPLATED_CLASSALLOC_clbind__ConstructorCreator, sizeof(clbind::ConstructorCreator), 0, "clbind::ConstructorCreator" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(clbind::ConstructorCreator),_mostDerivedClassSymbol), "_mostDerivedClassSymbol" },
{ templated_class_jump_table_index, 13, 0, 0, "" },
{ class_kind, KIND_LISPALLOC_llvmo__GlobalValue_O, sizeof(llvmo::GlobalValue_O), 0, "llvmo::GlobalValue_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::GlobalValue_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__TargetMachine_O, sizeof(llvmo::TargetMachine_O), 0, "llvmo::TargetMachine_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::TargetMachine_O),_Class), "_Class" },
{ class_kind, KIND_CLASSALLOC_core__MacroClosure, sizeof(core::MacroClosure), 0, "core::MacroClosure" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::MacroClosure),name), "name" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::MacroClosure),closedEnvironment), "closedEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(core::MacroClosure),kind), "kind" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::MacroClosure),_cleavir_ast), "_cleavir_ast" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::MacroClosure),_sourceFileInfoHandle), "_sourceFileInfoHandle" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::MacroClosure),_filePos), "_filePos" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::MacroClosure),_lineno), "_lineno" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::MacroClosure),_column), "_column" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::LambdaListHandler_O>), offsetof(SAFE_TYPE_MACRO(core::MacroClosure),_lambdaListHandler), "_lambdaListHandler" },
{ class_kind, KIND_LISPALLOC_core__Integer_O, sizeof(core::Integer_O), 0, "core::Integer_O" },
{ class_kind, KIND_LISPALLOC_llvmo__UnreachableInst_O, sizeof(llvmo::UnreachableInst_O), 0, "llvmo::UnreachableInst_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::UnreachableInst_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__Triple_O, sizeof(llvmo::Triple_O), 0, "llvmo::Triple_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::Triple_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__AllocaInst_O, sizeof(llvmo::AllocaInst_O), 0, "llvmo::AllocaInst_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::AllocaInst_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__MDNode_O, sizeof(llvmo::MDNode_O), 0, "llvmo::MDNode_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::MDNode_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__StructType_O, sizeof(llvmo::StructType_O), 0, "llvmo::StructType_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::StructType_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__IRBuilderBase_O, sizeof(llvmo::IRBuilderBase_O), 0, "llvmo::IRBuilderBase_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::IRBuilderBase_O),_Class), "_Class" },
{ class_kind, KIND_BOOTSTRAP_core__Specializer_O, sizeof(core::Specializer_O), 0, "core::Specializer_O" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_, sizeof(gctools::GCVector_moveable<asttooling::ContextFrame>), 0, "gctools::GCVector_moveable<asttooling::ContextFrame>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<asttooling::ContextFrame>),_Data), "_Data" },
{  variable_capacity, sizeof(asttooling::ContextFrame), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<asttooling::ContextFrame>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<asttooling::ContextFrame>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Cons_O>), offsetof(SAFE_TYPE_MACRO(asttooling::ContextFrame),Range), "Range" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__, sizeof(gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>), 0, "gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>),_Data), "_Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<core::Package_O>), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Package_O>), 0, "only" },
{ class_kind, KIND_LISPALLOC_core__SourcePosInfo_O, sizeof(core::SourcePosInfo_O), 0, "core::SourcePosInfo_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::SourcePosInfo_O),_FileId), "_FileId" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::SourcePosInfo_O),_Filepos), "_Filepos" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::SourcePosInfo_O),_Lineno), "_Lineno" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::SourcePosInfo_O),_Column), "_Column" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_, sizeof(gctools::GCVector_moveable<core::SymbolClassPair>), 0, "gctools::GCVector_moveable<core::SymbolClassPair>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::SymbolClassPair>),_Data), "_Data" },
{  variable_capacity, sizeof(core::SymbolClassPair), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::SymbolClassPair>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::SymbolClassPair>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(core::SymbolClassPair),symbol), "symbol" },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(core::SymbolClassPair),theClass), "theClass" },
{ class_kind, KIND_LISPALLOC_core__Float_O, sizeof(core::Float_O), 0, "core::Float_O" },
{ class_kind, KIND_LISPALLOC_core__LongFloat_O, sizeof(core::LongFloat_O), 0, "core::LongFloat_O" },
{ class_kind, KIND_LISPALLOC_llvmo__ArrayType_O, sizeof(llvmo::ArrayType_O), 0, "llvmo::ArrayType_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::ArrayType_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_core__ValueFrame_O, sizeof(core::ValueFrame_O), 0, "core::ValueFrame_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::ValueFrame_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::ValueFrame_O),_ParentFrame), "_ParentFrame" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>>), offsetof(SAFE_TYPE_MACRO(core::ValueFrame_O),_Objects._Array._Contents), "_Objects._Array._Contents" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::ValueFrame_O),_DebuggingInfo), "_DebuggingInfo" },
{ class_kind, KIND_LISPALLOC_llvmo__Metadata_O, sizeof(llvmo::Metadata_O), 0, "llvmo::Metadata_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::Metadata_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O, sizeof(core::RuntimeVisibleEnvironment_O), 0, "core::RuntimeVisibleEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::RuntimeVisibleEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::RuntimeVisibleEnvironment_O),_ParentEnvironment), "_ParentEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::RuntimeVisibleEnvironment_O),_Metadata), "_Metadata" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::RuntimeVisibleEnvironment_O),_RuntimeEnvironment), "_RuntimeEnvironment" },
{ class_kind, KIND_LISPALLOC_llvmo__ConstantFP_O, sizeof(llvmo::ConstantFP_O), 0, "llvmo::ConstantFP_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::ConstantFP_O),_Class), "_Class" },
{ class_kind, KIND_CLASSALLOC_clbind__DummyCreator, sizeof(clbind::DummyCreator), 0, "clbind::DummyCreator" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(clbind::DummyCreator),_name.__r_.__first_.NO-NAME.__l.__cap_), "_name.__r_.__first_.NO-NAME.__l.__cap_" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(clbind::DummyCreator),_name.__r_.__first_.NO-NAME.__l.__size_), "_name.__r_.__first_.NO-NAME.__l.__size_" },
{  fixed_field, ctype_char, sizeof(char), offsetof(SAFE_TYPE_MACRO(clbind::DummyCreator),_name.__r_.__first_.NO-NAME.__s.NO-NAME.__lx), "_name.__r_.__first_.NO-NAME.__s.NO-NAME.__lx" },
{  fixed_field, ARRAY_OFFSET, sizeof(NIL), offsetof(SAFE_TYPE_MACRO(clbind::DummyCreator),_name.__r_.__first_.NO-NAME.__s.__data_), "_name.__r_.__first_.NO-NAME.__s.__data_" },
{  fixed_field, ARRAY_OFFSET, sizeof(NIL), offsetof(SAFE_TYPE_MACRO(clbind::DummyCreator),_name.__r_.__first_.NO-NAME.__r.__words), "_name.__r_.__first_.NO-NAME.__r.__words" },
{ class_kind, KIND_LISPALLOC_llvmo__SequentialType_O, sizeof(llvmo::SequentialType_O), 0, "llvmo::SequentialType_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::SequentialType_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_core__HashTableEqual_O, sizeof(core::HashTableEqual_O), 0, "core::HashTableEqual_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::HashTableEqual_O),_InitialSize), "_InitialSize" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Number_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEqual_O),_RehashSize), "_RehashSize" },
{  fixed_field, ctype_double, sizeof(double), offsetof(SAFE_TYPE_MACRO(core::HashTableEqual_O),_RehashThreshold), "_RehashThreshold" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::VectorObjects_O>), offsetof(SAFE_TYPE_MACRO(core::HashTableEqual_O),_HashTable), "_HashTable" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::HashTableEqual_O),_HashTableCount), "_HashTableCount" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::HashTableEqual_O),_LocationDependencyTracker._epoch), "_LocationDependencyTracker._epoch" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::HashTableEqual_O),_LocationDependencyTracker._rs), "_LocationDependencyTracker._rs" },
{ class_kind, KIND_LISPALLOC_llvmo__CompositeType_O, sizeof(llvmo::CompositeType_O), 0, "llvmo::CompositeType_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::CompositeType_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__VAArgInst_O, sizeof(llvmo::VAArgInst_O), 0, "llvmo::VAArgInst_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::VAArgInst_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__Argument_O, sizeof(llvmo::Argument_O), 0, "llvmo::Argument_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::Argument_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_core__CandoException_O, sizeof(core::CandoException_O), 0, "core::CandoException_O" },
{  fixed_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCString_moveable<char>>), offsetof(SAFE_TYPE_MACRO(core::CandoException_O),_message._Contents), "_message._Contents" },
{ class_kind, KIND_LISPALLOC_core__Iterator_O, sizeof(core::Iterator_O), 0, "core::Iterator_O" },
{ class_kind, KIND_LISPALLOC_core__Record_O, sizeof(core::Record_O), 0, "core::Record_O" },
{  fixed_field, ctype_core__Record_O__RecordStage, sizeof(core::Record_O::RecordStage), offsetof(SAFE_TYPE_MACRO(core::Record_O),_stage), "_stage" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::Record_O),_alist), "_alist" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Record_O),_replacement_table), "_replacement_table" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::Record_O),_Seen), "_Seen" },
{ class_kind, KIND_LISPALLOC_llvmo__SwitchInst_O, sizeof(llvmo::SwitchInst_O), 0, "llvmo::SwitchInst_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::SwitchInst_O),_Class), "_Class" },
{ templated_kind, KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure, sizeof(core::BuiltinClosure), 0, "core::BuiltinClosure" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::BuiltinClosure),name), "name" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::BuiltinClosure),closedEnvironment), "closedEnvironment" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(core::BuiltinClosure),kind), "kind" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::BuiltinClosure),_cleavir_ast), "_cleavir_ast" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::BuiltinClosure),_sourceFileInfoHandle), "_sourceFileInfoHandle" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::BuiltinClosure),_filePos), "_filePos" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::BuiltinClosure),_lineno), "_lineno" },
{  fixed_field, ctype_long, sizeof(long), offsetof(SAFE_TYPE_MACRO(core::BuiltinClosure),_column), "_column" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::LambdaListHandler_O>), offsetof(SAFE_TYPE_MACRO(core::BuiltinClosure),_lambdaListHandler), "_lambdaListHandler" },
{ templated_class_jump_table_index, 17, 0, 0, "" },
{ class_kind, KIND_LISPALLOC_llvmo__GlobalVariable_O, sizeof(llvmo::GlobalVariable_O), 0, "llvmo::GlobalVariable_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::GlobalVariable_O),_Class), "_Class" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_, sizeof(gctools::GCVector_moveable<core::DynamicBinding>), 0, "gctools::GCVector_moveable<core::DynamicBinding>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::DynamicBinding>),_Data), "_Data" },
{  variable_capacity, sizeof(core::DynamicBinding), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::DynamicBinding>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<core::DynamicBinding>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Symbol_O>), offsetof(SAFE_TYPE_MACRO(core::DynamicBinding),_Var), "_Var" },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::DynamicBinding),_Val), "_Val" },
{ class_kind, KIND_LISPALLOC_core__General_O, sizeof(core::General_O), 0, "core::General_O" },
{ class_kind, KIND_LISPALLOC_llvmo__BlockAddress_O, sizeof(llvmo::BlockAddress_O), 0, "llvmo::BlockAddress_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::BlockAddress_O),_Class), "_Class" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ErrorContent_, sizeof(gctools::GCVector_moveable<asttooling::ErrorContent>), 0, "gctools::GCVector_moveable<asttooling::ErrorContent>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<asttooling::ErrorContent>),_Data), "_Data" },
{  variable_capacity, sizeof(asttooling::ErrorContent), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<asttooling::ErrorContent>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<asttooling::ErrorContent>),_Capacity), NULL },
{    variable_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<asttooling::ContextFrame>>), offsetof(SAFE_TYPE_MACRO(asttooling::ErrorContent),ContextStack._Vector._Contents), "ContextStack._Vector._Contents" },
{    variable_field, TAGGED_POINTER_OFFSET, sizeof(gctools::tagged_pointer<gctools::GCVector_moveable<asttooling::Message>>), offsetof(SAFE_TYPE_MACRO(asttooling::ErrorContent),Messages._Vector._Contents), "Messages._Vector._Contents" },
{ class_kind, KIND_LISPALLOC_llvmo__ValueAsMetadata_O, sizeof(llvmo::ValueAsMetadata_O), 0, "llvmo::ValueAsMetadata_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::ValueAsMetadata_O),_Class), "_Class" },
{ container_kind, KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_clbind__ClassRep_O__, sizeof(gctools::GCVector_moveable<gctools::smart_ptr<clbind::ClassRep_O>>), 0, "gctools::GCVector_moveable<gctools::smart_ptr<clbind::ClassRep_O>>" },
{  variable_array0, 0, 0, offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<clbind::ClassRep_O>>),_Data), "_Data" },
{  variable_capacity, sizeof(gctools::smart_ptr<clbind::ClassRep_O>), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<clbind::ClassRep_O>>),_End), offsetof(SAFE_TYPE_MACRO(gctools::GCVector_moveable<gctools::smart_ptr<clbind::ClassRep_O>>),_Capacity), NULL },
{    variable_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<clbind::ClassRep_O>), 0, "only" },
{ class_kind, KIND_LISPALLOC_llvmo__Pass_O, sizeof(llvmo::Pass_O), 0, "llvmo::Pass_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::Pass_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__Attribute_O, sizeof(llvmo::Attribute_O), 0, "llvmo::Attribute_O" },
{ class_kind, KIND_LISPALLOC_core__SourceFileInfo_O, sizeof(core::SourceFileInfo_O), 0, "core::SourceFileInfo_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Pathname_O>), offsetof(SAFE_TYPE_MACRO(core::SourceFileInfo_O),_pathname), "_pathname" },
{  fixed_field, ctype_int, sizeof(int), offsetof(SAFE_TYPE_MACRO(core::SourceFileInfo_O),_FileHandle), "_FileHandle" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::T_O>), offsetof(SAFE_TYPE_MACRO(core::SourceFileInfo_O),_SourceDebugNamestring), "_SourceDebugNamestring" },
{  fixed_field, ctype_unsigned_long, sizeof(unsigned long), offsetof(SAFE_TYPE_MACRO(core::SourceFileInfo_O),_SourceDebugOffset), "_SourceDebugOffset" },
{ class_kind, KIND_LISPALLOC_core__SingleFloat_dummy_O, sizeof(core::SingleFloat_dummy_O), 0, "core::SingleFloat_dummy_O" },
{ class_kind, KIND_LISPALLOC_llvmo__TargetOptions_O, sizeof(llvmo::TargetOptions_O), 0, "llvmo::TargetOptions_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::TargetOptions_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__LLVMContext_O, sizeof(llvmo::LLVMContext_O), 0, "llvmo::LLVMContext_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::LLVMContext_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_llvmo__AtomicCmpXchgInst_O, sizeof(llvmo::AtomicCmpXchgInst_O), 0, "llvmo::AtomicCmpXchgInst_O" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::Class_O>), offsetof(SAFE_TYPE_MACRO(llvmo::AtomicCmpXchgInst_O),_Class), "_Class" },
{ class_kind, KIND_LISPALLOC_core__GlueEnvironment_O, sizeof(core::GlueEnvironment_O), 0, "core::GlueEnvironment_O" },
{  fixed_field, ctype_unsigned_int, sizeof(unsigned int), offsetof(SAFE_TYPE_MACRO(core::GlueEnvironment_O),_EnvId), "_EnvId" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::HashTableEq_O>), offsetof(SAFE_TYPE_MACRO(core::GlueEnvironment_O),_Map), "_Map" },
{  fixed_field, SMART_PTR_OFFSET, sizeof(gctools::smart_ptr<core::List_V>), offsetof(SAFE_TYPE_MACRO(core::GlueEnvironment_O),_Args), "_Args" },

#endif // defined(GC_OBJ_SCAN_HELPERS)
#if defined(GC_OBJ_SCAN_TABLE)
static void* OBJ_SCAN_table[] = { 
  /* 17 */ &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__,
  /* 23 */ &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__,
  /* 24 */ &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_,
  /* 31 */ &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__,
  /* 59 */ &&obj_scan_KIND_GCARRAY_gctools__GCArray_moveable_core__SlotData_0_,
  /* 73 */ &&obj_scan_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_,
  /* 81 */ &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_,
  /* 83 */ &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__Message_,
  /* 86 */ &&obj_scan_KIND_GCSTRING_gctools__GCString_moveable_char_,
  /* 97 */ &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_,
  /* 113 */ &&obj_scan_KIND_TEMPLATED_LISPALLOC_core__WrappedPointer_O,
  /* 116 */ &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__,
  /* 124 */ &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__,
  /* 127 */ &&obj_scan_KIND_TEMPLATED_CLASSALLOC_clbind__ConstructorCreator,
  /* 139 */ &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_,
  /* 140 */ &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__,
  /* 142 */ &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_,
  /* 160 */ &&obj_scan_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure,
  /* 162 */ &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_,
  /* 165 */ &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ErrorContent_,
  /* 167 */ &&obj_scan_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_clbind__ClassRep_O__,
   NULL
};
#endif // defined(GC_OBJ_SCAN_TABLE)
#if defined(GC_OBJ_FINALIZE)
obj_finalize_KIND_LISPALLOC_llvmo__APInt_O:
{
    llvmo::APInt_O* obj_gc_safe = reinterpret_cast<llvmo::APInt_O*>(client);
    obj_gc_safe->~APInt_O();
    return;
}
obj_finalize_KIND_BOOTSTRAP_core__Symbol_O:
{
    core::Symbol_O* obj_gc_safe = reinterpret_cast<core::Symbol_O*>(client);
    obj_gc_safe->~Symbol_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__IRBuilder_O:
{
    llvmo::IRBuilder_O* obj_gc_safe = reinterpret_cast<llvmo::IRBuilder_O*>(client);
    obj_gc_safe->~IRBuilder_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__FunctionValueEnvironment_O:
{
    core::FunctionValueEnvironment_O* obj_gc_safe = reinterpret_cast<core::FunctionValueEnvironment_O*>(client);
    obj_gc_safe->~FunctionValueEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__FenceInst_O:
{
    llvmo::FenceInst_O* obj_gc_safe = reinterpret_cast<llvmo::FenceInst_O*>(client);
    obj_gc_safe->~FenceInst_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__VectorObjects_O:
{
    core::VectorObjects_O* obj_gc_safe = reinterpret_cast<core::VectorObjects_O*>(client);
    obj_gc_safe->~VectorObjects_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__FunctionContainerEnvironment_O:
{
    core::FunctionContainerEnvironment_O* obj_gc_safe = reinterpret_cast<core::FunctionContainerEnvironment_O*>(client);
    obj_gc_safe->~FunctionContainerEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__ConstantArray_O:
{
    llvmo::ConstantArray_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantArray_O*>(client);
    obj_gc_safe->~ConstantArray_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__IntegerType_O:
{
    llvmo::IntegerType_O* obj_gc_safe = reinterpret_cast<llvmo::IntegerType_O*>(client);
    obj_gc_safe->~IntegerType_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Function_O:
{
    core::Function_O* obj_gc_safe = reinterpret_cast<core::Function_O*>(client);
    obj_gc_safe->~Function_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__ConstantInt_O:
{
    llvmo::ConstantInt_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantInt_O*>(client);
    obj_gc_safe->~ConstantInt_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__BranchInst_O:
{
    llvmo::BranchInst_O* obj_gc_safe = reinterpret_cast<llvmo::BranchInst_O*>(client);
    obj_gc_safe->~BranchInst_O();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>"));}
obj_finalize_KIND_BOOTSTRAP_core__StandardObject_O:
{
    core::StandardObject_O* obj_gc_safe = reinterpret_cast<core::StandardObject_O*>(client);
    obj_gc_safe->~StandardObject_O();
    return;
}
obj_finalize_KIND_LISPALLOC_asttooling__DerivableFrontendActionFactory:
{
    asttooling::DerivableFrontendActionFactory* obj_gc_safe = reinterpret_cast<asttooling::DerivableFrontendActionFactory*>(client);
    obj_gc_safe->~DerivableFrontendActionFactory();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__AtomicRMWInst_O:
{
    llvmo::AtomicRMWInst_O* obj_gc_safe = reinterpret_cast<llvmo::AtomicRMWInst_O*>(client);
    obj_gc_safe->~AtomicRMWInst_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__UnaryInstruction_O:
{
    llvmo::UnaryInstruction_O* obj_gc_safe = reinterpret_cast<llvmo::UnaryInstruction_O*>(client);
    obj_gc_safe->~UnaryInstruction_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__DoubleFloat_O:
{
    core::DoubleFloat_O* obj_gc_safe = reinterpret_cast<core::DoubleFloat_O*>(client);
    obj_gc_safe->~DoubleFloat_O();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<asttooling::RegMap::SymbolMatcherDescriptorPair>"));}
obj_finalize_KIND_LISPALLOC_llvmo__ConstantExpr_O:
{
    llvmo::ConstantExpr_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantExpr_O*>(client);
    obj_gc_safe->~ConstantExpr_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__LoadInst_O:
{
    llvmo::LoadInst_O* obj_gc_safe = reinterpret_cast<llvmo::LoadInst_O*>(client);
    obj_gc_safe->~LoadInst_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__AttributeSet_O:
{
    llvmo::AttributeSet_O* obj_gc_safe = reinterpret_cast<llvmo::AttributeSet_O*>(client);
    obj_gc_safe->~AttributeSet_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Instance_O:
{
    core::Instance_O* obj_gc_safe = reinterpret_cast<core::Instance_O*>(client);
    obj_gc_safe->~Instance_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__StoreInst_O:
{
    llvmo::StoreInst_O* obj_gc_safe = reinterpret_cast<llvmo::StoreInst_O*>(client);
    obj_gc_safe->~StoreInst_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__MCSubtargetInfo_O:
{
    llvmo::MCSubtargetInfo_O* obj_gc_safe = reinterpret_cast<llvmo::MCSubtargetInfo_O*>(client);
    obj_gc_safe->~MCSubtargetInfo_O();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>"));}
obj_finalize_KIND_LISPALLOC_llvmo__CallInst_O:
{
    llvmo::CallInst_O* obj_gc_safe = reinterpret_cast<llvmo::CallInst_O*>(client);
    obj_gc_safe->~CallInst_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__PHINode_O:
{
    llvmo::PHINode_O* obj_gc_safe = reinterpret_cast<llvmo::PHINode_O*>(client);
    obj_gc_safe->~PHINode_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__ImmutablePass_O:
{
    llvmo::ImmutablePass_O* obj_gc_safe = reinterpret_cast<llvmo::ImmutablePass_O*>(client);
    obj_gc_safe->~ImmutablePass_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__CompiledFunction_O:
{
    core::CompiledFunction_O* obj_gc_safe = reinterpret_cast<core::CompiledFunction_O*>(client);
    obj_gc_safe->~CompiledFunction_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Rational_O:
{
    core::Rational_O* obj_gc_safe = reinterpret_cast<core::Rational_O*>(client);
    obj_gc_safe->~Rational_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__ForeignData_O:
{
    core::ForeignData_O* obj_gc_safe = reinterpret_cast<core::ForeignData_O*>(client);
    obj_gc_safe->~ForeignData_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__VectorType_O:
{
    llvmo::VectorType_O* obj_gc_safe = reinterpret_cast<llvmo::VectorType_O*>(client);
    obj_gc_safe->~VectorType_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__FunctionPass_O:
{
    llvmo::FunctionPass_O* obj_gc_safe = reinterpret_cast<llvmo::FunctionPass_O*>(client);
    obj_gc_safe->~FunctionPass_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__ConstantStruct_O:
{
    llvmo::ConstantStruct_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantStruct_O*>(client);
    obj_gc_safe->~ConstantStruct_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__TargetSubtargetInfo_O:
{
    llvmo::TargetSubtargetInfo_O* obj_gc_safe = reinterpret_cast<llvmo::TargetSubtargetInfo_O*>(client);
    obj_gc_safe->~TargetSubtargetInfo_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__Instruction_O:
{
    llvmo::Instruction_O* obj_gc_safe = reinterpret_cast<llvmo::Instruction_O*>(client);
    obj_gc_safe->~Instruction_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__User_O:
{
    llvmo::User_O* obj_gc_safe = reinterpret_cast<llvmo::User_O*>(client);
    obj_gc_safe->~User_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__CatchEnvironment_O:
{
    core::CatchEnvironment_O* obj_gc_safe = reinterpret_cast<core::CatchEnvironment_O*>(client);
    obj_gc_safe->~CatchEnvironment_O();
    return;
}
obj_finalize_KIND_BOOTSTRAP_core__Str_O:
{
    core::Str_O* obj_gc_safe = reinterpret_cast<core::Str_O*>(client);
    obj_gc_safe->~Str_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Pointer_O:
{
    core::Pointer_O* obj_gc_safe = reinterpret_cast<core::Pointer_O*>(client);
    obj_gc_safe->~Pointer_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__PassManager_O:
{
    llvmo::PassManager_O* obj_gc_safe = reinterpret_cast<llvmo::PassManager_O*>(client);
    obj_gc_safe->~PassManager_O();
    return;
}
obj_finalize_KIND_LISPALLOC_clbind__ClassRegistry_O:
{
    clbind::ClassRegistry_O* obj_gc_safe = reinterpret_cast<clbind::ClassRegistry_O*>(client);
    obj_gc_safe->~ClassRegistry_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__PointerType_O:
{
    llvmo::PointerType_O* obj_gc_safe = reinterpret_cast<llvmo::PointerType_O*>(client);
    obj_gc_safe->~PointerType_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__DataLayout_O:
{
    llvmo::DataLayout_O* obj_gc_safe = reinterpret_cast<llvmo::DataLayout_O*>(client);
    obj_gc_safe->~DataLayout_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__APFloat_O:
{
    llvmo::APFloat_O* obj_gc_safe = reinterpret_cast<llvmo::APFloat_O*>(client);
    obj_gc_safe->~APFloat_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__ResumeInst_O:
{
    llvmo::ResumeInst_O* obj_gc_safe = reinterpret_cast<llvmo::ResumeInst_O*>(client);
    obj_gc_safe->~ResumeInst_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__ConstantDataArray_O:
{
    llvmo::ConstantDataArray_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantDataArray_O*>(client);
    obj_gc_safe->~ConstantDataArray_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Number_O:
{
    core::Number_O* obj_gc_safe = reinterpret_cast<core::Number_O*>(client);
    obj_gc_safe->~Number_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__TargetLibraryInfo_O:
{
    llvmo::TargetLibraryInfo_O* obj_gc_safe = reinterpret_cast<llvmo::TargetLibraryInfo_O*>(client);
    obj_gc_safe->~TargetLibraryInfo_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__UndefValue_O:
{
    llvmo::UndefValue_O* obj_gc_safe = reinterpret_cast<llvmo::UndefValue_O*>(client);
    obj_gc_safe->~UndefValue_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__EngineBuilder_O:
{
    llvmo::EngineBuilder_O* obj_gc_safe = reinterpret_cast<llvmo::EngineBuilder_O*>(client);
    obj_gc_safe->~EngineBuilder_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Environment_O:
{
    core::Environment_O* obj_gc_safe = reinterpret_cast<core::Environment_O*>(client);
    obj_gc_safe->~Environment_O();
    return;
}
obj_finalize_KIND_GCARRAY_gctools__GCArray_moveable_core__SlotData_0_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCArray_moveable<core::SlotData,0>"));}
obj_finalize_KIND_LISPALLOC_llvmo__ReturnInst_O:
{
    llvmo::ReturnInst_O* obj_gc_safe = reinterpret_cast<llvmo::ReturnInst_O*>(client);
    obj_gc_safe->~ReturnInst_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__MDString_O:
{
    llvmo::MDString_O* obj_gc_safe = reinterpret_cast<llvmo::MDString_O*>(client);
    obj_gc_safe->~MDString_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Complex_O:
{
    core::Complex_O* obj_gc_safe = reinterpret_cast<core::Complex_O*>(client);
    obj_gc_safe->~Complex_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SourceManager_O:
{
    core::SourceManager_O* obj_gc_safe = reinterpret_cast<core::SourceManager_O*>(client);
    obj_gc_safe->~SourceManager_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__LLVMTargetMachine_O:
{
    llvmo::LLVMTargetMachine_O* obj_gc_safe = reinterpret_cast<llvmo::LLVMTargetMachine_O*>(client);
    obj_gc_safe->~LLVMTargetMachine_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__LexicalEnvironment_O:
{
    core::LexicalEnvironment_O* obj_gc_safe = reinterpret_cast<core::LexicalEnvironment_O*>(client);
    obj_gc_safe->~LexicalEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O:
{
    core::SymbolMacroletEnvironment_O* obj_gc_safe = reinterpret_cast<core::SymbolMacroletEnvironment_O*>(client);
    obj_gc_safe->~SymbolMacroletEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__StackValueEnvironment_O:
{
    core::StackValueEnvironment_O* obj_gc_safe = reinterpret_cast<core::StackValueEnvironment_O*>(client);
    obj_gc_safe->~StackValueEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__PassManagerBuilder_O:
{
    llvmo::PassManagerBuilder_O* obj_gc_safe = reinterpret_cast<llvmo::PassManagerBuilder_O*>(client);
    obj_gc_safe->~PassManagerBuilder_O();
    return;
}
obj_finalize_KIND_LISPALLOC_asttooling__DerivableSyntaxOnlyAction:
{
    asttooling::DerivableSyntaxOnlyAction* obj_gc_safe = reinterpret_cast<asttooling::DerivableSyntaxOnlyAction*>(client);
    obj_gc_safe->~DerivableSyntaxOnlyAction();
    return;
}
obj_finalize_KIND_LISPALLOC_core__CompileTimeEnvironment_O:
{
    core::CompileTimeEnvironment_O* obj_gc_safe = reinterpret_cast<core::CompileTimeEnvironment_O*>(client);
    obj_gc_safe->~CompileTimeEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__ExecutionEngine_O:
{
    llvmo::ExecutionEngine_O* obj_gc_safe = reinterpret_cast<llvmo::ExecutionEngine_O*>(client);
    obj_gc_safe->~ExecutionEngine_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__TagbodyFrame_O:
{
    core::TagbodyFrame_O* obj_gc_safe = reinterpret_cast<core::TagbodyFrame_O*>(client);
    obj_gc_safe->~TagbodyFrame_O();
    return;
}
obj_finalize_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>"));}
obj_finalize_KIND_LISPALLOC_core__MacroletEnvironment_O:
{
    core::MacroletEnvironment_O* obj_gc_safe = reinterpret_cast<core::MacroletEnvironment_O*>(client);
    obj_gc_safe->~MacroletEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__Linker_O:
{
    llvmo::Linker_O* obj_gc_safe = reinterpret_cast<llvmo::Linker_O*>(client);
    obj_gc_safe->~Linker_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__DataLayoutPass_O:
{
    llvmo::DataLayoutPass_O* obj_gc_safe = reinterpret_cast<llvmo::DataLayoutPass_O*>(client);
    obj_gc_safe->~DataLayoutPass_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__NamedMDNode_O:
{
    llvmo::NamedMDNode_O* obj_gc_safe = reinterpret_cast<llvmo::NamedMDNode_O*>(client);
    obj_gc_safe->~NamedMDNode_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__ConstantPointerNull_O:
{
    llvmo::ConstantPointerNull_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantPointerNull_O*>(client);
    obj_gc_safe->~ConstantPointerNull_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__Constant_O:
{
    llvmo::Constant_O* obj_gc_safe = reinterpret_cast<llvmo::Constant_O*>(client);
    obj_gc_safe->~Constant_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__String_O:
{
    core::String_O* obj_gc_safe = reinterpret_cast<core::String_O*>(client);
    obj_gc_safe->~String_O();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::ExceptionEntry>"));}
obj_finalize_KIND_LISPALLOC_core__SymbolToEnumConverter_O:
{
    core::SymbolToEnumConverter_O* obj_gc_safe = reinterpret_cast<core::SymbolToEnumConverter_O*>(client);
    obj_gc_safe->~SymbolToEnumConverter_O();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__Message_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<asttooling::Message>"));}
obj_finalize_KIND_LISPALLOC_clbind__ClassRep_O:
{
    clbind::ClassRep_O* obj_gc_safe = reinterpret_cast<clbind::ClassRep_O*>(client);
    obj_gc_safe->~ClassRep_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__Value_O:
{
    llvmo::Value_O* obj_gc_safe = reinterpret_cast<llvmo::Value_O*>(client);
    obj_gc_safe->~Value_O();
    return;
}
obj_finalize_KIND_GCSTRING_gctools__GCString_moveable_char_:
{
    THROW_HARD_ERROR(BF("Should never finalize gcstrings gctools::GCString_moveable<char>"));}
obj_finalize_KIND_LISPALLOC_llvmo__Function_O:
{
    llvmo::Function_O* obj_gc_safe = reinterpret_cast<llvmo::Function_O*>(client);
    obj_gc_safe->~Function_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__Type_O:
{
    llvmo::Type_O* obj_gc_safe = reinterpret_cast<llvmo::Type_O*>(client);
    obj_gc_safe->~Type_O();
    return;
}
obj_finalize_KIND_LISPALLOC_asttooling__DerivableMatchCallback:
{
    asttooling::DerivableMatchCallback* obj_gc_safe = reinterpret_cast<asttooling::DerivableMatchCallback*>(client);
    obj_gc_safe->~DerivableMatchCallback();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__LandingPadInst_O:
{
    llvmo::LandingPadInst_O* obj_gc_safe = reinterpret_cast<llvmo::LandingPadInst_O*>(client);
    obj_gc_safe->~LandingPadInst_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Vector_O:
{
    core::Vector_O* obj_gc_safe = reinterpret_cast<core::Vector_O*>(client);
    obj_gc_safe->~Vector_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__UnwindProtectEnvironment_O:
{
    core::UnwindProtectEnvironment_O* obj_gc_safe = reinterpret_cast<core::UnwindProtectEnvironment_O*>(client);
    obj_gc_safe->~UnwindProtectEnvironment_O();
    return;
}
obj_finalize_KIND_BOOTSTRAP_core__Metaobject_O:
{
    core::Metaobject_O* obj_gc_safe = reinterpret_cast<core::Metaobject_O*>(client);
    obj_gc_safe->~Metaobject_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__InvokeInst_O:
{
    llvmo::InvokeInst_O* obj_gc_safe = reinterpret_cast<llvmo::InvokeInst_O*>(client);
    obj_gc_safe->~InvokeInst_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Real_O:
{
    core::Real_O* obj_gc_safe = reinterpret_cast<core::Real_O*>(client);
    obj_gc_safe->~Real_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Array_O:
{
    core::Array_O* obj_gc_safe = reinterpret_cast<core::Array_O*>(client);
    obj_gc_safe->~Array_O();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::CacheRecord>"));}
obj_finalize_KIND_LISPALLOC_core__ShortFloat_O:
{
    core::ShortFloat_O* obj_gc_safe = reinterpret_cast<core::ShortFloat_O*>(client);
    obj_gc_safe->~ShortFloat_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__BlockEnvironment_O:
{
    core::BlockEnvironment_O* obj_gc_safe = reinterpret_cast<core::BlockEnvironment_O*>(client);
    obj_gc_safe->~BlockEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Fixnum_dummy_O:
{
    core::Fixnum_dummy_O* obj_gc_safe = reinterpret_cast<core::Fixnum_dummy_O*>(client);
    obj_gc_safe->~Fixnum_dummy_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__PassManagerBase_O:
{
    llvmo::PassManagerBase_O* obj_gc_safe = reinterpret_cast<llvmo::PassManagerBase_O*>(client);
    obj_gc_safe->~PassManagerBase_O();
    return;
}
obj_finalize_KIND_BOOTSTRAP_core__Class_O:
{
    core::Class_O* obj_gc_safe = reinterpret_cast<core::Class_O*>(client);
    obj_gc_safe->~Class_O();
    return;
}
obj_finalize_KIND_LISPALLOC_asttooling__DerivableASTFrontendAction:
{
    asttooling::DerivableASTFrontendAction* obj_gc_safe = reinterpret_cast<asttooling::DerivableASTFrontendAction*>(client);
    obj_gc_safe->~DerivableASTFrontendAction();
    return;
}
obj_finalize_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O:
{
    core::VectorObjectsWithFillPtr_O* obj_gc_safe = reinterpret_cast<core::VectorObjectsWithFillPtr_O*>(client);
    obj_gc_safe->~VectorObjectsWithFillPtr_O();
    return;
}
obj_finalize_KIND_BOOTSTRAP_core__BuiltInClass_O:
{
    core::BuiltInClass_O* obj_gc_safe = reinterpret_cast<core::BuiltInClass_O*>(client);
    obj_gc_safe->~BuiltInClass_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__ValueEnvironment_O:
{
    core::ValueEnvironment_O* obj_gc_safe = reinterpret_cast<core::ValueEnvironment_O*>(client);
    obj_gc_safe->~ValueEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__IndirectBrInst_O:
{
    llvmo::IndirectBrInst_O* obj_gc_safe = reinterpret_cast<llvmo::IndirectBrInst_O*>(client);
    obj_gc_safe->~IndirectBrInst_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__Target_O:
{
    llvmo::Target_O* obj_gc_safe = reinterpret_cast<llvmo::Target_O*>(client);
    obj_gc_safe->~Target_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__Module_O:
{
    llvmo::Module_O* obj_gc_safe = reinterpret_cast<llvmo::Module_O*>(client);
    obj_gc_safe->~Module_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__FunctionFrame_O:
{
    core::FunctionFrame_O* obj_gc_safe = reinterpret_cast<core::FunctionFrame_O*>(client);
    obj_gc_safe->~FunctionFrame_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Bignum_O:
{
    core::Bignum_O* obj_gc_safe = reinterpret_cast<core::Bignum_O*>(client);
    obj_gc_safe->~Bignum_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__ExternalObject_O:
{
    core::ExternalObject_O* obj_gc_safe = reinterpret_cast<core::ExternalObject_O*>(client);
    obj_gc_safe->~ExternalObject_O();
    return;
}
obj_finalize_KIND_TEMPLATED_LISPALLOC_core__WrappedPointer_O:
{
    core::WrappedPointer_O* obj_gc_safe = reinterpret_cast<core::WrappedPointer_O*>(client);
    obj_gc_safe->~WrappedPointer_O();
}
obj_finalize_KIND_LISPALLOC_core__Ratio_O:
{
    core::Ratio_O* obj_gc_safe = reinterpret_cast<core::Ratio_O*>(client);
    obj_gc_safe->~Ratio_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__WeakHashTable_O:
{
    core::WeakHashTable_O* obj_gc_safe = reinterpret_cast<core::WeakHashTable_O*>(client);
    obj_gc_safe->~WeakHashTable_O();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>"));}
obj_finalize_KIND_LISPALLOC_llvmo__BasicBlock_O:
{
    llvmo::BasicBlock_O* obj_gc_safe = reinterpret_cast<llvmo::BasicBlock_O*>(client);
    obj_gc_safe->~BasicBlock_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__TerminatorInst_O:
{
    llvmo::TerminatorInst_O* obj_gc_safe = reinterpret_cast<llvmo::TerminatorInst_O*>(client);
    obj_gc_safe->~TerminatorInst_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__FunctionType_O:
{
    llvmo::FunctionType_O* obj_gc_safe = reinterpret_cast<llvmo::FunctionType_O*>(client);
    obj_gc_safe->~FunctionType_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__WeakKeyHashTable_O:
{
    core::WeakKeyHashTable_O* obj_gc_safe = reinterpret_cast<core::WeakKeyHashTable_O*>(client);
    obj_gc_safe->~WeakKeyHashTable_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__CxxObject_O:
{
    core::CxxObject_O* obj_gc_safe = reinterpret_cast<core::CxxObject_O*>(client);
    obj_gc_safe->~CxxObject_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__ModulePass_O:
{
    llvmo::ModulePass_O* obj_gc_safe = reinterpret_cast<llvmo::ModulePass_O*>(client);
    obj_gc_safe->~ModulePass_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__TagbodyEnvironment_O:
{
    core::TagbodyEnvironment_O* obj_gc_safe = reinterpret_cast<core::TagbodyEnvironment_O*>(client);
    obj_gc_safe->~TagbodyEnvironment_O();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>"));}
obj_finalize_KIND_LISPALLOC_llvmo__FunctionPassManager_O:
{
    llvmo::FunctionPassManager_O* obj_gc_safe = reinterpret_cast<llvmo::FunctionPassManager_O*>(client);
    obj_gc_safe->~FunctionPassManager_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__ConstantDataSequential_O:
{
    llvmo::ConstantDataSequential_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantDataSequential_O*>(client);
    obj_gc_safe->~ConstantDataSequential_O();
    return;
}
obj_finalize_KIND_TEMPLATED_CLASSALLOC_clbind__ConstructorCreator:
{
    clbind::ConstructorCreator* obj_gc_safe = reinterpret_cast<clbind::ConstructorCreator*>(client);
    obj_gc_safe->~ConstructorCreator();
}
obj_finalize_KIND_LISPALLOC_llvmo__GlobalValue_O:
{
    llvmo::GlobalValue_O* obj_gc_safe = reinterpret_cast<llvmo::GlobalValue_O*>(client);
    obj_gc_safe->~GlobalValue_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__TargetMachine_O:
{
    llvmo::TargetMachine_O* obj_gc_safe = reinterpret_cast<llvmo::TargetMachine_O*>(client);
    obj_gc_safe->~TargetMachine_O();
    return;
}
obj_finalize_KIND_CLASSALLOC_core__MacroClosure:
{
    core::MacroClosure* obj_gc_safe = reinterpret_cast<core::MacroClosure*>(client);
    obj_gc_safe->~MacroClosure();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Integer_O:
{
    core::Integer_O* obj_gc_safe = reinterpret_cast<core::Integer_O*>(client);
    obj_gc_safe->~Integer_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__UnreachableInst_O:
{
    llvmo::UnreachableInst_O* obj_gc_safe = reinterpret_cast<llvmo::UnreachableInst_O*>(client);
    obj_gc_safe->~UnreachableInst_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__Triple_O:
{
    llvmo::Triple_O* obj_gc_safe = reinterpret_cast<llvmo::Triple_O*>(client);
    obj_gc_safe->~Triple_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__AllocaInst_O:
{
    llvmo::AllocaInst_O* obj_gc_safe = reinterpret_cast<llvmo::AllocaInst_O*>(client);
    obj_gc_safe->~AllocaInst_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__MDNode_O:
{
    llvmo::MDNode_O* obj_gc_safe = reinterpret_cast<llvmo::MDNode_O*>(client);
    obj_gc_safe->~MDNode_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__StructType_O:
{
    llvmo::StructType_O* obj_gc_safe = reinterpret_cast<llvmo::StructType_O*>(client);
    obj_gc_safe->~StructType_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__IRBuilderBase_O:
{
    llvmo::IRBuilderBase_O* obj_gc_safe = reinterpret_cast<llvmo::IRBuilderBase_O*>(client);
    obj_gc_safe->~IRBuilderBase_O();
    return;
}
obj_finalize_KIND_BOOTSTRAP_core__Specializer_O:
{
    core::Specializer_O* obj_gc_safe = reinterpret_cast<core::Specializer_O*>(client);
    obj_gc_safe->~Specializer_O();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<asttooling::ContextFrame>"));}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>"));}
obj_finalize_KIND_LISPALLOC_core__SourcePosInfo_O:
{
    core::SourcePosInfo_O* obj_gc_safe = reinterpret_cast<core::SourcePosInfo_O*>(client);
    obj_gc_safe->~SourcePosInfo_O();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::SymbolClassPair>"));}
obj_finalize_KIND_LISPALLOC_core__Float_O:
{
    core::Float_O* obj_gc_safe = reinterpret_cast<core::Float_O*>(client);
    obj_gc_safe->~Float_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__LongFloat_O:
{
    core::LongFloat_O* obj_gc_safe = reinterpret_cast<core::LongFloat_O*>(client);
    obj_gc_safe->~LongFloat_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__ArrayType_O:
{
    llvmo::ArrayType_O* obj_gc_safe = reinterpret_cast<llvmo::ArrayType_O*>(client);
    obj_gc_safe->~ArrayType_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__ValueFrame_O:
{
    core::ValueFrame_O* obj_gc_safe = reinterpret_cast<core::ValueFrame_O*>(client);
    obj_gc_safe->~ValueFrame_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__Metadata_O:
{
    llvmo::Metadata_O* obj_gc_safe = reinterpret_cast<llvmo::Metadata_O*>(client);
    obj_gc_safe->~Metadata_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O:
{
    core::RuntimeVisibleEnvironment_O* obj_gc_safe = reinterpret_cast<core::RuntimeVisibleEnvironment_O*>(client);
    obj_gc_safe->~RuntimeVisibleEnvironment_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__ConstantFP_O:
{
    llvmo::ConstantFP_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantFP_O*>(client);
    obj_gc_safe->~ConstantFP_O();
    return;
}
obj_finalize_KIND_CLASSALLOC_clbind__DummyCreator:
{
    clbind::DummyCreator* obj_gc_safe = reinterpret_cast<clbind::DummyCreator*>(client);
    obj_gc_safe->~DummyCreator();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__SequentialType_O:
{
    llvmo::SequentialType_O* obj_gc_safe = reinterpret_cast<llvmo::SequentialType_O*>(client);
    obj_gc_safe->~SequentialType_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__HashTableEqual_O:
{
    core::HashTableEqual_O* obj_gc_safe = reinterpret_cast<core::HashTableEqual_O*>(client);
    obj_gc_safe->~HashTableEqual_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__CompositeType_O:
{
    llvmo::CompositeType_O* obj_gc_safe = reinterpret_cast<llvmo::CompositeType_O*>(client);
    obj_gc_safe->~CompositeType_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__VAArgInst_O:
{
    llvmo::VAArgInst_O* obj_gc_safe = reinterpret_cast<llvmo::VAArgInst_O*>(client);
    obj_gc_safe->~VAArgInst_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__Argument_O:
{
    llvmo::Argument_O* obj_gc_safe = reinterpret_cast<llvmo::Argument_O*>(client);
    obj_gc_safe->~Argument_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__CandoException_O:
{
    core::CandoException_O* obj_gc_safe = reinterpret_cast<core::CandoException_O*>(client);
    obj_gc_safe->~CandoException_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Iterator_O:
{
    core::Iterator_O* obj_gc_safe = reinterpret_cast<core::Iterator_O*>(client);
    obj_gc_safe->~Iterator_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__Record_O:
{
    core::Record_O* obj_gc_safe = reinterpret_cast<core::Record_O*>(client);
    obj_gc_safe->~Record_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__SwitchInst_O:
{
    llvmo::SwitchInst_O* obj_gc_safe = reinterpret_cast<llvmo::SwitchInst_O*>(client);
    obj_gc_safe->~SwitchInst_O();
    return;
}
obj_finalize_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure:
{
    core::BuiltinClosure* obj_gc_safe = reinterpret_cast<core::BuiltinClosure*>(client);
    obj_gc_safe->~BuiltinClosure();
}
obj_finalize_KIND_LISPALLOC_llvmo__GlobalVariable_O:
{
    llvmo::GlobalVariable_O* obj_gc_safe = reinterpret_cast<llvmo::GlobalVariable_O*>(client);
    obj_gc_safe->~GlobalVariable_O();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<core::DynamicBinding>"));}
obj_finalize_KIND_LISPALLOC_core__General_O:
{
    core::General_O* obj_gc_safe = reinterpret_cast<core::General_O*>(client);
    obj_gc_safe->~General_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__BlockAddress_O:
{
    llvmo::BlockAddress_O* obj_gc_safe = reinterpret_cast<llvmo::BlockAddress_O*>(client);
    obj_gc_safe->~BlockAddress_O();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ErrorContent_:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<asttooling::ErrorContent>"));}
obj_finalize_KIND_LISPALLOC_llvmo__ValueAsMetadata_O:
{
    llvmo::ValueAsMetadata_O* obj_gc_safe = reinterpret_cast<llvmo::ValueAsMetadata_O*>(client);
    obj_gc_safe->~ValueAsMetadata_O();
    return;
}
obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_clbind__ClassRep_O__:
{
    THROW_HARD_ERROR(BF("Should never finalize containers gctools::GCVector_moveable<gctools::smart_ptr<clbind::ClassRep_O>>"));}
obj_finalize_KIND_LISPALLOC_llvmo__Pass_O:
{
    llvmo::Pass_O* obj_gc_safe = reinterpret_cast<llvmo::Pass_O*>(client);
    obj_gc_safe->~Pass_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__Attribute_O:
{
    llvmo::Attribute_O* obj_gc_safe = reinterpret_cast<llvmo::Attribute_O*>(client);
    obj_gc_safe->~Attribute_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SourceFileInfo_O:
{
    core::SourceFileInfo_O* obj_gc_safe = reinterpret_cast<core::SourceFileInfo_O*>(client);
    obj_gc_safe->~SourceFileInfo_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__SingleFloat_dummy_O:
{
    core::SingleFloat_dummy_O* obj_gc_safe = reinterpret_cast<core::SingleFloat_dummy_O*>(client);
    obj_gc_safe->~SingleFloat_dummy_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__TargetOptions_O:
{
    llvmo::TargetOptions_O* obj_gc_safe = reinterpret_cast<llvmo::TargetOptions_O*>(client);
    obj_gc_safe->~TargetOptions_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__LLVMContext_O:
{
    llvmo::LLVMContext_O* obj_gc_safe = reinterpret_cast<llvmo::LLVMContext_O*>(client);
    obj_gc_safe->~LLVMContext_O();
    return;
}
obj_finalize_KIND_LISPALLOC_llvmo__AtomicCmpXchgInst_O:
{
    llvmo::AtomicCmpXchgInst_O* obj_gc_safe = reinterpret_cast<llvmo::AtomicCmpXchgInst_O*>(client);
    obj_gc_safe->~AtomicCmpXchgInst_O();
    return;
}
obj_finalize_KIND_LISPALLOC_core__GlueEnvironment_O:
{
    core::GlueEnvironment_O* obj_gc_safe = reinterpret_cast<core::GlueEnvironment_O*>(client);
    obj_gc_safe->~GlueEnvironment_O();
    return;
}
#endif // defined(GC_OBJ_FINALIZE)
#if defined(GC_OBJ_FINALIZE_HELPERS)

#endif // defined(GC_OBJ_FINALIZE_HELPERS)
#if defined(GC_OBJ_FINALIZE_TABLE)
static void* OBJ_FINALIZE_table[] = { 
  /* 5 */ &&obj_finalize_KIND_LISPALLOC_llvmo__APInt_O,
  /* 6 */ &&obj_finalize_KIND_BOOTSTRAP_core__Symbol_O,
  /* 7 */ &&obj_finalize_KIND_LISPALLOC_llvmo__IRBuilder_O,
  /* 8 */ &&obj_finalize_KIND_LISPALLOC_core__FunctionValueEnvironment_O,
  /* 9 */ &&obj_finalize_KIND_LISPALLOC_llvmo__FenceInst_O,
  /* 10 */ &&obj_finalize_KIND_LISPALLOC_core__VectorObjects_O,
  /* 11 */ &&obj_finalize_KIND_LISPALLOC_core__FunctionContainerEnvironment_O,
  /* 12 */ &&obj_finalize_KIND_LISPALLOC_llvmo__ConstantArray_O,
  /* 13 */ &&obj_finalize_KIND_LISPALLOC_llvmo__IntegerType_O,
  /* 14 */ &&obj_finalize_KIND_LISPALLOC_core__Function_O,
  /* 15 */ &&obj_finalize_KIND_LISPALLOC_llvmo__ConstantInt_O,
  /* 16 */ &&obj_finalize_KIND_LISPALLOC_llvmo__BranchInst_O,
  /* 17 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__,
  /* 18 */ &&obj_finalize_KIND_BOOTSTRAP_core__StandardObject_O,
  /* 19 */ &&obj_finalize_KIND_LISPALLOC_asttooling__DerivableFrontendActionFactory,
  /* 20 */ &&obj_finalize_KIND_LISPALLOC_llvmo__AtomicRMWInst_O,
  /* 21 */ &&obj_finalize_KIND_LISPALLOC_llvmo__UnaryInstruction_O,
  /* 22 */ &&obj_finalize_KIND_LISPALLOC_core__DoubleFloat_O,
  /* 23 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__,
  /* 24 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_,
  /* 25 */ &&obj_finalize_KIND_LISPALLOC_llvmo__ConstantExpr_O,
  /* 26 */ &&obj_finalize_KIND_LISPALLOC_llvmo__LoadInst_O,
  /* 27 */ &&obj_finalize_KIND_LISPALLOC_llvmo__AttributeSet_O,
  /* 28 */ &&obj_finalize_KIND_LISPALLOC_core__Instance_O,
  /* 29 */ &&obj_finalize_KIND_LISPALLOC_llvmo__StoreInst_O,
  /* 30 */ &&obj_finalize_KIND_LISPALLOC_llvmo__MCSubtargetInfo_O,
  /* 31 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__,
  /* 32 */ &&obj_finalize_KIND_LISPALLOC_llvmo__CallInst_O,
  /* 33 */ &&obj_finalize_KIND_LISPALLOC_llvmo__PHINode_O,
  /* 34 */ &&obj_finalize_KIND_LISPALLOC_llvmo__ImmutablePass_O,
  /* 35 */ &&obj_finalize_KIND_LISPALLOC_core__CompiledFunction_O,
  /* 36 */ &&obj_finalize_KIND_LISPALLOC_core__Rational_O,
  /* 37 */ &&obj_finalize_KIND_LISPALLOC_core__ForeignData_O,
  /* 38 */ &&obj_finalize_KIND_LISPALLOC_llvmo__VectorType_O,
  /* 39 */ &&obj_finalize_KIND_LISPALLOC_llvmo__FunctionPass_O,
  /* 40 */ &&obj_finalize_KIND_LISPALLOC_llvmo__ConstantStruct_O,
  /* 41 */ &&obj_finalize_KIND_LISPALLOC_llvmo__TargetSubtargetInfo_O,
  /* 42 */ &&obj_finalize_KIND_LISPALLOC_llvmo__Instruction_O,
  /* 43 */ &&obj_finalize_KIND_LISPALLOC_llvmo__User_O,
  /* 44 */ &&obj_finalize_KIND_LISPALLOC_core__CatchEnvironment_O,
  /* 45 */ &&obj_finalize_KIND_BOOTSTRAP_core__Str_O,
  /* 46 */ &&obj_finalize_KIND_LISPALLOC_core__Pointer_O,
  /* 47 */ &&obj_finalize_KIND_LISPALLOC_llvmo__PassManager_O,
  /* 48 */ &&obj_finalize_KIND_LISPALLOC_clbind__ClassRegistry_O,
  /* 49 */ &&obj_finalize_KIND_LISPALLOC_llvmo__PointerType_O,
  /* 50 */ &&obj_finalize_KIND_LISPALLOC_llvmo__DataLayout_O,
  /* 51 */ &&obj_finalize_KIND_LISPALLOC_llvmo__APFloat_O,
  /* 52 */ &&obj_finalize_KIND_LISPALLOC_llvmo__ResumeInst_O,
  /* 53 */ &&obj_finalize_KIND_LISPALLOC_llvmo__ConstantDataArray_O,
  /* 54 */ &&obj_finalize_KIND_LISPALLOC_core__Number_O,
  /* 55 */ &&obj_finalize_KIND_LISPALLOC_llvmo__TargetLibraryInfo_O,
  /* 56 */ &&obj_finalize_KIND_LISPALLOC_llvmo__UndefValue_O,
  /* 57 */ &&obj_finalize_KIND_LISPALLOC_llvmo__EngineBuilder_O,
  /* 58 */ &&obj_finalize_KIND_LISPALLOC_core__Environment_O,
  /* 59 */ &&obj_finalize_KIND_GCARRAY_gctools__GCArray_moveable_core__SlotData_0_,
  /* 60 */ &&obj_finalize_KIND_LISPALLOC_llvmo__ReturnInst_O,
  /* 61 */ &&obj_finalize_KIND_LISPALLOC_llvmo__MDString_O,
  /* 62 */ &&obj_finalize_KIND_LISPALLOC_core__Complex_O,
  /* 63 */ &&obj_finalize_KIND_LISPALLOC_core__SourceManager_O,
  /* 64 */ &&obj_finalize_KIND_LISPALLOC_llvmo__LLVMTargetMachine_O,
  /* 65 */ &&obj_finalize_KIND_LISPALLOC_core__LexicalEnvironment_O,
  /* 66 */ &&obj_finalize_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O,
  /* 67 */ &&obj_finalize_KIND_LISPALLOC_core__StackValueEnvironment_O,
  /* 68 */ &&obj_finalize_KIND_LISPALLOC_llvmo__PassManagerBuilder_O,
  /* 69 */ &&obj_finalize_KIND_LISPALLOC_asttooling__DerivableSyntaxOnlyAction,
  /* 70 */ &&obj_finalize_KIND_LISPALLOC_core__CompileTimeEnvironment_O,
  /* 71 */ &&obj_finalize_KIND_LISPALLOC_llvmo__ExecutionEngine_O,
  /* 72 */ &&obj_finalize_KIND_LISPALLOC_core__TagbodyFrame_O,
  /* 73 */ &&obj_finalize_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_,
  /* 74 */ &&obj_finalize_KIND_LISPALLOC_core__MacroletEnvironment_O,
  /* 75 */ &&obj_finalize_KIND_LISPALLOC_llvmo__Linker_O,
  /* 76 */ &&obj_finalize_KIND_LISPALLOC_llvmo__DataLayoutPass_O,
  /* 77 */ &&obj_finalize_KIND_LISPALLOC_llvmo__NamedMDNode_O,
  /* 78 */ &&obj_finalize_KIND_LISPALLOC_llvmo__ConstantPointerNull_O,
  /* 79 */ &&obj_finalize_KIND_LISPALLOC_llvmo__Constant_O,
  /* 80 */ &&obj_finalize_KIND_LISPALLOC_core__String_O,
  /* 81 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_,
  /* 82 */ &&obj_finalize_KIND_LISPALLOC_core__SymbolToEnumConverter_O,
  /* 83 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__Message_,
  /* 84 */ &&obj_finalize_KIND_LISPALLOC_clbind__ClassRep_O,
  /* 85 */ &&obj_finalize_KIND_LISPALLOC_llvmo__Value_O,
  /* 86 */ &&obj_finalize_KIND_GCSTRING_gctools__GCString_moveable_char_,
  /* 87 */ &&obj_finalize_KIND_LISPALLOC_llvmo__Function_O,
  /* 88 */ &&obj_finalize_KIND_LISPALLOC_llvmo__Type_O,
  /* 89 */ &&obj_finalize_KIND_LISPALLOC_asttooling__DerivableMatchCallback,
  /* 90 */ &&obj_finalize_KIND_LISPALLOC_llvmo__LandingPadInst_O,
  /* 91 */ &&obj_finalize_KIND_LISPALLOC_core__Vector_O,
  /* 92 */ &&obj_finalize_KIND_LISPALLOC_core__UnwindProtectEnvironment_O,
  /* 93 */ &&obj_finalize_KIND_BOOTSTRAP_core__Metaobject_O,
  /* 94 */ &&obj_finalize_KIND_LISPALLOC_llvmo__InvokeInst_O,
  /* 95 */ &&obj_finalize_KIND_LISPALLOC_core__Real_O,
  /* 96 */ &&obj_finalize_KIND_LISPALLOC_core__Array_O,
  /* 97 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_,
  /* 98 */ &&obj_finalize_KIND_LISPALLOC_core__ShortFloat_O,
  /* 99 */ &&obj_finalize_KIND_LISPALLOC_core__BlockEnvironment_O,
  /* 100 */ &&obj_finalize_KIND_LISPALLOC_core__Fixnum_dummy_O,
  /* 101 */ &&obj_finalize_KIND_LISPALLOC_llvmo__PassManagerBase_O,
  /* 102 */ &&obj_finalize_KIND_BOOTSTRAP_core__Class_O,
  /* 103 */ &&obj_finalize_KIND_LISPALLOC_asttooling__DerivableASTFrontendAction,
  /* 104 */ &&obj_finalize_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O,
  /* 105 */ &&obj_finalize_KIND_BOOTSTRAP_core__BuiltInClass_O,
  /* 106 */ &&obj_finalize_KIND_LISPALLOC_core__ValueEnvironment_O,
  /* 107 */ &&obj_finalize_KIND_LISPALLOC_llvmo__IndirectBrInst_O,
  /* 108 */ &&obj_finalize_KIND_LISPALLOC_llvmo__Target_O,
  /* 109 */ &&obj_finalize_KIND_LISPALLOC_llvmo__Module_O,
  /* 110 */ &&obj_finalize_KIND_LISPALLOC_core__FunctionFrame_O,
  /* 111 */ &&obj_finalize_KIND_LISPALLOC_core__Bignum_O,
  /* 112 */ &&obj_finalize_KIND_LISPALLOC_core__ExternalObject_O,
  /* 113 */ &&obj_finalize_KIND_TEMPLATED_LISPALLOC_core__WrappedPointer_O,
  /* 114 */ &&obj_finalize_KIND_LISPALLOC_core__Ratio_O,
  /* 115 */ &&obj_finalize_KIND_LISPALLOC_core__WeakHashTable_O,
  /* 116 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__,
  /* 117 */ &&obj_finalize_KIND_LISPALLOC_llvmo__BasicBlock_O,
  /* 118 */ &&obj_finalize_KIND_LISPALLOC_llvmo__TerminatorInst_O,
  /* 119 */ &&obj_finalize_KIND_LISPALLOC_llvmo__FunctionType_O,
  /* 120 */ &&obj_finalize_KIND_LISPALLOC_core__WeakKeyHashTable_O,
  /* 121 */ &&obj_finalize_KIND_LISPALLOC_core__CxxObject_O,
  /* 122 */ &&obj_finalize_KIND_LISPALLOC_llvmo__ModulePass_O,
  /* 123 */ &&obj_finalize_KIND_LISPALLOC_core__TagbodyEnvironment_O,
  /* 124 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__,
  /* 125 */ &&obj_finalize_KIND_LISPALLOC_llvmo__FunctionPassManager_O,
  /* 126 */ &&obj_finalize_KIND_LISPALLOC_llvmo__ConstantDataSequential_O,
  /* 127 */ &&obj_finalize_KIND_TEMPLATED_CLASSALLOC_clbind__ConstructorCreator,
  /* 128 */ &&obj_finalize_KIND_LISPALLOC_llvmo__GlobalValue_O,
  /* 129 */ &&obj_finalize_KIND_LISPALLOC_llvmo__TargetMachine_O,
  /* 130 */ &&obj_finalize_KIND_CLASSALLOC_core__MacroClosure,
  /* 131 */ &&obj_finalize_KIND_LISPALLOC_core__Integer_O,
  /* 132 */ &&obj_finalize_KIND_LISPALLOC_llvmo__UnreachableInst_O,
  /* 133 */ &&obj_finalize_KIND_LISPALLOC_llvmo__Triple_O,
  /* 134 */ &&obj_finalize_KIND_LISPALLOC_llvmo__AllocaInst_O,
  /* 135 */ &&obj_finalize_KIND_LISPALLOC_llvmo__MDNode_O,
  /* 136 */ &&obj_finalize_KIND_LISPALLOC_llvmo__StructType_O,
  /* 137 */ &&obj_finalize_KIND_LISPALLOC_llvmo__IRBuilderBase_O,
  /* 138 */ &&obj_finalize_KIND_BOOTSTRAP_core__Specializer_O,
  /* 139 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_,
  /* 140 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__,
  /* 141 */ &&obj_finalize_KIND_LISPALLOC_core__SourcePosInfo_O,
  /* 142 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_,
  /* 143 */ &&obj_finalize_KIND_LISPALLOC_core__Float_O,
  /* 144 */ &&obj_finalize_KIND_LISPALLOC_core__LongFloat_O,
  /* 145 */ &&obj_finalize_KIND_LISPALLOC_llvmo__ArrayType_O,
  /* 146 */ &&obj_finalize_KIND_LISPALLOC_core__ValueFrame_O,
  /* 147 */ &&obj_finalize_KIND_LISPALLOC_llvmo__Metadata_O,
  /* 148 */ &&obj_finalize_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O,
  /* 149 */ &&obj_finalize_KIND_LISPALLOC_llvmo__ConstantFP_O,
  /* 150 */ &&obj_finalize_KIND_CLASSALLOC_clbind__DummyCreator,
  /* 151 */ &&obj_finalize_KIND_LISPALLOC_llvmo__SequentialType_O,
  /* 152 */ &&obj_finalize_KIND_LISPALLOC_core__HashTableEqual_O,
  /* 153 */ &&obj_finalize_KIND_LISPALLOC_llvmo__CompositeType_O,
  /* 154 */ &&obj_finalize_KIND_LISPALLOC_llvmo__VAArgInst_O,
  /* 155 */ &&obj_finalize_KIND_LISPALLOC_llvmo__Argument_O,
  /* 156 */ &&obj_finalize_KIND_LISPALLOC_core__CandoException_O,
  /* 157 */ &&obj_finalize_KIND_LISPALLOC_core__Iterator_O,
  /* 158 */ &&obj_finalize_KIND_LISPALLOC_core__Record_O,
  /* 159 */ &&obj_finalize_KIND_LISPALLOC_llvmo__SwitchInst_O,
  /* 160 */ &&obj_finalize_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure,
  /* 161 */ &&obj_finalize_KIND_LISPALLOC_llvmo__GlobalVariable_O,
  /* 162 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_,
  /* 163 */ &&obj_finalize_KIND_LISPALLOC_core__General_O,
  /* 164 */ &&obj_finalize_KIND_LISPALLOC_llvmo__BlockAddress_O,
  /* 165 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ErrorContent_,
  /* 166 */ &&obj_finalize_KIND_LISPALLOC_llvmo__ValueAsMetadata_O,
  /* 167 */ &&obj_finalize_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_clbind__ClassRep_O__,
  /* 168 */ &&obj_finalize_KIND_LISPALLOC_llvmo__Pass_O,
  /* 169 */ &&obj_finalize_KIND_LISPALLOC_llvmo__Attribute_O,
  /* 170 */ &&obj_finalize_KIND_LISPALLOC_core__SourceFileInfo_O,
  /* 171 */ &&obj_finalize_KIND_LISPALLOC_core__SingleFloat_dummy_O,
  /* 172 */ &&obj_finalize_KIND_LISPALLOC_llvmo__TargetOptions_O,
  /* 173 */ &&obj_finalize_KIND_LISPALLOC_llvmo__LLVMContext_O,
  /* 174 */ &&obj_finalize_KIND_LISPALLOC_llvmo__AtomicCmpXchgInst_O,
  /* 175 */ &&obj_finalize_KIND_LISPALLOC_core__GlueEnvironment_O,
   NULL
};
#endif // defined(GC_OBJ_FINALIZE_TABLE)
#if defined(GC_OBJ_DEALLOCATOR)
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__APInt_O:
{
    llvmo::APInt_O* obj_gc_safe = reinterpret_cast<llvmo::APInt_O*>(client);
    GCObjectAllocator<llvmo::APInt_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Symbol_O:
{
    core::Symbol_O* obj_gc_safe = reinterpret_cast<core::Symbol_O*>(client);
    GCObjectAllocator<core::Symbol_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__IRBuilder_O:
{
    llvmo::IRBuilder_O* obj_gc_safe = reinterpret_cast<llvmo::IRBuilder_O*>(client);
    GCObjectAllocator<llvmo::IRBuilder_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionValueEnvironment_O:
{
    core::FunctionValueEnvironment_O* obj_gc_safe = reinterpret_cast<core::FunctionValueEnvironment_O*>(client);
    GCObjectAllocator<core::FunctionValueEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__FenceInst_O:
{
    llvmo::FenceInst_O* obj_gc_safe = reinterpret_cast<llvmo::FenceInst_O*>(client);
    GCObjectAllocator<llvmo::FenceInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__VectorObjects_O:
{
    core::VectorObjects_O* obj_gc_safe = reinterpret_cast<core::VectorObjects_O*>(client);
    GCObjectAllocator<core::VectorObjects_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionContainerEnvironment_O:
{
    core::FunctionContainerEnvironment_O* obj_gc_safe = reinterpret_cast<core::FunctionContainerEnvironment_O*>(client);
    GCObjectAllocator<core::FunctionContainerEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantArray_O:
{
    llvmo::ConstantArray_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantArray_O*>(client);
    GCObjectAllocator<llvmo::ConstantArray_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__IntegerType_O:
{
    llvmo::IntegerType_O* obj_gc_safe = reinterpret_cast<llvmo::IntegerType_O*>(client);
    GCObjectAllocator<llvmo::IntegerType_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Function_O:
{
    core::Function_O* obj_gc_safe = reinterpret_cast<core::Function_O*>(client);
    GCObjectAllocator<core::Function_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantInt_O:
{
    llvmo::ConstantInt_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantInt_O*>(client);
    GCObjectAllocator<llvmo::ConstantInt_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__BranchInst_O:
{
    llvmo::BranchInst_O* obj_gc_safe = reinterpret_cast<llvmo::BranchInst_O*>(client);
    GCObjectAllocator<llvmo::BranchInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>"));}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__StandardObject_O:
{
    core::StandardObject_O* obj_gc_safe = reinterpret_cast<core::StandardObject_O*>(client);
    GCObjectAllocator<core::StandardObject_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__DerivableFrontendActionFactory:
{
    asttooling::DerivableFrontendActionFactory* obj_gc_safe = reinterpret_cast<asttooling::DerivableFrontendActionFactory*>(client);
    GCObjectAllocator<asttooling::DerivableFrontendActionFactory>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__AtomicRMWInst_O:
{
    llvmo::AtomicRMWInst_O* obj_gc_safe = reinterpret_cast<llvmo::AtomicRMWInst_O*>(client);
    GCObjectAllocator<llvmo::AtomicRMWInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__UnaryInstruction_O:
{
    llvmo::UnaryInstruction_O* obj_gc_safe = reinterpret_cast<llvmo::UnaryInstruction_O*>(client);
    GCObjectAllocator<llvmo::UnaryInstruction_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__DoubleFloat_O:
{
    core::DoubleFloat_O* obj_gc_safe = reinterpret_cast<core::DoubleFloat_O*>(client);
    GCObjectAllocator<core::DoubleFloat_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<asttooling::RegMap::SymbolMatcherDescriptorPair>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantExpr_O:
{
    llvmo::ConstantExpr_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantExpr_O*>(client);
    GCObjectAllocator<llvmo::ConstantExpr_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__LoadInst_O:
{
    llvmo::LoadInst_O* obj_gc_safe = reinterpret_cast<llvmo::LoadInst_O*>(client);
    GCObjectAllocator<llvmo::LoadInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__AttributeSet_O:
{
    llvmo::AttributeSet_O* obj_gc_safe = reinterpret_cast<llvmo::AttributeSet_O*>(client);
    GCObjectAllocator<llvmo::AttributeSet_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Instance_O:
{
    core::Instance_O* obj_gc_safe = reinterpret_cast<core::Instance_O*>(client);
    GCObjectAllocator<core::Instance_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__StoreInst_O:
{
    llvmo::StoreInst_O* obj_gc_safe = reinterpret_cast<llvmo::StoreInst_O*>(client);
    GCObjectAllocator<llvmo::StoreInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__MCSubtargetInfo_O:
{
    llvmo::MCSubtargetInfo_O* obj_gc_safe = reinterpret_cast<llvmo::MCSubtargetInfo_O*>(client);
    GCObjectAllocator<llvmo::MCSubtargetInfo_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__CallInst_O:
{
    llvmo::CallInst_O* obj_gc_safe = reinterpret_cast<llvmo::CallInst_O*>(client);
    GCObjectAllocator<llvmo::CallInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__PHINode_O:
{
    llvmo::PHINode_O* obj_gc_safe = reinterpret_cast<llvmo::PHINode_O*>(client);
    GCObjectAllocator<llvmo::PHINode_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ImmutablePass_O:
{
    llvmo::ImmutablePass_O* obj_gc_safe = reinterpret_cast<llvmo::ImmutablePass_O*>(client);
    GCObjectAllocator<llvmo::ImmutablePass_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CompiledFunction_O:
{
    core::CompiledFunction_O* obj_gc_safe = reinterpret_cast<core::CompiledFunction_O*>(client);
    GCObjectAllocator<core::CompiledFunction_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Rational_O:
{
    core::Rational_O* obj_gc_safe = reinterpret_cast<core::Rational_O*>(client);
    GCObjectAllocator<core::Rational_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ForeignData_O:
{
    core::ForeignData_O* obj_gc_safe = reinterpret_cast<core::ForeignData_O*>(client);
    GCObjectAllocator<core::ForeignData_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__VectorType_O:
{
    llvmo::VectorType_O* obj_gc_safe = reinterpret_cast<llvmo::VectorType_O*>(client);
    GCObjectAllocator<llvmo::VectorType_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__FunctionPass_O:
{
    llvmo::FunctionPass_O* obj_gc_safe = reinterpret_cast<llvmo::FunctionPass_O*>(client);
    GCObjectAllocator<llvmo::FunctionPass_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantStruct_O:
{
    llvmo::ConstantStruct_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantStruct_O*>(client);
    GCObjectAllocator<llvmo::ConstantStruct_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__TargetSubtargetInfo_O:
{
    llvmo::TargetSubtargetInfo_O* obj_gc_safe = reinterpret_cast<llvmo::TargetSubtargetInfo_O*>(client);
    GCObjectAllocator<llvmo::TargetSubtargetInfo_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Instruction_O:
{
    llvmo::Instruction_O* obj_gc_safe = reinterpret_cast<llvmo::Instruction_O*>(client);
    GCObjectAllocator<llvmo::Instruction_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__User_O:
{
    llvmo::User_O* obj_gc_safe = reinterpret_cast<llvmo::User_O*>(client);
    GCObjectAllocator<llvmo::User_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CatchEnvironment_O:
{
    core::CatchEnvironment_O* obj_gc_safe = reinterpret_cast<core::CatchEnvironment_O*>(client);
    GCObjectAllocator<core::CatchEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Str_O:
{
    core::Str_O* obj_gc_safe = reinterpret_cast<core::Str_O*>(client);
    GCObjectAllocator<core::Str_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Pointer_O:
{
    core::Pointer_O* obj_gc_safe = reinterpret_cast<core::Pointer_O*>(client);
    GCObjectAllocator<core::Pointer_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__PassManager_O:
{
    llvmo::PassManager_O* obj_gc_safe = reinterpret_cast<llvmo::PassManager_O*>(client);
    GCObjectAllocator<llvmo::PassManager_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_clbind__ClassRegistry_O:
{
    clbind::ClassRegistry_O* obj_gc_safe = reinterpret_cast<clbind::ClassRegistry_O*>(client);
    GCObjectAllocator<clbind::ClassRegistry_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__PointerType_O:
{
    llvmo::PointerType_O* obj_gc_safe = reinterpret_cast<llvmo::PointerType_O*>(client);
    GCObjectAllocator<llvmo::PointerType_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DataLayout_O:
{
    llvmo::DataLayout_O* obj_gc_safe = reinterpret_cast<llvmo::DataLayout_O*>(client);
    GCObjectAllocator<llvmo::DataLayout_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__APFloat_O:
{
    llvmo::APFloat_O* obj_gc_safe = reinterpret_cast<llvmo::APFloat_O*>(client);
    GCObjectAllocator<llvmo::APFloat_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ResumeInst_O:
{
    llvmo::ResumeInst_O* obj_gc_safe = reinterpret_cast<llvmo::ResumeInst_O*>(client);
    GCObjectAllocator<llvmo::ResumeInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantDataArray_O:
{
    llvmo::ConstantDataArray_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantDataArray_O*>(client);
    GCObjectAllocator<llvmo::ConstantDataArray_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Number_O:
{
    core::Number_O* obj_gc_safe = reinterpret_cast<core::Number_O*>(client);
    GCObjectAllocator<core::Number_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__TargetLibraryInfo_O:
{
    llvmo::TargetLibraryInfo_O* obj_gc_safe = reinterpret_cast<llvmo::TargetLibraryInfo_O*>(client);
    GCObjectAllocator<llvmo::TargetLibraryInfo_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__UndefValue_O:
{
    llvmo::UndefValue_O* obj_gc_safe = reinterpret_cast<llvmo::UndefValue_O*>(client);
    GCObjectAllocator<llvmo::UndefValue_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__EngineBuilder_O:
{
    llvmo::EngineBuilder_O* obj_gc_safe = reinterpret_cast<llvmo::EngineBuilder_O*>(client);
    GCObjectAllocator<llvmo::EngineBuilder_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Environment_O:
{
    core::Environment_O* obj_gc_safe = reinterpret_cast<core::Environment_O*>(client);
    GCObjectAllocator<core::Environment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCARRAY_gctools__GCArray_moveable_core__SlotData_0_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCArray_moveable<core::SlotData,0>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ReturnInst_O:
{
    llvmo::ReturnInst_O* obj_gc_safe = reinterpret_cast<llvmo::ReturnInst_O*>(client);
    GCObjectAllocator<llvmo::ReturnInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__MDString_O:
{
    llvmo::MDString_O* obj_gc_safe = reinterpret_cast<llvmo::MDString_O*>(client);
    GCObjectAllocator<llvmo::MDString_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Complex_O:
{
    core::Complex_O* obj_gc_safe = reinterpret_cast<core::Complex_O*>(client);
    GCObjectAllocator<core::Complex_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SourceManager_O:
{
    core::SourceManager_O* obj_gc_safe = reinterpret_cast<core::SourceManager_O*>(client);
    GCObjectAllocator<core::SourceManager_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__LLVMTargetMachine_O:
{
    llvmo::LLVMTargetMachine_O* obj_gc_safe = reinterpret_cast<llvmo::LLVMTargetMachine_O*>(client);
    GCObjectAllocator<llvmo::LLVMTargetMachine_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LexicalEnvironment_O:
{
    core::LexicalEnvironment_O* obj_gc_safe = reinterpret_cast<core::LexicalEnvironment_O*>(client);
    GCObjectAllocator<core::LexicalEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O:
{
    core::SymbolMacroletEnvironment_O* obj_gc_safe = reinterpret_cast<core::SymbolMacroletEnvironment_O*>(client);
    GCObjectAllocator<core::SymbolMacroletEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__StackValueEnvironment_O:
{
    core::StackValueEnvironment_O* obj_gc_safe = reinterpret_cast<core::StackValueEnvironment_O*>(client);
    GCObjectAllocator<core::StackValueEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__PassManagerBuilder_O:
{
    llvmo::PassManagerBuilder_O* obj_gc_safe = reinterpret_cast<llvmo::PassManagerBuilder_O*>(client);
    GCObjectAllocator<llvmo::PassManagerBuilder_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__DerivableSyntaxOnlyAction:
{
    asttooling::DerivableSyntaxOnlyAction* obj_gc_safe = reinterpret_cast<asttooling::DerivableSyntaxOnlyAction*>(client);
    GCObjectAllocator<asttooling::DerivableSyntaxOnlyAction>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CompileTimeEnvironment_O:
{
    core::CompileTimeEnvironment_O* obj_gc_safe = reinterpret_cast<core::CompileTimeEnvironment_O*>(client);
    GCObjectAllocator<core::CompileTimeEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ExecutionEngine_O:
{
    llvmo::ExecutionEngine_O* obj_gc_safe = reinterpret_cast<llvmo::ExecutionEngine_O*>(client);
    GCObjectAllocator<llvmo::ExecutionEngine_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__TagbodyFrame_O:
{
    core::TagbodyFrame_O* obj_gc_safe = reinterpret_cast<core::TagbodyFrame_O*>(client);
    GCObjectAllocator<core::TagbodyFrame_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>,0>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__MacroletEnvironment_O:
{
    core::MacroletEnvironment_O* obj_gc_safe = reinterpret_cast<core::MacroletEnvironment_O*>(client);
    GCObjectAllocator<core::MacroletEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Linker_O:
{
    llvmo::Linker_O* obj_gc_safe = reinterpret_cast<llvmo::Linker_O*>(client);
    GCObjectAllocator<llvmo::Linker_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DataLayoutPass_O:
{
    llvmo::DataLayoutPass_O* obj_gc_safe = reinterpret_cast<llvmo::DataLayoutPass_O*>(client);
    GCObjectAllocator<llvmo::DataLayoutPass_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__NamedMDNode_O:
{
    llvmo::NamedMDNode_O* obj_gc_safe = reinterpret_cast<llvmo::NamedMDNode_O*>(client);
    GCObjectAllocator<llvmo::NamedMDNode_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantPointerNull_O:
{
    llvmo::ConstantPointerNull_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantPointerNull_O*>(client);
    GCObjectAllocator<llvmo::ConstantPointerNull_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Constant_O:
{
    llvmo::Constant_O* obj_gc_safe = reinterpret_cast<llvmo::Constant_O*>(client);
    GCObjectAllocator<llvmo::Constant_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__String_O:
{
    core::String_O* obj_gc_safe = reinterpret_cast<core::String_O*>(client);
    GCObjectAllocator<core::String_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::ExceptionEntry>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SymbolToEnumConverter_O:
{
    core::SymbolToEnumConverter_O* obj_gc_safe = reinterpret_cast<core::SymbolToEnumConverter_O*>(client);
    GCObjectAllocator<core::SymbolToEnumConverter_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__Message_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<asttooling::Message>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_clbind__ClassRep_O:
{
    clbind::ClassRep_O* obj_gc_safe = reinterpret_cast<clbind::ClassRep_O*>(client);
    GCObjectAllocator<clbind::ClassRep_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Value_O:
{
    llvmo::Value_O* obj_gc_safe = reinterpret_cast<llvmo::Value_O*>(client);
    GCObjectAllocator<llvmo::Value_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCSTRING_gctools__GCString_moveable_char_:
{
    THROW_HARD_ERROR(BF("Should never deallocate gcstrings gctools::GCString_moveable<char>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Function_O:
{
    llvmo::Function_O* obj_gc_safe = reinterpret_cast<llvmo::Function_O*>(client);
    GCObjectAllocator<llvmo::Function_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Type_O:
{
    llvmo::Type_O* obj_gc_safe = reinterpret_cast<llvmo::Type_O*>(client);
    GCObjectAllocator<llvmo::Type_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__DerivableMatchCallback:
{
    asttooling::DerivableMatchCallback* obj_gc_safe = reinterpret_cast<asttooling::DerivableMatchCallback*>(client);
    GCObjectAllocator<asttooling::DerivableMatchCallback>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__LandingPadInst_O:
{
    llvmo::LandingPadInst_O* obj_gc_safe = reinterpret_cast<llvmo::LandingPadInst_O*>(client);
    GCObjectAllocator<llvmo::LandingPadInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Vector_O:
{
    core::Vector_O* obj_gc_safe = reinterpret_cast<core::Vector_O*>(client);
    GCObjectAllocator<core::Vector_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__UnwindProtectEnvironment_O:
{
    core::UnwindProtectEnvironment_O* obj_gc_safe = reinterpret_cast<core::UnwindProtectEnvironment_O*>(client);
    GCObjectAllocator<core::UnwindProtectEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Metaobject_O:
{
    core::Metaobject_O* obj_gc_safe = reinterpret_cast<core::Metaobject_O*>(client);
    GCObjectAllocator<core::Metaobject_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__InvokeInst_O:
{
    llvmo::InvokeInst_O* obj_gc_safe = reinterpret_cast<llvmo::InvokeInst_O*>(client);
    GCObjectAllocator<llvmo::InvokeInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Real_O:
{
    core::Real_O* obj_gc_safe = reinterpret_cast<core::Real_O*>(client);
    GCObjectAllocator<core::Real_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Array_O:
{
    core::Array_O* obj_gc_safe = reinterpret_cast<core::Array_O*>(client);
    GCObjectAllocator<core::Array_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::CacheRecord>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ShortFloat_O:
{
    core::ShortFloat_O* obj_gc_safe = reinterpret_cast<core::ShortFloat_O*>(client);
    GCObjectAllocator<core::ShortFloat_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__BlockEnvironment_O:
{
    core::BlockEnvironment_O* obj_gc_safe = reinterpret_cast<core::BlockEnvironment_O*>(client);
    GCObjectAllocator<core::BlockEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Fixnum_dummy_O:
{
    core::Fixnum_dummy_O* obj_gc_safe = reinterpret_cast<core::Fixnum_dummy_O*>(client);
    GCObjectAllocator<core::Fixnum_dummy_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__PassManagerBase_O:
{
    llvmo::PassManagerBase_O* obj_gc_safe = reinterpret_cast<llvmo::PassManagerBase_O*>(client);
    GCObjectAllocator<llvmo::PassManagerBase_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Class_O:
{
    core::Class_O* obj_gc_safe = reinterpret_cast<core::Class_O*>(client);
    GCObjectAllocator<core::Class_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__DerivableASTFrontendAction:
{
    asttooling::DerivableASTFrontendAction* obj_gc_safe = reinterpret_cast<asttooling::DerivableASTFrontendAction*>(client);
    GCObjectAllocator<asttooling::DerivableASTFrontendAction>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O:
{
    core::VectorObjectsWithFillPtr_O* obj_gc_safe = reinterpret_cast<core::VectorObjectsWithFillPtr_O*>(client);
    GCObjectAllocator<core::VectorObjectsWithFillPtr_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__BuiltInClass_O:
{
    core::BuiltInClass_O* obj_gc_safe = reinterpret_cast<core::BuiltInClass_O*>(client);
    GCObjectAllocator<core::BuiltInClass_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ValueEnvironment_O:
{
    core::ValueEnvironment_O* obj_gc_safe = reinterpret_cast<core::ValueEnvironment_O*>(client);
    GCObjectAllocator<core::ValueEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__IndirectBrInst_O:
{
    llvmo::IndirectBrInst_O* obj_gc_safe = reinterpret_cast<llvmo::IndirectBrInst_O*>(client);
    GCObjectAllocator<llvmo::IndirectBrInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Target_O:
{
    llvmo::Target_O* obj_gc_safe = reinterpret_cast<llvmo::Target_O*>(client);
    GCObjectAllocator<llvmo::Target_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Module_O:
{
    llvmo::Module_O* obj_gc_safe = reinterpret_cast<llvmo::Module_O*>(client);
    GCObjectAllocator<llvmo::Module_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionFrame_O:
{
    core::FunctionFrame_O* obj_gc_safe = reinterpret_cast<core::FunctionFrame_O*>(client);
    GCObjectAllocator<core::FunctionFrame_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Bignum_O:
{
    core::Bignum_O* obj_gc_safe = reinterpret_cast<core::Bignum_O*>(client);
    GCObjectAllocator<core::Bignum_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ExternalObject_O:
{
    core::ExternalObject_O* obj_gc_safe = reinterpret_cast<core::ExternalObject_O*>(client);
    GCObjectAllocator<core::ExternalObject_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_TEMPLATED_LISPALLOC_core__WrappedPointer_O:
{
    core::WrappedPointer_O* obj_gc_safe = reinterpret_cast<core::WrappedPointer_O*>(client);
    GCObjectAllocator<core::WrappedPointer_O>::deallocate_unmanaged_instance(obj_gc_safe);
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Ratio_O:
{
    core::Ratio_O* obj_gc_safe = reinterpret_cast<core::Ratio_O*>(client);
    GCObjectAllocator<core::Ratio_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__WeakHashTable_O:
{
    core::WeakHashTable_O* obj_gc_safe = reinterpret_cast<core::WeakHashTable_O*>(client);
    GCObjectAllocator<core::WeakHashTable_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__BasicBlock_O:
{
    llvmo::BasicBlock_O* obj_gc_safe = reinterpret_cast<llvmo::BasicBlock_O*>(client);
    GCObjectAllocator<llvmo::BasicBlock_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__TerminatorInst_O:
{
    llvmo::TerminatorInst_O* obj_gc_safe = reinterpret_cast<llvmo::TerminatorInst_O*>(client);
    GCObjectAllocator<llvmo::TerminatorInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__FunctionType_O:
{
    llvmo::FunctionType_O* obj_gc_safe = reinterpret_cast<llvmo::FunctionType_O*>(client);
    GCObjectAllocator<llvmo::FunctionType_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__WeakKeyHashTable_O:
{
    core::WeakKeyHashTable_O* obj_gc_safe = reinterpret_cast<core::WeakKeyHashTable_O*>(client);
    GCObjectAllocator<core::WeakKeyHashTable_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CxxObject_O:
{
    core::CxxObject_O* obj_gc_safe = reinterpret_cast<core::CxxObject_O*>(client);
    GCObjectAllocator<core::CxxObject_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ModulePass_O:
{
    llvmo::ModulePass_O* obj_gc_safe = reinterpret_cast<llvmo::ModulePass_O*>(client);
    GCObjectAllocator<llvmo::ModulePass_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__TagbodyEnvironment_O:
{
    core::TagbodyEnvironment_O* obj_gc_safe = reinterpret_cast<core::TagbodyEnvironment_O*>(client);
    GCObjectAllocator<core::TagbodyEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__FunctionPassManager_O:
{
    llvmo::FunctionPassManager_O* obj_gc_safe = reinterpret_cast<llvmo::FunctionPassManager_O*>(client);
    GCObjectAllocator<llvmo::FunctionPassManager_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantDataSequential_O:
{
    llvmo::ConstantDataSequential_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantDataSequential_O*>(client);
    GCObjectAllocator<llvmo::ConstantDataSequential_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_TEMPLATED_CLASSALLOC_clbind__ConstructorCreator:
{
    clbind::ConstructorCreator* obj_gc_safe = reinterpret_cast<clbind::ConstructorCreator*>(client);
    GCObjectAllocator<clbind::ConstructorCreator>::deallocate_unmanaged_instance(obj_gc_safe);
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__GlobalValue_O:
{
    llvmo::GlobalValue_O* obj_gc_safe = reinterpret_cast<llvmo::GlobalValue_O*>(client);
    GCObjectAllocator<llvmo::GlobalValue_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__TargetMachine_O:
{
    llvmo::TargetMachine_O* obj_gc_safe = reinterpret_cast<llvmo::TargetMachine_O*>(client);
    GCObjectAllocator<llvmo::TargetMachine_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_CLASSALLOC_core__MacroClosure:
{
    core::MacroClosure* obj_gc_safe = reinterpret_cast<core::MacroClosure*>(client);
    GCObjectAllocator<core::MacroClosure>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Integer_O:
{
    core::Integer_O* obj_gc_safe = reinterpret_cast<core::Integer_O*>(client);
    GCObjectAllocator<core::Integer_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__UnreachableInst_O:
{
    llvmo::UnreachableInst_O* obj_gc_safe = reinterpret_cast<llvmo::UnreachableInst_O*>(client);
    GCObjectAllocator<llvmo::UnreachableInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Triple_O:
{
    llvmo::Triple_O* obj_gc_safe = reinterpret_cast<llvmo::Triple_O*>(client);
    GCObjectAllocator<llvmo::Triple_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__AllocaInst_O:
{
    llvmo::AllocaInst_O* obj_gc_safe = reinterpret_cast<llvmo::AllocaInst_O*>(client);
    GCObjectAllocator<llvmo::AllocaInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__MDNode_O:
{
    llvmo::MDNode_O* obj_gc_safe = reinterpret_cast<llvmo::MDNode_O*>(client);
    GCObjectAllocator<llvmo::MDNode_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__StructType_O:
{
    llvmo::StructType_O* obj_gc_safe = reinterpret_cast<llvmo::StructType_O*>(client);
    GCObjectAllocator<llvmo::StructType_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__IRBuilderBase_O:
{
    llvmo::IRBuilderBase_O* obj_gc_safe = reinterpret_cast<llvmo::IRBuilderBase_O*>(client);
    GCObjectAllocator<llvmo::IRBuilderBase_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Specializer_O:
{
    core::Specializer_O* obj_gc_safe = reinterpret_cast<core::Specializer_O*>(client);
    GCObjectAllocator<core::Specializer_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<asttooling::ContextFrame>"));}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SourcePosInfo_O:
{
    core::SourcePosInfo_O* obj_gc_safe = reinterpret_cast<core::SourcePosInfo_O*>(client);
    GCObjectAllocator<core::SourcePosInfo_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::SymbolClassPair>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Float_O:
{
    core::Float_O* obj_gc_safe = reinterpret_cast<core::Float_O*>(client);
    GCObjectAllocator<core::Float_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LongFloat_O:
{
    core::LongFloat_O* obj_gc_safe = reinterpret_cast<core::LongFloat_O*>(client);
    GCObjectAllocator<core::LongFloat_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ArrayType_O:
{
    llvmo::ArrayType_O* obj_gc_safe = reinterpret_cast<llvmo::ArrayType_O*>(client);
    GCObjectAllocator<llvmo::ArrayType_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ValueFrame_O:
{
    core::ValueFrame_O* obj_gc_safe = reinterpret_cast<core::ValueFrame_O*>(client);
    GCObjectAllocator<core::ValueFrame_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Metadata_O:
{
    llvmo::Metadata_O* obj_gc_safe = reinterpret_cast<llvmo::Metadata_O*>(client);
    GCObjectAllocator<llvmo::Metadata_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O:
{
    core::RuntimeVisibleEnvironment_O* obj_gc_safe = reinterpret_cast<core::RuntimeVisibleEnvironment_O*>(client);
    GCObjectAllocator<core::RuntimeVisibleEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantFP_O:
{
    llvmo::ConstantFP_O* obj_gc_safe = reinterpret_cast<llvmo::ConstantFP_O*>(client);
    GCObjectAllocator<llvmo::ConstantFP_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_CLASSALLOC_clbind__DummyCreator:
{
    clbind::DummyCreator* obj_gc_safe = reinterpret_cast<clbind::DummyCreator*>(client);
    GCObjectAllocator<clbind::DummyCreator>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__SequentialType_O:
{
    llvmo::SequentialType_O* obj_gc_safe = reinterpret_cast<llvmo::SequentialType_O*>(client);
    GCObjectAllocator<llvmo::SequentialType_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__HashTableEqual_O:
{
    core::HashTableEqual_O* obj_gc_safe = reinterpret_cast<core::HashTableEqual_O*>(client);
    GCObjectAllocator<core::HashTableEqual_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__CompositeType_O:
{
    llvmo::CompositeType_O* obj_gc_safe = reinterpret_cast<llvmo::CompositeType_O*>(client);
    GCObjectAllocator<llvmo::CompositeType_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__VAArgInst_O:
{
    llvmo::VAArgInst_O* obj_gc_safe = reinterpret_cast<llvmo::VAArgInst_O*>(client);
    GCObjectAllocator<llvmo::VAArgInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Argument_O:
{
    llvmo::Argument_O* obj_gc_safe = reinterpret_cast<llvmo::Argument_O*>(client);
    GCObjectAllocator<llvmo::Argument_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CandoException_O:
{
    core::CandoException_O* obj_gc_safe = reinterpret_cast<core::CandoException_O*>(client);
    GCObjectAllocator<core::CandoException_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Iterator_O:
{
    core::Iterator_O* obj_gc_safe = reinterpret_cast<core::Iterator_O*>(client);
    GCObjectAllocator<core::Iterator_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Record_O:
{
    core::Record_O* obj_gc_safe = reinterpret_cast<core::Record_O*>(client);
    GCObjectAllocator<core::Record_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__SwitchInst_O:
{
    llvmo::SwitchInst_O* obj_gc_safe = reinterpret_cast<llvmo::SwitchInst_O*>(client);
    GCObjectAllocator<llvmo::SwitchInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure:
{
    core::BuiltinClosure* obj_gc_safe = reinterpret_cast<core::BuiltinClosure*>(client);
    GCObjectAllocator<core::BuiltinClosure>::deallocate_unmanaged_instance(obj_gc_safe);
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__GlobalVariable_O:
{
    llvmo::GlobalVariable_O* obj_gc_safe = reinterpret_cast<llvmo::GlobalVariable_O*>(client);
    GCObjectAllocator<llvmo::GlobalVariable_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<core::DynamicBinding>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__General_O:
{
    core::General_O* obj_gc_safe = reinterpret_cast<core::General_O*>(client);
    GCObjectAllocator<core::General_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__BlockAddress_O:
{
    llvmo::BlockAddress_O* obj_gc_safe = reinterpret_cast<llvmo::BlockAddress_O*>(client);
    GCObjectAllocator<llvmo::BlockAddress_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ErrorContent_:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<asttooling::ErrorContent>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ValueAsMetadata_O:
{
    llvmo::ValueAsMetadata_O* obj_gc_safe = reinterpret_cast<llvmo::ValueAsMetadata_O*>(client);
    GCObjectAllocator<llvmo::ValueAsMetadata_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_clbind__ClassRep_O__:
{
    THROW_HARD_ERROR(BF("Should never deallocate containers gctools::GCVector_moveable<gctools::smart_ptr<clbind::ClassRep_O>>"));}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Pass_O:
{
    llvmo::Pass_O* obj_gc_safe = reinterpret_cast<llvmo::Pass_O*>(client);
    GCObjectAllocator<llvmo::Pass_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Attribute_O:
{
    llvmo::Attribute_O* obj_gc_safe = reinterpret_cast<llvmo::Attribute_O*>(client);
    GCObjectAllocator<llvmo::Attribute_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SourceFileInfo_O:
{
    core::SourceFileInfo_O* obj_gc_safe = reinterpret_cast<core::SourceFileInfo_O*>(client);
    GCObjectAllocator<core::SourceFileInfo_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SingleFloat_dummy_O:
{
    core::SingleFloat_dummy_O* obj_gc_safe = reinterpret_cast<core::SingleFloat_dummy_O*>(client);
    GCObjectAllocator<core::SingleFloat_dummy_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__TargetOptions_O:
{
    llvmo::TargetOptions_O* obj_gc_safe = reinterpret_cast<llvmo::TargetOptions_O*>(client);
    GCObjectAllocator<llvmo::TargetOptions_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__LLVMContext_O:
{
    llvmo::LLVMContext_O* obj_gc_safe = reinterpret_cast<llvmo::LLVMContext_O*>(client);
    GCObjectAllocator<llvmo::LLVMContext_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__AtomicCmpXchgInst_O:
{
    llvmo::AtomicCmpXchgInst_O* obj_gc_safe = reinterpret_cast<llvmo::AtomicCmpXchgInst_O*>(client);
    GCObjectAllocator<llvmo::AtomicCmpXchgInst_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__GlueEnvironment_O:
{
    core::GlueEnvironment_O* obj_gc_safe = reinterpret_cast<core::GlueEnvironment_O*>(client);
    GCObjectAllocator<core::GlueEnvironment_O>::deallocate_unmanaged_instance(obj_gc_safe);
    return;
}
#endif // defined(GC_OBJ_DEALLOCATOR)
#if defined(GC_OBJ_DEALLOCATOR_HELPERS)

#endif // defined(GC_OBJ_DEALLOCATOR_HELPERS)
#if defined(GC_OBJ_DEALLOCATOR_TABLE)
static void* OBJ_DEALLOCATOR_table[] = { 
  /* 5 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__APInt_O,
  /* 6 */ &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Symbol_O,
  /* 7 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__IRBuilder_O,
  /* 8 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionValueEnvironment_O,
  /* 9 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__FenceInst_O,
  /* 10 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__VectorObjects_O,
  /* 11 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionContainerEnvironment_O,
  /* 12 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantArray_O,
  /* 13 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__IntegerType_O,
  /* 14 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Function_O,
  /* 15 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantInt_O,
  /* 16 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__BranchInst_O,
  /* 17 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__,
  /* 18 */ &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__StandardObject_O,
  /* 19 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__DerivableFrontendActionFactory,
  /* 20 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__AtomicRMWInst_O,
  /* 21 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__UnaryInstruction_O,
  /* 22 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__DoubleFloat_O,
  /* 23 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__,
  /* 24 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_,
  /* 25 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantExpr_O,
  /* 26 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__LoadInst_O,
  /* 27 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__AttributeSet_O,
  /* 28 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Instance_O,
  /* 29 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__StoreInst_O,
  /* 30 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__MCSubtargetInfo_O,
  /* 31 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__,
  /* 32 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__CallInst_O,
  /* 33 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__PHINode_O,
  /* 34 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ImmutablePass_O,
  /* 35 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CompiledFunction_O,
  /* 36 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Rational_O,
  /* 37 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ForeignData_O,
  /* 38 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__VectorType_O,
  /* 39 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__FunctionPass_O,
  /* 40 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantStruct_O,
  /* 41 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__TargetSubtargetInfo_O,
  /* 42 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Instruction_O,
  /* 43 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__User_O,
  /* 44 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CatchEnvironment_O,
  /* 45 */ &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Str_O,
  /* 46 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Pointer_O,
  /* 47 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__PassManager_O,
  /* 48 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_clbind__ClassRegistry_O,
  /* 49 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__PointerType_O,
  /* 50 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DataLayout_O,
  /* 51 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__APFloat_O,
  /* 52 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ResumeInst_O,
  /* 53 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantDataArray_O,
  /* 54 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Number_O,
  /* 55 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__TargetLibraryInfo_O,
  /* 56 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__UndefValue_O,
  /* 57 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__EngineBuilder_O,
  /* 58 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Environment_O,
  /* 59 */ &&obj_deallocate_unmanaged_instance_KIND_GCARRAY_gctools__GCArray_moveable_core__SlotData_0_,
  /* 60 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ReturnInst_O,
  /* 61 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__MDString_O,
  /* 62 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Complex_O,
  /* 63 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SourceManager_O,
  /* 64 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__LLVMTargetMachine_O,
  /* 65 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LexicalEnvironment_O,
  /* 66 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SymbolMacroletEnvironment_O,
  /* 67 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__StackValueEnvironment_O,
  /* 68 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__PassManagerBuilder_O,
  /* 69 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__DerivableSyntaxOnlyAction,
  /* 70 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CompileTimeEnvironment_O,
  /* 71 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ExecutionEngine_O,
  /* 72 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__TagbodyFrame_O,
  /* 73 */ &&obj_deallocate_unmanaged_instance_KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__0_,
  /* 74 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__MacroletEnvironment_O,
  /* 75 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Linker_O,
  /* 76 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__DataLayoutPass_O,
  /* 77 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__NamedMDNode_O,
  /* 78 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantPointerNull_O,
  /* 79 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Constant_O,
  /* 80 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__String_O,
  /* 81 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_,
  /* 82 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SymbolToEnumConverter_O,
  /* 83 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__Message_,
  /* 84 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_clbind__ClassRep_O,
  /* 85 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Value_O,
  /* 86 */ &&obj_deallocate_unmanaged_instance_KIND_GCSTRING_gctools__GCString_moveable_char_,
  /* 87 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Function_O,
  /* 88 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Type_O,
  /* 89 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__DerivableMatchCallback,
  /* 90 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__LandingPadInst_O,
  /* 91 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Vector_O,
  /* 92 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__UnwindProtectEnvironment_O,
  /* 93 */ &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Metaobject_O,
  /* 94 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__InvokeInst_O,
  /* 95 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Real_O,
  /* 96 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Array_O,
  /* 97 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_,
  /* 98 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ShortFloat_O,
  /* 99 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__BlockEnvironment_O,
  /* 100 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Fixnum_dummy_O,
  /* 101 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__PassManagerBase_O,
  /* 102 */ &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Class_O,
  /* 103 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_asttooling__DerivableASTFrontendAction,
  /* 104 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O,
  /* 105 */ &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__BuiltInClass_O,
  /* 106 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ValueEnvironment_O,
  /* 107 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__IndirectBrInst_O,
  /* 108 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Target_O,
  /* 109 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Module_O,
  /* 110 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__FunctionFrame_O,
  /* 111 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Bignum_O,
  /* 112 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ExternalObject_O,
  /* 113 */ &&obj_deallocate_unmanaged_instance_KIND_TEMPLATED_LISPALLOC_core__WrappedPointer_O,
  /* 114 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Ratio_O,
  /* 115 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__WeakHashTable_O,
  /* 116 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__,
  /* 117 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__BasicBlock_O,
  /* 118 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__TerminatorInst_O,
  /* 119 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__FunctionType_O,
  /* 120 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__WeakKeyHashTable_O,
  /* 121 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CxxObject_O,
  /* 122 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ModulePass_O,
  /* 123 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__TagbodyEnvironment_O,
  /* 124 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__,
  /* 125 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__FunctionPassManager_O,
  /* 126 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantDataSequential_O,
  /* 127 */ &&obj_deallocate_unmanaged_instance_KIND_TEMPLATED_CLASSALLOC_clbind__ConstructorCreator,
  /* 128 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__GlobalValue_O,
  /* 129 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__TargetMachine_O,
  /* 130 */ &&obj_deallocate_unmanaged_instance_KIND_CLASSALLOC_core__MacroClosure,
  /* 131 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Integer_O,
  /* 132 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__UnreachableInst_O,
  /* 133 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Triple_O,
  /* 134 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__AllocaInst_O,
  /* 135 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__MDNode_O,
  /* 136 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__StructType_O,
  /* 137 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__IRBuilderBase_O,
  /* 138 */ &&obj_deallocate_unmanaged_instance_KIND_BOOTSTRAP_core__Specializer_O,
  /* 139 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_,
  /* 140 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__,
  /* 141 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SourcePosInfo_O,
  /* 142 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_,
  /* 143 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Float_O,
  /* 144 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__LongFloat_O,
  /* 145 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ArrayType_O,
  /* 146 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__ValueFrame_O,
  /* 147 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Metadata_O,
  /* 148 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O,
  /* 149 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ConstantFP_O,
  /* 150 */ &&obj_deallocate_unmanaged_instance_KIND_CLASSALLOC_clbind__DummyCreator,
  /* 151 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__SequentialType_O,
  /* 152 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__HashTableEqual_O,
  /* 153 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__CompositeType_O,
  /* 154 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__VAArgInst_O,
  /* 155 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Argument_O,
  /* 156 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__CandoException_O,
  /* 157 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Iterator_O,
  /* 158 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__Record_O,
  /* 159 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__SwitchInst_O,
  /* 160 */ &&obj_deallocate_unmanaged_instance_KIND_TEMPLATED_CLASSALLOC_core__BuiltinClosure,
  /* 161 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__GlobalVariable_O,
  /* 162 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_,
  /* 163 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__General_O,
  /* 164 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__BlockAddress_O,
  /* 165 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ErrorContent_,
  /* 166 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__ValueAsMetadata_O,
  /* 167 */ &&obj_deallocate_unmanaged_instance_KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_clbind__ClassRep_O__,
  /* 168 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Pass_O,
  /* 169 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__Attribute_O,
  /* 170 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SourceFileInfo_O,
  /* 171 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__SingleFloat_dummy_O,
  /* 172 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__TargetOptions_O,
  /* 173 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__LLVMContext_O,
  /* 174 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_llvmo__AtomicCmpXchgInst_O,
  /* 175 */ &&obj_deallocate_unmanaged_instance_KIND_LISPALLOC_core__GlueEnvironment_O,
   NULL
};
#endif // defined(GC_OBJ_DEALLOCATOR_TABLE)
#if defined(GC_GLOBALS)
#endif // defined(GC_GLOBALS)
#if defined(GC_GLOBAL_SYMBOLS)
#endif // defined(GC_GLOBAL_SYMBOLS)
