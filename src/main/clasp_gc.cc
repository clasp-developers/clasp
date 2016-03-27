#ifdef DECLARE_FORWARDS
 namespace asttooling {
    class DerivableSyntaxOnlyAction;
    class DerivableASTFrontendAction;
    class DerivableMatchCallback;
    class DerivableFrontendActionFactory;
    class AsttoolingExposer_O;
    class Message;
    class AstVisitor_O;
    class ErrorContent;
    class ParserValue;
    class ContextFrame;
    namespace RegMap {
        class RegistryMaps_O;
        class SymbolMatcherDescriptorPair_O;
    };
    namespace internal {
        class OverloadedMatcherDescriptor_O;
        class FixedArgCountMatcherDescriptor_O;
        class FreeFuncMatcherDescriptor_O;
        class VariadicOperatorMatcherDescriptor_O;
    };
 };
 namespace cffi {
    class Pointer_O;
 };
 namespace clbind {
    class DummyCreator_O;
    class ClassRep_O;
    class ClassRegistry_O;
    namespace detail {
        class class_map;
    };
 };
 namespace llvmo {
    class DataLayoutPass_O;
    class AtomicRMWInst_O;
    class FunctionPass_O;
    class BlockAddress_O;
    class Argument_O;
    class DIDerivedType_O;
    class ModulePass_O;
    class DIArray_O;
    class Value_O;
    class IRBuilderBase_O;
    class LandingPadInst_O;
    class GlobalValue_O;
    class DIBasicType_O;
    class DISubprogram_O;
    class ConstantArray_O;
    class Instruction_O;
    class PHINode_O;
    class UnreachableInst_O;
    class ValueAsMetadata_O;
    class ConstantInt_O;
    class ConstantDataSequential_O;
    class DIBuilder_O;
    class SwitchInst_O;
    class CallInst_O;
    class ConstantStruct_O;
    class SequentialType_O;
    class User_O;
    class Metadata_O;
    class DILexicalBlock_O;
    class VectorType_O;
    class DICompileUnit_O;
    class DebugLoc_O;
    class Attribute_O;
    class IntegerType_O;
    class ConstantDataArray_O;
    class ExecutionEngine_O;
    class StructType_O;
    class ImmutablePass_O;
    class Constant_O;
    class APFloat_O;
    class LoadInst_O;
    class ConstantFP_O;
    class PassManagerBuilder_O;
    class DataLayout_O;
    class Triple_O;
    class ReturnInst_O;
    class APInt_O;
    class LLVMTargetMachine_O;
    class DIDescriptor_O;
    class MDNode_O;
    class DIType_O;
    class DISubroutineType_O;
    class TargetLibraryInfo_O;
    class ResumeInst_O;
    class StoreInst_O;
    class IRBuilder_O;
    class PassManagerBase_O;
    class GlobalVariable_O;
    class TargetMachine_O;
    class FunctionPassManager_O;
    class UnaryInstruction_O;
    class BranchInst_O;
    class DICompositeType_O;
    class PointerType_O;
    class TargetOptions_O;
    class DITypeArray_O;
    class InsertPoint_O;
    class InvokeInst_O;
    class Function_O;
    class AttributeSet_O;
    class PassManager_O;
    class CompositeType_O;
    class BasicBlock_O;
    class AllocaInst_O;
    class AtomicCmpXchgInst_O;
    class Type_O;
    class ArrayType_O;
    class NamedMDNode_O;
    class UndefValue_O;
    class TerminatorInst_O;
    class Linker_O;
    class Pass_O;
    class DIFile_O;
    class MDString_O;
    class MCSubtargetInfo_O;
    class DebugInfo_O;
    class Module_O;
    class EngineBuilder_O;
    class ConstantPointerNull_O;
    class IndirectBrInst_O;
    class DIScope_O;
    class FenceInst_O;
    class TargetSubtargetInfo_O;
    class LLVMContext_O;
    class ConstantExpr_O;
    class FunctionType_O;
    class Target_O;
    class VAArgInst_O;
 };
 namespace core {
    class MultiStringBuffer_O;
    class HashTableEq_O;
    class FileStream_O;
    class BitVector_O;
    class Complex_O;
    class ArrayObjects_O;
    class CompiledFunction_O;
    class Bignum_O;
    class UnwindProtectEnvironment_O;
    class ReadTable_O;
    class Number_O;
    class SymbolMacroletEnvironment_O;
    class SingleDispatchGenericFunction_O;
    class FileStatus_O;
    class Real_O;
    class WeakHashTable_O;
    class InstanceClosure_O;
    class SpecialForm_O;
    class AnsiStream_O;
    class Rational_O;
    class ConcatenatedStream_O;
    class Str_O;
    class ActivationFrame_O;
    class Environment_O;
    class CompiledClosure_O;
    class Array_O;
    class StringStream_O;
    class KeywordArgument;
    class Exposer_O;
    class TagbodyFrame_O;
    class SingleDispatchMethod_O;
    class SexpSaveArchive_O;
    class RandomState_O;
    class UserData_O;
    class SequenceStepper_O;
    class StdClass_O;
    class FunctionContainerEnvironment_O;
    class HashTableEqualp_O;
    class StructureClass_O;
    class Integer_O;
    class FunctionValueEnvironment_O;
    class RegexMatch_O;
    class WeakPointer_O;
    class VaList_dummy_O;
    class ValueFrame_O;
    class Closure_O;
    class RuntimeVisibleEnvironment_O;
    class StandardObject_O;
    class SimpleBitVector_O;
    class ExternalObject_O;
    class LoadTimeValues_O;
    class SynonymStream_O;
    class VectorDisplaced_O;
    class String_O;
    class Binder_O;
    class LeafSNode_O;
    class BitVectorWithFillPtr_O;
    class IntArray_O;
    class SourceManager_O;
    class SaveArchive_O;
    class Record_O;
    class StackValueEnvironment_O;
    class Specializer_O;
    class Null_O;
    class T_O;
    class Functor_O;
    class LightUserData_O;
    class Symbol_O;
    class DoubleFloat_O;
    class ConsStepper_O;
    class SourcePosInfo_O;
    class VectorObjectsWithFillPtr_O;
    class RequiredArgument;
    class Float_O;
    class SymbolClassPair;
    class SymbolStorage;
    class BlockEnvironment_O;
    class LongFloat_O;
    class Regex_O;
    class StandardClass_O;
    class PosixTimeDuration_O;
    class SymbolToEnumConverter_O;
    class EchoStream_O;
    class CandoException_O;
    class DynamicBinding;
    class StringInputStream_O;
    class SingleDispatchEffectiveMethodFunction_O;
    class BranchSNode_O;
    class Stream_O;
    class Reader_O;
    class SingleDispatchGenericFunctionClosure_O;
    class SharpEqualWrapper_O;
    class Cons_O;
    class Archive_O;
    class HashTable_O;
    class MacroletEnvironment_O;
    class General_O;
    class InstanceCreator_O;
    class CxxObject_O;
    class WeakKeyMapping_O;
    class Metaobject_O;
    class Cache_O;
    class ArrayDisplaced_O;
    class LambdaListHandler_O;
    class HashTableEql_O;
    class ForwardReferencedClass_O;
    class SourceFileInfo_O;
    class SNode_O;
    class Path_O;
    class Vector_O;
    class StringOutputStream_O;
    class AuxArgument;
    class SexpLoadArchive_O;
    class DirectoryIterator_O;
    class SingleFloat_dummy_O;
    class CxxClass_O;
    class BuiltInClass_O;
    class ValueEnvironment_O;
    class StructureObject_O;
    class StrWithFillPtr_O;
    class InvocationHistoryFrameIterator_O;
    class IOStreamStream_O;
    class TagbodyEnvironment_O;
    class CompileTimeEnvironment_O;
    class Fixnum_dummy_O;
    class Package_O;
    class RecursiveDirectoryIterator_O;
    class IOFileStream_O;
    class TwoWayStream_O;
    class FunctionFrame_O;
    class FuncallableStandardClass_O;
    class Class_O;
    class DirectoryEntry_O;
    class Character_dummy_O;
    class HashTableEqual_O;
    class Function_O;
    class Pointer_O;
    class LogicalPathname_O;
    class SmallMultimap_O;
    class OptionalArgument;
    class InterpretedClosure_O;
    class WeakKeyHashTable_O;
    class ForeignData_O;
    class CacheRecord;
    class VectorStepper_O;
    class Instance_O;
    class BroadcastStream_O;
    class CatchEnvironment_O;
    class LexicalEnvironment_O;
    class MacroClosure_O;
    class Pathname_O;
    class PosixTime_O;
    class SmallMap_O;
    class ShortFloat_O;
    class Lisp_O;
    class ExceptionEntry;
    class GlueEnvironment_O;
    class CoreExposer_O;
    class VectorObjects_O;
    class FunctionClosure_O;
    class Ratio_O;
    class LoadArchive_O;
 };
#endif // DECLARE_FORWARDS
#if defined(GC_ENUM)
enum { KIND_null = 0, 
KIND_CONS = 4, 
KIND_CHARACTER = 3, 
KIND_SINGLE_FLOAT = 2, 
KIND_FIXNUM = 1, 
KIND_ROOTCLASSALLOC_clbind__detail__class_map = 5,
KIND_BOOTSTRAP_core__T_O = 6,
KIND_LISPALLOC_core__VaList_dummy_O = 7,
KIND_LISPALLOC_core__Cons_O = 8,
KIND_LISPALLOC_core__General_O = 9,
KIND_LISPALLOC_core__MultiStringBuffer_O = 10,
KIND_LISPALLOC_core__ReadTable_O = 11,
KIND_LISPALLOC_core__Number_O = 12,
KIND_LISPALLOC_core__Complex_O = 13,
KIND_LISPALLOC_core__Real_O = 14,
KIND_LISPALLOC_core__Rational_O = 15,
KIND_LISPALLOC_core__Integer_O = 16,
KIND_LISPALLOC_core__Bignum_O = 17,
KIND_LISPALLOC_core__Fixnum_dummy_O = 18,
KIND_LISPALLOC_core__Ratio_O = 19,
KIND_LISPALLOC_core__Float_O = 20,
KIND_LISPALLOC_core__DoubleFloat_O = 21,
KIND_LISPALLOC_core__LongFloat_O = 22,
KIND_LISPALLOC_core__SingleFloat_dummy_O = 23,
KIND_LISPALLOC_core__ShortFloat_O = 24,
KIND_LISPALLOC_core__FileStatus_O = 25,
KIND_LISPALLOC_core__WeakHashTable_O = 26,
KIND_LISPALLOC_core__WeakKeyHashTable_O = 27,
KIND_LISPALLOC_core__Environment_O = 28,
KIND_LISPALLOC_core__ActivationFrame_O = 29,
KIND_LISPALLOC_core__TagbodyFrame_O = 30,
KIND_LISPALLOC_core__ValueFrame_O = 31,
KIND_LISPALLOC_core__FunctionFrame_O = 32,
KIND_LISPALLOC_core__LexicalEnvironment_O = 33,
KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O = 34,
KIND_LISPALLOC_core__FunctionValueEnvironment_O = 35,
KIND_LISPALLOC_core__ValueEnvironment_O = 36,
KIND_LISPALLOC_core__TagbodyEnvironment_O = 37,
KIND_LISPALLOC_core__CompileTimeEnvironment_O = 38,
KIND_LISPALLOC_core__UnwindProtectEnvironment_O = 39,
KIND_LISPALLOC_core__SymbolMacroletEnvironment_O = 40,
KIND_LISPALLOC_core__FunctionContainerEnvironment_O = 41,
KIND_LISPALLOC_core__StackValueEnvironment_O = 42,
KIND_LISPALLOC_core__BlockEnvironment_O = 43,
KIND_LISPALLOC_core__MacroletEnvironment_O = 44,
KIND_LISPALLOC_core__CatchEnvironment_O = 45,
KIND_LISPALLOC_core__GlueEnvironment_O = 46,
KIND_LISPALLOC_core__Array_O = 47,
KIND_LISPALLOC_core__ArrayObjects_O = 48,
KIND_LISPALLOC_core__ArrayDisplaced_O = 49,
KIND_LISPALLOC_core__Vector_O = 50,
KIND_LISPALLOC_core__BitVector_O = 51,
KIND_LISPALLOC_core__SimpleBitVector_O = 52,
KIND_LISPALLOC_core__BitVectorWithFillPtr_O = 53,
KIND_LISPALLOC_core__VectorDisplaced_O = 54,
KIND_LISPALLOC_core__String_O = 55,
KIND_BOOTSTRAP_core__Str_O = 56,
KIND_LISPALLOC_core__StrWithFillPtr_O = 57,
KIND_LISPALLOC_core__VectorObjects_O = 58,
KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O = 59,
KIND_LISPALLOC_core__Exposer_O = 60,
KIND_LISPALLOC_asttooling__AsttoolingExposer_O = 61,
KIND_LISPALLOC_core__CoreExposer_O = 62,
KIND_LISPALLOC_core__SingleDispatchMethod_O = 63,
KIND_LISPALLOC_core__RandomState_O = 64,
KIND_TEMPLATED_LISPALLOC_core__WrappedPointer_O = 65,
KIND_LISPALLOC_core__SequenceStepper_O = 66,
KIND_LISPALLOC_core__ConsStepper_O = 67,
KIND_LISPALLOC_core__VectorStepper_O = 68,
KIND_LISPALLOC_llvmo__DebugLoc_O = 69,
KIND_LISPALLOC_llvmo__Attribute_O = 70,
KIND_LISPALLOC_core__RegexMatch_O = 71,
KIND_LISPALLOC_core__WeakPointer_O = 72,
KIND_BOOTSTRAP_core__StandardObject_O = 73,
KIND_BOOTSTRAP_core__Metaobject_O = 74,
KIND_BOOTSTRAP_core__Specializer_O = 75,
KIND_BOOTSTRAP_core__Class_O = 76,
KIND_BOOTSTRAP_core__StdClass_O = 77,
KIND_BOOTSTRAP_core__StandardClass_O = 78,
KIND_LISPALLOC_core__FuncallableStandardClass_O = 79,
KIND_BOOTSTRAP_core__StructureClass_O = 80,
KIND_LISPALLOC_core__ForwardReferencedClass_O = 81,
KIND_LISPALLOC_core__CxxClass_O = 82,
KIND_BOOTSTRAP_core__BuiltInClass_O = 83,
KIND_LISPALLOC_clbind__ClassRep_O = 84,
KIND_LISPALLOC_core__ExternalObject_O = 85,
KIND_LISPALLOC_llvmo__Value_O = 86,
KIND_LISPALLOC_llvmo__Argument_O = 87,
KIND_LISPALLOC_llvmo__User_O = 88,
KIND_LISPALLOC_llvmo__Instruction_O = 89,
KIND_LISPALLOC_llvmo__AtomicRMWInst_O = 90,
KIND_LISPALLOC_llvmo__LandingPadInst_O = 91,
KIND_LISPALLOC_llvmo__PHINode_O = 92,
KIND_LISPALLOC_llvmo__CallInst_O = 93,
KIND_LISPALLOC_llvmo__StoreInst_O = 94,
KIND_LISPALLOC_llvmo__UnaryInstruction_O = 95,
KIND_LISPALLOC_llvmo__LoadInst_O = 96,
KIND_LISPALLOC_llvmo__AllocaInst_O = 97,
KIND_LISPALLOC_llvmo__VAArgInst_O = 98,
KIND_LISPALLOC_llvmo__AtomicCmpXchgInst_O = 99,
KIND_LISPALLOC_llvmo__TerminatorInst_O = 100,
KIND_LISPALLOC_llvmo__UnreachableInst_O = 101,
KIND_LISPALLOC_llvmo__SwitchInst_O = 102,
KIND_LISPALLOC_llvmo__ReturnInst_O = 103,
KIND_LISPALLOC_llvmo__ResumeInst_O = 104,
KIND_LISPALLOC_llvmo__BranchInst_O = 105,
KIND_LISPALLOC_llvmo__InvokeInst_O = 106,
KIND_LISPALLOC_llvmo__IndirectBrInst_O = 107,
KIND_LISPALLOC_llvmo__FenceInst_O = 108,
KIND_LISPALLOC_llvmo__Constant_O = 109,
KIND_LISPALLOC_llvmo__BlockAddress_O = 110,
KIND_LISPALLOC_llvmo__GlobalValue_O = 111,
KIND_LISPALLOC_llvmo__GlobalVariable_O = 112,
KIND_LISPALLOC_llvmo__Function_O = 113,
KIND_LISPALLOC_llvmo__ConstantArray_O = 114,
KIND_LISPALLOC_llvmo__ConstantInt_O = 115,
KIND_LISPALLOC_llvmo__ConstantDataSequential_O = 116,
KIND_LISPALLOC_llvmo__ConstantDataArray_O = 117,
KIND_LISPALLOC_llvmo__ConstantStruct_O = 118,
KIND_LISPALLOC_llvmo__ConstantFP_O = 119,
KIND_LISPALLOC_llvmo__UndefValue_O = 120,
KIND_LISPALLOC_llvmo__ConstantPointerNull_O = 121,
KIND_LISPALLOC_llvmo__ConstantExpr_O = 122,
KIND_LISPALLOC_llvmo__BasicBlock_O = 123,
KIND_LISPALLOC_llvmo__IRBuilderBase_O = 124,
KIND_LISPALLOC_llvmo__IRBuilder_O = 125,
KIND_LISPALLOC_llvmo__DIBuilder_O = 126,
KIND_LISPALLOC_llvmo__Metadata_O = 127,
KIND_LISPALLOC_llvmo__ValueAsMetadata_O = 128,
KIND_LISPALLOC_llvmo__MDNode_O = 129,
KIND_LISPALLOC_llvmo__MDString_O = 130,
KIND_LISPALLOC_llvmo__ExecutionEngine_O = 131,
KIND_LISPALLOC_llvmo__APFloat_O = 132,
KIND_LISPALLOC_llvmo__PassManagerBuilder_O = 133,
KIND_LISPALLOC_llvmo__DataLayout_O = 134,
KIND_LISPALLOC_llvmo__Triple_O = 135,
KIND_LISPALLOC_llvmo__APInt_O = 136,
KIND_LISPALLOC_llvmo__PassManagerBase_O = 137,
KIND_LISPALLOC_llvmo__FunctionPassManager_O = 138,
KIND_LISPALLOC_llvmo__PassManager_O = 139,
KIND_LISPALLOC_llvmo__TargetMachine_O = 140,
KIND_LISPALLOC_llvmo__LLVMTargetMachine_O = 141,
KIND_LISPALLOC_llvmo__TargetOptions_O = 142,
KIND_LISPALLOC_llvmo__Type_O = 143,
KIND_LISPALLOC_llvmo__IntegerType_O = 144,
KIND_LISPALLOC_llvmo__CompositeType_O = 145,
KIND_LISPALLOC_llvmo__SequentialType_O = 146,
KIND_LISPALLOC_llvmo__VectorType_O = 147,
KIND_LISPALLOC_llvmo__PointerType_O = 148,
KIND_LISPALLOC_llvmo__ArrayType_O = 149,
KIND_LISPALLOC_llvmo__StructType_O = 150,
KIND_LISPALLOC_llvmo__FunctionType_O = 151,
KIND_LISPALLOC_llvmo__NamedMDNode_O = 152,
KIND_LISPALLOC_llvmo__Linker_O = 153,
KIND_LISPALLOC_llvmo__Pass_O = 154,
KIND_LISPALLOC_llvmo__FunctionPass_O = 155,
KIND_LISPALLOC_llvmo__ModulePass_O = 156,
KIND_LISPALLOC_llvmo__ImmutablePass_O = 157,
KIND_LISPALLOC_llvmo__DataLayoutPass_O = 158,
KIND_LISPALLOC_llvmo__TargetLibraryInfo_O = 159,
KIND_LISPALLOC_llvmo__MCSubtargetInfo_O = 160,
KIND_LISPALLOC_llvmo__TargetSubtargetInfo_O = 161,
KIND_LISPALLOC_llvmo__Module_O = 162,
KIND_LISPALLOC_llvmo__EngineBuilder_O = 163,
KIND_LISPALLOC_core__ForeignData_O = 164,
KIND_LISPALLOC_llvmo__LLVMContext_O = 165,
KIND_LISPALLOC_llvmo__Target_O = 166,
KIND_LISPALLOC_core__LoadTimeValues_O = 167,
KIND_LISPALLOC_core__Binder_O = 168,
KIND_LISPALLOC_core__IntArray_O = 169,
KIND_LISPALLOC_core__SourceManager_O = 170,
KIND_LISPALLOC_core__Record_O = 171,
KIND_LISPALLOC_core__Functor_O = 172,
KIND_LISPALLOC_core__Closure_O = 173,
KIND_LISPALLOC_core__FunctionClosure_O = 174,
KIND_LISPALLOC_core__InstanceClosure_O = 175,
KIND_LISPALLOC_core__CompiledClosure_O = 176,
KIND_TEMPLATED_LISPALLOC_core__BuiltinClosure_O = 177,
KIND_LISPALLOC_core__MacroClosure_O = 178,
KIND_LISPALLOC_core__SingleDispatchGenericFunctionClosure_O = 179,
KIND_LISPALLOC_core__InterpretedClosure_O = 180,
KIND_LISPALLOC_core__LightUserData_O = 181,
KIND_LISPALLOC_core__UserData_O = 182,
KIND_BOOTSTRAP_core__Symbol_O = 183,
KIND_LISPALLOC_core__Null_O = 184,
KIND_LISPALLOC_core__SourcePosInfo_O = 185,
KIND_TEMPLATED_LISPALLOC_core__Iterator_O = 186,
KIND_LISPALLOC_core__DirectoryIterator_O = 187,
KIND_LISPALLOC_core__RecursiveDirectoryIterator_O = 188,
KIND_LISPALLOC_core__Regex_O = 189,
KIND_LISPALLOC_core__PosixTimeDuration_O = 190,
KIND_LISPALLOC_core__SymbolToEnumConverter_O = 191,
KIND_LISPALLOC_core__CandoException_O = 192,
KIND_LISPALLOC_core__Stream_O = 193,
KIND_LISPALLOC_core__AnsiStream_O = 194,
KIND_LISPALLOC_core__FileStream_O = 195,
KIND_LISPALLOC_core__IOStreamStream_O = 196,
KIND_LISPALLOC_core__IOFileStream_O = 197,
KIND_LISPALLOC_core__ConcatenatedStream_O = 198,
KIND_LISPALLOC_core__StringStream_O = 199,
KIND_LISPALLOC_core__StringInputStream_O = 200,
KIND_LISPALLOC_core__StringOutputStream_O = 201,
KIND_LISPALLOC_core__SynonymStream_O = 202,
KIND_LISPALLOC_core__EchoStream_O = 203,
KIND_LISPALLOC_core__TwoWayStream_O = 204,
KIND_LISPALLOC_core__BroadcastStream_O = 205,
KIND_LISPALLOC_core__Reader_O = 206,
KIND_LISPALLOC_core__SharpEqualWrapper_O = 207,
KIND_LISPALLOC_asttooling__RegMap__RegistryMaps_O = 208,
KIND_LISPALLOC_core__Archive_O = 209,
KIND_LISPALLOC_core__SaveArchive_O = 210,
KIND_LISPALLOC_core__SexpSaveArchive_O = 211,
KIND_LISPALLOC_core__LoadArchive_O = 212,
KIND_LISPALLOC_core__SexpLoadArchive_O = 213,
KIND_LISPALLOC_core__HashTable_O = 214,
KIND_LISPALLOC_core__HashTableEq_O = 215,
KIND_LISPALLOC_core__HashTableEqualp_O = 216,
KIND_LISPALLOC_core__HashTableEql_O = 217,
KIND_LISPALLOC_core__HashTableEqual_O = 218,
KIND_TEMPLATED_LISPALLOC_core__Creator_O = 219,
KIND_LISPALLOC_clbind__DummyCreator_O = 220,
KIND_TEMPLATED_LISPALLOC_clbind__ConstructorCreator_O = 221,
KIND_LISPALLOC_core__InstanceCreator_O = 222,
KIND_LISPALLOC_cffi__Pointer_O = 223,
KIND_LISPALLOC_core__CxxObject_O = 224,
KIND_LISPALLOC_core__WeakKeyMapping_O = 225,
KIND_LISPALLOC_core__Cache_O = 226,
KIND_LISPALLOC_core__LambdaListHandler_O = 227,
KIND_LISPALLOC_llvmo__InsertPoint_O = 228,
KIND_LISPALLOC_core__SourceFileInfo_O = 229,
KIND_LISPALLOC_core__SNode_O = 230,
KIND_LISPALLOC_core__LeafSNode_O = 231,
KIND_LISPALLOC_core__BranchSNode_O = 232,
KIND_LISPALLOC_core__Path_O = 233,
KIND_LISPALLOC_asttooling__AstVisitor_O = 234,
KIND_LISPALLOC_llvmo__AttributeSet_O = 235,
KIND_LISPALLOC_core__StructureObject_O = 236,
KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O = 237,
KIND_LISPALLOC_core__Package_O = 238,
KIND_LISPALLOC_core__DirectoryEntry_O = 239,
KIND_LISPALLOC_core__Character_dummy_O = 240,
KIND_LISPALLOC_core__Function_O = 241,
KIND_LISPALLOC_core__CompiledFunction_O = 242,
KIND_LISPALLOC_core__SingleDispatchGenericFunction_O = 243,
KIND_LISPALLOC_core__SpecialForm_O = 244,
KIND_LISPALLOC_core__SingleDispatchEffectiveMethodFunction_O = 245,
KIND_LISPALLOC_core__Instance_O = 246,
KIND_LISPALLOC_core__Pointer_O = 247,
KIND_LISPALLOC_clbind__ClassRegistry_O = 248,
KIND_LISPALLOC_llvmo__DebugInfo_O = 249,
KIND_LISPALLOC_llvmo__DIDerivedType_O = 250,
KIND_LISPALLOC_llvmo__DIArray_O = 251,
KIND_LISPALLOC_llvmo__DIBasicType_O = 252,
KIND_LISPALLOC_llvmo__DISubprogram_O = 253,
KIND_LISPALLOC_llvmo__DILexicalBlock_O = 254,
KIND_LISPALLOC_llvmo__DICompileUnit_O = 255,
KIND_LISPALLOC_llvmo__DIDescriptor_O = 256,
KIND_LISPALLOC_llvmo__DIType_O = 257,
KIND_LISPALLOC_llvmo__DISubroutineType_O = 258,
KIND_LISPALLOC_llvmo__DICompositeType_O = 259,
KIND_LISPALLOC_llvmo__DITypeArray_O = 260,
KIND_LISPALLOC_llvmo__DIFile_O = 261,
KIND_LISPALLOC_llvmo__DIScope_O = 262,
KIND_LISPALLOC_core__SmallMultimap_O = 263,
KIND_LISPALLOC_core__Pathname_O = 264,
KIND_LISPALLOC_core__LogicalPathname_O = 265,
KIND_LISPALLOC_core__PosixTime_O = 266,
KIND_LISPALLOC_core__SmallMap_O = 267,
KIND_ROOTCLASSALLOC_core__Lisp_O = 268,
KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_ = 269,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__ = 270,
KIND_GCSTRING_gctools__GCString_moveable_char_ = 271,
KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_ = 272,
KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_O_ = 273,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__ = 274,
KIND_LISPALLOC_asttooling__internal__VariadicOperatorMatcherDescriptor_O = 275,
KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__Symbol_O__gctools__smart_ptr_core__T_O___ = 276,
KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolStorage_ = 277,
KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_ = 278,
KIND_GCVECTOR_gctools__GCVector_moveable_core__T_O_P_ = 279,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_asttooling__internal__MatcherDescriptor_O__ = 280,
KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_ = 281,
KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ParserValue_ = 282,
KIND_LISPALLOC_asttooling__internal__FreeFuncMatcherDescriptor_O = 283,
KIND_LISPALLOC_asttooling__internal__FixedArgCountMatcherDescriptor_O = 284,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__ = 285,
KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_ = 286,
KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__T_O__gctools__smart_ptr_core__T_O___ = 287,
KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__ = 288,
KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_ = 289,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__ = 290,
KIND_LISPALLOC_asttooling__DerivableFrontendActionFactory = 291,
KIND_LISPALLOC_asttooling__DerivableMatchCallback = 292,
KIND_LISPALLOC_asttooling__internal__OverloadedMatcherDescriptor_O = 293,
KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ErrorContent_ = 294,
KIND_LISPALLOC_asttooling__DerivableASTFrontendAction = 295,
KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__Message_ = 296,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__ = 297,
KIND_LISPALLOC_asttooling__DerivableSyntaxOnlyAction = 298,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SingleDispatchMethod_O__ = 299,
KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_ = 300,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SequenceStepper_O__ = 301,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__ = 302,
KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_ = 303,
KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_ = 304,
KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_clbind__ClassRep_O__ = 305,
  KIND_max = 305
}
#endif // defined(GC_ENUM)
#if defined(GC_DYNAMIC_CAST)
template <typename FP> struct Cast<llvmo::VAArgInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 98 98 
      return (kindVal == 98);
  };
};
template <typename FP> struct Cast<core::LoadArchive_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 212 213 
      return ((212 <= kindVal) && (kindVal <= 213));
  };
};
template <typename FP> struct Cast<core::Ratio_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 19 19 
      return (kindVal == 19);
  };
};
template <typename FP> struct Cast<core::FunctionClosure_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 174 180 
      return ((174 <= kindVal) && (kindVal <= 180));
  };
};
template <typename FP> struct Cast<core::VectorObjects_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 58 59 
      return ((58 <= kindVal) && (kindVal <= 59));
  };
};
template <typename FP> struct Cast<core::CoreExposer_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 62 62 
      return (kindVal == 62);
  };
};
template <typename FP> struct Cast<core::GlueEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 46 46 
      return (kindVal == 46);
  };
};
template <typename FP> struct Cast<llvmo::Target_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 166 166 
      return (kindVal == 166);
  };
};
template <typename FP> struct Cast<llvmo::FunctionType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 151 151 
      return (kindVal == 151);
  };
};
template <typename FP> struct Cast<llvmo::ConstantExpr_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 122 122 
      return (kindVal == 122);
  };
};
template <typename FP> struct Cast<llvmo::LLVMContext_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 165 165 
      return (kindVal == 165);
  };
};
template <typename FP> struct Cast<core::Lisp_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 268 268 
      return (kindVal == 268);
  };
};
template <typename FP> struct Cast<llvmo::TargetSubtargetInfo_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 161 161 
      return (kindVal == 161);
  };
};
template <typename FP> struct Cast<core::ShortFloat_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 24 24 
      return (kindVal == 24);
  };
};
template <typename FP> struct Cast<core::SmallMap_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 267 267 
      return (kindVal == 267);
  };
};
template <typename FP> struct Cast<llvmo::FenceInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 108 108 
      return (kindVal == 108);
  };
};
template <typename FP> struct Cast<core::PosixTime_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 266 266 
      return (kindVal == 266);
  };
};
template <typename FP> struct Cast<core::Pathname_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 264 265 
      return ((264 <= kindVal) && (kindVal <= 265));
  };
};
template <typename FP> struct Cast<core::MacroClosure_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 178 178 
      return (kindVal == 178);
  };
};
template <typename FP> struct Cast<core::LexicalEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 33 45 
      return ((33 <= kindVal) && (kindVal <= 45));
  };
};
template <typename FP> struct Cast<core::CatchEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 45 45 
      return (kindVal == 45);
  };
};
template <typename FP> struct Cast<core::BroadcastStream_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 205 205 
      return (kindVal == 205);
  };
};
template <typename FP> struct Cast<core::Instance_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 246 246 
      return (kindVal == 246);
  };
};
template <typename FP> struct Cast<core::VectorStepper_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 68 68 
      return (kindVal == 68);
  };
};
template <typename FP> struct Cast<llvmo::DIScope_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 262 262 
      return (kindVal == 262);
  };
};
template <typename FP> struct Cast<core::ForeignData_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 164 164 
      return (kindVal == 164);
  };
};
template <typename FP> struct Cast<core::WeakKeyHashTable_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 27 27 
      return (kindVal == 27);
  };
};
template <typename FP> struct Cast<llvmo::IndirectBrInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 107 107 
      return (kindVal == 107);
  };
};
template <typename FP> struct Cast<core::InterpretedClosure_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 180 180 
      return (kindVal == 180);
  };
};
template <typename FP> struct Cast<llvmo::ConstantPointerNull_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 121 121 
      return (kindVal == 121);
  };
};
template <typename FP> struct Cast<llvmo::EngineBuilder_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 163 163 
      return (kindVal == 163);
  };
};
template <typename FP> struct Cast<llvmo::Module_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 162 162 
      return (kindVal == 162);
  };
};
template <typename FP> struct Cast<core::SmallMultimap_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 263 263 
      return (kindVal == 263);
  };
};
template <typename FP> struct Cast<core::LogicalPathname_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 265 265 
      return (kindVal == 265);
  };
};
template <typename FP> struct Cast<llvmo::DebugInfo_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 249 262 
      return ((249 <= kindVal) && (kindVal <= 262));
  };
};
template <typename FP> struct Cast<clbind::ClassRegistry_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 248 248 
      return (kindVal == 248);
  };
};
template <typename FP> struct Cast<core::Pointer_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 247 247 
      return (kindVal == 247);
  };
};
template <typename FP> struct Cast<core::Function_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 241 246 
      return ((241 <= kindVal) && (kindVal <= 246));
  };
};
template <typename FP> struct Cast<core::HashTableEqual_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 218 218 
      return (kindVal == 218);
  };
};
template <typename FP> struct Cast<core::Character_dummy_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 240 240 
      return (kindVal == 240);
  };
};
template <typename FP> struct Cast<core::DirectoryEntry_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 239 239 
      return (kindVal == 239);
  };
};
template <typename FP> struct Cast<llvmo::MCSubtargetInfo_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 160 161 
      return ((160 <= kindVal) && (kindVal <= 161));
  };
};
template <typename FP> struct Cast<llvmo::MDString_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 130 130 
      return (kindVal == 130);
  };
};
template <typename FP> struct Cast<core::Class_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 76 84 
      return ((76 <= kindVal) && (kindVal <= 84));
  };
};
template <typename FP> struct Cast<core::FuncallableStandardClass_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 79 79 
      return (kindVal == 79);
  };
};
template <typename FP> struct Cast<llvmo::DIFile_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 261 261 
      return (kindVal == 261);
  };
};
template <typename FP> struct Cast<llvmo::Pass_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 154 159 
      return ((154 <= kindVal) && (kindVal <= 159));
  };
};
template <typename FP> struct Cast<llvmo::Linker_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 153 153 
      return (kindVal == 153);
  };
};
template <typename FP> struct Cast<llvmo::TerminatorInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 100 107 
      return ((100 <= kindVal) && (kindVal <= 107));
  };
};
template <typename FP> struct Cast<core::FunctionFrame_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 32 32 
      return (kindVal == 32);
  };
};
template <typename FP> struct Cast<llvmo::UndefValue_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 120 120 
      return (kindVal == 120);
  };
};
template <typename FP> struct Cast<core::TwoWayStream_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 204 204 
      return (kindVal == 204);
  };
};
template <typename FP> struct Cast<core::IOFileStream_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 197 197 
      return (kindVal == 197);
  };
};
template <typename FP> struct Cast<llvmo::NamedMDNode_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 152 152 
      return (kindVal == 152);
  };
};
template <typename FP> struct Cast<llvmo::ArrayType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 149 149 
      return (kindVal == 149);
  };
};
template <typename FP> struct Cast<core::RecursiveDirectoryIterator_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 188 188 
      return (kindVal == 188);
  };
};
template <typename FP> struct Cast<llvmo::Type_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 143 151 
      return ((143 <= kindVal) && (kindVal <= 151));
  };
};
template <typename FP> struct Cast<llvmo::AtomicCmpXchgInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 99 99 
      return (kindVal == 99);
  };
};
template <typename FP> struct Cast<llvmo::AllocaInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 97 97 
      return (kindVal == 97);
  };
};
template <typename FP> struct Cast<core::Package_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 238 238 
      return (kindVal == 238);
  };
};
template <typename FP> struct Cast<core::Fixnum_dummy_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 18 18 
      return (kindVal == 18);
  };
};
template <typename FP> struct Cast<llvmo::BasicBlock_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 123 123 
      return (kindVal == 123);
  };
};
template <typename FP> struct Cast<llvmo::CompositeType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 145 150 
      return ((145 <= kindVal) && (kindVal <= 150));
  };
};
template <typename FP> struct Cast<core::CompileTimeEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 38 45 
      return ((38 <= kindVal) && (kindVal <= 45));
  };
};
template <typename FP> struct Cast<llvmo::PassManager_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 139 139 
      return (kindVal == 139);
  };
};
template <typename FP> struct Cast<core::TagbodyEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 37 37 
      return (kindVal == 37);
  };
};
template <typename FP> struct Cast<core::IOStreamStream_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 196 196 
      return (kindVal == 196);
  };
};
template <typename FP> struct Cast<core::InvocationHistoryFrameIterator_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 237 237 
      return (kindVal == 237);
  };
};
template <typename FP> struct Cast<core::StrWithFillPtr_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 57 57 
      return (kindVal == 57);
  };
};
template <typename FP> struct Cast<core::StructureObject_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 236 236 
      return (kindVal == 236);
  };
};
template <typename FP> struct Cast<core::ValueEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 36 36 
      return (kindVal == 36);
  };
};
template <typename FP> struct Cast<llvmo::AttributeSet_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 235 235 
      return (kindVal == 235);
  };
};
template <typename FP> struct Cast<core::BuiltInClass_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 83 84 
      return ((83 <= kindVal) && (kindVal <= 84));
  };
};
template <typename FP> struct Cast<core::CxxClass_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 82 82 
      return (kindVal == 82);
  };
};
template <typename FP> struct Cast<core::SingleFloat_dummy_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 23 23 
      return (kindVal == 23);
  };
};
template <typename FP> struct Cast<core::DirectoryIterator_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 187 187 
      return (kindVal == 187);
  };
};
template <typename FP> struct Cast<core::SexpLoadArchive_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 213 213 
      return (kindVal == 213);
  };
};
template <typename FP> struct Cast<core::StringOutputStream_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 201 201 
      return (kindVal == 201);
  };
};
template <typename FP> struct Cast<asttooling::AstVisitor_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 234 234 
      return (kindVal == 234);
  };
};
template <typename FP> struct Cast<core::Vector_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 50 59 
      return ((50 <= kindVal) && (kindVal <= 59));
  };
};
template <typename FP> struct Cast<core::Path_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 233 233 
      return (kindVal == 233);
  };
};
template <typename FP> struct Cast<llvmo::Function_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 113 113 
      return (kindVal == 113);
  };
};
template <typename FP> struct Cast<llvmo::InvokeInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 106 106 
      return (kindVal == 106);
  };
};
template <typename FP> struct Cast<core::SNode_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 230 232 
      return ((230 <= kindVal) && (kindVal <= 232));
  };
};
template <typename FP> struct Cast<core::SourceFileInfo_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 229 229 
      return (kindVal == 229);
  };
};
template <typename FP> struct Cast<llvmo::InsertPoint_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 228 228 
      return (kindVal == 228);
  };
};
template <typename FP> struct Cast<llvmo::DITypeArray_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 260 260 
      return (kindVal == 260);
  };
};
template <typename FP> struct Cast<core::ForwardReferencedClass_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 81 81 
      return (kindVal == 81);
  };
};
template <typename FP> struct Cast<core::HashTableEql_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 217 217 
      return (kindVal == 217);
  };
};
template <typename FP> struct Cast<core::LambdaListHandler_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 227 227 
      return (kindVal == 227);
  };
};
template <typename FP> struct Cast<core::ArrayDisplaced_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 49 49 
      return (kindVal == 49);
  };
};
template <typename FP> struct Cast<core::Cache_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 226 226 
      return (kindVal == 226);
  };
};
template <typename FP> struct Cast<llvmo::TargetOptions_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 142 142 
      return (kindVal == 142);
  };
};
template <typename FP> struct Cast<core::Metaobject_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 74 84 
      return ((74 <= kindVal) && (kindVal <= 84));
  };
};
template <typename FP> struct Cast<llvmo::PointerType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 148 148 
      return (kindVal == 148);
  };
};
template <typename FP> struct Cast<core::WeakKeyMapping_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 225 225 
      return (kindVal == 225);
  };
};
template <typename FP> struct Cast<core::CxxObject_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 224 224 
      return (kindVal == 224);
  };
};
template <typename FP> struct Cast<cffi::Pointer_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 223 223 
      return (kindVal == 223);
  };
};
template <typename FP> struct Cast<core::InstanceCreator_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 222 222 
      return (kindVal == 222);
  };
};
template <typename FP> struct Cast<llvmo::DICompositeType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 259 259 
      return (kindVal == 259);
  };
};
template <typename FP> struct Cast<core::General_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 9 267 
      return ((9 <= kindVal) && (kindVal <= 267));
  };
};
template <typename FP> struct Cast<core::Creator_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 219 222 
      return ((219 <= kindVal) && (kindVal <= 222));
  };
};
template <typename FP> struct Cast<llvmo::BranchInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 105 105 
      return (kindVal == 105);
  };
};
template <typename FP> struct Cast<core::MacroletEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 44 44 
      return (kindVal == 44);
  };
};
template <typename FP> struct Cast<core::HashTable_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 214 218 
      return ((214 <= kindVal) && (kindVal <= 218));
  };
};
template <typename FP> struct Cast<core::Archive_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 209 213 
      return ((209 <= kindVal) && (kindVal <= 213));
  };
};
template <typename FP> struct Cast<asttooling::RegMap::RegistryMaps_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 208 208 
      return (kindVal == 208);
  };
};
template <typename FP> struct Cast<core::Cons_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 8 8 
      return (kindVal == 8);
  };
};
template <typename FP> struct Cast<core::SharpEqualWrapper_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 207 207 
      return (kindVal == 207);
  };
};
template <typename FP> struct Cast<core::SingleDispatchGenericFunctionClosure_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 179 179 
      return (kindVal == 179);
  };
};
template <typename FP> struct Cast<core::Reader_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 206 206 
      return (kindVal == 206);
  };
};
template <typename FP> struct Cast<core::Stream_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 193 205 
      return ((193 <= kindVal) && (kindVal <= 205));
  };
};
template <typename FP> struct Cast<llvmo::UnaryInstruction_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 95 98 
      return ((95 <= kindVal) && (kindVal <= 98));
  };
};
template <typename FP> struct Cast<core::BranchSNode_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 232 232 
      return (kindVal == 232);
  };
};
template <typename FP> struct Cast<llvmo::FunctionPassManager_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 138 138 
      return (kindVal == 138);
  };
};
template <typename FP> struct Cast<core::SingleDispatchEffectiveMethodFunction_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 245 245 
      return (kindVal == 245);
  };
};
template <typename FP> struct Cast<core::StringInputStream_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 200 200 
      return (kindVal == 200);
  };
};
template <typename FP> struct Cast<llvmo::TargetMachine_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 140 141 
      return ((140 <= kindVal) && (kindVal <= 141));
  };
};
template <typename FP> struct Cast<core::CandoException_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 192 192 
      return (kindVal == 192);
  };
};
template <typename FP> struct Cast<llvmo::GlobalVariable_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 112 112 
      return (kindVal == 112);
  };
};
template <typename FP> struct Cast<core::EchoStream_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 203 203 
      return (kindVal == 203);
  };
};
template <typename FP> struct Cast<core::SymbolToEnumConverter_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 191 191 
      return (kindVal == 191);
  };
};
template <typename FP> struct Cast<core::PosixTimeDuration_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 190 190 
      return (kindVal == 190);
  };
};
template <typename FP> struct Cast<core::StandardClass_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 78 78 
      return (kindVal == 78);
  };
};
template <typename FP> struct Cast<core::Regex_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 189 189 
      return (kindVal == 189);
  };
};
template <typename FP> struct Cast<llvmo::PassManagerBase_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 137 139 
      return ((137 <= kindVal) && (kindVal <= 139));
  };
};
template <typename FP> struct Cast<core::Iterator_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 186 188 
      return ((186 <= kindVal) && (kindVal <= 188));
  };
};
template <typename FP> struct Cast<clbind::ClassRep_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 84 84 
      return (kindVal == 84);
  };
};
template <typename FP> struct Cast<llvmo::IRBuilder_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 125 125 
      return (kindVal == 125);
  };
};
template <typename FP> struct Cast<llvmo::StoreInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 94 94 
      return (kindVal == 94);
  };
};
template <typename FP> struct Cast<core::LongFloat_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 22 22 
      return (kindVal == 22);
  };
};
template <typename FP> struct Cast<core::BlockEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 43 43 
      return (kindVal == 43);
  };
};
template <typename FP> struct Cast<llvmo::ResumeInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 104 104 
      return (kindVal == 104);
  };
};
template <typename FP> struct Cast<llvmo::TargetLibraryInfo_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 159 159 
      return (kindVal == 159);
  };
};
template <typename FP> struct Cast<llvmo::DISubroutineType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 258 258 
      return (kindVal == 258);
  };
};
template <typename FP> struct Cast<llvmo::DIType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 257 257 
      return (kindVal == 257);
  };
};
template <typename FP> struct Cast<core::Float_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 20 24 
      return ((20 <= kindVal) && (kindVal <= 24));
  };
};
template <typename FP> struct Cast<core::VectorObjectsWithFillPtr_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 59 59 
      return (kindVal == 59);
  };
};
template <typename FP> struct Cast<llvmo::MDNode_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 129 129 
      return (kindVal == 129);
  };
};
template <typename FP> struct Cast<core::SourcePosInfo_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 185 185 
      return (kindVal == 185);
  };
};
template <typename FP> struct Cast<llvmo::DIDescriptor_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 256 256 
      return (kindVal == 256);
  };
};
template <typename FP> struct Cast<llvmo::LLVMTargetMachine_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 141 141 
      return (kindVal == 141);
  };
};
template <typename FP> struct Cast<llvmo::APInt_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 136 136 
      return (kindVal == 136);
  };
};
template <typename FP> struct Cast<llvmo::ReturnInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 103 103 
      return (kindVal == 103);
  };
};
template <typename FP> struct Cast<core::ConsStepper_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 67 67 
      return (kindVal == 67);
  };
};
template <typename FP> struct Cast<llvmo::Triple_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 135 135 
      return (kindVal == 135);
  };
};
template <typename FP> struct Cast<core::DoubleFloat_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 21 21 
      return (kindVal == 21);
  };
};
template <typename FP> struct Cast<core::Symbol_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 183 184 
      return ((183 <= kindVal) && (kindVal <= 184));
  };
};
template <typename FP> struct Cast<llvmo::DataLayout_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 134 134 
      return (kindVal == 134);
  };
};
template <typename FP> struct Cast<core::LightUserData_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 181 182 
      return ((181 <= kindVal) && (kindVal <= 182));
  };
};
template <typename FP> struct Cast<core::Functor_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 172 180 
      return ((172 <= kindVal) && (kindVal <= 180));
  };
};
template <typename FP> struct Cast<core::T_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 6 267 
      return ((6 <= kindVal) && (kindVal <= 267));
  };
};
template <typename FP> struct Cast<core::Null_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 184 184 
      return (kindVal == 184);
  };
};
template <typename FP> struct Cast<asttooling::AsttoolingExposer_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 61 61 
      return (kindVal == 61);
  };
};
template <typename FP> struct Cast<llvmo::PassManagerBuilder_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 133 133 
      return (kindVal == 133);
  };
};
template <typename FP> struct Cast<core::Specializer_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 75 84 
      return ((75 <= kindVal) && (kindVal <= 84));
  };
};
template <typename FP> struct Cast<core::StackValueEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 42 42 
      return (kindVal == 42);
  };
};
template <typename FP> struct Cast<llvmo::ConstantFP_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 119 119 
      return (kindVal == 119);
  };
};
template <typename FP> struct Cast<llvmo::LoadInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 96 96 
      return (kindVal == 96);
  };
};
template <typename FP> struct Cast<llvmo::APFloat_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 132 132 
      return (kindVal == 132);
  };
};
template <typename FP> struct Cast<core::Record_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 171 171 
      return (kindVal == 171);
  };
};
template <typename FP> struct Cast<core::SaveArchive_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 210 211 
      return ((210 <= kindVal) && (kindVal <= 211));
  };
};
template <typename FP> struct Cast<core::SourceManager_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 170 170 
      return (kindVal == 170);
  };
};
template <typename FP> struct Cast<core::IntArray_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 169 169 
      return (kindVal == 169);
  };
};
template <typename FP> struct Cast<clbind::detail::class_map*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 5 5 
      return (kindVal == 5);
  };
};
template <typename FP> struct Cast<core::BitVectorWithFillPtr_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 53 53 
      return (kindVal == 53);
  };
};
template <typename FP> struct Cast<core::LeafSNode_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 231 231 
      return (kindVal == 231);
  };
};
template <typename FP> struct Cast<llvmo::Constant_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 109 122 
      return ((109 <= kindVal) && (kindVal <= 122));
  };
};
template <typename FP> struct Cast<core::Binder_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 168 168 
      return (kindVal == 168);
  };
};
template <typename FP> struct Cast<core::String_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 55 57 
      return ((55 <= kindVal) && (kindVal <= 57));
  };
};
template <typename FP> struct Cast<core::VectorDisplaced_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 54 54 
      return (kindVal == 54);
  };
};
template <typename FP> struct Cast<core::SynonymStream_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 202 202 
      return (kindVal == 202);
  };
};
template <typename FP> struct Cast<core::LoadTimeValues_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 167 167 
      return (kindVal == 167);
  };
};
template <typename FP> struct Cast<core::ExternalObject_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 85 166 
      return ((85 <= kindVal) && (kindVal <= 166));
  };
};
template <typename FP> struct Cast<core::SimpleBitVector_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 52 52 
      return (kindVal == 52);
  };
};
template <typename FP> struct Cast<core::StandardObject_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 73 84 
      return ((73 <= kindVal) && (kindVal <= 84));
  };
};
template <typename FP> struct Cast<llvmo::ImmutablePass_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 157 159 
      return ((157 <= kindVal) && (kindVal <= 159));
  };
};
template <typename FP> struct Cast<core::RuntimeVisibleEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 34 37 
      return ((34 <= kindVal) && (kindVal <= 37));
  };
};
template <typename FP> struct Cast<core::Closure_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 173 180 
      return ((173 <= kindVal) && (kindVal <= 180));
  };
};
template <typename FP> struct Cast<core::ValueFrame_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 31 31 
      return (kindVal == 31);
  };
};
template <typename FP> struct Cast<core::VaList_dummy_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 7 7 
      return (kindVal == 7);
  };
};
template <typename FP> struct Cast<llvmo::StructType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 150 150 
      return (kindVal == 150);
  };
};
template <typename FP> struct Cast<core::WeakPointer_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 72 72 
      return (kindVal == 72);
  };
};
template <typename FP> struct Cast<llvmo::ExecutionEngine_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 131 131 
      return (kindVal == 131);
  };
};
template <typename FP> struct Cast<llvmo::ConstantDataArray_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 117 117 
      return (kindVal == 117);
  };
};
template <typename FP> struct Cast<core::RegexMatch_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 71 71 
      return (kindVal == 71);
  };
};
template <typename FP> struct Cast<core::FunctionValueEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 35 35 
      return (kindVal == 35);
  };
};
template <typename FP> struct Cast<clbind::ConstructorCreator_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 221 221 
      return (kindVal == 221);
  };
};
template <typename FP> struct Cast<core::BuiltinClosure_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 177 178 
      return ((177 <= kindVal) && (kindVal <= 178));
  };
};
template <typename FP> struct Cast<core::Integer_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 16 18 
      return ((16 <= kindVal) && (kindVal <= 18));
  };
};
template <typename FP> struct Cast<core::StructureClass_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 80 80 
      return (kindVal == 80);
  };
};
template <typename FP> struct Cast<llvmo::IntegerType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 144 144 
      return (kindVal == 144);
  };
};
template <typename FP> struct Cast<llvmo::Attribute_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 70 70 
      return (kindVal == 70);
  };
};
template <typename FP> struct Cast<core::HashTableEqualp_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 216 216 
      return (kindVal == 216);
  };
};
template <typename FP> struct Cast<llvmo::DebugLoc_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 69 69 
      return (kindVal == 69);
  };
};
template <typename FP> struct Cast<core::FunctionContainerEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 41 41 
      return (kindVal == 41);
  };
};
template <typename FP> struct Cast<llvmo::DICompileUnit_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 255 255 
      return (kindVal == 255);
  };
};
template <typename FP> struct Cast<llvmo::VectorType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 147 147 
      return (kindVal == 147);
  };
};
template <typename FP> struct Cast<core::StdClass_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 77 79 
      return ((77 <= kindVal) && (kindVal <= 79));
  };
};
template <typename FP> struct Cast<core::SequenceStepper_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 66 68 
      return ((66 <= kindVal) && (kindVal <= 68));
  };
};
template <typename FP> struct Cast<llvmo::DILexicalBlock_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 254 254 
      return (kindVal == 254);
  };
};
template <typename FP> struct Cast<llvmo::Metadata_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 127 130 
      return ((127 <= kindVal) && (kindVal <= 130));
  };
};
template <typename FP> struct Cast<llvmo::User_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 88 122 
      return ((88 <= kindVal) && (kindVal <= 122));
  };
};
template <typename FP> struct Cast<core::WrappedPointer_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 65 65 
      return (kindVal == 65);
  };
};
template <typename FP> struct Cast<core::UserData_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 182 182 
      return (kindVal == 182);
  };
};
template <typename FP> struct Cast<core::RandomState_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 64 64 
      return (kindVal == 64);
  };
};
template <typename FP> struct Cast<llvmo::SequentialType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 146 149 
      return ((146 <= kindVal) && (kindVal <= 149));
  };
};
template <typename FP> struct Cast<llvmo::ConstantStruct_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 118 118 
      return (kindVal == 118);
  };
};
template <typename FP> struct Cast<llvmo::CallInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 93 93 
      return (kindVal == 93);
  };
};
template <typename FP> struct Cast<llvmo::SwitchInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 102 102 
      return (kindVal == 102);
  };
};
template <typename FP> struct Cast<core::SexpSaveArchive_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 211 211 
      return (kindVal == 211);
  };
};
template <typename FP> struct Cast<core::SingleDispatchMethod_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 63 63 
      return (kindVal == 63);
  };
};
template <typename FP> struct Cast<core::TagbodyFrame_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 30 30 
      return (kindVal == 30);
  };
};
template <typename FP> struct Cast<core::Exposer_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 60 62 
      return ((60 <= kindVal) && (kindVal <= 62));
  };
};
template <typename FP> struct Cast<core::StringStream_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 199 201 
      return ((199 <= kindVal) && (kindVal <= 201));
  };
};
template <typename FP> struct Cast<llvmo::DIBuilder_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 126 126 
      return (kindVal == 126);
  };
};
template <typename FP> struct Cast<core::Array_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 47 59 
      return ((47 <= kindVal) && (kindVal <= 59));
  };
};
template <typename FP> struct Cast<llvmo::ConstantDataSequential_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 116 117 
      return ((116 <= kindVal) && (kindVal <= 117));
  };
};
template <typename FP> struct Cast<llvmo::ConstantInt_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 115 115 
      return (kindVal == 115);
  };
};
template <typename FP> struct Cast<core::CompiledClosure_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 176 176 
      return (kindVal == 176);
  };
};
template <typename FP> struct Cast<core::Environment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 28 46 
      return ((28 <= kindVal) && (kindVal <= 46));
  };
};
template <typename FP> struct Cast<llvmo::ValueAsMetadata_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 128 128 
      return (kindVal == 128);
  };
};
template <typename FP> struct Cast<core::ActivationFrame_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 29 32 
      return ((29 <= kindVal) && (kindVal <= 32));
  };
};
template <typename FP> struct Cast<core::Str_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 56 57 
      return ((56 <= kindVal) && (kindVal <= 57));
  };
};
template <typename FP> struct Cast<llvmo::UnreachableInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 101 101 
      return (kindVal == 101);
  };
};
template <typename FP> struct Cast<core::ConcatenatedStream_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 198 198 
      return (kindVal == 198);
  };
};
template <typename FP> struct Cast<llvmo::PHINode_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 92 92 
      return (kindVal == 92);
  };
};
template <typename FP> struct Cast<llvmo::Instruction_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 89 108 
      return ((89 <= kindVal) && (kindVal <= 108));
  };
};
template <typename FP> struct Cast<core::Rational_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 15 19 
      return ((15 <= kindVal) && (kindVal <= 19));
  };
};
template <typename FP> struct Cast<core::AnsiStream_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 194 205 
      return ((194 <= kindVal) && (kindVal <= 205));
  };
};
template <typename FP> struct Cast<core::SpecialForm_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 244 244 
      return (kindVal == 244);
  };
};
template <typename FP> struct Cast<llvmo::ConstantArray_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 114 114 
      return (kindVal == 114);
  };
};
template <typename FP> struct Cast<core::InstanceClosure_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 175 175 
      return (kindVal == 175);
  };
};
template <typename FP> struct Cast<core::WeakHashTable_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 26 27 
      return ((26 <= kindVal) && (kindVal <= 27));
  };
};
template <typename FP> struct Cast<core::Real_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 14 24 
      return ((14 <= kindVal) && (kindVal <= 24));
  };
};
template <typename FP> struct Cast<llvmo::DISubprogram_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 253 253 
      return (kindVal == 253);
  };
};
template <typename FP> struct Cast<llvmo::DIBasicType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 252 252 
      return (kindVal == 252);
  };
};
template <typename FP> struct Cast<core::FileStatus_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 25 25 
      return (kindVal == 25);
  };
};
template <typename FP> struct Cast<llvmo::GlobalValue_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 111 113 
      return ((111 <= kindVal) && (kindVal <= 113));
  };
};
template <typename FP> struct Cast<llvmo::LandingPadInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 91 91 
      return (kindVal == 91);
  };
};
template <typename FP> struct Cast<llvmo::IRBuilderBase_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 124 125 
      return ((124 <= kindVal) && (kindVal <= 125));
  };
};
template <typename FP> struct Cast<core::SingleDispatchGenericFunction_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 243 243 
      return (kindVal == 243);
  };
};
template <typename FP> struct Cast<core::SymbolMacroletEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 40 40 
      return (kindVal == 40);
  };
};
template <typename FP> struct Cast<llvmo::Value_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 86 123 
      return ((86 <= kindVal) && (kindVal <= 123));
  };
};
template <typename FP> struct Cast<core::Number_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 12 24 
      return ((12 <= kindVal) && (kindVal <= 24));
  };
};
template <typename FP> struct Cast<core::ReadTable_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 11 11 
      return (kindVal == 11);
  };
};
template <typename FP> struct Cast<core::UnwindProtectEnvironment_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 39 39 
      return (kindVal == 39);
  };
};
template <typename FP> struct Cast<core::Bignum_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 17 17 
      return (kindVal == 17);
  };
};
template <typename FP> struct Cast<core::CompiledFunction_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 242 242 
      return (kindVal == 242);
  };
};
template <typename FP> struct Cast<core::ArrayObjects_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 48 48 
      return (kindVal == 48);
  };
};
template <typename FP> struct Cast<llvmo::DIArray_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 251 251 
      return (kindVal == 251);
  };
};
template <typename FP> struct Cast<llvmo::ModulePass_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 156 159 
      return ((156 <= kindVal) && (kindVal <= 159));
  };
};
template <typename FP> struct Cast<llvmo::DIDerivedType_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 250 250 
      return (kindVal == 250);
  };
};
template <typename FP> struct Cast<llvmo::Argument_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 87 87 
      return (kindVal == 87);
  };
};
template <typename FP> struct Cast<core::Complex_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 13 13 
      return (kindVal == 13);
  };
};
template <typename FP> struct Cast<llvmo::BlockAddress_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 110 110 
      return (kindVal == 110);
  };
};
template <typename FP> struct Cast<core::BitVector_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 51 53 
      return ((51 <= kindVal) && (kindVal <= 53));
  };
};
template <typename FP> struct Cast<llvmo::FunctionPass_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 155 155 
      return (kindVal == 155);
  };
};
template <typename FP> struct Cast<core::FileStream_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 195 197 
      return ((195 <= kindVal) && (kindVal <= 197));
  };
};
template <typename FP> struct Cast<llvmo::AtomicRMWInst_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 90 90 
      return (kindVal == 90);
  };
};
template <typename FP> struct Cast<core::HashTableEq_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 215 215 
      return (kindVal == 215);
  };
};
template <typename FP> struct Cast<llvmo::DataLayoutPass_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 158 158 
      return (kindVal == 158);
  };
};
template <typename FP> struct Cast<core::MultiStringBuffer_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 10 10 
      return (kindVal == 10);
  };
};
template <typename FP> struct Cast<clbind::DummyCreator_O*,FP> {
  inline static bool isA(FP client) {
      gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
      int kindVal = header->kind();
      // low high --> 220 220 
      return (kindVal == 220);
  };
};
#endif // defined(GC_DYNAMIC_CAST)
#if defined(GC_KIND_SELECTORS)
template <> class gctools::GCKind<llvmo::VAArgInst_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__VAArgInst_O ;
};
template <> class gctools::GCKind<core::LoadArchive_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__LoadArchive_O ;
};
template <> class gctools::GCKind<core::Ratio_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Ratio_O ;
};
template <> class gctools::GCKind<core::FunctionClosure_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__FunctionClosure_O ;
};
template <> class gctools::GCKind<core::VectorObjects_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__VectorObjects_O ;
};
template <> class gctools::GCKind<core::CoreExposer_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__CoreExposer_O ;
};
template <> class gctools::GCKind<core::GlueEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__GlueEnvironment_O ;
};
template <> class gctools::GCKind<llvmo::Target_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__Target_O ;
};
template <> class gctools::GCKind<llvmo::FunctionType_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__FunctionType_O ;
};
template <> class gctools::GCKind<llvmo::ConstantExpr_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__ConstantExpr_O ;
};
template <> class gctools::GCKind<llvmo::LLVMContext_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__LLVMContext_O ;
};
template <> class gctools::GCKind<core::Lisp_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_ROOTCLASSALLOC_core__Lisp_O ;
};
template <> class gctools::GCKind<llvmo::TargetSubtargetInfo_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__TargetSubtargetInfo_O ;
};
template <> class gctools::GCKind<core::ShortFloat_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__ShortFloat_O ;
};
template <> class gctools::GCKind<core::SmallMap_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SmallMap_O ;
};
template <> class gctools::GCKind<llvmo::FenceInst_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__FenceInst_O ;
};
template <> class gctools::GCKind<core::PosixTime_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__PosixTime_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::KeywordArgument>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__KeywordArgument_ ;
};
template <> class gctools::GCKind<core::Pathname_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Pathname_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<core::List_V>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__List_V__ ;
};
template <> class gctools::GCKind<core::MacroClosure_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__MacroClosure_O ;
};
template <> class gctools::GCKind<core::LexicalEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__LexicalEnvironment_O ;
};
template <> class gctools::GCKind<gctools::GCString_moveable<char>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCSTRING_gctools__GCString_moveable_char_ ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::RequiredArgument>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__RequiredArgument_ ;
};
template <> class gctools::GCKind<core::CatchEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__CatchEnvironment_O ;
};
template <> class gctools::GCKind<core::BroadcastStream_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__BroadcastStream_O ;
};
template <> class gctools::GCKind<core::Instance_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Instance_O ;
};
template <> class gctools::GCKind<core::VectorStepper_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__VectorStepper_O ;
};
template <> class gctools::GCKind<llvmo::DIScope_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__DIScope_O ;
};
template <> class gctools::GCKind<core::ForeignData_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__ForeignData_O ;
};
template <> class gctools::GCKind<core::WeakKeyHashTable_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__WeakKeyHashTable_O ;
};
template <> class gctools::GCKind<llvmo::IndirectBrInst_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__IndirectBrInst_O ;
};
template <> class gctools::GCKind<core::InterpretedClosure_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__InterpretedClosure_O ;
};
template <> class gctools::GCKind<llvmo::ConstantPointerNull_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__ConstantPointerNull_O ;
};
template <> class gctools::GCKind<llvmo::EngineBuilder_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__EngineBuilder_O ;
};
template <> class gctools::GCKind<llvmo::Module_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__Module_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<asttooling::RegMap::SymbolMatcherDescriptorPair_O>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__RegMap__SymbolMatcherDescriptorPair_O_ ;
};
template <> class gctools::GCKind<core::SmallMultimap_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SmallMultimap_O ;
};
template <> class gctools::GCKind<core::LogicalPathname_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__LogicalPathname_O ;
};
template <> class gctools::GCKind<llvmo::DebugInfo_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__DebugInfo_O ;
};
template <> class gctools::GCKind<clbind::ClassRegistry_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_clbind__ClassRegistry_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<core::SourceFileInfo_O>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SourceFileInfo_O__ ;
};
template <> class gctools::GCKind<core::Pointer_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Pointer_O ;
};
template <> class gctools::GCKind<core::Function_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Function_O ;
};
template <> class gctools::GCKind<core::HashTableEqual_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__HashTableEqual_O ;
};
template <> class gctools::GCKind<core::Character_dummy_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Character_dummy_O ;
};
template <> class gctools::GCKind<core::DirectoryEntry_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__DirectoryEntry_O ;
};
template <> class gctools::GCKind<llvmo::MCSubtargetInfo_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__MCSubtargetInfo_O ;
};
template <> class gctools::GCKind<asttooling::internal::VariadicOperatorMatcherDescriptor_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_asttooling__internal__VariadicOperatorMatcherDescriptor_O ;
};
template <> class gctools::GCKind<llvmo::MDString_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__MDString_O ;
};
template <> class gctools::GCKind<core::Class_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__Class_O ;
};
template <> class gctools::GCKind<core::FuncallableStandardClass_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__FuncallableStandardClass_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::Symbol_O>,gctools::smart_ptr<core::T_O>>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__Symbol_O__gctools__smart_ptr_core__T_O___ ;
};
template <> class gctools::GCKind<llvmo::DIFile_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__DIFile_O ;
};
template <> class gctools::GCKind<llvmo::Pass_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__Pass_O ;
};
template <> class gctools::GCKind<llvmo::Linker_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__Linker_O ;
};
template <> class gctools::GCKind<llvmo::TerminatorInst_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__TerminatorInst_O ;
};
template <> class gctools::GCKind<core::FunctionFrame_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__FunctionFrame_O ;
};
template <> class gctools::GCKind<llvmo::UndefValue_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__UndefValue_O ;
};
template <> class gctools::GCKind<core::TwoWayStream_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__TwoWayStream_O ;
};
template <> class gctools::GCKind<core::IOFileStream_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__IOFileStream_O ;
};
template <> class gctools::GCKind<llvmo::NamedMDNode_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__NamedMDNode_O ;
};
template <> class gctools::GCKind<llvmo::ArrayType_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__ArrayType_O ;
};
template <> class gctools::GCKind<core::RecursiveDirectoryIterator_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__RecursiveDirectoryIterator_O ;
};
template <> class gctools::GCKind<llvmo::Type_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__Type_O ;
};
template <> class gctools::GCKind<llvmo::AtomicCmpXchgInst_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__AtomicCmpXchgInst_O ;
};
template <> class gctools::GCKind<llvmo::AllocaInst_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__AllocaInst_O ;
};
template <> class gctools::GCKind<core::Package_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Package_O ;
};
template <> class gctools::GCKind<core::Fixnum_dummy_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Fixnum_dummy_O ;
};
template <> class gctools::GCKind<llvmo::BasicBlock_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__BasicBlock_O ;
};
template <> class gctools::GCKind<llvmo::CompositeType_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__CompositeType_O ;
};
template <> class gctools::GCKind<core::CompileTimeEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__CompileTimeEnvironment_O ;
};
template <> class gctools::GCKind<llvmo::PassManager_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__PassManager_O ;
};
template <> class gctools::GCKind<core::TagbodyEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__TagbodyEnvironment_O ;
};
template <> class gctools::GCKind<core::IOStreamStream_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__IOStreamStream_O ;
};
template <> class gctools::GCKind<core::InvocationHistoryFrameIterator_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__InvocationHistoryFrameIterator_O ;
};
template <> class gctools::GCKind<core::StrWithFillPtr_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__StrWithFillPtr_O ;
};
template <> class gctools::GCKind<core::StructureObject_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__StructureObject_O ;
};
template <> class gctools::GCKind<core::ValueEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__ValueEnvironment_O ;
};
template <> class gctools::GCKind<llvmo::AttributeSet_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__AttributeSet_O ;
};
template <> class gctools::GCKind<core::BuiltInClass_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__BuiltInClass_O ;
};
template <> class gctools::GCKind<core::CxxClass_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__CxxClass_O ;
};
template <> class gctools::GCKind<core::SingleFloat_dummy_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SingleFloat_dummy_O ;
};
template <> class gctools::GCKind<core::DirectoryIterator_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__DirectoryIterator_O ;
};
template <> class gctools::GCKind<core::SexpLoadArchive_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SexpLoadArchive_O ;
};
template <> class gctools::GCKind<core::StringOutputStream_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__StringOutputStream_O ;
};
template <> class gctools::GCKind<asttooling::AstVisitor_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_asttooling__AstVisitor_O ;
};
template <> class gctools::GCKind<core::Vector_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Vector_O ;
};
template <> class gctools::GCKind<core::Path_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Path_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::SymbolStorage>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolStorage_ ;
};
template <> class gctools::GCKind<llvmo::Function_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__Function_O ;
};
template <> class gctools::GCKind<llvmo::InvokeInst_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__InvokeInst_O ;
};
template <> class gctools::GCKind<core::SNode_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SNode_O ;
};
template <> class gctools::GCKind<core::SourceFileInfo_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SourceFileInfo_O ;
};
template <> class gctools::GCKind<llvmo::InsertPoint_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__InsertPoint_O ;
};
template <> class gctools::GCKind<llvmo::DITypeArray_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__DITypeArray_O ;
};
template <> class gctools::GCKind<core::ForwardReferencedClass_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__ForwardReferencedClass_O ;
};
template <> class gctools::GCKind<core::HashTableEql_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__HashTableEql_O ;
};
template <> class gctools::GCKind<core::LambdaListHandler_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__LambdaListHandler_O ;
};
template <> class gctools::GCKind<core::ArrayDisplaced_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__ArrayDisplaced_O ;
};
template <> class gctools::GCKind<core::Cache_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Cache_O ;
};
template <> class gctools::GCKind<llvmo::TargetOptions_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__TargetOptions_O ;
};
template <> class gctools::GCKind<core::Metaobject_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__Metaobject_O ;
};
template <> class gctools::GCKind<llvmo::PointerType_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__PointerType_O ;
};
template <> class gctools::GCKind<core::WeakKeyMapping_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__WeakKeyMapping_O ;
};
template <> class gctools::GCKind<core::CxxObject_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__CxxObject_O ;
};
template <> class gctools::GCKind<cffi::Pointer_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_cffi__Pointer_O ;
};
template <> class gctools::GCKind<core::InstanceCreator_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__InstanceCreator_O ;
};
template <> class gctools::GCKind<llvmo::DICompositeType_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__DICompositeType_O ;
};
template <> class gctools::GCKind<core::General_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__General_O ;
};
template <> class gctools::GCKind<core::Creator_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_TEMPLATED_LISPALLOC_core__Creator_O ;
};
template <> class gctools::GCKind<llvmo::BranchInst_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__BranchInst_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<asttooling::ContextFrame>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ContextFrame_ ;
};
template <> class gctools::GCKind<core::MacroletEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__MacroletEnvironment_O ;
};
template <> class gctools::GCKind<core::HashTable_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__HashTable_O ;
};
template <> class gctools::GCKind<core::Archive_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Archive_O ;
};
template <> class gctools::GCKind<asttooling::RegMap::RegistryMaps_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_asttooling__RegMap__RegistryMaps_O ;
};
template <> class gctools::GCKind<core::Cons_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Cons_O ;
};
template <> class gctools::GCKind<core::SharpEqualWrapper_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SharpEqualWrapper_O ;
};
template <> class gctools::GCKind<core::SingleDispatchGenericFunctionClosure_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SingleDispatchGenericFunctionClosure_O ;
};
template <> class gctools::GCKind<core::Reader_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Reader_O ;
};
template <> class gctools::GCKind<core::Stream_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Stream_O ;
};
template <> class gctools::GCKind<llvmo::UnaryInstruction_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__UnaryInstruction_O ;
};
template <> class gctools::GCKind<core::BranchSNode_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__BranchSNode_O ;
};
template <> class gctools::GCKind<llvmo::FunctionPassManager_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__FunctionPassManager_O ;
};
template <> class gctools::GCKind<core::SingleDispatchEffectiveMethodFunction_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SingleDispatchEffectiveMethodFunction_O ;
};
template <> class gctools::GCKind<core::StringInputStream_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__StringInputStream_O ;
};
template <> class gctools::GCKind<llvmo::TargetMachine_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__TargetMachine_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::T_O *>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__T_O_P_ ;
};
template <> class gctools::GCKind<core::CandoException_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__CandoException_O ;
};
template <> class gctools::GCKind<llvmo::GlobalVariable_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__GlobalVariable_O ;
};
template <> class gctools::GCKind<core::EchoStream_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__EchoStream_O ;
};
template <> class gctools::GCKind<core::SymbolToEnumConverter_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SymbolToEnumConverter_O ;
};
template <> class gctools::GCKind<core::PosixTimeDuration_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__PosixTimeDuration_O ;
};
template <> class gctools::GCKind<core::StandardClass_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__StandardClass_O ;
};
template <> class gctools::GCKind<core::Regex_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Regex_O ;
};
template <> class gctools::GCKind<llvmo::PassManagerBase_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__PassManagerBase_O ;
};
template <> class gctools::GCKind<core::Iterator_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_TEMPLATED_LISPALLOC_core__Iterator_O ;
};
template <> class gctools::GCKind<clbind::ClassRep_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_clbind__ClassRep_O ;
};
template <> class gctools::GCKind<llvmo::IRBuilder_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__IRBuilder_O ;
};
template <> class gctools::GCKind<llvmo::StoreInst_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__StoreInst_O ;
};
template <> class gctools::GCKind<core::LongFloat_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__LongFloat_O ;
};
template <> class gctools::GCKind<core::BlockEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__BlockEnvironment_O ;
};
template <> class gctools::GCKind<llvmo::ResumeInst_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__ResumeInst_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<asttooling::internal::MatcherDescriptor_O>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_asttooling__internal__MatcherDescriptor_O__ ;
};
template <> class gctools::GCKind<llvmo::TargetLibraryInfo_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__TargetLibraryInfo_O ;
};
template <> class gctools::GCKind<llvmo::DISubroutineType_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__DISubroutineType_O ;
};
template <> class gctools::GCKind<llvmo::DIType_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__DIType_O ;
};
template <> class gctools::GCKind<core::Float_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Float_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::AuxArgument>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__AuxArgument_ ;
};
template <> class gctools::GCKind<core::VectorObjectsWithFillPtr_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__VectorObjectsWithFillPtr_O ;
};
template <> class gctools::GCKind<llvmo::MDNode_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__MDNode_O ;
};
template <> class gctools::GCKind<core::SourcePosInfo_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SourcePosInfo_O ;
};
template <> class gctools::GCKind<llvmo::DIDescriptor_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__DIDescriptor_O ;
};
template <> class gctools::GCKind<llvmo::LLVMTargetMachine_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__LLVMTargetMachine_O ;
};
template <> class gctools::GCKind<llvmo::APInt_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__APInt_O ;
};
template <> class gctools::GCKind<llvmo::ReturnInst_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__ReturnInst_O ;
};
template <> class gctools::GCKind<core::ConsStepper_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__ConsStepper_O ;
};
template <> class gctools::GCKind<llvmo::Triple_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__Triple_O ;
};
template <> class gctools::GCKind<core::DoubleFloat_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__DoubleFloat_O ;
};
template <> class gctools::GCKind<core::Symbol_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__Symbol_O ;
};
template <> class gctools::GCKind<llvmo::DataLayout_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__DataLayout_O ;
};
template <> class gctools::GCKind<core::LightUserData_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__LightUserData_O ;
};
template <> class gctools::GCKind<core::Functor_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Functor_O ;
};
template <> class gctools::GCKind<core::T_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__T_O ;
};
template <> class gctools::GCKind<core::Null_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Null_O ;
};
template <> class gctools::GCKind<asttooling::AsttoolingExposer_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_asttooling__AsttoolingExposer_O ;
};
template <> class gctools::GCKind<llvmo::PassManagerBuilder_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__PassManagerBuilder_O ;
};
template <> class gctools::GCKind<core::Specializer_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__Specializer_O ;
};
template <> class gctools::GCKind<core::StackValueEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__StackValueEnvironment_O ;
};
template <> class gctools::GCKind<llvmo::ConstantFP_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__ConstantFP_O ;
};
template <> class gctools::GCKind<llvmo::LoadInst_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__LoadInst_O ;
};
template <> class gctools::GCKind<llvmo::APFloat_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__APFloat_O ;
};
template <> class gctools::GCKind<core::Record_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Record_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<asttooling::ParserValue>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ParserValue_ ;
};
template <> class gctools::GCKind<core::SaveArchive_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SaveArchive_O ;
};
template <> class gctools::GCKind<core::SourceManager_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SourceManager_O ;
};
template <> class gctools::GCKind<core::IntArray_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__IntArray_O ;
};
template <> class gctools::GCKind<asttooling::internal::FreeFuncMatcherDescriptor_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_asttooling__internal__FreeFuncMatcherDescriptor_O ;
};
template <> class gctools::GCKind<clbind::detail::class_map> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_ROOTCLASSALLOC_clbind__detail__class_map ;
};
template <> class gctools::GCKind<core::BitVectorWithFillPtr_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__BitVectorWithFillPtr_O ;
};
template <> class gctools::GCKind<core::LeafSNode_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__LeafSNode_O ;
};
template <> class gctools::GCKind<asttooling::internal::FixedArgCountMatcherDescriptor_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_asttooling__internal__FixedArgCountMatcherDescriptor_O ;
};
template <> class gctools::GCKind<llvmo::Constant_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__Constant_O ;
};
template <> class gctools::GCKind<core::Binder_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Binder_O ;
};
template <> class gctools::GCKind<core::String_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__String_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<core::Symbol_O>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Symbol_O__ ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::SymbolClassPair>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__SymbolClassPair_ ;
};
template <> class gctools::GCKind<core::VectorDisplaced_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__VectorDisplaced_O ;
};
template <> class gctools::GCKind<core::SynonymStream_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SynonymStream_O ;
};
template <> class gctools::GCKind<core::LoadTimeValues_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__LoadTimeValues_O ;
};
template <> class gctools::GCKind<core::ExternalObject_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__ExternalObject_O ;
};
template <> class gctools::GCKind<core::SimpleBitVector_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SimpleBitVector_O ;
};
template <> class gctools::GCKind<core::StandardObject_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__StandardObject_O ;
};
template <> class gctools::GCKind<llvmo::ImmutablePass_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__ImmutablePass_O ;
};
template <> class gctools::GCKind<core::RuntimeVisibleEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__RuntimeVisibleEnvironment_O ;
};
template <> class gctools::GCKind<core::Closure_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Closure_O ;
};
template <> class gctools::GCKind<core::ValueFrame_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__ValueFrame_O ;
};
template <> class gctools::GCKind<core::VaList_dummy_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__VaList_dummy_O ;
};
template <> class gctools::GCKind<llvmo::StructType_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__StructType_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<std::pair<gctools::smart_ptr<core::T_O>,gctools::smart_ptr<core::T_O>>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_std__pair_gctools__smart_ptr_core__T_O__gctools__smart_ptr_core__T_O___ ;
};
template <> class gctools::GCKind<core::WeakPointer_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__WeakPointer_O ;
};
template <> class gctools::GCKind<gctools::GCArray_moveable<gctools::smart_ptr<core::T_O>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCARRAY_gctools__GCArray_moveable_gctools__smart_ptr_core__T_O__ ;
};
template <> class gctools::GCKind<llvmo::ExecutionEngine_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__ExecutionEngine_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::CacheRecord>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__CacheRecord_ ;
};
template <> class gctools::GCKind<llvmo::ConstantDataArray_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__ConstantDataArray_O ;
};
template <> class gctools::GCKind<core::RegexMatch_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__RegexMatch_O ;
};
template <> class gctools::GCKind<core::FunctionValueEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__FunctionValueEnvironment_O ;
};
template <> class gctools::GCKind<clbind::ConstructorCreator_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_TEMPLATED_LISPALLOC_clbind__ConstructorCreator_O ;
};
template <> class gctools::GCKind<core::BuiltinClosure_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_TEMPLATED_LISPALLOC_core__BuiltinClosure_O ;
};
template <> class gctools::GCKind<core::Integer_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Integer_O ;
};
template <> class gctools::GCKind<core::StructureClass_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__StructureClass_O ;
};
template <> class gctools::GCKind<llvmo::IntegerType_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__IntegerType_O ;
};
template <> class gctools::GCKind<llvmo::Attribute_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__Attribute_O ;
};
template <> class gctools::GCKind<core::HashTableEqualp_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__HashTableEqualp_O ;
};
template <> class gctools::GCKind<llvmo::DebugLoc_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__DebugLoc_O ;
};
template <> class gctools::GCKind<core::FunctionContainerEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__FunctionContainerEnvironment_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<core::Cons_O>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Cons_O__ ;
};
template <> class gctools::GCKind<llvmo::DICompileUnit_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__DICompileUnit_O ;
};
template <> class gctools::GCKind<asttooling::DerivableFrontendActionFactory> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_asttooling__DerivableFrontendActionFactory ;
};
template <> class gctools::GCKind<llvmo::VectorType_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__VectorType_O ;
};
template <> class gctools::GCKind<core::StdClass_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__StdClass_O ;
};
template <> class gctools::GCKind<core::SequenceStepper_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SequenceStepper_O ;
};
template <> class gctools::GCKind<llvmo::DILexicalBlock_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__DILexicalBlock_O ;
};
template <> class gctools::GCKind<llvmo::Metadata_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__Metadata_O ;
};
template <> class gctools::GCKind<llvmo::User_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__User_O ;
};
template <> class gctools::GCKind<core::WrappedPointer_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_TEMPLATED_LISPALLOC_core__WrappedPointer_O ;
};
template <> class gctools::GCKind<asttooling::DerivableMatchCallback> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_asttooling__DerivableMatchCallback ;
};
template <> class gctools::GCKind<core::UserData_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__UserData_O ;
};
template <> class gctools::GCKind<core::RandomState_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__RandomState_O ;
};
template <> class gctools::GCKind<llvmo::SequentialType_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__SequentialType_O ;
};
template <> class gctools::GCKind<llvmo::ConstantStruct_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__ConstantStruct_O ;
};
template <> class gctools::GCKind<llvmo::CallInst_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__CallInst_O ;
};
template <> class gctools::GCKind<asttooling::internal::OverloadedMatcherDescriptor_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_asttooling__internal__OverloadedMatcherDescriptor_O ;
};
template <> class gctools::GCKind<llvmo::SwitchInst_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__SwitchInst_O ;
};
template <> class gctools::GCKind<core::SexpSaveArchive_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SexpSaveArchive_O ;
};
template <> class gctools::GCKind<core::SingleDispatchMethod_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SingleDispatchMethod_O ;
};
template <> class gctools::GCKind<core::TagbodyFrame_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__TagbodyFrame_O ;
};
template <> class gctools::GCKind<core::Exposer_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Exposer_O ;
};
template <> class gctools::GCKind<core::StringStream_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__StringStream_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<asttooling::ErrorContent>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__ErrorContent_ ;
};
template <> class gctools::GCKind<llvmo::DIBuilder_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__DIBuilder_O ;
};
template <> class gctools::GCKind<asttooling::DerivableASTFrontendAction> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_asttooling__DerivableASTFrontendAction ;
};
template <> class gctools::GCKind<core::Array_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Array_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<asttooling::Message>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_asttooling__Message_ ;
};
template <> class gctools::GCKind<llvmo::ConstantDataSequential_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__ConstantDataSequential_O ;
};
template <> class gctools::GCKind<llvmo::ConstantInt_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__ConstantInt_O ;
};
template <> class gctools::GCKind<core::CompiledClosure_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__CompiledClosure_O ;
};
template <> class gctools::GCKind<core::Environment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Environment_O ;
};
template <> class gctools::GCKind<llvmo::ValueAsMetadata_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__ValueAsMetadata_O ;
};
template <> class gctools::GCKind<core::ActivationFrame_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__ActivationFrame_O ;
};
template <> class gctools::GCKind<core::Str_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_BOOTSTRAP_core__Str_O ;
};
template <> class gctools::GCKind<llvmo::UnreachableInst_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__UnreachableInst_O ;
};
template <> class gctools::GCKind<core::ConcatenatedStream_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__ConcatenatedStream_O ;
};
template <> class gctools::GCKind<llvmo::PHINode_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__PHINode_O ;
};
template <> class gctools::GCKind<llvmo::Instruction_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__Instruction_O ;
};
template <> class gctools::GCKind<core::Rational_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Rational_O ;
};
template <> class gctools::GCKind<core::AnsiStream_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__AnsiStream_O ;
};
template <> class gctools::GCKind<core::SpecialForm_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SpecialForm_O ;
};
template <> class gctools::GCKind<llvmo::ConstantArray_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__ConstantArray_O ;
};
template <> class gctools::GCKind<core::InstanceClosure_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__InstanceClosure_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<core::T_O>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__T_O__ ;
};
template <> class gctools::GCKind<core::WeakHashTable_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__WeakHashTable_O ;
};
template <> class gctools::GCKind<asttooling::DerivableSyntaxOnlyAction> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_asttooling__DerivableSyntaxOnlyAction ;
};
template <> class gctools::GCKind<core::Real_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Real_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<core::SingleDispatchMethod_O>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SingleDispatchMethod_O__ ;
};
template <> class gctools::GCKind<llvmo::DISubprogram_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__DISubprogram_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::OptionalArgument>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__OptionalArgument_ ;
};
template <> class gctools::GCKind<llvmo::DIBasicType_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__DIBasicType_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<core::SequenceStepper_O>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__SequenceStepper_O__ ;
};
template <> class gctools::GCKind<core::FileStatus_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__FileStatus_O ;
};
template <> class gctools::GCKind<llvmo::GlobalValue_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__GlobalValue_O ;
};
template <> class gctools::GCKind<llvmo::LandingPadInst_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__LandingPadInst_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<core::Package_O>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_core__Package_O__ ;
};
template <> class gctools::GCKind<llvmo::IRBuilderBase_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__IRBuilderBase_O ;
};
template <> class gctools::GCKind<core::SingleDispatchGenericFunction_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SingleDispatchGenericFunction_O ;
};
template <> class gctools::GCKind<core::SymbolMacroletEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__SymbolMacroletEnvironment_O ;
};
template <> class gctools::GCKind<llvmo::Value_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__Value_O ;
};
template <> class gctools::GCKind<core::Number_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Number_O ;
};
template <> class gctools::GCKind<core::ReadTable_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__ReadTable_O ;
};
template <> class gctools::GCKind<core::UnwindProtectEnvironment_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__UnwindProtectEnvironment_O ;
};
template <> class gctools::GCKind<core::Bignum_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Bignum_O ;
};
template <> class gctools::GCKind<core::CompiledFunction_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__CompiledFunction_O ;
};
template <> class gctools::GCKind<core::ArrayObjects_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__ArrayObjects_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::ExceptionEntry>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__ExceptionEntry_ ;
};
template <> class gctools::GCKind<llvmo::DIArray_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__DIArray_O ;
};
template <> class gctools::GCKind<llvmo::ModulePass_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__ModulePass_O ;
};
template <> class gctools::GCKind<llvmo::DIDerivedType_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__DIDerivedType_O ;
};
template <> class gctools::GCKind<llvmo::Argument_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__Argument_O ;
};
template <> class gctools::GCKind<core::Complex_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__Complex_O ;
};
template <> class gctools::GCKind<llvmo::BlockAddress_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__BlockAddress_O ;
};
template <> class gctools::GCKind<core::BitVector_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__BitVector_O ;
};
template <> class gctools::GCKind<llvmo::FunctionPass_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__FunctionPass_O ;
};
template <> class gctools::GCKind<core::FileStream_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__FileStream_O ;
};
template <> class gctools::GCKind<llvmo::AtomicRMWInst_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__AtomicRMWInst_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<core::DynamicBinding>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_core__DynamicBinding_ ;
};
template <> class gctools::GCKind<core::HashTableEq_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__HashTableEq_O ;
};
template <> class gctools::GCKind<llvmo::DataLayoutPass_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_llvmo__DataLayoutPass_O ;
};
template <> class gctools::GCKind<core::MultiStringBuffer_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_core__MultiStringBuffer_O ;
};
template <> class gctools::GCKind<gctools::GCVector_moveable<gctools::smart_ptr<clbind::ClassRep_O>>> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_GCVECTOR_gctools__GCVector_moveable_gctools__smart_ptr_clbind__ClassRep_O__ ;
};
template <> class gctools::GCKind<clbind::DummyCreator_O> {
public:
  static gctools::GCKindEnum const Kind = gctools::KIND_LISPALLOC_clbind__DummyCreator_O ;
};
#endif // defined(GC_KIND_SELECTORS)
#if defined(GC_OBJ_SCAN)
